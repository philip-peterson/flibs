! nfa.f90 --
!     Regular expression implementation.
!     Supports only ( | ) * + ?.  No escapes.
!     Compiles to NFA and then simulates NFA
!     using Thompson's algorithm.
!
!     Straightforward translation into Fortran
!
!     TODO:
!     - Add support for ".", "^", "$" and escaped characters
!     - Add support for character classes (including [^a])
!     - Add wrappers for convenient matching:
!       - Find the first substring (return start and stop) that matches
!       - Find the longest substring (return start and stop) that matches
!
!
! From the original source file:
!/*
! * Permission is hereby granted, free of charge, to any person
! * obtaining a copy of this software and associated
! * documentation files (the "Software"), to deal in the
! * Software without restriction, including without limitation
! * the rights to use, copy, modify, merge, publish, distribute,
! * sublicense, and/or sell copies of the Software, and to
! * permit persons to whom the Software is furnished to do so,
! * subject to the following conditions:
! *
! * The above copyright notice and this permission notice shall
! * be included in all copies or substantial portions of the
! * Software.
! *
! * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
! * KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
! * WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! * PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS
! * OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
! * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
! * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
! * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
! *
! * See also http://swtch.com/~rsc/regexp/ and
! * Thompson, Ken.  Regular Expression Search Algorithm,
! * Communications of the ACM 11(6) (June 1968), pp. 419-422.
! *
! * Copyright (c) 2007 Russ Cox.
! * Can be distributed under the MIT license, see bottom of file.
! */
!

module regexp
    implicit none
    private

    type re_code_element
        integer    :: code
        character  :: char
    end type re_code_element

    public :: re2post, re_code_element ! Temporary
    public :: match_full

    integer, parameter :: CHAR      = 1
    integer, parameter :: CONCAT    = 2
    integer, parameter :: ALTERN    = 3
    integer, parameter :: ZERO_MORE = 4
    integer, parameter :: ONE_MORE  = 5
    integer, parameter :: ZERO_ONE  = 6
    integer, parameter :: MATCH     = 101
    integer, parameter :: SPLIT     = 102

    type re_state
        integer   code       ! RE code
        integer   out        ! Index in the state array
        integer   out1       ! Index in the state array
        integer   lastlist   ! ??
        character char       ! Character to match
    end type re_state

    type re_list
        integer, dimension(:), allocatable :: state_idx  ! Indices into state array
        integer                            :: n          ! Number of filled state indices
    end type re_list

    type re_ptrlist
        integer                   :: start
        type(re_ptrlist), pointer :: next
    end type re_ptrlist

    !
    ! A partially built NFA without the matching state filled in.
    ! frag%start points at the start state.
    ! frag%out is a list of places that need to be set to the
    ! next state for this fragment.
    !
    type re_frag
        integer                   :: start
        type(re_ptrlist), pointer :: out
    end type re_frag

    integer, save :: matchstate = 1 ! matching state
    integer       :: nstate
    integer       :: nplist
    integer       :: listid
    type(re_list) :: clist, nlist
    type(re_state),   dimension(:), allocatable, target :: states
    type(re_ptrlist), dimension(:), allocatable, target :: plists

    interface state
        module procedure state_integer
        module procedure state_code
    end interface
contains

! re2post --
!     Convert infix regexp to postfix-like notation
!
! Arguments:
!     re           String containing the regular expression
!     re_code      Array with codes representing the RE
!     n_code       Length of the RE codes array
!     error        If true, invalid RE
!
! Original comments:
!     Convert infix regexp re to postfix notation.
!     Insert . as explicit concatenation operator.
!     Cheesy parser, return static buffer.
!
subroutine re2post( re, re_code, n_code, error, string )
    character(len=*)                    :: re
    character(len=*)                    :: string
    type(re_code_element), dimension(:) :: re_code
    integer                             :: n_code
    logical                             :: error

    type parenthesis
        integer :: nalt
        integer :: natom
    end type parenthesis

    type(parenthesis), dimension(100)   :: paren
    integer                             :: c
    integer                             :: p
    integer                             :: dst
    integer                             :: nalt
    integer                             :: natom

    error = .false.

    p     = 1           ! Index into paren array
    dst   = 1           ! Index into re_code array
    nalt  = 0
    natom = 0

    string = ' '

    do c = 1,len(re)
        select case( re(c:c) )
            case( '(' )
                if ( natom > 1 ) then
                    natom = natom - 1
                    re_code(dst)%code = CONCAT
                    string(dst:dst)   = '.'
                    dst   = dst + 1
                endif
                if ( p > 100 ) then
                    error = .true.
                    return
                endif

                paren(p)%nalt  = nalt
                paren(p)%natom = natom
                p     = p + 1
                nalt  = 0
                natom = 0

            case ( '|' )
                if( natom == 0 ) then
                    error = .true.
                    return
                endif

                do
                    natom = natom - 1
                    if ( natom <= 0 ) then
                        exit
                    endif
                    re_code(dst)%code = CONCAT
                    string(dst:dst)   = '.'
                    dst = dst + 1
                enddo
                nalt = nalt + 1

            case ( ')' )
                if ( p == 1 .or. natom == 0 ) then
                    error = .true.
                    return
                endif

                do
                    natom = natom - 1
                    if ( natom <= 0 ) then
                        exit
                    endif

                    re_code(dst)%code = CONCAT
                    string(dst:dst)   = '.'
                    dst = dst + 1
                enddo
                do nalt = nalt,1,-1
                    re_code(dst)%code = ALTERN
                    string(dst:dst)   = '|'
                    dst = dst + 1
                enddo
                p     = p - 1
                nalt  = paren(p)%nalt
                natom = paren(p)%natom + 1

            case ( '*', '+', '?' )
                if ( natom == 0 ) then
                    error = .true.
                    return
                endif

                select case ( re(c:c) )
                    case ( '*' )
                        re_code(dst)%code = ZERO_MORE
                        string(dst:dst)   = '*'
                    case ( '+' )
                        re_code(dst)%code = ONE_MORE
                        string(dst:dst)   = '+'
                    case ( '?' )
                        re_code(dst)%code = ZERO_ONE
                        string(dst:dst)   = '?'
                end select
                dst = dst + 1

            case default
                if ( natom > 1 ) then
                    natom = natom - 1
                    re_code(dst)%code = CONCAT
                    string(dst:dst)   = '.'
                    dst = dst + 1
                endif
                re_code(dst)%char = re(c:c)
                re_code(dst)%code = CHAR
                string(dst:dst)   = re(c:c)
                dst   = dst   + 1
                natom = natom + 1
        end select
    enddo

    if ( p > 1 ) then
        error = .true.
        return
    endif

    do
        natom = natom - 1
        if ( natom <= 0 ) then
            exit
        endif
        re_code(dst)%code = CONCAT
        string(dst:dst)   = '.'
        dst = dst + 1
    enddo
    do nalt = nalt,1,-1
        re_code(dst)%code = ALTERN
        string(dst:dst)   = '|'
        dst = dst + 1
    enddo

    n_code = dst
end subroutine re2post

! state_integer --
!     Allocate and initialize State
!
! Arguments:
!     code           Raw RE code
!     out            RE state index
!     out1           RE state index
integer function state_integer(code, out, out1)
    integer               :: code
    integer               :: out
    integer               :: out1

    nstate = nstate + 1

    states(nstate)%lastlist = 0
    states(nstate)%code     = code
    states(nstate)%char     = ' '
    states(nstate)%out      = out
    states(nstate)%out1     = out1

    state_integer = nstate
end function state_integer

! state_code --
!     Allocate and initialize State
!
! Arguments:
!     re_code        RE code element
!     out            RE state index
!     out1           RE state index
integer function state_code(code, out, out1)
    type(re_code_element) :: code
    integer               :: out
    integer               :: out1

    nstate = nstate + 1

    states(nstate)%lastlist = 0
    states(nstate)%code     = code%code
    states(nstate)%char     = code%char
    states(nstate)%out      = out
    states(nstate)%out1     = out1

    state_code = nstate
end function state_code

! frag --
!     Initialize Frag struct
!
! Arguments:
!     start         Start state
!     out           List of states
!
function frag( start, out )
    type(re_frag)             :: frag
    integer                   :: start
    type(re_ptrlist), pointer :: out

    frag%start =  start
    frag%out   => out
end function frag

! list1 --
!     Create singleton list containing just outp
!
! Arguments:
!     plist       New list
!     outp        State to be stored
!
subroutine list1( plist, outp )
    type(re_ptrlist), pointer :: plist
    integer                   :: outp

    nplist =  nplist + 1
    write(*,*) nplist, outp
    write(*,*) allocated(plists)
    write(*,*) size(plists)
    plist  => plists(nplist)
    plist%start =  outp
    plist%next  => null()
end subroutine list1

! patch --
!     Patch the list of states at out to point to start
!
! Arguments:
!     plist       List of states
!     state       State to point to
!
subroutine patch( plist, state )
    type(re_ptrlist), target           :: plist
    integer                            :: state

    integer                            :: newsize
    type(re_ptrlist), pointer          :: pnext

    pnext => plist
    do while ( associated(pnext) )
        pnext%start =  state
        pnext       => pnext%next
    enddo
end subroutine patch

! append --
!     Join the two lists l1 and l2, returning the combination
!
! Arguments:
!     plist1         First list
!     plist2         Second list
!
! Note:
!     The second list is added to the first
!
subroutine append( plist1, plist2 )
    type(re_ptrlist), pointer          :: plist1
    type(re_ptrlist), pointer          :: plist2

    type(re_ptrlist), pointer          :: pnext
    integer                            :: newsize

    pnext => plist1
    do while ( associated(pnext%next) )
        pnext       => pnext%next
    enddo

    pnext%next => plist2
end subroutine append

! post2nfa --
!     Convert postfix regular expression to NFA
!     Return start state
!
! Arguments:
!     postfix         RE converted into postfix notation
!     start           Start state
!
subroutine post2nfa( postfix, start )
    type(re_code_element), dimension(:) :: postfix
    integer                             :: start

    type(re_frag), dimension(1000)      :: stack
    integer                             :: stackp
    integer                             :: i

    type(re_ptrlist), pointer           :: plist
    type(re_frag)                       :: e1, e2, e
    integer                             :: s

    if ( size(postfix) == 0 ) then
        start = -1
        return
    endif

    stackp = 1

    do i = 1,size(postfix)
        write(*,*) i, ' -- ', postfix(i)%char
        select case (postfix(i)%code )
            case default
                s = state( postfix(i)%code, 0, 0)
                write(*,*) 'state: ', s
                call list1( plist, states(s)%out )
                write(*,*) 'list '
                call push( frag(s, plist) )
                write(*,*) 'push '

            case (CONCAT)  ! Concatenate two states
                e2 = pop()
                e1 = pop()
                call patch( e1%out, e2%start )
                call push( frag(e1%start, e2%out) )

            case (ALTERN)  ! Alternate
                e2 = pop()
                e1 = pop()
                s = state( SPLIT, e1%start, e2%start )
                call append( e1%out, e2%out )
                call push( frag(s, e1%out) )

            case (ZERO_ONE)  ! Zero or one
                e = pop()
                s = state(SPLIT, e%start, 0)
                call list1( plist, states(s)%out1 )
                call append( e%out, plist )
                call push( frag(s, e%out) )

            case (ZERO_MORE)  ! Zero or more
                e = pop()
                s = state(SPLIT, e%start, 0)
                call patch( e%out, s )
                call list1( plist, states(s)%out1 )
                call push(frag(s, plist) )

            case (ONE_MORE)   ! One or more
                e = pop()
                s = state(SPLIT, e%start, 0)
                call patch( e%out, s )
                call list1( plist, states(s)%out1 )
                call push(frag(e%start, plist) )
        end select
    enddo

    e = pop()
    if ( stackp /= 0 ) then
        start = -1
    else
        call patch( e%out, matchstate )
        start = e%start
    endif

    write(*,*) 'post2nfa done'
contains

! push --
!     Auxiliary routine to push a new state on the stack
!
subroutine push( frag )
    type(re_frag) :: frag

    stack(stackp) = frag
    stackp        = stackp + 1
end subroutine push

! pop --
!     Auxiliary routine to pop the top state from the stack
!
function pop()
    type(re_frag) :: pop

    stackp = stackp - 1
    pop = stack(stackp)
end function pop
end subroutine post2nfa

! startlist --
!     Compute initial state list
!
! Arguments:
!     list          List to be filled
!     start         Start state
!
subroutine startlist( list, start )
    type(re_list) :: list
    integer       :: start

    list%n = 0
    listid = listid + 1
    call addstate( list, start )
end subroutine startlist

! ismatch --
!     Check whether state list contains a match
!
! Arguments:
!     list       List of current states
!
! Returns:
!     true if the list does contain the special state "matchstate"
!
logical function ismatch( list )
    type(re_list) :: list

    integer i

    ismatch = any( list%state_idx == matchstate )
end function ismatch

! addstate --
!     Add s to l, following unlabeled arrows
!
! Arguments:
!    list        List of state indices
!    s_idx       State index to be added
!
recursive subroutine addstate( list, s_idx)
    type(re_list) :: list
    integer       :: s_idx

    if ( s_idx == 0 .or. states(s_idx)%lastlist == listid ) then
        return
    endif

    states(s_idx)%lastlist = listid

    if ( states(s_idx)%code == SPLIT ) then
        !
        ! Follow unlabeled arrows
        !
        call addstate( list, states(s_idx)%out  )
        call addstate( list, states(s_idx)%out1 )
        return
    endif

    list%state_idx(list%n) = s_idx
    list%n                 = list%n + 1
end subroutine addstate

! step --
!     Step the NFA from the states in clist past the character c,
!     to create next NFA state set nlist.
!
! Arguments:
!     clist         List of current states
!     c             Character to match
!     nlist         List of next states
!
subroutine step( clist, c, nlist)
    type(re_list)  :: clist
    character      :: c
    type(re_list)  :: nlist

    integer        :: i

    type(re_state) :: s

    listid = listid + 1
    nlist%n = 0

    do i = 1, clist%n
        write(*,*) clist%state_idx(i)
        s = states(clist%state_idx(i))

        ! TODO: Here we need to add support for "." and character classes

        write(*,*) s%char, s%code
        if ( s%char == c ) then
            call addstate( nlist, s%out)
        endif
    enddo
end subroutine step

! match_full_string --
!     Run the NFA to determine whether it matches the full string
!
! Arguments:
!     start        Start state of the NFA (index)
!     string       String to be examined
!
! Returns:
!     true if the entire string matches, false otherwise
!
logical function match_full_string( start, string )
    integer          :: start
    character(len=*) :: string

    integer          :: i
    logical          :: use_clist

    call startlist( clist, start )
    use_clist = .true.

    do i = 1,len(string)
        write(*,*) 'Character: ', string(i:i)
        if ( use_clist ) then
            call step( clist, string(i:i), nlist )
        else
            call step( nlist, string(i:i), clist )
        endif
        use_clist = .not. use_clist
    enddo
    match_full_string = ismatch(clist)
end function match_full_string

! match_full --
!     Wrapper function to fully match a string
!
! Arguments:
!     re             Regular expression
!     string         String to match
!
! Returns:
!     true, if the string match the regular expression, false otherwise
!     (also if the regular expression contains an error)
!
logical function match_full( re, string )
    character(len=*) :: re
    character(len=*) :: string

    type(re_code_element), dimension(2*len(re))         :: re_code
    type(re_state),        dimension(2*len(re)), target :: new_states
    integer                                             :: n_code
    integer                                             :: start

    character(len=2*len(string)) :: check
    logical                      :: error

    match_full =  .false.

    call re2post( re, re_code, n_code, error, check )
    if ( error ) then
        return
    endif

    if ( allocated(states) ) then
        deallocate( states )
    endif
    allocate( states(1+n_code) )

    if ( allocated(plists) ) then
        deallocate( plists )
    endif
    allocate( plists(n_code) )

    if ( allocated(clist%state_idx) ) then
        deallocate( clist%state_idx )
        deallocate( nlist%state_idx )
    endif
    allocate( clist%state_idx(1+nstate) )
    allocate( nlist%state_idx(1+nstate) )

    states(1)%code = MATCH
    nstate         = 1
    nplist         = 0

    call post2nfa( re_code(1:n_code), start )

    match_full = match_full_string( start, string )

    deallocate( states )
    deallocate( plists )
    deallocate( clist%state_idx )
    deallocate( nlist%state_idx )
end function match_full

end module regexp

program test_regexp
    use regexp
    implicit none

    character(len=40)                    :: re, string
    type(re_code_element), dimension(40) :: re_code
    logical                              :: error
    logical                              :: match
    integer                              :: n_code

    re = 'a(b|c)+d'
    call re2post( trim(re), re_code, n_code, error, string )
    write(*,*) trim(string), ' -- ', error

    re = 'abcd'
    call re2post( trim(re), re_code, n_code, error, string )
    write(*,*) trim(string), ' -- ', error

    re = 'a+bcd'
    call re2post( trim(re), re_code, n_code, error, string )
    write(*,*) trim(string), ' -- ', error

    re = '(a|b)cd'
    call re2post( trim(re), re_code, n_code, error, string )
    write(*,*) trim(string), ' -- ', error

    re = 'a|bcd'
    call re2post( trim(re), re_code, n_code, error, string )
    write(*,*) trim(string), ' -- ', error

    !
    ! Tests for REs
    re = 'abcd'
    string = 'abcd'
    match  = match_full( trim(re), trim(string) )
    write(*,*) trim(re), ' -- ', trim(string), ' -- ', match

    re = 'abcd'
    string = 'aabcd'
    match  = match_full( trim(re), trim(string) )
    write(*,*) trim(re), ' -- ', trim(string), ' -- ', match

end program test_regexp
