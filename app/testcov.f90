! testcov.f90 --
!     Program to instrument Fortran code (free form) so that the
!     test coverage can be determined and reported.
!
!     Note:
!     The type of coverage this program provides is "branch coverage",
!     a fairly basic form of test coverage.
!
program testcov
    use testcov_reg

    implicit none

    character(len=200) :: line
    character(len=200) :: inname
    character(len=200) :: outname

    integer            :: ierr
    logical            :: report
    logical            :: exists

    !
    ! Statement types
    !
    integer, parameter :: is_comment            = 1
    integer, parameter :: is_declaration        = 2
    integer, parameter :: is_other              = 3
    integer, parameter :: is_endsubprogram      = 4
    integer, parameter :: is_endprogram         = 5
    integer, parameter :: is_subprogram         = 6
    integer, parameter :: is_program            = 7
    integer, parameter :: if_statement          = 8
    integer, parameter :: else_statement        = 9
    integer, parameter :: endif_statement       = 10
    integer, parameter :: do_statement          = 11
    integer, parameter :: cycle_statement       = 12
    integer, parameter :: exit_statement        = 13
    integer, parameter :: enddo_statement       = 14
    integer, parameter :: select_statement      = 15
    integer, parameter :: case_statement        = 16
    integer, parameter :: casedefault_statement = 17
    integer, parameter :: endselect_statement   = 18
    integer, parameter :: goto_statement        = 19
    integer, parameter :: continue_statement    = 20


    open( 10, file = 'testcov.inp', status = 'old' )

    report = .false.

    do
        read( 10, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) then
            exit
        endif

        line = adjustl(line)

        if ( line(1:1) == '!' ) then
            cycle
        endif

        if ( line == 'MODE=report' ) then
            call testcov_load
            report = .true.
            cycle
        endif
        if ( line == 'MODE=instrument' ) then
            report = .false.
            cycle
        endif

        !
        ! Any other line is considered the name of a source file
        !
        inname = line
        inquire( file = inname, exist = exists )
        if ( .not. exists ) then
            write(*,*) 'File "'// trim(inname) //'" does not exist - skipping'
            cycle
        endif

        write(*,*) 'Processing "'// trim(inname) //'"'

        call make_outname( inname, outname, report )

        open( 20, file = inname )
        open( 21, file = outname )

        call process_file( report )

        close( 20 )
        close( 21 )
    enddo

    write(*,*) 'Done'

contains

! make_outname --
!     Determine the name of the output file
!
! Arguments:
!     inname         Name of the input file
!     outname        Name of the output file
!     report         Report coverage or not (instrument instead)
!
! Result:
!     If the input is "mysource.f90" then the result is either
!     "_mysource.f90" (for instrumenting) or "mysource.lst" (for
!     reporting)
!
subroutine make_outname( inname, outname, report )
    character( len=*), intent(in)  :: inname
    character( len=*), intent(out) :: outname
    logical, intent(in)            :: report

    integer                        :: k
    character(len=2)               :: slash = '/\\' ! Some compilers see \ as an escape character ...

    if ( report ) then
        k = index( inname, '.', .true. )
        if ( k == 0 ) then
            k = len_trim(line) ! Highly unlikely ..
        endif

        outname = inname(1:k) // 'lst'
    else
        k = index( inname, slash(1:1), .true. )
        if ( k == 0 ) then
            k = index( inname, slash(2:2), .true. )
        endif
        outname = inname(1:k-1) // '_' // inname(k+1:)
    endif
end subroutine make_outname

! process_file --
!     Read the source code and produce an instrumented file or a
!     coverage report
!
! Arguments:
!     report         Report coverage or not (instrument instead)
!
subroutine process_file( report )
    logical, intent(in)            :: report

    integer                        :: ierr
    integer                        :: k
    character(len=2)               :: slash = '/\\' ! Some compilers see \ as an escape character ...
    character(len=200), dimension(40) :: line

    logical                        :: start_sub
    logical                        :: start_program
    integer                        :: lineno
    integer                        :: codetype
    integer                        :: nolines
    integer                        :: i

    lineno        = 0
    start_sub     = .false.
    start_program = .true.    ! The case of a missing program statement!
    do
        !
        ! TODO: continuation lines!
        !
        call read_line( line, nolines, ierr )
        if ( ierr /= 0 ) then
            exit
        endif

        lineno = lineno + nolines

        call classify_line( line(1), codetype )
       !write( 21, '(i5,a)' ) codetype, trim(line(1))

        select case( codetype )
            case ( is_comment, is_declaration )
                if ( report ) then
                    write( 21, '(10x,a)' ) (trim(line(i)) ,i=1,nolines)
                else
                    write( 21, '(a)' ) (trim(line(i)) ,i=1,nolines)
                endif
                cycle

            case ( is_subprogram, is_program )
                if ( codetype == is_program ) then
                    start_program = .true.
                endif
                start_sub = .true.
                if ( report ) then
                    write( 21, '(10x,a)' ) (trim(line(i)) ,i=1,nolines)
                else
                    write( 21, '(a)' ) (trim(line(i)) ,i=1,nolines)
                endif
                cycle

            case ( is_endprogram, is_endsubprogram )
                if ( start_program ) then
                    start_program = .false.
                    call handle_start( line, lineno, nolines, codetype )
                else
                    if ( report ) then
                        write( 21, '(10x,a)' ) (trim(line(i)) ,i=1,nolines)
                    else
                        write( 21, '(a)' ) (trim(line(i)) ,i=1,nolines)
                    endif
                endif
                cycle

            case ( is_other )
                if ( start_sub ) then
                    start_sub = .false.
                    call handle_start( line, lineno, nolines, codetype )
                else
                    if ( report ) then
                        write( 21, '(10x,a)' ) (trim(line(i)) ,i=1,nolines)
                    else
                        write( 21, '(a)' ) (trim(line(i)) ,i=1,nolines)
                    endif
                endif
                cycle

            case ( if_statement, else_statement, endif_statement )
                if ( start_sub ) then
                    start_sub = .false.
                    call handle_start( line, lineno, nolines, codetype )
                endif
                call handle_if( line, lineno, nolines, codetype )

            case ( do_statement, cycle_statement, exit_statement, enddo_statement )
                if ( start_sub ) then
                    start_sub = .false.
                    call handle_start( line, lineno, nolines, codetype )
                endif
                call handle_do( line, lineno, nolines, codetype )

            case ( select_statement, case_statement, casedefault_statement, endselect_statement )
                if ( start_sub ) then
                    start_sub = .false.
                    call handle_start( line, lineno, nolines, codetype )
                endif
                call handle_case( line, lineno, nolines, codetype )

            case ( goto_statement, continue_statement )
                if ( start_sub ) then
                    start_sub = .false.
                    call handle_start( line, lineno, nolines, codetype )
                endif
!               call handle_goto( line, lineno, codetype )

            case default
                write(*,*) 'Programming error: unknown statement type!'
                write(*,*) 'Line: ', trim(line(1))
                write( 21, '(a)' ) (trim(line(i)) ,i=1,nolines)

        end select
    enddo
end subroutine process_file

! tolower --
!     Convert a string to lower case
!
! Arguments:
!     string         String to be converted
!
! Result:
!     String in lowercase
!
function tolower( string )
    character(len=*), intent(in) :: string
    character(len=len(string))   :: tolower

    character(len=26), parameter :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=26), parameter :: lower = 'abcdefghijklmnopqrstuvwxyz'

    integer                    :: i
    integer                    :: k
    integer                    :: length

    length  = len(string)
    tolower = string
    do i = 1,length
        k = index( upper, string(i:i) )
        if ( k > 0 ) then
           tolower(i:i) = lower(k:k)
        endif
    enddo
end function tolower

! is_digit --
!     Is a character a digit or not
!
! Arguments:
!     char           Character to be examined
!
! Result:
!     True if digit, false otherwise
!
logical function is_digit( char )
    character(len=1), intent(in) :: char

    is_digit = index( '01234567890', char ) > 0

end function is_digit

! is_letter --
!     Is a character a (lower/uppercase) letter or not
!
! Arguments:
!     char           Character to be examined
!
! Result:
!     True if letter, false otherwise
!
logical function is_letter( char )
    character(len=1), intent(in) :: char

    character(len=52), parameter :: letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'

    is_letter = index( letters, char ) > 0

end function is_letter

! adjust_line --
!     Convert to lower case and remove superfluous spaces and tabs
!
! Arguments:
!     line           Line to be adjusted
!
subroutine adjust_line( line )
    character(len=*), intent(inout)  :: line

    integer                          :: i
    integer                          :: j

    line = adjustl( tolower(line) )

    do i =1,len_trim(line)
        if ( line(i:i) == char(9) ) then
            line(i:i) = ' '
        endif
    enddo

    j = 0
    do i = 1,len_trim(line)
        if ( line(i:i) /= ' ' ) then
            j = j + 1
            line(j:j) = line(i:i)
        elseif ( line(i-1:i-1) /= ' ' ) then
            j = j + 1
            line(j:j) = line(i:i)
        endif
    enddo

    line(j+1:) = ' '

end subroutine adjust_line

! classify_line --
!     Determine the class of the line of code
!
! Arguments:
!     line           Line of code to examine
!     codetype       Class to which it belongs
!
subroutine classify_line( line, codetype )
    character(len=*), intent(in)    :: line
    integer, intent(out)            :: codetype

    character(len=200)              :: adjusted_line
    character(len=200)              :: next_piece

    integer                         :: i
    integer                         :: idxkey
    integer                         :: k
    logical                         :: found
    character(len=1)                :: type_next
    character(len=20)               :: keyword

    integer, dimension(10)          :: subprogram_type = &
    (/ is_endsubprogram, is_endprogram, &
       is_endprogram, is_endsubprogram, &
       is_endsubprogram, is_endsubprogram, &
       is_endsubprogram, &
       is_program, is_subprogram, is_subprogram /)
    character(len=20), dimension(10) :: subprogram_keyword = &
    (/ 'contains', 'end program', 'endprogram', 'end subroutine', &
       'endsubroutine', 'end function', 'endfunction', &
       'program', 'subroutine', 'function' /)

    ! Note: "end" is treated here, as we need to check the entire line

    integer, dimension(15)           :: control_type = &
    (/ do_statement, enddo_statement, enddo_statement, &
       if_statement, else_statement, if_statement, if_statement, &
       endif_statement, endif_statement, &
       select_statement, endselect_statement, endselect_statement, &
       casedefault_statement, case_statement, is_endsubprogram /)
    character(len=20), dimension(15) :: control_keyword = &
    (/ '$do', '%enddo', '%end do', '#if', '@else', '#elseif', '#else if', &
       '%endif', '%end if', '#select case', '%endselect', '%end select',  &
       '@case default', '#case', '@end' /)

    character(len=20), dimension(71) :: declare_keyword = &
    (/ '#real::',     '#real ::',     '#real,',     '#real ,',     '#real(',     '#real (',     '%real',     &
       '#integer::',  '#integer ::',  '#integer,',  '#integer ,',  '#integer(',  '#integer (',  '%integer',  &
       '#logical::',  '#logical ::',  '#logical,',  '#logical ,',  '#logical(',  '#logical (',  '%logical',  &
       '#complex::',  '#complex ::',  '#complex,',  '#complex ,',  '#complex(',  '#complex (',  '%complex',  &
       '#character::','#character ::','#character,','#character ,','#character(','#character (','%character',&
       '#double precision::', '#double precision,', '#double precision ,', '%double precision',              &
       '#type(', '#type (', '%type', '%end type', '%endtype',                                                &
       '#implicit none', '%implicit',                                                                        &
       '%use', '#data (', '%data',                                                                           &
       '#parameter::', '#parameter ::', '#parameter,', '#parameter ,', '#parameter(', '#parameter (',        &
       '#save::',      '#save ::',      '%save',                                                             &
       '#private::',   '#private ::',   '%private',                                                          &
       '#public::',    '#public ::',    '%public',                                                           &
       '%interface',   '%end interface','%endinterface',                                                     &
       '#external::',  '#external ::',  '%external',                                                         &
       '%module procedure'                                                                                   &
    /)
    ! target, pointer, allocatable, module ...
    ! Oh, and of course: subroutine, function inside interface block

    !
    ! Massage the line we read: lower case characters and no multiple spaces
    !
    codetype = is_other
    adjusted_line = line
    call adjust_line( adjusted_line )
    found = .false.

    !
    ! Comments
    !
    if ( adjusted_line(1:1) == '!' .or. adjusted_line == ' ' ) then
        codetype = is_comment
        return
    endif

    !
    ! Program, subroutine or function
    !
    do i = 1,size(subprogram_keyword)
        k = index( adjusted_line, trim(subprogram_keyword(i)) // ' ' )
        if ( k > 0 ) then
            k = k + len_trim(subprogram_keyword(i))
            next_piece = adjustl( adjusted_line(k:) )
            if ( is_letter(next_piece(1:1)) .or. next_piece(1:1) == ' ') then
                codetype = subprogram_type(i)
                found = .true.
                exit
            endif
        endif
    enddo

    if ( found ) return

    !
    ! Do, if, case ...
    !
    do i = 1,size(control_keyword)
        type_next = control_keyword(i)(1:1)
        keyword   = control_keyword(i)(2:)
        idxkey    = i

        k = index( adjusted_line, trim(keyword) )
        if ( k > 0 ) then
            select case ( type_next )
                case ( '@' )
                    if ( adjusted_line == keyword ) then
                        found = .true.
                        exit
                    endif
                case ( '$' )
                    k = k + len_trim(keyword) + 1
                    if ( adjusted_line(k-1:k-1) == ' ' .and. &
                         ( is_letter(adjusted_line(k:k)) .or. &
                           is_digit(adjusted_line(k:k))  .or. &
                           adjusted_line == keyword ) )       then
                        found = .true.
                        exit
                    endif
                case ( '#' )
                    k = k + len_trim(keyword)
                    if ( adjusted_line(k:k)     == '(' .or. &
                         adjusted_line(k+1:k+1) == '('      ) then
                        found = .true.
                        exit
                    endif
                case ( '%' )
                    k = k + len_trim(keyword)
                    if ( adjusted_line(k:k) == ' ' ) then
                        found = .true.
                        exit
                    endif
            end select
        endif
    enddo

    if ( found ) then
        codetype = control_type(idxkey)
        return
    endif

    !
    ! Declarations ...
    !
    do i = 1,size(declare_keyword)
        type_next = declare_keyword(i)(1:1)
        keyword   = declare_keyword(i)(2:)
        idxkey    = i

        k = index( adjusted_line, trim(keyword) )
        if ( k == 1 ) then
            select case ( type_next )
                case ( '#' )
                    k = k + len_trim(keyword) + 1
                    if ( is_letter(adjusted_line(k:k)) .or. &
                         adjusted_line(k:k) == '_' ) then
                        found = .true.
                        exit
                    endif
                case ( '%' )
                    k = k + len_trim(keyword) + 2
                    if ( is_letter(adjusted_line(k:k)) .or. &
                         adjusted_line(k:k) == '_' ) then
                        found = .true.
                        exit
                    endif
            end select
        endif
    enddo

    if ( found ) then
        codetype = is_declaration
        return
    endif

end subroutine classify_line

! read_line --
!     Read the line and possible continuation lines
!
! Arguments:
!     line              Line of code
!     nolines           Number of lines
!     ierr              Error code (if any)
!
subroutine read_line( line, nolines, ierr )
    character(len=*), dimension(:), intent(inout) :: line
    integer, intent(out)                          :: nolines
    integer, intent(out)                          :: ierr

    integer                                       :: i
    integer                                       :: length

    nolines = size(line)
    do i = 1,size(line)
        read( 20, '(a)', iostat = ierr ) line(i)
        length = len_trim(line(i))
        if ( line(i)(length:length) /= '&' ) then
            nolines = i
            exit
        endif
    enddo

end subroutine read_line

! write_line --
!     Write the intrumentation code (low-level)
!
! Arguments:
!     line              Line of code
!     lineno            Line number
!     nolines           Number of lines
!     inname            Name of source file
!     code              What registration code to use
!
subroutine write_line( line, lineno, nolines, inname, code )
    character(len=*), dimension(:), intent(in) :: line
    integer, intent(in)                        :: lineno
    integer, intent(in)                        :: nolines
    character(len=*), intent(in)               :: inname
    integer, intent(in)                        :: code

    integer                                    :: i

    write( 21, '(a)' ) (trim(line(i)) ,i=1,nolines)
    write( 21, '(3a,i0,a,i0,a)' ) &
        '        call testcov_register__("', trim(inname), '",', &
        lineno, ',', code, ')'
end subroutine write_line

! handle_start --
!     Output for the start of the subprogram
!
! Arguments:
!     line           Line of code to instrument/report
!     lineno         Line number
!     nolines        Number of lines
!     codetype       Class to which it belongs
!
subroutine handle_start( line, lineno, nolines, codetype )
    character(len=*), dimension(:), intent(in)    :: line
    integer, intent(in)                           :: lineno
    integer, intent(in)                           :: nolines
    integer, intent(in)                           :: codetype

    integer                                       :: count
    integer                                       :: i

    if ( report ) then
        if ( codetype == is_endprogram .or. codetype == is_endsubprogram ) then
            write( 21, '(10x,a)' ) trim(line(1))
        else
            call testcov_get_count( inname, lineno, 1, count )
            write( 21, '(i8,a,a)' ) count, ': ', trim(line(1))
            if ( count == 0 ) then
                 write( 21, '(a8,a)'  ) '==> ', ': (subroutine/function never called)'
             endif
             if ( nolines > 1 ) then
                 write( 21, '(10x,a)'  ) ( trim(line(i)), i = 2,nolines )
             endif
         endif
    else
        !
        ! If the routine starts with an IF or DO or SELECT, then
        ! the number of entries will be counted anyway
        !
        if ( codetype == is_other ) then
            call write_line( (/ ' ' /), lineno, 1, inname, 1 )
        endif
        if ( codetype == is_endprogram .or. codetype == is_endsubprogram ) then
            write( 21, '(a)' ) '      call testcov_register__("",0,0)'
            write( 21, '(a)' ) ( trim(line(i)), i = 1,nolines )
        endif
    endif
end subroutine handle_start

! handle_do --
!     Output for the control statements of a do-loop
!
! Arguments:
!     line           Line of code to instrument/report
!     lineno         Line number
!     nolines        Number of lines
!     codetype       Class to which it belongs
!
! Note:
!     FORTRAN 77 style do-loops not treated correctedly!
!
subroutine handle_do( line, lineno, nolines, codetype )
    character(len=*), dimension(:), intent(in)    :: line
    integer, intent(in)                           :: lineno
    integer, intent(in)                           :: nolines
    integer, intent(in)                           :: codetype

    integer                                       :: i
    integer, dimension(100), save                 :: do_start = 0
    integer, save                                 :: do_level = 0
    integer                                       :: count
    integer                                       :: skipped

    if ( report ) then
        if ( codetype == do_statement ) then
            call testcov_get_count( inname, lineno, 1, count   )
            call testcov_get_count( inname, lineno, 2, skipped )
            write( 21, '(i8,a,a)' ) count, ': ', line(1)
            if ( count == 0 ) then
                write( 21, '(a8,a)'  ) '==> ', ': (do-loop never ran)'
            endif
            if ( skipped == 0 ) then
                write( 21, '(a8,a)'  ) '==> ', &
                    ': (do-loop never skipped, that is: always at least one iteration)'
            endif
            if ( nolines > 1 ) then
                write( 21, '(10x,a)'  ) ( line(i), i = 2,nolines )
            endif
        else
            write( 21, '(10x,a)'  ) ( line(i), i = 1,nolines )
        endif
    else
        if ( codetype == do_statement ) then
            do_level = do_level + 1
            do_start(do_level) = lineno
            call write_line( (/ ' ' /), lineno, 1, inname, 2 )
            call write_line( line, lineno, nolines, inname, 1 )
        else
            !
            ! TODO: should be written with lineno belonging to DO
            !
            call write_line( line, do_start(do_level), nolines, inname, 3 )
            do_level = do_level - 1

            if ( do_level < 0 ) then
                write(*,*) 'Error in handle_do: more enddo than dos!'
            endif
        endif
    endif
end subroutine handle_do

! handle_if --
!     Output for the control statements of an if-block/statement
!
! Arguments:
!     line           Line of code to instrument/report
!     lineno         Line number
!     nolines        Number of lines
!     codetype       Class to which it belongs
!
! Note:
!     No provision yet for an if-statement
!
subroutine handle_if( line, lineno, nolines, codetype )
    character(len=*), dimension(:), intent(in)    :: line
    integer, intent(in)                           :: lineno
    integer, intent(in)                           :: nolines
    integer, intent(in)                           :: codetype

    logical, dimension(100), save                 :: has_else = .false.
    integer, save                                 :: if_level = 0
    integer                                       :: count
    integer                                       :: i

    if ( report ) then
        select case (codetype)
            case ( if_statement )
                if_level = if_level + 1
                has_else(if_level) = .false.
                call testcov_get_count( inname, lineno, 1, count )
                write( 21, '(i8,a,a)' ) count, ': ', line(1)
                if ( count == 0 ) then
                    write( 21, '(a8,a)'  ) '==> ', ': (if/elseif condition was never true)'
                endif
                if ( nolines > 1 ) then
                    write( 21, '(10x,a)'  ) ( line(i), i = 2,nolines )
                endif

            case ( else_statement )
                has_else(if_level) = .true.
                call testcov_get_count( inname, lineno, 1, count )
                write( 21, '(i8,a,a)' ) count, ': ', line(1)
                if ( count == 0 ) then
                    write( 21, '(a8,a)'  ) '==> ', ': (else condition was never true)'
                endif
                if ( nolines > 1 ) then
                    write( 21, '(10x,a)'  ) ( line(i), i = 2,nolines )
                endif

            case ( endif_statement )
                write( 21, '(10x,a)'  ) ( line(i), i = 1,nolines )
                if ( .not. has_else(if_level) ) then
                    call testcov_get_count( inname, lineno, 2, count )
                    if ( count > 0 ) then
                        write( 21, '(i8,a,a)' ) count, ': (if condition false)'
                    else
                        write( 21, '(a8,a)' )   '==> ', ': (if condition always true ==> implicit else did not occur)'
                    endif
                endif
                if_level = if_level - 1

        end select
    else
        !
        ! TODO: if-statements instead of blocks
        !
        select case (codetype)
            case ( if_statement )
                if_level = if_level + 1
                has_else(if_level) = .false.
                call write_line( line, lineno, nolines, inname, 1 )

            case ( else_statement )
                has_else(if_level) = .true.
                call write_line( line, lineno, nolines, inname, 1 )

            case ( endif_statement )
                if ( .not. has_else(if_level) ) then
                    write( 21, '(a)' ) '        else'
                    call write_line( (/ ' ' /), lineno, 1, inname, 3 )
                endif
                call write_line( line, lineno, nolines, inname, 1 )
                if_level = if_level - 1

                if ( if_level < 0 ) then
                    write(*,*) 'Error in handle_if: more endifs than ifs!'
                endif

            case default
                write(*,*) 'Programming error in handle_if - unknown case'
        endselect
    endif
end subroutine handle_if

! handle_case --
!     Output for the control statements of a select-case block
!
! Arguments:
!     line           Line of code to instrument/report
!     lineno         Line number
!     nolines        Number of lines
!     codetype       Class to which it belongs
!
subroutine handle_case( line, lineno, nolines, codetype )
    character(len=*), dimension(:), intent(in)    :: line
    integer, intent(in)                           :: lineno
    integer, intent(in)                           :: nolines
    integer, intent(in)                           :: codetype

    logical, dimension(100), save                 :: has_default = .false.
    integer, save                                 :: select_level = 0
    integer                                       :: i
    integer                                       :: count

    if ( report ) then
        call testcov_get_count( inname, lineno, 1, count )
        select case (codetype)
            case ( select_statement )
                select_level = select_level + 1
                write( 21, '(10x,a)'  ) ( line(i), i = 1,nolines )

            case ( case_statement )
                write( 21, '(i8,a,a)' ) count, ': ', line(1)
                if ( count == 0 ) then
                    write( 21, '(a8,a)'  ) '==> ', ': (case never occurred)'
                endif
                if ( nolines > 1 ) then
                    write( 21, '(10x,a)'  ) ( line(i), i = 2,nolines )
                endif

            case ( casedefault_statement )
                has_default(select_level) = .true.
                write( 21, '(i8,a,a)' ) count, ': ', line(1)
                if ( count == 0 ) then
                    write( 21, '(a8,a)'  ) '==> ', ': (default case never occurred)'
                endif
                if ( nolines > 1 ) then
                    write( 21, '(10x,a)'  ) ( line(i), i = 2,nolines )
                endif

            case ( endselect_statement )
                call testcov_get_count( inname, lineno, 2, count )
                write( 21, '(i8,a,a)'  ) count, ': ', line(1)
                if ( nolines > 1 ) then
                    write( 21, '(10x,a)'  ) ( line(i), i = 2,nolines )
                endif
                if ( .not. has_default(select_level) ) then
                    if ( count == 0 ) then
                        write( 21, '(a8,a)'  ) '==> ', ': (implicit default case never occurred)'
                    endif
                endif
                select_level = select_level - 1

                if ( select_level < 0 ) then
                    write(21,*) 'Error in handle_case: more endselects than selects!'
                    write(*,*) 'Error in handle_case: more endselects than selects!'
                endif

            case default
                write(*,*) 'Programming error in handle_select - unknown case'
        endselect
    else
        !
        ! TODO: if-statements instead of blocks
        !
        select case (codetype)
            case ( select_statement )
                select_level = select_level + 1
                has_default(select_level) = .false.
                write( 21, '(a)' ) (trim(line(i)) ,i=1,nolines)

            case ( case_statement )
                call write_line( line, lineno, nolines, inname, 1 )

            case ( casedefault_statement )
                has_default(select_level) = .true.
                call write_line( line, lineno, nolines, inname, 1 )

            case ( endselect_statement )
                if ( .not. has_default(select_level) ) then
                    write( 21, '(a)' ) '        case default'
                    call write_line( line, lineno, nolines, inname, 3 )
                else
                    write( 21, '(a)' ) (trim(line(i)) ,i=1,nolines)
                endif
                select_level = select_level - 1

                if ( select_level < 0 ) then
                    write(21,*) 'Error in handle_case: more endselects than selects!'
                    write(*,*) 'Error in handle_case: more endselects than selects!'
                endif

            case default
                write(*,*) 'Programming error in handle_select - unknown case'
        endselect
    endif
end subroutine handle_case

end program
