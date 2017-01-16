! tokenlist.f90 --
!    Module for holding a list of tokens
!
! Note:
! This module builds on the tokenize module. It simply provides
! a more convenient interface.
!
! $Id: tokenlist.f90,v 1.1 2012/11/04 12:57:06 arjenmarkus Exp $
!

! tokenlists --
!    Module for holding tokenized strings
!
module tokenlists
    use tokenize

    implicit none

    private
    !
    !  Define some convenient parameters:
    !  whitespace - blanks only (gaps)
    !  tsv        - tabs (typically as separators)
    !  csv        - commas (typically as separators)
    !  quotes     - " and ' (typically as delimiters)
    !  empty      - no separators of this type
    !
    public :: token_whitespace
    public :: token_tsv
    public :: token_csv
    public :: token_quotes
    public :: token_empty

    !
    ! Data structure defining the tokenlist
    !
    type tokenlist
        logical                                     :: initialized = .false.
        type(tokenizer)                             :: tokenizer_data
        character(len=1), dimension(:), allocatable :: char
        integer, dimension(:), allocatable          :: first
        integer, dimension(:), allocatable          :: length_token
    contains
        procedure                                   :: set_tokenizer => set_tokenizer_data
        procedure                                   :: tokenize      => tokenize_string
        procedure                                   :: number        => number_tokens
        procedure                                   :: length        => length_of_token
        procedure                                   :: token         => get_token
    end type tokenlist

    !
    ! Other public items
    !
    public :: tokenlist

contains

! set_tokenizer_data
!    Initialize the tokenlist with a proper tokenizer
!
! Arguments:
!    list           The tokenlist
!    gaps           Which characters form a gap
!    separators     Which characters are separators
!    delimiters     Which characters delimit tokens
!
subroutine set_tokenizer_data( list, gaps, separators, delimiters )
    class(tokenlist), intent(inout) :: list
    character(len=*), intent(in)   :: gaps
    character(len=*), intent(in)   :: separators
    character(len=*), intent(in)   :: delimiters

    list%initialized = .true.
    call set_tokenizer( list%tokenizer_data, gaps, separators, delimiters )

end subroutine set_tokenizer_data

! tokenize_string
!    Construct a list of tokens from a string
!
! Arguments:
!    list           The tokenlist
!    string         String to tokenize
!
subroutine tokenize_string( list, string )
    class(tokenlist), intent(inout)     :: list
    character(len=*), intent(in)       :: string

    integer                            :: i
    integer                            :: idx
    integer                            :: length_token
    integer, dimension(:), allocatable :: first
    integer, dimension(:), allocatable :: length
    character(len=len(string))          :: token

    !
    ! First store the string and then determine the tokens
    !
    if ( allocated(list%char) ) then
        deallocate( list%char         )
        deallocate( list%first        )
        deallocate( list%length_token )
    endif
    allocate( list%char(len(string)) )

    do i = 1,len(string)
        list%char(i) = string(i:i)
    enddo

    allocate( first(len(string)), length(len(string)) )

    !
    ! If we do not have a tokenizer yet, use a default
    !
    if ( .not. list%initialized ) then
        list%initialized = .true.
        call list%set_tokenizer( token_whitespace, token_empty, token_quotes )
    endif

    !
    ! Now tokenize the string
    !
    idx   = 1
    token = first_token( list%tokenizer_data, string, length_token )
    do while ( length_token > -1 )
        first(idx)  = list%tokenizer_data%start_token
        length(idx) = length_token

        token = next_token( list%tokenizer_data, string, length_token )
        idx   = idx + 1
    enddo

    allocate( list%first(0:idx), list%length_token(0:idx) )

    list%first(0)              = 0
    list%first(idx)            = 0
    list%length_token(0)       = 0
    list%length_token(idx)     = 0
    list%first(1:idx-1)        = first(1:idx-1)
    list%length_token(1:idx-1) = length(1:idx-1)

    deallocate( first, length )

end subroutine tokenize_string

! number_tokens
!    Return the number of tokens
!
! Arguments:
!    list           The tokenlist
!
integer function number_tokens( list )
    class(tokenlist), intent(in) :: list

    number_tokens = size(list%length_token) - 2

end function number_tokens

! length_token
!    Return the length of a token
!
! Arguments:
!    list           The tokenlist
!    idx            Index of the token
!
integer function length_of_token( list, idx )
    class(tokenlist), intent(in) :: list
    integer, intent(in)         :: idx

    if ( idx <= 0 .or. idx >= size(list%length_token)-1 ) then
        length_of_token = 0
    else
        length_of_token = list%length_token(idx)
    endif
end function length_of_token

! get_token
!    Return the nth token (or zero-length string)
!
! Arguments:
!    list           The tokenlist
!    idx            Index of the token
!
function get_token( list, idx )
    class(tokenlist), intent(in) :: list
    integer, intent(in)         :: idx
    character(len=(list%length_token(max(0,min(size(list%length_token)-2,idx))))) :: get_token

    integer :: i, c

    if ( idx <= 0 .or. idx >= size(list%length_token)-1 ) then
        get_token = ''
    else
        do i = 1,list%length_token(idx)
            c = list%first(idx) + i - 1
            get_token(i:i) = list%char(c)
        enddo
    endif
end function get_token

end module tokenlists
