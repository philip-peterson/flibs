! chk_i0_format --
!     Check: does the compiler support the I0 format for reading?
!
!     Note: this is an extension or an oversight (possibly the format
!     is interpreted just as In, "read n digits or blanks"
!
!     Might require a new category
!
program chk_i0_format
    implicit none

    integer :: ierr
    integer :: x
    character(len=20) :: string = '123'

    write( *, '(a)' ) 'Reading integer value via I0 format'

    x = -1
    read( string, '(i0)', iostat = ierr ) x

    if ( ierr == 0 ) then
        write( *, '(a,i5,2a)' ) '    Integer value read with no problems: ', x, ' - ', &
           merge( 'expected value', 'wrong value   ', x == 123 )
    else
        write( *, '(a,i5)' ) '    An error occurred - I0 format probably not supported for input - iostat = ', ierr
    endif

    write( *, '(a)' ) 'Reading integer value via I0 format with leading blanks'

    string = '    123'

    x = -1
    read( string, '(i0)', iostat = ierr ) x

    if ( ierr == 0 ) then
        write( *, '(a,i5,2a)', iostat = ierr  ) '    Integer value read with no problems: ', x, ' - ', &
           merge( 'expected value', 'wrong value   ', x == 123 )
    else
        write( *, '(a,i5)' ) '    An error occurred - leading blanks probably not allowed - iostat = ', ierr
    endif
end program chk_i0_format
