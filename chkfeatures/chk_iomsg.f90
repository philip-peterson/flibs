! chk_iomsg --
!     Check: does the compiler support the IOMSG= keyword?
!
program chk_iomsg
    implicit none

    character(len=20)  :: string = 'Just some text'
    character(len=200) :: msg
    integer            :: ierr
    integer            :: value

    msg  = '?'
    ierr = 0

    write( *, '(a)' )    'Reading a value from a string - causing a runtime error'

    read( string, *, iostat = ierr, iomsg = msg ) value

    write( *, '(a,i0)' ) '    IOSTAT = ', ierr
    write( *, '(a,a)' )  '    Message: ', trim(msg)

    if ( ierr == 0 ) then
        write( *, '(/,a)' ) '    The error code was NOT set apparently!'
    endif
end program chk_iomsg
