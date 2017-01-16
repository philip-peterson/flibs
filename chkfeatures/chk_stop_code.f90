! chk_stop_code --
!     Check: does the compiler support a code with the STOP statement?
!
program chk_stop_code
    implicit none

    write( *, '(a)' ) 'Stopping with code "123"'

    stop 123

end program chk_stop_code
