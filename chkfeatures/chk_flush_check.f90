! chk_flush_check --
!     Check: does the compiler support the flush statement?
!
!     Note:
!     To check that the flushing is actually working, the program
!     calls itself. For this it relies on execute_command_line and
!     get_command_argument
!
program chk_flush_check
    implicit none

    integer             :: ierr
    character(len=100)  :: cmd
    real, dimension(10) :: x

    if ( command_argument_count() == 0 ) then
        call random_number( x )

        open( 10, file = 'chk_flush_check.out' )
        write( 10, * ) x
        flush( 10 )

        call get_command_argument( 0, cmd )
        write(*,*) trim(cmd)

        cmd = trim(cmd) // ' 1'
        call execute_command_line( cmd )
    else
        open( 20, file = 'chk_flush_check.out', action = 'read' )
        read( 20, *, iostat = ierr ) x

        if ( ierr == 0 ) then
            write( *, '(a)' ) 'Flushing appears to work as expected'
        else
            write( *, '(a)' ) 'Error reading the file chk_flush_check.out - flushing appears NOT to work correctly'
        endif
    endif

end program chk_flush_check
