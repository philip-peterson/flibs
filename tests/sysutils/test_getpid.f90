! getpid --
!     Print the process-ID of the process
!
program test_getpid
    use sysutils
    implicit none

    integer :: pid

    call get_process_id( pid )
    write(*,*) 'Process ID = ', pid
end program test_getpid
