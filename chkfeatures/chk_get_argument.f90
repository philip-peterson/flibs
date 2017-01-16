! chk_get_argument --
!     Check: does the compiler support the get_command_argument routine?
!
program chk_get_argument
    implicit none

    integer             :: i, number
    character(len=100)  :: cmd

    number = command_argument_count()

    write( *, '(a,i0)' ) 'Number of command line arguments: ', number

    do i = 0,number
        call get_command_argument( i, cmd )
        write( *, '(i5,a,a)' ) i, ': ', cmd
    enddo

end program chk_get_argument
