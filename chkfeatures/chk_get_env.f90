! chk_get_env.f90
!     Check: does the compiler support the GET_ENVIRONMENT_VARIABLE subroutine?
!
program chk_get_env
    implicit none

    character(len=1)   :: dummy
    character(len=100) :: username
    integer            :: length

    call get_environment_variable( "USER", username, length )
    if ( length == 0 ) then
        call get_environment_variable( "USERNAME", username, length )
    endif

    if ( length > 0 ) then
        write( *, '(a)' ) 'User name: ', trim(username)
    else
        write( *, '(a)' ) 'Could not determine the user name - neither "USER" nor "USERNAME" exist'
    endif
end program chk_get_env
