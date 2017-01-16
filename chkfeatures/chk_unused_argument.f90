! chk_unused_argument.f90
!     Check: does the compiler report unused variables?
!
!     NOte: most probably just a warning
!
program chk_unused_argument
    implicit none

    integer :: not_used, used

    call sub_not_using_argument( not_used, used )

contains
subroutine sub_not_using_argument( arg, second_arg )
    integer :: arg, second_arg

    second_arg = 2

    write( *, '(a)' ) 'If the compiler issues a warning about "arg", it report unused arguments'
end subroutine sub_not_using_argument
end program chk_unused_argument
