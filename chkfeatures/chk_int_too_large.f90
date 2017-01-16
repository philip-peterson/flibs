! chk_int_too_large.f90 --
!     Check: does the compiler accept integer values that exceed the limit?
!
program chk_int_too_large
    implicit none

    integer :: x

    x = 4000000000

    write( *, '(a)'    ) 'Variable was set to 4*10**9'
    write( *, '(a,i0)' ) 'Actual value for the variable:             ', x
    write( *, '(a,i0)' ) 'Maximum value for default integers (HUGE): ', huge(x)

    write( *, '(/,a)'  ) 'The compiler accepts such large values'
end program chk_int_too_large
