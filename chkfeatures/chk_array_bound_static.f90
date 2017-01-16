! chk_array_bound_static --
!     Check: does the compiler check array bounds statically?
!
program chk_array_bound_static
    implicit none

    real, dimension(10) :: x

    x(20) = 1.0

    write( *, '(a)' ) 'Array bounds are NOT checked statically or merely'
    write( *, '(a)' ) 'give rise to warnings'

end program chk_array_bound_static
