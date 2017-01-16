! chk_erf.f90 --
!     Check if the compiler support the new error and error-complement functions (erf, erfc, erfc_scaled)
!
!     Note: there is a whole list of such special functions defined for the Fortran 2008 standard, not
!     all of them are tested here
!
program chk_erf
    implicit none

    real, dimension(9) :: x = (/ -5.0, -2.0, -1.0, -0.5, 0.0, 0.5, 1.0, 2.0, 5.0 /)
    integer            :: i, n

    write( *, '(a)' ) 'Selected values for the error function and its relatives:'

    write( *, '(5a12)' ) 'x', 'erf', 'erfc', 'erfc scaled'
    do i = 1,size(x)
        write( *, '(f12.4,3e12.4)' ) x(i), erf(x(i)), erfc(x(i)), erfc_scaled(x(i))
    enddo
end program chk_erf
