! chk_gamma.f90 --
!     Check if the compiler support the new Gamma and log Gamma functions
!
!     Note: there is a whole list of such special functions defined for the Fortran 2008 standard, not
!     all of them are tested here
!
program chk_gamma
    implicit none

    real, dimension(11) :: x = (/ -2.5, -1.5, -0.5, 0.1, 0.2, 0.5, 1.0, 2.0, 3.0, 4.0, 6.0 /)
    integer             :: i, n

    write( *, '(a)' ) 'Selected values for the Gamma and log Gamma functions:'

    write( *, '(5a12)' ) 'x', 'Gamma', 'log Gamma'
    do i = 1,size(x)
        write( *, '(3f12.4)' ) x(i), gamma(x(i)), log_gamma(x(i))
    enddo
end program chk_gamma
