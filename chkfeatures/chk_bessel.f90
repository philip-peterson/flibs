! chk_bessel.f90 --
!     Check if the compiler support the new Bessel functions (J0, Y0, J1, Y1, Jn, Yn)
!
!     Note: there is a whole list of such special functions defined for the Fortran 2008 standard, not
!     all of them are tested here
!
program chk_bessel
    implicit none

    real, dimension(4) :: x = (/ 0.1, 0.2, 1.0, 2.0 /)
    integer            :: i, n

    write( *, '(a)' ) 'Selected values for some Bessel functions of the first and second kind:'

    write( *, '(5a12)' ) 'x', 'J0', 'Y0', 'J1', 'Y1'
    do i = 1,size(x)
        write( *, '(5f12.4)' ) x(i), bessel_j0(x(i)), bessel_y0(x(i)), bessel_j1(x(i)), bessel_y1(x(i))
    enddo

    write( *, '(5a12)' ) 'x', 'J2', 'Y2', 'J3', 'Y3'
    do i = 1,size(x)
        write( *, '(5f12.4)' ) x(i), bessel_jn(2,x(i)), bessel_yn(2,x(i)), bessel_jn(3,x(i)), bessel_yn(3,x(i))
    enddo
end program chk_bessel
