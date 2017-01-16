! test_newuoa.f90 --
!
!     The Chebyquad test problem (Fletcher, 1965) for N = 2,4,6 and 8,
!     with NPT = 2N+1.
!
module newuoa_functions
    use newuoa_minimisation, only: wp

    implicit none

contains

subroutine calfun( x, f )
    real(kind=wp), dimension(:) :: x
    real(kind=wp)               :: f

    real(kind=wp), dimension(:) :: y(10,10)
    real(kind=wp)               :: sum
    integer                     :: i, j, n, np, iw

    n = size(x)
    do j = 1,n
        y(1,j) = 1.0d0
        y(2,j) = 2.0d0 * x(j) - 1.0d0
    enddo

    do i = 2,n
        do j = 1,n
            y(i+1,j) = 2.0d0 * y(2,j) * y(i,j) - y(i-1,j)
        enddo
    enddo

    f  = 0.0d0
    np = n + 1
    iw = 1

    do i = 1,np
        sum = 0.0d0
        do j = 1,n
            sum = sum + y(i,j)
        enddo
        sum = sum / real(n,wp)
        if ( iw > 0) sum = sum + 1.0d0 / real(i*i-2*i,wp)
        iw = -iw
        f  =  f + sum * sum
    enddo
end subroutine calfun

end module newuoa_functions

program test_newuoa
    use newuoa_minimisation
    use newuoa_functions

    real(kind=wp), dimension(10) :: x
    real(kind=wp)                :: rhobeg, rhoend
    integer                      :: iprint, maxfun
    integer                      :: i, n, npt

    iprint = 2
    maxfun = 5000
    rhoend = 1.0d-6

    do n = 2,8,2
        npt = 2 * n + 1
        do i = 1,n
            x(i) = real(i,wp)/real(n+1,wp)
        enddo

        rhobeg = 0.2d0 * x(1)
        print 20, n,npt
 20     format (//4x,'results with n =',i2,' and npt =',i3)

        call newuoa( npt, x(1:n), rhobeg, rhoend, iprint, maxfun, calfun )

    enddo
end program test_newuoa
