! test_bobyqa.f90 --
!
!     Test problem for BOBYQA, the objective function being the sum of
!     the reciprocals of all pairwise distances between the points P_I,
!     I=1,2,...,M in two dimensions, where M=N/2 and where the components
!     of P_I are X(2*I-1) and X(2*I). Thus each vector X of N variables
!     defines the M points P_I. The initial X gives equally spaced points
!     on a circle. Four different choices of the pairs (N,NPT) are tried,
!     namely (10,16), (10,21), (20,26) and (20,41). Convergence to a local
!     minimum that is not global occurs in both the N=10 cases. The details
!     of the results are highly sensitive to computer rounding errors. The
!     choice IPRINT=2 provides the current X and optimal F so far whenever
!     RHO is reduced. The bound constraints of the problem require every
!     component of X to be in the interval [-1,1].
!
module bobyqa_functions
    use bobyqa_minimisation, only: wp
    implicit none

contains
subroutine calfun (x,f)
    real(kind=wp), dimension(:) :: x
    real(kind=wp)               :: f

    integer                     :: i, j, n
    real(kind=wp)               :: temp

    n = size(x)
    f = 0.0d0
    do i = 4,n,2
        do j=2,i-2,2
            temp = (x(i-1)-x(j-1))**2+(x(i)-x(j))**2
            temp = max( temp, 1.0d-6 )
            f    = f + 1.0d0 / sqrt(temp)
        enddo
    enddo
end subroutine calfun
end module bobyqa_functions

program test_bobyqa
    use bobyqa_minimisation
    use bobyqa_functions

    implicit none
    real(kind=wp), dimension(100) :: x, xl, xu
    real(kind=wp)                 :: twopi, bdl, bdu, rhobeg, rhoend, temp
    integer                       :: iprint, maxfun
    integer                       :: m, n, j, jcase, npt

    twopi  =  8.0d0 * atan(1.0d0)
    bdl    = -1.0d0
    bdu    =  1.0d0
    iprint =  2
    maxfun =  500000
    rhobeg =  1.0d-1
    rhoend =  1.0d-6

    m = 5
    do while ( m <= 10 )
        n  = 2 * m
        xl = bdl
        xu = bdu

        do jcase = 1,2
            npt = n + 6
            if ( jcase == 2 ) npt = 2 * n + 1
            print 30, m,n,npt
   30       format (//5x,'2d output with m =',i4,',  n =',i4, &
                         '  and  npt =',i4)
            do j = 1,m
                temp = real(j,wp) * twopi / real(m,wp)
                x(2*j-1) = cos(temp)
                x(2*j)   = sin(temp)
            enddo

            call bobyqa( npt, x(1:n), xl(1:n), xu(1:n), rhobeg, rhoend, iprint, maxfun, calfun)
        enddo
        m = 2 * m
    enddo
end program
