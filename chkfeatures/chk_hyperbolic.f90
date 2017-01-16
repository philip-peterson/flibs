! chk_hyperbolic.f90 --
!     Check: does the compiler support the hyperbolic and inverse hyperbolic functions for complex arguments?
!
!     Note: avoid real/imaginary component of 1 - not all functions are defined for such arguments
!
program chk_hyperbolic
    implicit none

    complex, dimension(5) :: z = (/ cmplx(0.9,0.0), cmplx(0.0,0.9), cmplx(0.9,0.9), cmplx(-0.9,0.0), cmplx(0.0,-0.9) /)

    write( *, '(a)'                  ) 'Complex hyperbolic functions:  '
    write( *, '(a,5(2f7.4,'' | ''))' ) 'argument: ', z
    write( *, '(a,5(2f7.4,'' | ''))' ) 'sinh:     ', sinh(z)
    write( *, '(a,5(2f7.4,'' | ''))' ) 'cosh:     ', cosh(z)
    write( *, '(a,5(2f7.4,'' | ''))' ) 'tanh:     ', tanh(z)
    write( *, '(a)' )                  'Inverse complex hyperbolic functions:  '
    write( *, '(a,5(2f7.4,'' | ''))' ) 'argument: ', z
    write( *, '(a,5(2f7.4,'' | ''))' ) 'asinh:    ', asinh(z)
    write( *, '(a,5(2f7.4,'' | ''))' ) 'acosh:    ', acosh(z)
    write( *, '(a,5(2f7.4,'' | ''))' ) 'atanh:    ', atanh(z)
end program chk_hyperbolic
