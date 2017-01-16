! chk_isnan.f90
!     Check: can we check if a number is not-a-number via the IEEE features?
!
program chk_isnan
    use ieee_arithmetic
    implicit none

    real :: x

    call random_number( x )
    x = sqrt(x-1.0)

    write( *, '(a,a)' ) 'The value of x should be "not-a-number" - ', merge( 'YES', 'NO ', ieee_is_nan(x) )
    write( *, '(a,a)' ) 'The ieee_is_nan intrinsic function is available'

end program chk_isnan
