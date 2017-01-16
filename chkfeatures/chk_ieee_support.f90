! chk_ieee_support.f90
!     Check: does the compiler have the ieee_artihmetic support functions?
!
program chk_ieee_support
    use ieee_arithmetic
    use ieee_exceptions
    implicit none

    real                   :: single
    real(kind=kind(1.0d0)) :: double

    write( *, '(a)' )        'IEEE arithmetic support:'
    write( *, '(a20,3a10)' ) 'Type of support', ' All kinds', '    Single', '    Double'
    write( *, '(a20,3l10)' ) 'IEEE reals  ',ieee_support_datatype(), ieee_support_datatype(single), ieee_support_datatype(double)
    write( *, '(a20,3l10)' ) 'Denormal    ',ieee_support_denormal(), ieee_support_denormal(single), ieee_support_denormal(double)
    write( *, '(a20,3l10)' ) 'Division    ',ieee_support_divide(),   ieee_support_divide(single),   ieee_support_divide(double)
    write( *, '(a20,3l10)' ) 'Infinity    ',ieee_support_inf(),      ieee_support_inf(single),      ieee_support_inf(double)
    write( *, '(a20,3l10)' ) 'NaN         ',ieee_support_nan(),      ieee_support_nan(single),      ieee_support_nan(double)
    write( *, '(a20,3l10)' ) 'SQRT        ',ieee_support_sqrt(),     ieee_support_sqrt(single),     ieee_support_sqrt(double)
    write( *, '(a20,3l10)' ) 'All standard',ieee_support_standard(), ieee_support_standard(single), ieee_support_standard(double)
    write( *, '(a20,3l10)' ) 'Underflow   ',ieee_support_underflow_control(), &
                                            ieee_support_underflow_control(single), &
                                            ieee_support_underflow_control(double)

    write( *, '(a,a)' )      'Rounding modes:'
    write( *, '(a20,3l10)' ) 'Nearest     ',ieee_support_rounding(ieee_nearest),        &
                                            ieee_support_rounding(ieee_nearest,single), &
                                            ieee_support_rounding(ieee_nearest,double)
    write( *, '(a20,3l10)' ) 'To zero     ',ieee_support_rounding(ieee_to_zero),        &
                                            ieee_support_rounding(ieee_to_zero,single), &
                                            ieee_support_rounding(ieee_to_zero,double)
    write( *, '(a20,3l10)' ) 'Up          ',ieee_support_rounding(ieee_up),             &
                                            ieee_support_rounding(ieee_up,single),      &
                                            ieee_support_rounding(ieee_up,double)
    write( *, '(a20,3l10)' ) 'Down        ',ieee_support_rounding(ieee_down),           &
                                            ieee_support_rounding(ieee_down,single),    &
                                            ieee_support_rounding(ieee_down,double)
    write( *, '(a20,3l10)' ) 'Other       ',ieee_support_rounding(ieee_other),          &
                                            ieee_support_rounding(ieee_other,single),   &
                                            ieee_support_rounding(ieee_other,double)

    write( *, '(/,a)' ) 'Exceptions:'
    write( *, '(a20,3a10)' ) 'Flagging support', ' All kinds', '    Single', '    Double'

    write( *, '(a20,3l10)' ) 'Invalid       ', ieee_support_flag(ieee_invalid),         &
                                               ieee_support_flag(ieee_invalid, single), &
                                               ieee_support_flag(ieee_invalid, double)

    write( *, '(a20,3l10)' ) 'Overflow      ', ieee_support_flag(ieee_overflow),         &
                                               ieee_support_flag(ieee_overflow, single), &
                                               ieee_support_flag(ieee_overflow, double)

    write( *, '(a20,3l10)' ) 'Divide by zero', ieee_support_flag(ieee_divide_by_zero),         &
                                               ieee_support_flag(ieee_divide_by_zero, single), &
                                               ieee_support_flag(ieee_divide_by_zero, double)

    write( *, '(a20,3l10)' ) 'Inexact       ', ieee_support_flag(ieee_inexact),         &
                                               ieee_support_flag(ieee_inexact, single), &
                                               ieee_support_flag(ieee_inexact, double)

    write( *, '(a20,3l10)' ) 'Underflow     ', ieee_support_flag(ieee_underflow),         &
                                               ieee_support_flag(ieee_underflow, single), &
                                               ieee_support_flag(ieee_underflow, double)

    write( *, '(a20,3a10)' ) 'Halting support', ' All kinds'
    write( *, '(a20,l10)' ) 'Invalid       ', ieee_support_halting(ieee_invalid)
    write( *, '(a20,l10)' ) 'Overflow      ', ieee_support_halting(ieee_overflow)
    write( *, '(a20,l10)' ) 'Divide by zero', ieee_support_halting(ieee_divide_by_zero)
    write( *, '(a20,l10)' ) 'Inexact       ', ieee_support_halting(ieee_inexact)
    write( *, '(a20,l10)' ) 'Underflow     ', ieee_support_halting(ieee_underflow)
end program chk_ieee_support
