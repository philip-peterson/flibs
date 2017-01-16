! chk_real_literal.f90 --
!     Check: does the compiler warn about overly precise constants?
!
program chk_real_literal
    implicit none

    real(kind=kind(1.0d0)) :: value
    real(kind=kind(1.0d0)) :: value_init = 1.2345678901234567890

    value = 1.2345678901234567890  ! NOTE: single-precision constant

    write( *, '(a)'      ) 'The compiler may or may not issue a warning, but the value'
    write( *, '(a)'      ) '  "1.2345678901234567890" is stored as'
    write( *, '(f25.20)' ) value
    write( *, '(/,a)'      ) 'Note: the literal value is single-precision'
end program chk_real_literal
