! chk_boz_values --
!     Check: does the compiler support the BOZ values with int/real/dble/cmplx?
!
program chk_boz_values
    implicit none

    integer                :: int_value
    real                   :: real_value
    real(kind=kind(1.0d0)) :: double_value
    complex                :: complex_value

    int_value     = int(z'3f7')
    real_value    = real(z'3f700000')
    double_value  = dble(z'3f70000000000000')
    complex_value = cmplx(z'3f700000',z'3f700000')

    write( *, '(a)'        ) 'Using hexadecimal values:'
    write( *, '(a,i0)'     ) '    Integer:          ', int_value
    write( *, '(a,e14.6)'  ) '    Single precision: ', real_value
    write( *, '(a,e20.12)' ) '    Double precision: ', double_value
    write( *, '(a,2e14.6)' ) '    Complex:          ', complex_value

    write( *, '(a)'        ) 'Using an octal value:'
    write( *, '(a,i0)'     ) '    Integer:          ', int(o'123')
    write( *, '(a)'        ) 'Using a binary value:'
    write( *, '(a,i0)'     ) '    Integer:          ', int(b'110011')

end program chk_boz_values
