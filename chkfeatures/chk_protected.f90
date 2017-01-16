! chk_protected.f90
!     Check: does the compiler support the PROTECTED attribute?
!
module protected_var
    implicit none

    integer, protected :: readonly = 123
end module protected_var

program chk_protected
    use protected_var

    implicit none

    write( *, '(a,i0)' ) 'Value of the protected variable: ', readonly
    write( *, '(a,i0)' ) 'Expected value:                  ', 123
    write( *, '(a)'    ) '(This only confirms syntactical support for the attribute)'
end program chk_protected
