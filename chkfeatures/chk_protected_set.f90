! chk_protected_set.f90
!     Check: does the compiler allow a PROTECTED variable to be set outside its module?
!
module protected_var
    implicit none

    integer, protected :: readonly = 123
end module protected_var

program chk_protected_set
    use protected_var

    implicit none

    readonly = 0

    write( *, '(a,i0)' ) 'Reset value of the protected variable: ', readonly
    write( *, '(a,i0)' ) 'Original value:                        ', 123

    if ( readonly /= 123 ) then
        write( *, '(a)'    ) 'NOTE: this is unexoected - it defies the purpose of PROTECTED'
    else
        write( *, '(a)'    ) 'NOTE: apparently the compiler does not regard this as an error'
        write( *, '(a)'    ) '      but at run-time it is refused'
    endif
end program chk_protected_set
