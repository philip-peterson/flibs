! chk_quadreal.f90 --
!     Check: does the compiler support reals with larger precision than double precision?
!
!     Note: if not, then the compilation will fail (kind < 0)
!
program chk_quadreal
    implicit none

    integer, parameter :: quadruple = selected_real_kind(precision(1.0d0)+1)
    real(quadruple)    :: quadruple_value = 1.0_quadruple

    write(*,'(a,i0)') 'Precision for "quadruple" reals: ', precision(quadruple_value)
    write(*,'(a,i0)') 'Range for "quadruple" reals:     ', range(quadruple_value)
end program chk_quadreal
