! chk_quadcomplex.f90 --
!     Check: does the compiler support complex numbers with larger precision than double precision?
!
!     Note: if not, then the compilation will fail (kind < 0)
!
program chk_quadcomplex
    implicit none

    integer, parameter :: quadruple = selected_real_kind(precision(1.0d0)+1)
    complex(quadruple) :: quadruple_value = (1.0_quadruple,1.0_quadruple)

    write(*,'(a,i0)') 'Precision for "quadruple" complex numbers: ', precision(quadruple_value)
    write(*,'(a,i0)') 'Range for "quadruple" complex numbers:     ', range(quadruple_value)
end program chk_quadcomplex
