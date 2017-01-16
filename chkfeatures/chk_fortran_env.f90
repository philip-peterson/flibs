! chk_fortran_env.f90
!     Check: does the compiler support the ISO_FORTRAN_ENV intrinsic module?
!
program chk_fortran_env
    use, intrinsic :: iso_fortran_env

    implicit none

    write( *, '(a,i0)' ) 'Unit for input (READ(*,...):   ', input_unit
    write( *, '(a,i0)' ) 'Unit for output (WRITE(*,...): ', output_unit
    write( *, '(a,i0)' ) 'Unit for error reporting:      ', error_unit

end program chk_fortran_env
