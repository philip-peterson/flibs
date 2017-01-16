! chk_g0_format --
!     Check: does the compiler support the G0 format?
!
program chk_g0_format
    implicit none

    integer :: ierr
    integer :: x = 1
    real    :: y = 1.0

    write( *, '(a,g0)', iostat = ierr ) 'Integer value formatted via "G0":', x

    if ( ierr == 0 ) then
        write( *, '(a,g0)', iostat = ierr  ) 'Real value formatted via "G0":   ', y
    endif

    if ( ierr == 0 ) then
        write( *, '(a)' ) 'The Fortran 2008 "G0" format is supported for both integers and reals'
    else
        write( *, '(a)' ) 'The Fortran 2008 "G0" format is NOT supported'
    endif
end program chk_g0_format
