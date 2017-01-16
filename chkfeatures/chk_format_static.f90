! chk_format_static --
!     Check: does the compiler check formats statically or dynamically?
!
program chk_format_static
    implicit none

    logical :: errors
    integer :: ierr
    integer :: x = 1
    real    :: y = 1.0

    errors = .false.

    write( *, '(a)' ) 'Checking what happens with incorrect formats ...'

    write( *, '(a,f12.4)', iostat = ierr ) 'Integer as real:', x
    if ( ierr /= 0 ) then
        errors = .true.
        write( *, '(a)' ) 'Wrong format (F12.4 for integer) causes a run-time error'
    endif

    write( *, '(a,i12)', iostat = ierr ) 'Real as integer:', y
    if ( ierr /= 0 ) then
        errors = .true.
        write( *, '(a)' ) 'Wrong format (I12 for real) causes a run-time error'
    endif

    if ( .not. errors ) then
        write( *, '(a)' ) 'Selecting a wrong format doesn''t cause a detectable run-time error'
    endif
end program chk_format_static
