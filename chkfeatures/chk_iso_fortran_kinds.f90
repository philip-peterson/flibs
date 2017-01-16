! chk_iso_fortran_kinds.f90
!     Check: does the ISO_FORTRAN_ENV module list the various kinds?
!
program chk_iso_fortran_kinds
    use iso_fortran_env

    implicit none

    integer :: i, k, kprev

    write( *, '(a)'       ) 'Character kinds:'
    write( *, '(4x,10i4)' ) character_kinds
    write( *, '(a)'       ) 'Logical kinds:'
    write( *, '(4x,10i4)' ) logical_kinds
    write( *, '(a)'       ) 'Integer kinds:'
    write( *, '(4x,10i4)' ) integer_kinds
    write( *, '(a)'       ) 'Real kinds:'
    write( *, '(4x,10i4)' ) real_kinds
    write( *, '(a)'       ) 'Properties of the integer kinds:'
    write( *, '(4x,2a10)' ) 'Kind', 'Range'
    kprev = 0
    do i = 1,100
        k = selected_int_kind( i )
        if ( k /= kprev .and. kprev /= 0 ) then
            write( *, '(4x,2i10)' ) kprev, i
        endif
        kprev = k

        if ( k == -1 ) then
            exit
        endif
    enddo
    write( *, '(a)'       ) 'Properties of the real kinds:'
    write( *, '(4x,2a10)' ) 'Kind', 'Precision'
    kprev = 0
    do i = 1,100
        k = selected_real_kind( i )
        if ( k /= kprev .and. kprev /= 0 ) then
            write( *, '(4x,2i10)' ) kprev, i
        endif
        kprev = k

        if ( k == -1 ) then
            exit
        endif
    enddo
end program chk_iso_fortran_kinds
