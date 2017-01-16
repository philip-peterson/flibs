! chk_newunit.f90 --
!     Check: does the compiler support the NEWUNIT keyword for OPEN?
!
program chk_newunit
    implicit none

    integer :: lun, ierr

    open( newunit = lun, file = 'chk_newunit.inp', iostat = ierr )

    write(*,'(a)')    'Checking NEWUNIT=:'
    write(*,'(a,i5)') '    File opened at unit = ', lun
    write(*,'(a,i5)') '    Error code:           ', ierr
end program chk_newunit
