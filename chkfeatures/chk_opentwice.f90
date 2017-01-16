! chk_opentwice.f90 --
!     Check: does the compiler support opening the same file on two different units?
!
!     Note: this is an extension to the standard
!
program chk_opentwice
    implicit none

    integer :: i, ierr
    integer :: value1
    integer :: value2
    integer :: lun1 = 10
    integer :: lun2 = 20
    logical :: opened


    !
    ! First create a file that we can read
    !
    open( lun1, file = 'chk_opentwice.inp' )
    write( lun1, '(i0)' ) (i ,i=1,10)
    close( lun1 )

    !
    ! Now open it on lun1 and read the third number
    !
    open( lun1, file = 'chk_opentwice.inp' )
    read( lun1, * ) (value1 , i=1,3)

    !
    ! Now open it again on lun2 and read a single value
    !
    open( lun2, file = 'chk_opentwice.inp', iostat = ierr )

    if ( ierr /= 0 ) then
        write( *, '(a)' ) 'Error opening the file again - extension not supported'
        stop
    endif

    !
    ! If all went well, try reading via both LU-numbers
    !
    read( lun2, * ) value2

    read( lun1, *, iostat = ierr ) value1 ! Expecting the value 4, if the extension is supported

    !
    ! Is the file still open on lun1?
    !
    inquire( lun1, opened = opened )

    if ( opened ) then
        write( *, '(a)'    ) 'File could be opened independently on a second unit'
        write( *, '(a,i0)' ) '    Value from the second unit: ', value2
        if ( value2 == 1 ) then
            write( *, '(a,i0)' ) '    Value is 1, as expected'
        else
            write( *, '(a,i0)' ) '    BUT: value should have been 1!'
        endif
        if ( ierr /= 0 ) then
            write( *, '(a,i0)' ) '    BUT: reading the file on the first unit gave an error!'
        elseif ( value1 /= 4 ) then
            write( *, '(a,i0)' ) '    BUT: reading the file on the first unit gave an unexpected value!'
            write( *, '(a,i0)' ) '         value read:       ', value1
            write( *, '(a,i0)' ) '         should have been: ', 4
        else
            write( *, '(a,i0)' ) '    Value from the first unit: ', value1
            write( *, '(a,i0)' ) '    Value is 4, as expected'
            write( *, '(a)'    ) 'The extension is supported'
        endif
    else
        write( *, '(a)'    ) 'File can NOT be opened independently on a second unit'
    endif
end program chk_opentwice
