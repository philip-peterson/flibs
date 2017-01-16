! chk_advance_no --
!     Check: does the compiler support the ADVANCE=NO option?
!
program chk_advance_no
    implicit none

    integer             :: i, ierr
    real, dimension(10) :: x

    call random_number( x )

    open( 10, file = 'chk_advance_no.out' )
    do i = 1,size(x)
        write( 10, '(f10.4)', advance = 'no' ) x(i)
    enddo

    write( 10, '(a)', advance = 'yes' ) ''
    close( 10 )

    !
    ! The file should contain one line only ...
    !
    open( 11, file = 'chk_advance_no.out' )
    read( 11, *, iostat = ierr ) x(1)

    if ( ierr /= 0 ) then
        write( *, '(a)' ) 'Error reading the first line - this is unexpected!'
        write( *, '(a)' ) 'Please check the program and the auxiliary file "chk_advance_no.out"'
    else
        read( 11, *, iostat = ierr ) x(2)
        if ( ierr == 0 ) then
            write( *, '(a)' ) 'No error reading the second line - this is unexpected!'
            write( *, '(a)' ) 'Please check the program and the auxiliary file "chk_advance_no.out"'
        else
            write( *, '(a)' ) 'As expected: only line in the auxiliary file'
            write( *, '(a)' ) 'This is a correct result'
        endif
    endif

end program chk_advance_no
