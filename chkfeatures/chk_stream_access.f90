! chk_stream_access.f90 --
!     Check: does the compiler support ACCESS='STREAM'?
!
program chk_stream_access
    implicit none

    integer             :: i, ierr
    real, dimension(10) :: value
    real                :: first

    call random_number( value )

    open( 10, file = 'chk_stream_access.dat', access = 'stream' )

    write( 10 ) value

    close( 10 )

    open( 10, file = 'chk_stream_access.dat', access = 'stream' )

    do i = 1,size(value)
        read( 10, iostat = ierr ) value(i)

        if ( ierr /= 0 ) then
            write( *, '(a,i0,a)' ) 'Error reading number ', i, 'from file'
            exit
        endif
    enddo

    if ( ierr /= 0 ) then
        write( *, '(a)' ) 'There should have been no reading error!'
    else
        read( 10, pos = 1 ) first
        if ( first /= value(1) ) then
            write( *, '(a)' ) 'Reading the first nubmer again gave a different value'
            write( *, '(a)' ) 'Support for stream access questionable'
        else
            write( *, '(a)' ) 'Tests for stream access successful'
        endif
    endif
end program chk_stream_access
