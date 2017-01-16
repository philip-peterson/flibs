! chk_file_incomplete.f90 --
!     Check: what happens if the program reads a file whose last line does not end properly?
!
!     Note:
!     Uses stream access to create a suitable file
!
program chk_file_incomplete
    implicit none

    integer           :: ierr
    character(len=10) :: string

    open( 10, file = 'chk_file_incomplete.txt', access = 'stream' )

    string = '1234567890'

    write( 10 ) string(1:4)
    close( 10 )

    !
    ! Try to read the file
    !
    open( 20, file = 'chk_file_incomplete.txt' )

    write( *, '(a)' ) 'Formatted read:'

    read( 20, '(a)', iostat = ierr, end = 100 ) string

    if ( ierr /= 0 ) then
        write( *, '(a,i0)' ) '    Reading an incomplete (text) file caused a run-time error - iostat = ', ierr
    else
        write( *, '(a)' ) '    Reading an incomplete (text) file did not cause any run-time error'
    endif

    goto 200

100 continue
    write( *, '(a,i0)' ) '    Reading an incomplete (text) file caused an end-of-file condition - iostat = ', ierr


200 continue
    write( *, '(a)' ) 'List-directed read:'
    rewind( 20 )
    read( 20, *, iostat = ierr, end = 300 ) string

    if ( ierr /= 0 ) then
        write( *, '(a,i0)' ) '    Reading an incomplete (text) file caused a run-time error - iostat = ', ierr
    else
        write( *, '(a)' ) '    Reading an incomplete (text) file did not cause any run-time error'
    endif

    goto 400

300 continue
    write( *, '(a,i0)' ) '    Reading an incomplete (text) file caused an end-of-file condition - iostat = ', ierr

400 continue
    !close( 20, status = 'delete' )

end program
