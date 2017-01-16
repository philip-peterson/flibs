! chk_io_encoding --
!     Check: does the compiler support the encoding keyword?
!
program chk_io_encoding
    implicit none

    integer           :: ierr
    character(len=20) :: string

    write( *, '(a)' ) 'Encoding the file as "default":'

    open( 10, file = 'chk_io_encoding.txt', encoding = 'default', iostat = ierr )

    if ( ierr == 0 ) then
        write( *, '(a)' ) '    The file could be opened with "default" encoding without any error'
    else
        write( *, '(a,i0)' ) '    The file could not be properly opened with "default" encoding - error code ', ierr
    endif

    inquire( 10, encoding = string )
    write( *, '(2a)' ) '    The inquire statement returns: encoding = ', string

    close( 10 )

    write( *, '(a)' ) 'Encoding the file as "UTF-8":'

    open( 10, file = 'chk_io_encoding.txt', encoding = 'utf-8', iostat = ierr )

    if ( ierr == 0 ) then
        write( *, '(a)' ) '    The file could be opened with "utf-8" encoding without any error'
    else
        write( *, '(a,i0)' ) '    The file could not be properly opened with "utf-8" encoding - error code ', ierr
    endif

    inquire( 10, encoding = string )
    write( *, '(2a)' ) '    The inquire statement returns: encoding = ', string

    close( 10 )
end program chk_io_encoding
