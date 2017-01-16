! chk_inquire_size.f90 --
!     Check: does the compiler support the SIZE= keyword in the INQUIRE statement?
!
program chk_size
    implicit none

    integer :: filesize
    logical :: exists

    write( *, '(a)' ) 'Examine the size of the source file:'

    inquire( file = 'chk_inquire_size.f90', size = filesize, exist = exists )

    if ( exists ) then
        write( *, '(a,i0)') '    Reported file size: ', filesize
        if ( filesize < 0 ) then
            write( *, '(a,i0)') '    Note: there is something wrong - the file exists and'
            write( *, '(a,i0)') '          should therefore be at least 0 bytes in size'
        endif
    else
        write( *, '(a)')   '    The file "chk_inquire_size,f90" does not seem to exist'
        write( *, '(a)')   '    Please check this'
    endif

    open( 10, file = 'chk_inquire_size.f90' )
    inquire( 10, size = filesize, exist = exists )

    write( *, '(a)' ) 'After opening the file:'
    if ( exists ) then
        write( *, '(a,i0)') '    Reported file size: ', filesize
        if ( filesize < 0 ) then
            write( *, '(a,i0)') '    Note: there is something wrong - the file exists and'
            write( *, '(a,i0)') '          should therefore be at least 0 bytes in size'
        endif
    else
        write( *, '(a)')   '    The file "chk_inquire_size,f90" is reported as non-existent'
        write( *, '(a)')   '    There is something seriously wrong'
    endif
end program chk_size
