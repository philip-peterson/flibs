! dirsep.f90 --
!     Small program to check if a slash or a backslash are treated
!     as directory separator
!
program dirsep

    character(len=20) :: filename
    character(len=2)  :: bslash
    integer           :: ierr
    logical           :: exists

    bslash = '\\'   ! Some compilers interpret a backslash as an escape character

    write(*,'(a)') 'Directory separators:'
    write(*,'(a)') '1. Treatment of backslashes ('//bslash(1:1)//')'

    if ( bslash(2:2) == ' ' ) then
        write(*,'(a)') '   Careful: backslashes appear to be used as escape characters!'
        write(*,'(a)') '   Check: '//bslash(1:1)//'n appears as a new line?'
        write(*,'(a)') '   If so, then ==>\n<== appears on two lines'
    else
        write(*,'(a)') '   Backslashes are treated as ordinary characters'
    endif


    write(*,'(a)') '2. Forward slash (/) as directory separator'

    filename = 'dirsep.inp'
    inquire( file = filename, exist = exists )
    if ( exists ) then
        open( 10, file = filename )
        close( 10, status = 'delete' )
    endif

    filename = './dirsep.inp'
    open( 10, file = filename, status = 'new', iostat = ierr )
    if ( ierr /= 0 ) then
        write(*,'(a)') '   Error opening file: ' // trim(filename)
        write(*,'(a)') '   Forward slashes seem unusable as directory separator!'
    else
        inquire( file = 'dirsep.inp', exist = exists ) ! No directory information given
        if ( exists ) then
            write(*,'(a)') '   Forward slashes are usable'
        else
            write(*,'(a)') '   Forward slashes seem unusable as directory separator!'
        endif
        close( 10, status = 'delete' )
    endif


    write(*,'(a)') '3. Backward slash ('//bslash(1:1)//') as directory separator'

    filename(2:2) = bslash(1:1)
    open( 10, file = filename, status = 'new', iostat = ierr )
    if ( ierr /= 0 ) then
        write(*,'(a)') '   Error opening file: ' // trim(filename)
        write(*,'(a)') '   Backward slashes seem unusable as directory separator!'
    else
        inquire( file = 'dirsep.inp', exist = exists ) ! No directory information given
        if ( exists ) then
            write(*,'(a)') '   Backward slashes are usable'
        else
            write(*,'(a)') '   Backward slashes seem unusable as directory separator!'
        endif
        close( 10, status = 'delete' )
    endif

end program
