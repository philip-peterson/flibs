! test_readchar.f90 --
!     Test program for reading a file character by character
!
program test_characters
    use read_characters

    implicit none

    type(character_stream) :: stream
    character(len=1)       :: char
    integer                :: type
    logical                :: success

    !
    ! Open a non-existing file
    !
    call stream%open( "non-existing-file", success )
    if ( success ) then
        write(*,*) 'File seems to exist, but it should not!'
    endif

    !
    ! Open an existing file and copy it to standard-output
    !
    call stream%open( "readchar.txt", success )
    if ( .not. success ) then
        write(*,'(a)') 'File "readchar.txt" does not exist - cannot continue'
        stop
    endif

    write(*,'(a)') 'Copy of "readchar.txt":'

    type = single_character
    do
        call stream%get( char, type )
        if ( type == end_of_file ) then
            exit
        endif

        if ( type == single_character ) then
            write(*,'(a)',advance = 'no' ) char
        elseif ( type == end_of_line ) then
            write(*,'(a)',advance = 'yes' )
        else
            write(*,'(a)') '--- reading error - cannot continue'
            exit
        endif
    enddo

    write(*,'(a)') '-- Complete file read'

    call stream%close

end program test_characters
