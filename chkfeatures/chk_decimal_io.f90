! chk_decimal_io.f90
!     Check: does the compiler support the DECIMAL specifier in READ/WRITE statements?
!
program chk_decimal_io
    implicit none

    integer            :: ierr
    real, dimension(5) :: value
    real, dimension(5) :: copy
    character(len=20)  :: string

    value = (/ 1.1, 2.3, 4.0, 5.6, 17.11 /)

    write( *, '(a)' ) 'Writing with comma as decimal separator'

    open( 10, file = 'chk_decimal_io.data' )

    write( 10, '(5f10.3)', decimal = 'comma' ) value

    rewind( 10 )

    !
    ! Read with default values - then we expect the first two to be one and 100
    !
    read( 10, * ) copy

    if ( copy(1) /= 1.0 .or. copy(2) /= 100.0 ) then
        write( *, '(a,2e15.5)' ) '   Expected value 1.0 and 100.0, but got: ', copy(1:2)
    else
        write( *, '(a,2e15.5)' ) '   First test succeeded, as expected got: ', copy(1:2)
    endif

    !
    ! Read using commas as decimal separators
    !
    write( *, '(a)' ) 'Reading with list-directed READ:'
    rewind( 10 )

    read( 10, *, decimal = 'comma' ) copy

    if ( any( value /= copy ) ) then
        write( *, '(/,a)' )      '    NOTE: Found differences with the original values'
        write( *, '(a,5e15.5)' ) '    Original: ', value
        write( *, '(a,5e15.5)' ) '    Read:     ', copy
    else
        write( *, '(a,2e15.5)' ) '    Second test succeeded - all values are the same'
    endif

    write( *, '(a)' ) 'Reading with explicit format:'
    rewind( 10 )

    read( 10, '(5f10.0)', decimal = 'comma' ) copy

    if ( any( value /= copy ) ) then
        write( *, '(/,a)' )      '    NOTE: Found differences with the original values'
        write( *, '(a,5e15.5)' ) '    Original: ', value
        write( *, '(a,5e15.5)' ) '    Read:     ', copy
    else
        write( *, '(a,2e15.5)' ) '    Third test succeeded - all values are the same'
    endif

    !
    ! Now find out if list-directed reads can deal with semicolons as separator
    !
    write( *, '(a)' ) 'Reading a file with commas and semicolons:'

    rewind( 10 )
    write( 10, '(a)' ) '1,2;3,4;5,6'
    rewind( 10 )

    read(  10, *, decimal = 'comma', iostat = ierr ) value(1:3)

    if ( ierr /= 0 ) then
        write( *, '(a)' ) '    An error occurred while reading the string "1,2;3,4;5,6"'
    else
        write( *, '(a)' ) '    String "1,2;3,4;5,6" read without problems'
        string = "1.2 3.4 5.6"
        read( string, * ) copy(1:3)

        if ( any( value(1:3) /= copy(1:3) ) ) then
            write( *, '(a)' ) '    Differences between the values that were read', &
                              '    with commas/semilcolons and points/commas:'
            write( *, '(a,5e15.5)' ) '    Read with commas: ', value
            write( *, '(a,5e15.5)' ) '    Read with points: ', copy
        else
            write( *, '(a)' ) '    All values identical - no difference using commas/semicolons or points/commas'
        endif
    endif

end program chk_decimal_io
