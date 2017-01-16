! chk_end_of_line.f90 --
!     Check: how does a program handle files with different end-of-line conventions?
!
!     Note:
!     This program relies on stream access to write a few test files. If stream
!     access is not supported, the program will not be compiled.
!
program chk_end_of_line
    implicit none

    logical           :: errors, cr_left
    integer           :: ierr, k1, k2
    character(len=20) :: line1, line2

    !
    ! First create a file with CRLF and one with LF only as the end of line
    ! sequence
    !
    open( 10, file = 'chk_end_of_line1.inp', access = 'stream' )
    open( 20, file = 'chk_end_of_line2.inp', access = 'stream' )

    write( 10 ) 'Line1' // achar(13) // achar(10)
    write( 10 ) 'Line2 - 1234567890' // achar(13) // achar(10)
    write( 20 ) 'Line1' // achar(10)
    write( 20 ) 'Line2 - 1234567890' // achar(10)

    close( 10 )
    close( 20 )

    !
    ! Read the first file
    !
    write( *, '(a)' ) 'Reading file with CR/LF as line ending'

    errors  = .false.
    cr_left = .false.

    open( 10, file = 'chk_end_of_line1.inp' )
    read( 10, '(a)', iostat = ierr ) line1
    if ( ierr == 0 ) then
        read( 10, '(a)', iostat = ierr ) line2
        if ( ierr == 0 ) then
            write( *, '(a)' ) '    File can be read without problems'
        else
            errors = .true.
            write( *, '(a)' ) '    Error reading the second line'
        endif
    else
        errors = .true.
        write( *, '(a)' ) '    Error reading the first line'
    endif

    if ( .not. errors ) then
        k1 = index( line1, achar(13) )
        k2 = index( line2, achar(13) )
        if ( k1 > 0 .or. k2 > 0 ) then
            cr_left = .true.
            write( *, '(a)' ) '    NOTE: Reading a file with CR/LF line endings leaves the carriage return character (ACHAR(13))'
        else
            write( *, '(a)' ) '    Reading a file with CR/LF line endings removes the carriage return character'
        endif

        if ( k1 > 0 ) then
            line1(k1:) = ' '
        endif
        if ( k2 > 0 ) then
            line2(k2:) = ' '
        endif
        if ( line1 /= 'Line1' ) then
            errors = .true.
            write( *, '(a)' ) '    Line 1 has incorrect value: >', trim(line1), '<'
        endif
        if ( line2 /= 'Line2 - 1234567890' ) then
            errors = .true.
            write( *, '(a)' ) '    Line 2 has incorrect value: >', trim(line2), '<'
        endif
    endif

    !
    ! Read the second file
    !
    write( *, '(a)' ) 'Reading file with LF as line ending'

    open( 20, file = 'chk_end_of_line2.inp' )
    read( 20, '(a)', iostat = ierr ) line1
    if ( ierr == 0 ) then
        read( 20, '(a)', iostat = ierr ) line2
        if ( ierr == 0 ) then
            write( *, '(a)' ) '    File can be read without problems'
        else
            write( *, '(a)' ) '    Error reading the second line'
        endif

        if ( line1 /= 'Line1' ) then
            errors = .true.
            write( *, '(a)' ) '    Line 1 has incorrect value: >', trim(line1), '<'
        endif
        if ( line2 /= 'Line2 - 1234567890' ) then
            errors = .true.
            write( *, '(a)' ) '    Line 2 has incorrect value: >', trim(line2), '<'
        endif
    else
        write( *, '(a)' ) '    Error reading the first line'
    endif

    if ( .not. errors ) then
        if ( .not. cr_left ) then
            write( *, '(/,a)' ) &
                'Reading a file with CR/LF line endings (Windows) or LF line endings (Linux) causes no specific problems'
        else
            write( *, '(/,a)' ) &
                'Reading a file with CR/LF line endings (Windows) requires removing CR characters'
        endif
    else
        write( *, '(a)' ) '    Reading a file with alternative line endings causes problems'
    endif
end program chk_end_of_line
