! mktranslation.f90 --
!     Auxiliary program to turn a "translations" file into source code
!
!     TODO:
!     - Check length of keys and description
!     - Check for duplicate keys
!
program mktranslation
    character(len=10)  :: lang, default
    character(len=40)  :: key
    character(len=160) :: text
    character(len=200) :: line

    character(len=80)  :: filename
    integer            :: number_strings, idx, pos, arglen, error

    call get_command_argument( 1, filename, arglen, error )

    if ( error /= 0 ) then
        write(*,*) 'Please specify the name of the input file'
        stop
    endif

    open( 10, file = filename, status = 'old', iostat = ierr )

    if ( ierr /= 0 ) then
        write(*,*) 'Error opening the file: ', trim(filename)
    endif

    open( 20, file = 'translation.inc' )

    !
    ! First count the number of text strings
    !
    default = 'EN'
    number_strings = 0

    do
        read( 10, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) then
            exit
        endif

        line = adjustl(line)

        if ( line(1:1) == '#' ) then
            cycle
        endif

        if ( index( line, 'default:' ) == 1 ) then
            pos = index( line, ':' )
            default = adjustl(line(pos+1:))
            cycle
        endif

        if ( index( line, 'key:' ) == 1 ) then
            cycle
        endif

        if ( index( line, ':' ) <= 0 ) then
            cycle
        endif

        !
        ! Any other line: "language: text"
        !
        number_strings = number_strings + 1
    enddo

    !
    ! Write the declarations
    !
    write( 20, '(a,i0,a)' ) 'type(text_entry), dimension(', number_strings, ') :: entry'
    write( 20, '(3a)' )    'character(len=10) :: default_lang = "', trim(default), '"'

    !
    ! Now write the data statements
    !
    rewind( 10 )

    idx = 0
    do
        read( 10, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) then
            exit
        endif

        line = adjustl(line)

        if ( line(1:1) == '#' ) then
            cycle
        endif

        if ( index( line, 'default:' ) == 1 ) then
            cycle
        endif
        if ( index( line, 'key:' ) == 1 ) then
            pos = index( line, ':' )
            key = adjustl(line(pos+1:))
            cycle
        endif

        if ( index( line, ':' ) <= 0 ) then
            cycle
        endif

        !
        ! Any other line: "language: text"
        !
        idx = idx + 1

        pos  = index( line, ':' )
        lang = adjustl(line(1:pos-1))
        text = adjustl(line(pos+1:))

        write( 20, '(a,i0,3a)' ) 'data entry(', idx, ')%key / "', trim(key), '"/'
        write( 20, '(a,i0,3a)' ) 'data entry(', idx, ')%lang / "', trim(lang), '"/'
        text = quoted(text)
        if (len_trim(text) < 80 ) then
            write( 20, '(a,i0,3a)' ) 'data entry(', idx, ')%text / "', trim(text), '"/'
        else
            write( 20, '(a,i0,3a)' ) 'data entry(', idx, ')%text / "', text(1:80), '&'
            write( 20, '(3a)' )      '    &', text(81:160), '"/'
        endif
    enddo

contains

function quoted( text ) result(qtext)
    character(len=*)         :: text
    character(len=len(text)) :: qtext

    integer :: pos, first, last, offset, qfirst, qlast

    qtext = text
    last  = len_trim(text)

    offset = -1
    first  =  1
    qfirst =  1
    qlast  =  last
    do
        pos = index( text(first:last), '"' )

        if ( pos <= 0 ) then
            exit
        else
            pos    = pos    + first - 1
            offset = offset + 1
            qfirst = pos    + offset
            first  = pos    + 1
            qlast  = qlast  + 1
            qtext(qfirst:qfirst)  = '"'
            qtext(qfirst+1:qlast) = text(pos:)
        endif
    enddo
end function quoted
end program mktranslation
