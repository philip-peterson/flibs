! chk_tab_io --
!     Check: how does a program deal with tabs in the input?
!
program chk_tab_io
    implicit none

    character(len=20) :: input
    character(len=20) :: string
    real              :: value(2)
    integer           :: ierr

    write( *, '(a)' )    'Reading strings containing tabs'

    input = 'A' // achar(9) // 'B'

    read( input, *, iostat = ierr ) string

    write( *, '(a,a)' ) '    Input string: ', trim(input)

    if ( ierr == 0 ) then
        write( *, '(a,a)' ) '    String as read via list-directed read: ', trim(string)
        if ( len_trim(string) == 3 ) then
            write( *, '(a)' ) '    Tab treated as an ordinary character'
        else
            write( *, '(a)' ) '    Tab apparently treated as a separator'
        endif
    else
        write( *, '(a)' ) '    Error reading a string with a tab'
    endif

end program chk_tab_io
