! run_long_computation,f90 --
!     Simple example of "remote computing": start a computation
!     and monitor the progress
!     This version: fast-CGI
!
!     Each computation is started in a separate directory
!
program run_long_computation

    use fcgi_protocol

    implicit none

    type(dict_struct), pointer :: dict => null()

    integer :: dummy
    integer :: last_id
    integer :: current_id

    integer :: lun
    integer :: lunsave
    integer :: ierr

    character(len=80) :: string

    !
    ! Read the last used computation ID - if any
    !
    open( newunit = lunsave, file = 'long_computations.saved', status = 'old', iostat = ierr )
    if ( ierr == 0 ) then
        read( lunsave, * ) last_id
        last_id = last_id + 1
    else
        open( newunit = lunsave, file = 'long_computations.saved' )
        last_id = 1
    endif

    !
    ! Prepare the communication with the FGI server by
    ! opening a file to write the HTML output
    !
    open( newunit = lun, file = 'result_computation.html' )

    do while ( fcgip_accept_environment_variables() >= 0 )

        !
        ! Build the dictionary with query information etc.
        !
        call fcgip_make_dictionary( dict, lun )

        !
        ! If we have no computation-ID, start a new computation
        !
        current_id = -1
        call cgi_get( dict, 'COMPUTATION_ID', current_id )
        if ( current_id > 0 ) then
            call copy_result( current_id )
        else
            current_id = last_id

            rewind( lunsave )
            write( lunsave, * ) current_id
            flush( lunsave )

            !
            ! NOTE: This works under Linux, not under Windows
            !       See the man page
            !
            ! Linux command
            write( string, '(a,i0)' ) 'mkdir ', current_id
            ! Windows command
            !! write( string, '(a,i0)' ) 'md ', current_id

            call execute_command_line( string )

            ! Linux command
            write( string, '(a,i0)' ) 'long_computation &', current_id
            ! Windows command
            ! Is "start" really required? Apparently. along with /B
            !! write( string, '(a,i0)' ) 'start /B long_computation ', current_id

            call execute_command_line( string, wait = .false. )

            write( lun, '(a)' ) &
                '<html>', &
                '<head><title>Long computation</title>', &
                '<body>'
            write( lun, '(a,i0,a)' ) &
                '<h1>Results of long computation ', current_id, '<h1>'

            write( string, '(a,i0,a)' ) &
                '<input type="hidden" name="COMPUTATION_ID" value="', current_id, '">'

            write( lun, '(a)' ) &
                '<form action="update" method="get">', &
                '<input type="submit" value="Update">', &
                string, &
                '</form>', '<p>','Computation started ...', '</body>', '</html>'
        endif

        !
        ! Copy the output file to the web server
        !
        call fcgip_put_file( lun )
    enddo

    close( lun )
contains

subroutine copy_result( id )
    integer           :: id

    character(len=80) :: string
    character(len=40) :: filename
    integer           :: ierr
    integer           :: luninp

    !
    ! Write the header
    !
    write( lun, '(a)' ) &
        '<html>', &
        '<head><title>Long computation</title>', &
        '<body>'
    write( lun, '(a,i0,a)' ) &
        '<h1>Results of long computation', id, '<h1>'

    write( string, '(a,i0,a)' ) &
        '<input type="hidden" name="COMPUTATION_ID" value="', current_id, '>'

    write( lun, '(a)' ) &
        '<form action="update" method="get">', &
        '<input type="submit" value="Update">', &
        string, &
        '</form>', '<p>'

    !
    ! Simply copy the output from the associated process
    ! (For simplicity we ignore the guarding file "writing"
    !
    write( filename, '(i0,a)' ) id, '/computation.out'
    open( newunit = luninp, file = filename, status = 'old', iostat = ierr )

    if ( ierr /= 0 ) then
        write( lun, '(a)' ) '(No output yet)'
    else
        write( lun, '(a)' ) '<pre>'
        do
            read( luninp, '(a)', iostat = ierr ) string
            if ( ierr /= 0 ) then
                exit
            else
                write( lun, '(a)' ) trim(string)
            endif
        enddo
        write( lun, '(a)' ) '</pre>'
    endif
    !
    ! Complete the output
    !
    write( lun, '(a)' ) &
        '</body>', &
        '</html>'

end subroutine copy_result

end program run_long_computation
