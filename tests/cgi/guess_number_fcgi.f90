! guess_number_plain,f90 --
!     Simple game: guess a number between 1 and 100
!     This version: fast-CGI
!
!     The fast-CGI version:
!     The idea is that you can serve more than one connection
!     within the same process. While it is possible to use
!     threads, the processing is so simple and compact that
!     we trust it to be fast enough so that multithreading
!     is not needed.
!
program guess_number_fcgi

    use fcgi_protocol

    implicit none

    type(dict_struct), pointer :: dict => null()

    type game_data_t
        integer :: number_to_guess
        integer :: guesses
        logical :: inuse = .false.
    end type game_data_t

    integer :: dummy
    integer :: connection_id
    integer :: user_number
    real    :: r

    type(game_data_t), dimension(100) :: game_data
    integer                           :: lun

    !
    ! Phase 0a: prepare the communication with the FGI server by
    !           opening a file to write the HTML output
    !
    !!open( newunit = lun, status = 'scratch', form = 'formatted' )
    open( newunit = lun, file = 'guess_number.html' )

    do while ( fcgip_accept_environment_variables() >= 0 )

        !
        ! Phase 0b: build the dictionary with query information etc.
        !
        call fcgip_make_dictionary( dict, lun )

        !
        ! Phase 1: set up a new game
        !
        ! This means:
        ! - We have no connection ID yet
        ! - Determine a random number between 1 and 100
        ! Problem:
        ! We need a way to start the random number generator "truly"
        ! random
        !
        connection_id = -1
        call cgi_get( dict, 'CONNECTION_ID', connection_id )
        if ( connection_id > 0 ) then
            call examine_guess( dict, game_data(connection_id), connection_id )
        else
            call start_game( game_data )
        endif

        !
        ! Phase 2: copy the output file to the web server
        !
        call fcgip_put_file( lun )
    enddo

    close( lun )
contains

subroutine start_game( game_data )
    type(game_data_t), dimension(:) :: game_data

    integer                         :: id
    integer                         :: i
    integer                         :: ticks
    real                            :: r
    character(len=80)               :: id_string

    !
    ! Find a free slot
    !
    id = -1
    do i = 1,size(game_data)
        if ( .not. game_data(i)%inuse ) then
            game_data(i)%inuse = .true.
            id = i
            exit
        endif
    enddo

    !
    ! No free slot?
    !
    if ( id == -1 ) then
        write( lun, '(a)' ) 'Sorry, all slots are occupied at the moment - try again later'
        return
    endif

    !
    ! Start the new game
    !
    call system_clock( ticks )
    do i = 1,mod(ticks, 1000 )
        call random_number( r )
    enddo

    game_data(id)%number_to_guess = 1 + mod( int(100 * r), 100 )
    game_data(id)%guesses         = 0

    !
    ! The HTML page
    !
    ! Note the use of a hidden input variable, this way we know what the
    ! ID of the game is
    !
    write( id_string, '(a,i0,a)' ) &
        '<input type="hidden" name="CONNECTION_ID" value="', id, '">'

    write( lun, '(a)' ) &
        '<html>', &
        '<head><title>Guess a number</title>', &
        '<body>', &
        '<h1>Guess a number</h1>', &
        '<form action="check" method="get">', &
        'Enter a number between 1 and 100: <input type="entry" name="number" value="1">', &
        '<p>', &
        '<input type="submit" value="Check">', &
        id_string, &
        '</form>', &
        '</body>', &
        '</html>'

end subroutine start_game

subroutine examine_guess( dict, game_data, id )
    type(dict_struct), pointer :: dict
    type(game_data_t)          :: game_data
    integer                    :: id

    character(len=80)          :: id_string
    integer                    :: user_number

    game_data%guesses = game_data%guesses + 1

    user_number = 1
    call cgi_get( dict, "number", user_number )

    write( id_string, '(a,i0,a)' ) &
        '<input type="hidden" name="CONNECTION_ID" value="', id, '">'

    write( lun, '(a)' ) &
        '<html>', &
        '<head><title>Guess a number</title>', &
        '<body>', &
        '<h1>Guess a number</h1>'
    !
    ! Check the number
    !
    write( lun, '(a,i0,a)' ) 'You entered: ', user_number

    if ( user_number /= game_data%number_to_guess ) then
        if ( user_number > game_data%number_to_guess ) then
            write( lun, '(a)' ) '<p>', 'The number is <b>lower</b>'
        elseif ( user_number < game_data%number_to_guess ) then
            write( lun, '(a)' ) '<p>', 'The number is <b>greater</b>'
        endif

        write( lun, '(a)' )      '<form action="check" method="get">'
        write( lun, '(a,i0,a)' ) 'Enter your next guess: <input type="entry" name="number" value="', user_number, '">'
        write( lun, '(a)' )     '<p>', '<input type="submit" value="Check">', &
                                 id_string, '</form>'

    else
        !
        ! The user found it
        !
        write( lun, '(a)' ) 'Congratulations: You guessed it!'
        write( lun, '(2a,i0)' ) '<p>', 'Number of guesses: ', game_data%guesses
    endif

    write( lun, '(a)' ) '</body>', '</html>'

end subroutine examine_guess

end program guess_number_fcgi
