! chk_random_number --
!     Check: does the compiler initialise the random number generator with a fixed seed or not?
!
!     Note:
!     The program has to be run twice, to see if the random numbers start with the same
!     sequence or not. The first run writes a file with the ten random numbers that were
!     generated, the second run reads that file and compares these numbers with the new ones.
!
!     Problem: random_seed is not sufficient to guarantee a new series.
!
program chk_random_number
    implicit none

    real, dimension(10) :: rnd, new_rnd
    logical             :: exists

    !call random_seed

    inquire( file = 'chk_random_number.bin', exist = exists )

    if ( .not. exists ) then
        !
        ! First run: write a file with random numbers
        !
        call random_number( rnd )
        open( 10, file = 'chk_random_number.bin', form = 'unformatted' )
        write( 10 ) rnd
        close( 10 )
    else
        !
        ! Second run: read the file with random numbers and check with the current ones
        !
        call random_number( new_rnd )
        open( 10, file = 'chk_random_number.bin', form = 'unformatted' )
        read( 10 ) rnd
        close( 10, status = 'delete' )

        write( *, '(a)' ) 'Random number generation:'
        write( *, '(a)' ) '    Probing the actual behaviour of the implementation'

        if ( all( rnd == new_rnd ) ) then
            write( *, '(a)' ) '    The second run gives the same random numbers'
            write( *, '(a)' ) '    Use random_seed if a new series is required'

            !
            ! Check if a simple call to random_seed is enough to reinitialise
            ! it arbitrarily
            !
            call random_seed
            call random_number( rnd )
            if ( all( rnd == new_rnd ) ) then
                write( *, '(a)' ) '    Note:'
                write( *, '(a)' ) '        You need to construct a new seed explicitly!'
            else
                write( *, '(a)' ) '        (A simple call to random_seed suffices)'
            endif
        else
            write( *, '(a)' ) '    The second run gives different random numbers'
            write( *, '(a)' ) '    Use random_seed to retrieve the seed and initialise'
            write( *, '(a)' ) '    to the same series'
        endif
    endif
end program chk_random_number
