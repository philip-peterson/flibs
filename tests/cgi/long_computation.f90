! long_computation.f90 --
!     Program that simulates the behaviour of
!     programs that require long computation times
!
!     It simply writes a few records to a file with
!     long pauses
!
program long_computation
    implicit none
    
    integer           :: i
    integer           :: lun
    integer           :: lunguard
    character(len=80) :: directory
    character(len=8)  :: date
    character(len=10) :: time
    
    call get_command_argument( 1, directory )
    if ( directory == '' ) then
        directory = '.'
    endif
    
    open( newunit = lun, file = trim(directory)//"/computation.out" )
    
    do i = 1,10
        ! 
        ! Keep a file open during the writing 
        !
        open( newunit = lunguard, file = trim(directory) // "/writing" )
        call date_and_time( date, time )
        write( lun, * ) date, ' ', time, ' step ', i 
        flush( lun )
        close( lunguard, status = 'delete' ) ! Signal that we are done
        call sleep( 10 )
    enddo
    
    call date_and_time( date, time )
    write( lun, * ) date, ' ', time, ' Done' 

end program long_computation
