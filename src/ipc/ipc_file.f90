! ipc_file.f90 --
!     Simple method for inter-process communication via file I/O
!
! ipc_file --
!     Module for IPC via file I/O
!
! NOTES:
!     Start in separate DOS-boxes
!     Order of starting important - right now!
!
module ipc_file
    use dflib ! Hm, compiler-dependency here!

    implicit none

    type ipc_comm
        character(len=20)  :: me          ! Identifying string for calling program
        character(len=20)  :: connection  ! Id for connecting program
        character(len=20)  :: tag         ! Tag for the data (type)
        character(len=400) :: directory   ! Directory to use for the various files
        character(len=400) :: send_file   ! "Send" file
        character(len=400) :: recv_file   ! "Received" file
        character(len=400) :: data_file   ! Data file
        integer            :: id          ! Numerical identfier
        integer            :: lun
    end type ipc_comm

    interface ipc_send_data
        module procedure ipc_send_int_scalar
        module procedure ipc_send_int_1d
        module procedure ipc_send_int_2d
        module procedure ipc_send_int_3d
        module procedure ipc_send_real_scalar
        module procedure ipc_send_real_1d
        module procedure ipc_send_real_2d
        module procedure ipc_send_real_3d
        module procedure ipc_send_dbl_scalar
        module procedure ipc_send_dbl_1d
        module procedure ipc_send_dbl_2d
        module procedure ipc_send_dbl_3d
        module procedure ipc_send_log_scalar
        module procedure ipc_send_log_1d
        module procedure ipc_send_log_2d
        module procedure ipc_send_log_3d
        module procedure ipc_send_char_scalar
        module procedure ipc_send_char_1d
        module procedure ipc_send_char_2d
        module procedure ipc_send_char_3d
        module procedure ipc_send_cmplx_scalar
        module procedure ipc_send_cmplx_1d
        module procedure ipc_send_cmplx_2d
        module procedure ipc_send_cmplx_3d
    end interface

    interface ipc_receive_data
        module procedure ipc_receive_int_scalar
        module procedure ipc_receive_int_1d
        module procedure ipc_receive_int_2d
        module procedure ipc_receive_int_3d
        module procedure ipc_receive_real_scalar
        module procedure ipc_receive_real_1d
        module procedure ipc_receive_real_2d
        module procedure ipc_receive_real_3d
        module procedure ipc_receive_dbl_scalar
        module procedure ipc_receive_dbl_1d
        module procedure ipc_receive_dbl_2d
        module procedure ipc_receive_dbl_3d
        module procedure ipc_receive_log_scalar
        module procedure ipc_receive_log_1d
        module procedure ipc_receive_log_2d
        module procedure ipc_receive_log_3d
        module procedure ipc_receive_char_scalar
        module procedure ipc_receive_char_1d
        module procedure ipc_receive_char_2d
        module procedure ipc_receive_char_3d
        module procedure ipc_receive_cmplx_scalar
        module procedure ipc_receive_cmplx_1d
        module procedure ipc_receive_cmplx_2d
        module procedure ipc_receive_cmplx_3d
    end interface
contains

! ipc_open --
!     Initialise the connection on "this" side
!
! Arguments:
!     me         String identifying the calling process
!     directory  (Optional) directory to use for the files
!
! Result:
!     Initialised structure ready for further communications
!
type(ipc_comm) function ipc_open( me, directory )
    character(len=*)           :: me
    character(len=*), optional :: directory

    ipc_open%me = me

    if ( present(directory) ) then
        ipc_open%directory = trim(directory) // '/'
    else
        ipc_open%directory = './'
    endif

end function ipc_open

! ipc_try_connect --
!     Try to open the connection to another process
!
! Arguments:
!     comm       Connection structure
!     dest       Identifying string for that other process
!     location   Location (directory) to be used
!     idstring   Unique identifier to check the connection
!     space      Name of the tuplespace
!     success    Whether the connection was achieved or not
!
! Note:
!     This routine is meant for use in the context of the tuple_space
!     module.
!
subroutine ipc_try_connect( comm, dest, location, idstring, space, success )
    type(ipc_comm)   :: comm
    character(len=*) :: dest
    character(len=*) :: location
    character(len=*) :: idstring
    character(len=*) :: space
    logical          :: success

    integer                      :: ierr
    integer                      :: lun
    integer                      :: id
    character(len=len(idstring)) :: answer
    logical                      :: error

    success = .false.

    !
    ! Open the send file - but it must not yet exist
    !
    !!!TODO!!!
    lun = 10

    comm = ipc_open( 'client', location )

    open( lun, file = trim(comm%directory) // 'client-' // trim(dest) // ".send", &
        form = 'unformatted', status = 'new', iostat = ierr )
    close( lun )

    if ( ierr /= 0 ) then
        return
    endif

    id = 0
    call ipc_send_start(  comm, dest, 'CONNECT', id )
    call ipc_send_data(   comm, idstring, error )
    call ipc_send_data(   comm, space, error )
    call ipc_send_finish( comm )

    if ( error ) then
        success = .false.
        return
    endif

    !
    ! Now wait for the _same_ string to be returned
    !

    call ipc_receive_start(  comm, dest, idstring, id )
    call ipc_receive_data(   comm, answer, error )
    call ipc_receive_finish( comm )

    if ( error ) then
        success = .false.
        return
    endif

    if ( idstring == answer ) then
        success = .true.

        comm = ipc_open( idstring, location )

    endif

end subroutine ipc_try_connect


! ipc_check_connect --
!     Check if there is a connection file
!
! Arguments:
!     comm       Connection structure
!     server     Name of the server
!     location   Location (directory) to be used
!     success    Whether the connection was achieved or not
!
! Note:
!     This routine is meant for use in the context of the tuple_space
!     module.
!
subroutine ipc_check_connect( comm, server, location, success )
    type(ipc_comm), intent(inout) :: comm
    character(len=*), intent(in)  :: server
    character(len=*), intent(in)  :: location
    logical, intent(out)          :: success

    success = .false.

    inquire( file = trim(location) // '/client-' // trim(server) // ".send", &
        exist = success )

    if ( success ) then
        comm = ipc_open( server, location )
    endif

end subroutine ipc_check_connect


subroutine ipc_cleanup( comm )
    type(ipc_comm)   :: comm

    integer :: ierr

    !
    ! Clean-up in this order
    !
    open( comm%lun, file = comm%recv_file, status = 'old', iostat = ierr )
    if ( ierr == 0 ) then
        close( comm%lun, status = 'delete' )
    endif

    open( comm%lun, file = comm%data_file, status = 'old', iostat = ierr )
    if ( ierr == 0 ) then
        close( comm%lun, status = 'delete' )
    endif

    open( comm%lun, file = comm%send_file, status = 'old', iostat = ierr )
    if ( ierr == 0 ) then
        close( comm%lun, status = 'delete' )
    endif

end subroutine ipc_cleanup


subroutine ipc_send_start( comm, dest, tag, id )
    type(ipc_comm)   :: comm
    character(len=*) :: dest
    character(len=*) :: tag
    integer          :: id

    integer          :: lun

    lun = 10

    write(*,*) 'Send started'

    comm%lun        = lun
    comm%connection = dest
    comm%tag        = tag
    comm%id         = id

    comm%send_file = trim(comm%directory) // trim(comm%me) // "-" // trim(dest) // ".send"
    comm%data_file = trim(comm%directory) // trim(comm%me) // "-" // trim(dest) // ".data"
    comm%recv_file = trim(comm%directory) // trim(comm%me) // "-" // trim(dest) // ".recv"

    !
    ! Clean the send file
    !
    open( lun, file = comm%send_file, form = 'unformatted' )
    write( lun )
    close( lun )

    !
    ! Open the data file
    !
    open( lun, file = comm%data_file, form = 'unformatted' )

end subroutine


subroutine ipc_send_finish( comm )
    type(ipc_comm)   :: comm
    integer          :: lun
    logical          :: okay

    integer          :: ierr

    close( comm%lun )
    open( comm%lun, file = comm%send_file, form = 'unformatted' )
    write( comm%lun ) comm%me, comm%connection, comm%tag, comm%id
    close( comm%lun )

    do
        open( comm%lun, file = comm%recv_file, form = 'unformatted', status = 'old', iostat = ierr )
        write(*,*) 'Send finishing: ', ierr
        if ( ierr == 0 ) then
            read( comm%lun, iostat = ierr ) okay
            close( comm%lun )
            write(*,*) 'Send finishing: ', okay
            if ( ierr == 0 .and. okay ) exit
        endif
        call ipc_sleep( 1000 )
    enddo
    open( comm%lun, file = comm%recv_file, form = 'unformatted', status = 'old', iostat = ierr )
    write( comm%lun )
    close( comm%lun )
    write(*,*) 'Send finished'

end subroutine


subroutine ipc_receive_start( comm, src, tag, id )
    type(ipc_comm)   :: comm
    character(len=*) :: src
    character(len=*) :: tag
    integer          :: id
    integer          :: lun

    character(len=20) :: src_
    character(len=20) :: dest_
    character(len=20) :: tag_
    integer           :: id_

    integer           :: ierr

    lun = 10

    comm%lun        = lun
    comm%connection = src
    comm%tag        = tag

    comm%send_file = trim(comm%directory) // trim(src) // "-" // trim(comm%me) // ".send"
    comm%data_file = trim(comm%directory) // trim(src) // "-" // trim(comm%me) // ".data"
    comm%recv_file = trim(comm%directory) // trim(src) // "-" // trim(comm%me) // ".recv"

    write(*,*) 'Receive started'
    !
    ! Clean the receive file
    !
!    open( lun, file = trim(src) // "-" // trim(comm%me) // ".recv", &
!        form = 'unformatted' )
!    write( lun )
!    close( lun )

    !
    ! Open the send file
    !
    do
        open( lun, file = comm%send_file, form = 'unformatted', status = 'old', iostat = ierr, &
            position = 'rewind' )
        write(*,*) 'Receive starting: ', ierr
        if ( ierr == 0 ) then
            read( lun, iostat = ierr ) src_, dest_, tag_, id_
            close( lun )
            write(*,*) 'Receive starting: (read) ', ierr
            write(*,*) 'Receive starting: (read) ', src_, src
            write(*,*) 'Receive starting: (read) ', dest_, comm%me
            write(*,*) 'Receive starting: (read) ', tag_, tag

            if ( ierr == 0 .and. &
                 src_ == src .and. dest_ .eq. comm%me .and. &
                 tag_ == tag ) then
               exit
            endif
        endif

        call ipc_sleep( 1000 ) ! One second
    enddo

    comm%id         = id_
    id              = id_

    open( lun, file = comm%data_file, form = 'unformatted', status = 'old', iostat = ierr )

end subroutine


subroutine ipc_receive_finish( comm )
    type(ipc_comm)   :: comm
    integer          :: lun
    logical          :: okay

    close( comm%lun )
    open( comm%lun, file = comm%send_file, form = 'unformatted' )
    write( comm%lun )
    close( comm%lun )

    open( comm%lun, file = comm%recv_file, form = 'unformatted' )
    write( comm%lun ) .true.
    close( comm%lun )
    write(*,*) 'Receive finished'

end subroutine

include "ipc_file_data.f90"


! ipc_sleep --
!     Sleep for a number of milliseconds
!
! Arguments:
!     millis          Number of milliseconds
!
subroutine ipc_sleep( millis )
    integer, intent(in) :: millis

    call sleepqq( millis )

end subroutine ipc_sleep

end module ipc_file
