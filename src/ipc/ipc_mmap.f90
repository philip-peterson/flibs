! ipc_mmap.f90 --
!     Simple method for inter-process communication via memory-mapped files
!
! ipc_mmap --
!     Module for IPC via file I/O
!
! NOTES:
!     Start in separate DOS-boxes
!     Order of starting important - right now!
!
! NOTE:
!     The first two integers are reserved:
!     - integer 0: if set to 1, then data are being sent
!                  if set to 2, then send is completed
!                  (the routine will wait until integer 1 is set to 2
!                  and then it resets integer 0 to 0)

!     - integer 1: if set to 1, then data are being received
!                  if set to 2, then receive is completed
!                  (the routine will wait until integer 0 is set to 0
!                  and then it resets integer 1 to 0)
!
module ipc_mmap

    implicit none

    type ipc_comm
        character(len=20) :: me          ! Identifying string for calling program
        character(len=20) :: connection  ! Id for connecting program
        character(len=20) :: tag         ! Tag for the data (type)
        integer           :: id          ! Numerical identfier
        integer           :: idcomm      ! Communication ID
        integer           :: maxsize     ! Maximum size for the mmap'ed file
        integer           :: pos         ! Current position in the file
        logical           :: in_progress ! Send/receive is in progress
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
        module procedure ipc_receive_char_scalar
        module procedure ipc_receive_char_1d
        module procedure ipc_receive_char_2d
        module procedure ipc_receive_char_3d
        module procedure ipc_receive_cmplx_scalar
        module procedure ipc_receive_cmplx_1d
        module procedure ipc_receive_cmplx_2d
        module procedure ipc_receive_cmplx_3d
    end interface

    !
    ! Interface to the C routines
    !
    interface
        subroutine fsleep( millis )
            integer          :: millis
        end subroutine fsleep

        subroutine ipc_start_c( send, src, dest, maxsize, idcomm )
            integer          :: send
            character(len=*) :: src
            character(len=*) :: dest
            integer          :: maxsize
            integer          :: idcomm
        end subroutine ipc_start_c

        subroutine ipc_get_data_c( idcomm, pos, value )
            integer          :: idcomm
            integer          :: pos
            integer          :: value
        end subroutine ipc_get_data_c

        subroutine ipc_set_data_c( idcomm, pos, value )
            integer          :: idcomm
            integer          :: pos
            integer          :: value
        end subroutine ipc_set_data_c

        subroutine ipc_send_int_c( idcomm, pos, data, number )
            integer               :: idcomm
            integer               :: pos
            integer, dimension(*) :: data
            integer               :: number
        end subroutine ipc_send_int_c

        subroutine ipc_receive_int_c( idcomm, pos, data, number )
            integer               :: idcomm
            integer               :: pos
            integer, dimension(*) :: data
            integer               :: number
        end subroutine ipc_receive_int_c

        subroutine ipc_send_real_c( idcomm, pos, data, number )
            integer               :: idcomm
            integer               :: pos
            real, dimension(*)    :: data
            integer               :: number
        end subroutine ipc_send_real_c

        subroutine ipc_receive_real_c( idcomm, pos, data, number )
            integer               :: idcomm
            integer               :: pos
            real, dimension(*)    :: data
            integer               :: number
        end subroutine ipc_receive_real_c

        subroutine ipc_send_dbl_c( idcomm, pos, data, number )
            integer               :: idcomm
            integer               :: pos
            real(kind=kind(1.0d0)), dimension(*) :: data
            integer               :: number
        end subroutine ipc_send_dbl_c

        subroutine ipc_receive_dbl_c( idcomm, pos, data, number )
            integer               :: idcomm
            integer               :: pos
            real(kind=kind(1.0d0)), dimension(*) :: data
            integer               :: number
        end subroutine ipc_receive_dbl_c

        subroutine ipc_send_log_c( idcomm, pos, data, number )
            integer               :: idcomm
            integer               :: pos
            logical, dimension(*) :: data
            integer               :: number
        end subroutine ipc_send_log_c

        subroutine ipc_receive_log_c( idcomm, pos, data, number )
            integer               :: idcomm
            integer               :: pos
            logical, dimension(*) :: data
            integer               :: number
        end subroutine ipc_receive_log_c

        subroutine ipc_send_char_c( idcomm, pos, data, number )
            integer               :: idcomm
            integer               :: pos
            character(len=*), dimension(*) :: data
            integer               :: number
        end subroutine ipc_send_char_c

        subroutine ipc_receive_char_c( idcomm, pos, data, number )
            integer               :: idcomm
            integer               :: pos
            character(len=*), dimension(*) :: data
            integer               :: number
        end subroutine ipc_receive_char_c

        subroutine ipc_receive_complx_c( idcomm, pos, data, number )
            integer               :: idcomm
            integer               :: pos
            complex, dimension(*) :: data
            integer               :: number
        end subroutine ipc_receive_complx_c

        subroutine ipc_send_complx_c( idcomm, pos, data, number )
            integer               :: idcomm
            integer               :: pos
            complex, dimension(*) :: data
            integer               :: number
        end subroutine ipc_send_complx_c
    end interface

contains

! ipc_open --
!     Initialise the connection on "this" side
!
! Arguments:
!     me         String identifying the calling process
!     maxsize    Maximum total size of a message (default: 204800 bytes)
!
! Result:
!     Initialised structure ready for further communications
!
type(ipc_comm) function ipc_open( me, maxsize )
    character(len=*)  :: me
    integer, optional :: maxsize

    ipc_open%me = me
    if ( present(maxsize) ) then
        ipc_open%maxsize = maxsize
    else
        ipc_open%maxsize = 204800
    endif
    ipc_open%in_progress = .false.
end function ipc_open

! ipc_try_connect --
!     Try to open the connection to another process
!
! Arguments:
!     comm       Initialised connection structure
!     dest       Identifying string for that other process
!     idstring   Unique identifier to check the connection
!     success    Whether the connection was achieved or not
!
! Note:
!     This routine is meant for use in the context of the tuple_space
!     module.
!
subroutine ipc_try_connect( comm, dest, idstring, success )
    type(ipc_comm)   :: comm
    character(len=*) :: dest
    character(len=*) :: idstring
    logical          :: success

    integer                      :: ierr
    character(len=len(idstring)) :: answer
    logical                      :: error

    success = .false.

    !
    ! Open the send file - but it must not yet exist
    !
!!  TODO!!!
!!  lun = 10
!!
!!  open( lun, file = trim(comm%me) // "-" // trim(dest) // ".send", &
!!      form = 'unformatted', status = 'new', iostat = ierr )
!!
!!  if ( ierr /= 0 ) then
!!      return
!!  endif
!!  close( lun )

    call ipc_send_start(  comm, dest, idstring, 0 )
    call ipc_send_data(   comm, idstring, error )
    call ipc_send_finish( comm )

    !
    ! Now wait for the _same_ string to be returned
    !

    call ipc_receive_start(  comm, dest, idstring, 0 )
    call ipc_receive_data(   comm, answer, error )
    call ipc_receive_finish( comm )

    if ( idstring == answer ) then
        success = .true.
    endif

end subroutine ipc_try_connect

subroutine ipc_cleanup( comm )
    type(ipc_comm)   :: comm

    integer          :: value
    !
    ! Wait until the other party is finished receiving the information
    !
    do
        call ipc_get_data_c( comm%idcomm, 1, value )
        if ( value == 2 ) then
            call ipc_set_data_c( comm%idcomm, 0, 0 )
            exit
        endif
        call fsleep( 1000 )
    enddo

!!  TODO: release the mmapped file

end subroutine ipc_cleanup

! ipc_send_start --
!     Start sending the information
!
! Arguments:
!     comm           Communication information
!     dest           Name of destination
!     tag            Tag identifying the type of information to be sent
!     id             Numerical identifier
!
subroutine ipc_send_start( comm, dest, tag, id )
    type(ipc_comm)   :: comm
    character(len=*) :: dest
    character(len=*) :: tag
    integer          :: id

    integer          :: idcomm
    integer          :: value
    logical          :: error

    call ipc_start_c( 1, trim(comm%me), trim(dest), comm%maxsize, idcomm )
    comm%in_progress = .true.

    !
    ! Wait until the communication channel is free
    !
    do
        call ipc_get_data_c( idcomm, 0, value )
        if ( value == 0 ) then
            call ipc_set_data_c( idcomm, 0, 2 )
            exit
        endif

        call fsleep( 1 )
    enddo

    !
    ! Fill in the communication data
    !
    comm%connection = dest
    comm%tag        = tag
    comm%id         = id
    comm%idcomm     = idcomm
    comm%pos        = 2        ! Use C convention

    !
    ! Send the header information
    !
    call ipc_send_data( comm, comm%me, error )
    call ipc_send_data( comm, dest,    error )
    call ipc_send_data( comm, tag,     error )
    call ipc_send_data( comm, id,      error )

    call ipc_get_data_c( idcomm, 0, value )
end subroutine

! ipc_send_finish --
!     Close the message, allowing the other party to retrieve it
!
! Arguments:
!     comm           Communication information
!
subroutine ipc_send_finish( comm )
    type(ipc_comm)   :: comm
    integer          :: lun
    logical          :: okay

    integer          :: value

    call ipc_set_data_c( comm%idcomm, 0, 1 )
    comm%in_progress = .false.
    call ipc_get_data_c( comm%idcomm, 0, value )

end subroutine

! ipc_receive_start --
!     Start receiving the information
!
! Arguments:
!     comm           Communication information
!     src            Name of the source
!     tag            Tag identifying the type of information to be sent
!     id             Numerical identifier
!
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

    logical           :: error
    integer           :: value

    comm%connection = src
    comm%tag        = tag

    call ipc_start_c( 0, trim(src), trim(comm%me), comm%maxsize, comm%idcomm )
    comm%in_progress = .true.
    comm%pos         = 2        ! Use C convention

    do
        call ipc_get_data_c( comm%idcomm, 0, value )
        if ( value == 1 ) then
            !
            ! Retrieve the header ...
            !
            call ipc_receive_data( comm, src_,  error )
            call ipc_receive_data( comm, dest_, error )
            call ipc_receive_data( comm, tag_,  error )
            call ipc_receive_data( comm, id_,   error )

            if ( .not. error .and. &
                 src_ == src .and. dest_ .eq. comm%me .and. &
                 tag_ == tag ) then
               exit
            endif
        endif

        call fsleep( 1 )
    enddo

    comm%id         = id_
    id              = id_

end subroutine

! ipc_receive_finish --
!     Close the message, allowing the other party to continue (it might
!     be blocked on the next message)
!
! Arguments:
!     comm           Communication information
!

subroutine ipc_receive_finish( comm )
    type(ipc_comm)   :: comm

    call ipc_set_data_c( comm%idcomm, 0, 0 )
    comm%in_progress = .false.

end subroutine

include "ipc_mmap_data.f90"

end module ipc_mmap
