! tupleserver.f90 --
!      Tuple server based on the IPC modules
!
!      *** UNDER CONSTRUCTION ***
!
!      NOTE:
!      - Synchronous reads/ins must be stored until a tuple appears that
!        matches their pattern
!
program tupleserver
    use tuplespaces

    implicit none

    character(len=80)                             :: directory_name
    character(len=40)                             :: server_name
    integer                                       :: buffer_size

    type(ipc_comm)                                :: comm
    type(tuple_data),       dimension(:), pointer :: tuple
    type(tuple_connection), dimension(:), pointer :: connection

    integer :: i

    allocate( connection(0) )

    allocate( tuple(100) )

    open( 10, file = 'tupleserver.inp' )
    read( 10, * ) directory_name
    read( 10, * ) server_name
    read( 10, * ) buffer_size
    close( 10 )

    !
    ! Initialise the IPC mechanism
    !
    !TODO: set buffer size (mmap), directory (file, mmap)

    comm = ipc_open( server_name, directory_name )
    call ipc_cleanup( comm )


    !
    ! Infinite loop:
    ! - Look for incoming or closing connections
    ! - Look for new requests from exisintg connections
    !
    do
        call handle_connections

        do i = 1,size(connection)
             call handle_request( connection(i) )
        enddo
    enddo

contains

! handle_connections
!     Look for any new connections
!
! Arguments:
!     None
!
subroutine handle_connections
    type(tuple_connection)                        :: comm
    type(tuple_connection), dimension(:), pointer :: new_connection
    character(len=40)                             :: space
    integer                                       :: i
    integer                                       :: idx
    logical                                       :: success
    logical                                       :: found
                                                  ! location!
    call tuple_server_connect( comm, server_name, directory_name, space, success )

    if ( success ) then
        found = .false.
        do i = 1,size(connection)
            if ( .not. connection(i)%active ) then
                found             = .true.
                idx               = i
                exit
            endif
        enddo

        if ( .not. found ) then
            allocate( new_connection(size(connection)+1) )
            new_connection(1:size(connection)) = connection
            deallocate( connection )
            connection => new_connection
            idx = size(connection)
        endif

        connection(idx)        = comm
        connection(idx)%active = .true.

        write(*,*) 'Connection succeeded: ', comm
    endif
end subroutine handle_connections

subroutine handle_request( comm )
    type(tuple_connection) :: comm

    character(len=20)      :: src
    character(len=20)      :: dest
    character(len=20)      :: tag
    integer                :: id
    integer                :: i
    integer                :: number_elems
    logical                :: success
    logical                :: error
    logical                :: match
    type(tuple_data)       :: new_data

    call ipc_check_receive( comm%comm, src, dest, tag, id, success )

    if ( success ) then
        call ipc_receive_start( comm%comm, src, dest, tag, id )
        call ipc_receive_data(  comm%comm, number_elems, error )

        if ( .not. error ) then
            allocate( new_data%elem(number_elems) )

            do i = 1,number_elems
                call tuple_receive_elem( comm%comm, new_data%elem(i) )
            enddo
            call ipc_receive_finish( comm%comm )

            if ( tag == 'OUT' ) then

                call add_tuple( new_data )

            elseif ( tag == 'IN' .or. tag == 'READ' ) then

                do id = 1,size(tuple)
                    match = tuple_match( tuple(id), new_data )
                    if ( match ) then
                        exit
                    endif
                enddo

                if ( match ) then
                    tag = 'SEND'
                    number_elems = size( tuple(id)%elem )
                    call ipc_send_start( comm%comm, dest, src, tag, id )
                    call ipc_send_data(  comm%comm, number_elems, error )

                    do i = 1,number_elems
                        call tuple_send_elem( comm%comm, tuple(id)%elem(i) )
                    enddo

                    call ipc_send_finish( comm%comm )

                    if ( tag == 'READ' ) then
                        call tuple_free( tuple(id) )
                    endif
                endif
            endif

        endif
    endif

end subroutine handle_request

subroutine add_tuple( new_data )

    type(tuple_data), intent(in)            :: new_data

    type(tuple_data), dimension(:), pointer :: new_tuple

    integer                                 :: i
    integer                                 :: id
    integer                                 :: new_size
    logical                                 :: found

    do i = 1,size(tuple)
        if ( .not. associated( tuple(i)%elem ) ) then
            found = .true.
            id    = i
        endif
    enddo

    if ( .not. found ) then
        new_size = size(tuple) + 100

        allocate( new_tuple(new_size) )

        new_tuple(1:size(tuple)) = tuple
        id = size(tuple) + 1

        deallocate( tuple )
        tuple => new_tuple
    endif

    tuple(id) = new_data

end subroutine add_tuple

end program
