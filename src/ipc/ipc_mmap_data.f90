
subroutine ipc_send_int_scalar( comm, data, error )
    type(ipc_comm) :: comm
    integer       :: data
    logical        :: error

    integer, dimension(1),save :: typeid = 1
    integer, dimension(1)    :: data_
    integer, dimension(3)      :: dims
    integer, dimension(1)      :: length

    error = .true.
    if ( comm%pos + 7 > comm%maxsize ) return
    if ( comm%pos + 7 > comm%maxsize ) return
    error = .false.

    data_(1)  = data
    dims      = 0
    length(1) = 0

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_int_c( comm%idcomm, comm%pos, data_, size(data_) )
    comm%pos = comm%pos + size(data_)

end subroutine ipc_send_int_scalar

subroutine ipc_receive_int_scalar( comm, data, error )
    type(ipc_comm)          :: comm
    integer                :: data
    integer, dimension(1) :: data_
    logical                 :: error
    integer, save           :: typeid = 1
    integer, dimension(1)   :: typeid_
    integer                 :: ierr
    integer, dimension(3)   :: dims_
    integer                 :: length
    integer, dimension(1)   :: length_
    integer                 :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length ) then
        error = .true.
        return
    endif

    call ipc_receive_int_c( comm%idcomm, comm%pos, data_, size(data_) )
    comm%pos = comm%pos + size(length_)

    data = data_(1)

end subroutine ipc_receive_int_scalar

subroutine ipc_send_real_scalar( comm, data, error )
    type(ipc_comm) :: comm
    real       :: data
    logical        :: error

    integer, dimension(1),save :: typeid = 2
    real, dimension(1)    :: data_
    integer, dimension(3)      :: dims
    integer, dimension(1)      :: length

    error = .true.
    if ( comm%pos + 7 > comm%maxsize ) return
    if ( comm%pos + 7 > comm%maxsize ) return
    error = .false.

    data_(1)  = data
    dims      = 0
    length(1) = 0

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_real_c( comm%idcomm, comm%pos, data_, size(data_) )
    comm%pos = comm%pos + size(data_)

end subroutine ipc_send_real_scalar

subroutine ipc_receive_real_scalar( comm, data, error )
    type(ipc_comm)          :: comm
    real                :: data
    real, dimension(1) :: data_
    logical                 :: error
    integer, save           :: typeid = 2
    integer, dimension(1)   :: typeid_
    integer                 :: ierr
    integer, dimension(3)   :: dims_
    integer                 :: length
    integer, dimension(1)   :: length_
    integer                 :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length ) then
        error = .true.
        return
    endif

    call ipc_receive_real_c( comm%idcomm, comm%pos, data_, size(data_) )
    comm%pos = comm%pos + size(length_)

    data = data_(1)

end subroutine ipc_receive_real_scalar

subroutine ipc_send_dbl_scalar( comm, data, error )
    type(ipc_comm) :: comm
    real(kind(1.0d0))       :: data
    logical        :: error

    integer, dimension(1),save :: typeid = 3
    real(kind(1.0d0)), dimension(1)    :: data_
    integer, dimension(3)      :: dims
    integer, dimension(1)      :: length

    error = .true.
    if ( comm%pos + 7 > comm%maxsize ) return
    if ( comm%pos + 7 > comm%maxsize ) return
    error = .false.

    data_(1)  = data
    dims      = 0
    length(1) = 0

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_dbl_c( comm%idcomm, comm%pos, data_, 2*size(data_) )
    comm%pos = comm%pos + 2*size(data_)

end subroutine ipc_send_dbl_scalar

subroutine ipc_receive_dbl_scalar( comm, data, error )
    type(ipc_comm)          :: comm
    real(kind(1.0d0))                :: data
    real(kind(1.0d0)), dimension(1) :: data_
    logical                 :: error
    integer, save           :: typeid = 3
    integer, dimension(1)   :: typeid_
    integer                 :: ierr
    integer, dimension(3)   :: dims_
    integer                 :: length
    integer, dimension(1)   :: length_
    integer                 :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length ) then
        error = .true.
        return
    endif

    call ipc_receive_dbl_c( comm%idcomm, comm%pos, data_, 2*size(data_) )
    comm%pos = comm%pos + size(length_)

    data = data_(1)

end subroutine ipc_receive_dbl_scalar

subroutine ipc_send_log_scalar( comm, data, error )
    type(ipc_comm) :: comm
    logical       :: data
    logical        :: error

    integer, dimension(1),save :: typeid = 4
    logical, dimension(1)    :: data_
    integer, dimension(3)      :: dims
    integer, dimension(1)      :: length

    error = .true.
    if ( comm%pos + 7 > comm%maxsize ) return
    if ( comm%pos + 7 > comm%maxsize ) return
    error = .false.

    data_(1)  = data
    dims      = 0
    length(1) = 0

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_log_c( comm%idcomm, comm%pos, data_, size(data_) )
    comm%pos = comm%pos + size(data_)

end subroutine ipc_send_log_scalar

subroutine ipc_receive_log_scalar( comm, data, error )
    type(ipc_comm)          :: comm
    logical                :: data
    logical, dimension(1) :: data_
    logical                 :: error
    integer, save           :: typeid = 4
    integer, dimension(1)   :: typeid_
    integer                 :: ierr
    integer, dimension(3)   :: dims_
    integer                 :: length
    integer, dimension(1)   :: length_
    integer                 :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length ) then
        error = .true.
        return
    endif

    call ipc_receive_log_c( comm%idcomm, comm%pos, data_, size(data_) )
    comm%pos = comm%pos + size(length_)

    data = data_(1)

end subroutine ipc_receive_log_scalar

subroutine ipc_send_char_scalar( comm, data, error )
    type(ipc_comm) :: comm
    character(len=*)       :: data
    logical        :: error

    integer, dimension(1),save :: typeid = 5
    character(len=len(data)), dimension(1)    :: data_
    integer, dimension(3)      :: dims
    integer, dimension(1)      :: length

    error = .true.
    if ( comm%pos + 7 > comm%maxsize ) return
    if ( comm%pos + 7 > comm%maxsize ) return
    error = .false.

    data_(1)  = data
    dims      = 0
    length(1) = len(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_char_c( comm%idcomm, comm%pos, data_, (len(data)+3)/4 )
    comm%pos = comm%pos + (len(data)+3)/4

end subroutine ipc_send_char_scalar

subroutine ipc_receive_char_scalar( comm, data, error )
    type(ipc_comm)          :: comm
    character(len=*)                :: data
    character(len=len(data)), dimension(1) :: data_
    logical                 :: error
    integer, save           :: typeid = 5
    integer, dimension(1)   :: typeid_
    integer                 :: ierr
    integer, dimension(3)   :: dims_
    integer                 :: length
    integer, dimension(1)   :: length_
    integer                 :: dummy

    error  = .false.
    length = len(data)

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    ! Tricky: it is the contents that counts ...
    if ( typeid_(1) /= typeid .or. length_(1) >  length ) then
        error = .true.
        return
    endif

    call ipc_receive_char_c( comm%idcomm, comm%pos, data_, (length_(1)+3)/4 )
    comm%pos = comm%pos + (length_(1)+3)/4

    data = data_(1)(1:length_(1))

end subroutine ipc_receive_char_scalar

subroutine ipc_send_cmplx_scalar( comm, data, error )
    type(ipc_comm) :: comm
    complex       :: data
    logical        :: error

    integer, dimension(1),save :: typeid = 6
    complex, dimension(1)    :: data_
    integer, dimension(3)      :: dims
    integer, dimension(1)      :: length

    error = .true.
    if ( comm%pos + 7 > comm%maxsize ) return
    if ( comm%pos + 7 > comm%maxsize ) return
    error = .false.

    data_(1)  = data
    dims      = 0
    length(1) = 0

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_cmplx_c( comm%idcomm, comm%pos, data_, 2*size(data_) )
    comm%pos = comm%pos + 2*size(data_)

end subroutine ipc_send_cmplx_scalar

subroutine ipc_receive_cmplx_scalar( comm, data, error )
    type(ipc_comm)          :: comm
    complex                :: data
    complex, dimension(1) :: data_
    logical                 :: error
    integer, save           :: typeid = 6
    integer, dimension(1)   :: typeid_
    integer                 :: ierr
    integer, dimension(3)   :: dims_
    integer                 :: length
    integer, dimension(1)   :: length_
    integer                 :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length ) then
        error = .true.
        return
    endif

    call ipc_receive_cmplx_c( comm%idcomm, comm%pos, data_, 2*size(data_) )
    comm%pos = comm%pos + size(length_)

    data = data_(1)

end subroutine ipc_receive_cmplx_scalar

subroutine ipc_send_int_1d( comm, data, error )
    type(ipc_comm)               :: comm
    integer, dimension(:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 1
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_int_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_int_1d( comm, data, error )
    type(ipc_comm)         :: comm
    integer, dimension(:) :: data
    logical                :: error
    integer, save          :: typeid = 1
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_int_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)

end subroutine ipc_receive_int_1d

subroutine ipc_send_int_2d( comm, data, error )
    type(ipc_comm)               :: comm
    integer, dimension(:,:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 1
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_int_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_int_2d( comm, data, error )
    type(ipc_comm)         :: comm
    integer, dimension(:,:) :: data
    logical                :: error
    integer, save          :: typeid = 1
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_int_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)

end subroutine ipc_receive_int_2d

subroutine ipc_send_int_3d( comm, data, error )
    type(ipc_comm)               :: comm
    integer, dimension(:,:,:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 1
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_int_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_int_3d( comm, data, error )
    type(ipc_comm)         :: comm
    integer, dimension(:,:,:) :: data
    logical                :: error
    integer, save          :: typeid = 1
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_int_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)

end subroutine ipc_receive_int_3d

subroutine ipc_send_real_1d( comm, data, error )
    type(ipc_comm)               :: comm
    real, dimension(:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 2
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_real_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_real_1d( comm, data, error )
    type(ipc_comm)         :: comm
    real, dimension(:) :: data
    logical                :: error
    integer, save          :: typeid = 2
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_real_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)

end subroutine ipc_receive_real_1d

subroutine ipc_send_real_2d( comm, data, error )
    type(ipc_comm)               :: comm
    real, dimension(:,:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 2
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_real_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_real_2d( comm, data, error )
    type(ipc_comm)         :: comm
    real, dimension(:,:) :: data
    logical                :: error
    integer, save          :: typeid = 2
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_real_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)

end subroutine ipc_receive_real_2d

subroutine ipc_send_real_3d( comm, data, error )
    type(ipc_comm)               :: comm
    real, dimension(:,:,:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 2
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_real_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_real_3d( comm, data, error )
    type(ipc_comm)         :: comm
    real, dimension(:,:,:) :: data
    logical                :: error
    integer, save          :: typeid = 2
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_real_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)

end subroutine ipc_receive_real_3d

subroutine ipc_send_dbl_1d( comm, data, error )
    type(ipc_comm)               :: comm
    real(kind(1.0d0)), dimension(:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 3
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = 2*size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_dbl_c( comm%idcomm, comm%pos, data, 2*size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_dbl_1d( comm, data, error )
    type(ipc_comm)         :: comm
    real(kind(1.0d0)), dimension(:) :: data
    logical                :: error
    integer, save          :: typeid = 3
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_dbl_c( comm%idcomm, comm%pos, data, 2*size(data) )
    comm%pos = comm%pos + 2*size(data)

end subroutine ipc_receive_dbl_1d

subroutine ipc_send_dbl_2d( comm, data, error )
    type(ipc_comm)               :: comm
    real(kind(1.0d0)), dimension(:,:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 3
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = 2*size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_dbl_c( comm%idcomm, comm%pos, data, 2*size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_dbl_2d( comm, data, error )
    type(ipc_comm)         :: comm
    real(kind(1.0d0)), dimension(:,:) :: data
    logical                :: error
    integer, save          :: typeid = 3
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_dbl_c( comm%idcomm, comm%pos, data, 2*size(data) )
    comm%pos = comm%pos + 2*size(data)

end subroutine ipc_receive_dbl_2d

subroutine ipc_send_dbl_3d( comm, data, error )
    type(ipc_comm)               :: comm
    real(kind(1.0d0)), dimension(:,:,:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 3
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = 2*size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_dbl_c( comm%idcomm, comm%pos, data, 2*size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_dbl_3d( comm, data, error )
    type(ipc_comm)         :: comm
    real(kind(1.0d0)), dimension(:,:,:) :: data
    logical                :: error
    integer, save          :: typeid = 3
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_dbl_c( comm%idcomm, comm%pos, data, 2*size(data) )
    comm%pos = comm%pos + 2*size(data)

end subroutine ipc_receive_dbl_3d

subroutine ipc_send_log_1d( comm, data, error )
    type(ipc_comm)               :: comm
    logical, dimension(:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 4
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_log_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_log_1d( comm, data, error )
    type(ipc_comm)         :: comm
    logical, dimension(:) :: data
    logical                :: error
    integer, save          :: typeid = 4
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_log_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)

end subroutine ipc_receive_log_1d

subroutine ipc_send_log_2d( comm, data, error )
    type(ipc_comm)               :: comm
    logical, dimension(:,:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 4
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_log_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_log_2d( comm, data, error )
    type(ipc_comm)         :: comm
    logical, dimension(:,:) :: data
    logical                :: error
    integer, save          :: typeid = 4
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_log_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)

end subroutine ipc_receive_log_2d

subroutine ipc_send_log_3d( comm, data, error )
    type(ipc_comm)               :: comm
    logical, dimension(:,:,:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 4
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_log_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_log_3d( comm, data, error )
    type(ipc_comm)         :: comm
    logical, dimension(:,:,:) :: data
    logical                :: error
    integer, save          :: typeid = 4
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_log_c( comm%idcomm, comm%pos, data, size(data) )
    comm%pos = comm%pos + size(data)

end subroutine ipc_receive_log_3d

subroutine ipc_send_char_1d( comm, data, error )
    type(ipc_comm)               :: comm
    character(len=*), dimension(:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 5
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = len(data(1))
    sizedata = (size(data)*len(data)+3)/4

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_char_c( comm%idcomm, comm%pos, data, (size(data)*len(data)+3)/4 )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_char_1d( comm, data, error )
    type(ipc_comm)         :: comm
    character(len=*), dimension(:) :: data
    logical                :: error
    integer, save          :: typeid = 5
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = len(data(1))

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_char_c( comm%idcomm, comm%pos, data, (size(data)*len(data)+3)/4 )
    comm%pos = comm%pos + (size(data)*len(data)+3)/4

end subroutine ipc_receive_char_1d

subroutine ipc_send_char_2d( comm, data, error )
    type(ipc_comm)               :: comm
    character(len=*), dimension(:,:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 5
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = len(data(1,1))
    sizedata = (size(data)*len(data)+3)/4

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_char_c( comm%idcomm, comm%pos, data, (size(data)*len(data)+3)/4 )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_char_2d( comm, data, error )
    type(ipc_comm)         :: comm
    character(len=*), dimension(:,:) :: data
    logical                :: error
    integer, save          :: typeid = 5
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = len(data(1,1))

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_char_c( comm%idcomm, comm%pos, data, (size(data)*len(data)+3)/4 )
    comm%pos = comm%pos + (size(data)*len(data)+3)/4

end subroutine ipc_receive_char_2d

subroutine ipc_send_char_3d( comm, data, error )
    type(ipc_comm)               :: comm
    character(len=*), dimension(:,:,:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 5
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = len(data(1,1,1))
    sizedata = (size(data)*len(data)+3)/4

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_char_c( comm%idcomm, comm%pos, data, (size(data)*len(data)+3)/4 )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_char_3d( comm, data, error )
    type(ipc_comm)         :: comm
    character(len=*), dimension(:,:,:) :: data
    logical                :: error
    integer, save          :: typeid = 5
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = len(data(1,1,1))

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_char_c( comm%idcomm, comm%pos, data, (size(data)*len(data)+3)/4 )
    comm%pos = comm%pos + (size(data)*len(data)+3)/4

end subroutine ipc_receive_char_3d

subroutine ipc_send_cmplx_1d( comm, data, error )
    type(ipc_comm)               :: comm
    complex, dimension(:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 6
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = 2*size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_cmplx_c( comm%idcomm, comm%pos, data, 2*size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_cmplx_1d( comm, data, error )
    type(ipc_comm)         :: comm
    complex, dimension(:) :: data
    logical                :: error
    integer, save          :: typeid = 6
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_cmplx_c( comm%idcomm, comm%pos, data, 2*size(data) )
    comm%pos = comm%pos + 2*size(data)

end subroutine ipc_receive_cmplx_1d

subroutine ipc_send_cmplx_2d( comm, data, error )
    type(ipc_comm)               :: comm
    complex, dimension(:,:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 6
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = 2*size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_cmplx_c( comm%idcomm, comm%pos, data, 2*size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_cmplx_2d( comm, data, error )
    type(ipc_comm)         :: comm
    complex, dimension(:,:) :: data
    logical                :: error
    integer, save          :: typeid = 6
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_cmplx_c( comm%idcomm, comm%pos, data, 2*size(data) )
    comm%pos = comm%pos + 2*size(data)

end subroutine ipc_receive_cmplx_2d

subroutine ipc_send_cmplx_3d( comm, data, error )
    type(ipc_comm)               :: comm
    complex, dimension(:,:,:) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = 6
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = 0
    sizedata = 2*size(data)

    error = .true.
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    if ( comm%pos + 6 + sizedata > comm%maxsize ) return
    error = .false.

    dims = 0
    dims(1:size(shape(data))) = size(data)

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_cmplx_c( comm%idcomm, comm%pos, data, 2*size(data) )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_cmplx_3d( comm, data, error )
    type(ipc_comm)         :: comm
    complex, dimension(:,:,:) :: data
    logical                :: error
    integer, save          :: typeid = 6
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = 0

    call ipc_receive_int_c( comm%idcomm, comm%pos, typeid_, size(typeid_) )
    comm%pos = comm%pos + size(typeid_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, dims_, size(dims_) )
    comm%pos = comm%pos + size(dims_)

    call ipc_receive_int_c( comm%idcomm, comm%pos, length_, size(length_) )
    comm%pos = comm%pos + size(length_)

    if ( typeid_(1) /= typeid .or. length_(1) /= length .or. &
         any( dims_(1:size(shape(data))) /= size(data) ) ) then
        error = .true.
        return
    endif

    call ipc_receive_cmplx_c( comm%idcomm, comm%pos, data, 2*size(data) )
    comm%pos = comm%pos + 2*size(data)

end subroutine ipc_receive_cmplx_3d
