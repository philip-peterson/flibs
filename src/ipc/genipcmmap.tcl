# genipcmmap.tcl --
#     Generate the code for the various specific send/receive routines
#     (A lot of it is completely generic)
#
#     Note:
#     There is a problem with the character version: this is less
#     generic than I hoped for. Hence the "_dummy" part in the file name
#
set outfile [open "ipc_mmap_data_dummy.f90" w]

#
# Scalar variants
#
foreach {
    typedecl          typedeclx                 typeid typen length    size} {
    integer           integer                   1      int   0         size(data_)
    real              real                      2      real  0         size(data_)
    real(kind(1.0d0)) real(kind(1.0d0))         3      dbl   0         2*size(data_)
    logical           logical                   4      log   0         size(data_)
    character(len=*)  character(len=len(data))  5      char  len(data) (len(data)+3)/4
    complex           complex                   6      cmplx 0         2*size(data_)
    } {
    set subst [list TYPEDECLX $typedeclx TYPEDECL $typedecl TYPEID $typeid \
                    TYPEN $typen LENGTH $length SIZE $size]

    puts $outfile [string map $subst {
subroutine ipc_send_TYPEN_scalar( comm, data, error )
    type(ipc_comm) :: comm
    TYPEDECL       :: data
    logical        :: error

    integer, dimension(1),save :: typeid = TYPEID
    TYPEDECLX, dimension(1)    :: data_
    integer, dimension(3)      :: dims
    integer, dimension(1)      :: length

    error = .true.
    if ( comm%pos + 7 > comm%maxsize ) return
    if ( comm%pos + 7 > comm%maxsize ) return
    error = .false.

    data_(1)  = data
    dims      = 0
    length(1) = LENGTH

    call ipc_send_int_c( comm%idcomm, comm%pos, typeid, 1 )
    comm%pos = comm%pos + size(typeid)

    call ipc_send_int_c( comm%idcomm, comm%pos, dims,   3 )
    comm%pos = comm%pos + size(dims)

    call ipc_send_int_c( comm%idcomm, comm%pos, length, 1 )
    comm%pos = comm%pos + size(length)

    call ipc_send_TYPEN_c( comm%idcomm, comm%pos, data_, SIZE )
    comm%pos = comm%pos + SIZE

end subroutine ipc_send_TYPEN_scalar

subroutine ipc_receive_TYPEN_scalar( comm, data, error )
    type(ipc_comm)          :: comm
    TYPEDECL                :: data
    TYPEDECLX, dimension(1) :: data_
    logical                 :: error
    integer, save           :: typeid = TYPEID
    integer, dimension(1)   :: typeid_
    integer                 :: ierr
    integer, dimension(3)   :: dims_
    integer                 :: length
    integer, dimension(1)   :: length_
    integer                 :: dummy

    error  = .false.
    length = LENGTH

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

    call ipc_receive_TYPEN_c( comm%idcomm, comm%pos, data_, SIZE )
    comm%pos = comm%pos + size(length_)

    data = data_(1)

end subroutine ipc_receive_TYPEN_scalar}]
}

#
# Array variants
#
foreach {
    typedecl           typeid typen dimdecl dimn   length            size} {
    integer            1      int   :       1d     0                 size(data)
    integer            1      int   :,:     2d     0                 size(data)
    integer            1      int   :,:,:   3d     0                 size(data)
    real               2      real  :       1d     0                 size(data)
    real               2      real  :,:     2d     0                 size(data)
    real               2      real  :,:,:   3d     0                 size(data)
    real(kind(1.0d0))  3      dbl   :       1d     0                 2*size(data)
    real(kind(1.0d0))  3      dbl   :,:     2d     0                 2*size(data)
    real(kind(1.0d0))  3      dbl   :,:,:   3d     0                 2*size(data)
    logical            4      log   :       1d     0                 size(data)
    logical            4      log   :,:     2d     0                 size(data)
    logical            4      log   :,:,:   3d     0                 size(data)
    character(len=*)   5      char  :       1d     len(data(1))      (size(data)*len(data)+3)/4
    character(len=*)   5      char  :,:     2d     len(data(1,1))    (size(data)*len(data)+3)/4
    character(len=*)   5      char  :,:,:   3d     len(data(1,1,1))  (size(data)*len(data)+3)/4
    complex            6      cmplx :       1d     0                 2*size(data)
    complex            6      cmplx :,:     2d     0                 2*size(data)
    complex            6      cmplx :,:,:   3d     0                 2*size(data)
    } {
    set subst [list TYPEDECL $typedecl TYPEDECLX $typedeclx TYPEID $typeid TYPEN $typen \
                    DIMDECL $dimdecl DIMN $dimn LENGTH $length SIZE $size]

    puts $outfile [string map $subst {
subroutine ipc_send_TYPEN_DIMN( comm, data, error )
    type(ipc_comm)               :: comm
    TYPEDECL, dimension(DIMDECL) :: data
    logical                      :: error

    integer, dimension(1), save  :: typeid = TYPEID
    integer, dimension(1)        :: length
    integer, dimension(3)        :: dims
    integer                      :: sizedata
    integer                      :: i

    length   = LENGTH
    sizedata = SIZE

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

    call ipc_send_TYPEN_c( comm%idcomm, comm%pos, data, SIZE )
    comm%pos = comm%pos + size(data)
end subroutine

subroutine ipc_receive_TYPEN_DIMN( comm, data, error )
    type(ipc_comm)         :: comm
    TYPEDECL, dimension(DIMDECL) :: data
    logical                :: error
    integer, save          :: typeid = TYPEID
    integer, dimension(1)  :: typeid_
    integer                :: ierr
    integer, dimension(3)  :: dims_
    integer                :: length
    integer, dimension(1)  :: length_
    integer                :: dummy

    error  = .false.
    length = LENGTH

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

    call ipc_receive_TYPEN_c( comm%idcomm, comm%pos, data, SIZE )
    comm%pos = comm%pos + SIZE

end subroutine ipc_receive_TYPEN_DIMN}]
}

close $outfile
