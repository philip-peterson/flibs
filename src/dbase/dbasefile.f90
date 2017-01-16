! dbasefile.f90 --
!     Reader for dBase III files
!
!     Note:
!     Currently it does not support dBase IV files nor is writing
!     supported
!
!     Note:
!     The field types are:
!     N     (Double-precision real) number, use the dvalue field
!     I     Integer number, use the ivalue field
!     L     Logical value (lvalue)
!     S     String value (svalue)
!     D     Date value (stored in svalue)
!
!     Note:
!     Code based on dbfreader.tcl (http://wiki.tcl.tk/29173)
!
!     Limitations:
!     - No memo fields
!     - Ignores status of records (deleted records are simply returned)
!
module dbase_reader
    implicit none

    type dbasefield
        integer           :: width
        integer           :: decimals
        character(len=32) :: name
        character(len=1)  :: type
    end type dbasefield

    type dbasedata
        real(kind=kind(1.0d0)) :: dvalue
        integer                :: ivalue
        integer                :: length
        logical                :: lvalue
        character(len=256)     :: svalue
        character(len=1)       :: type
    end type dbasedata

    type dbasefile
        logical                                     :: opened = .false.
        integer                                     :: number_records
        integer                                     :: record_length
        character(len=1024)                         :: filename
        type(dbasefield), dimension(:), allocatable :: field
        character(len=1), dimension(:), allocatable :: data
    end type dbasefile

contains

! dbase_load --
!     Open the dBase file and load the data
!
! Arguments:
!     db             Database object
!     filename       Name of the file to open
!     error          If true, some error occurred
!
subroutine dbase_load( db, filename, error )
    type(dbasefile)  :: db
    character(len=*) :: filename
    logical          :: error

    integer                         :: ierr
    integer                         :: lun
    integer                         :: i, j
    integer                         :: ncols, nrows, offset, reclen
    character(len=1), dimension(4)  :: byte
    character(len=1), dimension(24) :: header_data
    character(len=1), dimension(32) :: field_definition

    error     = .true.
    db%opened = .false.

    open( newunit = lun, file = filename, status = 'old', access = 'stream', iostat = ierr )

    if ( ierr /= 0 ) then
        return
    endif

    read( lun, iostat = ierr ) byte, nrows, header_data

    if ( ierr /= 0 ) then
        close( lun )
        return
    endif

    if ( ichar(byte(1)) /= 3 ) then
        close( lun )
        return
    endif

    !
    ! Get the offset and the record length
    !
    error       = .false.
    db%filename = filename
    db%opened   = .true.

    offset = ichar(header_data(1)) + ichar(header_data(2)) * 256
    reclen = ichar(header_data(3)) + ichar(header_data(4)) * 256

    ncols  = (offset - 1) / 32 - 1

    db%number_records = nrows
    db%record_length  = reclen

    allocate( db%field(ncols) )

    do i = 1,ncols
        read( lun, iostat = ierr ) field_definition

        !
        ! Extract the name (11 characters), the type (1 character),
        ! skip four unused bytes, then the width and the number of
        ! decimals
        !
        db%field(i)%name = ' '
        do j = 1,10
            if ( ichar(field_definition(j)) /= 0 ) then
                db%field(i)%name(j:j) = field_definition(j)
            else
                exit
            endif
        enddo

        db%field(i)%type     = field_definition(12)
        db%field(i)%width    = ichar(field_definition(17))
        db%field(i)%decimals = ichar(field_definition(18))
        if ( db%field(i)%type == 'N' .and. db%field(i)%decimals == 0 ) then
            db%field(i)%type = 'I'
        endif
    enddo

    !
    ! Now get the actual data for later use
    !
    allocate( db%data(nrows*reclen) )

    read( lun, iostat = ierr ) byte(1)   ! Skip end-of-header marker
    read( lun, iostat = ierr ) db%data
    close( lun )

    if ( ierr /= 0 ) then
        error     = .true.
        db%opened = .false.
        deallocate( db%field )
        deallocate( db%data  )
    endif

end subroutine

! dbase_release --
!     Clean up the dBase file object
!
! Arguments:
!     db             Database object
!
subroutine dbase_release( db )
    type(dbasefile)  :: db

    if ( db%opened ) then
        db%opened = .false.
        deallocate( db%data, db%field )
    endif
end subroutine dbase_release

! dbase_get_fields --
!     Fill an array of type dbasefield with the field information
!
! Arguments:
!     db             Database object
!     field          Array of type dbasefield, to be filled
!     error          True if not opened
!
subroutine dbase_get_fields( db, field, error )
    type(dbasefile)                :: db
    type(dbasefield), dimension(:) :: field
    logical                        :: error

    integer                        :: n

    error = .true.
    if ( db%opened ) then
        n = min( size(field), size(db%field) )
        field(1:n) = db%field(1:n)
        error = .false.
    endif
end subroutine dbase_get_fields

! dbase_number_records --
!     Return the number of records
!
! Arguments:
!     db             Database object
!
integer function dbase_number_records( db )
    type(dbasefile)  :: db

    if ( db%opened ) then
        dbase_number_records = db%number_records
    else
        dbase_number_records = -1
    endif
end function dbase_number_records

! dbase_number_fields --
!     Return the number of fields
!
! Arguments:
!     db             Database object
!
integer function dbase_number_fields( db )
    type(dbasefile)  :: db

    if ( db%opened ) then
        dbase_number_fields = size(db%field)
    else
        dbase_number_fields = -1
    endif
end function dbase_number_fields

! dbase_get_record --
!     Get a particular record
!
! Arguments:
!     db             Database object
!     rec            Record to retrieve
!     data           Array of dbasedata structures
!     error          If true, some error occurred
!
subroutine dbase_get_record( db, rec, data, error )
    type(dbasefile)               :: db
    integer                       :: rec
    type(dbasedata), dimension(:) :: data
    logical                       :: error

    integer                       :: i, j
    integer                       :: offset, bytenr
    character(len=256)            :: buffer

    if ( .not. db%opened ) then
        error = .true.
    elseif ( rec < 1 .or. rec > db%number_records ) then
        error = .true.
    endif
    if ( error ) then
        return
    endif

    offset = db%record_length * (rec - 1) + 1  ! The first character indicates the status of the record
    bytenr = 0

    do i = 1,min(size(data),size(db%field))
        buffer = ' '
        do j = 1,db%field(i)%width
            bytenr      = bytenr + 1
            buffer(j:j) = db%data(offset+bytenr)
            if ( ichar(buffer(j:j)) == 0 ) then
                buffer(j:j) = ' '
                exit
            endif
        enddo
        data(i)%svalue = buffer
        data(i)%type   = db%field(i)%type

        select case ( db%field(i)%type )
            case( 'I' )
                read( buffer, * ) data(i)%ivalue

            case( 'N' )
                read( buffer, * ) data(i)%dvalue

            case( 'L' )
                data%lvalue = index( 'YyTt1', buffer(1:1) ) > 0

            case( 'D', 'S' )
                ! Nothing to do
        end select
    enddo

end subroutine dbase_get_record

end module dbase_reader
