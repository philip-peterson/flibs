! fodbc.90 --
!     Fortran interface to ODBC
!
!     $Id: fodbc.f90,v 1.11 2013/03/05 12:05:13 arjenmarkus Exp $
!
!     TODO:
!     - odbc_prepare_select: more robust method to compute the string length needed
!                            split up because of extra_clause
!
module odbc_types
    implicit none

    integer, parameter         :: dp = kind(1.0d0)

    integer, parameter         :: ODBC_INT     = 1
    integer, parameter         :: ODBC_REAL    = 2
    integer, parameter         :: ODBC_DOUBLE  = 3
    integer, parameter         :: ODBC_CHAR    = 4
    integer, parameter         :: ODBC_BINARY  = -3 ! For BLOBs - SQL_VARBINARY

    !
    ! Note: these must be kept in sync with the C code!
    !
    integer, parameter         :: ODBC_NO_BLOB_SUPPORT = -1 ! BLOBs: no support
    integer, parameter         :: ODBC_PLAIN_BYTES     =  0 ! BLOBs: BLOB(..) and raw bytes returned
    integer, parameter         :: ODBC_POSTGRESQL_HEX  =  1 ! BLOBs: BYTEA and hexadecimal string returned

    integer, parameter         :: ODBC_ROW     = 100
    integer, parameter         :: ODBC_ERRCODE = 999

    character(len=40), parameter :: ODBC_MSACCESS   = "MicroSoft Access Driver (*.mdb)"
    character(len=40), parameter :: ODBC_MSEXCEL    = "MicroSoft Excel Driver (*.xls)"
    character(len=40), parameter :: ODBC_SQLITE3    = "SQLite3 ODBC Driver"
    character(len=40), parameter :: ODBC_POSTGRESQL = "PostGreSQL Driver"

    type ODBC_STATEMENT
        integer, dimension(2)   :: stmt_handle
        integer                 :: blob_type
    end type ODBC_STATEMENT

    type ODBC_DATABASE
        integer, dimension(2)   :: db_handle   = 0
        integer, dimension(2)   :: stmt_handle = 0 ! For querying table names
        integer                 :: blob_type   = ODBC_PLAIN_BYTES
        integer                 :: error
        character(len=80)       :: errmsg
    end type ODBC_DATABASE

    type ODBC_BLOB
        integer                            :: set_size    = 0 ! Size as set via odbc_column_props
        integer                            :: actual_size = 0 ! Size in bytes of the actual data
        integer, dimension(:), allocatable :: data            ! Using an integer array as convenient storage
    end type ODBC_BLOB

    type ODBC_COLUMN
        character(len=40)       :: name     = ' '
        character(len=40)       :: type     = ' '
        character(len=40)       :: function = ' '
        integer                 :: type_set
        integer                 :: int_value
        real(kind=dp)           :: double_value
        character(len=80)       :: char_value
        type(ODBC_BLOB)         :: blob_value
    end type ODBC_COLUMN
end module odbc_types

module odbc
    use odbc_types

    implicit none

    private :: typename
    private :: column_func
    private :: stringtof
    private :: stringtoc

    interface odbc_open
        module procedure odbc_open_dsn
        module procedure odbc_open_file
    end interface

    interface odbc_errmsg
        module procedure odbc_errmsg_db
        module procedure odbc_errmsg_stmt
    end interface

    interface odbc_errmsg_print
        module procedure odbc_errmsg_print_db
        module procedure odbc_errmsg_print_stmt
    end interface

   !
   ! Convenient interfaces
   !
   interface odbc_set_column
      module procedure odbc_set_column_int
      module procedure odbc_set_column_real
      module procedure odbc_set_column_double
      module procedure odbc_set_column_char
      module procedure odbc_set_column_blob
   end interface
   interface odbc_get_column
      module procedure odbc_get_column_int
      module procedure odbc_get_column_real
      module procedure odbc_get_column_double
      module procedure odbc_get_column_char
      module procedure odbc_get_column_blob
   end interface

contains

! typename --
!    Construct the type and attributes of a column
!    in a new table
! Arguments:
!    column        Column information
!    primary       Name of the primary key
!
character(len=40) function typename( column, primary, blob_type )
   type(ODBC_COLUMN), intent(in) :: column
   character(len=*), intent(in)  :: primary
   integer, intent(in)           :: blob_type

   if ( column%type(1:4) == 'BLOB' .and. blob_type == ODBC_POSTGRESQL_HEX ) then
       typename = 'BYTEA'
   else if ( column%name .ne. primary ) then
      typename = column%type
   else
      !write( typename, '(2a)' ) trim(column%type), ' primary key'
      typename = trim(column%type) // ' primary key'
   endif

end function typename


! column_func --
!    Construct the name and function of a column
!    in a new table
! Arguments:
!    column        Column information
!
character(len=80) function column_func( column )
   type(ODBC_COLUMN), intent(in) :: column

   if ( column%function .ne. ' ' ) then
      column_func = trim(column%function) // '(' // trim(column%name) // ')'
   else
      column_func = column%name
   endif

end function column_func


! stringtof --
!    Convert a C string to Fortran
! Arguments:
!    string        String to be converted
!
subroutine stringtof( string )
   character(len=*) :: string

   integer          :: last
   last = index( string, char(0) )
   if ( last .gt. 0 ) then
      string(last:) = ' '
   endif

end subroutine stringtof


! stringtoc --
!    Convert a Fortran string to C
! Arguments:
!    string        String to be converted
! Note:
!    It is assumed that the last character
!    is a space. As this is a private
!    routine, this should have been taken
!    care of in the caller.
!
subroutine stringtoc( string )
   character(len=*) :: string

   integer          :: last

   last = 1 + len_trim(string)
   string(last:last) = char(0)

end subroutine stringtoc


! odbc_column_props --
!    Convenience routine to set the properties of a column
! Arguments:
!    column        Column structure
!    name          Name of the column
!    type          Type of the column
!    length        Length if a string
! Side effects:
!    Fields in column filled
!
subroutine odbc_column_props( column, name, type, length )
   type(ODBC_COLUMN), intent(inout) :: column
   character(len=*), intent(in)       :: name
   integer, intent(in)                :: type
   integer, intent(in), optional      :: length

   integer                            :: length_
   character(len=40)                  :: type_expr

   length_ = 20
   if ( present(length) ) then
      length_ = length
   endif

   column%name     = name
   column%type_set = type

   select case ( type )
   case (ODBC_INT)
      column%type = 'INT'
   case (ODBC_REAL)
      column%type = 'FLOAT'
   case (ODBC_DOUBLE)
      column%type = 'DOUBLE'
   case (ODBC_CHAR)
      write( column%type, '(a,i0,a)' ) 'CHAR(', length_, ')'
   case (ODBC_BINARY)
      write( column%type, '(a,i0,a)' ) 'BLOB(', length_, ')'
      column%blob_value%set_size = length_
   case default
      column%type = 'UNKNOWN!'
   end select

end subroutine odbc_column_props


! odbc_column_query --
!    Convenience routine to query a column or a function of that column
! Arguments:
!    column        Column structure
!    name          Name of the column
!    type          Type of the column
!    length        Length if a string (optional)
!    function      Name of the function to apply (if any)
! Side effects:
!    Fields in column filled
!
subroutine odbc_column_query( column, name, type, length, function )
   type(ODBC_COLUMN), intent(inout)     :: column
   character(len=*), intent(in)           :: name
   integer, intent(in)                    :: type
   integer, intent(in), optional          :: length
   character(len=*), intent(in), optional :: function

   column%function = ' '
   if ( present(function) ) then
      column%function = function
   endif
   if ( present(length) ) then
      call odbc_column_props( column, name, type, length )
   else
      call odbc_column_props( column, name, type )
   endif

end subroutine odbc_column_query


! odbc_set_column_int    --
! odbc_set_column_real   --
! odbc_set_column_double --
! odbc_set_column_char   --
! odbc_set_column_blob   --
!    Convenience routines to set the value of a column
! Arguments:
!    column        Column structure
!    value         The value to be set
! Side effects:
!    Appropriate value field in column set
!
subroutine odbc_set_column_int( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   integer, intent(in)                :: value

   column%int_value = value
   column%type_set  = ODBC_INT
end subroutine odbc_set_column_int

subroutine odbc_set_column_real( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   real, intent(in)                 :: value

   column%double_value = value
   column%type_set  = ODBC_DOUBLE
end subroutine odbc_set_column_real

subroutine odbc_set_column_double( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   real(kind=dp), intent(in)        :: value

   column%double_value = value
   column%type_set  = ODBC_DOUBLE
end subroutine odbc_set_column_double

subroutine odbc_set_column_char( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   character(len=*), intent(in)     :: value

   column%char_value = value
   column%type_set  = ODBC_CHAR
end subroutine odbc_set_column_char

subroutine odbc_set_column_blob( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   type(ODBC_BLOB), intent(in)      :: value

   if ( allocated(column%blob_value%data) ) then
       deallocate( column%blob_value%data )
   endif

   allocate( column%blob_value%data(size(value%data)) )
   if ( value%actual_size <= 0 ) then
       column%blob_value%actual_size = 4 * size(value%data)
   else
       column%blob_value%actual_size = value%actual_size
   endif
   column%blob_value%data        = value%data
   column%type_set               = ODBC_BINARY
end subroutine odbc_set_column_blob


! odbc_get_column_int    --
! odbc_get_column_real   --
! odbc_get_column_double --
! odbc_get_column_char   --
! odbc_get_column_blob   --
!    Convenience routines to get the value of a column
! Arguments:
!    column        Column structure
!    value         Value on return
! Side effects:
!    Value argument will be set
! Note:
!    No attempt is made to convert the value
!    to the requested value. You will have to
!    check this yourself
!
subroutine odbc_get_column_int( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   integer, intent(out)             :: value

   value = column%int_value
end subroutine odbc_get_column_int

subroutine odbc_get_column_real( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   real, intent(out)                :: value

   value = column%double_value
end subroutine odbc_get_column_real

subroutine odbc_get_column_double( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   real(kind=dp), intent(out)       :: value

   value = column%double_value
end subroutine odbc_get_column_double

subroutine odbc_get_column_char( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   character(len=*), intent(out)    :: value

   value = column%char_value
end subroutine odbc_get_column_char

subroutine odbc_get_column_blob( column, value )
   type(ODBC_COLUMN), intent(inout) :: column
   type(ODBC_BLOB), intent(out)     :: value

   if ( allocated(value%data) ) then
       deallocate( value%data )
   endif

   allocate( value%data(column%blob_value%actual_size) )
   value%actual_size = column%blob_value%actual_size
   value%data        = column%blob_value%data(1:column%blob_value%actual_size)
end subroutine odbc_get_column_blob


! odbc_open_dsn --
!     Open the database connection based on the DSN name
!
! Arguments:
!     dsnname       Name of the data source
!     db            Database connection
!
subroutine odbc_open_dsn( dsnname, db )

    character(len=*), intent(in)     :: dsnname
    type(odbc_database), intent(out) :: db

    character(len=len(dsnname)+20)   :: connection_string

    connection_string = "DSN=" // trim(dsnname) // ";"

    call odbc_connect( connection_string, db )
end subroutine odbc_open_dsn


! odbc_open_file --
!     Open the database connection based on the name of a file
!     and the driver that should be used
!
! Arguments:
!     filename      Name of the database file
!     driver        Name of the driver
!     db            Database connection
!
subroutine odbc_open_file( filename, driver, db )

    character(len=*), intent(in)     :: filename
    character(len=*), intent(in)     :: driver
    type(odbc_database), intent(out) :: db

    character(len=len(filename)+len(driver)+100)   :: connection_string

    select case (driver)
        case (odbc_msaccess, odbc_msexcel)
            connection_string = "DRIVER={" // trim(driver) // "};" // &
                "DBQ=" // trim(filename) // ";Uid=Admin;Pwd=;"
            db%blob_type = ODBC_NO_BLOB_SUPPORT
        case (odbc_sqlite3)
            connection_string = "DRIVER={" // trim(driver) // "};" // &
                "Database=" // trim(filename) // ";"
            db%blob_type = ODBC_NO_BLOB_SUPPORT
        case (odbc_postgresql)
            connection_string = "DRIVER={" // trim(driver) // "};" // &
                "Database=" // trim(filename) // ";"
            db%blob_type = ODBC_POSTGRESQL_HEX
        case default
            db%errmsg = "Unknown driver: " // trim(driver)
            db%blob_type = ODBC_NO_BLOB_SUPPORT
            db%error  = -1
            return
    end select

    call odbc_connect( connection_string, db )

end subroutine odbc_open_file


! odbc_connect --
!     Open the database connection based on the connection string
!
! Arguments:
!     connection_string    Full connection string
!     db                   Database connection
!
subroutine odbc_connect( connection_string, db )

    character(len=*), intent(in)            :: connection_string
    type(odbc_database), intent(out)        :: db

    character(len=len(connection_string)+1) :: connectc

    interface
        integer function odbc_connect_c( connectc, db_handle )
            character(len=*)      :: connectc
            integer, dimension(*) :: db_handle
        end function odbc_connect_c
    end interface

    db%db_handle   = 0
    db%error       = 0
    db%errmsg       = ' '

    connectc = connection_string
    call stringtoc( connectc )

    db%error = odbc_connect_c( connectc, db%db_handle )

end subroutine odbc_connect


! odbc_close --
!     Close the database connection
!
! Arguments:
!     db                   Database connection
!
subroutine odbc_close( db )
    type(odbc_database), intent(out)        :: db

    interface
        integer function odbc_close_c( db_handle, stmt_handle )
            integer, dimension(*) :: db_handle
            integer, dimension(*) :: stmt_handle
        end function odbc_close_c
    end interface

    db%error = odbc_close_c( db%db_handle, db%stmt_handle )

end subroutine odbc_close


! odbc_set_blob_type --
!    Set the type of BLOB support
! Arguments:
!    db            Connection to the database
!    blob_type     Type of BLOB support
!
subroutine odbc_set_blob_type( db, blob_type )
   type(odbc_database) :: db
   integer, intent(in) :: blob_type

   db%blob_type = blob_type
end subroutine odbc_set_blob_type


! odbc_error --
!    Return the last error code
! Arguments:
!    db            Connection to the database
! Returns:
!    Last ODBC error code for this database
!
logical function odbc_error( db )
   type(odbc_database) :: db

   odbc_error = db%error .ne. 0
end function odbc_error


! odbc_errmsg_print_db --
!    Print the last error message(s)
! Arguments:
!    db            Connection to the database
!    lun           (Optional) LU-number to write to
!
subroutine odbc_errmsg_print_db( db, lun )
    type(odbc_database) :: db
    integer, optional   :: lun

    interface
        integer function odbc_get_diagnostics_c( handle, type, idx, state, text )
            integer, dimension(*) :: handle
            integer               :: type
            integer               :: idx
            character(len=*)      :: state
            character(len=*)      :: text
        end function odbc_get_diagnostics_c
    end interface

    integer           :: rc
    integer           :: i
    character(len=10) :: state
    character(len=80) :: text

    do i = 1,10
        rc = odbc_get_diagnostics_c( db%db_handle, 0, i, state, text )

        if ( rc /= 0 ) exit

        call stringtof( state )
        call stringtof( text )

        if ( present(lun) ) then
            write( lun, *) state, trim(text)
        else
            write(*,*) state, trim(text)
        endif
    enddo

end subroutine odbc_errmsg_print_db


! odbc_errmsg_print_stmt --
!    Print the last error message (TODO)
! Arguments:
!    stmt          Prepared statement
!    lun           (Optional) LU-number to write to
!
subroutine odbc_errmsg_print_stmt( stmt, lun )
    type(odbc_statement) :: stmt
    integer, optional   :: lun

    interface
        integer function odbc_get_diagnostics_c( handle, type, idx, state, text )
            integer, dimension(*) :: handle
            integer               :: type
            integer               :: idx
            character(len=*)      :: state
            character(len=*)      :: text
        end function odbc_get_diagnostics_c
    end interface

    integer           :: rc
    integer           :: i
    character(len=10) :: state
    character(len=80) :: text

    do i = 1,10
        rc = odbc_get_diagnostics_c( stmt%stmt_handle, 1, i, state, text )

        if ( rc /= 0 ) exit

        call stringtof( state )
        call stringtof( text )

        if ( present(lun) ) then
            write( lun, *) state, trim(text)
        else
            write(*,*) state, trim(text)
        endif
    enddo

end subroutine odbc_errmsg_print_stmt


! odbc_errmsg_db --
!    Return the last error message
! Arguments:
!    db            Connection to the database
!
function odbc_errmsg_db( db ) result( text )
    type(odbc_database) :: db
    character(len=80)   :: text

    interface
        integer function odbc_get_diagnostics_c( handle, type, idx, state, text )
            integer, dimension(*) :: handle
            integer               :: type
            integer               :: idx
            character(len=*)      :: state
            character(len=*)      :: text
        end function odbc_get_diagnostics_c
    end interface

    integer           :: rc
    integer           :: i
    character(len=10) :: state

    if ( db%errmsg /= ' ' ) then
        text = db%errmsg
        return
    else
        rc = odbc_get_diagnostics_c( db%db_handle, 0, 1, state, text )
        call stringtof( text )
    endif

end function odbc_errmsg_db


! odbc_errmsg_stmt --
!    Return the last error message
! Arguments:
!    stmt          Prepared statement
!
function odbc_errmsg_stmt( stmt ) result( text )
    type(odbc_statement) :: stmt
    character(len=80)    :: text

    interface
        integer function odbc_get_diagnostics_c( handle, type, idx, state, text )
            integer, dimension(*) :: handle
            integer               :: type
            integer               :: idx
            character(len=*)      :: state
            character(len=*)      :: text
        end function odbc_get_diagnostics_c
    end interface

    integer           :: rc
    integer           :: i
    character(len=10) :: state

    rc = odbc_get_diagnostics_c( stmt%stmt_handle, 1, i, state, text )
    call stringtof( text )

end function odbc_errmsg_stmt


! odbc_get_data_source --
!     Get the first or the next data source name
!
! Arguments:
!     next                 Get next data source?
!     dsnname              Data source name
!     description          Description of the data source
!     success              Whether we had success or not
!                          (if not, no source was returned)
!
subroutine odbc_get_data_source( next, dsnname, description, success )
    logical, intent(in)                :: next
    character(len=*), intent(out)      :: dsnname
    character(len=*), intent(out)      :: description
    logical, intent(out)               :: success

    interface
        integer function odbc_get_data_source_c( direction, dsnname, description )
            integer               :: direction
            character(len=*)      :: dsnname
            character(len=*)      :: description
        end function odbc_get_data_source_c
    end interface

    integer :: direction
    integer :: error

    direction = merge( 1, 0, next )
    error = odbc_get_data_source_c( direction, dsnname, description )

    if ( error == 0 ) then
        success = .true.
        call stringtof( dsnname )
        call stringtof( description )
    else
        success = .false.
        dsnname = ' '
        description = ' '
    endif

end subroutine odbc_get_data_source


! odbc_get_driver --
!     Get the first or the next installed driver
!
! Arguments:
!     next                 Get next driver
!     driver               Driver name
!     description          Description/attributes of the driver
!     success              Whether we had success or not
!                          (if not, no source was returned)
!
subroutine odbc_get_driver( next, driver, description, success )
    logical, intent(in)                :: next
    character(len=*), intent(out)      :: driver
    character(len=*), intent(out)      :: description
    logical, intent(out)               :: success

    interface
        integer function odbc_get_driver_c( direction, driver, description )
            integer               :: direction
            character(len=*)      :: driver
            character(len=*)      :: description
        end function odbc_get_driver_c
    end interface

    integer :: direction
    integer :: error

    direction = merge( 1, 0, next )
    error = odbc_get_driver_c( direction, driver, description )

    if ( error == 0 ) then
        success = .true.
        call stringtof( driver )
        call stringtof( description )
    else
        success     = .false.
        driver      = ' '
        description = ' '
    endif

end subroutine odbc_get_driver


! odbc_do --
!    Run a single SQL command
! Arguments:
!    db            Structure for the database
!    command       Complete SQL command
! Side effects:
!    Whatever effects the command has. Note
!    that no output is reported back to the
!    caller (except for error codes and
!    messages if any)
!    longer be accessed
!
subroutine odbc_do( db, command )
   type(ODBC_DATABASE) :: db
   character(len=*)    :: command

   interface
      integer function odbc_do_c( handle, command, errmsg )
         integer, dimension(*) :: handle
         character(len=*)      :: command
         character(len=*)      :: errmsg
      end function odbc_do_c
   end interface

   character(len=len(command)+1) :: commandc
   integer                       :: k

   commandc = command
   call stringtoc( commandc )

   db%errmsg = ' '
   db%error  = odbc_do_c( db%db_handle, commandc, db%errmsg )

end subroutine odbc_do


! odbc_begin --
!    Start a transaction on the given database
! Arguments:
!    db            Structure for the database
! Note:
!    Should be accompanied by a call to either
!    odbc_commit or odbc_rollback
!
subroutine odbc_begin( db )
   type(ODBC_DATABASE) :: db

   call odbc_do( db, "BEGIN TRANSACTION" )

end subroutine odbc_begin


! odbc_commit --
!    Commits a transaction on the given database
! Arguments:
!    db            Structure for the database
! Note:
!    Accompanies odbc_begin
!
subroutine odbc_commit( db )
   type(ODBC_DATABASE) :: db

   call odbc_do( db, "COMMIT TRANSACTION" )

end subroutine odbc_commit


! odbc_rollback --
!    Rolls back any changes to the database since the last commit
! Arguments:
!    db            Structure for the database
! Note:
!    Accompanies odbc_begin
!
subroutine odbc_rollback( db )
   type(ODBC_DATABASE) :: db

   call odbc_do( db, "ROLLBACK" )

end subroutine odbc_rollback


! odbc_get_table_name --
!     Get information on the first or the next table
!
! Arguments:
!     db                   Database to query
!     next                 Get next driver
!     table                Table name
!     description          Array of strings describing the table
!                          (the first 5 elements are filled)
!     success              Whether we had success or not
!                          (if not, no source was returned)
!
subroutine odbc_get_table_name( db, next, table, description, success )
    type(odbc_database), intent(inout)          :: db
    logical, intent(in)                         :: next
    character(len=*), intent(out)               :: table
    character(len=*), dimension(:), intent(out) :: description
    logical, intent(out)                        :: success

    interface
        integer function odbc_get_table_name_c( db, stmt, direction, &
                nstrings, description )
            integer, dimension(*)          :: db
            integer, dimension(*)          :: stmt
            integer                        :: direction
            integer                        :: nstrings
            character(len=*), dimension(*) :: description
        end function odbc_get_table_name_c
    end interface

    integer :: direction
    integer :: error
    integer :: nstrings
    integer :: i

    if ( size(description) < 5 ) then
        db%error  = -1
        db%errmsg = 'Too few elements in description array'
        return
    endif

    direction   = merge( 1, 0, next )
    nstrings    = size(description)
    table       = ' '
    description = ' '
    error = odbc_get_table_name_c( db%db_handle, db%stmt_handle, direction, &
                nstrings, description )

    if ( error == 0 ) then
        success = .true.
        do i = 1,nstrings
            call stringtof( description(i) )
        enddo
        table = description(3)
    else
        success     = .false.
        table       = ' '
        description = ' '
    endif

end subroutine odbc_get_table_name


! odbc_query_table --
!    Retrieve the column names and types from a table
! Arguments:
!    db            Structure for the database
!    tablename     Name of the table
!    columns       Columns (allocated array)
! Side effects:
!    The columns array is allocated and filled
! Note:
!    On entry the columns argument must not be
!    associated. On exit, it will point to a
!    freshly allocated array of column names/types
!
subroutine odbc_query_table( db, tablename, columns )
   type(ODBC_DATABASE)                       :: db
   character(len=*)                          :: tablename
   type(ODBC_COLUMN), dimension(:), pointer  :: columns     ! On return: actual columns!

   type(ODBC_STATEMENT)                      :: stmt
   character(len=20+len(tablename))          :: command

   write( command, '(3a)' ) 'select * from ',tablename, ';'

   !
   ! Note, we must free the columns, but we can not be sure
   ! they are no longer used. So simply disassociate.
   if ( associated(columns) ) then
      nullify( columns )
   endif
   call odbc_prepare( db, command, stmt, columns )
   call odbc_finalize( stmt )

end subroutine odbc_query_table


! odbc_delete_table --
!    Delete a table
! Arguments:
!    db            Structure for the database
!    tablename     Name of the table to be deleted
! Note:
!    The table can not be recovered, unless this
!    is part of a transaction
!
subroutine odbc_delete_table( db, tablename )
   type(ODBC_DATABASE) :: db
   character(len=*)      :: tablename

   character(len=20+len(tablename)) :: command

   write( command, "(2A)" ) "DELETE TABLE ", tablename
   call odbc_do( db, command )

end subroutine odbc_delete_table


! odbc_create_table --
!    Create a new table
! Arguments:
!    db            Structure for the database
!    tablename     Name of the table
!    columns       Properties of the columns
!    primary       Name of the primary key (if any)
! Side effects:
!    The new table is created
!
subroutine odbc_create_table( db, tablename, columns, primary )
   type(ODBC_DATABASE)              :: db
   character(len=*)                   :: tablename
   type(ODBC_COLUMN), dimension(:)  :: columns
   character(len=*), optional         :: primary

   character(len=20+80*size(columns)) :: command
   character(len=40)                  :: primary_
   integer                            :: i
   integer                            :: ncols

   primary_ = ' '
   if ( present(primary) ) then
      primary_ = primary
   endif

   ncols = size(columns)
   write( command, '(100a)' ) 'create table ', tablename, ' (', &
      ( trim(columns(i)%name), ' ', trim(typename(columns(i), primary_,db%blob_type)), ', ', &
           i = 1,ncols-1 ), &
      trim(columns(ncols)%name), ' ', trim(typename(columns(ncols),primary_,db%blob_type)), ')'

   call odbc_do( db, command )
end subroutine odbc_create_table


! odbc_prepare_select --
!    Prepare a selection of data from the database
! Arguments:
!    db            Structure for the database
!    tablename     Name of the table
!    columns       Columns to be returned
!    stmt          Prepared statement (returned)
!    extra_clause  Extra clause for SELECT statement (appended)
! Side effects:
!    A new selection is prepared
!
subroutine odbc_prepare_select( db, tablename, columns, stmt, extra_clause )
    type(ODBC_DATABASE)                       :: db
    character(len=*)                          :: tablename
    type(ODBC_COLUMN), dimension(:), pointer  :: columns     ! On return: actual columns!
    character(len=*), optional                :: extra_clause
    type(ODBC_STATEMENT), intent(out)         :: stmt

    character(len=20+80*size(columns))        :: command
    integer                                   :: nocols
    integer                                   :: i

    interface
        subroutine odbc_exec_c( handle, rc )
            integer, dimension(*) :: handle
            integer               :: rc
        end subroutine odbc_exec_c
    end interface

    !
    ! Prepare the select statement for this table
    !
    ! TODO: expand the syntax!!
    !
    nocols = size(columns)
    write( command, '(100a)' ) 'select ', &
        (trim(column_func(columns(i))), ',', i = 1,nocols-1), &
         trim(column_func(columns(nocols))), &
        ' from ', trim(tablename)

    !
    ! Hm, appending a string of arbitrary length is tricky ...
    !
    if ( present(extra_clause) ) then
        command = trim(command) // ' ' // extra_clause
    endif

    command = trim(command) // ';'

    call stringtoc( command )
    call odbc_prepare( db, command, stmt, columns )
    call odbc_exec_c( stmt%stmt_handle, db%error )
    stmt%blob_type = db%blob_type

end subroutine odbc_prepare_select


! odbc_insert --
!    Insert a row into the given table
! Arguments:
!    db            Structure for the database
!    tablename     Name of the table
!    columns       Columns whose value is to be inserted
! Side effects:
!    A new row is written to the database
!
subroutine odbc_insert( db, tablename, columns )
    type(ODBC_DATABASE)                       :: db
    character(len=*)                          :: tablename
    type(ODBC_COLUMN), dimension(:), target   :: columns
    character(len=20+80*size(columns))        :: command

    type(ODBC_COLUMN), dimension(:), pointer  :: prepared_columns
    type(ODBC_STATEMENT)                      :: stmt
    integer                                   :: i
    integer                                   :: rc
    integer, save                             :: indicator

    character (len=10+40*size(columns))       :: string

    interface
        subroutine odbc_errmsg_c( handle, errmsg )
            integer, dimension(*) :: handle
            character(len=*)      :: errmsg
        end subroutine odbc_errmsg_c
    end interface

    interface
        integer function odbc_bind_param_int_c( handle, colidx, value, indicator )
            integer, dimension(*) :: handle
            integer               :: colidx
            integer               :: value
            integer               :: indicator
        end function odbc_bind_param_int_c
    end interface

    interface
        integer function odbc_bind_param_double_c( handle, colidx, value, indicator )
            use odbc_types
            integer, dimension(*) :: handle
            integer               :: colidx
            real(kind=dp)         :: value
            integer               :: indicator
        end function odbc_bind_param_double_c
    end interface

    interface
        integer function odbc_bind_param_text_c( handle, colidx, value, indicator )
            integer, dimension(*) :: handle
            integer               :: colidx
            character(len=*)      :: value
            integer               :: indicator
        end function odbc_bind_param_text_c
    end interface

    interface
        integer function odbc_bind_param_blob_c( handle, colidx, value, size_value, indicator, blob_type )
            integer, dimension(*) :: handle
            integer               :: colidx
            integer, dimension(*) :: value
            integer               :: size_value
            integer               :: indicator
            integer               :: blob_type
        end function odbc_bind_param_blob_c
    end interface

    interface
        subroutine odbc_exec_c( handle, rc )
            integer, dimension(*) :: handle
            integer               :: rc
        end subroutine odbc_exec_c
    end interface

    !
    ! Prepare the insert statement for this table
    !
    i = 0
    string = '('//trim(columns(1)%name)
    do i = 2,size(columns)
       string = trim(string)//','//trim(columns(i)%name)
    end do
    string = trim(string) //') '

    write( command, '(100a)' ) 'insert into ', trim(tablename),trim(string), ' values(', &
       ('?,', i = 1,size(columns)-1), '?);'

    call stringtoc( command )
    prepared_columns => columns
    call odbc_prepare( db, command, stmt, prepared_columns )

    !
    ! Bind the values
    !
    do i = 1,size(columns)

       select case (columns(i)%type_set)
           case (ODBC_INT)
               rc = odbc_bind_param_int_c( stmt%stmt_handle, i, columns(i)%int_value, indicator )
           case (ODBC_REAL, ODBC_DOUBLE)
               rc = odbc_bind_param_double_c( stmt%stmt_handle, i, columns(i)%double_value, indicator )
           case (ODBC_CHAR)
               call stringtoc( columns(i)%char_value )
               columns(i)%int_value = len_trim(columns(i)%char_value)
               rc = odbc_bind_param_text_c( stmt%stmt_handle, i, columns(i)%char_value, &
                        columns(i)%int_value )
           case (ODBC_BINARY)
               rc = odbc_bind_param_blob_c( stmt%stmt_handle, i, columns(i)%blob_value%data, &
                        size(columns(i)%blob_value%data), columns(i)%blob_value%actual_size, db%blob_type )
        end select
        if ( rc .ne. 0 ) then
           db%error = rc
           call odbc_errmsg_c( db%db_handle, db%errmsg )
           call stringtof( db%errmsg )
        endif
    enddo

    !
    ! Actually perform the insert command
    !
    call odbc_exec_c( stmt%stmt_handle, rc )

    if ( rc /= 0 ) then
        db%errmsg = odbc_errmsg_stmt( stmt )
    endif
    db%error = rc
    call odbc_finalize( stmt )
end subroutine odbc_insert


! odbc_next_row --
!    Gets the next row of data from a selection
! Arguments:
!    stmt          Prepared statement
!    columns       Columns to be returned
!    finished      Indicates there are no more data
!
subroutine odbc_next_row( stmt, columns, finished )
    type(ODBC_STATEMENT)            :: stmt
    type(ODBC_COLUMN), dimension(:) :: columns
    logical                         :: finished

    interface
        integer function odbc_column_int_c( handle, colidx, value, indicator )
            integer, dimension(*) :: handle
            integer               :: colidx
            integer               :: value
            integer               :: indicator
        end function odbc_column_int_c
    end interface

    interface
        integer function odbc_column_double_c( handle, colidx, value, indicator )
            use odbc_types
            integer, dimension(*) :: handle
            integer               :: colidx
            real(kind=dp)         :: value
            integer               :: indicator
        end function odbc_column_double_c
    end interface

    interface
        integer function odbc_column_text_c( handle, colidx, value, indicator )
            integer, dimension(*) :: handle
            integer               :: colidx
            character(len=*)      :: value
            integer               :: indicator
        end function odbc_column_text_c
    end interface

    interface
        integer function odbc_column_blob_c( handle, colidx, value, size_value, indicator, blob_type )
            integer, dimension(*) :: handle
            integer               :: colidx
            integer, dimension(*) :: value
            integer               :: size_value
            integer               :: indicator
            integer               :: blob_type
        end function odbc_column_blob_c
    end interface

    integer                           :: rc
    integer                           :: i
    integer, save                     :: indicator

    call odbc_step( stmt, rc )

    if ( rc .eq. ODBC_ROW ) then
        finished = .false.

        !
        ! Get the values
        !
        ! TODO: check validity of "type_set"
        !
        do i = 1,size(columns)
            select case (columns(i)%type_set)
                case (ODBC_INT)
                    rc = odbc_column_int_c( stmt%stmt_handle, i, columns(i)%int_value, indicator )
                case (ODBC_REAL,ODBC_DOUBLE)
                    rc = odbc_column_double_c( stmt%stmt_handle, i, columns(i)%double_value, indicator  )
                case (ODBC_CHAR)
                    rc = odbc_column_text_c( stmt%stmt_handle, i, columns(i)%char_value, indicator  )
                    call stringtof( columns(i)%char_value )
                case (ODBC_BINARY)
                    if ( allocated(columns(i)%blob_value%data) ) then
                        if ( size(columns(i)%blob_value%data) /= columns(i)%blob_value%set_size ) then
                            deallocate( columns(i)%blob_value%data )
                            allocate( columns(i)%blob_value%data(columns(i)%blob_value%set_size) )
                        endif
                    else
                        allocate( columns(i)%blob_value%data(columns(i)%blob_value%set_size) )
                    endif
                    rc = odbc_column_blob_c( stmt%stmt_handle, i, columns(i)%blob_value%data, &
                             columns(i)%blob_value%set_size, indicator, stmt%blob_type )
                    if (rc == 0 ) then
                        columns(i)%blob_value%actual_size = (indicator+3)/4
                    else
                        columns(i)%blob_value%actual_size = 0
                    endif
            end select
            ! if ( rc .ne. 0 ) then
            !    db%error = rc
            !    call odbc_errmsg_c( db%db_handle, db%errmsg )
            !    call stringtof( db%errmsg )
            ! endif
        enddo
    else
        finished = .true.
    endif

end subroutine odbc_next_row


! odbc_prepare --
!    Reset the prepared SQL statement so that it can
!    be used again
! Arguments:
!    stmt          Handle to the prepared statement
!
subroutine odbc_prepare( db, command, stmt, columns )
    type(ODBC_DATABASE), intent(inout)        :: db
    character(len=*), intent(in)              :: command
    type(ODBC_STATEMENT), intent(out)         :: stmt
    type(ODBC_COLUMN), dimension(:), pointer  :: columns     ! On return: actual columns!

    interface
       integer function odbc_prepare_c( db, command, stmt )
          integer, dimension(*) :: db
          character(len=*)      :: command
          integer, dimension(*) :: stmt
       end function odbc_prepare_c
    end interface

    interface
       subroutine odbc_column_count_c( handle, count )
          integer, dimension(*) :: handle
          integer               :: count
       end subroutine odbc_column_count_c
    end interface

    interface
       subroutine odbc_column_name_type_c( handle, colidx, name, type )
          integer, dimension(*) :: handle
          integer               :: colidx
          character(len=*)      :: name
          character(len=*)      :: type
       end subroutine odbc_column_name_type_c
    end interface

    integer                                     :: count
    integer                                     :: i
    character(len=len(command)+1)               :: commandc

    commandc = command
    call stringtoc( commandc )
    db%error = odbc_prepare_c( db%db_handle, commandc, stmt%stmt_handle )
    stmt%blob_type = db%blob_type

    if ( db%error .eq. 0 ) then
        if ( associated(columns) ) return ! Assumption: they are already known

        call odbc_column_count_c( stmt%stmt_handle, count )

        allocate( columns(1:count) )

        do i = 1,count
            call odbc_column_name_type_c( stmt%stmt_handle, i, &
                columns(i)%name, columns(i)%type )
            call stringtof( columns(i)%name )
            call stringtof( columns(i)%type )

            select case (columns(i)%type(1:4) )
                case( 'INT ', 'INTE' )
                    columns(i)%type_set = ODBC_INT
                case( 'FLOA', 'DOUB' )
                    columns(i)%type_set = ODBC_DOUBLE
                case( 'CHAR', 'VARC' )
                    columns(i)%type_set = ODBC_CHAR
                case( 'BLOB' )
                    columns(i)%type_set = ODBC_BINARY
            end select

        enddo
    else
        call odbc_errmsg_c( db%db_handle, db%errmsg )
    endif

end subroutine odbc_prepare


! odbc_finalize --
!    Finalize the prepared SQL statement
! Arguments:
!    stmt          Handle to the prepared statement
!
subroutine odbc_finalize( stmt )
   type(ODBC_STATEMENT) :: stmt

   call odbc_finalize_c( stmt%stmt_handle )

end subroutine odbc_finalize


! odbc_step --
!    Run the prepared SQL statement
! Arguments:
!    stmt          Handle to the prepared statement
!    completion    Return code, indicating if the command is complete or
!                  not (ODBC_DONE, ODBC_MISUSE or ODBC_ERROR)
!
subroutine odbc_step( stmt, completion )
   type(ODBC_STATEMENT)                      :: stmt
   integer, intent(out)                        :: completion

   interface
      subroutine odbc_step_c( stmt, completion )
         integer, dimension(*) :: stmt
         integer               :: completion
      end subroutine odbc_step_c
   end interface

   call odbc_step_c( stmt%stmt_handle, completion )

end subroutine odbc_step

end module
