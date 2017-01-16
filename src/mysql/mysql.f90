! MySQL bindings for Fortran.
! Copyright (C) 2009  Daniel Kraft <d@domob.eu>

MODULE myfortran_binding
  USE ISO_C_BINDING
  IMPLICIT NONE


  ! Maximum error length.
  INTEGER, PARAMETER :: error_length = 256

  ! The error handling routine installed.
  PROCEDURE(error_stop), POINTER :: error_handler => NULL ()

  ! This type represents a database connection.
  TYPE myfortran
    TYPE(C_PTR) :: mysql
  END TYPE myfortran

  ! This type represents a result set.
  TYPE myfortran_result
    TYPE(C_PTR) :: res
  END TYPE myfortran_result

  ! This is a row returned.
  TYPE myfortran_row
    INTEGER :: num_fields
    INTEGER(C_LONG), ALLOCATABLE :: lengths(:)
    TYPE(C_PTR) :: c_row
  END TYPE myfortran_row


  ! Bind procedures.
  ! ================

  INTERFACE

    FUNCTION c_shutdown () &
    BIND(C, NAME='mysql_library_end')
      USE ISO_C_BINDING
      IMPLICIT NONE
      LOGICAL(C_BOOL) :: c_shutdown
      !gcc$ attributes stdcall:: c_shutdown
    END FUNCTION c_shutdown

    FUNCTION c_init (argc, argv, groups) &
    BIND(C, NAME='mysql_library_init')
      USE ISO_C_BINDING
      IMPLICIT NONE
      LOGICAL(C_BOOL) :: c_init
      INTEGER(C_INT), VALUE :: argc
      TYPE(C_PTR), DIMENSION(*) :: argv
      TYPE(C_PTR), DIMENSION(*) :: groups
      !gcc$ attributes stdcall:: c_init
    END FUNCTION c_init

    FUNCTION c_error (mysql) &
    BIND(C, NAME='mysql_error')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR) :: c_error
      TYPE(C_PTR), VALUE :: mysql
    END FUNCTION c_error

    FUNCTION c_connect (mysql, host, user, pwd, db, port, socket, flags) &
    BIND(C, NAME='mysql_real_connect')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR) :: c_connect
      TYPE(C_PTR), VALUE :: mysql
      CHARACTER(C_CHAR), DIMENSION(*) :: host, user, pwd, db
      INTEGER(C_INT), VALUE :: port
      TYPE(C_PTR), VALUE :: socket
      INTEGER(C_INT), VALUE :: flags
    END FUNCTION c_connect

    FUNCTION c_init_mysql (mysql) &
    BIND(C, NAME='mysql_init')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR) :: c_init_mysql
      TYPE(C_PTR), VALUE :: mysql
      !gcc$ attributes stdcall:: c_init_mysql
      !dec$ attributes stdcall:: mysql_init
    END FUNCTION c_init_mysql

    SUBROUTINE c_close (mysql) &
    BIND(C, NAME='mysql_close')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: mysql
      !gcc$ attributes stdcall:: c_close
      !dec$ attributes stdcall:: mysql_close
    END SUBROUTINE c_close

    FUNCTION c_query (mysql, sql, len) &
    BIND(C, NAME='mysql_real_query')
      USE ISO_C_BINDING
      IMPLICIT NONE
      INTEGER(C_INT) :: c_query
      TYPE(C_PTR), VALUE :: mysql
      CHARACTER(C_CHAR), DIMENSION(*) :: sql
      INTEGER(C_LONG), VALUE :: len
    END FUNCTION c_query

    FUNCTION c_store_result (mysql) &
    BIND(C, NAME='mysql_store_result')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR) :: c_store_result
      TYPE(C_PTR), VALUE :: mysql
    END FUNCTION c_store_result

    SUBROUTINE c_free_result (res) &
    BIND(C, NAME='mysql_free_result')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: res
    END SUBROUTINE c_free_result

    FUNCTION c_fetch_row (res) &
    BIND(C, NAME='mysql_fetch_row')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: res
      TYPE(C_PTR) :: c_fetch_row
    END FUNCTION c_fetch_row

    FUNCTION c_fetch_lengths (res) &
    BIND(C, NAME='mysql_fetch_lengths')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: res
      TYPE(C_PTR) :: c_fetch_lengths
    END FUNCTION c_fetch_lengths

    FUNCTION c_num_fields (res) &
    BIND(C, NAME='mysql_num_fields')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: res
      INTEGER(C_INT) :: c_num_fields
    END FUNCTION c_num_fields

    FUNCTION c_num_rows (res) &
    BIND(C, NAME='mysql_num_rows')
      USE ISO_C_BINDING
      IMPLICIT NONE
      TYPE(C_PTR), VALUE :: res
      INTEGER(C_LONG_LONG) :: c_num_rows
    END FUNCTION c_num_rows

  END INTERFACE

! ==============================================================================
CONTAINS


  ! Initialization and shutdown.
  ! ============================


  ! Initialize the library.
  SUBROUTINE myfortran_init ()
    IMPLICIT NONE
    LOGICAL(C_BOOL) :: c_result

    IF (.NOT. ASSOCIATED (error_handler)) THEN
      error_handler => error_stop
    END IF

    ! XXX
    !c_result = c_init (0, C_NULL_PTR, C_NULL_PTR)
    c_result = .TRUE.
    IF (.NOT. c_result) THEN
      CALL error_handler ("Error initializing MySQL")
    END IF
  END SUBROUTINE myfortran_init


  ! Shutdown the library.  Returns TRUE on success and FALSE on failure.
  SUBROUTINE myfortran_shutdown ()
    IMPLICIT NONE
    LOGICAL(C_BOOL) :: c_result

    ! XXX
    !c_result = c_shutdown ()
    c_result = .TRUE.
    IF (.NOT. c_result) THEN
      CALL error_handler ("Error shutting down MySQL")
    END IF
  END SUBROUTINE myfortran_shutdown


  ! Connecting / disconnecting.
  ! ===========================


  ! Open a fresh connection.
  TYPE(myfortran) FUNCTION myfortran_connect (host, user, pwd, db)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: host, user, pwd, db
    TYPE(C_PTR) :: mysql

    mysql = c_init_mysql (C_NULL_PTR)
    myfortran_connect%mysql = mysql
    IF (.NOT. C_ASSOCIATED (mysql)) THEN
      CALL error_handler ("mysql_init failed to allocate structure")
      RETURN
    END IF

    mysql = c_connect (mysql, host // C_NULL_CHAR, user // C_NULL_CHAR, &
                       pwd // C_NULL_CHAR, db // C_NULL_CHAR, 0, C_NULL_PTR, 0)
    IF (.NOT. C_ASSOCIATED (mysql)) THEN
      CALL error_handler (myfortran_error (myfortran_connect))
      RETURN
    END IF
  END FUNCTION myfortran_connect


  ! Close a connection.
  SUBROUTINE myfortran_disconnect (myf)
    IMPLICIT NONE
    TYPE(myfortran) :: myf
    CALL c_close (myf%mysql)
  END SUBROUTINE myfortran_disconnect


  ! Querying.
  ! =========


  ! Perform a query not expecting a result (that is, like UPDATE or DELETE).
  SUBROUTINE myfortran_query_do (myf, sql)
    IMPLICIT NONE
    TYPE(myfortran), INTENT(INOUT) :: myf
    CHARACTER(LEN=*), INTENT(IN) :: sql
    INTEGER(C_INT) :: c_result

    c_result = c_query (myf%mysql, sql, INT (LEN (sql), KIND=C_LONG))
    IF (c_result /= 0) THEN
      CALL error_handler (myfortran_error (myf))
    END IF
  END SUBROUTINE myfortran_query_do


  ! Perform a query and get the result set returned.
  ! Optionally, an expected number of results can be specified for checking.
  FUNCTION myfortran_query_get (myf, sql, num)
    IMPLICIT NONE
    TYPE(myfortran), INTENT(INOUT) :: myf
    CHARACTER(LEN=*), INTENT(IN) :: sql
    INTEGER, INTENT(IN), OPTIONAL :: num
    TYPE(myfortran_result) :: myfortran_query_get

    CALL myfortran_query_do (myf, sql)
    myfortran_query_get%res = c_store_result (myf%mysql)

    IF (.NOT. C_ASSOCIATED (myfortran_query_get%res)) THEN
      CALL error_handler ("No result set returned.")
    END IF

    IF (PRESENT (num)) THEN
      IF (num /= myfortran_num_rows (myfortran_query_get)) THEN
        CALL error_handler ("Result did not match expected number of rows.")
      END IF
    END IF
  END FUNCTION myfortran_query_get


  ! Working with result sets.
  ! =========================


  ! Free a result set when done with it.
  SUBROUTINE myfortran_free_result (res)
    IMPLICIT NONE
    TYPE(myfortran_result), INTENT(INOUT) :: res
    CALL c_free_result (res%res)
  END SUBROUTINE myfortran_free_result


  ! Get the number of rows in a result set.
  INTEGER(C_LONG_LONG) FUNCTION myfortran_num_rows (res)
    IMPLICIT NONE
    TYPE(myfortran_result), INTENT(IN) :: res
    myfortran_num_rows = c_num_rows (res%res)
  END FUNCTION myfortran_num_rows


  ! Fetch a row.  Returns false if no more rows available.
  LOGICAL FUNCTION myfortran_fetch_row (res, row)
    IMPLICIT NONE
    TYPE(myfortran_result), INTENT(INOUT) :: res
    TYPE(myfortran_row), INTENT(OUT) :: row
    INTEGER(C_LONG), POINTER :: lenptr(:)
    TYPE(C_PTR) :: lengths

    ! Fetch the row data itself.
    row%c_row = c_fetch_row (res%res)
    myfortran_fetch_row = C_ASSOCIATED (row%c_row)
    IF (.NOT. myfortran_fetch_row) RETURN

    ! Fetch number of fields.
    row%num_fields = c_num_fields (res%res)

    ! Get field lengths.
    ALLOCATE (row%lengths(row%num_fields))
    lengths = c_fetch_lengths (res%res)
    CALL C_F_POINTER (lengths, lenptr, SHAPE (row%lengths))
    row%lengths = lenptr
  END FUNCTION myfortran_fetch_row


  ! Get a field from a row.
  FUNCTION myfortran_get_field (row, ind)
    IMPLICIT NONE
    TYPE(myfortran_row), INTENT(IN) :: row
    INTEGER, INTENT(IN) :: ind
    CHARACTER(LEN=row%lengths(ind)) :: myfortran_get_field

    TYPE(C_PTR), POINTER :: cstrs(:)
    CHARACTER(KIND=C_CHAR), POINTER :: cstr(:)
    INTEGER(C_LONG) :: i

    CALL C_F_POINTER (row%c_row, cstrs, SHAPE (row%lengths))
    CALL C_F_POINTER (cstrs(ind), cstr, (/ row%lengths(ind) /))

    DO i = 1_C_LONG, row%lengths(ind)
      myfortran_get_field(i:i) = cstr(i)
    END DO
  END FUNCTION myfortran_get_field


  ! Error handling routines.
  ! ========================


  ! Get the MySQL error.
  FUNCTION myfortran_error (myf)
    IMPLICIT NONE
    CHARACTER(LEN=error_length) :: myfortran_error
    CHARACTER(KIND=C_CHAR, LEN=error_length), POINTER :: chararr
    TYPE(myfortran) :: myf
    TYPE(C_PTR) :: c_str
    INTEGER :: i

    c_str = c_error (myf%mysql)
    CALL C_F_POINTER (c_str, chararr)

    myfortran_error = ""
    DO i = 1, error_length
      IF (chararr(i:i) == C_NULL_CHAR) EXIT
      myfortran_error(i:i) = chararr(i:i)
    END DO
  END FUNCTION myfortran_error


  ! Error handling routine that stops with error message.
  SUBROUTINE error_stop (msg)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: msg

    PRINT *, "Error in MySQL:"
    PRINT *, msg
    STOP 1
  END SUBROUTINE error_stop


END MODULE myfortran_binding
