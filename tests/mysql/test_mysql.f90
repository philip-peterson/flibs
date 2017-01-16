PROGRAM test
  USE myfortran_binding
  IMPLICIT NONE

  TYPE(myfortran) :: myf
  TYPE(myfortran_result) :: res
  TYPE(myfortran_row) :: row

  CHARACTER(LEN=64) :: str
  INTEGER :: num

  CALL myfortran_init ()
  myf = myfortran_connect ("localhost", "user", "pwd", "mydb")

  CALL myfortran_query_do (myf, "INSERT INTO ...")

  res = myfortran_query_get (myf, "SELECT ...")
  DO WHILE (myfortran_fetch_row (res, row))
    str = myfortran_get_field (row, 1)
    READ (str, *) num
    PRINT *, num
  END DO

  CALL myfortran_disconnect (myf)
  CALL myfortran_shutdown ()
END PROGRAM test
