/* codbc.c --
      C wrappers callable from Fortran for ODBC

      $Id: codbc.c,v 1.9 2013/06/30 10:26:50 arjenmarkus Exp $
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Macros for the type of BLOB support - keep in sync with Fortran source */
#define ODBC_NO_BLOB_SUPPORT (-1)
#define ODBC_PLAIN_BYTES     0
#define ODBC_POSTGRESQL_HEX  1

#if !defined(LOWERCASE) && !defined(DBL_UNDERSCORE)
#ifdef WIN32
//#define FTNCALL __stdcall
#define FTNCALL
//#define INBETWEEN
#define odbc_connect_c_             ODBC_CONNECT_C
#define odbc_open_c_                ODBC_OPEN_C
#define odbc_close_c_               ODBC_CLOSE_C
#define odbc_do_c_                  ODBC_DO_C
#define odbc_exec_c_                ODBC_EXEC_C
#define odbc_get_data_source_c_     ODBC_GET_DATA_SOURCE_C
#define odbc_get_driver_c_          ODBC_GET_DRIVER_C
#define odbc_get_table_name_c_      ODBC_GET_TABLE_NAME_C
#define odbc_get_diagnostics_c_     ODBC_GET_DIAGNOSTICS_C
#define odbc_finalize_c_            ODBC_FINALIZE_C
#define odbc_reset_c_               ODBC_RESET_C
#define odbc_step_c_                ODBC_STEP_C
#define odbc_insert_c_              ODBC_INSERT_C
#define odbc_prepare_c_             ODBC_PREPARE_C
#define odbc_column_count_c_        ODBC_COLUMN_COUNT_C
#define odbc_column_name_type_c_    ODBC_COLUMN_NAME_TYPE_C
#define odbc_errmsg_c_              ODBC_ERRMSG_C
#define odbc_bind_int_c_            ODBC_BIND_INT_C
#define odbc_bind_null_c_           ODBC_BIND_NULL_C
#define odbc_bind_double_c_         ODBC_BIND_DOUBLE_C
#define odbc_bind_text_c_           ODBC_BIND_TEXT_C
#define odbc_bind_blob_c_           ODBC_BIND_BLOB_C
#define odbc_bind_param_int_c_      ODBC_BIND_PARAM_INT_C
#define odbc_bind_param_double_c_   ODBC_BIND_PARAM_DOUBLE_C
#define odbc_bind_param_text_c_     ODBC_BIND_PARAM_TEXT_C
#define odbc_bind_param_blob_c_     ODBC_BIND_PARAM_BLOB_C
#define odbc_column_int_c_          ODBC_COLUMN_INT_C
#define odbc_column_double_c_       ODBC_COLUMN_DOUBLE_C
#define odbc_column_text_c_         ODBC_COLUMN_TEXT_C
#define odbc_column_blob_c_         ODBC_COLUMN_BLOB_C
#define odbc_get_table_name_1_c_    ODBC_GET_TABLE_1_C
#define odbc_get_table_name_2_c_    ODBC_GET_TABLE_2_C
#include <windows.h>
#endif /* WIN32 */
#endif /* !defined(LOWERCASE) && !defined(DBL_UNDERSCORE) */

#if defined(LOWERCASE) || defined(DBL_UNDERSCORE)
#define FTNCALL
#if defined(DBL_UNDERSCORE)
#define odbc_connect_c_             odbc_connect_c__
#define odbc_open_c_                odbc_open_c__
#define odbc_close_c_               odbc_close_c__
#define odbc_do_c_                  odbc_do_c__
#define odbc_exec_c_                odbc_exec_c__
#define odbc_get_data_source_c_     odbc_get_data_source_c__
#define odbc_get_driver_c_          odbc_get_driver_c__
#define odbc_get_table_name_c_      odbc_get_table_name_c
#define odbc_get_diagnostics_c_     odbc_get_diagnostics_c__
#define odbc_finalize_c_            odbc_finalize_c__
#define odbc_reset_c_               odbc_reset_c__
#define odbc_step_c_                odbc_step_c__
#define odbc_insert_c_              odbc_insert_c__
#define odbc_prepare_c_             odbc_prepare_c__
#define odbc_column_count_c_        odbc_column_count_c__
#define odbc_column_name_type_c_    odbc_column_name_type_c__
#define odbc_errmsg_c_              odbc_errmsg_c__
#define odbc_bind_int_c_            odbc_bind_int_c__
#define odbc_bind_null_c_           odbc_bind_null_c__
#define odbc_bind_double_c_         odbc_bind_double_c__
#define odbc_bind_text_c_           odbc_bind_text_c__
#define odbc_bind_blob_c_           odbc_bind_blob_c__
#define odbc_bind_param_int_c_      odbc_bind_param_int_c__
#define odbc_bind_param_double_c_   odbc_bind_param_double_c__
#define odbc_bind_param_text_c_     odbc_bind_param_text_c__
#define odbc_bind_param_blob_c_     odbc_bind_param_blob_c__
#define odbc_column_int_c_          odbc_column_int_c__
#define odbc_column_double_c_       odbc_column_double_c__
#define odbc_column_text_c_         odbc_column_text_c__
#define odbc_column_blob_c_         odbc_column_blob_c__
#define odbc_get_table_1_c_         odbc_get_table_1_c__
#define odbc_get_table_2_c_         odbc_get_table_2_c__
#endif /* defined(DBL_UNDERSCORE) */
#endif /* defined(LOWERCASE) || defined(DBL_UNDERSCORE) */

#include <sql.h>
#include <sqlext.h>

/* Environment handle
*/
static SQLHENV environment = (SQLHENV) NULL;
static int     count_dbs   = 0;

static void InitEnv() {
    SQLAllocHandle( SQL_HANDLE_ENV, SQL_NULL_HANDLE, &environment );
    SQLSetEnvAttr( environment, SQL_ATTR_ODBC_VERSION, (void *) SQL_OV_ODBC3, 0);
}

int FTNCALL odbc_connect_c_(
        char    *connectc,
#ifdef INBETWEEN
        int      len_connectc,
#endif
        SQLHDBC *db
#ifndef INBETWEEN
       ,int      len_connectc
#endif
      )
{
    SQLRETURN   rc;

    if ( environment == (SQLHENV) NULL ) {
        InitEnv();
    }

    SQLAllocHandle( SQL_HANDLE_DBC, environment, db );

    rc = SQLDriverConnect( *db, NULL, (SQLCHAR *)connectc, SQL_NTS, NULL, 0, NULL,
             SQL_DRIVER_NOPROMPT );

    if ( SQL_SUCCEEDED(rc) ) {
        count_dbs ++;
        return 0;
    } else {
        return (int) rc ;
    }
}

int FTNCALL odbc_close_c_(
       SQLHDBC  *db,
       SQLHSTMT *stmt
      )
{
   SQLDisconnect( *db );

   if ( stmt != (SQLHSTMT) NULL ) {
       SQLFreeHandle( SQL_HANDLE_STMT, *stmt );
   }
   SQLFreeHandle( SQL_HANDLE_DBC, *db );

   count_dbs -- ;
   if ( count_dbs == 0 ) {
       SQLFreeHandle( SQL_HANDLE_ENV, environment );
       environment = (SQLHENV) NULL;
   }
   return 0;
}

int FTNCALL odbc_do_c_(
       SQLHDBC *db,
       char    *command,
#ifdef INBETWEEN
       int      len_command,
#endif
       char    *errmsg,

#ifndef INBETWEEN
       int      len_command,
#endif
       int      len_errmsg
      )
{
    SQLRETURN rc  ;
    SQLHSTMT  stmt;

    SQLAllocHandle( SQL_HANDLE_STMT, *db, &stmt );
    rc = SQLExecDirect(stmt, (SQLCHAR *)command, SQL_NTS );

    if ( SQL_SUCCEEDED(rc) ) {
        count_dbs ++;
        SQLFreeHandle( SQL_HANDLE_STMT, stmt ) ;
        return 0;
    } else {
        SQLFreeHandle( SQL_HANDLE_STMT, stmt ) ;
        return (int) rc;
    }
}

void FTNCALL odbc_step_c_(
       SQLHSTMT *stmt,
       int      *completion
      )
{
    SQLRETURN rc ;

    rc = SQLFetch( *stmt ) ;

    if ( SQL_SUCCEEDED(rc) ) {
        *completion = 100;
    } else {
        if ( rc == SQL_NO_DATA ) {
            *completion = 998;
        } else {
            *completion = 999;
        }
    }
}

void FTNCALL odbc_exec_c_(
       SQLHSTMT *stmt,
       int      *code
      )
{
    SQLRETURN rc ;

    rc = SQLExecute( *stmt ) ;

    if ( SQL_SUCCEEDED(rc) ) {
        *code = 0;
    } else {
        if ( rc == SQL_NO_DATA ) {
            *code = 100;
        } else {
            *code = 999;
        }
    }
}

void FTNCALL odbc_finalize_c_(
       SQLHSTMT *stmt
      )
{
    SQLRETURN rc ;

    rc = SQLFreeHandle( SQL_HANDLE_STMT, *stmt );

    return;
}

int FTNCALL odbc_get_data_source_c_(
       int     *direction,
       char    *dsnname,
#ifdef INBETWEEN
       int      len_dsnname,
#endif
       char    *description,
#ifdef INBETWEEN
       int      len_description
#endif
#ifndef INBETWEEN
       int      len_dsnname
      ,int      len_description
#endif
      )
{
    SQLRETURN   rc;
    SQLSMALLINT len_dsn;
    SQLSMALLINT len_desc;

    if ( environment == (SQLHENV) NULL ) {
        InitEnv();
    }

    rc = SQLDataSources( environment,
             ((*direction)==0? SQL_FETCH_FIRST : SQL_FETCH_NEXT),
             (SQLCHAR *)dsnname, len_dsnname-1, &len_dsn,
             (SQLCHAR *)description, len_description-1, &len_desc);

    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

int FTNCALL odbc_get_driver_c_(
       int     *direction,
       char    *driver,
#ifdef INBETWEEN
       int      len_driver,
#endif
       char    *description,
#ifdef INBETWEEN
       int      len_description
#endif
#ifndef INBETWEEN
       int      len_driver
      ,int      len_description
#endif
      )
{
    SQLRETURN   rc;
    SQLSMALLINT len_drv;
    SQLSMALLINT len_desc;

    if ( environment == (SQLHENV) NULL ) {
        InitEnv();
    }

    rc = SQLDrivers( environment,
             ((*direction)==0? SQL_FETCH_FIRST : SQL_FETCH_NEXT),
             (SQLCHAR *)driver, len_driver-1, &len_drv,
             (SQLCHAR *)description, len_description-1, &len_desc);

    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

int FTNCALL odbc_get_table_name_c_(
       SQLHDBC  *db,
       SQLHSTMT *stmt,
       int      *direction,
       int      *nstrings,
       char     *description,
       int       len_description
      )
{
    SQLRETURN   rc    ;
    SQLSMALLINT ncols ;
    SQLSMALLINT i     ;

    if ( environment == (SQLHENV) NULL ) {
        InitEnv();
    }

    if ( (*db) == (SQLHDBC) NULL ) {
        return -1;
    }

    if ( (*stmt) == (SQLHSTMT) NULL || (*direction) == 0 ) {
        if ( (*stmt) == (SQLHSTMT) NULL ) {
            SQLAllocHandle( SQL_HANDLE_STMT, *db, stmt );
        }

        SQLTables( *stmt, NULL, 0, NULL, 0, NULL, 0, NULL, 0 );
    }

    rc = SQLFetch( *stmt );

    if ( SQL_SUCCEEDED(rc) ) {
        SQLNumResultCols( *stmt, &ncols ) ;

        for ( i = 0; i < ncols && i < (*nstrings); i ++ ) {
            SQLRETURN  ret;
            SQLLEN     indicator;
            ret = SQLGetData( *stmt, i+1, SQL_C_CHAR,
                      description+i*len_description, len_description,
                      &indicator );
            if ( SQL_SUCCEEDED(ret) ) {
                if ( indicator == SQL_NULL_DATA ) {
                    strcpy( description+i*len_description, "NULL" ) ;
                }
            }
        }
        return 0;
    } else {
        return (int) rc;
    }
}

void FTNCALL odbc_errmsg_c_(
       SQLHDBC *db,
       char    *errmsg,
       int      len_errmsg
      )
{

//   pstr = odbc_errmsg( *db ) ;
//   strncpy( errmsg, pstr, len_errmsg ) ;

    return ;
}

int FTNCALL odbc_get_diagnostics_c_(
       SQLHANDLE *handle,
       int       *type,
       int       *idx,
       char      *state,
#ifdef INBETWEEN
       int        len_state,
#endif
       char      *text
#ifdef INBETWEEN
      ,int        len_text
#endif
#ifndef INBETWEEN
      ,int        len_state
      ,int        len_text
#endif
      )
{
    SQLRETURN   rc          ;
    SQLSMALLINT handle_type ;
    SQLINTEGER  native      ;
    SQLSMALLINT dummy       ;

    handle_type = SQL_HANDLE_DBC;
    if ( *type == 1 ) {
        handle_type = SQL_HANDLE_STMT;
    }

    rc = SQLGetDiagRec( handle_type, *handle, *idx, (SQLCHAR *)state, &native,
             (SQLCHAR *)text, len_text, &dummy );
    return (int) rc;
}

int FTNCALL odbc_prepare_c_(
       SQLHDBC *db,
       char *command,
#ifdef INBETWEEN
       int   len_command,
#endif
       SQLHSTMT *stmt
#ifndef INBETWEEN
      ,int   len_command
#endif
      )
{
    SQLRETURN rc ;

    SQLAllocHandle( SQL_HANDLE_STMT, *db, stmt );
    rc = SQLPrepare( *stmt, (SQLCHAR *)command, SQL_NTS ) ;

    return (int) rc ;
}

void FTNCALL odbc_column_count_c_(
       SQLHSTMT *stmt,
       int      *count
      )
{
    SQLSMALLINT cols;

    SQLNumResultCols( *stmt, &cols );
    *count = cols;
    return ;
}

void FTNCALL odbc_column_name_type_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       char     *name,
#ifdef INBETWEEN
       int       len_name,
#endif
       char     *type,

#ifndef INBETWEEN
       int       len_name,
#endif
       int       len_type
      )
{
    SQLRETURN    rc            ;
    SQLSMALLINT  actual_length ;
    SQLSMALLINT  data_type     ;
    SQLULEN      column_size   ;
    SQLSMALLINT  decimals      ;
    SQLSMALLINT  nullable      ;

    rc = SQLDescribeCol( *stmt, *colidx, (SQLCHAR *)name, (SQLSMALLINT)len_name, &actual_length,
             &data_type, &column_size, &decimals, &nullable ) ;

    name[len_name-1] = '\0' ;

    switch (data_type) {
        case SQL_INTEGER:
        case SQL_SMALLINT:
            strcpy( type, "INTEGER" ) ;
            break ;
        case SQL_REAL:
        case SQL_FLOAT:
            strcpy( type, "FLOAT" ) ;
            break ;
        case SQL_DOUBLE:
        case SQL_NUMERIC:
        case SQL_DECIMAL:
            strcpy( type, "DOUBLE" ) ;
            break ;
        case SQL_CHAR:
        case SQL_VARCHAR:
            strcpy( type, "CHARACTER" ) ;
            break ;
        case SQL_DATETIME:
        case SQL_TYPE_TIME:
        case SQL_TYPE_DATE:
        case SQL_TYPE_TIMESTAMP:
            strcpy( type, "CHARACTER" ) ; /* For now at least */
            break ;
        case SQL_BINARY:
        case SQL_VARBINARY:
        case SQL_LONGVARBINARY:
            strcpy( type, "BLOB" ) ;
            break ;
        default:
            strcpy( type, "????" ) ;
            break ;
    }

    return ;
}

int FTNCALL odbc_bind_int_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       long     *value,
       int      *indicator
      )
{
    SQLRETURN rc  ;
    SQLLEN    ind ;

    rc = SQLBindCol(*stmt, *colidx, SQL_INTEGER, value, 0, &ind ) ;
    *indicator = (int) ind ;
    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

int FTNCALL odbc_bind_double_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       double   *value,
       int      *indicator
      )
{
    SQLRETURN rc  ;
    SQLLEN    ind ;

    rc = SQLBindCol(*stmt, *colidx, SQL_DOUBLE, value, 0, &ind ) ;
    *indicator = (int) ind ;
    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

int FTNCALL odbc_bind_text_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       char     *text,
#ifdef INBETWEEN
       int       len_text,
#endif
       int      *indicator
#ifndef INBETWEEN
      ,int       len_text
#endif
      )
{
    SQLRETURN rc  ;
    SQLLEN    ind ;

    rc = SQLBindCol(*stmt, *colidx, SQL_CHAR, text, len_text, &ind ) ;
    *indicator = (int) ind ;
    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

int FTNCALL odbc_bind_blob_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       int     *blob,
       int     *size_blob,
       int      *indicator,
       int      *blob_type
      )
{
    SQLRETURN rc  ;
    SQLLEN    ind ;

    rc = SQLBindCol(*stmt, *colidx, SQL_BINARY, blob, (*size_blob)*sizeof(int), &ind ) ;
    *indicator = (int) ind ;

    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

/* ODBC distinguishes columns in a result set and parameters in a general SQL statement.
   These functions are used for the insert statement.
*/
int FTNCALL odbc_bind_param_int_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       long     *value,
       int      *indicator
      )
{
    SQLRETURN rc  ;
    SQLLEN    ind ;

    rc = SQLBindParameter(*stmt, *colidx, SQL_PARAM_INPUT, SQL_INTEGER, SQL_INTEGER, 0, 0, value, 0, &ind ) ;
    *indicator = (int) ind ;
    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

int FTNCALL odbc_bind_param_double_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       double   *value,
       int      *indicator
      )
{
    SQLRETURN rc  ;
    SQLLEN    ind ;

    rc = SQLBindParameter(*stmt, *colidx, SQL_PARAM_INPUT, SQL_DOUBLE, SQL_DOUBLE, 0, 0, value, 0, &ind ) ;
    *indicator = (int) ind ;
    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

int FTNCALL odbc_bind_param_text_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       char     *text,
#ifdef INBETWEEN
       int       len_text,
#endif
       int      *actual_length
#ifndef INBETWEEN
      ,int       len_text
#endif
      )
{
    SQLRETURN rc  ;
    SQLLEN    act ;

    rc = SQLBindParameter(*stmt, *colidx, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_CHAR,
             strlen(text)+1, 0, text, 0, &act ) ;
    *actual_length = (int) act ;
    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

int FTNCALL odbc_bind_param_blob_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       int      *blob,
       int      *size_blob,
       int      *actual_length
      )
{
    SQLRETURN rc  ;
    SQLLEN    act ;

    rc = SQLBindParameter(*stmt, *colidx, SQL_PARAM_INPUT, SQL_C_BINARY, SQL_BINARY,
             (*size_blob)*sizeof(int), 0, blob, 0, &act ) ;
    *actual_length = (int) act ;
    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

int FTNCALL odbc_column_int_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       long     *value,
       int      *indicator
      )
{
    SQLRETURN rc  ;
    SQLLEN    ind ;

    rc = SQLGetData(*stmt, *colidx, SQL_INTEGER, value, 0, &ind ) ;
    *indicator = (int) ind ;
    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

int FTNCALL odbc_column_double_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       double   *value,
       int      *indicator
      )
{
    SQLRETURN rc  ;
    SQLLEN    ind ;

    rc = SQLGetData(*stmt, *colidx, SQL_DOUBLE, value, 0, &ind ) ;
    *indicator = (int) ind ;
    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

int FTNCALL odbc_column_text_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       char     *text,
#ifdef INBETWEEN
       int       len_text,
#endif
       int      *indicator
#ifndef INBETWEEN
      ,int       len_text
#endif
      )
{
    SQLRETURN rc  ;
    SQLLEN    ind ;

    rc = SQLGetData(*stmt, *colidx, SQL_CHAR, text, len_text, &ind ) ;
    *indicator = (int) ind ;
    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

int FTNCALL odbc_column_blob_c_(
       SQLHSTMT *stmt,
       int      *colidx,
       int      *blob,
       int      *size_blob,
       int      *indicator,
       int      *blob_type
      )
{
    SQLRETURN rc  ;
    SQLLEN    ind ;

    int         i            ;
    char       *hexstring    ;

    if ( (*blob_type) != ODBC_POSTGRESQL_HEX ) {
        rc = SQLGetData(*stmt, *colidx, SQL_BINARY, blob, (*size_blob)*sizeof(int), &ind ) ;
        *indicator = (int) ind ;
    } else {

        /* Handle PostgreSQL's hexdecimal output style */
        /* TODO: This does not work yet! It should handle the particular endianness of the
                 platform and the PostgreSQL driver on Linux that I used seems confused about
                 the data that are returned. For instance: I got back a string of which the
                 first character was missing or maimed and the indicator was set to 39, where
                 I expected 40.
        */

        hexstring = (char *) malloc( 2 * (*size_blob)*sizeof(int) + 4 ) ;
        rc = SQLGetData(*stmt, *colidx, SQL_BINARY, hexstring, 2*(*size_blob)*sizeof(int)+4, &ind ) ;
        *indicator = (int) ind ;

        /* TODO: determine if there is one byte or more in front of the hexadecimal string */

        if ( SQL_SUCCEEDED(rc) ) {
            for ( i = 0 ; i < ((*indicator)-1)/8 ; i ++ ) {
                sscanf(hexstring+1+8*i,"%8x", &blob[i] ) ;
            }
        }

        free( hexstring) ;
    }

    if ( SQL_SUCCEEDED(rc) ) {
        return 0;
    } else {
        return (int) rc;
    }
}

#if 0
void FTNCALL odbc_reset_c_(
       odbc_stmt **stmt
      )
{
   SQLRETURN rc  ;

   rc = odbc_reset( *stmt ) ;

   return ;
}

int FTNCALL odbc_get_table_1_c_(
       odbc **db,
       char *command,
#ifdef INBETWEEN
       int   len_command,
#endif
       int  *ncol,
       int  *nrow,
       char *errmsg,
#ifndef INBETWEEN
       int   len_command,
#endif
       int   len_errmsg
      )
{
   SQLRETURN rc  ;
   char      *msg ;

   rc = odbc_get_table_name(*db, command, &result, nrow, ncol, (SQLCHAR *)&msg ) ;
   if ( msg != NULL )
   {
      strncpy( errmsg, msg, len_errmsg ) ;
   }

   return (int) rc ;
}

void FTNCALL odbc_get_table_2_c_(
       int  *ncol,
       int  *nrow,
       char *result_table,
       int   len_result
      )
{
   int   i   ;
   int   j   ;
   int   k   ;
   int   n   ;

   /* Note: one extra row! */
   for ( j = 0 ; j <= (*nrow) ; j ++ )
   {
      for ( i = 0 ; i < (*ncol) ; i ++ )
      {
         k = i + j*(*ncol) ;

         strncpy( &result_table[k*len_result], result[k], len_result ) ;

         for ( n = strlen(result[k]) ; n < len_result ; n ++ )
         {
            result_table[k*len_result+n] = ' ' ;
         }
      }
   }

   odbc_free_table( result ) ;
   result = NULL;

   return ;
}
#endif
