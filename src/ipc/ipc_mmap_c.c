/* ipc_mmap_c.c --
       Auxiliary routines for dealing with memory-mapped files

       Note:
       This has been tested to work on native Windows, Cygwin
       and Linux (the latter two if you define the LINUX macro)
       (MingW does not compile this code - sys/mman.h is missing)

       TODO:
       A lot!
*/

#ifndef LINUX
#include <windows.h>

#ifndef MINGW
#define FTNCALL __stdcall
#else
#define FTNCALL
#endif

#else
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/fcntl.h>
#define caddr_t void *

typedef int HANDLE;

#define FTNCALL

#endif

#include <stdlib.h>
#include <stdio.h>

#ifdef FTN_ALLCAPS
#define fsleep               FSLEEP
#define ipc_start_c          IPC_START_C
#define ipc_finish_c         IPC_FINISH_C
#define ipc_set_data_c       IPC_SET_DATA_C
#define ipc_get_data_c       IPC_GET_DATA_C
#define ipc_send_int_c       IPC_SEND_INT_C
#define ipc_send_real_c      IPC_SEND_REAL_C
#define ipc_send_dbl_c       IPC_SEND_DBL_C
#define ipc_send_log_c       IPC_SEND_LOG_C
#define ipc_send_char_c      IPC_SEND_CHAR_C
#define ipc_send_cmplx_c     IPC_SEND_CMPLX_C
#define ipc_receive_int_c    IPC_RECEIVE_INT_C
#define ipc_receive_real_c   IPC_RECEIVE_REAL_C
#define ipc_receive_dbl_c    IPC_RECEIVE_DBL_C
#define ipc_receive_log_c    IPC_RECEIVE_LOG_C
#define ipc_receive_char_c   IPC_RECEIVE_CHAR_C
#define ipc_receive_cmplx_c  IPC_RECEIVE_CMPLX_C
#endif

#ifdef FTN_UNDERSCORE
#define fsleep               fsleep_
#define ipc_start_c          ipc_start_c_
#define ipc_finish_c         ipc_finish_c_
#define ipc_set_data_c       ipc_set_data_c_
#define ipc_get_data_c       ipc_get_data_c_
#define ipc_send_int_c       ipc_send_int_c_
#define ipc_send_real_c      ipc_send_real_c_
#define ipc_send_dbl_c       ipc_send_dbl_c_
#define ipc_send_log_c       ipc_send_log_c_
#define ipc_send_char_c      ipc_send_char_c_
#define ipc_send_cmplx_c     ipc_send_cmplx_c_
#define ipc_receive_int_c    ipc_receive_int_c_
#define ipc_receive_real_c   ipc_receive_real_c_
#define ipc_receive_dbl_c    ipc_receive_dbl_c_
#define ipc_receive_log_c    ipc_receive_log_c_
#define ipc_receive_char_c   ipc_receive_char_c_
#define ipc_receive_cmplx_c  ipc_receive_cmplx_c_
#endif

#ifdef FTN_DBL_UNDERSCORE
#define fsleep               fsleep_
#define ipc_start_c          ipc_start_c__
#define ipc_finish_c         ipc_finish_c__
#define ipc_set_data_c       ipc_set_data_c__
#define ipc_get_data_c       ipc_get_data_c__
#define ipc_send_int_c       ipc_send_int_c__
#define ipc_send_real_c      ipc_send_real_c__
#define ipc_send_dbl_c       ipc_send_dbl_c__
#define ipc_send_log_c       ipc_send_log_c__
#define ipc_send_char_c      ipc_send_char_c__
#define ipc_send_cmplx_c     ipc_send_cmplx_c__
#define ipc_receive_int_c    ipc_receive_int_c__
#define ipc_receive_real_c   ipc_receive_real_c__
#define ipc_receive_dbl_c    ipc_receive_dbl_c__
#define ipc_receive_log_c    ipc_receive_log_c__
#define ipc_receive_char_c   ipc_receive_char_c__
#define ipc_receive_cmplx_c  ipc_receive_cmplx_c__
#endif

/* Typedefs and globals
*/
typedef struct _CommStruct {
    int    *data;      /* Pointer to the actual data */
    HANDLE  hfile;     /* Handle to the mmapped file */
    int     maxsize;
    char    src[21];
    char    dest[21];
} CommStruct;

static CommStruct *comm = NULL;
static int noConnects = 0;

/* fsleep --
       Sleep for a while - Fortran interface

   Arguments:
       wait         Number of ms to wait
*/
void FTNCALL fsleep(
    int *wait
    ) {

#ifdef LINUX
    usleep( (*wait) * 1000 );
#else
    Sleep( (*wait) );
#endif
}

/* ipc_start_c --
       Find or prepare a large enough mmapped file

   Arguments:
       send         If 1, called from the sender - initialise the file
       src          String identifying source
       dest         String identifying destination
       maxsize      Maximum size of individual message
       id           (Returned) ID for the mmapped file
*/
void FTNCALL ipc_start_c(
    int *send,
    char *src,
#ifdef INBETWEEN
    int len_src,
#endif
    char *dest,
#ifdef INBETWEEN
    int len_dest,
#endif
    int *maxsize,
    int *id
#ifndef INBETWEEN
   ,int len_src
   ,int len_dest
#endif
    ) {

#ifndef LINUX
    HANDLE hfile;
    HANDLE hmap;
#else
    int    fd;
#endif

    char   filename[256];
    int   *pdata;
    int    found;
    int    i;

    /* Search in the list of mmapped files for an existing connection
       If found, we are done. Otherwise create the mmap'ed file
    */
    found = 0;
    if ( comm != NULL ) {
        for ( i = 0; i < noConnects; i ++ ) {
            if ( strncmp( comm[i].src, src, len_src ) == 0    &&
                 comm[i].src[len_src] == '\0'                 &&
                 strncmp( comm[i].dest, dest, len_dest ) == 0 &&
                 comm[i].src[len_dest] == '\0'                   ) {
                found = 1;
                *id   = i;
                return;
            }
        }
    }

    if ( !found ) {
        *id = noConnects;
        noConnects ++;
        if ( comm == NULL ) {
            comm = (CommStruct *) malloc( noConnects * sizeof(CommStruct) );
        } else {
            comm = (CommStruct *) realloc( comm, noConnects * sizeof(CommStruct) );
        }
        strncpy( comm[*id].src,  src, len_src );
        comm[*id].src[len_src]  = '\0';
        strncpy( comm[*id].dest, dest, len_dest );
        comm[*id].src[len_dest] = '\0';
        comm[*id].maxsize = *maxsize;

        /* Create the mmapped file
           Note: With the Linux style, we need to fill the file with
           enough data.

           TODO: directory
        */
        strcpy( filename, comm[*id].src );
        strcat( filename, "-"     );
        strcat( filename, comm[*id].dest );
        strcat( filename, ".mmap" );

#ifndef LINUX
        hfile = CreateFile( filename, GENERIC_WRITE | GENERIC_READ,
                    FILE_SHARE_WRITE, NULL, CREATE_ALWAYS,
                    FILE_ATTRIBUTE_TEMPORARY, NULL );

        hmap = CreateFileMapping( hfile, NULL, PAGE_READWRITE, 0, (*maxsize), "MAP" );

        comm[*id].data  = (int *) MapViewOfFile( hmap, FILE_MAP_ALL_ACCESS, 0, 0, 0 );
        comm[*id].hfile = hfile;
#else
        fd = open( filename, O_CREAT|O_RDWR, S_IRWXU );

        if ( *send ) {
            pdata = (int *) malloc( (size_t)(*maxsize) );
            pdata[0] = 0;
            write( fd, pdata, (size_t)(*maxsize) );
            free( pdata );
        }
        comm[*id].data  = (int *) mmap( (caddr_t)0, (*maxsize),
            PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0 );
        comm[*id].hfile = fd;
#endif
    }
}

/* ipc_set_data_c --
       Set a single integer value

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       value        New value
*/
void FTNCALL ipc_set_data_c( int *idcomm, int *pos, int *value ) {

    comm[*idcomm].data[*pos] = *value ;
}

/* ipc_get_data_c --
       Get a single integer value

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       value        Current value of the integer
*/
void FTNCALL ipc_get_data_c( int *idcomm, int *pos, int *value ) {

    *value = comm[*idcomm].data[*pos] ;
}

/* ipc_send_int_c --
       Send an array of integers

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       data         Array with values to send
       number       Number of values to send
*/
void FTNCALL ipc_send_int_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        comm[*idcomm].data[i + (*pos)] = data[i] ;
    }
}

/* ipc_receive_int_c --
       Receive an array of integers

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       data         Array to store the values in
       number       Number of values to receive
*/
void FTNCALL ipc_receive_int_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        data[i] = comm[*idcomm].data[i + (*pos)] ;
    }
}

/* ipc_send_real_c --
       Send an array of single precision reals

   Arguments:
       idcomm       Communication ID
       pos          Position of the real
       data         Array with values to send
       number       Number of values to send

   NOTE:
       The type for the "data" argument remains the same!
*/
void FTNCALL ipc_send_real_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        comm[*idcomm].data[i + (*pos)] = data[i] ;
    }
}

/* ipc_receive_real_c --
       Receive an array of single precision reals

   Arguments:
       idcomm       Communication ID
       pos          Position of the real
       data         Array to store the values in
       number       Number of values to receive

   NOTE:
       The type for the "data" argument remains the same!
*/
void FTNCALL ipc_receive_real_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        data[i] = comm[*idcomm].data[i + (*pos)] ;
    }
}

/* ipc_send_dbl_c --
       Send an array of double precision reals

   Arguments:
       idcomm       Communication ID
       pos          Position of the real
       data         Array with values to send
       number       Number of values to send (twice actually)

   NOTE:
       The type for the "data" argument remains the same!
*/
void FTNCALL ipc_send_dbl_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        comm[*idcomm].data[i + (*pos)] = data[i] ;
    }
}

/* ipc_receive_dbl_c --
       Receive an array of double precision reals

   Arguments:
       idcomm       Communication ID
       pos          Position of the real
       data         Array to store the values in
       number       Number of values to receive (twice actually)

   NOTE:
       The type for the "data" argument remains the same!
*/
void FTNCALL ipc_receive_dbl_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        data[i] = comm[*idcomm].data[i + (*pos)] ;
    }
}

/* ipc_send_log_c --
       Send an array of logicals

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       data         Array with values to send
       number       Number of values to send

   NOTE:
       The type for the "data" argument remains the same!
*/
void FTNCALL ipc_send_log_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        comm[*idcomm].data[i + (*pos)] = data[i] ;
    }
}

/* ipc_receive_log_c --
       Receive an array of logicals

   Arguments:
       idcomm       Communication ID
       pos          Position of the logical
       data         Array to store the values in
       number       Number of values to receive

   NOTE:
       The type for the "data" argument remains the same!
*/
void FTNCALL ipc_receive_log_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        data[i] = comm[*idcomm].data[i + (*pos)] ;
    }
}

/* ipc_send_cmplx_c --
       Send an array of single precision complex numbers

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       data         Array with values to send
       number       Number of values to send (twice actually)

   NOTE:
       The type for the "data" argument remains the same!
*/
void FTNCALL ipc_send_cmplx_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        comm[*idcomm].data[i + (*pos)] = data[i] ;
    }
}

/* ipc_receive_complx_c --
       Receive an array of complex numbers

   Arguments:
       idcomm       Communication ID
       pos          Position of the logical
       data         Array to store the values in
       number       Number of values to receive (twice actually)

   NOTE:
       The type for the "data" argument remains the same!
*/
void FTNCALL ipc_receive_cmplx_c( int *idcomm, int *pos, int *data, int *number ) {
    int  i      ;

    for ( i = 0 ; i < (*number) ; i ++ ) {
        data[i] = comm[*idcomm].data[i + (*pos)] ;
    }
}

/* ipc_send_char_c --
       Send a character string

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       string       String to send
       number       Number of packets of 4 bytes
       len_string   Length of the string
*/
void FTNCALL ipc_send_char_c( int *idcomm, int *pos, char *string,
#ifdef INBETWEEN
    int len_string,
#endif
    int *number
#ifndef INBETWEEN
    , int len_string
#endif
    ) {

    /* TODO: beware of the last few bytes! */
    memcpy( &comm[*idcomm].data[(*pos)], string, 4*(*number) ) ;
}

/* ipc_receive_char_c --
       Receive an array of integers

   Arguments:
       idcomm       Communication ID
       pos          Position of the integer
       string       String to send
       len_string   Length of the string
*/
void FTNCALL ipc_receive_char_c( int *idcomm, int *pos, char *string,
#ifdef INBETWEEN
    int len_string,
#endif
    int *number
#ifndef INBETWEEN
    , int len_string
#endif
    ) {

    /* TODO: beware of the last few bytes! */
    memcpy( string, &comm[*idcomm].data[(*pos)], 4*(*number) ) ;
}
