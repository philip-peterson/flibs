! test_store.f90 --
!     Test program for the ftnunit_store module
!
program test_store

    use ftnunit_store

    implicit none

    integer                            :: i, j, k
    integer                            :: int
    integer, dimension(10)             :: int1d
    integer, dimension(10,11)          :: int2d
    integer, dimension(10,11,12)       :: int3d

    character(len=13)                      :: chr
    character(len=14), dimension(10)       :: char1d
    character(len=15), dimension(10,11)    :: char2d
    character(len=16), dimension(10,11,12) :: char3d

    integer                            :: int_retrieved
    integer, dimension(:), pointer     :: int1d_retrieved
    integer, dimension(:,:), pointer   :: int2d_retrieved
    integer, dimension(:,:,:), pointer :: int3d_retrieved

    character(len=13)                               :: char_retrieved
    character(len=14), dimension(:), pointer        :: char1d_retrieved
    character(len=15), dimension(:,:), pointer      :: char2d_retrieved
    character(len=16), dimension(:,:,:), pointer    :: char3d_retrieved

    integer                            :: lun
    character(len=60)                  :: description
    character(len=80)                  :: desc_retrieved ! Different length than description

    description = 'Test file for ftnunit_store module'


    int    = 123
    int1d  = (/ (i, i=1,10) /)
    int2d  = reshape( (/ ((i+10*j, i=1,10) ,j=1,11) /), (/ 10, 11 /) )
    int3d  = reshape( (/ (((i+10*j+110*k, i=1,10) ,j=1,11) ,k=1,12) /), (/ 10, 11, 12 /) )

    chr    = 'Scalar'
    char1d = (/ (char(i), i=1,10) /)
    char2d = reshape( (/ ((char(i+10*j), i=1,10) ,j=1,11) /), (/ 10, 11 /) )
    char3d = reshape( (/ (((char(i+10*j+k), i=1,10) ,j=1,11) ,k=1,12) /), (/ 10, 11, 12 /) )


    call test_open_storage_file( 'test_store.bin', lun, description, .true. )

    call test_store_data( lun, int,    'scalar integer int'  )
    call test_store_data( lun, int1d,  'integer array int1d' )
    call test_store_data( lun, int2d,  'integer array int2d' )
    call test_store_data( lun, int3d,  'integer array int3d' )
    call test_store_data( lun, chr,    'scalar character char'  )
    call test_store_data( lun, char1d, 'character array char1d' )
    call test_store_data( lun, char2d, 'character array char2d' )
    call test_store_data( lun, char3d, 'character array char3d' )

    call test_close_storage_file( lun )

    !
    ! Now retrieve the data and compare
    !

    call test_open_storage_file( 'test_store.bin', lun, desc_retrieved, .false. )

    if ( desc_retrieved /= description ) then
        write(*,*) 'Difference in file description'
        write(*,*) '    Expected: ', description
        write(*,*) '    Actual:   ', desc_retrieved
    endif

    call test_retrieve_data( lun, int_retrieved,   desc_retrieved )
    call test_retrieve_data( lun, int1d_retrieved, desc_retrieved )
    call test_retrieve_data( lun, int2d_retrieved, desc_retrieved )
    call test_retrieve_data( lun, int3d_retrieved, desc_retrieved )

    call test_retrieve_data( lun, char_retrieved, desc_retrieved )
    call test_retrieve_data( lun, char1d_retrieved, desc_retrieved )
    call test_retrieve_data( lun, char2d_retrieved, desc_retrieved )
    call test_retrieve_data( lun, char3d_retrieved, desc_retrieved )

    call test_close_storage_file( lun )

    if ( int_retrieved /= int ) then
        write(*,*) 'Difference in scalar: ', int, ' -- ', int_retrieved
    endif

    if ( .not. associated( int1d_retrieved ) ) then
        write(*,*) 'Not associated: 1D array'
    else
        write(*,*) 'Associated:', associated(int1d_retrieved)
        write(*,*) 'Shape:', Shape(int1d_retrieved)
        write(*,*) 'Shape:', Shape(int1d)
        if ( any( shape(int1d_retrieved) /= shape(int1d) ) ) then
            write(*,*) 'Mismatch in shape: 1D array', shape(int1d_retrieved), ' -- ', shape(int1d)
        else
            if ( any( int1d_retrieved /= int1d ) ) then
                write(*,*) 'Difference in 1D array'
            endif
        endif
    endif

    if ( .not. associated( int2d_retrieved ) ) then
        write(*,*) 'Not associated: 2D array'
    else
        if ( any( shape(int2d_retrieved) /= shape(int2d) ) ) then
            write(*,*) 'Mismatch in shape: 2D array', shape(int2d_retrieved), ' -- ', shape(int2d)
        else
            if ( any( int2d_retrieved /= int2d ) ) then
                write(*,*) 'Difference in 2D array'
            endif
        endif
    endif

    if ( .not. associated( int3d_retrieved ) ) then
        write(*,*) 'Not associated: 3D array'
    else
        if ( any( shape(int3d_retrieved) /= shape(int3d) ) ) then
            write(*,*) 'Mismatch in shape: 3D array', shape(int3d_retrieved), ' -- ', shape(int3d)
        else
            if ( any( int3d_retrieved /= int3d ) ) then
                write(*,*) 'Difference in 3D array'
            endif
        endif
    endif


    if ( char_retrieved /= chr ) then
        write(*,*) 'Difference in scalar character: ', chr, ' -- ', char_retrieved
    endif

    if ( .not. associated( char1d_retrieved ) ) then
        write(*,*) 'Not associated: 1D character array'
    else
        if ( any( shape(char1d_retrieved) /= shape(char1d) ) ) then
            write(*,*) 'Mismatch in shape: 1D character array', shape(char1d_retrieved), ' -- ', shape(char1d)
        else
            if ( any( char1d_retrieved /= char1d ) ) then
                write(*,*) 'Difference in 1D character array'
            endif
        endif
    endif

    if ( .not. associated( char2d_retrieved ) ) then
        write(*,*) 'Not associated: 2D character array'
    else
        if ( any( shape(char2d_retrieved) /= shape(char2d) ) ) then
            write(*,*) 'Mismatch in shape: 2D character array', shape(char2d_retrieved), ' -- ', shape(char2d)
        else
            if ( any( char2d_retrieved /= char2d ) ) then
                write(*,*) 'Difference in 2D character array'
            endif
        endif
    endif

    if ( .not. associated( char3d_retrieved ) ) then
        write(*,*) 'Not associated: 3D character array'
    else
        if ( any( shape(char3d_retrieved) /= shape(char3d) ) ) then
            write(*,*) 'Mismatch in shape: 3D character array', shape(char3d_retrieved), ' -- ', shape(char3d)
        else
            if ( any( char3d_retrieved /= char3d ) ) then
                write(*,*) 'Difference in 3D character array'
            endif
        endif
    endif

    !
    ! Final test: overwrite existing file?
    !
    write(*,*) 'Overwriting existing file - this should cause an error'

    call test_open_storage_file( 'test_store.bin', lun, desc_retrieved, .true. )

    write(*,*) 'Error! This message indicates the open routine did not stop the program!'

end program test_store
