! chk_iso_fortran_storage.f90
!     Check: report the various storage units from the ISO_FORTRAN_ENV module
!
program chk_iso_fortran_storage
    use iso_fortran_env

    implicit none

    write( *, '(a,i5,a)'  ) 'Character storage size:', character_storage_size, ' bits'
    write( *, '(a,i5,a)'  ) 'Numeric storage size:  ', numeric_storage_size,   ' bits'
    write( *, '(a,i5,a)'  ) 'File storage size:     ', file_storage_size,      ' bits'
    write( *, '(a)'       ) '    (used for direct and stream access files)'
    write( *, '(/,a)'     ) 'Integer kinds:'
    write( *, '(a,i5,a)'  ) '    INT8 ', int8,  merge( '                ', ' - not supported', int8  /= -1 )
    write( *, '(a,i5,a)'  ) '    INT16', int16, merge( '                ', ' - not supported', int16 /= -1 )
    write( *, '(a,i5,a)'  ) '    INT32', int32, merge( '                ', ' - not supported', int32 /= -1 )
    write( *, '(a,i5,a)'  ) '    INT64', int64, merge( '                ', ' - not supported', int64 /= -1 )
    write( *, '(/,a)'     ) 'Real kinds:'
    write( *, '(a,i5,a)'  ) '    REAL32 ', real32,  merge( '                ', ' - not supported', real32  /= -1 )
    write( *, '(a,i5,a)'  ) '    REAL64 ', real64,  merge( '                ', ' - not supported', real64  /= -1 )
    write( *, '(a,i5,a)'  ) '    REAL128', real128, merge( '                ', ' - not supported', real128 /= -1 )

end program chk_iso_fortran_storage
