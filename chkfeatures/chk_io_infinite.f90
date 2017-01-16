! chk_io_infinite --
!     Check: does the compiler support reading and writing of "infinite" numbers?
!
program chk_io_infinite
    implicit none

    integer            :: ierr
    real, dimension(4) :: value
    character(len=60)  :: data
    logical            :: isinfinite

    isinfinite = .false.
    !
    ! Infinity: reading
    !
           !123456789012345678901234567890
    data = "1.0       Inf       Infinity  2.0"

    write( *, '(a)' )     'Reading infinity from a string:'
    write( *, '(4x,2a)' ) 'String: ', trim(data)

    read( data, *, iostat = ierr ) value

    if ( ierr == 0 ) then
        write( *, '(4x,a)' )    'List-directed reading possible without any error'
    else
        write( *, '(4x,a,i0)' ) 'List-directed reading caused an error - iostat = ', ierr
    endif

    if ( value(2) > huge(value(2)) .and. value(3) > huge(value(3)) ) then
        isinfinite = .true.
        write( *, '(4x,a)' ) 'Values 2 and 3 are indeed infinite'
    else
        write( *, '(4x,a)' ) 'Values 2 and 3 shoud have been infinite'
    endif

    read( data, '(4f10.0)', iostat = ierr ) value

    if ( ierr == 0 ) then
        write( *, '(4x,a)' )    'Formatted reading possible without any error'
    else
        write( *, '(4x,a,i0)' ) 'Formatted reading caused an error - iostat = ', ierr
    endif

    if ( value(2) > huge(value(2)) .and. value(3) > huge(value(3)) ) then
        isinfinite = .true.
        write( *, '(4x,a)' ) 'Values 2 and 3 are indeed infinite'
    else
        write( *, '(4x,a)' ) 'Values 2 and 3 shoud have been infinite'
    endif

    if ( isinfinite ) then
        write( *, '(/,4x,a,f10.0)' ) 'Infinity is written via "f10.0" as: ', value(2)
        write( *, '(4x,a,f4.0)'  )   'Infinity is written via "f4.0" as:  ', value(2)
        write( *, '(4x,a,e12.4)' )   'Infinity is written via "e12.4" as: ', value(2)
    else
        write( *, '(/,a,e12.4)' ) 'The "value" Infinity was not read correctly - no attempt to write it'
    endif


    data = "1.0       Nan       NAN       2.0"

    write( *, '(/,a)' )     'Reading Not-a-number from a string:'
    write( *, '(4x,2a)' ) 'String: ', trim(data)

    read( data, *, iostat = ierr ) value

    if ( ierr == 0 ) then
        write( *, '(4x,a)' )    'List-directed reading possible without any error'
    else
        write( *, '(4x,a,i0)' ) 'List-directed reading caused an error - iostat = ', ierr
    endif

    read( data, '(4f10.0)', iostat = ierr ) value

    if ( ierr == 0 ) then
        write( *, '(4x,a)' )    'Formatted reading possible without any error'
    else
        write( *, '(4x,a,i0)' ) 'Formatted reading caused an error - iostat = ', ierr
    endif

    write( *, '(/,a,/,a)' ) 'Note: no attempt to check for or write "not-a-number" - ',   &
                            'this would be reliable only with the IEEE_ARITHMETIC module'

end program chk_io_infinite
