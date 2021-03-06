! test_libdate.f90 --
!     Simple tests for the libdate module
!
program test_diff
    use libdate

    implicit none

    type(DATETYPE)    :: date1, date2, date3
    integer           :: i
    logical           :: found
    logical           :: error

    character(len=20) :: datestring

    date1 = datetype( 2008, 1, 1, 0, 0 ) ! 1 january 2008, 0:00)
    date2 = datetype( 2009, 1, 1, 0, 0 ) ! 1 january 2009, 0:00)
    date3 = datetype( 2009, 2, 1, 0, 0 ) ! 1 february 2009, 0:00)

    write(*,*) 'Day of the year: ', doy(date1)
    write(*,*) 'Day of the year: ', doy(date2)
    write(*,*) 'Day of the year: ', doy(date3)
    write(*,*) '1 january 2008 > 1 january 2009? ', date1 > date2
    write(*,*) '1 january 2008 < 1 january 2009? ', date1 < date2
    write(*,*) 'First of 1 january 2008, 1 february 2009? ', mindate(date1 , date2)
    write(*,*) 'Last of 1 january 2008, 1 february 2009?  ', maxdate(date1 , date2)

    date3 = datetype( 2009, 2, 1, 10, 1 ) ! 1 february 2009, 10:01)

    write(*,*) 'Time between 1 january 0:00 and 1 february 10:01 (in days): ', &
        timelag(date3 , date2)

    call format_date( date3, 'yyyy/mm/dd HH:MM', datestring )
    write(*,*) 'Date: ', datestring
    call format_date( date3, 'yyyy/ms/ds HS:MS', datestring )
    write(*,*) 'Date: ', datestring

    !
    ! Scan strings
    !
    datestring = '2001/01/02     10:11'
    call scan_date( 'yyyy/ms/ds HS:MS', datestring, date3, error )
    write(*,*) 'Date: ', date3, ' (error: ', error, ' - expecting NO error)'

    datestring = '2001/1/02 10:11'
    call scan_date( 'yyyy/mm/dd HS:MS', datestring, date3, error )
    write(*,*) 'Date: ', date3, ' (error: ', error, ' - should be an error)'

end program
