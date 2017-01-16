! test_dbasefile.f90 --
!     Test program for the dbase_reader module (reading dBase III files)
!
program test_dbasefile
    use dbase_reader

    implicit none

    type(dbasefile)                             :: db
    type(dbasedata),  dimension(:), allocatable :: data
    type(dbasefield), dimension(:), allocatable :: field
    logical                                     :: error
    integer                                     :: i, j
    integer                                     :: ncols, nrows

    call dbase_load( db, "example.dbf", error )

    nrows = dbase_number_records(db)
    ncols = dbase_number_fields(db)
    allocate( data(ncols), field(ncols) )

    call dbase_get_fields( db, field, error )

    write(*,'(a)') 'Fields:'
    do i = 1,ncols
        write(*,'(i5,1x,a10,1x,a,1x,i5,i5)') i, field(i)%name, field(i)%type, &
            field(i)%width, field(i)%decimals
    enddo

    do i = 1,nrows
        write(*,'(a,i5)') 'Record:', i
        call dbase_get_record( db, i, data, error )
        do j = 1,ncols
            select case( data(j)%type )
                case( 'N' )
                    write(*,'(4x,a10,1x,e16.6)') field(j)%name, data(j)%dvalue

                case( 'I' )
                    write(*,'(4x,a10,1x,i10)') field(j)%name, data(j)%ivalue

                case( 'L' )
                    write(*,'(4x,a10,1x,a)') field(j)%name, merge( 'true ', 'false', data(j)%lvalue )

                case( 'D', 'S' )
                    write(*,'(4x,a10,1x,a)') field(j)%name, trim(data(j)%svalue)
            end select
        enddo
    enddo

    call dbase_release( db )

end program test_dbasefile
