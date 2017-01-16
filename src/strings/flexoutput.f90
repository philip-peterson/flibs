! flexoutput.f90 --
!     Module to provide for output strings with flexible substitution of data
!     For instance:
!     "format string": "The value of {2} is {1}"
!     where {2} will be substituted by the second array-element and {1} by the
!     first, depending on the data type of each element.
!
!     TODO:
!     A lot - explicit formats, newline via {/}, logicals, arrays
!
module flexoutput
    implicit none

    type item_t
        integer                       :: value_type
        integer                       :: int_value
        real(kind=kind(1.0))          :: real_value
        real(kind=kind(1.0d0))        :: double_value
        character(len=:), allocatable :: string_value
    end type item_t

    interface item
        module procedure item_int_value
        module procedure item_real_value
        module procedure item_double_value
        module procedure item_string_value
    end interface

contains
! write_items --
!     Write a string with substitutions to the file
!
! Arguments:
!     lun          LU-number of the file
!     format       Format string to be used
!     item_array   Array of items
!     advance      Write a newline or not (default to yes)
!
subroutine write_items( lun, format, item_array, advance )
    integer, intent(in)                    :: lun
    character(len=*), intent(in)           :: format
    type(item_t), dimension(:), intent(in) :: item_array
    logical, optional, intent(in)          :: advance

    integer                                :: start
    integer                                :: pos1
    integer                                :: pos2
    integer                                :: indx
    character(len=20)                      :: substring
    logical                                :: adv

    start = 1
    do
        pos1 = index( format(start:), '{' )
        pos2 = index( format(start:), '}' )

        ! NOTE: No error handling at the moment

        !
        ! Write the literal string before {
        !
        if ( pos1 > 0 ) then
            if ( pos1 /= 1 ) then
                write( lun, '(a)', advance = 'no' ) format(start:start+pos1-2)
            endif

            substring = format(start+pos1:start+pos2-2)

            read( substring, * ) indx

            if ( indx < 1 .or. indx > size(item_array) ) then
                write( lun, '(a)', advance = 'no' ) '??'
            else
                select case ( item_array(indx)%value_type )
                    case( 1 )
                        write( lun, '(i0)', advance = 'no' ) item_array(indx)%int_value
                    case( 2 )
                        write( lun, '(g0)', advance = 'no' ) item_array(indx)%real_value
                    case( 3 )
                        write( lun, '(g0)', advance = 'no' ) item_array(indx)%double_value
                    case( 4 )
                        write( lun, '(a)', advance = 'no' )  item_array(indx)%string_value
                    case default
                        write( lun, '(a)', advance = 'no' ) '??'
                end select
            endif
        else
            write( lun, '(a)', advance = 'no' ) trim(format(start:))
            exit
        endif

        start = start + pos2
    enddo

    if ( present(advance) ) then
        adv = advance
    else
        adv = .true.
    endif

    if ( adv ) then
        write( lun, '(a)' ) ''
    endif
end subroutine write_items

function item_int_value( value )
    integer, intent(in) :: value
    type(item_t)        :: item_int_value

    item_int_value%value_type = 1
    item_int_value%int_value  = value
end function item_int_value

function item_real_value( value )
    real(kind=kind(1.0)), intent(in) :: value
    type(item_t)                     :: item_real_value

    item_real_value%value_type = 2
    item_real_value%real_value  = value
end function item_real_value

function item_double_value( value )
    real(kind=kind(1.0d0)), intent(in) :: value
    type(item_t)                       :: item_double_value

    item_double_value%value_type = 3
    item_double_value%double_value  = value
end function item_double_value

function item_string_value( value )
    character(len=*), intent(in) :: value
    type(item_t)                 :: item_string_value

    item_string_value%value_type = 4
    item_string_value%string_value  = value
end function item_string_value

end module flexoutput
