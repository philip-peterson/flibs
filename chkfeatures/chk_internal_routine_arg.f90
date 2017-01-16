! chk_internal_routine_arg.f90 --
!     Check: does the compiler support passing internal routines as arguments?
!
module pass_routines
    implicit none
contains
subroutine run_func( func )
    integer :: x

    interface
        integer function func( y )
            implicit none
            integer, intent(in) :: y
        end function func
    end interface

    x = func(1)
    write( *, '(a,i0)' ) 'Value: ', x
    if ( x == 1 ) then
        write( *, '(a)' ) '    Value is as expected'
    else
        write( *, '(a)' ) '    Unexpected value - please check!'
    endif
end subroutine run_func
end module pass_routines

program chk_internal_routine_arg
    use pass_routines

    implicit none

    integer, dimension(4) :: array

    array = (/ 1, 2, 3, 4 /)

    call run_func( f )

    write( *, '(a)' ) 'Passing internal routines as argument is supported'
contains
integer function f( y )
    integer, intent(in) :: y

    f = array(y)
end function f
end program chk_internal_routine_arg
