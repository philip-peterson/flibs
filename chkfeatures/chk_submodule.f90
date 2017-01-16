! chk_submodule.f90
!     Check: does the compiler support sumodules?
!
module base_module
    implicit none

    interface
        real module function add_some_number( value )
            real, intent(in) :: value
        end function add_some_number
    end interface
end module base_module

submodule(base_module) actual_function
contains
module procedure add_some_number
    some_number = 1.0 + value
end procedure add_some_number
end submodule actual_function

program chk_submodule
    use base_module

    implicit none

    real :: x, y

    x = 1.0
    y = add_some_number( x )

    write( *, '(a)'   ) 'Submodules are supported'

end program chk_submodule
