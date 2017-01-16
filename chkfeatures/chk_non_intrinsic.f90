! chk_non_intrinsic --
!     Check: does the compiler support the non_intrinsic keyword?
!

! Use a module name that is almost certainly available as an intrinsic module
module iso_c_binding
     implicit none

     integer, parameter :: my_module_parameter = 1
end module iso_c_binding

program chk_non_intrinsic
    use, non_intrinsic :: iso_c_binding

    implicit none

    write( *, '(a)' )    'Using the user-defined "iso_c_binding" module ...'
    write( *, '(a,i0)' ) '    Value of the parameter: ', my_module_parameter
    write( *, '(a)' )    'Note: the keyword is supported'
end program chk_non_intrinsic
