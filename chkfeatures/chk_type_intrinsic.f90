! chk_type_intrinsic --
!     Check: does the compiler support the Fortran 2008 syntax of type(intrinsic)?
!
!     Note: such syntax unifies the way to define variables of intrinsic and derived types.
!     This could be used for a form of generic programming
!
program chk_type_intrinsic
    implicit none

    type(integer) :: value

    value = 1
    write(*, '(a,i0)') 'Value: ', value
end program chk_type_intrinsic
