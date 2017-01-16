! chk_constructor_mismatch.f90 --
!     Check if the compiler checks an obvious mismatch in array sizes at assignment
!
program chk_constructor_mismatch
    implicit none

    real, dimension(4) :: vector

    vector = (/ 1.0, 2.0, 3.0 /)

    write( *, '(a,4F10.4)' ) 'Vector: ', vector

    write( *, '(a)' )       'Apparently the compiler does not regard the shape mismatch as an error'

end program chk_constructor_mismatch
