! chk_contiguous.f90 --
!     Check if the compiler supports the CONTIGUOUS attribute
!
module contiguous_arrays
    implicit none
contains
subroutine copy_double_array( a, b )
    real, dimension(:), contiguous :: a, b

    a = 2.0 * b
end subroutine copy_double_array
end module contiguous_arrays

program chk_contiguous
    use contiguous_arrays

    implicit none

    real, dimension(10) :: a, b

    call random_number( b )

    call copy_double_array( a, b )

    write( *, '(a)' ) 'The compiler supports the CONTIGUOUS attribute'
end program chk_contiguous
