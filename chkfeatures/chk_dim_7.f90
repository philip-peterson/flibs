! chk_dim_7.f90 --
!     Check: does the compiler support arrays with 7 dimensions
!
program chk_dim_7
    implicit none

    real, dimension(2,2,2,2, 2,2,2) :: array

    write( *, '(a)'      ) 'The compiler supports arrays of at least seven dimensions:'
    write( *, '(a,i0)'   ) '    Total size: ', size(array)
    write( *, '(a,i0)'   ) '    Dimensions: ', size(shape(array))
    write( *, '(a,10i3)' ) '    Shape:      ', shape(array)
end program chk_dim_7
