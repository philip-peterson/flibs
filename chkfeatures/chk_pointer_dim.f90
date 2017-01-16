! chk_pointer_dim.f90 --
!     Check: does the compiler support a 2D pointer being associated to a 1D array?
!
!     Note: this is described in the document n1729.pdf by John Reid on Fortran 2008
!     as being allowed in Fortran 2003
!
program chk_pointer_dim
    implicit none

    real, dimension(100), target  :: array
    real, dimension(:,:), pointer :: matrix

    call random_number( array )

    matrix(1:20,1:5) => array

    array(1)  = 1.0
    array(21) = 2.0
    array(41) = 3.0

    write( *, '(a)' )        'Two-dimensional pointer, pointing to one-dimensional array'
    write( *, '(a,i5)' )     'Shape 1D array:  ', shape(array)
    write( *, '(a,2i5)' )    'Shape 2D matrix: ', shape(matrix)
    write( *, '(a,5f10.4)' ) 'Values: ', matrix(1,1:5)

    ! This doesn't work:
    ! write( *, '(a,l)' )   'Associated?      ', associated(matrix,array)

end program chk_pointer_dim
