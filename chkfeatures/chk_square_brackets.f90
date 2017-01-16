! chk_square_brackets.f90 --
!     Check: does the compiler support square brackets for array constructors?
!
program chk_square_brackets
    implicit none

    integer, dimension(4) :: value

    value = [1, 2, 3, 4]

    write( *, '(a)' ) 'Square brackets are supported for array constructors'
end program chk_square_brackets
