! random_permutation.f90 --
!     Provide routines for permuting arrays
!
!     Derived from a posting on comp.lang.fortran by Bart Vanderwoestyne
!
!     $Id: random_permutation.f90,v 1.1 2012/12/23 10:18:12 arjenmarkus Exp $
!
module random_permutations

    implicit none

    interface randomly_permute
        module procedure randomly_permute_int
    end interface

contains

! fill_random_permutation --
!     Fill an array with randomly permuted integers from 1 to N
!
! Arguments:
!     array         Array to be filled
!
! Result:
!     The array is filled with integers 1 to N in random order.
!     It can be used to permute arrays of arbitrary type like:
!
!     call fill_random_permutation( order )
!     data = data(order)
!
subroutine fill_random_permutation( array )
    integer, dimension(:), intent(out) :: array

    integer                            :: i

    array = (/ (i, i=1,size(array)) /)

    call randomly_permute( array )
end subroutine fill_random_permutation

! randomly_permute_int --
!     Randomly permute an array of integers
!
! Arguments:
!     array         Array to be permuted
!
! Result:
!     Randomly permuted content
!
! Note:
!     The algorithm is derived from the book by Donald Knuth:
!     Adaptation of Knuth Volume 2, Algorithm 3.4.2P.
!
subroutine randomly_permute_int( array )
    integer, dimension(:), intent(inout) :: array

    integer                :: i, j, k
    integer                :: temp
    real(kind=kind(1.0d0)) :: u


    do j=size(array),2,-1

        call random_number(u)
        k = floor(j*u) + 1

        !
        ! Exchange array(k) and array(j)
        !
        temp     = array(k)
        array(k) = array(j)
        array(j) = temp

    end do

endsubroutine randomly_permute_int

endmodule random_permutations
