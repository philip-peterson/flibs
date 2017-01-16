! test_permutations
!     Test the random permutation routines
!
!     $Id: test_random_permutation.f90,v 1.1 2012/12/23 10:18:13 arjenmarkus Exp $
!
program test_permutations
    use random_permutations

    integer, dimension(10)       :: array
    real, dimension(size(array)) :: mean
    integer                      :: i
    integer                      :: count = 1000

    mean = 0.0

    do i = 1,count
        call fill_random_permutation( array )

        mean = mean + array
    enddo

    mean = mean / count

    write(*,*) mean

    write(*,*) 'Minimum: ',minval(mean)
    write(*,*) 'Maximum: ',maxval(mean)
    write(*,*) 'Expected deviation: ',sqrt(real(size(array))/count) ! Correct?

end program test_permutations
