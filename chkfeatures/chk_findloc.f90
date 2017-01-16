! chk_findloc --
!     Check: does the compiler support the findloc function?
!
program chk_findloc
    implicit none

    integer, dimension(10) :: array = (/ 1, 2, 3, 2, 4, 3, 2, 4, 5, 6 /)
    integer                :: i

    write( *, '(a,10i2)' ) 'Array of integers:', array
    write( *, '(a)' )      '    Location of integers:'
    do i = 1,size(array)
        write( *, '(4x,i4,a,i4)') i, ': ', findloc( array, i )
    enddo
end program chk_findloc
