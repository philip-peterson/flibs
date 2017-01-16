! test_enum_space.f90 --
!     Test program for enumerate_space module
!
program test_enum
    use enumerate_space

    implicit none

    integer :: i, index, x, y, z

    index = start_enumeration

    do i = 1,20
        call enum_quadrant( index, x, y )
        write(*,'(3i5)') index, x, y
    enddo

    x     = 60000
    index = (x / 2) * (x + 1) - 2 ! Watch out for overflows
    do i = 1,5
        call enum_quadrant( index, x, y )
        write(*,'(3i12)') index, x, y
    enddo

    index = start_enumeration
    do i = 1,10
        call enum_1dspace( index, x )
        write(*,'(3i12)') index, x
    enddo

    !
    ! Test the enumeration of the 3D octant
    !
    index = start_enumeration
    do i = 1,20
        call enum_octant( index, x, y, z )
        write(*,'(4i12)') index, x, y, z
    enddo

    !
    ! Test the enumeration of 2D space
    !
    index = start_enumeration
    do i = 1,20
        call enum_2dspace( index, x, y )
        write(*,'(4i12)') index, x, y
    enddo

!
! 2D space:
!
!                    +
!                    |
!              19   10   14
!                    |
!         17    8    3    5   12
!                    |
!   --+---11----4----1----2----7---16---
!                    |
!              13    6    9   18
!                    |
!                   15   20
!                    |
!                    +
!                    |
end program test_enum
