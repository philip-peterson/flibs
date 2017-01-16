! enum_space.f90 --
!     Module with subroutines to enumerate grid points in space
!
!     Motivation:
!     When solving Diophantine equations with brute force (so simply try
!     all combinations of integer values and see whether that gives a
!     solution), one would like to search a limited region of the solution
!     space and stop after, say, 100 solutions. For instance:
!
!     x**2 + y**2 = z**3 + 3*z**2
!
!     You never know in which region these solutions lie, so it may that
!     you need to examine a very large range for all variables involved:
!
!     do z = 1,1000
!         do y = 1,1000
!             do x = 1,1000
!                 ... is (x,y,z) a solution? Print it
!             enddo
!         enddo
!     enddo
!
!     The drawback is that you examine the cube line by line and if
!     the solutions have coordinates of the same order of magnitude,
!     So examining solutions with very different sizes (x=1000, y= 10, z=1)
!     may waste a lot of time.
!     The procedures in this module use a simple enumeration technique that
!     makes it possible to enumerate the grid points around the origin
!     in ever wider shells (square around the origin or an octaeder in 3D
!     space):
!
!     |
!     9
!     |
!     5   8   .
!     | .   .   .
!     2   4   7   .
!     | .   .   .   .
!     0---1---3---6--10-
!
!     This works for all dimensions - just repeatedly split up the
!     coordinates (see the implementation of enum_octant), but it
!     is non-trivial to expand this to an arbitrary dimension.
!
!     To cover both positive and negative integers, the module uses an
!     alternating scheme: 0, 1, -1, 2, -2, 3, -3, ...
!
!     TODO: enum_ndspace? Non-trivial as said
!
module enumerate_space
    implicit none

    integer, parameter :: start_enumeration = 0

contains

! enum_1dspace --
!     Enumerate the grid points on a line
!
! Arguments:
!     index         The index of the grid point (updated to give the next
!                   index, unless you specify update = .false.)
!     x             The x-coordinate of the grid point (output)
!     update        (Optional) whether to update the index (default) or not
!
! Result:
!     The x-coordinate of the grid point with the given index
!
! Note:
!     Initialise the index to "start_enumeration"
!
subroutine enum_1dspace( index, x, update )
    integer, intent(inout)        :: index
    integer, intent(out)          :: x
    logical, intent(in), optional :: update

    logical                       :: do_update

    do_update = .true.
    if ( present(update) ) then
        do_update = update
    endif

    x = (index + 1) / 2
    if ( mod(index,2) == 0 ) then
        x = -x
    endif

    if ( do_update ) then
        index = index + 1
    endif
end subroutine enum_1dspace

! enum_2dspace --
!     Enumerate the grid points on a plane
!
! Arguments:
!     index         The index of the grid point (updated to give the next
!                   index, unless you specify update = .false.)
!     x             The x-coordinate of the grid point (output)
!     y             The y-coordinate of the grid point (output)
!     update        (Optional) whether to update the index (default) or not
!
! Result:
!     The x- and y-coordinates of the grid point with the given index
!
! Note:
!     Initialise the index to "start_enumeration"
!
subroutine enum_2dspace( index, x, y, update )
    integer, intent(inout)        :: index
    integer, intent(out)          :: x
    integer, intent(out)          :: y
    logical, intent(in), optional :: update

    logical                       :: do_update
    integer                       :: xx, yy

    do_update = .true.
    if ( present(update) ) then
        do_update = update
    endif

    call enum_quadrant( index, xx, yy, do_update )
    call enum_1dspace( xx, x )
    call enum_1dspace( yy, y )

end subroutine enum_2dspace

! enum_3dspace --
!     Enumerate the grid points in 3D space
!
! Arguments:
!     index         The index of the grid point (updated to give the next
!                   index, unless you specify update = .false.)
!     x             The x-coordinate of the grid point (output)
!     y             The y-coordinate of the grid point (output)
!     z             The z-coordinate of the grid point (output)
!     update        (Optional) whether to update the index (default) or not
!
! Result:
!     The x- and y-coordinates of the grid point with the given index
!
! Note:
!     Initialise the index to "start_enumeration"
!
subroutine enum_3dspace( index, x, y, z, update )
    integer, intent(inout)        :: index
    integer, intent(out)          :: x
    integer, intent(out)          :: y
    integer, intent(out)          :: z
    logical, intent(in), optional :: update

    logical                       :: do_update
    integer                       :: xx, yy, zz

    do_update = .true.
    if ( present(update) ) then
        do_update = update
    endif

    call enum_octant( index, xx, yy, zz, do_update )
    call enum_1dspace( xx, x )
    call enum_1dspace( yy, y )
    call enum_1dspace( zz, z )

end subroutine enum_3dspace

! enum_quadrant --
!     Enumerate the grid points in the first 2D quadrant
!
! Arguments:
!     index         The index of the grid point (updated to give the next
!                   index, unless you specify update = .false.)
!     x             The x-coordinate of the grid point (output)
!     y             The y-coordinate of the grid point (output)
!     update        (Optional) whether to update the index (default) or not
!
! Result:
!     The x- and y-coordinates of the grid point with the given index
!
! Note:
!     Initialise the index to "start_enumeration"
!
subroutine enum_quadrant( index, x, y, update )
    integer, intent(inout)        :: index
    integer, intent(out)          :: x
    integer, intent(out)          :: y
    logical, intent(in), optional :: update

    integer, parameter            :: dp = kind(1.0d0)
    real(kind=dp)                 :: dindex
    real(kind=dp)                 :: dn
    integer                       :: n
    logical                       :: do_update

    do_update = .true.
    if ( present(update) ) then
        do_update = update
    endif

    !
    ! Use double-precision reals to avoid rounding and overflow problems
    ! And take care of very large indices
    !
    dindex = index

    dn     = -0.5_dp + sqrt( 200.0 * dindex + 25.0_dp + 1.0_dp ) / 10.0_dp
    n      = dn

    if ( mod(n,2) == 0 ) then
        y = index - (n / 2) * (n + 1)
    else
        y = index - n * ((n + 1) / 2)
    endif
    x  = n - y

    if ( do_update ) then
        index = index + 1
    endif
end subroutine enum_quadrant

! enum_octant --
!     Enumerate the grid points in the first 3D octant
!
! Arguments:
!     index         The index of the grid point (updated to give the next
!                   index, unless you specify update = .false.)
!     x             The x-coordinate of the grid point (output)
!     y             The y-coordinate of the grid point (output)
!     z             The y-coordinate of the grid point (output)
!     update        (Optional) whether to update the index (default) or not
!
! Result:
!     The x-, y and z-coordinates of the grid point with the given index
!
! Note:
!     Initialise the index to "start_enumeration"
!
subroutine enum_octant( index, x, y, z, update )
    integer, intent(inout)        :: index
    integer, intent(out)          :: x
    integer, intent(out)          :: y
    integer, intent(out)          :: z
    logical, intent(in), optional :: update

    integer, parameter            :: dp = kind(1.0d0)
    real(kind=dp)                 :: dindex
    real(kind=dp)                 :: dn
    integer                       :: n
    integer                       :: nindex, prev_index
    logical                       :: do_update

    do_update = .true.
    if ( present(update) ) then
        do_update = update
    endif

    !
    ! We can not use the same approach as for 2D, as it difficult to solve
    ! the third-degree equation that would be involved. Use the brute force
    ! approach instead.
    !
    ! For each plane we have n(n+1)/2 points, so count how many such planes
    ! are completely filled. Some manipulation of the numbers involved is
    ! required to make sure all points are covered.
    !
    nindex = index + 1
    n      = 1

    do while( nindex > 0 )
        prev_index = nindex
        if ( mod(n,2) == 0 ) then
            nindex = nindex - (n / 2) * (n + 1)
        else
            nindex = nindex - n * ((n + 1) / 2)
        endif

        n = n + 1
    enddo

    nindex = prev_index - 1
    n      = n - 2
    call enum_quadrant( nindex, x, y )
    z = n - x - y

    if ( do_update ) then
        index = index + 1
    endif
end subroutine enum_octant
end module enumerate_space
