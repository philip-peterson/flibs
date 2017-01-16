! traverse_grid.f90 --
!     Module to traverse an N-dimensional grid of points
!
!     Three types defined:
!     grid_traversing  - traverse a grid along the grid points
!                        (integer coordinates only)
!     grid_coordinates - traverse a grid along the grid points
!                        but convert the indices to (real) coordinates
!                        of the midpoints
!     grid_sampler     - sample the grid points in a quasi-random way;
!                        useful as an alternative to Monte Carlo
!
module traverse_grid
    use select_precision

    implicit none

    !
    ! Type to systematically traverse an N-dimensional grid
    !
    type grid_traversing
        integer, dimension(:), allocatable :: index
        integer, dimension(:), allocatable :: max_index
    contains
        procedure :: init_grid_traversing_uniform
        procedure :: init_grid_traversing_diverse
        procedure :: next    => next_point_grid_traversing
        procedure :: reset   => reset_grid_traversing
        procedure :: indices => indices_grid_traversing
        generic   :: init    => init_grid_traversing_uniform, &
                                init_grid_traversing_diverse
    end type grid_traversing

    !
    ! Type to use real coordinates instead of integer indices
    !
    type, extends(grid_traversing) :: grid_coordinates
        real(kind=wp), dimension(:,:), allocatable :: range
    contains
        procedure :: init_grid_coordinates_uniform
        procedure :: init_grid_coordinates_diverse
        procedure :: coords  => coords_grid_coordinates
        generic   :: init    => init_grid_coordinates_uniform, &
                                init_grid_coordinates_diverse
    end type grid_coordinates

    !
    ! Type to step through a grid using prime steps so that
    ! you can sample a function more effectively (an alternative
    ! to the Monte Carlo method)
    !
    type, extends(grid_coordinates) :: grid_sampler
        integer, dimension(:), allocatable         :: step
    contains
!        procedure :: init_grid_sampler_uniform
!        procedure :: init_grid_sampler_diverse
!        generic   :: init   => init_grid_sampler_uniform, &
!                               init_grid_sampler_diverse
        procedure :: next   => next_point_grid_sampler
    end type grid_sampler

    interface init_grid_traversing
        module procedure init_grid_traversing_uniform
        module procedure init_grid_traversing_diverse
    end interface

    interface next_point
        module procedure next_point_grid_traversing
    end interface

    private :: determine_steps

contains

! init_grid_traversing_uniform --
!     Initialise a grid traversing object with uniform dimension
!
! Arguments:
!     traverse      The traversing object
!     dimensions    Number of dimensions
!     max_index     Maximum value for the indexinates (holds for all dimensions)
!
! Result:
!     Filled in traversing object
!
subroutine init_grid_traversing_uniform( traverse, dimensions, max_index )
    class(grid_traversing)        :: traverse
    integer, intent(in)           :: dimensions
    integer, intent(in)           :: max_index

    if ( allocated(traverse%index) ) then
        deallocate( traverse%index     )
        deallocate( traverse%max_index )
    endif

    allocate( traverse%index(dimensions)     )
    allocate( traverse%max_index(dimensions) )

    traverse%index     = 0
    traverse%max_index = max_index
end subroutine init_grid_traversing_uniform

! init_grid_traversing_diverse --
!     Initialise a grid traversing object with diverse dimensions
!
! Arguments:
!     traverse      The traversing object
!     max_index     Array with the maximum values for the indexinates per dimension
!
! Result:
!     Filled in traversing object
!
subroutine init_grid_traversing_diverse( traverse, max_index )
    class(grid_traversing)            :: traverse
    integer, dimension(:), intent(in) :: max_index

    if ( allocated(traverse%index) ) then
        deallocate( traverse%index     )
        deallocate( traverse%max_index )
    endif

    allocate( traverse%index(size(max_index))     )
    allocate( traverse%max_index(size(max_index)) )

    traverse%index     = 0
    traverse%max_index = max_index
end subroutine init_grid_traversing_diverse

! init_grid_coordinates_specific --
!     Initialise the specific part of a grid coordinates object
!
! Arguments:
!     coordinates   The coordinates object
!     range         Range for the actual coordinates (minimum and maximum per dimension;
!                   second dimension is the number of dimensions for the grid
!
! Result:
!     Filled in specific parts of the coordinates object
!
subroutine init_grid_coordinates_specific( coordinates, range )
    class(grid_coordinates)                   :: coordinates
    real(kind=wp), dimension(:,:), intent(in) :: range

    integer                                   :: prime
    integer                                   :: i

    if ( allocated(coordinates%range) ) then
        deallocate( coordinates%range )
    endif

    allocate( coordinates%range(2,size(range,2)) )

    !
    ! Use the range to determine the first coordinate value and the step per dimension
    !
    coordinates%range(2,:) = ( range(2,:) - range(1,:) ) / coordinates%max_index
    coordinates%range(1,:) = range(1,:) + 0.5_wp * coordinates%range(2,:)
end subroutine init_grid_coordinates_specific

! init_grid_sampler_specific --
!     Initialise the specific part of a grid sampler object
!
! Arguments:
!     sampler       The sampler object
!
! Result:
!     Filled in specific parts of the sampler object
!
subroutine init_grid_sampler_specific( sampler )
    class(grid_sampler)                       :: sampler

    !
    ! The most involved part: determine the primes to step through each dimension
    ! Postpone this until the next point is determined, just initialise the
    ! steps.
    !
    if ( allocated(sampler%step) ) then
        deallocate( sampler%step  )
    endif
    allocate( sampler%step(size(sampler%range,2)) )

    sampler%step = 37

end subroutine init_grid_sampler_specific

! determine_steps --
!     Private routine to set the prime steps whenever
!     this is required.
!
! Arguments:
!     sampler         Grid sampler object
!
! Note:
!     This routine is called whenever the sampler completes
!     a cycle (recognised by
!
subroutine determine_steps( sampler )
    type(grid_sampler) :: sampler

    integer            :: prime
    integer            :: i

    prime = maxval( sampler%step )

    do i = 1,size(sampler%step)
        call get_next_prime( prime )
        do
            if ( mod( sampler%max_index(i), prime ) /= 0 ) then
                sampler%step(i) = prime
                exit
            else
                call get_next_prime( prime )
            endif
        enddo
    enddo
contains
subroutine get_next_prime( prime )
    integer :: prime
    integer :: factor
    logical :: is_prime

    do
        prime    = prime + 2
        is_prime = .true.
        do factor = 3,int(sqrt(real(prime)))
            if ( mod(prime, factor) == 0 ) then
                is_prime = .false.
                exit
            endif
        enddo
        if ( is_prime ) then
            exit
        endif
    enddo
end subroutine get_next_prime
end subroutine determine_steps

! init_grid_coordinates_uniform --
!     Initialise a grid coordinates object with uniform dimension
!
! Arguments:
!     coordinates   The coordinatesr object
!     max_index     Maximum value for the indices (holds for all dimensions)
!     range         Range for the actual coordinates (minimum and maximum per dimension;
!                   second dimension is the number of dimensions for the grid
!
! Result:
!     Filled in coordinates object
!
subroutine init_grid_coordinates_uniform( coordinates, max_index, range )
    class(grid_coordinates)                   :: coordinates
    integer, intent(in)                       :: max_index
    real(kind=wp), dimension(:,:), intent(in) :: range

    call coordinates%grid_traversing%init( size(range,2), max_index )
    call init_grid_coordinates_specific( coordinates, range )

    select type (coordinates)
        class is (grid_sampler)
            call init_grid_sampler_specific( coordinates )
    end select

end subroutine init_grid_coordinates_uniform

! init_grid_coordinates_diverse --
!     Initialise a grid coordinates object with diverse dimensions
!
! Arguments:
!     coordinates   The coordinates object
!     max_index     Array with the maximum values for the indexinates per dimension
!
! Result:
!     Filled in coordinates object
!
subroutine init_grid_coordinates_diverse( coordinates, max_index, range )
    class(grid_coordinates)                   :: coordinates
    integer, dimension(:), intent(in)         :: max_index
    real(kind=wp), dimension(:,:), intent(in) :: range

    call coordinates%grid_traversing%init( max_index )
    call init_grid_coordinates_specific( coordinates, range )

end subroutine init_grid_coordinates_diverse

! reset_grid_traversing --
!     Reset a grid traversing object
!
! Arguments:
!     traverse      The traversing object
!
! Result:
!     Traversing object that starts at the first point
!
subroutine reset_grid_traversing( traverse )
    class(grid_traversing) :: traverse

    if ( allocated(traverse%index) ) then
        traverse%index = 0
    endif
end subroutine reset_grid_traversing

! indices_grid_traversing --
!     Get the indices of the current grid point
!
! Arguments:
!     traverse      The traversing object
!
! Result:
!     Array with indices
!
function indices_grid_traversing( traverse )
    class(grid_traversing)                   :: traverse
    integer, dimension(size(traverse%index)) :: indices_grid_traversing

    indices_grid_traversing = traverse%index
end function indices_grid_traversing

! coords_grid_coordinates --
!     Get the coordinates of the current grid point
!
! Arguments:
!     coordinates   The coordinates object
!
! Result:
!     Array with coordinates based on the ranges
!
function coords_grid_coordinates( coordinates )
    class(grid_coordinates)                           :: coordinates
    real(kind=wp), dimension(size(coordinates%index)) :: coords_grid_coordinates

    coords_grid_coordinates = coordinates%range(1,:) + coordinates%range(2,:) * coordinates%index
end function coords_grid_coordinates

! next_point_grid_traversing --
!     Determine the indices of the next grid point
!
! Arguments:
!     traverse      The traversing object
!     indices       (Optional) indices of the new point
!
! Result:
!     Array with indexinates
!
subroutine next_point_grid_traversing( traverse, indices )
    class(grid_traversing)          :: traverse
    integer, dimension(:), optional :: indices

    integer                         :: i

    do i = 1,size(traverse%index)
        traverse%index(i) = traverse%index(i) + 1
        if ( traverse%index(i) >= traverse%max_index(i) ) then
            traverse%index(i) = 0
        else
            exit
        endif
    enddo

    if ( present(indices) ) then
        if ( size(indices) >= size(traverse%index) ) then
            indices(1:size(traverse%index)) = traverse%index
        else
            indices = -1
        endif
    endif
end subroutine next_point_grid_traversing

! next_point_grid_sampler --
!     Determine the indices of the next grid point by stepping thtough all dimensions
!
! Arguments:
!     traverse      The sampler object
!     indices       (Optional) indices of the new point
!
! Result:
!     Array with indices
!
subroutine next_point_grid_sampler( traverse, indices )
    class(grid_sampler)             :: traverse
    integer, dimension(:), optional :: indices

    integer                         :: i

    !
    ! Determine (new) steps?
    !
    if ( all( traverse%index == 0 ) ) then
        call determine_steps( traverse )
    endif

    traverse%index = mod( traverse%index + traverse%step, traverse%max_index )

    if ( present(indices) ) then
        if ( size(indices) >= size(traverse%index) ) then
            indices(1:size(traverse%index)) = traverse%index
        else
            indices = -1
        endif
    endif
end subroutine next_point_grid_sampler
end module traverse_grid
