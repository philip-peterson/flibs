! test_traverse_grid.f90 --
!     Test program for the traverse_grid module
!
program test_traverse_grid
    use traverse_grid
    implicit none

    type(grid_traversing)         :: traverse
    type(grid_sampler)            :: sampler
    integer, dimension(3)         :: range, indices
    real(kind=wp), dimension(2)   :: coord
    real(kind=wp), dimension(2,2) :: coord_range
    real(kind=wp)                 :: sum

    integer                       :: i, j

    range = (/ 2, 3, 2 /)

    call init_grid_traversing( traverse, range )

    indices = indices_grid_traversing( traverse )
    write( *, '(a,3i5)' ) 'start', indices

    do i = 1, product(range)
        call next_point( traverse, indices )
        write( *, '(4i5)' ) i, indices
    enddo
    write(*,*) 'Reset:'
    do i = 1, 4
        call next_point( traverse )
    enddo
    indices = indices_grid_traversing( traverse )
    write( *, '(a,3i5)' ) 'Now:   ', indices
    call reset_grid_traversing( traverse )
    indices = indices_grid_traversing( traverse )
    write( *, '(a,3i5)' ) 'Reset: ', indices

    !
    ! Second part: grid sampler
    !
    coord_range = reshape( (/ 0.0, 1.0,   0.0, 1.0  /), (/2,2/) )
    call sampler%init( 100, coord_range )
    do i = 1,10
        call sampler%next( indices )
        coord = sampler%coords()
        write(*, '(2i5,2f12.8)' ) indices(1:2), coord
    enddo

    !
    ! Now let us see how this can be used for integration a la Monte Carlo
    !
    ! Note:
    ! If the sampler's cycle is completed, the steps are automatically
    ! adjusted. This way it is guaranteed that different points are
    ! sampled.
    !
    write(*,'(a)') 'Exact value:'
    write(*,'(f12.7)') (exp(1.0_wp)-1.0_wp) * sin(6.0_wp*1.0_wp) / 6.0_wp
    write(*,'(a)') 'Systematic sampling:'
   ! call sampler%init( (/1000, 1001 /), coord_range )
    call sampler%init( 1000, coord_range )
    do j = 1,10
        sum = 0.0_wp
        do i = 1,1000
            call sampler%next( indices )
            coord = sampler%coords()
            !if ( i < 10 ) then
            !    write(*,*) j, indices(1:2), coord
            !endif

            sum = sum + f(coord(1),coord(2))
        enddo
        write(*,'(i5,f12.8)') j, sum/1000.0_wp
    enddo

    write(*,'(a)') 'Random sampling (Monte Carlo): 1000 samples'
    do j = 1,10
        sum = 0.0_wp
        do i = 1,1000
            call random_number( coord )
            sum = sum + f(coord(1),coord(2))
        enddo
        write(*,'(i5,f10.5)') j, sum/1000.0_wp
    enddo
    write(*,'(a)') 'Random sampling (Monte Carlo): 10000 samples'
    do j = 1,10
        sum = 0.0_wp
        do i = 1,10000
            call random_number( coord )
            sum = sum + f(coord(1),coord(2))
        enddo
        write(*,'(i5,f10.5)') j, sum/10000.0_wp
    enddo

contains

real(kind=wp) function f( x, y )
    real(kind=wp) :: x, y

    f = exp(x)*cos(6.0_wp*y)
end function f
end program test_traverse_grid
