! chk_allocate_source.f90
!     Check: does the compiler support the SOURCE argument to the allocate statement?
!
program chk_allocate_source
    implicit none

    integer, dimension(10)             :: src
    integer, dimension(:), allocatable :: target

    src = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /)

    allocate( target, source = src )

    write( *, '(a,i0)' ) 'Size of the allocated array: ', size(target)
    write( *, '(a)' )    'Contents: '
    write( *, * )        target

    if ( any( src /= target ) ) then
        write( *, '(/,a)' ) 'NOTE: this is not what was expected (numbers 1 ... 10)'
    else
        write( *, '(/,a)' ) 'Contents as expected (copy of source array)'
    endif
end program chk_allocate_source
