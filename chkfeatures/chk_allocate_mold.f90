! chk_allocate_mold.f90
!     Check: does the compiler support the MOLD argument to the allocate statement?
!
program chk_allocate_mold
    implicit none

    integer, dimension(10,7)           :: src
    integer, dimension(:,:), allocatable :: target

    allocate( target, mold = src )

    write( *, '(a,i0)' )   'Size of the allocated array: ', size(target)
    write( *, '(a,10i5)' ) 'Shape:                       ', shape(target)

    if ( any( shape(src) /= shape(target) ) ) then
        write( *, '(/,a)' ) 'NOTE: this is not what was expected (expected shape: 10, 7)'
    else
        write( *, '(/,a)' ) 'Shape as expected (same as source array)'
    endif
end program chk_allocate_mold
