! chk_auto_realloc.f90 --
!     Check: does the compiler support automatic reallocation?
!
program chk_auto_realloc
    implicit none

    integer                            :: i
    integer, dimension(:), allocatable :: array

    allocate( array(1) )

    array = (/ (i ,i=1,10) /)
    write( *, '(a,i0)' ) 'Allocated length: ', size(array)

    if ( size(array) == 10 ) then
        write( *, '(a)' ) '    Automatic reallocation is supported'
    else
        write( *, '(a)' )      '    Automatic reallocation is NOT supported'
        write( *, '(a,i0,a)' ) '    Size is ', size(array), ' instead of 10'
    endif
end program chk_auto_realloc
