! chk_alloc_long --
!     Check: can we use "long" integers to allocate arrays?
!
program chk_alloc_long
    implicit none

    integer, parameter                 :: long = selected_int_kind(range(1)+1)
    integer(kind=long), parameter      :: bignumber = huge(1_long)

    integer                            :: ierr
    integer, dimension(:), allocatable :: value
    integer(kind=long)                 :: i
    integer(kind=long), dimension(1)   :: lower, upper

    write(*, '(a)' ) 'Using "long" integers to allocated the array:'

    allocate( value(bignumber-10:bignumber), stat = ierr )

    if ( ierr /= 0 ) then
        write( *, '(a,i0)' ) 'Allocation failed - error = ', ierr
    else
        write(*, '(a,i0)') 'Lower bound: ', lbound(value,kind=long)
        write(*, '(a,i0)') 'Upper bound: ', ubound(value,kind=long)
        write(*, '(a,i0)') 'Size:        ', size(value,kind=long)
        write(*, '(a,i0)') 'Shape:       ', shape(value,kind=long)
    endif

end program chk_alloc_long
