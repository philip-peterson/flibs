! chk_index_long --
!     Check: can we use "long" integers as indices in DO-loops and arrays?
!
program chk_index_long
    implicit none

    integer, parameter                         :: long = selected_int_kind(range(1)+1)
    integer(kind=long), parameter              :: bignumber = huge(1_long)

    integer, dimension(bignumber-10:bignumber) :: value
    integer(kind=long)                         :: i
    integer(kind=long), dimension(1)           :: lower, upper

    write(*, '(a)' ) 'Using "long" integers as array indices:'

    value = 1
    write(*, '(a,i0)') 'Lower bound: ', lbound(value,kind=long)
    write(*, '(a,i0)') 'Upper bound: ', ubound(value,kind=long)

    lower = lbound(value,kind=long)
    upper = ubound(value,kind=long)
    value(lower(1)) = 1
    do i = lower(1)+1, upper(1)
        value(i) = value(i-1) + 1
    enddo
    write(*, '(a,11i5)') 'Values: ', value

end program chk_index_long
