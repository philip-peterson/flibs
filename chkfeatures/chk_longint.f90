! chk_longint.f90 --
!     Check: does the compiler support integers longer than 32 bits?
!
!     Note: if not, then the compilation will fail (kind < 0)
!
program chk_longint
    implicit none

    integer, parameter :: ordinary = 1
    integer, parameter :: long = selected_int_kind(range(ordinary)+1)
    integer(kind=long) :: large_value = 1

    write(*,'(a,i0)') 'HUGE for "long" integers: ', huge(large_value)
end program chk_longint
