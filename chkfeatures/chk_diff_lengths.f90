! chk_diff_lengths.f90 --
!     Check: does the compiler support array constructors with strings of different lengths?
!
program chk_diff_lengths
    implicit none

    character(len=10), dimension(3) :: strings

    strings = (/ 'A', 'BC', 'DEF' /)

    write(*,'(a)') 'You can use strings of different lengths in an array constructor'
end program chk_diff_lengths
