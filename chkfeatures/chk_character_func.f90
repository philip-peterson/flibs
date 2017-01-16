! chk_character_func.f90
!     Check: does the compiler support functions returning strings with allocatable length?
!
program chk_character_func
    implicit none

    write( *, '(2a)' ) 'Allocated string: ', alloc_string(10)
contains
function alloc_string( length )
    integer                       :: length
    character(len=:), allocatable :: alloc_string

    integer                       :: i

    allocate( character(len=length) :: alloc_string )

    do i = 1,length
        alloc_string(i:i) = 'A'
    enddo
end function alloc_string
end program chk_character_func
