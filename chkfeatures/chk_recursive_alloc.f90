! chk_recursive_alloc --
!     Check: does the compiler support types with a recursive allocatable component?
!
program chk_recursive_alloc
    implicit none

    type recursive
        integer :: x
        type(recursive), allocatable :: y
    end type recursive

    type(recursive), target  :: list
    type(recursive), pointer :: plist
    integer                  :: i

    write( *, '(a)' ) 'Setting up a simple list of integers'
    write( *, '(a)' ) '    Values stored:'

    plist => list
    do i = 1,10
       plist%x = i
       allocate( plist%y )
       plist => plist%y
    enddo

    i = 0
    plist => list
    do while ( allocated(plist%y) )
        write( *, '(8x,i0,l4)' ) plist%x, allocated(plist%y)
        plist => plist%y
        i = i + 1
    enddo

end program chk_recursive_alloc
