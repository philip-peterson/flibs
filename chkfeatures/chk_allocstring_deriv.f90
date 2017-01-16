! chk_allocstring_deriv --
!     Check: does the compiler support derived types with allocatable-length character strings?
!
program chk_allocstring_deriv
    implicit none
    type address_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: street
    end type address_t

    type(address_t) :: address

    allocate( character(len=10) :: address%name )
    allocate( character(len=20) :: address%street )

    write( *, '(a,i0)' ) 'Allocated length name:   ', len(address%name)
    write( *, '(a,i0)' ) 'Allocated length street: ', len(address%street)

    address = address_t( 'My Name', 'My Street' )

    write( *, '(a,i0)' ) 'Length name after assignment:   ', len(address%name)
    write( *, '(a,i0)' ) 'Length street after assignment: ', len(address%street)
    if ( len(address%name) /= 10 .or. len(address%street) /= 20 ) then
        write( *, '(a)' ) '    Be aware that the length may be influenced by assignment!'
        write( *, '(a)' ) '    (automatic reallocation is taking place)'
    endif
end program chk_allocstring_deriv
