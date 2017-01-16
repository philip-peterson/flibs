! chk_allocstring --
!     Check: does the compiler support character strings with allocatable length?
!
program chk_allocstring
    character(len=:), allocatable :: string

    allocate( character(len=10) :: string )

    write( *, '(a,i0)' ) 'Allocated length: ', len(string)

    string = '1'

    write( *, '(a,i0)' ) 'Length after assignment: ', len(string)
    if ( len(string) /= 10 ) then
        write( *, '(a)' ) '    Be aware that the length may be influenced by assignment!'
        write( *, '(a)' ) '    (automatic reallocation is taking place)'
    endif
end program chk_allocstring
