! chk_alloc_message --
!     Check: does the compiler support the error message keyword with ALLOCATE?
!
program chk_alloc_message
    implicit none

    integer, dimension(:), allocatable :: array
    integer                            :: ierr
    character(len=200)                 :: msg

    allocate( array(10) )
    allocate( array(20), stat = ierr, errmsg = msg )

    write( *, '(a)'    ) 'Deliberate failure with allocating an array:'
    write( *, '(a,i0)' ) '    Error code: ', ierr
    write( *, '(a,a)'  ) '    Message:    ', trim(msg)

end program chk_alloc_message
