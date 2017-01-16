! chk_pointer_init.f90 --
!     Check: does the compiler support pointer initialisation?
!
program chk_pointer_init
    implicit none

    real, dimension(100), target  :: array
    real, dimension(:), pointer   :: alternative => array

    call random_number( array )

    write( *, '(a)' )        'Pointer initialised:'
    write( *, '(a,l)'      ) '    Associated: ', associated(alternative,array)
    write( *, '(a,5f10.4)' ) '    Values array: ', array(1:5)
    write( *, '(a,5f10.4)' ) '    Alternative:  ', alternative(1:5)

    if ( .not. associated(alternative) ) then
        write( *, '(/,a)' )  '    Note: the syntax is accepted, but it does not work properly'
    endif

end program chk_pointer_init
