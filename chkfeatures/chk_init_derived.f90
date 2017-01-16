! chk_init_derived.f90
!     Check: does the compiler initialise local variables of a derived type?
!
!     Note:
!     The behaviour could be influenced by the compiler's using static
!     memory for local variables. Which is why it is interesting to test
!     the behaviour.
!
program chk_init_deriv
    implicit none

    type mytype
        integer :: value = 1
        integer :: value2
    end type mytype

    call sub( 1 )
    call sub2
    call sub( 2 )

contains
subroutine sub( mode )
    integer, intent(in) :: mode

    type(mytype) :: local_var

    if ( mode == 1 ) then
        local_var%value  = 2
        local_var%value2 = 2
    else
        write( *, '(a,i0)' ) 'Value of "local_var%value: ', local_var%value
        if ( local_var%value /= 1 ) then
            write( *, '(a,i0)' ) '    Expected value:        ', 1
        else
            write( *, '(a)' )    '    Value is as expected'
        endif

        write( *, '(a,i0)' ) 'Value of "local_var%value2: ', local_var%value2
        if ( local_var%value == 2 ) then
            write( *, '(a)' )    '    It looks as if static memory is used for local variables'
        else
            write( *, '(a)' )    '    Probably dynamic memory (stack?) is used for local variables'
        endif
    endif
end subroutine sub

! Intends to overwrite (stack) memory
subroutine sub2
    type(mytype) :: local_var
    type(mytype) :: other_var

    local_var = mytype(11111,22222)
    other_var = mytype(33333,44444)
end subroutine sub2

end program chk_init_deriv
