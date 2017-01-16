! chk_associate --
!     Check: does the compiler support the ASSOCIATE construct?
!
program chk_associate
    implicit none

    type mytype
        real :: x, y
    endtype mytype

    type(mytype) :: coords
    real         :: xcoord

    coords = mytype( 0.0, 0.0 )

    xcoord = 1.0
    associate( x => coords%x )
        x = xcoord
    end associate

    write( *, '(a,f10.4)' ) 'X-coordinate set to: ', coords%x
    if ( xcoord == coords%x ) then
        write( *, '(a)' ) '    (value is as expected)'
    else
        write( *, '(a)' ) '    Unexpected value - please check!'
    endif
end program chk_associate







