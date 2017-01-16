! test_keyvars.f90 --
!     Simple test program for the keyvars module
!
! Usage:
!     test_keyvars           -- will read "test_keyvars.inp"
!     test_keyvars -help     -- print usage information
!
program test_keyvars
    use keyvars

    implicit none

    integer :: x, x2, y2, x3
    real    :: y
    character(len=20) :: string

    x = -1
    y = -1.0

    call get_values( 'test_keyvars.inp', [keyvar("int",  x, "Integer value"), &
                                          keyvar("real", y, "Real value"), &
                                          keyvar("char", string, "Some text"), &
                                          keyvar("Section", "int2", x2, "Alternative parameter for x"), &
                                          keyvar("Section3", "int3", x3, "Yet another one"), &
                                          keyvar("Section", "inty2", y2, "Alternative parameter for y")] )

    write(*,*) 'x = ', x
    write(*,*) 'y = ', y
    write(*,*) 'string = ', string
    write(*,*) 'x2 = ', x2
end program test_keyvars
