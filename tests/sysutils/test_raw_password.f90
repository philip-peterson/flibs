! test_raw_password.f90 --
!     Interactive program to test the raw_password module
!
program test_raw_password
    use raw_password_facility

    character(len=10) :: password
    integer           :: length

    write(*,*) 'Enter short password (less than 10 characters)'
    call get_raw_password( password, length )

    write(*,*) 'Length of the string: ', length
    write(*,*) 'Entered string: >', password(1:length), '<'


    write(*,*) 'Enter short password using backspaces (for Windows)'
    call get_raw_password( password, length )

    write(*,*) 'Length of the string: ', length
    write(*,*) 'Entered string: >', password(1:length), '<'


    write(*,*) 'Enter long password (more than 10 characters)'
    call get_raw_password( password, length )

    write(*,*) 'Length of the string: ', length
    write(*,*) 'Entered string: >', password(1:length), '<'

end program test_raw_password


