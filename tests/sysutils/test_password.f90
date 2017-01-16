! test_password.f90 --
!     Interactive program to test the password_facilities module
!
program test_password
    use password_facilities

    character(len=10) :: password
    integer           :: length

    call show_prompt( "Password:" )
    call get_password( password, length )

    write(*,*) 'Length of the string: ', length
    write(*,*) 'Entered string: >', password(1:length), '<'

    call show_prompt( "Password (will be truncated to 5 characters):" )
    call get_password( password, 5, length )

    write(*,*) 'Length of the string: ', length
    write(*,*) 'Entered string: >', password(1:length), '<'

    call show_prompt( "Password:" )
    call get_password( password, 5, length, identity_encryption )

    write(*,*) 'Length of the string: ', length
    write(*,*) 'Entered string: >', password(1:length), '<'

end program test_password


