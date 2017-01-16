! password.f90 --
!     Module providing facilities for reading a password from the keyboard
!
!     The module assumes that in general you want to get an encrypted password.
!     It therefore offers a very basic encryption routine and an interface to
!     specify a more sophisticated routine of your own choice.
!
!     As such routines may fill a buffer that is longer than the maximum length
!     of a password, the buffer you pass may have a different length than the
!     maximum length of the password.
!
module password_facilities
    use raw_password_facility
    implicit none

    interface get_password
        module procedure get_password_simple_max_length
        module procedure get_password_default
        module procedure get_password_with_encryption
    end interface

contains

! show_prompt --
!     Convenience routine to show a prompt and keep the cursor on the same line
!
! Arguments:
!     prompt     String to be shown as prompt (will be followed by a space)
!
! Note:
!     This does not always work as expected! For instance under the Intel Fortran
!     compiler on Windows the prompt is not shown correctly.
!
subroutine show_prompt( prompt )
    use, intrinsic :: iso_fortran_env

    character(len=*), intent(in) :: prompt

    write( *, '(a,1x)', advance = 'no' ) trim(prompt)
    flush( output_unit )
end subroutine show_prompt

! get_password_simple_max_length --
!     Get a password using default encryption and the maximum length equal to that of the string
!
! Arguments:
!     password          String that will contain the encrypted password
!     actual_length     Actual length of the password (as encrypted)
!
subroutine get_password_simple_max_length( password, actual_length )
    character(len=*), intent(out) :: password
    integer, intent(out)          :: actual_length

    integer                       :: max_length

    max_length = len(password)
    call get_password_with_encryption( password, max_length, actual_length, default_encryption )

end subroutine get_password_simple_max_length

! get_password_default --
!     Get a password using default encryption
!
! Arguments:
!     password          String that will contain the encrypted password
!     max_length        Maximum length of the password the user can enter
!     actual_length     Actual length of the password (as encrypted)
!
subroutine get_password_default( password, max_length, actual_length )
    character(len=*), intent(out) :: password
    integer, intent(in)           :: max_length
    integer, intent(out)          :: actual_length

    call get_password_with_encryption( password, max_length, actual_length, default_encryption )
end subroutine get_password_default

! get_password_with_encryption --
!     Get a password using the supplied encryption routine
!
! Arguments:
!     password          String that will contain the encrypted password
!     max_length        Maximum length of the password the user can enter
!     actual_length     Actual length of the password (as encrypted)
!     encryption        The encryption routine
!
subroutine get_password_with_encryption( password, max_length, actual_length, encryption )
    character(len=*), intent(out) :: password
    integer, intent(in)           :: max_length
    integer, intent(out)          :: actual_length

    interface
        subroutine encryption( raw_password, password, actual_length )
            character(len=*), intent(in)  :: raw_password
            character(len=*), intent(out) :: password
            integer, intent(out)          :: actual_length
        end subroutine
    end interface

    character(len=max_length)     :: raw_password

    call get_raw_password( raw_password, actual_length )
    call encryption( raw_password(1:actual_length), password, actual_length )

end subroutine get_password_with_encryption

! identity_encryption --
!     Identity encryption routine
!
! Arguments:
!     raw_password      String that contains the raw password
!     password          String that will contain the encrypted password
!     actual_length     Actual length of the encrypted password
!
subroutine identity_encryption( raw_password, password, actual_length )
    character(len=*), intent(in)  :: raw_password
    character(len=*), intent(out) :: password
    integer, intent(out)          :: actual_length

    actual_length = len(raw_password)
    password      = raw_password

end subroutine identity_encryption

! default_encryption --
!     Default encryption routine - serves as an example
!
! Arguments:
!     raw_password      String that contains the raw password
!     password          String that will contain the encrypted password
!     actual_length     Actual length of the encrypted password
!
subroutine default_encryption( raw_password, password, actual_length )
    character(len=*), intent(in)  :: raw_password
    character(len=*), intent(out) :: password
    integer, intent(out)          :: actual_length

    integer                       :: i
    integer                       :: ch

    actual_length = len(raw_password)
    password      = ' '

    !
    ! Use a rot13-like transformation
    !
    do i = 1,len(raw_password)
        ch = iachar(raw_password(i:i))

        if ( ch > 27 ) then
            if ( ch < 128 ) then
                ch = 155 - ch
            else
                ch = 383 - ch
            endif
        endif

        password(i:i) = achar(ch)
    enddo
end subroutine default_encryption

end module password_facilities
