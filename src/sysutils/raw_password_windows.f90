! raw_password_windows.f90 --
!     Module for reading a password from the keyboard - Windows version
!
!     Windows provides the function getch() to read a single character
!     unseen from the keyboard
!
module raw_password_facility
    use iso_c_binding
    implicit none

    interface
        integer(kind=c_int) function getch() bind(C, name="getch" )
            use, intrinsic :: iso_c_binding
        end function getch
    end interface

contains

! get_raw_password --
!     Read a string from the keyboard without echoing it
!
! Arguments:
!     password          String to be filled with the password
!     length            Effective length (i.e. password(1:length) is the string that was read)
!
! Note:
!     Supports backspace and reading superfluous characters up to CR/LF
!
subroutine get_raw_password( password, length )
   character(len=*), intent(out) :: password
   integer, intent(out)          :: length

   integer           :: i, ch

   length   = 0
   password = '?'

   i = 1
   do
       ch = getch()
       if ( ch /= 13 .and. ch /= 10 ) then
           if ( ch == 8 ) then
               i = max( 1, i - 1 )
           elseif ( i <= len(password) ) then
               password(i:i) = achar(ch)
               i = i + 1
           endif
       else
           exit
       endif
   enddo

   length = i - 1

end subroutine get_raw_password
end module raw_password_facility
