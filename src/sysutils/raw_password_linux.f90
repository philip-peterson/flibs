! raw_password_linux.f90 --
!     Module for reading a password from the keyboard - Linux and Cygwin version
!
!     Linux and Cygwin under Windows provide the command stty to turn on/off echoing
!     of characters typed on the keyboard. (An alternative is to use the ncurses
!     library to suppress the echoing)
!
!     Complication: not all compilers provide the execute_command_line intrinsic
!     This can be emulalted however
!
module raw_password_facility
    implicit none

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
! This does not work properly yet!!!!
!
subroutine get_raw_password( password, length )
    character(len=*), intent(out) :: password
    integer, intent(out)          :: length

    character(len=1)              :: ch

    length   = 0
    password = '?'

    call execute_command_line( "stty -echo" )
    read( *, '(2a)', advance = 'no', eor = 100, size = length ) password, ch

    !
    ! We have enough characters, but get rid of any remaining characters
    !
    length = len(password)

    read( *, '(a)' )
    call execute_command_line( "stty echo" )
    return

    !
    ! We reached the end of the record (length already set)
    !
100 continue
    call execute_command_line( "stty echo" )

end subroutine get_raw_password
end module raw_password_facility
