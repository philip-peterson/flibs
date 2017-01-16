! readchar.f90 --
!     Read  a file character by character
!
module read_characters
    implicit none

    private

    type, public :: character_stream
        integer :: lun      = -1       ! Logical unit number to read
    contains
        procedure :: open  => open_file
        procedure :: get   => get_char   ! Get a single character
        procedure :: close => close_file
    end type character_stream

    integer, parameter, public :: single_character = 0
    integer, parameter, public :: end_of_line      = 1
    integer, parameter, public :: end_of_file      = 2
    integer, parameter, public :: read_error       = 3

contains

! open_file --
!     Open the file to be read with the correct attributes
!
! Arguments:
!     this       The character stream object
!     filename   Name of the file to open
!     success    Successful or not
!
subroutine open_file( this, filename, success )
    class(character_stream) :: this
    character(len=*)        :: filename
    logical, intent(out)    :: success

    integer                 :: i
    integer                 :: ierr
    logical                 :: opened

    success  = .false.
    this%lun = -1
    do i = 10,99
        inquire( unit = i, opened = opened )
        if ( .not. opened ) then
            this%lun = i
            exit
        endif
    enddo

    if ( this%lun > 0 ) then
        open( unit = this%lun, file = filename, status = 'old', iostat = ierr )

        if ( ierr /= 0 ) then
            this%lun = -1
            return
        else
            success       = .true.
        endif
    endif
end subroutine open_file

! close_file --
!     Close the file
!
! Arguments:
!     this       The character stream object

subroutine close_file( this )
    class(character_stream) :: this

    if ( this%lun > 0 ) then
        close( this%lun )
        this%lun = -1
    endif

end subroutine close_file

! get_char --
!     Get a single character
!
! Arguments:
!     this       The character stream object
!     char       Character that was read
!     type       Type: a character, end-of-line or end-of-file

subroutine get_char( this, char, type )
    class(character_stream)       :: this
    character(len=1), intent(out) :: char
    integer, intent(out)          :: type

    integer                       :: eor, ierr

    type = single_character

    read( this%lun, '(a)', iostat = ierr, advance = 'no' ) char

    if ( ierr > 0 ) then
        type = read_error
    elseif ( is_iostat_end(ierr) ) then
        type = end_of_file
    elseif ( is_iostat_eor(ierr) ) then
        type = end_of_line
    endif

end subroutine get_char

end module read_characters
