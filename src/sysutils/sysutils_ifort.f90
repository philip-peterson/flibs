! sysutils_ifort.f90 --
!     Provide unified interfaces to some operating system functions
!
!     Version for Intel Fortran: hides the dependency on the IFPORT module
!
module sysutils
    use ifport, only: getpid
    implicit none

    private
    public :: get_process_id

contains

subroutine get_process_id( id )
    integer, intent(out) :: id

    id = getpid()
end subroutine get_process_id

end module sysutils
