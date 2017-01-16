! sysutils_gfortran.f90 --
!     Provide unified interfaces to some operating system functions
!
!     Version for gfortran: some of the used routines are intrinsic routines
!
module sysutils
    implicit none

    private
    public    :: get_process_id
    intrinsic :: getpid

contains

subroutine get_process_id( id )
    integer, intent(out) :: id

    id = getpid()
end subroutine get_process_id

end module sysutils
