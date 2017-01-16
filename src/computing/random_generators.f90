! random_generators.f90 --
!     Collection of pseudo-random number generators
!
!     Reference:
!     Good Practice in (Pseudo) Random Number
!     Generation for Bioinformatics Applications
!     by David Jones, UCL Bioinformatics Group
!     http://www.cs.ucl.ac.uk/staff/d.jones/GoodPracticeRNG.pdf
!
!     TODO:
!     Use /dev/urandom if possible to initialise the seed
!     Export the seed via prng_get_seed for later use
!
module random_generators
    implicit none

    integer, parameter :: xdefault = 123456789
    integer, parameter :: ydefault = 234567891
    integer, parameter :: zdefault = 345678912
    integer, parameter :: wdefault = 456789123

    type prng_jkiss32
        logical :: init = .true.
        integer :: x, y, z, w, c
    end type prng_jkiss32

    private :: prng_jkiss32_random_integer_base
    integer, parameter, private :: dp = kind(1.0d0)

    !
    ! Subroutines
    !
    interface prng_random
        module procedure prng_jkiss32_random_real
        module procedure prng_jkiss32_random_double
!        module procedure prng_jkiss32_random_real1d
        module procedure prng_jkiss32_random_integer
    end interface

    !
    ! Functions
    !
!    interface prng_rnd
!        module procedure prng_jkiss32_rnd_real
!        module procedure prng_jkiss32_rnd_integer
!    end interface

contains

! prng_jkiss32_init --
!     Imitialise the PRNG object
!
! Arguments:
!     prng          Object to be initialised
!     xinit, ...    Four values for the initialisation (all optional)
!
subroutine prng_jkiss32_init( prng, xinit, yinit, zinit, winit )
    type(prng_jkiss32), intent(inout) :: prng
    integer, intent(in), optional     :: xinit
    integer, intent(in), optional     :: yinit
    integer, intent(in), optional     :: zinit
    integer, intent(in), optional     :: winit

    integer                           :: i
    integer                           :: xset
    integer                           :: yset
    integer                           :: zset
    integer                           :: wset
    real                              :: time
    real(kind=dp)                     :: r        ! Double precision to ensure
                                                  ! all bits are used
    integer                           :: rint

    if ( present(xinit) ) then
        xset = xinit
    else
        call system_clock( count = xset )
    endif

    if ( present(yinit) ) then
        yset = yinit
    else
        call cpu_time( time )
        yset = nint( mod( time, 1.0 ) * huge(1) )
    endif

    if ( present(zinit) ) then
        zset = zinit
    else
        call random_number( r )
        zset = nint( r * huge(1) )
    endif

    if ( present(winit) ) then
        wset = winit
    else
        call random_number( r )
        wset = nint( r * huge(1) )
    endif

    prng%init = .false.
    prng%x    = xset
    prng%y    = yset
    prng%z    = zset
    prng%w    = wset

    !
    ! For good measure
    !
    do i = 1,100
        call prng_jkiss32_random_integer_base( prng, rint )
    enddo
end subroutine prng_jkiss32_init

! prng_random_integer_base --
!     Return a random integer between -huge(1) and huge(1)
!
! Arguments:
!     prng          PRNG object
!     rint          Random integer
!
! Note:
!     This is the routine that does the actual work -
!     to avoid recursive calls
!
subroutine prng_jkiss32_random_integer_base( prng, rint )
    type(prng_jkiss32), intent(inout) :: prng
    integer, intent(out)              :: rint

    integer                           :: t

    prng%y = ieor( prng%y, ishft( prng%y,  5 ) )
    prng%y = ieor( prng%y, ishft( prng%y, -7 ) )
    prng%y = ieor( prng%y, ishft( prng%y, 22 ) )

    t      = prng%z +prng%w + prng%c
    prng%z = prng%w
    prng%c = merge( 1, 0, t < 0 )
    prng%w = iand( t, 2147483647 )

    prng%x = prng%x + 1411392427

    rint   = prng%x + prng%y + prng%w

end subroutine prng_jkiss32_random_integer_base

! prng_random_integer --
!     Return a random integer between -huge(1) and huge(1)
!
! Arguments:
!     prng          PRNG object
!     rint          Random integer
!
! Note:
!     If the PRNG object has not been initialised
!     yet, it is done here.
!
subroutine prng_jkiss32_random_integer( prng, rint )
    type(prng_jkiss32), intent(inout) :: prng
    integer, intent(out)              :: rint

    if ( prng%init ) then
        call prng_jkiss32_init( prng, xdefault, ydefault, zdefault, wdefault )
    endif

    call prng_jkiss32_random_integer_base( prng, rint )

end subroutine prng_jkiss32_random_integer

! prng_random_real --
!     Return a random real between -huge(1) and huge(1)
!
! Arguments:
!     prng          PRNG object
!     rreal         Random single-precision real (between 0 and 1)
!
! Note:
!     If the PRNG object has not been initialised
!     yet, it is done here.
!
subroutine prng_jkiss32_random_real( prng, rreal )
    type(prng_jkiss32), intent(inout) :: prng
    real, intent(out)                 :: rreal

    integer                           :: rint

    if ( prng%init ) then
        call prng_jkiss32_init( prng, xdefault, ydefault, zdefault, wdefault )
    endif

    call prng_jkiss32_random_integer_base( prng, rint )

    rreal = rint / 4294967296.0_dp + 0.5_dp

end subroutine prng_jkiss32_random_real

! prng_random_double --
!     Return a random double precision real between -huge(1) and huge(1)
!
! Arguments:
!     prng          PRNG object
!     rdble         Random double-precision real (between 0 and 1)
!
! Note:
!     If the PRNG object has not been initialised
!     yet, it is done here.
!
subroutine prng_jkiss32_random_real( prng, rdble )
    type(prng_jkiss32), intent(inout) :: prng
    real(kind=dp), intent(out)        :: rdble

    integer                           :: rint1, rint2

    if ( prng%init ) then
        call prng_jkiss32_init( prng, xdefault, ydefault, zdefault, wdefault )
    endif

    call prng_jkiss32_random_integer_base( prng, rint1 )
    call prng_jkiss32_random_integer_base( prng, rint2 )

    rint1 = rint1 / 2**6
    rint2 = rint2 / 2**5

    rdble = (rint1 * 134217728.0_dp + rint2) / 9007199254740992.0_dp

end subroutine prng_jkiss32_random_real

end module random_generators

program test_prng
    use random_generators

    implicit none

    type(prng_jkiss32) :: prng
    integer            :: i
    integer            :: j
    integer            :: x
    real               :: r

     do i = 1,100
         call prng_random( prng, j )
         call prng_random( prng, r )
         write(*,*) i, j, r
     enddo
end program test_prng
