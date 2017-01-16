! enum_set.f90 --
!     Yet another experiment with sets of integers:
!     use F2003 features to combine functions to
!     filter out unwanted integers and keep those
!     that have particular properties.
!
!     Note:
!     It is assumed in the combine function that
!     the properties lead to an ascending function
!
module enumeration_procedure

    type enum_func
        type(enum_func), pointer                 :: f    => null()
        type(enum_func), pointer                 :: g    => null()
        procedure(enumeration), pointer, pass(t) :: enum => null()
    end type enum_func

    abstract interface
        integer function enumeration( t, idx )
            import :: enum_func
            type(enum_func), intent(in) :: t
            integer, intent(in)         :: idx
        end function enumeration
    end interface

end module enumeration_procedure

module enumerate_sets
    use enumeration_procedure

    implicit none


!    procedure(enumeration) :: combine

contains

! func, combination --
!     Convenience functions to initialise the derived types
!
! Note:
!     Also used to get (hopefully) around a compiler limitation
!
function func( f )
    procedure(enumeration)         :: f
    type(enum_func)                :: func

    func%enum => f

end function func

function combination( f, g, c )
    type(enum_func), target        :: f
    type(enum_func), target        :: g
    procedure(enumeration)         :: c
    type(enum_func)                :: combination

    combination%f    => f
    combination%g    => g
    combination%enum => c

end function combination

! combine --
!     Combine the two integer sets in such a way that
!     only the integers that belong to both are returned

integer function combine( t, idx )
    type(enum_func), intent(in) :: t
    integer, intent(in)         :: idx

    integer                     :: count
    integer                     :: i
    integer                     :: j
    integer                     :: fi
    integer                     :: gj

    count = 0
    i     = 0
    j     = 0

    fi = t%f%enum(i)
    gj = t%g%enum(j)

    do while ( count < idx )

        if ( fi == gj ) then
            count = count + 1
            i     = i     + 1
        else if ( fi < gj ) then
            i  = i + 1
        else
            j  = j + 1
        endif
        fi = t%f%enum(i)
        gj = t%g%enum(j)

     enddo

     combine = i - 1

end function combine

end module


! pell_equation
!     Module to implement the Pell equation "y**2 = 2 * x**2 + 1"
!
module pell_equation
    use enumerate_sets

!    procedure(enumeration) :: y_square
!    procedure(enumeration) :: x_pell

contains

integer function y_square( t, idx )
    type(enum_func), intent(in) :: t    ! Not used
    integer, intent(in)         :: idx

    y_square = idx ** 2

end function y_square

integer function x_pell( t, idx )
    type(enum_func), intent(in) :: t    ! Not used
    integer, intent(in)         :: idx

    x_pell = 2 * idx ** 2 + 1

end function x_pell

end module pell_equation


program enum_set
    use pell_equation

    implicit none

    type(enum_func), target :: xright
    type(enum_func), target :: yleft
    type(enum_func) :: xpell
    type(enum_func) :: ypell

!   procedure(enumeration) :: combine
!   procedure(enumeration) :: y_square
!   procedure(enumeration) :: x_pell

    integer :: i
    integer :: x
    integer :: y

    !
    ! We want both x and y, so we have to compute them twice
    ! (We could of course adapt the interface to return
    ! the two integers ...)
    !
!    xright%enum => x_pell
!    yleft%enum  => y_square
!
!    xpell%f    => xright
!    xpell%g    => yleft
!    xpell%enum => combine
!
!    ypell%f    => yleft
!    ypell%g    => xright
!    ypell%enum => combine

     xright = func( x_pell )
     yleft  = func( y_square )

     xpell  = combination( xright, yleft, combine )
     ypell  = combination( yleft, xright, combine )

    do i = 1,5
        x = xpell%enum(i)
        y = ypell%enum(i)
       write(*,*) '>>', i, x, y
    enddo

    do i = 1,100000
        x = 2 * i ** 2 + 1
        y = int( sqrt(float(x)) ) ** 2
        if ( x == y ) then
            write(*,*) 'Direct:', i, int(sqrt(float(y)))
        endif
    enddo
end program enum_set
