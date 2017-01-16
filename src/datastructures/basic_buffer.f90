! basic_buffer.f90 --
!     Implementation of a buffer class that can store data of
!     one of the basic types (integer, real, ...)
!
!     The implementation uses a circular buffer to make updates
!     as fast as possible
!
!     Note:
!     A value is added to the _end_ of the buffer. If the buffer is
!     full, the first value is discarded. This means that the
!     last value added to a full buffer _always_ has the index "capacity".
!
!     Note:
!     Work in progress
!

!
! Modules for buffers containing basic types
!
module buffers_integer
    implicit none

    private

    type buffer_integer
        integer :: number_used
        integer :: current
        integer, dimension(:), allocatable :: data
    contains
        procedure :: create
        procedure :: add
        procedure :: value
        procedure :: min
        procedure :: max
        procedure :: mean
        procedure :: sum     => sum_of_data
    end type buffer_integer

    public :: buffer_integer

contains

! create --
!     Create a new buffer
!
! Arguments:
!     this       Buffer to be initialised
!     capacity   Maximum of data to be stored
!
subroutine create( this, capacity )
    class(buffer_integer), intent(inout) :: this
    integer, intent(in)                  :: capacity

    if ( allocated(this%data) ) then
        deallocate( this%data )
    endif

    allocate( this%data(capacity) )

    this%number_used = 0
    this%current     = 0
    this%data        = 0
end subroutine create

! add --
!     Add a number to the buffer
!
! Arguments:
!     this       Initialised buffer
!     value      Value to be stored
!
subroutine add( this, value )
    class(buffer_integer), intent(inout) :: this
    integer, intent(in)                  :: value

    !
    ! Silently fail, if not allocated
    !
    if ( .not. allocated(this%data) ) then
        return
    endif

    if ( this%number_used < size(this%data) ) then
        this%number_used = this%number_used + 1
    endif

    this%current = this%current + 1
    if ( this%current > size(this%data) ) then
        this%current = 1
    endif

    this%data(this%current) = value

end subroutine add

! value --
!     Get a value by index from the buffer
!
! Arguments:
!     this       Initialised buffer
!     idx        Index of the value to be retrieved
!
! Note:
!     If the index falls outside the valid range, then
!     -huge() is returned
!
integer function value( this, idx )
    class(buffer_integer), intent(in) :: this
    integer, intent(in)               :: idx

    integer                          :: idxv

    if ( .not. allocated(this%data) ) then
        value = -huge(this%data(1))
        return
    endif
    if ( idx < 1 .or. idx > this%number_used ) then
        value = -huge(this%data(1))
        return
    endif

    if ( this%number_used < size(this%data) ) then
        idxv = idx
    else
        idxv = this%current + idx - this%number_used
        if ( idxv < 1 ) then
            idxv = idxv + this%number_used
        endif
    endif

    value = this%data(idxv)

end function value

! min, max, sum, mean --
!     Basic operations on the contents of a buffer
!
! Arguments:
!     this       Initialised buffer
!
integer function min( this )
    class(buffer_integer), intent(in) :: this

    min = minval( this%data(1:this%number_used) )
end function min

integer function max( this )
    class(buffer_integer), intent(in) :: this

    max = maxval( this%data(1:this%number_used) )
end function max

integer function sum_of_data( this )
    class(buffer_integer), intent(in) :: this

    sum_of_data = sum( this%data(1:this%number_used) )
end function sum_of_data

real function mean( this )
    class(buffer_integer), intent(in) :: this

    if ( this%number_used > 0 ) then
        mean = sum( this%data(1:this%number_used) ) / real(this%number_used)
    else
        mean = -huge(mean)
    endif
end function mean

end module buffers_integer

! buffers_basic_types
!     Overall module to make buffers of various types available
!
module buffers_generic

    use buffers_integer
    use buffers_derived_type

end module buffers_generic

! test_buffers --
!     Simple test program for buffer objects
!
program test_buffers
    use buffers_generic

    implicit none

    type(buffer_integer) :: buffer
    integer              :: i
    integer              :: m
    integer              :: sum

    !
    ! Create a buffer and fill it more than completely
    !
    call buffer%create( 10 )

    do i = 1,20
        call buffer%add( i )
        m = min( i, 10 )
        write(*,*) i, buffer%value(m)
    enddo

    sum = 0
    do i = 1,10
        sum = sum + buffer%value(i)
    enddo

    write(*,*) 'Sum: ', sum

    !
    ! Create a new buffer and fill it only partly
    !
    call buffer%create( 10 )

    do i = 1,5
        call buffer%add( i )
    enddo

    write(*,*) 'Minimum: ', buffer%min()
    write(*,*) 'Maximum: ', buffer%max()
    write(*,*) 'Sum:     ', buffer%sum()
    write(*,*) 'Mean:    ', buffer%mean()

end program test_buffers
