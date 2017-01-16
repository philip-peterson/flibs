! generic_stack.f90 --
!     Implementation of a generic stack data structure
!
!     See the example/test program for the way to use it
!
!     Note:
!     This version uses Fortran 2003 fetaures like classes
!
!     Note:
!     Work in progress
!
!     $Id: generic_stack.f90,v 1.1 2012/11/27 07:36:51 arjenmarkus Exp $
!
module generic_stacks
    implicit none

    type :: generic_stack_data
        ! No data - all data must be defined in the extended classes
        integer :: dummy ! -- required by Intel Fortran 11.1
    !contains
        !procedure :: copy
    end type generic_stack_data

    type :: generic_stack
        integer :: number_used
        integer :: head
        logical :: fixed
        class(generic_stack_data), allocatable, dimension(:) :: data
    contains
        procedure :: create
        procedure :: is_empty
        procedure :: is_full
        procedure :: used_entries
        procedure :: free_entries
        procedure :: capacity
        procedure :: pop
        procedure :: push
        procedure, nopass :: copy
    end type generic_stack

contains

! copy --
!     Dummy copy routine - must be overwritten
!
subroutine copy( output, input )
    class(generic_stack_data) :: output
    class(generic_stack_data) :: input

    ! Nothing to do
end subroutine copy

! create --
!     Create (initialise) the stack
!
! Arguments:
!     this           The stack to be created (initialised)
!     capacity       Size of the stack (initial, if not fixed size)
!     fixed          Fixed size or dynamically extendable (default: true)
!
subroutine create( this, capacity, fixed )
    class(generic_stack) :: this
    integer              :: capacity
    logical, optional    :: fixed

    this%fixed = .true.
    if ( present(fixed) ) then
        this%fixed = fixed
    endif

    if ( .not. this%fixed ) then
        write(*,*) 'Error: extendable stacks not implemented yet'
    endif

    this%number_used = 0
    this%head        = 0

    ! We leave this to the concrete type!
    !    allocate( this%data(capacity) )

end subroutine create

! pop --
!     Retrieve the stack data at the top, remove it from the stack
!
! Arguments:
!     this           The stack in question
!     data           Variable that will contain the data from the top
!     success        Whether the operation was successful or not
!
subroutine pop( this, data, success )
    class(generic_stack)      :: this
    class(generic_stack_data) :: data
    logical                   :: success

    if ( this%number_used > 0 ) then
        !call data%copy( this%data(this%head) )
        call this%copy( data, this%data(this%head) )

        this%number_used = this%number_used - 1
        this%head        = this%head - 1
        if ( this%head < 1 .and. this%number_used > 0 ) then
            this%head = size(this%data)
        endif
        success = .true.
    else
        success = .false.
    endif
end subroutine pop

! push --
!     Put new data on the top of the stack
!
! Arguments:
!     this           The stack in question
!     data           Variable that contains the data to be put on the top
!     full           Whether the stack was full and data were lost
!
subroutine push( this, data, full )
    class(generic_stack)      :: this
    class(generic_stack_data) :: data
    logical                   :: full

    this%number_used = this%number_used + 1
    this%head        = this%head + 1
    if ( this%head > size(this%data) ) then
        this%head = 1
    endif

    !call this%data(this%head)%copy( data )
    call this%copy( this%data(this%head), data )

    if ( this%number_used <= size(this%data) ) then
        full = .false.
    else
        full = .true.
    endif
end subroutine push

! is_empty --
!     Returns whether the stack is empty or not
!
! Arguments:
!     this           The stack in question
!
logical function is_empty( this )
    class(generic_stack)      :: this

    is_empty = this%number_used == 0
end function is_empty

! is_full --
!     Returns whether the stack is full or not
!     (If so, adding new data will cause it to drop the oldest data)
!
! Arguments:
!     this           The stack in question
!
logical function is_full( this )
    class(generic_stack)      :: this

    is_full = this%number_used >= size(this%data)
end function is_full

! used_entries, free_entries, capacity --
!     Return various information on the stack
!
! Arguments:
!     this           The stack in question
!
integer function used_entries( this )
    class(generic_stack)      :: this

    used_entries = this%number_used
end function used_entries

integer function free_entries( this )
    class(generic_stack)      :: this

    free_entries = size(this%data) - this%number_used
end function free_entries

integer function capacity( this )
    class(generic_stack)      :: this

    capacity = size(this%data)
end function capacity

end module generic_stacks

! Simple test program
!
module mystack_type
    use generic_stacks

    type, extends(generic_stack_data) :: mystack_data
        integer :: idx
    !contains
        !procedure :: copy => copy_mystack_data
    end type mystack_data

    type, extends(generic_stack) :: mystack
        integer :: idx
    contains
        procedure :: create => create_mystack
        procedure, nopass :: copy => copy_mystack
    end type mystack

contains

subroutine copy_mystack( output, input )
    class(generic_stack_data)  :: output, input

    !
    ! Rather clumsy, I am afraid ...
    !
    select type( output )
        class is (mystack_data)
            select type( input )
                class is (mystack_data)
                    input%idx = output%idx
                class is (generic_stack_data)
                    write(*,*) 'Unknown type!'
            end select
    end select
end subroutine copy_mystack

subroutine create_mystack( this, capacity, fixed )
    class(mystack)       :: this
    integer              :: capacity
    logical, optional    :: fixed

    logical              :: fixed_

    fixed_ = .true.
    if ( present(fixed) ) then
        fixed_ = fixed
    endif

    call this%generic_stack%create( capacity, fixed_ )

    allocate( this%data(capacity) )
end subroutine create_mystack

end module mystack_type

program test_stacks
    use mystack_type

    type(mystack)      :: stack
    type(mystack_data) :: data
    logical            :: full
    logical            :: success

    call stack%create( 100, .true. )

    call stack%push( mystack_data(0,1), full )
    call stack%push( mystack_data(0,2), full  )
    call stack%push( mystack_data(0,3), full  )
    call stack%push( mystack_data(0,4), full  )

    call stack%pop( data, success )
    write(*,*) data, success

    write(*,*) stack%head
    select type (data => stack%data)
        class is (mystack_data)
            write(*,*) data(1:5)%idx
    end select

end program test_stacks
