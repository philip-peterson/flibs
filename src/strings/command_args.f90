! command_args.f90 --
!     Module to handle command arguments
!
!     In analogy to the C getopt facility.
!
!     The following types of arguments can be handled:
!     - Arguments start with a single or a double hyphen
!     - One-letter arguments that set or clear a logical option.
!       This type of arguments can be concatenated, like: -lrt being an
!       abbreviation of -l, -r and -t.
!     - One-letter arguments for which the value follows (integer, string,
!       real or logical) or is adjacent to the option itself:
!       -Lxyz or -L xyz - the option being -L and the value xyz
!     - Arguments of more than one letter, they cannot be concatenated
!       and if an explicit value is given, it must be the next argument.
!       Like: -out report.txt
!
module command_args
    implicit none

    private
    public :: cmd_option
    public :: optarg
    public :: handle_command_options
    public :: opt_value_next, opt_concat, opt_true, opt_false, opt_help, opt_stop, opt_ignore, opt_ignore_next
    public :: argument_ignored

    type cmd_option
        integer                         :: type_option
        character(len=1)                :: short_option
        character(len=:), allocatable   :: long_option
        character(len=:), allocatable   :: description
        logical, pointer                :: logical_var => null()
        integer, pointer                :: integer_var => null()
        real, pointer                   :: real_var    => null()
        real(kind=kind(1.0d0)), pointer :: double_var  => null()
        character(len=:), pointer       :: string_var  => null()
    end type cmd_option

    interface optarg
        module procedure optarg_integer, optarg_logical, optarg_real, optarg_double, &
                         optarg_string, optarg_novar
    end interface optarg

    integer, parameter :: opt_value_next  = 1  ! Value follows
    integer, parameter :: opt_concat      = 2  ! Value is concatenated
    integer, parameter :: opt_true        = 3  ! Logical value should be set to true
    integer, parameter :: opt_false       = 4  ! Logical value should be set to false
    integer, parameter :: opt_help        = 5  ! Print the help information
    integer, parameter :: opt_stop        = 6  ! Stop processing further arguments
    integer, parameter :: opt_ignore      = 7  ! Ignore this argument
    integer, parameter :: opt_ignore_next = 8  ! Ignore this argument and the next

    logical, dimension(:), allocatable :: argument_was_ignored

contains

! optarg_* --
!     Define the option
!
! Arguments:
!     var              Variable to be set
!     type             Type of option
!     short            Short option
!     long             Long option
!     description      Description
!
function optarg_integer( var, type, short, long, description ) result(opt)
    integer, target                        :: var
    integer, intent(in)                    :: type
    character(len=*), intent(in)           :: short
    character(len=*), intent(in)           :: long
    character(len=*), intent(in)           :: description

    type(cmd_option)                       :: opt

    opt%integer_var  => var
    opt%type_option  =  type
    opt%short_option =  short
    opt%long_option  =  long
    opt%description  =  description
end function optarg_integer

function optarg_logical( var, type, short, long, description ) result(opt)
    logical, target                        :: var
    integer, intent(in)                    :: type
    character(len=*), intent(in)           :: short
    character(len=*), intent(in)           :: long
    character(len=*), intent(in)           :: description

    type(cmd_option)                       :: opt

    opt%logical_var  => var
    opt%type_option  =  type
    opt%short_option =  short
    opt%long_option  =  long
    opt%description  =  description
end function optarg_logical

function optarg_real( var, type, short, long, description ) result(opt)
    real, target                           :: var
    integer, intent(in)                    :: type
    character(len=*), intent(in)           :: short
    character(len=*), intent(in)           :: long
    character(len=*), intent(in)           :: description

    type(cmd_option)                       :: opt

    opt%real_var     => var
    opt%type_option  =  type
    opt%short_option =  short
    opt%long_option  =  long
    opt%description  =  description
end function optarg_real

function optarg_double( var, type, short, long, description ) result(opt)
    real(kind=kind(1.0d0)), target         :: var
    integer, intent(in)                    :: type
    character(len=*), intent(in)           :: short
    character(len=*), intent(in)           :: long
    character(len=*), intent(in)           :: description

    type(cmd_option)                       :: opt

    opt%double_var   => var
    opt%type_option  =  type
    opt%short_option =  short
    opt%long_option  =  long
    opt%description  =  description
end function optarg_double

function optarg_string( var, type, short, long, description ) result(opt)
    character(len=*), target               :: var
    integer, intent(in)                    :: type
    character(len=*), intent(in)           :: short
    character(len=*), intent(in)           :: long
    character(len=*), intent(in)           :: description

    type(cmd_option)                       :: opt

    opt%string_var   => var
    opt%type_option  =  type
    opt%short_option =  short
    opt%long_option  =  long
    opt%description  =  description
end function optarg_string

function optarg_novar( type, short, long, description ) result(opt)
    integer, intent(in)                    :: type
    character(len=*), intent(in)           :: short
    character(len=*), intent(in)           :: long
    character(len=*), intent(in)           :: description

    type(cmd_option)                       :: opt

    opt%type_option  =  type
    opt%short_option =  short
    opt%long_option  =  long
    opt%description  =  description
end function optarg_novar

! handle_command_options --
!     Handle all the command arguments as defined by the optarg array
!
! Arguments:
!     optarg           Array of command-line options
!
subroutine handle_command_options( optarg )
    type(cmd_option), dimension(:) :: optarg

    integer                        :: number_arguments
    integer                        :: i, j, k, m
    integer                        :: help, stop
    integer                        :: length
    logical                        :: next_letter
    character(len=200)             :: string

    !
    ! Initialise the array argument_was_ignored - for later user-defined processing
    ! Also initialise the logical variables
    !
    number_arguments = command_argument_count()

    allocate( argument_was_ignored(number_arguments) )
    argument_was_ignored = .true.

    do i = 1,size(optarg)
        select case ( optarg(i)%type_option )
            case ( opt_true )
                optarg(i)%logical_var = .false.
            case ( opt_false )
                optarg(i)%logical_var = .true.
            case ( opt_stop )
                exit
        end select
    enddo

    !
    ! Request for help?
    !
    help = -1
    stop = -1
    do i = 1,size(optarg)
        if ( optarg(i)%type_option == opt_help ) then
            help = i
        endif
        if ( optarg(i)%type_option == opt_stop ) then
            stop = i
        endif
    enddo

    do i = 1,number_arguments
        call get_command_argument( i, string, length )

        if ( stop == -1 ) then
            if ( string(1:length) == '--' ) then
                exit
            endif
        else
            if ( string(1:length) == '-'  // optarg(stop)%short_option .or. &
                 string(1:length) == '--' // optarg(stop)%long_option ) then
                exit
            endif
        endif

        if ( help == -1 ) then
            if ( string(1:length) == '-h' .or. string(1:length) == '-?' .or. &
                 string(1:length) == '--help' ) then
                call print_help( optarg, help, stop )
                stop
            endif
        else
            if ( string(1:length) == '-'  // optarg(help)%short_option .or. &
                 string(1:length) == '--' // optarg(help)%long_option ) then
                call print_help( optarg, help, stop )
                stop
            endif
        endif
    enddo

    !
    ! Handle the short arguments - they may be "chained"
    !
    i = 0
    do while ( i < number_arguments )
        i = i + 1
        call get_command_argument( i, string, length )

        if ( stop == -1 ) then
            if ( string(1:length) == '--' ) then
                exit
            endif
        else
            if ( string(1:length) == '-'  // optarg(stop)%short_option .or. &
                 string(1:length) == '--' // optarg(stop)%long_option ) then
                exit
            endif
        endif

        if ( string(1:1) == '-' .and. string(1:2) /= '--' ) then
            next_letter = .false.
            do j = 1,size(optarg)
                if ( string(2:2) == optarg(j)%short_option ) then
                    select case ( optarg(j)%type_option )
                        case ( opt_ignore )
                            exit

                        case ( opt_ignore_next )
                            i = i + 1
                            exit

                        case ( opt_true )
                            argument_was_ignored(i) = .false.
                            optarg(j)%logical_var   = .true.
                            next_letter             = .true.

                        case ( opt_false )
                            argument_was_ignored(i) = .false.
                            optarg(j)%logical_var   = .false.
                            next_letter             = .true.

                        case ( opt_concat )
                            call handle_value( string(2:), optarg(j), i )
                            argument_was_ignored(i) = .false.

                        case ( opt_value_next )
                            i = i + 1
                            call get_command_argument( i, string, length )
                            call handle_value( string, optarg(j), i )
                            argument_was_ignored(i-1) = .false.

                        case default
                            ! Nothing to do
                    end select
                endif
            enddo

            !
            ! Handle the possible "chaining" of logical options
            !
            if ( next_letter ) then
                do k = 3,length
                    do m = 1,size(optarg)
                        if ( string(k:k) == optarg(m)%short_option ) then
                            select case ( optarg(m)%type_option )
                                case ( opt_true )
                                    optarg(m)%logical_var   = .true.

                                case ( opt_false )
                                    optarg(m)%logical_var   = .false.

                                case default
                                    ! Nothing to do
                            end select
                        endif
                    enddo
                enddo
            endif

        endif
    enddo

    !
    ! Handle the long options - they cannot be "chained" or have concatenated values
    !
    i = 0
    do while ( i < number_arguments )
        i = i + 1
        call get_command_argument( i, string, length )

        if ( stop == -1 ) then
            if ( string(1:length) == '--' ) then
                exit
            endif
        else
            if ( string(1:length) == '-'  // optarg(stop)%short_option .or. &
                 string(1:length) == '--' // optarg(stop)%long_option ) then
                exit
            endif
        endif

        if ( string(1:2) == '--' ) then
            do j = 1,size(optarg)
                if ( string(3:) == optarg(j)%long_option ) then
                    select case ( optarg(j)%type_option )
                        case ( opt_ignore )
                            exit

                        case ( opt_ignore_next )
                            i = i + 1
                            exit

                        case ( opt_true )
                            argument_was_ignored(i) = .false.
                            optarg(j)%logical_var   = .true.
                            next_letter             = .true.

                        case ( opt_false )
                            argument_was_ignored(i) = .false.
                            optarg(j)%logical_var   = .false.
                            next_letter             = .true.

                        case ( opt_concat, opt_value_next )
                            i = i + 1
                            call get_command_argument( i, string, length )
                            call handle_value( string, optarg(j), i )
                            argument_was_ignored(i-1) = .false.

                        case default
                            ! Nothing to do
                    end select
                endif
            enddo
        endif
    enddo
end subroutine handle_command_options

! argument_ignored --
!     Return whether the argument was ignored or not
!
! Arguments:
!     indx             Index of the argument
!
logical function argument_ignored( indx )
    integer, intent(in) :: indx

    if ( indx < 1 .or. indx > size(argument_was_ignored) ) then
        argument_ignored = .false.
    else
        argument_ignored = argument_was_ignored(indx)
    endif
end function argument_ignored

! handle_value --
!     Handle the value stored in the string
!
! Arguments:
!     string           String to be read
!     optarg           Argument that should receive the value
!     indx             Index of the option
!
subroutine handle_value( string, optarg, indx )
    character(len=*), intent(in)   :: string
    type(cmd_option)               :: optarg
    integer, intent(in)            :: indx

    integer                        :: ierr

    ierr = 0

    if ( associated( optarg%integer_var ) ) then
        read( string, *, iostat = ierr ) optarg%integer_var
    endif

    if ( associated( optarg%real_var ) ) then
        read( string, *, iostat = ierr ) optarg%real_var
    endif

    if ( associated( optarg%double_var ) ) then
        read( string, *, iostat = ierr ) optarg%double_var
    endif

    if ( associated( optarg%string_var ) ) then
        optarg%string_var = string
    endif

    if ( associated( optarg%logical_var ) ) then
        optarg%logical_var = &
            any( string == ['1   ', 't   ', 'T   ', 'yes ', 'true', 'YES ', 'TRUE', 'Yes ', 'True'] )
    endif

    if ( ierr /= 0 ) then
        write(*,*) 'Error reading the value: ', trim(string)
        write(*,*) '    - option ignored'
        argument_was_ignored(indx) = .true.
    else
        argument_was_ignored(indx) = .false.
    endif
end subroutine handle_value

! print_help --
!     Print the options
!
! Arguments:
!     optarg           Description of the command options
!     help             Index of the "help" option
!     stop             Index of the "stop processing" option
!
subroutine print_help( optarg, help, stop )
    type(cmd_option), dimension(:), intent(in) :: optarg
    integer, intent(in)                        :: help
    integer, intent(in)                        :: stop

    integer            :: i, k1, k2
    character(len=200) :: string

    call get_command_argument( 0, string )
    k1 = index( string, '\', .true. )
    k2 = index( string, '/', .true. )

    if ( k1 > 0 ) then
        string = string(k1+1:)
    elseif ( k2 > 0 ) then
        string = string(k2+1:)
    endif

    write(*,*) 'Usage:'
    write(*,*) '    ', trim(string), ' ?options?'
    write(*,*) 'where the options are:'

    do i = 1,size(optarg)
        write(*,*) '    ', '-' // optarg(i)%short_option, ' / --' // trim(optarg(i)%long_option), &
                   '    ', trim(optarg(i)%description)
    enddo

    if ( stop == -1 .or. help == -1 ) then
        write(*,*) 'In addition'
        if ( stop == -1 ) then
            write(*,*) '    --     Arguments following this option will not be examined'
        endif
        if ( help == -1 ) then
            write(*,*) '    -h/-?/--help    Print this information'
        endif
    endif

end subroutine print_help

end module command_args
