! m_multilog.f90
!
!   The module m_multilog provides a logging type with a set of type
!   bound procedures and other methods to manage one or more log
!   files. The module and its test program are based on the m_logger
!   module (including unit test) which is part of the flibs library.
!   For applications in need of only one log, m_logger is the
!   recommended module.
!
! Overview
!
!   The goal of this component is to provide a way to write messages
!   both on standard output and on one or more log files, so that a
!   trace of the execution - with desired level of detail - can be
!   read by the user after the execution.
!
!   One single statement should be able to write to all open log files
!   with appropriate log level set. Example:
!
!      call log_msg ('Log message', WARNING)
!
!   will write to logs with log level equal to or lower than
!   WARNING, i.e. WARNING, INFO, FINE and DEBUG.
!
!   The module provides methods to
!   - connect a file to a log object
!   - configure the logging process, for example disable the standard
!     output messages or enable time stamps,
!   - log messages,
!   - group log objects,
!   - write log messages to a group of log objects.
!
!   Individual logs are started with log_t%startup (filename [,options]).
!   The method takes the log file name as first argument :
!   its main purpose is to connect the log to the file.
!   Optional arguments: log information level, append true/false
!
!   Logs can be added to the log group with "add_log". Logs can be
!   removed from the log group using the "remove_log" method, which will
!   also close the file connected to the log.
!
!   The messages are sent to one or more of the active logs with the
!   static method "log_msg". Infolevel is an optional argument to
!   log_msg, if left out it's set to ALL which means that the message
!   will be written to all logs.
!
!   In the following example, extracted from the unit tests of
!   m_multilog provided with the project, one connects the file
!   "test_m_multilog.log" to the log, two messages of which only the
!   first should be written to file and shut down the logging system.
!
!      type (log_t) :: test
!
!      call test%startup ( 'test_m_multilog.log' , INFO )
!      call add_log ( test )
!      call log_msg ( 'First message' , INFO )
!      call log_msg ( 'Second message', FINE )
!      call shutdown_log_group ()
!
!    By default, the logging is done on all log files with appropriate
!    infolevel but not on standard output. The user may want to
!    configure the behaviour of the log_group as a whole or the
!    separate logs so that message are not written on standard output.
!
!    The static method "log_group_configure ( option, value )" is the
!    central point to configure the log group itself or all logs
!    connected to it. It takes a character "option" string and a
!    "value" as arguments.
!
!    It is also possible to configure a specific log using the type
!    bound generic routine log_t%configure ( option, value ). In the
!    following example, one writes messages on file and in one case
!    also to stdout.
!
!      type (log_t) :: test
!
!      call test%startup ( 'test_m_multilog.log' )
!      call add_log ( test )
!      call test%configure ( "writeonstdout" , .true. )
!      call log_msg( 'This message is written both on screen and on file' )
!      call test%configure ( "writeonstdout" , .false. )
!      call log_msg( 'This message is written only on file' )
!      call shutdown_log_group ()
!
! TODO   Configurable log levels i.e. possibility to re-arrange the hierarchy
!        Parallel mode - split to one log/thread and then merge?
!
! Author: Karin Nyström, 2012-2013, knystrom at users.sourceforge.net
!
module m_multilog
  use iso_fortran_env
  implicit none
  private
  public :: shutdown_log_group
  public :: add_log
  public :: remove_log
  public :: log_group_configure
  public :: log_group_cget
  public :: log_group_reset
  public :: log_delimiter
  public :: log_msg
  !
  ! Date/time type, used internally for timestamp configuration
  !
  type :: datetime_t
     integer :: year
     integer :: month
     integer :: day
     integer :: hour
     integer :: minute
     integer :: sec
     contains
       procedure :: set_datetime
  end type datetime_t
  !
  ! Log type - one single log
  !
  type, public :: log_t
     character(len=500) :: filename
     ! Date/time formatting string, see format_date at the end of the module
     character(len=40)  :: timefmt
     integer :: logindex
     integer :: fileunit
     integer :: infolevel
     ! Set to true to include a timestamp
     logical :: timestamp
     ! Logical set to false if the user wants to inactivate the
     ! ouput to screen. The default value is true.
     logical :: writestdout
     ! Set to false if the user wants to inactivate output to file.
     ! Default is true.
     logical :: writetofile
   contains
     procedure :: startup
     procedure :: shutdown
     procedure :: reset => log_reset
     procedure :: write
     procedure :: log_configure_logical
     procedure :: log_configure_integer
     procedure :: log_configure_character
     generic :: configure => log_configure_logical, log_configure_integer, log_configure_character
     procedure :: log_cget_logical
     procedure :: log_cget_integer
     procedure :: log_cget_character
     generic :: cget => log_cget_logical, log_cget_integer, log_cget_character
  end type log_t
  !
  ! Pointer to log_t
  !
  type :: log_ptr_t
     type(log_t), pointer :: p
  end type log_ptr_t
  !
  ! Log group type - handles one to four logs
  !
  type :: log_group_t
     ! Keep track of current number of logs connected to this logger
     integer :: nroflogs
     ! Try to abort program when error in logger occurs?
     logical :: stoponerror
     ! Four info_lvl values -> array of max four log files
     type(log_ptr_t), dimension(4) :: log_array
  end type log_group_t
  !
  ! Interfaces for procedures using the global log_group object.
  ! Convert to type bound if there is need to use more than one
  ! simultaneous log group. (One log group handles several log files.)
  !
  interface log_group_configure
     module procedure log_group_configure_logical
     module procedure log_group_configure_integer
     module procedure log_group_configure_character
  end interface log_group_configure
  interface log_group_cget
     module procedure log_group_cget_logical
     module procedure log_group_cget_character
  end interface log_group_cget
  !
  ! Static fields
  !
  ! Log handler - global. Could remove this and change procedures
  ! related to logger to type bound if there is need for more than one
  ! log handler.
  !
  type(log_group_t), public :: log_group
  ! Logical unit associated with the standard output
  integer :: log_stdout = OUTPUT_UNIT
  !
  ! Strings used as default delimiters
  !
  integer, parameter, public :: LOG_LEVEL_DELIMITER_LENGTH = 50
  character(len=LOG_LEVEL_DELIMITER_LENGTH) :: log_level_string_volume = "==============="
  character(len=LOG_LEVEL_DELIMITER_LENGTH) :: log_level_string_chapter = "---------------"
  character(len=LOG_LEVEL_DELIMITER_LENGTH) :: log_level_string_section = "***************"
  character(len=LOG_LEVEL_DELIMITER_LENGTH) :: log_level_string_subsection = "+++++++++++++++"
  !
  ! List of available delimiters levels
  !
  integer, parameter, public :: LOG_LEVEL_VOLUME = 1
  integer, parameter, public :: LOG_LEVEL_CHAPTER = 2
  integer, parameter, public :: LOG_LEVEL_SECTION = 3
  integer, parameter, public :: LOG_LEVEL_SUBSECTION = 4
  !
  ! List of available log information levels
  ! DEBUG   - all messages
  ! FINE    - all messages
  ! INFO    - error, warning and info
  ! WARNING - error and warning
  ! ERROR   - only error messages
  !
  integer, parameter, public :: DEBUG   = 0
  integer, parameter, public :: FINE    = 0
  integer, parameter, public :: INFO    = 1
  integer, parameter, public :: WARNING = 2
  integer, parameter, public :: ERROR   = 3
  integer, parameter, public :: ALL     = 9

contains
  !
  ! Type bound procedures - log_t
  !
  ! startup --
  !     Initialises the log, connect it to the given filename
  !     and set default values.
  !
  ! Arguments:
  !     log_file           Name of the log file
  !     append, optional :
  !      - if present and true, then the messages will be appended
  !        to the end of the log file.
  !      - if present and false, then the initialization of the
  !        log overwrites the messages of the previous logging session.
  !      - if not provided, the default value is append=.true.
  !     info_lvl, optional:
  !      - if present, set log_t%infolevel to the provided value
  !      - if not present, set to default value INFO
  subroutine startup ( this, log_file, append, info_lvl )
    class(log_t),        intent(in out) :: this
    character(len=*),    intent(in)     :: log_file
    logical, intent(in), optional       :: append
    integer, intent(in), optional       :: info_lvl
    logical :: append_real
    !
    ! Process options
    !
    if ( present ( append ) ) then
       append_real = append
    else
       append_real = .true.
    end if
    if ( append_real ) then
       open (NEWUNIT=this%fileunit, FILE=log_file , ACTION='WRITE', STATUS='UNKNOWN', &
            POSITION ='APPEND')
    else
       open (NEWUNIT=this%fileunit, FILE=log_file , ACTION='WRITE', STATUS='UNKNOWN')
    end if
    this%timestamp = .false.
    ! Default timestamp format
    this%timefmt = 'yyyy-mm-dd HH:MM:SS'
    ! Set file name
    this%filename = log_file
    ! Default: no output to stdout, only to file
    this%writestdout = .false.
    this%writetofile = .true.
    ! Set log info level to provided value or default
    if ( present ( info_lvl ) ) then
       this%infolevel = info_lvl
    else
       this%infolevel = INFO
    end if
    this%logindex = 0
  end subroutine startup
  ! shutdown --
  !     Shutdown the log
  !
  ! Arguments:
  !     None
  !
  subroutine shutdown ( this )
    class(log_t), intent(in out) :: this
    close( this%fileunit )
  end subroutine shutdown
  !
  ! log_configure_logical --
  !   Set the logical static "option" of the component to "val" for this log.
  !   The "option" may be one of the following.
  ! option = "timestamp"
  !   Disable or enable the insertion of time stamps.
  !   If the time stamp option is enabled, a time stamp with
  !   format "yyyy-MM-dd hh:mm:ss" is inserted before the message.
  ! option = "writeonstdout"
  !   Disable or enable the writing on standard output.
  ! option = "writetofile"
  !   Disable or enable the writing to file.
  !
  ! Arguments:
  !    option        see above
  !    val            "    "
  !
  subroutine log_configure_logical ( this, option, val )
    class(log_t),     intent(in out) :: this
    character(len=*), intent(in)     :: option
    logical,          intent(in)     :: val
    character(len=500) :: message
    select case ( option )
    case ( "timestamp" )
       this%timestamp = val
    case ( "writeonstdout" )
       this%writestdout = val
    case ( "writeonfile" )
       this%writetofile = val
    case default
       write( message, "(A,A,A,l5,A)" ) "Unknown option ", option, &
            " for value ", val, " in log_configure_logical"
       call log_error( message )
    end select
  end subroutine log_configure_logical
  !
  ! log_configure_integer --
  !   Set the integer "option" to "val" for this log.
  !   The "option" may be one of the following.
  ! option = "infolevel"
  !   Set level of detail in the log. Available values: ERROR, WARNING, INFO, FINE
  !
  ! Arguments:
  !    option        see above
  !    val            "    "
  !
  subroutine log_configure_integer ( this, option , val )
    class(log_t),     intent(in out) :: this
    character(len=*), intent(in)     :: option
    integer,          intent(in)     :: val
    character(len=500) :: message
    select case ( option )
    case ( "infolevel" )
       if ( val >= 0 .and. val <= 3 ) then
          this%infolevel = val
       else
          write( message, "(A,I5,A)" ) "Unknown value ", val, &
               " for option infolevel in log_configure_integer"
          call log_error( message )
       end if
    case default
       write( message, "(A,A,A,I5,A)" ) "Unknown option ", option, &
            " for value ", val, " in log_configure_integer"
       call log_error( message )
    end select
  end subroutine log_configure_integer
  !
  ! log_configure_character --
  !   Set the character "option" to "val" for this log.
  !   The "option" may be one of the following.
  ! option = "timefmt"
  !   Set the string used for timestamp formatting.
  !
  subroutine log_configure_character ( this, option , val )
    class(log_t),     intent(in out) :: this
    character(len=*), intent(in)     :: option
    character(len=*), intent(in)     :: val
    character(len=500) :: message
    select case ( option )
    case ( "timefmt" )
       this%timefmt = val
    case default
       write( message, "(A,A,A,A,A)" ) "Unknown option ", option, &
            " for value ", val, " in log_configure_character"
       call log_error( message )
    end select
  end subroutine log_configure_character
  !
  ! log_cget_logical --
  !   Returns the value of the given logical option for this log.
  !   The "option" may be one of the following.
  ! option = "timestamp"
  !   Current value of the option to enable / disable insertion of time stamps.
  ! option = "writeonstdout"
  !   Current value of the option to enable / disable writing on standard output.
  ! option = "writetofile"
  !   Current value of the option to enable / disable writing on log file.
  !
  ! Arguments:
  !    option        see above
  !    val            "    "
  !
  subroutine log_cget_logical ( this, option , val )
    class(log_t),     intent(in)  :: this
    character(len=*), intent(in)  :: option
    logical,          intent(out) :: val
    character(len=500) :: message

    val = .false.
    select case ( option )
    case ( "timestamp" )
       val = this%timestamp
    case ( "writeonstdout" )
       val = this%writestdout
    case ( "writeonfile" )
       val = this%writetofile
    case default
       write( message, "(A,l5,A)" ) "Unknown option ", option, &
            " in log_cget_logical"
       call log_error( message )
    end select
  end subroutine log_cget_logical
  !
  ! log_cget_integer --
  !   Returns the value of the given integer option for this log.
  !   The "option" may be one of the following.
  ! option = "infolevel"
  !   Minimum level for writing messages
  ! option = "logfileunit"
  !   Current logical unit connected to the log provided as argument
  !
  subroutine log_cget_integer ( this, option, val )
    class(log_t),     intent(in)  :: this
    character(len=*), intent(in)  :: option
    integer,          intent(out) :: val
    character(len=500) :: message
    select case ( option )
    case ( "infolevel" )
       val = this%infolevel
    case ( "logfileunit" )
       val = this%fileunit
    case default
       write( message, "(A,I5,A)" ) "Unknown option ", option, &
            " in log_cget_integer"
       call log_error( message )
    end select
  end subroutine log_cget_integer
  !
  ! log_cget_character --
  !   Returns the value of the given option.
  !   The "option" may be one of the following.
  ! option = "timefmt"
  !   Get the string used for timestamp formatting
  !
  subroutine log_cget_character ( this, option , val )
    class(log_t),     intent(in)  :: this
    character(len=*), intent(in)  :: option
    character(len=*), intent(out) :: val
    character(len=500) :: message
    select case ( option )
    case ( "timefmt" )
       val = this%timefmt
    case default
       write( message, "(A,A,A)" ) "Unknown option ", option, &
            " in log_cget_character"
       call log_error( message )
    end select
  end subroutine log_cget_character
  !
  ! log_reset --
  !    Set all internal settings to default values for this log
  !
  subroutine log_reset ( this )
    class(log_t), intent(in out) :: this
    this%infolevel = INFO
    this%timestamp = .false.
    this%writestdout = .false.
    this%writetofile = .true.
    this%timefmt = 'yyyy-mm-dd HH:MM:SS'
  end subroutine log_reset
   ! write --
  !   Log the given character string to one logging unit.
  !   If the logging to standard output is enabled, writes the message
  !   on standard output.
  !   If the logging to the log file is enabled, writes the message
  !   into the log file.
  !   Before outputting directly the message string, the string is
  !   trimmed, that is to say that all trailing blanks are removed from
  !   the string.
  !   If the time stamp option is enabled, a time stamp with format
  !   "yyyy-mm-dd HH:MM:SS" or custom format set with
  !   log_*configure_character is inserted before the message.
  !
  ! Arguments:
  !     msg           Log message to be written
  !     info_lvl      Optional: log information level, default = INFO
  !
  subroutine write( this, msg, info_lvl )
    class(log_t),     intent(in out) :: this
    character(len=*), intent(in)     :: msg
    integer,optional, intent(in)     :: info_lvl
    character(len=40) :: date_string
    character(len=40) :: time_string
    character(len=40) :: stamp
    type(datetime_t)  :: now
    if ( this%timestamp ) then
       call date_and_time( date = date_string, time = time_string )
       call now%set_datetime( date_string(1:4), date_string(5:6), &
            date_string(7:8), time_string(1:2), &
            time_string(3:4), time_string(5:6) )
       call format_date( now, this%timefmt, stamp )
    else
       stamp = ' '
    end if
    ! Write to stdout?
    if ( this%writestdout ) then
       if ( this%timestamp ) then
          call log_write ( log_stdout, trim( stamp ) // ' ' // msg, info_lvl )
       else
          call log_write ( log_stdout, msg, info_lvl )
       end if
    end if
    ! Write to file?
    if ( this%writetofile ) then
       ! Timestamp before message?
       if ( this%timestamp ) then
          call log_write( this%fileunit, trim( stamp ) // ' ' // msg, info_lvl )
       else
          call log_write( this%fileunit, msg, info_lvl )
       end if
    end if
  end subroutine write
  ! ------------------------------------------------------------
  !
  ! Procedures related to the global log handler 'log_group'
  !
  ! log_group_reset --
  !    Set all internal settings to default values for all logs
  !
  subroutine log_group_reset ( )
    integer :: i
    do i = 1, log_group%nroflogs
       log_group%log_array(i)%p%infolevel = INFO
       log_group%log_array(i)%p%timestamp = .false.
       log_group%log_array(i)%p%writestdout = .false.
       log_group%log_array(i)%p%writetofile = .true.
       log_group%log_array(i)%p%timefmt = 'yyyy-mm-dd HH:MM:SS'
    end do
  end subroutine log_group_reset
  ! shutdown_log_group --
  !   Close all logs connected to the log_group object
  !
  subroutine shutdown_log_group ( )
    integer :: i
    do i = 1, log_group%nroflogs
       call log_group%log_array(i)%p%shutdown()
    end do
    log_group%nroflogs = 0
  end subroutine shutdown_log_group
  ! add_log --
  !   Add a log_t object to the log_group
  !
  ! Arguments:
  !   log        log_t object (initialized with log_t%startup)
  !
  subroutine add_log ( log )
    type(log_t), target, intent(in out) :: log
    log_group%nroflogs = log_group%nroflogs + 1
    if ( log_group%nroflogs > size (log_group%log_array) ) then
       call log_error( 'add_log: Tried to add log to a full log group.' )
    else
       log_group%log_array(log_group%nroflogs)%p => log
    end if
    log%logindex = log_group%nroflogs
  end subroutine add_log
  ! remove_log --
  !   Remove a log_t object from the log_group and close the
  !   connected file.
  !
  ! Arguments:
  !   log        log_t object
  !
  subroutine remove_log ( log )
    type(log_t), target, intent(in out) :: log
    integer :: idx
    idx = log%logindex
    call log%shutdown()
    ! Move pointers to logs with higher index than the removed log
    ! down one step
    if ( idx < log_group%nroflogs ) then
       do idx = log%logindex, log_group%nroflogs - 1
          log_group%log_array(idx)%p => log_group%log_array(idx + 1)%p
       end do
    end if
    nullify ( log_group%log_array(log_group%nroflogs)%p )
    log_group%nroflogs = log_group%nroflogs - 1
  end subroutine remove_log
  ! log_msg --
  !   Log the given character string to all logging units with relevant
  !   info_lvl. If no info_lvl is specified, ALL is used (write to all
  !   logs).
  !   If the logging to standard output is enabled, writes the message
  !   on standard output as well as to relevant log files.
  !   Before outputting directly the message string, the string is
  !   trimmed, that is to say that all trailing blanks are removed from
  !   the string.
  !   If the time stamp option is enabled, a time stamp with
  !   format "year-month-day hh:mm:ss" is inserted before the message.
  !
  ! Arguments:
  !     msg           Log message to be written
  !     info_lvl      Optional: log information level, default = INFO
  !
  subroutine log_msg ( msg, info_lvl )
     character(len=*),  intent(in) :: msg
     integer, optional, intent(in) :: info_lvl
     integer :: i, used_level
     if ( present ( info_lvl ) ) then
        used_level = info_lvl
     else
        used_level = INFO
     end if
     if ( log_group%nroflogs < 1 ) then
        call log_error( 'log_msg: Tried to write to empty log group' )
     else
        do i = 1, log_group%nroflogs
           if ( log_group%log_array(i)%p%infolevel <= used_level ) then
              call log_group%log_array(i)%p%write( msg, used_level )
           end if
        end do
     end if
  end subroutine log_msg
  !
  ! log_group_configure_logical --
  !   Set the logical static "option" of the component to "val" for
  !   log_group itself or for all logs depending on the option.
  !   The "option" may be one of the following.
  ! option = "stoponerror"
  !   Disable or enable abort on error in logging.
  ! option = "timestamp"
  !   Disable or enable the insertion of time stamps for all logs.
  !   If the time stamp option is enabled, a time stamp with
  !   format "year-month-day hh:mm:ss" is inserted before the message.
  ! option = "writeonstdout"
  !   Disable or enable the writing on standard output for all logs.
  ! option = "writetofile"
  !   Disable or enable the writing to file for all logs.
  ! Arguments:
  !    option        see above
  !    val            "    "
  !
  subroutine log_group_configure_logical ( option, val )
    character(len=*), intent(in) :: option
    logical,          intent(in) :: val
    character(len=500) :: message
    integer :: i

    select case ( option )
    case ( "stoponerror" )
       log_group%stoponerror = val
    case ( "timestamp" )
       do i = 1, log_group%nroflogs
          log_group%log_array(i)%p%timestamp = val
       end do
    case ( "writeonstdout" )
       do i = 1, log_group%nroflogs
          log_group%log_array(i)%p%writestdout = val
       end do
    case ( "writeonfile" )
       do i = 1, log_group%nroflogs
          log_group%log_array(i)%p%writetofile = val
       end do
    case default
       write( message, "(A,A,A,l5,A)" ) "Unknown option ", option, &
            " for value ", val, " in log_group_configure_logical"
       call log_error( message )
    end select
  end subroutine log_group_configure_logical
  !
  ! log_group_configure_integer --
  !   Set the integer static "option" of the component to "val" for
  !   log_group itself or for all logs depending on the option.
  !   The "option" may be one of the following.
  ! option = "infolevel"
  !   Set level of detail in the logs. Available values: 
  !   ERROR, WARNING, INFO, FINE, DEBUG (where FINE = DEBUG)
  !
  ! Arguments:
  !    option        see above
  !    val            "    "
  !
  subroutine log_group_configure_integer ( option , val )
    character(len=*), intent(in) :: option
    integer,          intent(in) :: val
    character(len=500) :: message
    integer :: i
    select case ( option )
    case ( "infolevel" )
       if ( val >= 0 .and. val <= 3 ) then
          do i = 1, log_group%nroflogs
             log_group%log_array(i)%p%infolevel = val
          end do
       else
          write( message, "(A,I5,A)" ) "Unknown value ", val, &
               " for option infolevel in log_group_configure_integer"
          call log_error( message )
       end if
    case default
       write( message, "(A,A,A,I5,A)" ) "Unknown option ", option, &
            " for value ", val, " in log_group_configure_integer"
       call log_error( message )
    end select
  end subroutine log_group_configure_integer
  !
  ! log_group_configure_character --
  !   Set the character static "option" of the component to "val".
  !   The "option" may be one of the following.
  ! option = "level_string_volume"
  !   Set the string used for volume delimiter.
  ! option = "level_string_chapter"
  !   Set the string used for chapter delimiter.
  ! option = "level_string_section"
  !   Set the string used for section delimiter.
  ! option = "level_string_subsection"
  !   Set the string used for subsection delimiter.
  ! option = "timefmt"
  !   Set the string used for timestamp formatting for all logs.
  !
  subroutine log_group_configure_character ( option , val )
    character(len=*), intent(in) :: option
    character(len=*), intent(in) :: val
    character(len=500) :: message
    integer :: i
    select case ( option )
    case ( "level_string_volume" )
       log_level_string_volume = val
    case ( "level_string_chapter" )
       log_level_string_chapter = val
    case ( "level_string_section" )
       log_level_string_chapter = val
    case ( "level_string_subsection" )
       log_level_string_chapter = val
    case ( "timefmt" )
       do i = 1, log_group%nroflogs
          log_group%log_array(i)%p%timefmt = val
       end do
    case default
       write( message, "(A,A,A,A,A)" ) "Unknown option ", option, &
            " for value ", val, " in log_group_configure_character"
       call log_error( message )
    end select
  end subroutine log_group_configure_character
  !
  ! log_group_cget_logical --
  !   Returns the value of the given logical option. Only handling
  !   options for log_group itself. Use log_t%cget to retrieve options
  !   for specific logs.
  !   The "option" may be one of the following.
  ! option = "stoponerror"
  !   Current value of the option to enable / disable stopping when an error is met.
  !
  ! Arguments:
  !    option        see above
  !    val            "    "
  !
  subroutine  log_group_cget_logical ( option , val )
    character(len=*) , intent(in)  :: option
    logical,           intent(out) :: val
    character(len=500) :: message

    val = .false.
    select case ( option )
    case ( "stoponerror" )
       val = log_group%stoponerror
    case default
       write( message, "(A,l5,A)" ) "Unknown option ", option, &
            " in log_group_cget_logical"
       call log_error( message )
    end select
  end subroutine log_group_cget_logical
  !
  ! log_group_cget_character --
  !   Returns the value of the given option.
  !   The "option" may be one of the following.
  ! option = "level_string_volume"
  !   Get the string used for volume delimiter.
  ! option = "level_string_chapter"
  !   Get the string used for chapter delimiter.
  ! option = "level_string_section"
  !   Get the string used for section delimiter.
  ! option = "level_string_subsection"
  !   Get the string used for subsection delimiter.
  !
  subroutine log_group_cget_character ( option , val )
    character(len=*), intent(in)  :: option
    character(len=*), intent(out) :: val
    character(len=500) :: message
    select case ( option )
    case ( "level_string_volume" )
       val = LOG_LEVEL_STRING_VOLUME
    case ( "level_string_chapter" )
       val = LOG_LEVEL_STRING_CHAPTER
    case ( "level_string_section" )
       val = LOG_LEVEL_STRING_SECTION
    case ( "level_string_subsection" )
       val = LOG_LEVEL_STRING_SUBSECTION
    case default
       write (message,"(A,A,A)") "Unknown option ", option, &
            " in log_group_cget_character"
       call log_error ( message )
    end select
  end subroutine log_group_cget_character
  ! -----------------------------------------------------------------
  !
  ! Message and error handling routines
  !
  ! log_write --
  !     Write the given log message "msg" of length "length"
  !     on the unit "unit".
  !     Trim the given string before writing the string.
  !
  ! Arguments:
  !     unit             LU-number to write to
  !     msg              Message
  !     info_lvl         Optional; DEBUG, FINE, INFO, WARNING or ERROR
  !                      INFO is the default log information level,
  !                      which is also used if provided value of
  !                      info_lvl is not recognized.
  !
  subroutine log_write( unit, msg, info_lvl )
    integer,           intent(in out) :: unit
    character(len=*),  intent(in)     :: msg
    integer, optional, intent(in)     :: info_lvl

    if ( unit == log_stdout ) then
       if ( present ( info_lvl )) then
          select case (info_lvl)
          case ( FINE, INFO ) ! INFO == DEBUG
             write ( unit, '(a)' ) trim(msg)
          case ( WARNING )
             write ( unit, '(a)' ) 'Warning: '//trim(msg)
          case ( ERROR )
             write ( unit, '(a)' ) 'ERROR: '//trim(msg)
          case default
             write ( unit, '(a)' ) trim(msg)
          end select
       else
          write ( *, '(a)' ) trim(msg)
       end if
    else
       if ( present ( info_lvl )) then
          select case (info_lvl)
          case ( FINE, INFO ) ! INFO == DEBUG
             write ( unit, '(a)' ) trim(msg)
          case ( WARNING )
             write ( unit, '(a)' ) 'Warning: '//trim(msg)
          case ( ERROR )
             write ( unit, '(a)' ) 'ERROR: '//trim(msg)
          case default
             write ( unit, '(a)' ) trim(msg)
          end select
       else
          write ( unit, '(a)' ) trim(msg)
       end if
       !
       ! Flush the file
       !
       flush ( unit )
    end if
  end subroutine log_write
  ! log_delimiter --
  !   Log a delimiter of given level, to make so that the log file
  !   contain different visual parts.
  !   Available values for level are : LOG_LEVEL_VOLUME,
  !   LOG_LEVEL_CHAPTER, LOG_LEVEL_SECTION, LOG_LEVEL_SUBSECTION.
  !   If level is not provided, the default value for level is LOG_LEVEL_VOLUME.
  !
  ! Arguments:
  !     level            Level to be written
  !
  subroutine log_delimiter( log, level )
    type(log_t), optional, intent(in out) :: log
    integer,     optional, intent(in)     :: level
    character(len=40) :: msg ! the delimiter string
    integer           :: used_level
    if ( present( level ) ) then
       used_level = level
    else
       used_level = LOG_LEVEL_VOLUME
    endif
    call log_get_delimiter( used_level , msg )
    if ( present ( log ) ) then
       call log%write( msg )
    else
       call log_msg( msg )
    end if
  end subroutine log_delimiter
  !
  ! log_error --
  !   Generates an error for the log handling
  !
  subroutine log_error ( message )
    character(len=*), intent(in) :: message
    write ( ERROR_UNIT, "(A)" ) "Error in m_multilog."
    write ( ERROR_UNIT , "(A)" ) message
    call log_error_stop ( )
  end subroutine log_error
  !
  ! log_error_stop --
  !   Stop the execution if desired and possible.
  !
  subroutine log_error_stop ( )
    if ( log_group%stoponerror ) then
       
       stop
    endif
  end subroutine log_error_stop
  !
  ! log_get_delimiter --
  !   Fills msg with a log delimiter of given level.
  !   Available values for level are : LOG_LEVEL_VOLUME,
  !   LOG_LEVEL_CHAPTER, LOG_LEVEL_SECTION, LOG_LEVEL_SUBSECTION
  !
  ! Arguments:
  !     level             Level in question
  !     msg               Corresponding string
  !
  subroutine log_get_delimiter ( level , msg )
    integer,          intent(in)  :: level
    character(len=*), intent(out) :: msg
    character(len=500) :: err_msg
    select case (level)
    case (LOG_LEVEL_VOLUME)
       write(msg,*) "==============="
    case (LOG_LEVEL_CHAPTER)
       write(msg,*) "---------------"
    case (LOG_LEVEL_SECTION)
       write(msg,*) "***************"
    case (LOG_LEVEL_SUBSECTION)
       write(msg,*) "+++++++++++++++"
    case default
       write (err_msg, *) "Bad value for message level in log_get_delimiter:" , level
       call log_error ( err_msg )
    end select
  end subroutine log_get_delimiter
  !
  ! Type bound procedure for date/time type
  !
  subroutine set_datetime( this, year, month, day, hour, min, sec )
    class(datetime_t), intent(in out) :: this
    character(len=*), intent(in) :: year
    character(len=*), intent(in) :: month
    character(len=*), intent(in) :: day
    character(len=*), intent(in) :: hour
    character(len=*), intent(in) :: min
    character(len=*), intent(in) :: sec
    read( year, '(i4)' ) this%year
    read( month, '(i2)' ) this%month
    read( day, '(i2)' ) this%day
    read( hour, '(i2)' ) this%hour
    read( min, '(i2)' ) this%minute
    read( sec, '(i2)' ) this%sec
  end subroutine set_datetime
  !> Format a date/time as a string. From LibDate, adapted to
  !> datetime_base_t and added seconds.
  !
  ! Arguments:
  !     date             Date/time to be formatted
  !     pattern          String that serves as the pattern
  !     datestring       Resulting string
  !
  ! Note:
  !     The pattern can contain any of the following substrings
  !     that will be replaced by the corresponding date/time information
  !
  !     dd        Day of month ("01" for instance)
  !     ds        Day of month ("1" for instance, s for space)
  !     HH        Hour (00-23)
  !     HS        Hour (0-23)
  !     mm        Month ("01" for january)
  !     ms        Month ("1" for january, s for space)
  !     MM        Minutes within the hour (00-59)
  !     MS        Minutes within the hour (0-59)
  !     SS        Seconds (00-59)
  !     Ss        Seconds (0-59)
  !     YY        Year without the century
  !     yyyy      Year with the century
  !
  !     Each substring is replaced by a string of the same length or
  !     shorter.
  !
  !     The third argument should in general be at least as long
  !     as the pattern.
  !
  ! (Added by Arjen Markus)
  !
  ! From libdate module, copied here to avoid dependency and also to
  ! enable two modifications: added support for seconds and removed
  ! day of year (which didn't seem relevant for timestamps?). 
  subroutine Format_date( date, pattern, datestring )
    class(datetime_t), intent(in)  :: date
    character(len=*),  intent(in)  :: pattern
    character(len=*),  intent(out) :: datestring
    ! Local
    character(len=4) :: piece
    integer :: k
    logical :: found

    datestring = pattern

    found = .true.
    do while ( found )
       found = .false.

       k = index( datestring, 'dd' )
       if (k > 0) then
          found = .true.
          write( piece, '(i2.2)' ) date%day
          datestring(k:k+1) = piece
       end if

       k = index( datestring, 'ds' )
       if (k > 0) then
          found = .true.
          write( piece, '(i2)' ) date%day
          datestring(k:) = adjustl( piece(1:2) // datestring(k+2:) )
       end if

       k = index (datestring, 'HH' )
       if (k > 0) then
          found = .true.
          write( piece, '(i2.2)' ) date%hour
          datestring(k:k+1) = piece
       end if

       k = index( datestring, 'HS' )
       if (k > 0) then
          found = .true.
          write( piece, '(i2)' ) date%hour
          datestring(k:) = adjustl( piece(1:2) // datestring(k+2:) )
       end if

       k = index( datestring, 'mm' )
       if (k > 0) then
          found = .true.
          write( piece, '(i2.2)' ) date%month
          datestring(k:k+1) = piece
       end if

       k = index( datestring, 'ms' )
       if (k > 0) then
          found = .true.
          write( piece, '(i2)' ) date%month
          datestring(k:) = adjustl( piece(1:2) // datestring(k+2:) )
       end if

       k = index( datestring, 'MM' )
       if (k > 0) then
          found = .true.
          write( piece, '(i2.2)' ) date%minute
          datestring(k:k+1) = piece
       end if

       k = index( datestring, 'MS' )
       if (k > 0) then
          found = .true.
          write( piece, '(i2)' ) date%minute
          datestring(k:) = adjustl( piece(1:2) // datestring(k+2:) )
       end if

       k = index( datestring, 'SS' )
       if (k > 0) then
          found = .true.
          write( piece, '(i2.2)' ) date%sec
          datestring(k:k+1) = piece
       end if

       k = index( datestring, 'Ss' )
       if (k > 0) then
          found = .true.
          write( piece, '(i2)' ) date%sec
          datestring(k:) = adjustl( piece(1:2) // datestring(k+2:) )
       end if

       k = index( datestring, 'YY' )
       if (k > 0) then
          found = .true.
          write( piece, '(i2.2)' ) mod( date%year, 100 )
          datestring(k:k+1) = piece
       end if

       k = index( datestring, 'yyyy' )
       if (k > 0) then
          found = .true.
          write( piece, '(i4)' ) date%year
          datestring(k:k+3) = piece
       end if
    end do
  end subroutine Format_date

end module m_multilog

