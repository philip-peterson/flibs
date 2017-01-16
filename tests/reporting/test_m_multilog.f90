! test_m_multilog.f90 --
!     Small test program for the multiple logs utility. Based on the
!     test program for m_logger.
!
!     Karin Nyström 2012 - 2013
!
program test_multilogging
  use m_multilog
  implicit none

  character(len=500) :: msg
  integer            :: logunit, i, infolevel
  integer, pointer   :: stop_program => null()
  type(log_t)        :: log_fine, log_info, log_warning, log_error
  !
  ! Initialize the log files. Log without explicit info_lvl should be
  ! assigned infolevel=INFO. append=.false. to get rid of previous test
  ! logs.
  !
  write (*,*) 'Starting logs log_fine, log_info, log_warning and log_error'
  call log_fine%startup( 'test_m_multilog_fine.log', append=.false., info_lvl=FINE)
  call log_info%startup( 'test_m_multilog_info.log', append=.false.)
  call log_warning%startup( 'test_m_multilog_warning.log', append=.false., info_lvl=WARNING)
  call log_error%startup( 'test_m_multilog_error.log', append=.false., info_lvl=ERROR)
  !
  ! Add the four logs to the log_group object
  !
  write (*,*) 'Adding all log files to log group.'
  call add_log( log_fine )
  call add_log( log_info )
  call add_log( log_warning )
  call add_log( log_error )
  !
  ! Message with and without timestamp written to all four logs. 
  ! 
  write (*,*) 'Writing two messages to all logs'
  write (*,*) 'Changing log configurations globally'
  call log_group_configure( "timestamp", .true. )
  call log_msg( 'First message, with default timestamp' )
  call log_group_configure( "timestamp", .false. )
  call log_msg( 'Second message, without timestamp' )
  !
  ! Enable/disable and change timestamp for a single log.
  !
  call log_info%configure( "timestamp", .true. )
  call log_msg( 'This message should have timestamp only in log_info' )
  call log_info%configure( "timefmt", "HH:MM:SS" )
  call log_msg( 'This message has a custom timestamp in log_info' )
  call log_info%configure( "timestamp", .false. )
  call log_msg( 'Message without timestamp to all logs' )
  !
  ! Check that all logs have the appropriate infolevels
  !
  call log_fine%cget( "infolevel", infolevel )
  write( msg, '(a,i1,a,i1)') "log_fine has infolevel ", infolevel, ", FINE=", FINE
  call log_msg( msg )
  call log_info%cget( "infolevel", infolevel )
  write( msg, '(a,i1,a,i1)') "log_info has infolevel ", infolevel, ", INFO=", INFO
  call log_msg( msg )
  call log_warning%cget( "infolevel", infolevel )
  write( msg, '(a,i1,a,i1)') "log_warning has infolevel ", infolevel, ", WARNING=", WARNING
  call log_msg( msg )
  call log_error%cget( "infolevel", infolevel )
  write( msg, '(a,i1,a,i1)') "log_error has infolevel ", infolevel, ", ERROR=", ERROR
  call log_msg( msg )
  !
  ! Close, remove, restart, add log_fine and write a message with level FINE
  !
  write (*,*) 'Closing log_fine, removing from logger, restarting with append = .false., '
  write (*,*) 'adding to logger and writing a message to that log only'
  call remove_log( log_fine )
  call log_fine%startup( 'test_m_multilog_fine.log', append=.false., info_lvl=FINE)
  call add_log( log_fine )
  call log_msg( 'Third message - should be the first row now', FINE )
  !
  ! Write one message per info level. ERROR should be written to all
  ! logs, WARNING to all except log_error, INFO to log_info and
  ! log_fine, FINE and DEBUG should only be written to log_fine.
  !
  write (*,*) 'Writing one message per info level'
  call log_msg( 'Error message', ERROR )
  call log_msg( 'Warning message', WARNING )
  call log_msg( 'Info message', INFO )
  call log_msg( 'Detailed message', FINE )
  call log_msg( 'Debug message', DEBUG )
  call log_msg( 'Message to all logs', ALL )
  !
  ! Add delimiter to structure the log files
  !
  write (*,*) 'Adding delimiters of all levels to all logs'
  call log_delimiter( level=LOG_LEVEL_VOLUME )
  call log_msg( 'Volume title' )
  call log_delimiter( level=LOG_LEVEL_CHAPTER )
  call log_msg( 'Chapter title' )
  call log_delimiter( level=LOG_LEVEL_SECTION )
  call log_msg( 'Section title' )
  call log_delimiter( level=LOG_LEVEL_SUBSECTION )
  call log_msg( 'Subsection title' )
  !
  ! Get the log unit and write directly to the logfile
  !
  write (*,*) 'Get unit number for log_warning and write to that'
  call log_warning%cget( "logfileunit" , logunit )
  write ( logunit , "(A)") "This is my manually written message, written only to log_warning's file."
  !
  ! Enable / disable writing on standard output. When no info
  ! level is supplied to log_msg, INFO should be used.
  !
  write (*,*) 'Write one message only to file.'
  call log_group_configure( "writeonstdout" , .false. )
  call log_msg( 'This message is written only on file' )
  write (*,*) 'Setting writestdout to .true. again'
  call log_group_configure( "writeonstdout" , .true. )
  call log_msg( 'This message is written both on screen and on file' )
  !
  ! Shut down one of the logs and write that to info_log.
  ! 
  write (*,*) 'Removing log_fine and writing message to log_info'
  call remove_log( log_fine )
  call log_msg( "log_fine shut down, now this (log_info) is the most detailed log.", INFO)
  !
  ! Shut down the rest of the logs.
  !
  write (*,*) 'Closing down rest of the logs and the log group.'
  call shutdown_log_group()
  !
  ! Abort the program to test that the log file is properly flushed
  !
  call log_info%startup( 'test_m_multilog_info.log' )
  call add_log( log_info )
  call log_msg( 'Before expressly aborting the program' )
  write( *, '(a)' ) ' '
  write( *, '(a)' ) 'Warning: the program will abort now - this is intentional'
  write( *, '(a)' ) 'On some systems an error or even a trace back will be printed,'
  write( *, '(a)' ) 'on others you may invited to use the debugger to see what went wrong'
  write( *, '(a)' ) ' '
  write( *, '(a)' ) 'Check that the log file "test_m_multilog_info.log" is complete,'
  write( *, '(a)' ) 'whereas the file "test_m_multilog.out" is not'

  open( 10, file = 'test_m_multilog.out' )
  do i = 1,100
     write( 10, '(a,i4)' ) 'This file is supposed to be truncated - hopefully it is'
  end do
  ! With some compilers/systems test_m_multilog.out will have 100
  ! lines, with others no or very few lines depending on how buffering
  ! is done.
  stop_program = 10 ! Uninitialised pointer - it will cause the program to fail
  write( 10, * ) 'Value: ', stop_program
  write( 10, * ) 'End of program'
  call log_msg( 'Program aborted - this will not appear in the log' )

end program test_multilogging
