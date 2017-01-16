! keyvars.f90 --
!     Module to automate getting values from a file into variables
!
!     TODO:
!     Actual implementation of get_values - done, apart from the extras
!     Perhaps support sections too as in INI-files - done, except for suitable keyvar
!     Reporting and stopping - extra arguments to get_values
!     display_help, write_input_file: no support for INI-file sections yet
!
!     Note: gfortran 4.8 does not like the "character(len=:), pointer" component
!
module keyvars
    implicit none

    interface keyvar
        module procedure keyvar_int
        module procedure keyvar_real
        module procedure keyvar_double
        module procedure keyvar_char
        module procedure keyvar_log
        module procedure keyvar_int_section
        module procedure keyvar_real_section
        module procedure keyvar_double_section
        module procedure keyvar_char_section
        module procedure keyvar_log_section
    end interface

    type keyvar_data
        integer, pointer                :: int_var => null()
        logical, pointer                :: log_var => null()
        real, pointer                   :: real_var => null()
        real(kind=kind(1.0d0)), pointer :: double_var => null()
        character(len=:), pointer       :: char_var => null()
        character(len=40)               :: section
        character(len=40)               :: name
        character(len=80)               :: description
    end type keyvar_data

contains

! get_values --
!     Get the values from a file with key value pairs
!
! Arguments:
!     filename_in         Name of the file to be used (can be overridden by the command arguments)
!     args                Array of descriptors for the key-value pairs - see keyvar routines
!
subroutine get_values( filename_in, args )
    character(len=*)                :: filename_in
    type(keyvar_data), dimension(:) :: args

    integer                         :: i, k
    integer                         :: lun
    integer                         :: ierr
    character(len=200)              :: line
    character(len=200)              :: value
    character(len=40)               :: section
    character(len=40)               :: name
    logical                         :: found, contin

    character(len=200)              :: filename

    character(len=6), dimension(6) :: truevalues = &
        ['true  ', '.true.', 'T     ', 'yes   ', 'Yes   ', 'YES   ']

    !
    ! Handle the command-line arguments, this may have as a consequence that the
    ! program stops (after -help, -output)
    !
    filename = filename_in
    call handle_command_arguments( filename, args )

    !
    ! Normal procedure: read the file
    !
    open( newunit = lun, file = filename, status = 'old', iostat = ierr )
    if ( ierr /= 0 ) then
        write(*,'(2a)') 'Input file not found: ', trim(filename)
        write(*,'(a)') 'Use -help for more information'
        stop
    endif

    section = ' '
    do
        read( lun, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) then
            exit
        endif

        !
        ! Remove comments
        !
        k    = index( line, '#' )
        if ( k > 0 ) then
            line = line(1:k-1)
        endif

        line = adjustl(line)

        !
        ! Skip empty lines
        !
        if ( len_trim(line) == 0 ) then
            cycle
        endif

        !
        ! Section? Like in INI-files
        !
        if ( line(1:1) == '[' ) then
            k       = index( line, ']' )
            if ( k <= 0 ) then
                k = len_trim(line)
            endif
            section = line(2:k-1)
        else
            !
            ! Ordinary key-value pair
            !
            k     = index( line, '=' )

            if ( k <= 0 ) then
                write(*,'(a)')  'Found line without key-value pair:'
                write(*,'(2a)') '    >', trim(line)
                write(*,'(a)')  'Ignored'
                cycle
            endif

            name  = line(1:k-1)
            value = adjustl(line(k+1:))

            !
            ! Look it up in the list
            !
            do i = 1,size(args)
                if ( args(i)%section == section .or. args(i)%section == '*' ) then
                    if ( args(i)%name == name ) then
                        ierr = 0
                        if ( associated(args(i)%int_var) ) then
                            read( value, *, iostat = ierr ) args(i)%int_var
                        elseif ( associated(args(i)%real_var) ) then
                            read( value, *, iostat = ierr ) args(i)%real_var
                        elseif ( associated(args(i)%double_var) ) then
                            read( value, *, iostat = ierr ) args(i)%double_var
                        elseif ( associated(args(i)%char_var) ) then
                            args(i)%char_var = value
                        elseif ( associated(args(i)%log_var) ) then
                            args(i)%log_var = any( truevalues == value )
                        endif

                        if ( ierr /= 0 ) then
                            write(*,'(a)') 'Error reading line:'
                            write(*,'(2a)') '    >', trim(line)
                            write(*,'(a)')  'Ignored'
                        else
                            found = .true.
                        endif
                    endif
                endif
            enddo

            if ( .not. found ) then
                write(*,'(2a)') 'Unknown keyword found: ', trim(name)
                write(*,'(a)')  'Please check this'
            endif
        endif
    enddo

    close( lun )
end subroutine get_values

! keyvar_* --
!     "Constructors" for the various data types
!
! Arguments:
!     name            Name of the key
!     pvar            Variable that will be pointed to
!     description     Description of the role of the variable in the program
!
!     section         (Second set) section in the INI-file
!
function keyvar_int( name, pvar, description ) result(key)
    character(len=*)  :: name
    integer, target   :: pvar
    character(len=*)  :: description
    type(keyvar_data) :: key

    key%section     =  '*'
    key%name        =  name
    key%description =  description
    key%int_var     => pvar
end function keyvar_int

function keyvar_real( name, pvar, description ) result(key)
    character(len=*)  :: name
    real, target      :: pvar
    character(len=*)  :: description
    type(keyvar_data) :: key

    key%section     =  '*'
    key%name        =  name
    key%description =  description
    key%real_var    => pvar
end function keyvar_real

function keyvar_double( name, pvar, description ) result(key)
    character(len=*)               :: name
    real(kind=kind(1.0d0)), target :: pvar
    character(len=*)               :: description
    type(keyvar_data)              :: key

    key%section     =  '*'
    key%name        =  name
    key%description =  description
    key%double_var    => pvar
end function keyvar_double

function keyvar_char( name, pvar, description ) result(key)
    character(len=*)         :: name
    character(len=*), target :: pvar
    character(len=*)         :: description
    type(keyvar_data)        :: key

    key%section     =  '*'
    key%name        =  name
    key%description =  description
    key%char_var    => pvar
end function keyvar_char

function keyvar_log( name, pvar, description ) result(key)
    character(len=*)  :: name
    logical, target   :: pvar
    character(len=*)  :: description
    type(keyvar_data) :: key

    key%section     =  '*'
    key%name        =  name
    key%description =  description
    key%log_var    => pvar
end function keyvar_log

function keyvar_int_section( section, name, pvar, description ) result(key)
    character(len=*)  :: section, name
    integer, target   :: pvar
    character(len=*)  :: description
    type(keyvar_data) :: key

    key%section     =  section
    key%name        =  name
    key%description =  description
    key%int_var     => pvar
end function keyvar_int_section

function keyvar_real_section( section, name, pvar, description ) result(key)
    character(len=*)  :: section, name
    real, target      :: pvar
    character(len=*)  :: description
    type(keyvar_data) :: key

    key%section     =  section
    key%name        =  name
    key%description =  description
    key%real_var    => pvar
end function keyvar_real_section

function keyvar_double_section( section, name, pvar, description ) result(key)
    character(len=*)               :: section, name
    real(kind=kind(1.0d0)), target :: pvar
    character(len=*)               :: description
    type(keyvar_data)              :: key

    key%section     =  '*'
    key%name        =  name
    key%description =  description
    key%double_var    => pvar
end function keyvar_double_section

function keyvar_char_section( section, name, pvar, description ) result(key)
    character(len=*)         :: section, name
    character(len=*), target :: pvar
    character(len=*)         :: description
    type(keyvar_data)        :: key

    key%section     =  section
    key%name        =  name
    key%description =  description
    key%char_var    => pvar
end function keyvar_char_section

function keyvar_log_section( section, name, pvar, description ) result(key)
    character(len=*)  :: section, name
    logical, target   :: pvar
    character(len=*)  :: description
    type(keyvar_data) :: key

    key%section     =  section
    key%name        =  name
    key%description =  description
    key%log_var    => pvar
end function keyvar_log_section

! handle_command_arguments --
!     Private routine to handle the command-line arguments
!
! Arguments:
!     filename             Name of the input file to use (possibly overwritten)
!     args                 Definition of the key-variable pairs
!
subroutine handle_command_arguments( filename, args )
    character(len=*)                :: filename
    type(keyvar_data), dimension(:) :: args

    integer                         :: i, ncmd
    character(len=20)               :: option

    ncmd = command_argument_count()
    do i = 1,ncmd
        call get_command_argument( i, option )
        select case ( option )
            case( '-?', '/?', '-help', '--help', '-h' )
                call display_help( args )
                stop

            case( '-i', '-input', '--input' )
                if ( i < ncmd ) then
                    call get_command_argument( i+1, filename )
                    exit
                else
                    write(*,'(a)') 'No input file name given for option ', option
                    stop
                endif

            case( '-o', '-output', '--output' )
                if ( i < ncmd ) then
                    call get_command_argument( i+1, filename )
                    call write_input_file( filename, args )
                    write(*,'(a)') 'Sample output file written: ', trim(filename)
                    stop
                else
                    write(*,'(a)') 'No input file name given for option ', option
                    stop
                endif
        end select
    enddo
end subroutine handle_command_arguments

! display_help --
!     Display simple online help, including the input parameters
!
! Arguments:
!     args                 "Descriptors" of the input parameters
!
subroutine display_help( args )
    type(keyvar_data), dimension(:) :: args

    integer                         :: i, width

    write(*,'(a)') 'Online help:'
    write(*,'(a)') '- The program takes an input file containing key-value pairs'
    write(*,'(a)') '- You can specify the name of that file using the command option "-input filename"'
    write(*,'(a)') '- You can get an example of such a file using the command option "-output filename"'
    write(*,'(a)') ' '
    write(*,'(a)') 'Key words and their meaning:'

    width = maxval( len_trim( args%name ) )
    do i = 1,size(args)
        write(*,'(a,4x,a)') args(i)%name(1:width), trim(args(i)%description)
    enddo

end subroutine display_help

! write_input_file --
!     Write a dummy input file
!
! Arguments:
!     filename             Name of the file to write
!     args                 "Descriptors" of the input parameters
!
subroutine write_input_file( filename, args )
    character(len=*)                 :: filename
    type(keyvar_data), dimension(:)  :: args

    integer                          :: i, j, ierr, lun, width
    character(len=len(args%section)) :: section
    logical, dimension(size(args))   :: handled

    open( newunit = lun, file = filename, status = 'new', iostat = ierr )

    if ( ierr /= 0 ) then
        write(*,'(2a)') 'Given file already exists: ', trim(filename)
        write(*,'(2a)') 'Please specify a different one'
        stop
    endif

    write(lun,'(a)') '# Key words and their meaning:'

    width = maxval( len_trim( args%name ) )
    do i = 1,size(args)
        write(lun,'(2a,4x,a)') '# ', args(i)%name(1:width), trim(args(i)%description)
    enddo

    handled = .false.
    do i = 1,size(args)
        if ( args(i)%section == '*' ) then
            handled(i) = .true.
            write(lun,'(2a,4x,a)') args(i)%name(1:width), ' = ...'
        endif
    enddo
    do i = 1,size(args)
        if ( .not. handled(i) ) then
            section = args(i)%section
            write(lun,'(3a)') '[', trim(args(i)%section), ']'

            do j = i,size(args)
                if ( args(j)%section == section .and. .not. handled(j) ) then
                    handled(j) = .true.
                    write(lun,'(2a,4x,a)') args(j)%name(1:width), ' = ...'
                endif
            enddo
        endif
    enddo

end subroutine write_input_file

end module keyvars
