! test_dynlib.f90 --
!     Test the module for dealing with dynamic libraries
!
program test_dynlib
    use dynamic_libraries

    implicit none

    type(dynamic_library) :: dynlib
    logical               :: success
    procedure(), pointer  :: proc

    !
    ! Try the Windows version
    !
    call load_library( dynlib, 'test_lib.dll', success )

!   write(*,*) dynlib
!   call get_procedure( dynlib, 'write_string', proc, success )
!   call proc( "SUCCESS!" )
!
!   stop

    if ( .not. success ) then
        !
        ! Try the Linux version
        !
        call load_library( dynlib, 'test_lib.so', success )
    endif

    if ( .not. success ) then
        write(*,*) 'Could not load the library'
    else
       call get_procedure( dynlib, 'write_string_', proc, success )
        if ( .not. success ) then
            write(*,*) 'Could not load the procedure "write_string_"'
        else
           call proc( "SUCCESS!" )
        endif
    endif
end program test_dynlib
