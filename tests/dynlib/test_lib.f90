! test_lib.f90 --
!     Provide a simple dynamic library for testing
!
subroutine write_string( string )
    !dec$ attributes dllexport :: write_string
    implicit none

    character(len=*) :: string

    write(*,*) 'Write_string: ',trim(string)

end subroutine write_string

subroutine write_string_doubled( string )
    !dec$ attributes dllexport :: write_string_doubled
    implicit none

    character(len=*) :: string

    write(*,*) 'Write_string twice: ', trim(string), ' -- ', trim(string)

end subroutine write_string_doubled
