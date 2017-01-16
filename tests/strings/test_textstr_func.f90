! test_textstr_func.f90 --
!     Test program for multline text manipulation
!

! Example of a filter
!
module filter_text
    use multiple_line_text_functions

    implicit none

contains

subroutine no_comments( pos, string, action, text_str, mltxt_str )
    integer, intent(in)          :: pos
    character(len=*), intent(in) :: string
    integer                      :: action
    type(text_string)            :: text_str
    type(multiline_text)         :: mltxt_str

    action = mltxt_continue
    if ( index( string, '!' ) == 1 ) then
        action = mltxt_delete_current
    endif
end subroutine no_comments

end module filter_text

! main --
!     Main program that illustrates the use
!
program test_functions
    use multiple_line_text_functions
    use filter_text

    type(multiline_text) :: text
    logical              :: error

    call mltxt_read_file( "test_textstr_func.f90", text, error )

    call mltxt_iterate( text, no_comments )

    call mltxt_write_file( text, "no_comments.txt", error )

end program test_functions
