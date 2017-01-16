! textstr_func.f90 --
!     Manipulate multiline_text variables
!
!     TODO:
!     Create version that accepts a class variable, so that it becomes
!     easier to parametrise the filter routine
!
module multiple_line_text_functions
    use multiple_line_text

    implicit none

    integer, parameter :: mltxt_insert_after_text     = 1
    integer, parameter :: mltxt_insert_after_mltxt    = 2
    integer, parameter :: mltxt_insert_before_text    = 3
    integer, parameter :: mltxt_insert_before_mltxt   = 4
    integer, parameter :: mltxt_delete_current        = 5
    integer, parameter :: mltxt_replace_text          = 6
    integer, parameter :: mltxt_replace_mltxt         = 7
    integer, parameter :: mltxt_exit                  = 100 ! May be combined with other actions
    integer, parameter :: mltxt_continue              = 0

    interface mltxt_iterate
        module procedure mltxt_iterate_sub
    end interface

contains
! mltxt_iterate_sub --
!     Iterate over the lines of a multiline text variable and take action
!     in accordance with the user-supplied "filter" routine
!
! Arguments:
!     text        Multiline text variable to be examined
!     user_sub    Subroutine implementing the filter
!     first       (Optional) the first line to be examined
!     last        (Optional) the last line to be examined
!
subroutine mltxt_iterate_sub( text, user_sub, first, last )
    type(multiline_text) :: text
    integer, optional    :: first, last

    interface
        subroutine user_sub( pos, string, action, text_str, mltxt_str )
            import text_string, multiline_text
            integer, intent(in)          :: pos
            character(len=*), intent(in) :: string
            integer                      :: action
            type(text_string)            :: text_str
            type(multiline_text)         :: mltxt_str
        end subroutine user_sub
    end interface

    integer                    :: i, pos, dir, offset
    integer                    :: firstn, lastn
    integer                    :: length
    integer                    :: action
    type(text_string)          :: text_str
    type(multiline_text)       :: mltxt_str
    type(text_string), pointer :: txtp

    firstn = 1
    lastn  = mltxt_number(text)

    if ( present(first) ) then
        firstn = first
    endif

    if ( present(first) ) then
        lastn = last
    endif

    dir = 1
    if ( firstn > lastn ) then
        dir = -1
    endif

    offset = 0
    do i = firstn, lastn, dir
        pos = i + offset

        call mltxt_get( text, pos, txtp )
        length = txt_length( txtp )
        call mltxt_examine_string( pos, txtp, length, user_sub, action, text_str, mltxt_str )

        select case ( mod(action,mltxt_exit) )
            case ( mltxt_delete_current )
                call mltxt_delete( text, pos )
                offset = offset - dir

            case ( mltxt_insert_after_text )
                call mltxt_insert( text, pos, text_str )
                offset = offset + dir

            case ( mltxt_insert_after_mltxt )
                call mltxt_insert( text, pos, mltxt_str )
                offset = offset + dir * mltxt_number(mltxt_str)

            case ( mltxt_insert_before_text )
                call mltxt_insert( text, pos-1, text_str )
                offset = offset + dir

            case ( mltxt_insert_before_mltxt )
                call mltxt_insert( text, pos-1, mltxt_str )
                offset = offset + dir * mltxt_number(mltxt_str)

            case ( mltxt_replace_text )
                call mltxt_delete( text, pos )
                call mltxt_insert( text, pos, text_str )

            case ( mltxt_replace_mltxt )
                call mltxt_delete( text, pos )
                call mltxt_insert( text, pos, mltxt_str )
                offset = offset + mltxt_number(mltxt_str) - dir

            case default
                ! Nothing to do here
        end select

        if ( action/mltxt_exit == 1 ) then
            exit
        endif
    enddo

contains
subroutine mltxt_examine_string( pos, text, length, user_sub, action, text_str, mltxt_str )
    integer               :: pos
    type(text_string)     :: text
    integer               :: length
    integer               :: action
    type(text_string)     :: text_str
    type(multiline_text)  :: mltxt_str

    interface
        subroutine user_sub( pos, string, action, text_str, mltxt_str )
            import text_string, multiline_text
            integer, intent(in)          :: pos
            character(len=*), intent(in) :: string
            integer                      :: action
            type(text_string)            :: text_str
            type(multiline_text)         :: mltxt_str
        end subroutine user_sub
    end interface

    character(len=length) :: string

    call txt_to_string( text, string )
    call user_sub( pos, string, action, text_str, mltxt_str )
end subroutine mltxt_examine_string
end subroutine mltxt_iterate_sub

end module multiple_line_text_functions
