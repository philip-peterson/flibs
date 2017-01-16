! translation.f90 --
!     Module to provide "localisation" support.
!
!     Note:
!     This version is rather basic - just provides a facility to look up
!     strings via a keyword and a "language".
!
module translation
    implicit none

    private
    public  :: set_language, get_text

    type text_entry
        character(len=40)  :: key
        character(len=10)  :: lang
        character(len=160) :: text
    end type text_entry

!
! The file 'translation.inc' should specify the set of strings forming the
! translation 'database'. It can be generated via the mktranslation program
!
! Note:
! The include file is supposed to define the default language (default_lang)
! and the array of text entries (entry(*))
!

include 'translation.inc'

    character(len=10), save :: preferred_lang = '?'

contains

! set_language --
!     Set the preferred language and the default language for translations
!
! Arguments:
!     lang             Abbreviation of the preferred language, such as 'EN'
!     default_lang     (Optional) abbreviation of the fall-back language
!
subroutine set_language( lang, default )
    character(len=*), intent(in)           :: lang
    character(len=*), intent(in), optional :: default

    preferred_lang = lang
    if ( present(default) ) then
        default_lang   = default
    endif
end subroutine set_language

! get_text --
!     Get the translation in the requested language
!
! Arguments:
!     keyword          Keyword by which to look up the text
!     text             Actual text found
!     found            (Optional) whether a text was found or not
!
subroutine get_text( keyword, text, found )
    character(len=*), intent(in)   :: keyword
    character(len=*), intent(out)  :: text
    logical, optional, intent(out) :: found

    logical :: found_local

    if ( preferred_lang == '?' ) then
        preferred_lang = default_lang
    endif

    call get_text_impl( preferred_lang, keyword, text, found_local )

    if ( .not. found_local ) then
        call get_text_impl( default_lang, keyword, text, found_local )
        if ( .not. found_local ) then
            text = keyword
        endif
    endif

    if ( present(found) ) then
        found = found_local
    endif
end subroutine get_text

! get_text_impl --
!     Get the translation in the requested language - actual implementation
!
! Arguments:
!     lang             Requested language
!     keyword          Keyword by which to look up the text
!     text             Actual text found
!     found            (Optional) whether a text was found or not
!
subroutine get_text_impl( lang, keyword, text, found )
    character(len=*), intent(in)   :: lang
    character(len=*), intent(in)   :: keyword
    character(len=*), intent(out)  :: text
    logical, intent(out)           :: found

    integer :: i

    found = .false.

    do i = 1,size(entry)
        if ( entry(i)%key == keyword .and. entry(i)%lang == lang ) then
            found = .true.
            text  = entry(i)%text
            exit
        endif
    enddo
end subroutine get_text_impl

end module translation
