! test_translation.f90 --
!     Test program for the translation module
!
program test_translation
    use translation

    character(len=10), dimension(4) :: lang = ['EN', 'NL', 'DE', 'FR']
    character(len=40), dimension(5) :: key  = ['FileNotFound               ', &
                                               'ErrorReadingFile           ', &
                                               'AQuotedStringShouldWorkFine', &
                                               'BackupMessage              ', &
                                               'Unknown                    ']
    character(len=80) :: text

    integer :: i, j
    logical :: found

    do j = 1,size(lang)
        write(*,*) ' '
        write(*,*) 'Language: ', lang(j)
        call set_language( lang(j) )
        do i = 1,size(key)
            call get_text( key(i), text, found )
            write(*,*) trim(text), ' -- ', found
        enddo
    enddo
end program test_translation
