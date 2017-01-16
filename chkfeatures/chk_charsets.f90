! chk_charsets --
!     Check: what standard character sets does the compiler support?
!
program chk_charsets
    implicit none

    write( *, '(a)' )       'Character sets (kinds):'
    write( *, '(a,i0)' )    '    Default:   ', selected_char_kind('DEFAULT')
    write( *, '(a,i0,2a)' ) '    ASCII:     ', selected_char_kind('ASCII'), ' - ', &
                            merge('supported    ','not supported', selected_char_kind('ASCII') > 0)
    write( *, '(a,i0,2a)' ) '    ISO_10646: ', selected_char_kind('ISO_10464'), ' - ', &
                            merge('supported    ','not supported', selected_char_kind('ISO_10646') > 0)

end program chk_charsets
