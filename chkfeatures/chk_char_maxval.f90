! chk_char_maxval --
!     Check: does the compiler support the maxval/minval/maxloc/minloc functions for character strings?
!
program chk_char_maxval
    implicit none

    character(len=5), dimension(5) :: array = (/ 'abcde', 'abc  ', 'ABCDE', 'AAAAA', 'aBcde' /)

    write( *, '(a,5a6)' ) 'Array of character strings:', array
    write( *, '(a,a)' )     '    Maximum value: ', maxval(array)
    write( *, '(a,i0)' )    '    Location:      ', maxloc(array)
    write( *, '(a,a)' )     '    Minimum value: ', minval(array)
    write( *, '(a,i0)' )    '    Location:      ', minloc(array)
end program chk_char_maxval
