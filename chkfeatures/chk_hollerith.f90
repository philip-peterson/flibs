! chk_hollerith --
!     Check: does the compiler warn about holleriths?
!
program chk_hollerith
    implicit none

    write( *, '(a)' ) 'Holleriths are an ancient device for I/O of characters.'
    write( *, '(a)' ) 'They should not be used anymore'
    write( *, 1000  )

1000 format(57HThe compiler supports them - but does it warn about them?)
    !          123456789012345678901234567890123456789012345678901234567

end program chk_hollerith
