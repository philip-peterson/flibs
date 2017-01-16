! chk_round_io --
!     Check: does the compiler support the ROUND/SIGN specifiers?
!
program chk_round_io
    implicit none

    real    :: value

    write( *, '(a)' )    'Writing the number 1.5 with ROUND and SIGN specifiers'

    value = 1.5

    write( *, '(a,f10.4)', sign  = 'plus' ) '    Value (+ expected):   ', value
    write( *, '(a,f10.0)', round = 'up' )   '    Value (rounded up):   ', value
    write( *, '(a,f10.0)', round = 'down' ) '    Value (rounded down): ', value
    write( *, '(a,f10.0)' )                 '    Value (no rounding):  ', value

    value = -1.5
    write( *, '(a)' )    'Writing the number -1.5 with ROUND specifiers'
    write( *, '(a,f10.0)', round = 'up' )   '    Value (rounded up):   ', value
    write( *, '(a,f10.0)', round = 'up' )   '    Value (rounded down): ', value
    write( *, '(a,f10.0)' )                 '    Value (no rounding):  ', value

end program chk_round_io
