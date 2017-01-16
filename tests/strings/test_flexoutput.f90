! test_flexoutput.f90 --
!     Test program for the module flexoutput
!
program test_flexoutput
    use flexoutput

    open( 10, file = 'test_flexoutput.out' )

    call write_items( 10, 'The value of {2} is {1}, as expected', [ item(1), item('Name') ] )
    call write_items( 10, 'The value of {2} is {1}, as a single-precision real', [ item(1.1), item('Name') ] )
    call write_items( 10, 'as a double-precision {1} instead', [ item(1.1d0), item('Name') ] )

    write(*,*) 'Check the file "test_flexoutput.out"'
end program test_flexoutput
