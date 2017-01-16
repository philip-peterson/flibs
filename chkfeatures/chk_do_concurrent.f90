! chk_do_concurrent.f90 --
!     Check: does the compiler support the DO CONCURRENT construct?
!
!     Note:
!     Even if the compiler supports this, it does not mean that the do-loop
!     is acutally run in parallel
!
program chk_do_concurrent
    implicit none

    integer :: i
    real, dimension(1000) :: array

    do concurrent (i=1:size(array))
        array(i) = exp(i/1000.0)
    enddo

    write( *, '(a)' ) 'The DO CONCURRENT construct is supported'
end program chk_do_concurrent
