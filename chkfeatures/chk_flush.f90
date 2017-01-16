! chk_flush --
!     Check: does the compiler support the flush statement?
!
!     Note:
!     It is fairly difficult to make sure in an automatic way that the
!     flush statement is actually doing its job. Hence merely check
!     that the compiler accepts the statement.
!
program chk_flush
    implicit none

    real, dimension(10) :: x

    call random_number( x )

    open( 10, file = 'chk_flush.out' )
    write( 10, * ) x
    flush( 10 )

    write( *, '(a)' ) 'Nothing conclusive, but the FLUSH statement is accepted'

end program chk_flush
