! chk_public_private --
!     Check: does the compiler support the public entities of a private type?
!
module entities
     implicit none

     private

     type keyword
         private
         character(len=10) :: word
     end type keyword

     type(keyword), parameter, public :: true = keyword('true')
     type(keyword), parameter, public :: false = keyword('false')

     public :: write_keyword
contains
subroutine write_keyword( word )
    type(keyword), intent(in) :: word

    write( *, '(2a)' ) '    Keyword: ', trim(word%word)
end subroutine write_keyword
end module entities

program chk_public_private
    use entities

    implicit none

    write( *, '(a)' ) 'Available keywords:'
    call write_keyword( true  )
    call write_keyword( false )
end program chk_public_private
