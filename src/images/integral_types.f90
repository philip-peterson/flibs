! integral_types.f90 --
!     Define integer parameters that select integers of specified size
!     Used by the bmp_file module, as BMP files use single-byte and double-byte
!     integers, as well four-byte integers.
!
!     Note:
!     To make sure that the integer kinds are of the right size, a check is made
!     against the number of bits. Unfortunately MERGE() is not accepted by all
!     compilers in a constant expression. Hence resort to a purely arithmetic expression
!
!     Note on testing:
!     - Tested with different number of bits (9 for one_byte)
!     - Tested with unavailable kind (precision of 100)
!     As the arithmetic expression is fairly complex, this is needed.
!
module integral_types
    implicit none

    private

    integer, parameter         :: one_byte_ = selected_int_kind(2)
    integer, parameter, public :: one_byte  = &
        one_byte_ - (max(one_byte_,0)+1) * abs(bit_size(0_one_byte_)-8)

    integer, parameter         :: two_bytes_ = selected_int_kind(4)
    integer, parameter, public :: two_bytes  = &
        two_bytes_ - (max(two_bytes_,0)+1) * abs(bit_size(0_two_bytes_)-16)

    integer, parameter         :: four_bytes_ = selected_int_kind(8)
    integer, parameter, public :: four_bytes  = &
        four_bytes_ - (max(four_bytes_,0)+1) * abs(bit_size(0_four_bytes_)-32)

    ! If the compiler does not support any of these kinds, then it will not be possible to
    ! define variables with these kinds
    !
    ! integer(one_byte)   :: test_one_byte
    ! integer(two_bytes)  :: test_two_bytes
    ! integer(four_bytes) :: test_four_bytes
      integer(one_byte)   :: test_one_byte
      integer(two_bytes)  :: test_two_bytes
      integer(four_bytes) :: test_four_bytes
end module integral_types
