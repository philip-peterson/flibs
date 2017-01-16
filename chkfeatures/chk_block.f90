! chk_block.f90 --
!     Check: does the compiler support the BLOCK/ENDBLOCK construct?
!
program chk_block
    implicit none

    integer :: i

    i = -1
    block
        integer :: i
        do i = 1,2
            write(*,'(a,i5)') 'In BLOCK: ', i
        enddo
    end block

    write(*,'(a,i5)') 'Outside the block: ', i
end program chk_block
