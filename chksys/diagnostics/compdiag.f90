! compdiag.f90 --
!     Program to analyse the behaviour of the compiler
!
program compdiag
    implicit none

    type summary
        integer :: success_count
        integer :: total_count
        logical :: error_expected   ! If the compiler complains, is that good or not?
        character(len=20) :: category
    end type summary

    type(summary), dimension(7) :: result = &
        (/ summary( 0, 0, .true.,  'basic'      ), &
           summary( 0, 0, .true.,  'medium'     ), &
           summary( 0, 0, .true.,  'advanced'   ), &
           summary( 0, 0, .false., 'limitation' ), &
           summary( 0, 0, .false., 'extension'  ), &
           summary( 0, 0, .false., 'f2003'      ), &
           summary( 0, 0, .false., 'other'      ) /)

    logical            :: exists

    !
    ! Do we need to do anything?
    !
    inquire( file = 'compdiag.complete', exist = exists )

    if ( exists ) then
        call print_summary
        stop
    endif

    !
    ! Analyse the compiler output or prepare a new test program
    !
    inquire( file = 'check.out', exist = exists )

    if ( exists ) then
        call analyse
    else
        call prepare
    endif

contains

subroutine prepare
    integer :: count
    integer :: casecnt
    integer :: ierr
    logical :: found
    character(len=132) :: line

    found = .false.

    open( 10, file = 'compdiag.count', status = 'old', iostat = ierr )
    if ( ierr == 0 ) then
        read( 10, * ) casecnt
        close( 10, status = 'delete' )
    else
        casecnt = 1
    endif

    open( 10, file = 'compdiag.inp', status = 'old' )

    count = 0
    do
        read( 10, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        if ( index( line, '@stop' ) == 1 ) then
            exit  ! Terminate the testing - for debugging only
        endif

        if ( index( line, '@desc' ) == 1 ) then
            count = count + 1

            if ( count == casecnt ) then
                found = .true.
                exit
            endif
        endif
    enddo

    ! We have found a new test case:
    ! copy it to file


    if ( found ) then
        write(*,'(2a)') 'Testing: ', trim(adjustl(line(6:)))

        open( 21, file = 'compdiag.test' )
        open( 22, file = 'check.f90' )

        write( 21, * ) count
        write( 21, '(a)' ) line

        do
            read( 10, '(a)', iostat = ierr ) line
            if ( ierr /= 0 ) exit

            if ( index( line, '@desc' ) == 1 ) then
                exit
            endif

            if ( index( line, '@' ) == 1 ) then
                write( 21, '(a)' ) trim(line)
            else
                write( 22, '(a)' ) trim(line)
            endif
        enddo

        close( 21 )
        close( 22 )
    else
        open( 21, file = 'compdiag.complete' )
        close( 21 )
    endif
end subroutine prepare

subroutine analyse
    integer :: count
    integer :: i
    integer :: idx
    integer :: ierr
    character(len=132) :: description
    character(len=132) :: category
    character(len=132) :: line
    logical :: exists
    logical :: failure
    logical :: empty

    open( 10, file = 'compdiag.test' )
    open( 11, file = 'check.out' )
    open( 12, file = 'check.f90' )
    open( 21, file = 'compdiag.log',     position = 'append' )
    open( 22, file = 'compdiag.summary', position = 'append' )

    !
    ! Retrieve the previous scores and the information on the
    ! current test
    !
    open( 13, file = 'compdiag.score', status = 'old', iostat = ierr )
    if ( ierr == 0 ) then
        do i = 1,size(result)
            read( 13, * ) result(i)%success_count, result(i)%total_count
        enddo
        close( 13 )
    else
        write( 21, '(a,/)' ) 'Testing compiler diagnostics'
        write( 22, '(a,/)' ) 'Testing compiler diagnostics', &
                             'Success   Category  Description'
    endif

    read( 10, *     ) count
    read( 10, '(a)' ) description
    read( 10, '(a)', iostat = ierr ) category
    if ( ierr /= 0 ) then
        category = 'other'
    endif
    close( 10 )

    open( 23, file = 'compdiag.count' )
    write( 23, * ) count + 1
    close( 23 )

    !
    ! Analyse the output file
    !
    inquire( file = 'compdiag.error', exist = exists )

    failure = .false.
    if ( exists ) then
        open( 13, file = 'compdiag.error' )
        close( 13, status = 'delete' )
        failure = .true.
    else
        call examine_compiler_output( failure )
    endif

    idx = size(result)
    do i = 1,size(result)
        if ( adjustl(category(10:)) == result(i)%category ) then
            idx = i
            exit
        endif
    enddo

    result(idx)%total_count = result(idx)%total_count + 1
    if ( failure .eqv. result(idx)%error_expected ) then
        result(idx)%success_count = result(idx)%success_count + 1
    endif

    write( 21, '(1x)' )
    write( 21, '(2a)' ) 'Testing:  ', trim(adjustl(description(6:)))
    write( 21, '(2a)' ) 'Category: ', trim(adjustl(category(10:)))
    write( 21, '(1x)' )

    write( 22, '(2x,a5,3x,a10,a)' ) &
        merge( 'Yes','No ', failure .eqv. result(idx)%error_expected ), &
            adjustl(category(10:)), description(6:)

    write( 21, '(a)' ) 'Code for this test:'
    do
        read( 12, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        write( 21, '(a)' ) trim(line)
    enddo
    close( 12 )

    write( 21, '(/,a)' ) 'Report from the compiler:'
    empty = .true.
    do
        read( 11, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit
        empty = .false.

        write( 21, '(a)' ) trim(line)
    enddo

    if ( empty ) then
        write( 21, '(a)' ) '-- no output --'
    endif
    write( 21, '(1x)' )

    !
    ! Save the new score
    !
    open( 12, file = 'compdiag.score' )
    do i = 1,size(result)
        write( 12, * ) result(i)%success_count, result(i)%total_count
    enddo
    close( 12 )

    !
    ! Clean up
    !
    close( 11, status = 'delete' )

end subroutine analyse

subroutine print_summary
    integer :: i
    integer :: lun
    integer :: ierr

    open( 13, file = 'compdiag.score', status = 'old', iostat = ierr )
    if ( ierr == 0 ) then
        do i = 1,size(result)
            read( 13, * ) result(i)%success_count, result(i)%total_count
        enddo
        close( 13 )
    else
        write(*,*) 'No score file found - no summary!'
        return
    endif

    open( 21, file = 'compdiag.log',     position = 'append' )
    open( 22, file = 'compdiag.summary', position = 'append' )

    do lun = 21,22
        write( lun, '(//,a)' ) 'Success   Total     Category'

        do i = 1,size(result)
            write( lun, '(2(i5,5x),a)' ) &
                result(i)%success_count, result(i)%total_count, &
                result(i)%category
        enddo
    enddo

    close( 21 )
    close( 22 )
end subroutine print_summary

subroutine examine_compiler_output( failure )
    logical, intent(inout)                :: failure

    character(len=200)                    :: line
    integer                               :: i
    integer                               :: k
    integer                               :: ierr
    character(len=20), dimension(6), save :: keyword = &
    (/ 'Error',    'ERROR',   'error',   &
       'Warningr', 'WARNING', 'warning'  /)

read_file: &
    do
        read( 11, '(a)', iostat = ierr ) line
        if ( ierr /= 0 ) exit

        do i = 1,size(keyword)
            k = index( line, trim(keyword(i)) )
            if ( k > 0 ) then
                failure = .true.
                exit read_file
            endif
        enddo
    enddo &
read_file

    rewind( 11 )

end subroutine examine_compiler_output

end program compdiag
