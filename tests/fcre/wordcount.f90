! program wordcount x
! wordcount.f90 --
!     Small program to demonstrate the use of the FCRE library
!     for matching strings to regular expressions
!
!     The program reads the given file, extracts the words
!     (defined as sequences of one or letters) and prints
!     a histogram
!
program wordcount
    use mod_regex
    implicit none

    type(Fregex) :: re

    character(len=80)             :: filename
    character(len=:), allocatable :: line         !<== allocatable should not be required - hm, required for "replace"
    integer                       :: ierr
    integer                       :: stat
    integer                       :: lines
    integer                       :: m
    integer                       :: offset
    integer                       :: start_index
    integer                       :: matched_length
    integer, dimension(11)        :: length

    !
    ! Create the regular expression:
    ! - A sequence of one or more letters
    ! - Case is not important
    !
    call re%setPattern( "(?i)[a-z]+" )

    call get_command_argument( 1, filename )
    open( 10, file = filename, status = 'old' )

    lines  = 0
    length = 0

    allocate( character(len=80):: line )

    do
        read( 10, '(a)', iostat = ierr ) line
        write(*,*) 'Input: ',line
        if ( ierr /= 0 ) then
            exit
        endif

        offset = 1
        do
            write(*,*) 'Offset:', offset, '>', trim(line(offset:))
            call re%match( line(offset:), start_index, matched_length, status=stat )
            write(*,*) 'Status:', stat

            if ( stat == 0 ) then
                write(*,*) line(offset+start_index-1:offset+start_index+matched_length-2)
            else
                write(*,*) 'Status:', stat
            endif

            !
            ! Was there a match?
            !
            if ( stat == 0 ) then
                lines     = lines + 1
                m         = min( matched_length, size(length) )
                length(m) = length(m) + 1

                offset    = offset + start_index + matched_length - 1
            else
                exit
            endif
        enddo
    enddo

    !
    ! Print the histogram
    !
    write( *, '(a,i0)' )     'Number of lines read: ', lines
    write( *, '(2a12)' )     'Word length', 'Number'
    write( *, '(2i12)' )     ( m, length(m), m = 1,size(length)-1)
    m = size(length)
    write( *, '(a,i11,i12)' ) '>', m, length(m)
end program
