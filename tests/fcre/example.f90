! Program example
!
! Small program to test and tinker with the FCRE library.
program example
    use mod_regex
    use fcre
    implicit none

    type(FRegex), allocatable :: re

    integer, parameter :: BUFFER_LENGTH = 1000
    character(BUFFER_LENGTH) :: buffer
    character(:), allocatable :: subject, replace, pattern, input_subject, versionString, name
    integer :: i, l, stat, c
    type(FString), dimension(:), allocatable :: captures
    integer, dimension(:), allocatable :: i_all, l_all

!
! Input parameters
!
    call get_command_argument(1, buffer)
    allocate(character(len_trim(buffer)) :: pattern)
    pattern = trim(buffer)

    call get_command_argument(2, buffer)
    allocate(character(len_trim(buffer)) :: subject)
    subject = trim(buffer)

    call get_command_argument(3, buffer)
    allocate(character(len_trim(buffer)) :: replace)
    replace = trim(buffer)

    if (command_argument_count() > 3) then
        call get_command_argument(4, buffer)
        allocate(character(len_trim(buffer)) :: name)
        name = trim(buffer)
    else
        allocate(character(0) :: name)
    end if


    ! Print the version info
    call c_f_string(pcre_version(), versionString)
    write(*,*) "PCRE version: ", trim(versionString)

    ! Setup the regular expression
    allocate(re)
    call re%setPattern(pattern)

    write(*,*)
    write(*,*) "******************************************************************************"
    write(*,*) "*    Simple matching tests                                                   *"
    write(*,*) "******************************************************************************"

   call re%match(subject=subject,  &
                   index=i,        &
           capture_array=captures, &
                  length=l,        &
                  status=stat)

    if (stat < 0) then
        write(*,*) "Error while matching: ", fcre_error_message(stat)
    else if (stat > 0) then
        write(*,*) "No match found"
    else
        write(*,*) "Match found at position:", i, "length:", l
    end if

    write(*,*)
    write(*,*) "Captured strings:", re%captureCount

    do c=1, re%captureCount
        write(*,*) "  " // captures(c)%string
    end do


    ! Test the named string capture <NYI>
    write(*,*)
    write(*,*) "Named captures:", re%namedCaptureCount
    do c=1, re%namedCaptureCount
    end do

    ! Test the substitution
    write(*,*)
    write(*,*) "Substitution"
    !re = FRegex(pattern, replace)
    re = fcre_new_regex(pattern, replace) ! FRegex not accepted by Intel Fortran
    ! Keep a copy of the initial string
    input_subject = subject
    call re%replace(subject=subject,       &
                      index=i,             &
                     length=l,             &
                     status=stat)
    write(*,*) "Modified string: "//subject
    subject = input_subject

    write(*,*)
    write(*,*) "******************************************************************************"
    write(*,*) "*    Case insensitive tests                                                  *"
    write(*,*) "******************************************************************************"

    ! Test the case insensitive matching
    call re%setCaseSensitive(.false.)
    write(*,*) '1'
    call re%setPattern(pattern)
    write(*,*) '2'

    call re%match(subject=subject,  &
                    index=i,        &
                   length=l,        &
            capture_array=captures, &
                   status=stat)
    write(*,*) '3'

    if (stat < 0) then
        write(*,*) "Error while matching: ", fcre_error_message(stat)
    else if (stat > 0) then
        write(*,*) "No match found"
    else
        write(*,*) "Match found at position:", i, "length:", l
    end if

    write(*,*)
    write(*,*) "Captured strings:", re%captureCount


    do c=1, re%captureCount
        write(*,*) "  " // captures(c)%string
    end do


    ! Test the named string capture <NYI>
    write(*,*)
    write(*,*) "Named captures:", re%namedCaptureCount
    do c=1, re%namedCaptureCount

    end do

    ! Test the substitution
    write(*,*)
    write(*,*) "Substitution"
    ! Keep a copy of the initial string
    input_subject = subject
    call re%replace(subject=subject,       &
                      index=i,             &
                     length=l,             &
                     status=stat)
    write(*,*) "Modified string: "//subject
    subject = input_subject

    call re%setCaseSensitive(.true.)
    call re%setPattern(pattern) ! Recompile the pattern
    write(*,*)
    write(*,*) "******************************************************************************"
    write(*,*) "*    Global mode tests                                                       *"
    write(*,*) "******************************************************************************"
    call re%setGlobal(.true.)
    call re%match(subject, i, captures, l, stat)


    if (stat < 0) then
        write(*,*) "Error while matching: ", fcre_error_message(stat)
    else if (stat > 0) then
        write(*,*) "No match found"
    end if

    do while(i /= 0)
        write(*,*) "Match found at position:", i, "length:", l

        write(*,*)
        write(*,*) "Captured strings:", re%captureCount

        do c=1, re%captureCount
            write(*,*) "  " // captures(c)%string
        end do
        write(*,*)

        call re%match(subject, i, captures, l)

    end do
    call re%setGlobal(.false.)
!
! Multiple match
!
    write(*,*)
    write(*,*) "******************************************************************************"
    write(*,*) "*    match_all tests                                                         *"
    write(*,*) "******************************************************************************"
    write(*,*)
    write(*,*) "Standard"
    call re%match_all(subject=subject, &
                        index=i_all,   &
                       length=l_all,   &
                       status=stat)

    if (stat < 0) then
        write(*,*) "Error while matching: ", fcre_error_message(stat)
    else if (stat > 0) then
        write(*,*) "No match found"
    else
        write(*,*) "Matches"
        write(*,*) "Positions:", i_all
        write(*,*) "Lengths:", l_all
    end if


    ! Test the substitution
    write(*,*)
    write(*,*) "Substitution"
    ! Keep a copy of the initial string
    input_subject = subject
    call re%replace_all(subject=subject,       &
                          index=i_all,         &
                         length=l_all,         &
                         status=stat)
    write(*,*) "Modified string: "//subject
    subject = input_subject


    write(*,*)
    write(*,*) "******************************************************************************"
    write(*,*) "*    Case insensitive match_all tests                                        *"
    write(*,*) "******************************************************************************"
    write(*,*)
    write(*,*) "Standard"
    call re%setCaseSensitive(.false.)

    call re%match_all(subject=subject, &
                        index=i_all,   &
                       length=l_all,   &
                       status=stat)

    if (stat < 0) then
        write(*,*) "Error while matching: ", fcre_error_message(stat)
    else if (stat > 0) then
        write(*,*) "No match found"
    else
        write(*,*) "Matches"
        write(*,*) "Positions:", i_all
        write(*,*) "Lengths:", l_all
    end if


    ! Test the substitution
    write(*,*)
    write(*,*) "Substitution"
    ! Keep a copy of the initial string
    input_subject = subject
    call re%replace_all(subject=subject,       &
                          index=i_all,         &
                         length=l_all,         &
                         status=stat)
    write(*,*) "Modified string: "//subject
    subject = input_subject
end program
