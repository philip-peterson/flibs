! buildscript.f90 --
!     Auxiliary program to write the batchfile and shell script that
!     run the specific checking programs
!
program buildscript
    implicit none

    integer :: ierr, k
    integer :: label
    integer :: lunin  = 10
    integer :: lunbat = 21
    integer :: lunscr = 22

    integer :: number_features    = 0
    integer :: number_diagnostics = 0
    integer :: number_probes      = 0
    integer :: number_extensions  = 0

    character(len=80) :: line
    character(len=20) :: type_program

    open( lunin, file = 'buildscript.set', status = 'old' , iostat = ierr )

    if ( ierr /= 0 ) then
        write(*,'(a)') 'Error opening the buildscript.set file - terminating this step'
        stop
    endif

    open( lunbat, file = 'runfeatures.bat' )
    open( lunscr, file = 'runfeatures.sh' )

    write( lunbat, '(a)' ) '@echo off'
    write( lunscr, '(a)' ) '#!/bin/sh'

    label = 0

    do
        read( lunin, '(a)', iostat = ierr ) line

        if ( ierr /= 0 ) then
            exit
        endif

        !
        ! Remove "carriage return" - some compilers do not remove it
        ! automatically
        !
        k = index( line, char(13) )
        if ( k > 0 ) then
            line(k:) = ' '
        endif

        !
        ! Handle the contents
        !
        if ( line(1:1) == '#' ) then
            cycle
        endif

        if ( line == ' ' ) then
            write( lunbat, '(a,i0)'  ) ':skip', label
            write( lunscr, '(a)'     ) 'fi'
            cycle
        endif

        if ( line(1:1) == '@' ) then
            read(  lunin, * ) type_program
            select case ( type_program )
                case( 'FEATURE' )
                    number_features = number_features + 1
                case( 'DIAGNOSTIC' )
                    number_diagnostics = number_diagnostics + 1
                case( 'EXTENSION' )
                    number_extensions = number_extensions + 1
                case( 'PROBE' )
                    number_probes = number_probes + 1
                case default
                    write( *, '(2a)' ) 'Error: unknown program type - ', trim(type_program)
            end select

            write( lunbat, '(a,a)' ) 'echo .'
            write( lunscr, '(a,a)' ) 'echo .'
            write( lunbat, '(a,a)' ) 'echo Check program: ', trim(line(2:))
            write( lunscr, '(a,a)' ) 'echo Check program: ', trim(line(2:))
            write( lunbat, '(a,a)' ) 'echo -------------'
            write( lunscr, '(a,a)' ) 'echo -------------'
            write( lunbat, '(a,a)' ) 'echo .'
            write( lunscr, '(a,a)' ) 'echo .'
            write( lunbat, '(3a)'  ) 'call compile ', trim(line(2:)), ' %1 %2 %3 %4 %5 %6 %7 %8'
            write( lunscr, '(3a)' ) './compile ', trim(line(2:)), ' $1 $2 $3 $4 $5 $6 $7 $8'
            write( lunbat, '(a,a)' ) 'echo .'
            write( lunscr, '(a,a)' ) 'echo .'

            label = label + 1
            write( lunbat, '(3a,i0)' ) 'if not exist ', trim(line(2:)), '.exe goto next', label
            write( lunbat, '(2a)'    ) trim(line(2:)), '.exe'
            write( lunbat, '(a,i0)'  ) 'goto skip', label
            write( lunbat, '(a,i0)'  ) ':next', label

            write( lunscr, '(3a)'    ) 'if [ -f ', trim(line(2:)), '.exe ]; then'
            write( lunscr, '(3x,3a)' ) './', trim(line(2:)), '.exe'
            write( lunscr, '(a)'     ) 'else'
        else
            write( lunbat, '(a,a)'   ) '   echo ', trim(line)
            write( lunscr, '(3a)'    ) '   echo ''', trim(line), ''''
        endif

    enddo

    write( lunbat, '(a,i0)'  ) ':skip', label
    write( lunscr, '(a)'  ) 'fi'

    write( *, '(/,a,i0)' ) 'Number of tests to run: ', label
    write( *, '(a,i0)' )   '    Number of feature checks:                 ', number_features
    write( *, '(a,i0)' )   '    Number of diagnostic checks:              ', number_diagnostics
    write( *, '(a,i0)' )   '    Number of checks on behaviour/properties: ', number_probes
    write( *, '(a,i0)' )   '    Number of checks on compiler extensions:  ', number_extensions
end program buildscript
