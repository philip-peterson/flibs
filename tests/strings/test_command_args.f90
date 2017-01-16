! test_command_args.f90 --
!     Test program for the module command_args
!
!     Usage examples:
!     test_command_args --integer 1 | -i 1
!     test_command_args --real 1.0  | -r 1.0
!     test_command_args -lmn        | -l -m -n | --lval --mval | --nval
!
program test_command_args
    use command_args

    implicit none

    integer            :: i
    integer            :: intvar
    real               :: realvar
    logical            :: l_var, m_var, n_var

    character(len=200) :: string

    intvar  = -999
    realvar = -999.0

    call handle_command_options( [optarg(intvar,  opt_value_next, 'i', 'integer', 'Integer value'),  &
                                  optarg(realvar, opt_value_next, 'r', 'real', 'Real value'),        &
                                  optarg(l_var,   opt_true,       'l', 'lval', 'Logical value "L"'), &
                                  optarg(m_var,   opt_true,       'm', 'mval', 'Logical value "M"'), &
                                  optarg(n_var,   opt_true,       'n', 'nval', 'Logical value "N"')] )

    !
    ! Report the values
    !
    write(*,*) 'Values set'
    write(*,*) '   intvar  = ', intvar
    write(*,*) '   realvar = ', realvar
    write(*,*) '   l_var   = ', l_var
    write(*,*) '   m_var   = ', m_var
    write(*,*) '   n_var   = ', n_var

    !
    ! Report which values were not examined
    !
    do i = 1,command_argument_count()
        if ( argument_ignored(i) ) then
            call get_command_argument(i, string )
            write(*,'(a,i0,2a)') 'Argument ', i, ' was ignored: ', trim(string)
        endif
    enddo
end program test_command_args
