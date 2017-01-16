! chk_param_derived --
!     Check: does the compiler support parameterised derived types?
!
program chk_param_derived
    implicit none

    type matrix(real_kind,m,n)
        integer, kind :: real_kind
        integer, len  :: m, n
        real(kind=real_kind) :: values(m,n)
    end type matrix

    type(matrix(kind(1.0),10,20)) :: m

    write( *, '(a)' )         'Matrix type:'
    write( *, '(a,i0)' )      '    Kind:  ', m%real_kind
    write( *, '(a,i0,a,i0)' ) '    Shape: ', m%m, 'x', m%n

end program chk_param_derived
