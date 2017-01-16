! test_unsigned.f90 --
!     Test the implementation of unsigned integers
!
program test_unsigned

    use unsigned_integers

    implicit none

    integer           :: a
    integer           :: b
    integer           :: c
    integer           :: k
    integer           :: expected
    integer           :: errors
    integer           :: ierr
    logical           :: cmp
    character(len=20) :: stringa
    character(len=20) :: stringb
    character(len=20) :: stringc
    character(len=2)  :: dummy
    character(len=2)  :: operation
    character(len=80) :: string

    open( 10, file = "unsigned.test", iostat = ierr, status = 'old' )
    open( 11, file = "unsigned.out" )

    if ( ierr /= 0 ) then
        write( *, '(a)' ) "Could not open file ""unsigned.test"" - program stopped"
        stop
    endif

    errors = 0

    do
        read( 10, '(a)', iostat = ierr ) string

        if ( ierr < 0 ) then
            exit
        endif
        if ( ierr > 0 ) then
            write( *, '(a)' ) "Error reading the input file ""unsigned.test"" - program stopped"
            stop
        endif

        !
        ! Awkward property of list-directed input: / means end of input
        ! So substitute a different character!
        !
        k = index( string, '/' )
        if ( k > 0 ) then
            string(k:k) = '%'
        endif

        read( string, * ) stringa, operation, stringb, dummy, stringc

        a        = to_unsigned( stringa )
        b        = to_unsigned( stringb )
        expected = to_unsigned( stringc )

        if ( operation == '%' ) then
            operation = '/'
        endif

        write( 11, * ) a, ' -- ', to_string(a), ' -- ', stringa
        write( 11, * ) b, ' -- ', to_string(b), ' -- ', stringb
        write( 11, * ) expected, ' -- ', to_string(expected), ' -- ', stringc

        select case( operation )
            case( '+' )
                c = a + b

            case( '-' )
                c = a - b

            case( '*' )
                c = a * b

            case( '/' )
                c = a .udiv. b

            case( '>' )
                cmp = a .ugt. b
                c   = merge( 1, 0, cmp )

            case( '>=' )
                cmp = a .uge. b
                c   = merge( 1, 0, cmp )

            case( '>>' )
                c = ishft(a, -b)

            case( '<' )
                cmp = a .ult. b
                c   = merge( 1, 0, cmp )

            case( '<=' )
                cmp = a .ule. b
                c   = merge( 1, 0, cmp )

            case( '<<' )
                c = ishft( a, b )

            case default
                write( *, '(2a)') "Unknown operation: ", operation
                cycle

        end select

        if ( c /= expected ) then
            errors = errors + 1
            write( 11, '(20a)' ) 'Difference encountered: ', trim(stringa), ' ', operation, ' ', &
                trim(stringb), ' should have been: ', trim(stringc), ' -- found: ', to_string(c)
        else
            write( 11, '(10a)' ) trim(stringa), ' ', operation, ' ', trim(stringb), ' = ', to_string(c), '(as expected)'
        endif

    enddo

    if ( errors == 0 ) then
        write( *, '(a)' ) 'No errors found'
    else
        write( *, '(a,i0,a)' ) 'Note: ', errors, ' errors found'
    endif
end program
