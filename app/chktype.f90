!  chktype.f90 --
 !
 !    The module chktype is meant to have the compiler check
 !    as much as possible if the terms in an expression all
 !    have the same precision. This can be used to find out
 !    if there are expressions like "2.1*x" with x a double
 !    precision value or "2.0*i/9" with i an integer.
 !
 !    Such statements cause serious problems:
 !    - the constant 2.1 in the first example is used as
 !      single precision whereas the expression as a whole
 !      will be of double precision. The value is, however,
 !      _not_ the same as 2.1d0*x.
 !    - Evaluating 2.0*i/9 may result in a wrong answer:
 !      set i to 1. If i/9 is computed first, then the result
 !      is 0, not 2.0/9= 0.2222...
 !
 !    The module is not intended for use in an actual program,
 !    it is merely meant for checking at run time
 !

module chktype
    type real_
        private
        real :: v
    endtype
    type double_
        private
        real(kind=kind(1.0d0)) :: v
    endtype
    type integer_
        private
        integer :: v
    endtype


    interface operator(+)
        module procedure add_real
        module procedure add_double
        module procedure add_integer
        module procedure add_real_tc
        module procedure add_double_tc
        module procedure add_integer_tc
        module procedure add_real_ct
        module procedure add_double_ct
        module procedure add_integer_ct
    end interface

    interface operator(-)
        module procedure sub_real
        module procedure sub_double
        module procedure sub_integer
        module procedure sub_real_tc
        module procedure sub_double_tc
        module procedure sub_integer_tc
        module procedure sub_real_ct
        module procedure sub_double_ct
        module procedure sub_integer_ct
    end interface

    interface operator(*)
        module procedure mult_real
        module procedure mult_double
        module procedure mult_integer
        module procedure mult_real_tc
        module procedure mult_double_tc
        module procedure mult_integer_tc
        module procedure mult_real_ct
        module procedure mult_double_ct
        module procedure mult_integer_ct
    end interface

    interface operator(/)
        module procedure div_real
        module procedure div_double
        module procedure div_integer
        module procedure div_real_tc
        module procedure div_double_tc
        module procedure div_integer_tc
        module procedure div_real_ct
        module procedure div_double_ct
        module procedure div_integer_ct
    end interface

    interface operator(**)
        module procedure expon_real
        module procedure expon_double
        module procedure expon_integer
        module procedure expon_real_tc
        module procedure expon_double_tc
        module procedure expon_integer_tc
        module procedure expon_real_ct
        module procedure expon_double_ct
        module procedure expon_integer_ct
    end interface

    interface operator(>)
        module procedure gt_real
        module procedure gt_double
        module procedure gt_integer
        module procedure gt_real_tc
        module procedure gt_double_tc
        module procedure gt_integer_tc
        module procedure gt_real_ct
        module procedure gt_double_ct
        module procedure gt_integer_ct
    end interface

    interface operator(<)
        module procedure lt_real
        module procedure lt_double
        module procedure lt_integer
        module procedure lt_real_tc
        module procedure lt_double_tc
        module procedure lt_integer_tc
        module procedure lt_real_ct
        module procedure lt_double_ct
        module procedure lt_integer_ct
    end interface

    interface operator(>=)
        module procedure ge_real
        module procedure ge_double
        module procedure ge_integer
        module procedure ge_real_tc
        module procedure ge_double_tc
        module procedure ge_integer_tc
        module procedure ge_real_ct
        module procedure ge_double_ct
        module procedure ge_integer_ct
    end interface

    interface operator(<=)
        module procedure le_real
        module procedure le_double
        module procedure le_integer
        module procedure le_real_tc
        module procedure le_double_tc
        module procedure le_integer_tc
        module procedure le_real_ct
        module procedure le_double_ct
        module procedure le_integer_ct
    end interface

    interface operator(==)
        module procedure eq_real
        module procedure eq_double
        module procedure eq_integer
        module procedure eq_real_tc
        module procedure eq_double_tc
        module procedure eq_integer_tc
        module procedure eq_real_ct
        module procedure eq_double_ct
        module procedure eq_integer_ct
    end interface

    interface operator(/=)
        module procedure ne_real
        module procedure ne_double
        module procedure ne_integer
        module procedure ne_real_tc
        module procedure ne_double_tc
        module procedure ne_integer_tc
        module procedure ne_real_ct
        module procedure ne_double_ct
        module procedure ne_integer_ct
    end interface

    interface assignment(=)
        module procedure assign_real
        module procedure assign_double
        module procedure assign_integer
        module procedure assign_real_const
        module procedure assign_double_const
        module procedure assign_integer_const
    end interface


contains


    elemental subroutine assign_real(x,y)
        type(real_), intent(in)  :: y
        type(real_), intent(out) :: x
        x%v = y%v
    end subroutine

    elemental subroutine assign_real_const(x,y)
        real, intent(in)       :: y
        type(real_), intent(out) :: x
        x%v = y
    end subroutine

    elemental subroutine assign_double(x,y)
        type(double_), intent(in)  :: y
        type(double_), intent(out) :: x
        x%v = y%v
    end subroutine

    elemental subroutine assign_double_const(x,y)
        real(kind=kind(1.0d0)), intent(in)       :: y
        type(double_), intent(out) :: x
        x%v = y
    end subroutine

    elemental subroutine assign_integer(x,y)
        type(integer_), intent(in)  :: y
        type(integer_), intent(out) :: x
        x%v = y%v
    end subroutine

    elemental subroutine assign_integer_const(x,y)
        integer, intent(in)       :: y
        type(integer_), intent(out) :: x
        x%v = y
    end subroutine

    elemental type(real_) function add_real(x,y)
        type(real_), intent(in) :: x,y
        add_real%v = x%v + y%v
    end function
    elemental type(real_) function add_real_tc(x,y)
        type(real_), intent(in) :: x
        real, intent(in)      :: y
        add_real_tc%v = x%v + y
    end function
    elemental type(real_) function add_real_ct(x,y)
        real, intent(in)      :: x
        type(real_), intent(in) :: y
        add_real_ct%v = x + y%v
    end function

    elemental type(double_) function add_double(x,y)
        type(double_), intent(in) :: x,y
        add_double%v = x%v + y%v
    end function
    elemental type(double_) function add_double_tc(x,y)
        type(double_), intent(in) :: x
        real(kind=kind(1.0d0)), intent(in)      :: y
        add_double_tc%v = x%v + y
    end function
    elemental type(double_) function add_double_ct(x,y)
        real(kind=kind(1.0d0)), intent(in)      :: x
        type(double_), intent(in) :: y
        add_double_ct%v = x + y%v
    end function

    elemental type(integer_) function add_integer(x,y)
        type(integer_), intent(in) :: x,y
        add_integer%v = x%v + y%v
    end function
    elemental type(integer_) function add_integer_tc(x,y)
        type(integer_), intent(in) :: x
        integer, intent(in)      :: y
        add_integer_tc%v = x%v + y
    end function
    elemental type(integer_) function add_integer_ct(x,y)
        integer, intent(in)      :: x
        type(integer_), intent(in) :: y
        add_integer_ct%v = x + y%v
    end function

    elemental type(real_) function sub_real(x,y)
        type(real_), intent(in) :: x,y
        sub_real%v = x%v - y%v
    end function
    elemental type(real_) function sub_real_tc(x,y)
        type(real_), intent(in) :: x
        real, intent(in)      :: y
        sub_real_tc%v = x%v - y
    end function
    elemental type(real_) function sub_real_ct(x,y)
        real, intent(in)      :: x
        type(real_), intent(in) :: y
        sub_real_ct%v = x - y%v
    end function

    elemental type(double_) function sub_double(x,y)
        type(double_), intent(in) :: x,y
        sub_double%v = x%v - y%v
    end function
    elemental type(double_) function sub_double_tc(x,y)
        type(double_), intent(in) :: x
        real(kind=kind(1.0d0)), intent(in)      :: y
        sub_double_tc%v = x%v - y
    end function
    elemental type(double_) function sub_double_ct(x,y)
        real(kind=kind(1.0d0)), intent(in)      :: x
        type(double_), intent(in) :: y
        sub_double_ct%v = x - y%v
    end function

    elemental type(integer_) function sub_integer(x,y)
        type(integer_), intent(in) :: x,y
        sub_integer%v = x%v - y%v
    end function
    elemental type(integer_) function sub_integer_tc(x,y)
        type(integer_), intent(in) :: x
        integer, intent(in)      :: y
        sub_integer_tc%v = x%v - y
    end function
    elemental type(integer_) function sub_integer_ct(x,y)
        integer, intent(in)      :: x
        type(integer_), intent(in) :: y
        sub_integer_ct%v = x - y%v
    end function

    elemental type(real_) function mult_real(x,y)
        type(real_), intent(in) :: x,y
        mult_real%v = x%v * y%v
    end function
    elemental type(real_) function mult_real_tc(x,y)
        type(real_), intent(in) :: x
        real, intent(in)      :: y
        mult_real_tc%v = x%v * y
    end function
    elemental type(real_) function mult_real_ct(x,y)
        real, intent(in)      :: x
        type(real_), intent(in) :: y
        mult_real_ct%v = x * y%v
    end function

    elemental type(double_) function mult_double(x,y)
        type(double_), intent(in) :: x,y
        mult_double%v = x%v * y%v
    end function
    elemental type(double_) function mult_double_tc(x,y)
        type(double_), intent(in) :: x
        real(kind=kind(1.0d0)), intent(in)      :: y
        mult_double_tc%v = x%v * y
    end function
    elemental type(double_) function mult_double_ct(x,y)
        real(kind=kind(1.0d0)), intent(in)      :: x
        type(double_), intent(in) :: y
        mult_double_ct%v = x * y%v
    end function

    elemental type(integer_) function mult_integer(x,y)
        type(integer_), intent(in) :: x,y
        mult_integer%v = x%v * y%v
    end function
    elemental type(integer_) function mult_integer_tc(x,y)
        type(integer_), intent(in) :: x
        integer, intent(in)      :: y
        mult_integer_tc%v = x%v * y
    end function
    elemental type(integer_) function mult_integer_ct(x,y)
        integer, intent(in)      :: x
        type(integer_), intent(in) :: y
        mult_integer_ct%v = x * y%v
    end function

    elemental type(real_) function div_real(x,y)
        type(real_), intent(in) :: x,y
        div_real%v = x%v / y%v
    end function
    elemental type(real_) function div_real_tc(x,y)
        type(real_), intent(in) :: x
        real, intent(in)      :: y
        div_real_tc%v = x%v / y
    end function
    elemental type(real_) function div_real_ct(x,y)
        real, intent(in)      :: x
        type(real_), intent(in) :: y
        div_real_ct%v = x / y%v
    end function

    elemental type(double_) function div_double(x,y)
        type(double_), intent(in) :: x,y
        div_double%v = x%v / y%v
    end function
    elemental type(double_) function div_double_tc(x,y)
        type(double_), intent(in) :: x
        real(kind=kind(1.0d0)), intent(in)      :: y
        div_double_tc%v = x%v / y
    end function
    elemental type(double_) function div_double_ct(x,y)
        real(kind=kind(1.0d0)), intent(in)      :: x
        type(double_), intent(in) :: y
        div_double_ct%v = x / y%v
    end function

    elemental type(integer_) function div_integer(x,y)
        type(integer_), intent(in) :: x,y
        div_integer%v = x%v / y%v
    end function
    elemental type(integer_) function div_integer_tc(x,y)
        type(integer_), intent(in) :: x
        integer, intent(in)      :: y
        div_integer_tc%v = x%v / y
    end function
    elemental type(integer_) function div_integer_ct(x,y)
        integer, intent(in)      :: x
        type(integer_), intent(in) :: y
        div_integer_ct%v = x / y%v
    end function

    elemental type(real_) function expon_real(x,y)
        type(real_), intent(in) :: x,y
        expon_real%v = x%v ** y%v
    end function
    elemental type(real_) function expon_real_tc(x,y)
        type(real_), intent(in) :: x
        real, intent(in)      :: y
        expon_real_tc%v = x%v ** y
    end function
    elemental type(real_) function expon_real_ct(x,y)
        real, intent(in)      :: x
        type(real_), intent(in) :: y
        expon_real_ct%v = x ** y%v
    end function

    elemental type(double_) function expon_double(x,y)
        type(double_), intent(in) :: x,y
        expon_double%v = x%v ** y%v
    end function
    elemental type(double_) function expon_double_tc(x,y)
        type(double_), intent(in) :: x
        real(kind=kind(1.0d0)), intent(in)      :: y
        expon_double_tc%v = x%v ** y
    end function
    elemental type(double_) function expon_double_ct(x,y)
        real(kind=kind(1.0d0)), intent(in)      :: x
        type(double_), intent(in) :: y
        expon_double_ct%v = x ** y%v
    end function

    elemental type(integer_) function expon_integer(x,y)
        type(integer_), intent(in) :: x,y
        expon_integer%v = x%v ** y%v
    end function
    elemental type(integer_) function expon_integer_tc(x,y)
        type(integer_), intent(in) :: x
        integer, intent(in)      :: y
        expon_integer_tc%v = x%v ** y
    end function
    elemental type(integer_) function expon_integer_ct(x,y)
        integer, intent(in)      :: x
        type(integer_), intent(in) :: y
        expon_integer_ct%v = x ** y%v
    end function

    elemental logical function gt_real(x,y)
        type(real_), intent(in) :: x,y
        gt_real = x%v > y%v
    end function
    elemental logical function gt_real_tc(x,y)
        type(real_), intent(in) :: x
        real, intent(in)      :: y
        gt_real_tc = x%v > y
    end function
    elemental logical function gt_real_ct(x,y)
        real, intent(in)      :: x
        type(real_), intent(in) :: y
        gt_real_ct = x > y%v
    end function

    elemental logical function gt_double(x,y)
        type(double_), intent(in) :: x,y
        gt_double = x%v > y%v
    end function
    elemental logical function gt_double_tc(x,y)
        type(double_), intent(in) :: x
        real(kind=kind(1.0d0)), intent(in)      :: y
        gt_double_tc = x%v > y
    end function
    elemental logical function gt_double_ct(x,y)
        real(kind=kind(1.0d0)), intent(in)      :: x
        type(double_), intent(in) :: y
        gt_double_ct = x > y%v
    end function

    elemental logical function gt_integer(x,y)
        type(integer_), intent(in) :: x,y
        gt_integer = x%v > y%v
    end function
    elemental logical function gt_integer_tc(x,y)
        type(integer_), intent(in) :: x
        integer, intent(in)      :: y
        gt_integer_tc = x%v > y
    end function
    elemental logical function gt_integer_ct(x,y)
        integer, intent(in)      :: x
        type(integer_), intent(in) :: y
        gt_integer_ct = x > y%v
    end function

    elemental logical function lt_real(x,y)
        type(real_), intent(in) :: x,y
        lt_real = x%v < y%v
    end function
    elemental logical function lt_real_tc(x,y)
        type(real_), intent(in) :: x
        real, intent(in)      :: y
        lt_real_tc = x%v < y
    end function
    elemental logical function lt_real_ct(x,y)
        real, intent(in)      :: x
        type(real_), intent(in) :: y
        lt_real_ct = x < y%v
    end function

    elemental logical function lt_double(x,y)
        type(double_), intent(in) :: x,y
        lt_double = x%v < y%v
    end function
    elemental logical function lt_double_tc(x,y)
        type(double_), intent(in) :: x
        real(kind=kind(1.0d0)), intent(in)      :: y
        lt_double_tc = x%v < y
    end function
    elemental logical function lt_double_ct(x,y)
        real(kind=kind(1.0d0)), intent(in)      :: x
        type(double_), intent(in) :: y
        lt_double_ct = x < y%v
    end function

    elemental logical function lt_integer(x,y)
        type(integer_), intent(in) :: x,y
        lt_integer = x%v < y%v
    end function
    elemental logical function lt_integer_tc(x,y)
        type(integer_), intent(in) :: x
        integer, intent(in)      :: y
        lt_integer_tc = x%v < y
    end function
    elemental logical function lt_integer_ct(x,y)
        integer, intent(in)      :: x
        type(integer_), intent(in) :: y
        lt_integer_ct = x < y%v
    end function

    elemental logical function ge_real(x,y)
        type(real_), intent(in) :: x,y
        ge_real = x%v >= y%v
    end function
    elemental logical function ge_real_tc(x,y)
        type(real_), intent(in) :: x
        real, intent(in)      :: y
        ge_real_tc = x%v >= y
    end function
    elemental logical function ge_real_ct(x,y)
        real, intent(in)      :: x
        type(real_), intent(in) :: y
        ge_real_ct = x >= y%v
    end function

    elemental logical function ge_double(x,y)
        type(double_), intent(in) :: x,y
        ge_double = x%v >= y%v
    end function
    elemental logical function ge_double_tc(x,y)
        type(double_), intent(in) :: x
        real(kind=kind(1.0d0)), intent(in)      :: y
        ge_double_tc = x%v >= y
    end function
    elemental logical function ge_double_ct(x,y)
        real(kind=kind(1.0d0)), intent(in)      :: x
        type(double_), intent(in) :: y
        ge_double_ct = x >= y%v
    end function

    elemental logical function ge_integer(x,y)
        type(integer_), intent(in) :: x,y
        ge_integer = x%v >= y%v
    end function
    elemental logical function ge_integer_tc(x,y)
        type(integer_), intent(in) :: x
        integer, intent(in)      :: y
        ge_integer_tc = x%v >= y
    end function
    elemental logical function ge_integer_ct(x,y)
        integer, intent(in)      :: x
        type(integer_), intent(in) :: y
        ge_integer_ct = x >= y%v
    end function

    elemental logical function le_real(x,y)
        type(real_), intent(in) :: x,y
        le_real = x%v <= y%v
    end function
    elemental logical function le_real_tc(x,y)
        type(real_), intent(in) :: x
        real, intent(in)      :: y
        le_real_tc = x%v <= y
    end function
    elemental logical function le_real_ct(x,y)
        real, intent(in)      :: x
        type(real_), intent(in) :: y
        le_real_ct = x <= y%v
    end function

    elemental logical function le_double(x,y)
        type(double_), intent(in) :: x,y
        le_double = x%v <= y%v
    end function
    elemental logical function le_double_tc(x,y)
        type(double_), intent(in) :: x
        real(kind=kind(1.0d0)), intent(in)      :: y
        le_double_tc = x%v <= y
    end function
    elemental logical function le_double_ct(x,y)
        real(kind=kind(1.0d0)), intent(in)      :: x
        type(double_), intent(in) :: y
        le_double_ct = x <= y%v
    end function

    elemental logical function le_integer(x,y)
        type(integer_), intent(in) :: x,y
        le_integer = x%v <= y%v
    end function
    elemental logical function le_integer_tc(x,y)
        type(integer_), intent(in) :: x
        integer, intent(in)      :: y
        le_integer_tc = x%v <= y
    end function
    elemental logical function le_integer_ct(x,y)
        integer, intent(in)      :: x
        type(integer_), intent(in) :: y
        le_integer_ct = x <= y%v
    end function

    elemental logical function eq_real(x,y)
        type(real_), intent(in) :: x,y
        eq_real = x%v == y%v
    end function
    elemental logical function eq_real_tc(x,y)
        type(real_), intent(in) :: x
        real, intent(in)      :: y
        eq_real_tc = x%v == y
    end function
    elemental logical function eq_real_ct(x,y)
        real, intent(in)      :: x
        type(real_), intent(in) :: y
        eq_real_ct = x == y%v
    end function

    elemental logical function eq_double(x,y)
        type(double_), intent(in) :: x,y
        eq_double = x%v == y%v
    end function
    elemental logical function eq_double_tc(x,y)
        type(double_), intent(in) :: x
        real(kind=kind(1.0d0)), intent(in)      :: y
        eq_double_tc = x%v == y
    end function
    elemental logical function eq_double_ct(x,y)
        real(kind=kind(1.0d0)), intent(in)      :: x
        type(double_), intent(in) :: y
        eq_double_ct = x == y%v
    end function

    elemental logical function eq_integer(x,y)
        type(integer_), intent(in) :: x,y
        eq_integer = x%v == y%v
    end function
    elemental logical function eq_integer_tc(x,y)
        type(integer_), intent(in) :: x
        integer, intent(in)      :: y
        eq_integer_tc = x%v == y
    end function
    elemental logical function eq_integer_ct(x,y)
        integer, intent(in)      :: x
        type(integer_), intent(in) :: y
        eq_integer_ct = x == y%v
    end function

    elemental logical function ne_real(x,y)
        type(real_), intent(in) :: x,y
        ne_real = x%v /= y%v
    end function
    elemental logical function ne_real_tc(x,y)
        type(real_), intent(in) :: x
        real, intent(in)      :: y
        ne_real_tc = x%v /= y
    end function
    elemental logical function ne_real_ct(x,y)
        real, intent(in)      :: x
        type(real_), intent(in) :: y
        ne_real_ct = x /= y%v
    end function

    elemental logical function ne_double(x,y)
        type(double_), intent(in) :: x,y
        ne_double = x%v /= y%v
    end function
    elemental logical function ne_double_tc(x,y)
        type(double_), intent(in) :: x
        real(kind=kind(1.0d0)), intent(in)      :: y
        ne_double_tc = x%v /= y
    end function
    elemental logical function ne_double_ct(x,y)
        real(kind=kind(1.0d0)), intent(in)      :: x
        type(double_), intent(in) :: y
        ne_double_ct = x /= y%v
    end function

    elemental logical function ne_integer(x,y)
        type(integer_), intent(in) :: x,y
        ne_integer = x%v /= y%v
    end function
    elemental logical function ne_integer_tc(x,y)
        type(integer_), intent(in) :: x
        integer, intent(in)      :: y
        ne_integer_tc = x%v /= y
    end function
    elemental logical function ne_integer_ct(x,y)
        integer, intent(in)      :: x
        type(integer_), intent(in) :: y
        ne_integer_ct = x /= y%v
    end function

end module chktype

program aha
    use chktype

    type(real_) :: x
    type(real_) :: y

    x = 1.1
    y = 1.0d0
    x = 11 * y
end program

