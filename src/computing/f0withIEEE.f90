! Simulate F0 format with more options, for compilers implementing NaN, Inf.
! Fortran 95/2003 module and test program in this file f0withIEEE.f90 by
! J F Harper, Mathematics, Victoria University, Wellington, NZ 20 Jan 2012.
!
! Compilers vary in their own treatments of leading zero and optional + sign
! because Fortran standards f90,f95,f2003 differ, and f95 was ambiguous. The
! module lets users choose their own treatment. The test program assumes
! that NAN and INF are valid inputs to a READ statement for a real variable,
! as f2003 requires for processors that support IEEE 754 features. If your
! processor does not support them, use f0noIEEE.f90 instead of this
! program. The modules in f0noIEEE.f90 and fotest.f90 are identical but
! their test programs differ. IEEE intrinsic modules are not used.
!
! Usage: function f0(x,n,s) or f0(x,n) returns the shortest possible string
! of characters containing x in F0.n format, with leading zero and + sign
! controlled by the character string s if it is present, or by SS,F0.n
! format as in f2003 if not, so if the processor supports separate positive
! and negative zeros then f0(-0.0,n) shall begin with a - sign. Arguments:
!
! x: value to be written by f0 [double precision or default real]
! n: number of digits to be written after the decimal point [integer]
! s: specifying treatment of + sign and leading zero [character(*)]
!
! If s is absent or contains neither LZ nor SP the effect is that of format
! SS,F0.n as in the f2003 standard. If s contains SP the effect is that of
! SP,F0.n, i.e. a + sign if x>0 or x is +0.0, a - sign if x<0 or -0.0.
! If the processor does not support separate -0.0 and +0.0 then 0.0 is
! treated as +0.0. A zero before the decimal point is inserted if the output
! would otherwise have had no digits at all, or if s contains LZ and there
! would otherwise be no digits before the decimal point. If x is infinite or
! not a number, f0 returns the appropriate one of +Inf,Inf,-Inf,NaN.
!
! Public entities defined in this module are
! four integer parameters: p1 = kind(1.0), p2 = kind(1d0),
!     ispace = space needed for -huge(1) in I0 format,
!     rspace = space needed for -huge(1d0) in F0.0 format,
! two real parameters: zero1 = 0 of kind p1, zero2 = 0 of kind p2.
! generic character function: f0 with four private specific forms, for real
!     kinds p1 and p2, and s present or absent.
!
module leadzero
  implicit none
  private
  public p1,p2,zero1,zero2,ispace,rspace,f0
  integer,parameter :: &
       p2 = kind(1d0), &           ! double precision kind
       p1 = kind(1.0), &           ! default real (single precision) kind
       ispace = range(1) + 2, &    ! space needed for -huge(1) in I0 format
       rspace = range(1d0) + 4     ! space for -huge(real) in F0.0
  real(p1),parameter:: zero1 = 0
  real(p2),parameter:: zero2 = 0
  interface f0 ! no optional args in spec.expr. so 4 procedures here
     module procedure p2f0short, p1f0short, p2f0no_s, p1f0no_s
  end interface f0

contains

  pure function p1f0long(x,n,s) result(out)
    real(p1),intent(in)::x
    integer,intent(in) ::  n  ! using f0.n format
    character(*),intent(in)::s! for leading zero, SP options
    character(rspace+abs(n))::         out
! so len(out) = max possible space for x in f0.n format
    integer  :: ios
    character:: fmt*(7+2*ispace)
     out = ' '
    if (n<0) then
       out = '*** Error in f0: n<0 ***'
    else
       write(fmt,"(A,I0,A,I0,A)")'(SS,F',len(out),'.',n,')' ! SS: no + sign
       write(out,fmt,iostat=ios) x
       if(ios/=0) then
          out = '*** Error in f0: reading x ***'
       else
          out = fixstring(s,out,sign(1.0_p1,x)<zero1.and.x==zero1)
       end if
    end if
  end function p1f0long

  pure function p2f0long(x,n,s) result(out)
    real(p2),intent(in)::x
    integer,intent(in) ::  n  ! using f0.n format
    character(*),intent(in)::s! for leading zero, SP options
    character(rspace+abs(n))::         out
! so len(out) = max possible space for x in f0.n format
    integer  :: ios
    character:: fmt*(7+2*ispace)
    out = ' '
    if (n<0) then
       out = '*** Error n<0 ***'
    else
       write(fmt,"(A,I0,A,I0,A)")'(SS,F',len(out),'.',n,')' ! SS: no + sign
       write(out,fmt,iostat=ios) x
       if(ios/=0) then
          out = '*** Error reading x ***'
       else
          out = fixstring(s,out,sign(1.0_p2,x)<zero2.and.x==zero2)
       end if
    end if
  end function p2f0long

  pure function fixstring(   s,in,neg0) result(out)
    character(*),intent(in)::s,in
    logical,intent(in)::neg0 ! true if x is negative zero
    character(len(in))::                       out
    logical:: usingsp
    usingsp = (index(s,'SP')>0)
    out=adjustl(in)
    if (scan(out,'Aa')>0) then ! x is not a number
       out = 'NaN'
    else if(scan(out,'Ii')>0) then ! x is infinite
       out = merge('-Inf', &
            merge('+Inf','Inf ',usingsp),scan(out,'-')>0)// &
            repeat(' ',len(out)-4)
    else
       if (neg0.and.scan(out,'-')==0) out = '-'//out
! If s contains 'LZ' then insert leading zero; if not, delete it
       if (index(s,'LZ')>0) then
          if (out(1:2)=='-.' ) out = '-0'//out(2:)
          if (out(1:1)=='.'  ) out =  '0'//out
       else
          if (out(1:3)=='-0.') out =  '-'//out(3:)
          if (out(1:2)=='0.' ) out =       out(2:)
! Ensure at least one digit is written (n might be 0)
          if(trim(out)=='.'  ) out = '0.'
          if(trim(out)=='-.' ) out = '-0.'
       end if
! If s contains 'SP' then insert + sign if not negative
       if (usingsp.and.scan(out,'-')==0) out = '+'//out
    end if
  end function fixstring

  pure function p1f0short(x,n,s) ! trimmed invocation of p1f0long
    real(p1),intent(in) ::x
    integer,intent(in)  ::  n
    character,intent(in)::    s*(*)
    character:: p1f0short*(len_trim(p1f0long(x,n,s)))
    p1f0short = trim(p1f0long(x,n,s))
  end function p1f0short

  pure function p2f0short(x,n,s) ! trimmed invocation of p2f0long
    real(p2),intent(in) ::x
    integer,intent(in)  ::  n
    character,intent(in)::    s*(*)
    character:: p2f0short*(len_trim(p2f0long(x,n,s)))
    p2f0short = trim(p2f0long(x,n,s))
  end function p2f0short

  pure function p1f0no_s(x,n) ! like p1f0short with s absent
    real(p1),intent(in)::x
    integer,intent(in) ::  n
    character:: p1f0no_s*(len_trim(p1f0long(x,n,' ')))
    p1f0no_s = trim(p1f0long(x,n,' '))
  end function p1f0no_s

  pure function p2f0no_s(x,n) ! like p2f0short with s absent
    real(p2),intent(in)::x
    integer,intent(in) ::  n
    character:: p2f0no_s*(len_trim(p2f0long(x,n,' ')))
    p2f0no_s = trim(p2f0long(x,n,' '))
  end function p2f0no_s

end module leadzero
