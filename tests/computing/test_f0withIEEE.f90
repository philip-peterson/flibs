! test_f0withIEEE.f90 --
!     Test program for the leadzero module - version: with IEEE support for NaN and Inf
!
!     By J F Harper, Mathematics, Victoria University, Wellington, NZ 10 Jan 2012.
!
program tryf0
  use leadzero, only: p1,p2,zero1,zero2,f0,ispace,rspace
  implicit none
  integer  :: p1space = range(1.0_p1)+4 ! space for -huge(1.0) in F0.0
  character:: fmt*9 = "(A,T20,A," ! used in reveal and printline below
  character:: cx(3)*14 ='-NAN +INF -INF'
  real(p1) :: p1x(6) = (/-zero1,huge(zero1),0.2_p1,zero1,zero1,zero1/)
  real(p2) :: p2x(6) = (/-zero2,huge(zero2),0.2_p2,zero2,zero2,zero2/)
  integer  :: i,ios1,ios2,n
  read(cx,*,iostat=ios1) (p1x(i),i=4,6)
  read(cx,*,iostat=ios2) (p2x(i),i=4,6)
  if (ios1/=0) print *,'Error reading real(kind(1.0)) array p1x.'
  if (ios2/=0) print *,'Error reading real(kind(1d0)) array p2x.'
  if (ios1/=0.or.ios2/=0) stop &
       "NAN or INF didn't work. Try f0noIEEE.f90 instead of f0withIEEE.f90"
  do n = 0,2,2
     call revealp1(p1x,n)
     call revealp2(p2x,n)
  end do
contains

  subroutine revealp1(   x,n)
    real(p1),intent(in)::x(:)
    integer,intent(in) ::  n
    integer  :: i
    print "(/,2(A,I0))", &
         ' Single precision p1space+abs(n) = ',p1space+abs(n),' n = ',n
    do i = 1,size(x)
       print "(A,ES17.6E4,A,I0)", &
            ' x in ES17.6E4 format = ', x(i), ' n = ',n
       print fmt//"I0)", ' My f0(x,n)', &
            '= "'//f0(x(i),n)//'" len = ',len(f0(x(i),n))
       call p1printline(x(i),abs(n),"SS")
       print fmt//"I0)", ' My f0(x,n,''LZ''  )', &
            '= "'//f0(x(i),n,'LZ')//'" len = ',len(f0(x(i),n,'LZ'))
       print fmt//"I0)", ' My f0(x,n,''SP''  )', &
            '= "'//f0(x(i),n,'SP')//'" len = ',len(f0(x(i),n,'SP'))
       call p1printline(x(i),abs(n),"SP")
       print fmt//"I0)", ' My f0(x,n,''LZSP'')', &
            '= "'//f0(x(i),n,'LZSP')//'" len = ',len(f0(x(i),n,'LZSP'))
    end do
  end subroutine revealp1

  subroutine revealp2(x,n)
    real(p2),intent(in) :: x(:)
    integer,intent(in)  :: n
    integer  :: i
    print "(/,2(A,I0))", &
         ' Double precision rspace+abs(n) = ',rspace+abs(n),' n = ',n
    do i = 1,size(x)
       print "(A,ES17.6E4,A,I0)", &
            ' x in ES17.6E4 format = ', x(i), ' n = ',n
       print fmt//"I0)", ' My f0(x,n)', &
            '= "'//f0(x(i),n)//'" len = ',len(f0(x(i),n))
       call p2printline(x(i),abs(n),"SS")
       print fmt//"I0)", ' My f0(x,n,''LZ''  )', &
            '= "'//f0(x(i),n,'LZ')//'" len = ',len(f0(x(i),n,'LZ'))
       print fmt//"I0)", ' My f0(x,n,''SP''  )', &
            '= "'//f0(x(i),n,'SP')//'" len = ',len(f0(x(i),n,'SP'))
       call p2printline(x(i),abs(n),"SP")
       print fmt//"I0)", ' My f0(x,n,''LZSP'')', &
            '= "'//f0(x(i),n,'LZSP')//'" len = ',len(f0(x(i),n,'LZSP'))
    end do
  end subroutine revealp2

  subroutine p1printline(  x,n,edit)
    real(p1),intent(in)::  x
    integer,intent(in) ::    n
    character,intent(in)::     edit*(*)
    character nstring*(ispace), line*(p1space+abs(n)+25)
    write(nstring,"(I0)") n
    write(line,fmt//edit//",F0."//trim(nstring)//",A)") &
         ' Compiler''s '//edit,'= "',x,'"'
    print "(2A,I0)", &
         trim(line),' len = ',index(line,'"',.true.)-index(line,'"')-1
  end subroutine p1printline

  subroutine p2printline(  x,n,edit)
    real(p2),intent(in)::  x
    integer,intent(in) ::    n
    character,intent(in)::     edit*(*)
    character nstring*(ispace), line*(rspace+abs(n)+25)
    write(nstring,"(I0)") n
    write(line,fmt//edit//",F0."//trim(nstring)//",A)") &
         ' Compiler''s '//edit,'= "',x,'"'
    print "(2A,I0)", &
         trim(line),' len = ',index(line,'"',.true.)-index(line,'"')-1
  end subroutine p2printline

end program tryf0
