! chk_complex.f90 --
!     Check: does the compiler support the %RE and %IM syntax for complex numbers?
!
program chk_complex
    implicit none

    complex :: z

    z%re = 1.0
    z%im = 1.0

    write( *, '(a,2f7.4)'      ) 'Complex number 1+i:  ', z
    write( *, '(a,f7.4,a,f.4)' ) 'Real component:      ', z%re, ' -- ', real(z)
    write( *, '(a,f7.4,a,f.4)' ) 'Imaginary component: ', z%im, ' -- ', imag(z)
end program chk_complex
