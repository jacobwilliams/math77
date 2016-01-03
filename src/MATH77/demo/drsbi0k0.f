c     program DRSBI0K0
c>> 1996-05-28 DRSBI0K0 Krogh Moved formats up.
c>> 1994-10-19 DRSBI0K0 Krogh  Changes to use M77CON
c>> 1992-03-18 DRSBI0K0 CLL Added "c" to "program" line above.
c>> 1990-11-21 WVS (edited by CLL)
c     Demonstration driver for single precision hyperbolic Bessel
c     function subprograms.
c
c     Compute the Wronskian relation
c
c     z = x * (I1(x)*K0(x) + I0(x)*K1(x)) - 1.0
c
c     z should be approximately zero.
c
c     ------------------------------------------------------------------
c--S replaces "?": DR?BI0K0, ?BI0K0, ?BI1K1
c     ------------------------------------------------------------------
      real             X, BI0, BK0, BI1, BK1, Z
      integer IX, INFO
c
100   format (3x,a1,7x,4(a5,8x),a1/3x,'-',7x,4('-----',8x),'-')
150   format (1x,f4.1,1x,2(f12.8,1x),4x,'INFINITY',5x,'INFINITY')
200   format (1x,f4.1,1x,4(f12.8,1x),1x,g9.2)
c
      print 100, 'X', 'I0(X)','I1(X)','K0(X)','K1(X)','Z'
c
      x = 0.0e0
      call sbi0k0 (x, bi0, bk0, 1, info)
      call sbi1k1 (x, bi1, bk1, 1, info)
      print 150, x, bi0, bi1
      do 300 ix = 5, 50, 5
         x = ix / 10.0e0
         call sbi0k0 (x, bi0, bk0, -3, info)
         call sbi1k1 (x, bi1, bk1, -3, info)
         z = x * (bi1*bk0 + bi0*bk1) - 1.0e0
         print 200, x, bi0, bi1, bk0, bk1, z
300   continue
      stop
      end
