      program DRDERF
c>> 1996-06-17 DRDERF Krogh Minor change in formats for C conversion.
c>> 1994-10-19 DRDERF Krogh  Changes to use M77CON
c>> 1992-05-13 DRDERF CLL
c>> 1991-10-16 DRDERF CLL add demo of DERFCE
c>> 1987-12-09 DRDERF   Lawson  Initial Code.
c--D replaces "?": DR?ERF, ?ERF, ?ERFC, ?ERFCE
c
c     Demonstration driver for DERF, DERFC, and DERFCE
c
      external         D1MACH, DERF, DERFC, DERFCE
      double precision D1MACH, DERF, DERFC, DERFCE, X, XMAX, YE, YC, YCE
c
      if(log10(D1MACH(1)) .lt. -65.0d0) then
         XMAX = 12.0d0
      else
         XMAX = 9.0d0
      endif
      X= -6.0D0
      print '(4x,''X'',9x,''DERF'',12x,''DERFC'',10x,''DERFCE''/1x)'
   20 if(X .le. XMAX)then
         YE= DERF(X)
         YC= DERFC(X)
      if(X .lt. 0.0D0)then
            print '(1x,f5.1,1x,2e17.8)', X,YE,YC
      else
            YCE = DERFCE(X)
            print '(1x,f5.1,1x,3e17.8)', X,YE,YC,YCE
      end if
      if(X .lt. 6.0D0)then
            X = X + 0.5D0
      else
            X = X + 1.0D0
      end if
      go to 20
      end if
      stop
      end
