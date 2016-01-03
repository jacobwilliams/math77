      program DRSERF
c>> 1996-06-17 DRSERF Krogh Minor change in formats for C conversion.
c>> 1994-10-19 DRSERF Krogh  Changes to use M77CON
c>> 1992-05-13 DRSERF CLL
c>> 1991-10-16 DRSERF CLL add demo of SERFCE
c>> 1987-12-09 DRSERF   Lawson  Initial Code.
c--S replaces "?": DR?ERF, ?ERF, ?ERFC, ?ERFCE
c
c     Demonstration driver for SERF, SERFC, and SERFCE
c
      external         R1MACH, SERF, SERFC, SERFCE
      real             R1MACH, SERF, SERFC, SERFCE, X, XMAX, YE, YC, YCE
c
      if(log10(R1MACH(1)) .lt. -65.0e0) then
         XMAX = 12.0e0
      else
         XMAX = 9.0e0
      endif
      X= -6.0E0
      print '(4x,''X'',9x,''SERF'',12x,''SERFC'',10x,''SERFCE''/1x)'
   20 if(X .le. XMAX)then
         YE= SERF(X)
         YC= SERFC(X)
      if(X .lt. 0.0E0)then
            print '(1x,f5.1,1x,2e17.8)', X,YE,YC
      else
            YCE = SERFCE(X)
            print '(1x,f5.1,1x,3e17.8)', X,YE,YC,YCE
      end if
      if(X .lt. 6.0E0)then
            X = X + 0.5E0
      else
            X = X + 1.0E0
      end if
      go to 20
      end if
      stop
      end
