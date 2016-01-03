c     program DRSERFI
c>> 1996-06-27 DRSERFI  Krogh Minor change in formats for C conversion.
c>> 1996-05-28 DRSERFI  Krogh Added external statement.
c>> 1994-10-19 DRSERFI  Krogh  Changes to use M77CON
c>> 1992-04-21 DRSERFI  CLL
c>> 1991-11-19 CLL
c>> 1987-12-09 DRSERFI  Snyder  Initial Code.
c
c     Demonstration driver for SERFI and SERFCI.
c     ------------------------------------------------------------------
      external R1MACH, SERFI, SERFCI
      real             SERFI, SERFCI, R, X, R1MACH, FLOOR
      integer I
c     ------------------------------------------------------------------
c--S replaces "?": DR?ERFI, ?ERFI, ?ERFCI
c     ------------------------------------------------------------------
   90 format (11x,1p,g12.5,g29.14)
      print '(/11x,''DRSERFI.. Demo driver for SERFI and SERFCI''//
     *   16x,''X'',13x,''SERFI(X) = SERFCI(1-X)'')'
      R = 1.0e-5
      do 10 I = 1,4
         X = 1.0e0 - R
         R = 10.0e0 * R
         print 90, X, SERFI(x)
   10 continue
      X = 0.9e0
      do 20 I = 1,9
         print 90, X, SERFI(x)
         X = X - 0.1e0
   20 continue
      print 90, 1.0e-10, SERFI(1.0e-10)
      print 90, 0.0e0, SERFI(0.0e0)
c
      print '(/16x,''X'',13x,''SERFCI(X) = SERFI(1-X)'')'
      X = 1.0e-10
      FLOOR = R1MACH(1) * 1.0e11
      do 30 I = 1,10
        print 90, X, SERFCI(X)
        if (X .lt. FLOOR) go to 40
        X = 1.0e-10 * X
   30 continue
   40 continue
      stop
      end
