c     program DRDERFI
c>> 1996-06-27 DRDERFI  Krogh Minor change in formats for C conversion.
c>> 1996-05-28 DRDERFI  Krogh Added external statement.
c>> 1994-10-19 DRDERFI  Krogh  Changes to use M77CON
c>> 1992-04-21 DRDERFI  CLL
c>> 1991-11-19 CLL
c>> 1987-12-09 DRDERFI  Snyder  Initial Code.
c
c     Demonstration driver for DERFI and DERFCI.
c     ------------------------------------------------------------------
      external D1MACH, DERFI, DERFCI
      double precision DERFI, DERFCI, R, X, D1MACH, FLOOR
      integer I
c     ------------------------------------------------------------------
c--D replaces "?": DR?ERFI, ?ERFI, ?ERFCI
c     ------------------------------------------------------------------
   90 format (11x,1p,g12.5,g29.14)
      print '(/11x,''DRDERFI.. Demo driver for DERFI and DERFCI''//
     *   16x,''X'',13x,''DERFI(X) = DERFCI(1-X)'')'
      R = 1.0d-5
      do 10 I = 1,4
         X = 1.0d0 - R
         R = 10.0d0 * R
         print 90, X, DERFI(x)
   10 continue
      X = 0.9d0
      do 20 I = 1,9
         print 90, X, DERFI(x)
         X = X - 0.1d0
   20 continue
      print 90, 1.0d-10, DERFI(1.0d-10)
      print 90, 0.0d0, DERFI(0.0d0)
c
      print '(/16x,''X'',13x,''DERFCI(X) = DERFI(1-X)'')'
      X = 1.0d-10
      FLOOR = D1MACH(1) * 1.0d11
      do 30 I = 1,10
        print 90, X, DERFCI(X)
        if (X .lt. FLOOR) go to 40
        X = 1.0d-10 * X
   30 continue
   40 continue
      stop
      end
