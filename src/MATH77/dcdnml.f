      double precision function DCDNML (X, MU, SIGMA)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 DCDNML Krogh  Moved external statement up for mangle.
c>> 1994-10-20 DCDNML Krogh  Changes to use M77CON
c>> 1994-08-05 DCDNML WV Snyder
c
c     Compute the Cumulative Distribution Function for the Normal
c     (Gaussian) distribution, g(x;mu,sigma).  This is given in terms of
c     erf by g(x;mu,sigma) = (1 + erf((x-mu) / (sqrt(2)*sigma)) ) / 2 =
c     1 - erfc((x-mu) / (sqrt(2)*sigma)) / 2.  To avoid cancellation
c     error when (X-MU) < 0, substitute erfc(z) = 2 - erfc(-z) in the
c     second expression above to get g(-x;mu,sigma) = erfc((mu-x) /
c     (sqrt(2)*sigma)) / 2.  This also works well when (X-MU) >= 0.
c
      double precision X, MU, SIGMA
c--D replaces "?": ?CDNML, ?ERFC
      external D1MACH, DERFC
      double precision BIG, RSQT2, D1MACH, S, DERFC, U
c     BIG is the largest representable number.
c     RSQT2 = 1.0 / sqrt(2.0)
      parameter (RSQT2 = 0.707106781186547524400844362104849039284836d0)
      save BIG
      data BIG /-1.0d0/
c
      if (big .lt. 0.0d0) big = d1mach(2)
      u = (mu-x)*rsqt2
      s = abs(sigma)
      if (s .lt. 1.0d0) then
         if (u .gt. s * big) then
            dcdnml = 1.0d0
            return
         end if
      end if
      dcdnml = 0.5d0 * derfc(u/s)
      return
      end
