      double precision function DPPNML (U, MU, SIGMA)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 DPPNML Krogh  Moved external statement up for mangle.
c>> 1994-10-20 DPPNML Krogh  Changes to use M77CON
c>> 1994-08-05 DPPNML WV Snyder
c
c     Compute the Percentage Point of the Normal (Gaussian) Probability
c     Distribution, g**(-1) (u;mu,sigma).  This is given in terms of the
c     inverse of the co-error function, erfci, by x = mu -
c     sigma * sqrt(2) * erfci(2*u).
c
      double precision U, MU, SIGMA
c--D replaces "?": ?PPNML, ?ERFCI
      external DERFCI
      double precision SQRT2, DERFCI
      parameter (SQRT2 = 1.414213562373095048801688724209698078569672d0)
c
      dppnml = mu - sigma * sqrt2 * derfci(u+u)
      return
      end
