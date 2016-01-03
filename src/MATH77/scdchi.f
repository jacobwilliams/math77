      subroutine SCDCHI (CHISQ, NU, P, Q, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 SCDCHI Krogh  Changes to use M77CON
c>> 1994-08-05 SCDCHI WV Snyder
c
c     Compute the Cumulative Density Function of the Chi-squared
c     distribution for parameter argument CHISQ and NU degrees of
c     freedom.  This is usually denoted P(chi**2 | nu).  This procedure
c     also computes Q(chi**2 | nu) = 1 - P(chi**2 | nu).  CHISQ and NU
c     must both be non-negative, and at most one of them may be zero.
c
      real             CHISQ, NU, P, Q
      integer IERR
c--S replaces "?": ?CDCHI, ?gami
c
      call sgami (0.5e0 * nu, 0.5e0 * chisq, p, q, ierr)
      return
      end
