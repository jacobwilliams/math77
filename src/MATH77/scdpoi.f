      subroutine SCDPOI (N, LAMDA, P, Q, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 SCDPOI Krogh  Changes to use M77CON
c>> 1994-08-05 SCDPOI WV Snyder
c
c     Compute Q = exp(-lamda) sum (lamda**j/j!,j=0..n-1) and
c             P = exp(-lamda) sum (lamda**j/j!,j=n..infinity).
c
c     These functions are given by the incomplete gamma ratios
c     P(n,lamda) and Q(n,lamda).
c
      real             LAMDA, P, Q
      integer N, IERR
c--S replaces "?": ?CDPOI, ?gami
c
      call sgami (REAL(n),lamda,p,q,ierr)
      return
      end
