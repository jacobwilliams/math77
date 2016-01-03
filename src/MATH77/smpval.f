      REAL             FUNCTION SMPVAL (P, NDEG, X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-10-20 SMPVAL Krogh  Changes to use M77CON
C>> 1994-04-20 SMPVAL CLL Making DP and SP versions similar.
C>> 1987-12-09 SMPVAL Lawson  Initial code.
C     Evaluation of polynomial using monomial basis and a
c     linear transformation of the argument.
C     C.L.LAWSON, JPL, 1974 NOV 19
c     ------------------------------------------------------------------
c--S replaces "?": ?MPVAL
c     ------------------------------------------------------------------
      integer j, ndeg
      real             p(*), x, u
c     ------------------------------------------------------------------
      SMPVAL=0.0e0
      u=(x-p(1))/p(2)
      do 20 j = ndeg, 0, -1
          SMPVAL=SMPVAL*u + p(j+3)
   20 continue
      end
