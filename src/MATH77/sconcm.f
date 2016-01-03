      subroutine SCONCM(N, COEFF)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 SCONCM Krogh   Declared all vars.
c>> 1994-10-20 SCONCM Krogh  Changes to use M77CON
c>> 1992-06-04 SCONCM Krogh  Corrected line below.
c>> 1992-01-31 SCONCM Krogh  Initial Code.
c--S replaces "?": ?CONCM
c
c Converts COEFF in Chebyshev basis to the monomial basis.
c
      integer N, J, I
      real             COEFF(0:N), TP
c
      if (N .le. 0) return
      TP = 1.E0
      do 20 J = 0, N-2
         do 10 I = N-2, J, -1
            COEFF(I) = COEFF(I) - COEFF(I+2)
   10    continue
         COEFF(J+1) = .5E0 * COEFF(J+1)
         COEFF(J) = TP * COEFF(J)
         TP = 2.E0 * TP
   20 continue
      COEFF(N) = TP * COEFF(N)
      COEFF(N-1) = TP * COEFF(N-1)
      return
      end
