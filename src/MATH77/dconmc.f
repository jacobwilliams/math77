      subroutine DCONMC(N, COEFF)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 DCONMC Krogh   Declared all vars.
c>> 1994-10-20 DCONMC Krogh  Changes to use M77CON
c>> 1992-06-04 DCONMC Krogh  Corrected line below.
c>> 1992-01-31 DCONMC Krogh  Initial Code.
c--D replaces "?": ?CONMC
c
c Converts COEFF in monomial basis to the Chebyshev basis.
c
      integer N, J, I
      double precision COEFF(0:N), TP
c
      if (N .le. 0) return
      TP = .5D0**(N-1)
      COEFF(N) = TP * COEFF(N)
      COEFF(N-1) = TP * COEFF(N-1)
      do 20 J = N-2, 0, -1
         TP = 2.D0 * TP
         COEFF(J) = TP * COEFF(J)
         COEFF(J+1) = 2.D0 * COEFF(J+1)
         do 10 I = J, N-2
            COEFF(I) = COEFF(I) + COEFF(I+2)
   10    continue
   20 continue
      return
      end
