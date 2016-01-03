      SUBROUTINE DLASUM(X,N,A,SUM)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-10-20 DLASUM Krogh  Changes to use M77CON
C>> 1994-04-20 DLASUM CLL  Edited to make DP & SP files similar.
C>> 1992-03-13 DLASUM FTK  Removed implicit statements.
C>> 1985-08-02 DLASUM Lawson  Initial code.
C
C     THIS SUBROUTINE EVALUATES THE SUM OF
C          A(J) * L(J) FOR J = 0,...,N,
C     WHERE L(J)'S  ARE LAGUERRE POLYNOMIALS OF DEGREE J.
C
C      THE RECURSION FORMULA IS :
C      B(K) = B(K+1)*(2*K+1-X)/(K+1)-B(K+2)*(K+1)/(K+2)+A(K)
C
C      C.L.LAWSON & S.CHAN,JPL,1983 JUNE 9.
C
C     -------------------------------------------------------------
C     SUBROUTINE ARGUMENTS
C     --------------------
C     X     ARGUMENT OF LAG POLYS, X SHOULD BE NON-NEGATIVE.
C     N     SUM IS TO INCLUDE LAGUERRE POLYS OF DEGREE ZERO
C           THRU N.
C     A()   A(0),...,A(N) CONTAIN COEFFS TO BE USED IN
C           FORMING THE SUM.
C     SUM   SUM OF COMBINATION
C
C     -------------------------------------------------------------
c--D replaces "?": ?LASUM
C     -------------------------------------------------------------
      INTEGER K,N
      DOUBLE PRECISION A(0:N),B,B1,B2,C1,C2,C3
      DOUBLE PRECISION ONE,SUM,TWO,X,ZERO
      DATA ZERO,ONE,TWO / 0.D0, 1.D0, 2.D0 /
C     -------------------------------------------------------------
      IF (N .lt. 0) then
        SUM = ZERO
        RETURN
      endif
C
      C1 = N+1
      C3 = C1 + C1 - ONE
      B1 = ZERO
      B  = A(N)
C
      DO 10 K = N-1,0,-1
C
C     C1 = K + 1, C3 = 2K + 1
C
        C2 = C1
        C1 = C1 - ONE
        C3 = C3 - TWO
        B2 = B1
        B1 = B
        B = B1 * (C3-X) / C1 - B2 * C1 / C2 + A(K)
  10  CONTINUE
      SUM = B
      RETURN
      END
