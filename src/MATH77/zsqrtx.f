      SUBROUTINE ZSQRTX(CIN,COUT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2001-01-24 ZSQRTX Krogh  ZSQRT -> ZSQRTX to avoid C lib. conflicts.
C>> 1995-10-30 ZSQRT  Krogh  Fixed so M77CON can get S.P. for C conv.
C>> 1987-12-07 ZSQRT  Lawson Initial code.
c--Z replaces "?": ?SQRTX
C
C     Reference: Uspensky, "Theory of Equations", McGraw-Hill,
C     1948, Page 14-15.
C     Computes one of the square roots of the double precision
C     complex number whose real part is given in CIN(1) and
C     imaginary part is given in CIN(2). Result is given
C     similarly in COUT(1) and COUT(2).
C
C     C.L.Lawson & S.Y.Chan, JPL, June 3,1986.
C
C     ------------------------------------------------------------------
C
      DOUBLE PRECISION CIN(2), COUT(2)
      DOUBLE PRECISION A, A1, B, B1, R, X, Y
      DOUBLE PRECISION ZERO, HALF, ONE
C
      PARAMETER(ZERO= 0.0D0, HALF = 0.5D0, ONE = 1.0D0)
C
      A = CIN(1)
      B = CIN(2)
      IF (B .EQ. ZERO) THEN
        IF (A .EQ. ZERO) THEN
          COUT(1) = ZERO
          COUT(2) = ZERO
        ELSE IF (A .GT. ZERO) THEN
          COUT(1) = SQRT(A)
          COUT(2) = ZERO
        ELSE
          COUT(1) = ZERO
          COUT(2) = SQRT(-A)
        END IF
      ELSE
C                                          Here B .ne. 0
        IF (A .EQ. ZERO) THEN
          COUT(1) = SQRT(HALF*ABS(B))
          COUT(2) = COUT(1)
        ELSE
          A1 = ABS(A)
          B1 = ABS(B)
          IF (B1 .GT. A1) THEN
            R = B1 * SQRT(ONE + (A1/B1)**2)
          ELSE
            R = A1 * SQRT(ONE + (B1/A1)**2)
          END IF
          X = SQRT(HALF*(R+A1))
          Y = HALF * B1 / X
          IF (A .GT. ZERO) THEN
            COUT(1) = X
            COUT(2) = Y
          ELSE
            COUT(1) = Y
            COUT(2) = X
          END IF
        END IF
        IF (B .LT. ZERO) COUT(2) = -COUT(2)
      END IF
C
      RETURN
      END
