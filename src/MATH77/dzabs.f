C++ Replace "DZABS" = .Y.//.N.//"ABS"
      DOUBLE PRECISION FUNCTION DZABS(Z)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1995-10-30 DZABS  Krogh   Fixed so M77CON can get S.P. for C conv.
C>> 1987-12-07 DZABS  Lawson  Initial code.
C--Z replaces "?":
C
C     Returns sqrt(Z(1)**2 + Z(2)**2)
C     Lawson and Chiu, JPL, Oct 1987
C
      DOUBLE PRECISION Z(2), A, B, ONE, ZERO
      PARAMETER(ONE=1.0D0, ZERO=0.0D0)
C
      A = ABS(Z(1))
      B = ABS(Z(2))
      IF (A .GT. B) THEN
        DZABS = A * SQRT(ONE+(B/A)**2)
      ELSE IF (B .NE. ZERO) THEN
        DZABS = B * SQRT(ONE+(A/B)**2)
      ELSE
        DZABS = ZERO
      END IF
      RETURN
      END
