      SUBROUTINE SPOLZ2(A,Z)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-04-27 SPOLZ2 Krogh   Changes to use .C. and C%%.
c>> 1996-03-30 SPOLZ2 Krogh   Added external statement.
C>> 1996-01-18 SPOLZ2 Krogh   Added code for M77CON for conversion to C.
C>> 1992-03-13 SPOLZ2 FTK     Removed implicit statements.
C>> 1987-02-25 SPOLZ2 Lawson  Initial code.
c--S Replaces "?": ?POLZ2
c++ Default NO_COMPLEX = .C. | (.N. == 'D')
c++ Default COMPLEX = ~NO_COMPLEX
C
C     ------------------------------------------------------------------
C
C     Find the two roots of the quadratic polynomial
C               A(1)*X*X + A(2)*X + A(3)
C     Return the roots as the complex numbers (Z(1), Z(2)) and
C     (Z(3), Z(4)) if Z is not complex, else return the two complex
C     numbers, Z(1) and Z(2).
C     Require A(1) .ne. 0.
C
C     Method:
C     Divide through by A(1). New polynomial is
C            X*X + P*X + Q
C     Let U = -P/2
C     Roots are U + SQRT(U*U-Q) and U - SQRT(U*U-Q).
C     Avoid computing U*U explicity if it might overflow or
C     underflow. In case of real roots, U + Z and U - Z, the first
C     root is  U + SIGN(U)*ABS(Z) and the second is Q / Z(1).
C
c     C. L. Lawson & S. Chiu, JPL, 1987 Feb 16.
C     ------------------------------------------------------------------
C
      EXTERNAL R1MACH
      REAL             A(3),AQ,AU,C1,C16,C2,D,R1MACH,F,HALF,ONE,P,Q
      REAL             U,V,W,ZERO
      PARAMETER(ZERO=0.E0, HALF=.5E0, ONE=1.E0, C16=16.E0)
      LOGICAL FIRST
c++ CODE for COMPLEX is active
      COMPLEX Z(2), R1, R2
c++ CODE for NO_COMPLEX is inactive
C      REAL             Z(4), R1, R2, R3, R4
c++ END
C
      SAVE FIRST, C1, C2
C
      DATA FIRST / .TRUE. /
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF (FIRST) THEN
        FIRST = .FALSE.
C
C     R1MACH(1) is the underflow limit.
C
        C1 = SQRT(R1MACH(1)*C16)
C
C     R1MACH(2) is the overflow limit.
C
        C2 = SQRT(R1MACH(2)/C16)
      END IF
C
      IF (A(1) .EQ. ZERO) THEN
        CALL ERMSG('SPOLZ2',1,0,'A(1) .EQ. 0.','.')
        R1 = ZERO
        R2 = ZERO
c++ CODE for NO_COMPLEX is inactive
C        R3 = ZERO
C        R4 = ZERO
c++ END
        GO TO 99
      ELSE
        P = A(2) / A(1)
        Q = A(3) / A(1)
      END IF
C
      IF (Q .EQ. ZERO) THEN
        R1 = ZERO
c++ CODE for COMPLEX is active
        R2 = -P
c++ CODE for NO_COMPLEX is inactive
C        R2 = ZERO
C        R3 = -P
C        R4 = ZERO
c++ END
        GO TO 99
      END IF
C
      IF (P .EQ. ZERO) THEN
        W = SQRT(ABS(Q))
        IF (Q .GT. ZERO) THEN
c++ CODE for COMPLEX is active
          R1 = CMPLX(ZERO,W)
          R2 = CMPLX(ZERO,-W)
c++ CODE for NO_COMPLEX is inactive
C          R1 = ZERO
C          R2 = W
C          R3 = ZERO
C          R4 = -W
c++ END
        ELSE
c++ CODE for COMPLEX is active
          R1 = CMPLX(W,ZERO)
          R2 = CMPLX(-W,ZERO)
c++ CODE for NO_COMPLEX is inactive
C          R1 = W
C          R2 = ZERO
C          R3 = -W
C          R4 = ZERO
c++ END
        END IF
        GO TO 99
      END IF
      U = -P * HALF
C
C     Compute D having the sign of U*U-Q
C         and F = SQRT(ABS(U*U-Q))
C
      AU = ABS(U)
      IF (AU .GT. C2) THEN
        D = ONE - (Q/U) / U
        F = AU * SQRT(ABS(D))
      ELSE IF (AU .LT. C1) THEN
        AQ = ABS(Q)
        D = U * (U/AQ) - SIGN(ONE, Q)
        F = SQRT(AQ) * SQRT(ABS(D))
      ELSE
        D = U*U - Q
        F = SQRT(ABS(D))
      END IF
C
      IF (D .EQ. ZERO) THEN
c++ CODE for COMPLEX is active
        R1 = CMPLX(U,ZERO)
        R2 = R1
c++ CODE for NO_COMPLEX is inactive
C        R1 = U
C        R2 = ZERO
C        R3 = R1
C        R4 = ZERO
c++ END
      ELSE
        IF (D .GT. ZERO) THEN
          IF (U .GT. ZERO) THEN
            V = U + F
          ELSE
            V = U - F
          END IF
c++ CODE for COMPLEX is active
          R1 = CMPLX(V,ZERO)
          R2 = CMPLX(Q/V,ZERO)
c++ CODE for NO_COMPLEX is inactive
C          R1 = V
C          R2 = ZERO
C          R3 = Q/V
C          R4 = ZERO
c++ END
        ELSE
c++ CODE for COMPLEX is active
          R1 = CMPLX(U,F)
          R2 = CMPLX(U,-F)
c++ CODE for NO_COMPLEX is inactive
C          R1 = U
C          R2 = F
C          R3 = U
C          R4 = -F
c++ END
        END IF
      END IF
C
   99 CONTINUE
      Z(1) = R1
      Z(2) = R2
c++ CODE for NO_COMPLEX is inactive
C      Z(3) = R3
C      Z(4) = R4
c++ END
      RETURN
      END
