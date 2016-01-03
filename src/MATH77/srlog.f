      real             function SRLOG (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c  File srlog contains user callable procedures srlog & srlog1, and
c               lower level procedure srlog2.
c  srlog(x)  computes x - 1 - ln(x).
c  srlog1(x) computes x - ln(1 + x).
c>> 1996-03-30 SRLOG  Krogh  Added external statements.
C>> 1994-11-09 CLL Edited to avoid ENTRY statement.
C>> 1994-10-20 SRLOG Krogh  Changes to use M77CON
C>> 1994-05-23 SRLOG WVS JPL Combine SRLOG and SRLOG1
C>> 1994-05-23 SRLOG WVS JPL Make SP and DP alike using CHGTYP
C>> 1993-05-06 SRLOG WVS JPL Conversion from NSWC to Math 77
C ----------------------------------------------------------------------
c--S replaces "?": ?RLOG, ?RLOG1, ?RLOG2
C     ==================================================================
C             EVALUATION OF THE FUNCTION X - 1 - LN(X)
C     ------------------------------------------------------------------
      external srlog2
      real             srlog2, r, x
C     ------------------------------------------------------------------
      R = (X - 0.5E0) - 0.5E0
      IF (X .LT. 0.61E0 .OR. X .GT. 1.57E0) THEN
         SRLOG = R - LOG(X)
      else
         SRLOG = SRLOG2(R)
      endif
      end
c     ==================================================================
      real             function SRLOG1 (X)
C             EVALUATION OF THE FUNCTION X - LN(1 + X)
C     ------------------------------------------------------------------
      external srlog2
      real             srlog2, r, x
C     ------------------------------------------------------------------
      IF (X .LT. -0.39E0 .OR. X .GT. 0.57E0) THEN
         R = (X + 0.5E0) + 0.5E0
         SRLOG1 = X - LOG(R)
      else
         SRLOG1 = SRLOG2(X)
      endif
      end
c     ==================================================================
      real             function SRLOG2(RIN)
C            Complete computation started by SRLOG or SRLOG1.
C     ------------------------------------------------------------------
      external R1MACH
      real             R1MACH
      REAL             A, B, R, RIN, ROUND, T, U, UP2, W, Z
      SAVE ROUND
      REAL             C1, C2, C3, C4, C5
      REAL             P0, P1, P2, P3, Q1, Q2, Q3, Q4
      REAL             R0, R1, R2, S1, S2
c ------------------------
C     CI = 1/(2I + 1)
c ------------------------
      PARAMETER (C1 = 1.0E0 / 3.0E0)
      PARAMETER (C2 = 1.0E0 / 5.0E0)
      PARAMETER (C3 = 1.0E0 / 7.0E0)
      PARAMETER (C4 = 1.0E0 / 9.0E0)
      PARAMETER (C5 = 1.0E0 / 11.0E0)
c ------------------------
C     A = SRLOG (0.7)
C     B = SRLOG (4/3)
c ------------------------
      DATA A /.566749439387323789126387112411845E-01/
      DATA B /.456512608815524058941143273395059E-01/
c ------------------------
      DATA P0 / .7692307692307692307680E-01/,
     *     P1 /-.1505958055914600184836E+00/,
     *     P2 / .9302355725278521726994E-01/,
     *     P3 /-.1787900022182327735804E-01/
      DATA Q1 /-.2824412139355646910683E+01/,
     *     Q2 / .2892424216041495392509E+01/,
     *     Q3 /-.1263560605948009364422E+01/,
     *     Q4 / .1966769435894561313526E+00/
c ------------------------
      DATA R0/ .333333333333333E+00/, R1/-.224696413112536E+00/,
     *     R2/ .620886815375787E-02/
      DATA S1/-.127408923933623E+01/, S2/ .354508718369557E+00/
c ------------------------
      DATA ROUND /-1.0E0/
C     ------------------------------------------------------------------
C
C                 ARGUMENT REDUCTION
C
      R = RIN
      IF (R .LT. -0.18E0) THEN
         U = (10.0E0*R + 3.0E0)/7.0E0
         UP2 = U + 2.0E0
         W = A - U*0.3E0
      ELSE IF (R .GT. 0.18E0) THEN
         T = 0.75E0*R
         U = T - 0.25E0
         UP2 = T + 1.75E0
         W = B + U/3.0E0
      ELSE
         U = R
         UP2 = U + 2.0E0
         W = 0.0E0
      END IF
      IF (ROUND .LT. 0.0E0) ROUND = R1MACH(4)
C
C                  SERIES EXPANSION
C
      R = U/UP2
      T = R*R
      IF (ROUND .GT. 5.0E-14) THEN
         Z = ((R2*T + R1)*T + R0)/((S2*T + S1)*T + 1.0E0)
      ELSE
C
C        Z IS (AT FIRST) A MINIMAX APPROXIMATION OF THE SERIES
C
C               C6 + C7*R**2 + C8*R**4 + ...
C
C        FOR THE INTERVAL (0.0, 0.375). THE APPROXIMATION IS ACCURATE
C        TO WITHIN 1.6 UNITS OF THE 21-ST SIGNIFICANT DIGIT.
C
         Z = (((P3*T + P2)*T + P1)*T + P0)/
     *       ((((Q4*T + Q3)*T + Q2)*T + Q1)*T + 1.0E0)
C
         Z = ((((Z*T + C5)*T + C4)*T + C3)*T + C2)*T + C1
      END IF
      SRLOG2 = R*(U - 2.0E0*T*Z) + W
      END
