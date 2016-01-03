      double precision function DRLOG (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c  File drlog contains user callable procedures drlog & drlog1, and
c               lower level procedure drlog2.
c  drlog(x)  computes x - 1 - ln(x).
c  drlog1(x) computes x - ln(1 + x).
c>> 1996-03-30 DRLOG  Krogh  Added external statements.
C>> 1994-11-09 CLL Edited to avoid ENTRY statement.
C>> 1994-10-20 DRLOG Krogh  Changes to use M77CON
C>> 1994-05-23 DRLOG WVS JPL Combine DRLOG and DRLOG1
C>> 1994-05-23 DRLOG WVS JPL Make SP and DP alike using CHGTYP
C>> 1993-05-06 DRLOG WVS JPL Conversion from NSWC to Math 77
C ----------------------------------------------------------------------
c--D replaces "?": ?RLOG, ?RLOG1, ?RLOG2
C     ==================================================================
C             EVALUATION OF THE FUNCTION X - 1 - LN(X)
C     ------------------------------------------------------------------
      external drlog2
      double precision drlog2, r, x
C     ------------------------------------------------------------------
      R = (X - 0.5D0) - 0.5D0
      IF (X .LT. 0.61D0 .OR. X .GT. 1.57D0) THEN
         DRLOG = R - LOG(X)
      else
         DRLOG = DRLOG2(R)
      endif
      end
c     ==================================================================
      double precision function DRLOG1 (X)
C             EVALUATION OF THE FUNCTION X - LN(1 + X)
C     ------------------------------------------------------------------
      external drlog2
      double precision drlog2, r, x
C     ------------------------------------------------------------------
      IF (X .LT. -0.39D0 .OR. X .GT. 0.57D0) THEN
         R = (X + 0.5D0) + 0.5D0
         DRLOG1 = X - LOG(R)
      else
         DRLOG1 = DRLOG2(X)
      endif
      end
c     ==================================================================
      double precision function DRLOG2(RIN)
C            Complete computation started by DRLOG or DRLOG1.
C     ------------------------------------------------------------------
      external D1MACH
      double precision D1MACH
      DOUBLE PRECISION A, B, R, RIN, ROUND, T, U, UP2, W, Z
      SAVE ROUND
      DOUBLE PRECISION C1, C2, C3, C4, C5
      DOUBLE PRECISION P0, P1, P2, P3, Q1, Q2, Q3, Q4
      DOUBLE PRECISION R0, R1, R2, S1, S2
c ------------------------
C     CI = 1/(2I + 1)
c ------------------------
      PARAMETER (C1 = 1.0D0 / 3.0D0)
      PARAMETER (C2 = 1.0D0 / 5.0D0)
      PARAMETER (C3 = 1.0D0 / 7.0D0)
      PARAMETER (C4 = 1.0D0 / 9.0D0)
      PARAMETER (C5 = 1.0D0 / 11.0D0)
c ------------------------
C     A = DRLOG (0.7)
C     B = DRLOG (4/3)
c ------------------------
      DATA A /.566749439387323789126387112411845D-01/
      DATA B /.456512608815524058941143273395059D-01/
c ------------------------
      DATA P0 / .7692307692307692307680D-01/,
     *     P1 /-.1505958055914600184836D+00/,
     *     P2 / .9302355725278521726994D-01/,
     *     P3 /-.1787900022182327735804D-01/
      DATA Q1 /-.2824412139355646910683D+01/,
     *     Q2 / .2892424216041495392509D+01/,
     *     Q3 /-.1263560605948009364422D+01/,
     *     Q4 / .1966769435894561313526D+00/
c ------------------------
      DATA R0/ .333333333333333D+00/, R1/-.224696413112536D+00/,
     *     R2/ .620886815375787D-02/
      DATA S1/-.127408923933623D+01/, S2/ .354508718369557D+00/
c ------------------------
      DATA ROUND /-1.0D0/
C     ------------------------------------------------------------------
C
C                 ARGUMENT REDUCTION
C
      R = RIN
      IF (R .LT. -0.18D0) THEN
         U = (10.0D0*R + 3.0D0)/7.0D0
         UP2 = U + 2.0D0
         W = A - U*0.3D0
      ELSE IF (R .GT. 0.18D0) THEN
         T = 0.75D0*R
         U = T - 0.25D0
         UP2 = T + 1.75D0
         W = B + U/3.0D0
      ELSE
         U = R
         UP2 = U + 2.0D0
         W = 0.0D0
      END IF
      IF (ROUND .LT. 0.0D0) ROUND = D1MACH(4)
C
C                  SERIES EXPANSION
C
      R = U/UP2
      T = R*R
      IF (ROUND .GT. 5.0D-14) THEN
         Z = ((R2*T + R1)*T + R0)/((S2*T + S1)*T + 1.0D0)
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
     *       ((((Q4*T + Q3)*T + Q2)*T + Q1)*T + 1.0D0)
C
         Z = ((((Z*T + C5)*T + C4)*T + C3)*T + C2)*T + C1
      END IF
      DRLOG2 = R*(U - 2.0D0*T*Z) + W
      END
