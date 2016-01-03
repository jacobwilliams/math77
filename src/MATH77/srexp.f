      REAL             FUNCTION SREXP (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2001-07-16 SREXP Krogh  Change -1.0 to -1.E0.
C>> 1998-10-29 SREXP Krogh  Moved external statement up for mangle.
C>> 1994-10-20 SREXP Krogh  Changes to use M77CON
C>> 1994-05-20 SREXP WVS JPL Make SP and DP alike using CHGTYP
C>> 1993-05-06 SREXP WVS JPL Convert from NSWC to Math 77
c--S replaces "?": ?REXP
C ----------------------------------------------------------------------
C            EVALUATION OF THE FUNCTION EXP(X) - 1
c ----------------------------------------------------------------------
      REAL             X, EXMIN, ROUND, Z
      REAL             A0, A1, A2, A3, A4, B1, B2, B3, B4
      REAL             C1, C2, C3, C4, C5
      REAL             P1, P2, Q1, Q2, Q3, Q4
      EXTERNAL R1MACH
      REAL             R1MACH
C     EXMIN is the smallest number such that EXP(EXMIN) - 1 > -1.
C     ROUND is the smallest number such that ROUND + 1 > 1.
      SAVE EXMIN, ROUND
c --------------------------
C     CI = 1/FACTORIAL(I + 2)
c --------------------------
      PARAMETER (C1 = 1.0E0/6.0E0)
      PARAMETER (C2 = 1.0E0/24.0E0)
      PARAMETER (C3 = 1.0E0/120.0E0)
      PARAMETER (C4 = 1.0E0/720.0E0)
      PARAMETER (C5 = 1.0E0/5040.0E0)
c --------------------------
      DATA EXMIN /1.0E0/
      DATA A0/ .248015873015873015873016E-04/,
     *     A1/-.344452080605731005808147E-05/,
     *     A2/ .206664230430046597475413E-06/,
     *     A3/-.447300111094328162971036E-08/,
     *     A4/ .114734027080634968083920E-11/
      DATA B1/-.249994190011341852652396E+00/,
     *     B2/ .249987228833107957725728E-01/,
     *     B3/-.119037506846942249362528E-02/,
     *     B4/ .228908693387350391768682E-04/
c --------------------------
      DATA P1/ .914041914819518E-09/, P2/ .238082361044469E-01/,
     *     Q1/-.499999999085958E+00/, Q2/ .107141568980644E+00/,
     *     Q3/-.119041179760821E-01/, Q4/ .595130811860248E-03/
c ----------------------
      IF (EXMIN .GT. 0.0E0) THEN
         ROUND = R1MACH(4)
         EXMIN = LOG(ROUND)
      END IF
      IF (ABS(X) .LE. 0.15E0) THEN
         IF (ROUND .LT. 1.0E-14) THEN
C
C           Z IS A MINIMAX APPROXIMATION OF THE SERIES
C
C                C6 + C7*X + C8*X**2 + ....
C
C           THIS APPROXIMATION IS ACCURATE TO WITHIN
C           1 UNIT OF THE 23-RD SIGNIFICANT DIGIT.
C           THE RESULTING VALUE FOR SREXP IS ACCURATE TO
C           WITHIN 1 UNIT OF THE 33-RD SIGNIFICANT
C           DIGIT.
C
            Z = ((((A4*X + A3)*X + A2)*X + A1)*X + A0) /
     *          ((((B4*X + B3)*X + B2)*X + B1)*X + 1.E0)
            SREXP = X * (((((((Z*X + C5)*X + C4)*X + C3)*X + C2)*X +
     *                  C1)*X + 0.5E0)*X + 1.E0)
            RETURN
         END IF
         SREXP = X*(((P2*X + P1)*X + 1.0E0)/((((Q4*X + Q3)*X + Q2)*X +
     *           Q1)*X + 1.0E0))
         RETURN
      END IF
C
      IF (X .GE. 0.E0) THEN
         Z = EXP(X)
         SREXP = Z*(0.5E0 + (0.5E0 - 1.E0/Z))
         RETURN
      END IF
      IF (X .GE. EXMIN) THEN
         SREXP = (EXP(X) - 0.5E0) - 0.5E0
         RETURN
      END IF
      SREXP = -1.E0
      RETURN
      END
