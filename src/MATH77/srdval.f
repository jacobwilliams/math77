      SUBROUTINE SRDVAL (X, Y, Z, RD, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>>   2001-07-16 SRDVAL Krogh  Change -1.0 to -1.e0.
C>>   1998-10-29 SRDVAL Krogh  Moved external statement up for mangle.
C>>   1995-11-17 SRDVAL Krogh  Converted SFTRAN to Fortran 77.
c>>   1994-10-19 SRDVAL Krogh  Changes to use M77CON
c>>   1994-08-31 SRDVAL WV Snyder JPL use 2-arg min and max for C conv.
C>>   1991-10-31 SRDVAL WV Snyder JPL Incorporate changes from Carlson
C>>   1990-11-27 SRDVAL WV Snyder JPL Convert Carlson's code for MATH77
C
C     COMPUTE THE INCOMPLETE ELLIPTIC INTEGRAL OF THE SECOND KIND
C
C     RD(X,Y,Z) = INTEGRAL FROM ZERO TO INFINITY OF
C
C                           -1/2     -1/2     -3/2
C                 (3/2)(T+X)    (T+Y)    (T+Z)    DT,
C
C     WHERE X AND Y ARE NONNEGATIVE, X + Y IS POSITIVE, AND Z IS
C     POSITIVE.  IF X OR Y IS ZERO, THE INTEGRAL IS COMPLETE.
C     THE DUPLICATION THEOREM IS ITERATED UNTIL THE VARIABLES ARE
C     NEARLY EQUAL, AND THE FUNCTION IS THEN EXPANDED IN TAYLOR
C     SERIES TO FIFTH ORDER.
C     REFERENCE: B. C. CARLSON AND E. M. NOTIS, ALGORITHMS FOR
C     INCOMPLETE ELLIPTIC INTEGRALS, ACM TRANSACTIONS ON MATHEMA-
C     TICAL SOFTWARE, 7 (1981), 398-403; B. C. CARLSON, COMPUTING
C     ELLIPTIC INTEGRALS BY DUPLICATION, NUMER. MATH. 33 (1979),
C     1-16.
C     AUTHORS: B. C. CARLSON AND ELAINE M. NOTIS, AMES LABORATORY-
C     DOE, IOWA STATE UNIVERSITY, AMES, IA 50011, AND R. L. PEXTON,
C     LAWRENCE LIVERMORE NATIONAL LABORATORY, LIVERMORE, CA 94550.
C     AUG. 1, 1979, REVISED JAN. 15, 1987.
C
C     CHECK VALUE: RD(0,2,1) = 1.79721 03521 03388 31115 98837
C     CHECK: RD(X,Y,Z) + RD(Y,Z,X) + RD(Z,X,Y)
C     = 3 / DSQRT(X * Y * Z), WHERE X, Y, AND Z ARE POSITIVE.
C
      REAL             X, Y, Z, RD
      INTEGER IERR
C
C     INPUT ...
C
C     X, Y, AND Z ARE THE VARIABLES IN THE INTEGRAL RD(X,Y,Z).
C
C     OUTPUT ...
C
C     RD IS THE VALUE OF THE INCOMPLETE ELLIPTIC INTEGRAL.
C
C     IERR IS THE RETURN ERROR CODE.
C          IERR = 0  FOR NORMAL COMPLETION OF THE SUBROUTINE.
C          IERR = 1  X, Y, OR Z IS NEGATIVE.
C          IERR = 2  X+Y OR Z IS TOO SMALL.
C          IERR = 3  X, Y, OR Z IS TOO LARGE.
C
C ----------------------------------------------------------------------
C--S replaces "?": ?ERM1, ?ERV1, ?RDVAL
C ----------------------------------------------------------------------
      EXTERNAL R1MACH
      REAL             R1MACH
      REAL             C1,C2,C3,C4,EA,EB,EC,ED,EF,EPSLON,EPWR,ERRTOL
      REAL             LAMDA,LOLIM,MU,POWER4,SIGMA,S1,S2,UPLIM,XN,XNDEV
      REAL             XNROOT,YN,YNDEV,YNROOT,ZN,ZNDEV,ZNROOT
      PARAMETER (C1 = 3.0e0 / 14.0e0)
      PARAMETER (C2 = 1.0e0 / 6.0e0)
      PARAMETER (C3 = 9.0e0 / 22.0e0)
      PARAMETER (C4 = 3.0e0 / 26.0e0)
      PARAMETER (EPWR = -2.0e0 / 3.0e0)
C ----------------------------------------------------------------------
C
C     MACHINE DEPENDENT PARAMETERS ...
C
C     ERRTOL IS SET TO THE DESIRED ERROR TOLERANCE.
C     RELATIVE ERROR DUE TO TRUNCATION IS LESS THAN
C     3 * ERRTOL ** 6 / (1 - ERRTOL) ** 3/2.
C
C     LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
C     LOLIM IS NOT LESS THAN 2 / (MACHINE MAXIMUM) ** (2/3).
C     UPLIM IS NOT GREATER THAN (0.1 * ERRTOL / MACHINE
C     MINIMUM) ** (2/3).
C
      SAVE LOLIM, UPLIM, ERRTOL
      DATA LOLIM /-1.0e0/
C ----------------------------------------------------------------------
C          WARNING. CHANGES IN THE PROGRAM MAY IMPROVE SPEED AT THE
C          EXPENSE OF ROBUSTNESS.
C ----------------------------------------------------------------------
C
      RD = 0.0
      IF (MIN(X,MIN(Y,Z)) .LT. 0.0e0) THEN
         IERR = 1
         CALL SERM1 ('SRDVAL',1,0,'One of X, Y or Z is negative','X',x,
     1     ',')
         CALL SERV1 ('Y',Y,',')
         CALL SERV1 ('Z',Z,'.')
         RETURN
      END IF
      IF (LOLIM .LT. 0.0) THEN
         ERRTOL = (.28e0 * R1MACH(4)) ** (1.0e0/6.0e0)
         LOLIM = 2.0001e0 * R1MACH(2) ** EPWR
         UPLIM = (10.0e0 * R1MACH(1) / ERRTOL) ** EPWR
      END IF
      IF (MIN(X+Y,Z) .LT. LOLIM) THEN
         IERR = 2
         CALL SERM1 ('SRDVAL',2,0,'MIN(X+Y,Z) < LOLIM','X',x,
     1     ',')
         CALL SERV1 ('Y',Y,',')
         CALL SERV1 ('Z',Z,',')
         CALL SERV1 ('LOLIM',LOLIM,'.')
         RETURN
      END IF
      IF (MAX(X,MAX(Y,Z)) .GT. UPLIM) THEN
         IERR = 3
         CALL SERM1 ('SRDVAL',3,0,'One of X, Y or Z > UPLIM','X',x,
     1     ',')
         CALL SERV1 ('Y',Y,',')
         CALL SERV1 ('Z',Z,',')
         CALL SERV1 ('UPLIM',UPLIM,'.')
         RETURN
      END IF
C
      IERR = 0
      XN = X
      YN = Y
      ZN = Z
      SIGMA = 0.0e0
      POWER4 = 1.0e0
C
   20 CONTINUE
         MU = (XN + YN + 3.0e0 * ZN) / 5.0e0
         XNDEV = (MU - XN) / MU
         YNDEV = (MU - YN) / MU
         ZNDEV = (MU - ZN) / MU
         EPSLON = MAX(ABS(XNDEV),MAX(ABS(YNDEV),ABS(ZNDEV)))
         IF (EPSLON .LT. ERRTOL) THEN
            EA = XNDEV * YNDEV
            EB = ZNDEV * ZNDEV
            EC = EA - EB
            ED = EA - 6.0e0 * EB
            EF = ED + EC + EC
            EB = C4 * ZNDEV
            S1 = ED * (0.25e0 * C3 * ED - 1.5e0 * EB * EF - C1)
            S2 = ZNDEV * (C2 * EF + ZNDEV * (EB * EA - C3 * EC))
            RD = 3.0e0 * SIGMA + POWER4 * (1.0e0+S1+S2) / (MU*SQRT(MU))
            RETURN
         END IF
         XNROOT = SQRT(XN)
         YNROOT = SQRT(YN)
         ZNROOT = SQRT(ZN)
         LAMDA = XNROOT * (YNROOT + ZNROOT) + YNROOT * ZNROOT
         SIGMA = SIGMA + POWER4 / (ZNROOT * (ZN + LAMDA))
         POWER4 = POWER4 * 0.25e0
         XN = (XN + LAMDA) * 0.25e0
         YN = (YN + LAMDA) * 0.25e0
         ZN = (ZN + LAMDA) * 0.25e0
      GO TO 20
      END
