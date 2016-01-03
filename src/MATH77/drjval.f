      SUBROUTINE DRJVAL (X, Y, Z, R, RJ, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>>   2001-07-16 DRJVAL Krogh  Change -1.0 to -1.d0.
c>>   1996-03-30 DRJVAL Krogh  Added external statement.
C>>   1995-11-17 DRJVAL Krogh  Converted SFTRAN to Fortran 77.
c>>   1994-10-19 DRJVAL Krogh  Changes to use M77CON
c>>   1994-08-15 DRJVAL WV Snyder JPL use 2-arg min and max for C conv.
C>>   1992-04-17 DRJVAL WV Snyder JPL
C>>   1992-04-13 DRJVAL WV Snyder JPL Give RJ a value even if ierr.ne.0
C>>   1992-04-08 DRJVAL WV Snyder JPL Declare RCX, spell YN correctly.
C>>   1990-12-20 DRJVAL WV Snyder JPL Convert from NSWC for Math 77.
C
C     THIS SUBROUTINE COMPUTES THE INCOMPLETE ELLIPTIC INTEGRAL
C     OF THE THIRD KIND
C
C     RJ(X,Y,Z,R) = INTEGRAL FROM ZERO TO INFINITY OF
C
C                             -1/2     -1/2     -1/2     -1
C                   (3/2)(T+X)    (T+Y)    (T+Z)    (T+R)  DT,
C
C     WHERE X, Y, AND Z ARE NONNEGATIVE, AT MOST ONE OF THEM IS
C     ZERO, AND R IS NONZERO.  IF X OR Y OR Z IS ZERO, THE INTE-
C     GRAL IS COMPLETE.  IF R IS NEGATIVE, THE CAUCHY PRINCIPAL
C     VALUE IS COMPUTED BY USING A PRELIMINARY TRANSFORMATION
C     TO MAKE R POSITIVE; SEE EQUATION (2.22) OF THE SECOND REF-
C     ERENCE BELOW.  WHEN R IS POSITIVE, THE DUPLICATION THEOREM
C     IS ITERATED UNTIL THE VARIABLES ARE NEARLY EQUAL, AND THE
C     FUNCTION IS THEN EXPANDED IN TAYLOR SERIES TO FIFTH ORDER.
C     REFERENCES: B. C. CARLSON AND E. M. NOTIS, ALGORITHMS FOR
C     INCOMPLETE ELLIPTIC INTEGRALS, ACM TRANSACTIONS ON MATHEMA-
C     TICAL SOFTWARE, 7 (1981), 398-403; B. C. CARLSON, COMPUTING
C     ELLIPTIC INTEGRALS BY DUPLICATION, NUMER. MATH. 33 (1979),
C     1-16.
C     AUTHORS: B. C. CARLSON AND ELAINE M. NOTIS, AMES LABORATORY-
C     DOE, IOWA STATE UNIVERSITY, AMES, IA 50011, AND R. L. PEXTON,
C     LAWRENCE LIVERMORE NATIONAL LABORATORY, LIVERMORE, CA 94550.
C     AUG. 1, 1979, REVISED SEPT. 1, 1987.
C
C     CHECK VALUES: RJ(2,3,4,5) = 0.14297 57966 71567 53833 23308
C                  RJ(2,3,4,-5) = - 0.12711 23004 29638 83590 80083
C     CHECK BY ADDITION THEOREM: RJ(X,X+Z,X+W,X+P)
C        + RJ(Y,Y+Z,Y+W,Y+P) + (A-B) * RJ(A,B,B,A) + 3 / DSQRT(A)
C        = RJ(0,Z,W,P), WHERE X,Y,Z,W,P ARE POSITIVE AND X * Y
C        = Z * W,  A = P * P * (X+Y+Z+W),  B = P * (P+X) * (P+Y),
C        AND B - A = P * (P-Z) * (P-W).  THE SUM OF THE THIRD AND
C        FOURTH TERMS ON THE LEFT SIDE IS 3 * RC(A,B).
C
C     *****     Formal Arguments     ***********************************
C
C     INPUT ...
C
C     X, Y, Z, AND R ARE THE VARIABLES IN THE INTEGRAL RJ(X,Y,Z,R).
C
C     OUTPUT ...
C
C     RJ IS THE VALUE OF THE INCOMPLETE ELLIPTIC INTEGRAL.
C
C     IERR IS THE RETURN ERROR CODE.
C          IERR = 0  FOR NORMAL COMPLETION OF THE SUBROUTINE.
C          IERR = 1  X, Y, OR Z IS NEGATIVE, OR R = 0
C          IERR = 2  X+Y, X+Z, Y+Z, OR ABS(R) IS TOO SMALL.
C          IERR = 3  X, Y, Z, OR ABS(R) IS TOO LARGE.
C
      DOUBLE PRECISION X, Y, Z, R, RJ
      INTEGER IERR
C
C--D replaces "?": ?ERM1, ?ERV1, ?RCVAL, ?RFVLX, ?RJVAL
C
C     *****     External References     ********************************
C
C     DRCVAL COMPUTES RC.  IT CHECKS THE ARGUMENTS, BUT IT'S NOT
C     CALLED FROM WITHIN THE INNER LOOP.  THERE, WE INLINE DRCVAL.
C     DRFVLX COMPUTES RF, BUT ASSUMES ARGUMENTS ARE IN RANGE.
C     DRFVLX IS INSIDE SRFVAL.
C
      EXTERNAL D1MACH
      DOUBLE PRECISION D1MACH
C
C     *****     Local Variables     ************************************
C
      DOUBLE PRECISION A,B,ALFA,BETA,C1,C2,C3,C4,DELTA,EA,EB,EC,E1,E2,E3
      DOUBLE PRECISION EPSLON,ERRTOL,ETOLRC,FIFTH,GAMMA,LAMDA,LOLIM,MU
      DOUBLE PRECISION POWER4,RC,RCX,RF,RN,RNDEV,SIGMA,S1,S2,S3,SN,THIRD
      DOUBLE PRECISION UPLIM,XN,XNDEV,XNROOT,YN,YNDEV,YNROOT,YY
      DOUBLE PRECISION ZN,ZNDEV,ZNROOT,Z1,Z2,Z3,Z4
      PARAMETER (C1 = 3.0d0 / 14.0d0)
      PARAMETER (C2 = 1.0d0 / 3.0d0)
      PARAMETER (C3 = 3.0d0 / 22.0d0)
      PARAMETER (C4 = 3.0d0 / 26.0d0)
      PARAMETER (FIFTH = 1.0d0 / 5.0d0)
      PARAMETER (THIRD = 1.0d0 / 3.0d0)
      PARAMETER (Z1 = 3.0d0 / 10.0d0)
      PARAMETER (Z2 = 1.0d0 / 7.0d0)
      PARAMETER (Z3 = 3.0d0 / 8.0d0)
      PARAMETER (Z4 = 9.0d0 / 22.0d0)
      SAVE ERRTOL, ETOLRC, LOLIM, UPLIM
C
C     MACHINE DEPENDENT PARAMETERS ...
C
C     LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
C     LOLIM IS NOT LESS THAN THE CUBE ROOT OF THE VALUE
C     OF LOLIM USED IN THE CODE TO COMPUTE RC, AND
C     UPLIM IS NOT GREATER THAN 0.3 TIMES THE CUBE ROOT OF
C     THE VALUE OF UPLIM USED IN THE CODE TO COMPUTE RC.
C
C     ERRTOL IS SET TO THE DESIRED ERROR TOLERANCE. THE
C     RELATIVE ERROR DUE TO TRUNCATION OF THE SERIES FOR RJ
C     IS LESS THAN 3 * ERRTOL ** 6 / (1 - ERRTOL) ** 3/2.
C     ERRTOL / 2 IS USED IN THE CODE TO COMPUTE RC TO MAKE
C     THE TRUNCATION ERROR FOR RC LESS THAN FOR RJ.
C
      DATA LOLIM /-1.0d0/
C
C ----------------------------------------------------------------------
C     WARNING. CHANGES IN THE PROGRAM MAY IMPROVE SPEED AT THE
C     EXPENSE OF ROBUSTNESS.
C ----------------------------------------------------------------------
C
      IF (MIN(X,MIN(Y,Z)) .LT. 0.0d0 .OR. R .EQ. 0.0d0) THEN
         RJ = 0.0d0
         IERR = 1
         CALL DERM1 ('DRJVAL',1,0,
     1      'ONE OF X, Y, or Z is negative, or R is zero','X',X,',')
         CALL DERV1 ('Y',Y,',')
         CALL DERV1 ('Z',Z,',')
         CALL DERV1 ('R',R,'.')
         RETURN
      END IF
      IF (LOLIM .LT. 0.0) THEN
         LOLIM = 1.0001d0 * (5.0d0 * D1MACH(1))**THIRD
         UPLIM = .29999d0 * (D1MACH(2) / 5.0d0)**THIRD
         ERRTOL = (0.28d0 * D1MACH(4))**(1.0d0/6.0d0)
         ETOLRC = 0.5d0 * ERRTOL
      END IF
      IF (MAX(X,MAX(Y,MAX(Z,ABS(R)))) .GT. UPLIM) THEN
         RJ = 0.0d0
         IERR = 3
         CALL DERM1 ('DRJVAL',3,0,'ONE OF X, Y, Z or ABS(R) > UPLIM',
     1      'X',X,',')
         CALL DERV1 ('Y',Y,',')
         CALL DERV1 ('Z',Z,',')
         CALL DERV1 ('R',R,',')
         CALL DERV1 ('UPLIM',UPLIM,'.')
         RETURN
      END IF
      IF (MIN(X+Y,MIN(X+Z,MIN(Y+Z,ABS(R)))) .LT. LOLIM) THEN
         RJ = 0.0d0
         IERR = 2
         CALL DERM1 ('DRJVAL',2,0,
     1      'ONE OF X+Y, X+Z, Y+Z or ABS(R) < LOLIM','X',X,',')
         CALL DERV1 ('Y',Y,',')
         CALL DERV1 ('Z',Z,',')
         CALL DERV1 ('R',R,',')
         CALL DERV1 ('LOLIM',LOLIM,'.')
         RETURN
      END IF
C
      IERR = 0
      IF (R .GT. 0.0d0) THEN
         XN = X
         YN = Y
         ZN = Z
         RN = R
      ELSE
C        ORDER X,Y,Z AND TRANSFORM TO POSITIVE R
         XN = MIN(X,Y)
         YY = MAX(X,Y)
         ZN = MAX(YY,Z)
         YY = MIN(YY,Z)
         YN = MAX(XN,YY)
         XN = MIN(XN,YY)
         A = 1.0d0 / (YN - R)
         B = (ZN - YN) * A * (YN - XN)
         RN = YN + B
         ALFA = XN * ZN / YN
         BETA = R * RN / YN
         CALL DRCVAL (ALFA, BETA, RCX, IERR)
         IF (IERR .NE. 0) THEN
            RJ = 0.0d0
            RETURN
         END IF
      END IF
      SIGMA = 0.0d0
      POWER4 = 1.0d0
C
   20 CONTINUE
         MU = (XN + YN + ZN + RN + RN) * FIFTH
         XNDEV = (MU - XN) / MU
         YNDEV = (MU - YN) / MU
         ZNDEV = (MU - ZN) / MU
         RNDEV = (MU - RN) / MU
         EPSLON = MAX(ABS(XNDEV),MAX(ABS(YNDEV),MAX(ABS(ZNDEV),
     1                ABS(RNDEV))))
         IF (EPSLON .LT. ERRTOL) GO TO 80
         XNROOT = SQRT(XN)
         YNROOT = SQRT(YN)
         ZNROOT = SQRT(ZN)
         LAMDA = XNROOT * (YNROOT + ZNROOT) + YNROOT * ZNROOT
         ALFA = RN * (XNROOT + YNROOT + ZNROOT) + XNROOT*YNROOT*ZNROOT
         ALFA = ALFA * ALFA
         BETA = RN * (RN + LAMDA) * (RN + LAMDA)
C        CALL DRCVAL (ALFA, BETA, RC, IERR)
C        IF (IERR .NE. 0) RETURN
C        We use the following instead of calling DRCVAL to avoid the
C        tests for argument range, which we know will succeed.
   40    CONTINUE
            DELTA = (ALFA + BETA + BETA) * THIRD
            SN = (BETA + DELTA) / DELTA - 2.0d0
            IF (ABS(SN) .LT. ETOLRC) GO TO 60
            GAMMA = 2.0 * SQRT(ALFA) * SQRT(BETA) + BETA
            ALFA = (ALFA + GAMMA) * 0.25d0
            BETA = (BETA + GAMMA) * 0.25d0
         GO TO 40
   60    CONTINUE
         RC = (1.0d0 + SN*SN*(Z1 + SN*(Z2 + SN*(Z3+SN*Z4))))/SQRT(DELTA)
         SIGMA = SIGMA + POWER4 * RC
         POWER4 = POWER4 * 0.25d0
         XN = (XN + LAMDA) * 0.25d0
         YN = (YN + LAMDA) * 0.25d0
         ZN = (ZN + LAMDA) * 0.25d0
         RN = (RN + LAMDA) * 0.25d0
      GO TO 20
   80 CONTINUE
C
      E1 = YNDEV * ZNDEV
      EA = XNDEV * (YNDEV + ZNDEV) + E1
      EB = XNDEV * E1
      EC = RNDEV * RNDEV
      E2 = EA - 3.0d0 * EC
      E3 = EB + 2.0d0 * RNDEV * (EA - EC)
      S1 = 1.0d0 + E2 * (-C1 + 0.75d0 * C3 * E2 - 1.5d0 * C4 * E3)
      S2 = EB * (0.5d0 * C2 + RNDEV * (- C3 - C3 + RNDEV * C4))
      S3 = RNDEV * EA * (C2 - RNDEV * C3) - C2 * RNDEV * EC
      RJ = 3.0d0 * SIGMA + POWER4 * (S1 + S2 + S3) / (MU * SQRT(MU))
C
      IF (R .GT. 0.d0) RETURN
      CALL DRFVLX (XN,YN,ZN,RF)
      RJ = A * (B * RJ + 3.0d0 * (RCX - RF))
      RETURN
      END
