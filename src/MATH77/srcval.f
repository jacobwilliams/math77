      SUBROUTINE SRCVAL (X, Y, RC, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>>   2001-07-16 SRCVAL Krogh  Change -1.0 to -1.e0.
c>>   1996-03-30 SRCVAL Krogh  Added external statement.
C>>   1995-11-17 SRCVAL Krogh  Converted SFTRAN to Fortran 77.
C>>   1994-10-19 SRCVAL Krogh  Changes to use M77CON
C>>   1991-10-31 SRCVAL WV Snyder JPL Incorporate changes from Carlson
C>>   1990-12-20 SRCVAL WV Snyder JPL Convert from NSWC for Math 77.
C
C     THIS SUBROUTINE COMPUTES THE ELEMENTARY INTEGRAL
C
C     RC(X,Y) = INTEGRAL FROM ZERO TO INFINITY OF
C
C                         -1/2     -1
C               (1/2)(T+X)    (T+Y)  DT,
C
C     WHERE X IS NONNEGATIVE AND Y IS NONZERO.  IF Y IS NEGATIVE,
C     THE CAUCHY PRINCIPAL VALUE IS COMPUTED BY USING A PRELIMI-
C     NARY TRANSFORMATION TO MAKE Y POSITIVE; SEE EQUATION (2.12)
C     OF THE SECOND REFERENCE BELOW.  WHEN Y IS POSITIVE, THE
C     DUPULICATION THEOREM IS ITERATED UNTIL THE VARIABLES ARE
C     NEARLY EQUAL, AND THE FUNCTION IS THEN EXPANDED IN TAYLOR
C     SERIES TO FIFTH ORDER.  LOGARITHMIC, INVERSE CIRCULAR, AND
C     INVERSE HYPERBOLIC FUNCTIONS ARE EXPRESSED IN TERMS OF RC
C     BY EQUATIONS (4.9)-(4.13) OF THE SECOND REFERENCE BELOW.
C
C     REFERENCES: B. C. CARLSON AND E. M. NOTIS, ALGORITHMS FOR
C     INCOMPLETE ELLIPTIC INTEGRALS, ACM TRANSACTIONS ON MATHEMA-
C     TICAL SOFTWARE, 7 (1981), 398-403;
C     B. C. CARLSON, COMPUTING ELLIPTIC INTEGRALS BY DUPLICATION,
C     NUMER. MATH. 33 (1979), 1-16.
C     AUTHORS: B. C. CARLSON AND ELAINE M. NOTIS, AMES LABORATORY-
C     DOE, IOWA STATE UNIVERSITY, AMES, IA 50011, AND R. L. PEXTON,
C     LAWRENCE LIVERMORE NATIONAL LABORATORY, LIVERMORE, CA 94550.
C     AUG. 1, 1979, REVISED SEPT. 1, 1987.
C
C     CHECK VALUES: RC(0,1/4) = RC(1/16,1/8) = PI,
C                   RC(9/4,2) = LN(2),
C                   RC(1/4,-2) = LN(2)/3.
C     CHECK BY ADDITION THEOREM: RC(X,X+Z) + RC(Y,Y+Z) = RC(0,Z),
C     WHERE X, Y, AND Z ARE POSITIVE AND  X * Y = Z * Z.
C
C    *****     Formal Arguments     ***********************************
C
C          LOLIM AND UPLIM DETERMINE THE RANGE OF VALID ARGUMENTS.
C     LOLIM IS NOT LESS THAN THE MACHINE MINIMUM MULTIPLIED BY 5.
C     UPLIM IS NOT GREATER THAN THE MACHINE MAXIMUM DIVIDED BY 5.
C
C     INPUT ...
C
C     X AND Y ARE THE VARIABLES IN THE INTEGRAL RC(X,Y).
C
C     OUTPUT ...
C
C     RC IS THE VALUE OF THE INTEGRAL.
C
C     IERR IS THE RETURN ERROR CODE.
C          0  FOR NORMAL COMPLETION OF THE SUBROUTINE.
C          1  X IS NEGATIVE, OR Y = 0.
C          2  X+ABS(Y) IS TOO SMALL.
C          3  X OR ABS(Y) IS TOO LARGE, OR X + ABS(Y) IS TOO LARGE.
C          4  Y < -2.236/SQRT(LOLIM) AND 0 < X < (LOLIM*UPLIM)**2/25
C
      REAL             X, Y, RC
      INTEGER IERR
C
C--S replaces "?": ?ERM1, ?ERV1, ?RCVAL
C
C     *****     External References     ********************************
C
      EXTERNAL R1MACH
      REAL             R1MACH
C
C     *****     Local Variables     ************************************
C
      REAL             C1,C2,C3,C4,ERRTOL,LAMDA,LOLIM
      REAL             MU,SN,UPLIM,XLU225,XN,Y2236L,YN,W
      PARAMETER (C1 = 3.0e0 / 10.0e0)
      PARAMETER (C2 = 1.0e0 / 7.0e0)
      PARAMETER (C3 = 3.0e0 / 8.0e0)
      PARAMETER (C4 = 9.0e0 / 22.0e0)
      SAVE ERRTOL, LOLIM, UPLIM, XLU225, Y2236L
      DATA LOLIM /-1.0e0/
C
C     ERRTOL IS SET TO THE DESIRED ERROR TOLERANCE.
C     RELATIVE ERROR DUE TO TRUNCATION IS LESS THAN
C     16 * ERRTOL ** 6 / (1 - 2 * ERRTOL).
C
C     SAMPLE CHOICES   ERRTOL   RELATIVE TRUNCATION
C                               ERROR LESS THAN
C                      1.E-3    2.E-17
C                      3.E-3    2.E-14
C                      1.E-2    2.E-11
C                      3.E-2    2.E-8
C                      1.E-1    2.E-5
C
C     We could put in a Newton iteration to solve for ERRTOL in
C     terms of R1MACH(4), but it seems good enough to put
C     ERRTOL = (R1MACH(4)/16.0)**(1.0/6.0)
C
c ----------------------------------------------------------------------
C          WARNING. CHANGES IN THE PROGRAM MAY IMPROVE SPEED AT THE
C          EXPENSE OF ROBUSTNESS.
c ----------------------------------------------------------------------
C
      IF (X .LT. 0.0e0 .OR. Y .EQ. 0.0e0) THEN
         RC = 0.0e0
         CALL SERM1 ('SRCVAL',1,0,'X < 0 or Y = 0','X',X,',')
         CALL SERV1 ('Y',Y,'.')
         IERR = 1
         RETURN
      END IF
      IF (LOLIM .LT. 0.0e0) THEN
         LOLIM = 5.0e0 * R1MACH(1)
         UPLIM = R1MACH(2) / 5.0e0
         XLU225 = (LOLIM*UPLIM)**2 / 25.0e0
         Y2236L = -2.236e0/SQRT(LOLIM)
         ERRTOL = (R1MACH(4)/16.0e0) ** (1.0e0/6.0e0)
      END IF
      YN = ABS(Y)
      IF (X .GT. UPLIM .OR. YN .GT. UPLIM) THEN
         RC = 0.0e0
       CALL SERM1 ('SRCVAL',3,0,'X > UPLIM or ABS(Y) > UPLIM','X',X,',')
         CALL SERV1 ('Y',Y,',')
         CALL SERV1 ('UPLIM',UPLIM,'.')
         IERR = 3
         RETURN
      END IF
      IF (X + YN .LT. LOLIM) THEN
         RC = 0.0e0
         CALL SERM1 ('SRCVAL',2,0,'X + ABS(Y) < LOLIM','X',X,',')
         CALL SERV1 ('Y',Y,',')
         CALL SERV1 ('LOLIM',LOLIM,'.')
         IERR = 2
         RETURN
      END IF
      IF (X + YN .GT. UPLIM) THEN
         RC = 0.0e0
         CALL SERM1 ('SRCVAL',3,0,'X + ABS(Y) > UPLIM','X',X,',')
         CALL SERV1 ('Y',Y,',')
         CALL SERV1 ('UPLIM',UPLIM,'.')
         IERR = 3
         RETURN
      END IF
      IF (Y.LT.Y2236L .AND. X.GT.0.0e0 .AND. X.LT.XLU225) THEN
         RC = 0.0e0
         CALL SERM1 ('SRCVAL',4,0,
     1      'Y < -2.236/SQRT(LOLIM) AND 0 < X < (LOLIM*UPLIM)**2/25',
     2      'X',X,',')
         CALL SERV1 ('Y',Y,',')
         CALL SERV1 ('LOLIM',LOLIM,',')
         CALL SERV1 ('UPLIM',UPLIM,'.')
         IERR = 4
         RETURN
      END IF
C
      IERR = 0
      IF (Y.GT.0.0e0) THEN
         XN = X
         W = 1.0e0
      ELSE
C        TRANSFORM TO POSITIVE Y
         XN = X - Y
         W = SQRT(X) / SQRT(XN)
      END IF
C
   20 CONTINUE
         MU = (XN + YN + YN) / 3.0e0
         SN = (YN + MU) / MU - 2.0e0
         IF (ABS(SN) .LT. ERRTOL) THEN
            RC = W * (1.0e0+SN*SN*(C1+SN*(C2+SN*(C3+SN*C4)))) / SQRT(MU)
            RETURN
         END IF
         LAMDA = 2.0 * SQRT(XN) * SQRT(YN) + YN
         XN = (XN + LAMDA) * 0.25e0
         YN = (YN + LAMDA) * 0.25e0
      GO TO 20
      END
