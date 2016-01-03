      double precision function DGAMMA(X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2001-05-25 DGAMMA Krogh Minor change for making .f90 version.
c>> 1996-03-30 DGAMMA Krogh  Added external statement.
C>> 1994-10-20 DGAMMA Krogh  Changes to use M77CON
C>> 1991-10-21 DGAMMA CLL Eliminated DGAM1 as a separate subroutine.
C>> 1991-01-16 DGAMMA Lawson  Replaced D2MACH/R2MACH with DGAM1.
C>> 1985-08-02 DGAMMA Lawson  Initial code.
c--D replaces "?": ?GAMMA, ?ERM1, ?ERV1
C
C ----------------------------------------------------------------------
C
C  THIS ROUTINE CALCULATES THE GAMMA FUNCTION FOR A double precision
C      ARGUMENT X. PERMITS NEGATIVE AS WELL AS POSITIVE X. NOTE
C      THAT THE GAMMA FUNCTION HAS POLES AT ZERO AND AT NEGATIVE
C      ARGUMENTS. COMPUTATION IS BASED ON AN ALGORITHN OUTLINED IN
C      W.J.CODY, 'AN OVERVIEW OF SOFTWARE DEVELOPMENT FOR SPECIAL
C      FUNCTIONS', LECTURE NOTES IN MATHEMATICS, 506, NUMERICAL ANALYSIS
C      DUNDEE, 1975, G. A. WATSON (ED.),SPRINGER VERLAG, BERLIN, 1976.
C      THE PROGRAM USES RATIONAL FUNCTIONS THAT APPROXIMATE THE GAMMA
C      FUNCTION TO AT LEAST 20 SIGNIFICANT DECIMAL DIGITS. COEFFICIENTS
C      FOR THE APPROXIMATION OVER THE INTERVAL (1,2) ARE UNPUBLISHED.
C      THOSE FOR THE APPROXIMATION FOR X .GE. 12 ARE FROM HART, ET. AL.,
C      COMPUTER APPROXIMATIONS, WILEY AND SONS, NEW YORK, 1968.
C      LOWER ORDER APPROXIMATIONS CAN BE SUBSTITUTED FOR THESE ON
C      MACHINES WITH LESS PRECISE ARITHMETIC.
C
C  Designed & programmed by W.J.CODY, Argonne National Lab.,1982.
C  Minor changes for the JPL library by C.L.LAWSON & S.CHAN,JPL,1983.
C
C***********************************************************************
C
c-- Begin mask code changes
C  EXPLANATION OF MACHINE-DEPENDENT CONSTANTS
C
C  EPS    - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
C           1.0 + EPS .GT. 1.0
C  XINF   - THE LARGEST MACHINE REPRESENTABLE FLOATING-POINT NUMBER.
C  XMININ - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
c           both XMININ and 1/XMININ are representable.
C  XGBIG  - A value such that    Gamma(XGBIG) = 0.875 * XINF.
c           (Computed and used in [D/S]GAMMA.)
C  XLBIG  - A value such that LogGamma(XLBIG) = 0.875 * XINF.
c           (Computed and used in [D/S]LGAMA.)
C
C      Values of XINF, XGBIG, and XLBIG for some machines:
C
c        XINF              XGBIG     XLBIG       Machines
c
c  2**127  = 0.170e39      34.81  0.180e37     Vax SP & DP; Unisys SP
c  2**128  = 0.340e39      35.00  0.358e37     IEEE SP
c  2**252  = 0.723e76      57.54  0.376e74     IBM30xx DP
c  2**1023 = 0.899e308    171.46  0.112e306    Unisys DP
c  2**1024 = 0.180e309    171.60  0.2216e306   IEEE DP
c  2**1070 = 0.126e323    177.78  0.1501e320   CDC/7600 SP
c  2**8191 = 0.550e2466   966.94  0.8464e2462  Cray SP & DP
c-- End mask code changes
c
C***********************************************************************
C
C  ERROR RETURNS
C
C  THE PROGRAM RETURNS THE VALUE XINF FOR SINGULARITIES OR
C     WHEN OVERFLOW WOULD OCCUR. THE COMPUTATION IS BELIEVED
C     TO BE FREE OF UNDERFLOW AND OVERFLOW.
C
C  AUTHOR:W. J. CODY
C         APPLIED MATHMATICS DIVISION
C         ARGONNE NATIONAL LABORATORY
C         ARGONNE, IL 60439
C
C  LATEST MODIFICATION by Cody: MAY 18, 1982
C
C     ------------------------------------------------------------------
      external D1MACH
      double precision D1MACH
      double precision C(7), CONST, DEL, EPS, F, FACT, FP, HALF
      double precision ONE,P(8), PI,Q(8), RES,C1
      double precision SUM,TEMP, TWELVE,TWO
      double precision X,X1, X2, XGBIG,XDEN,XINF,XMININ,XNUM
      double precision Y,Y1,YSQ,Z,ZERO
      integer I,J,N
      logical PARITY
C
      save EPS, XGBIG, XMININ, XINF
C
      parameter( ONE = 1.0D0, HALF = 0.5D0, TWO = 2.0d0)
      parameter( ZERO = 0.0D0, TWELVE = 12.0D0)
C
C                      C1 = LOG base e of SQRT(2*PI)
C
      parameter( C1 = 0.9189385332046727417803297D0)
C
      parameter( PI = 3.1415926535897932384626434D0)
C
      data XINF/0.0D0/
C
C ----------------------------------------------------------------------
C  NUMERATOR AND DENOMINATOR COEFFICIENTS FOR RATIONAL MINIMAX
C     APPROXIMATION OVER (1,2).
C ----------------------------------------------------------------------
      DATA P/-1.71618513886549492533811D+0,2.47656508055759199108314D+1,
     *       -3.79804256470945635097577D+2,6.29331155312818442661052D+2,
     *       8.66966202790413211295064D+2,-3.14512729688483675254357D+4,
     *       -3.61444134186911729807069D+4,6.64561438202405440627855D+4/
      DATA Q/-3.08402300119738975254353D+1,3.15350626979604161529144D+2,
     *      -1.01515636749021914166146D+3,-3.10777167157231109440444D+3,
     *        2.25381184209801510330112D+4,4.75584627752788110767815D+3,
     *      -1.34659959864969306392456D+5,-1.15132259675553483497211D+5/
C ----------------------------------------------------------------------
C  COEFFICIENTS FOR MINIMAX APPROXIMATION OVER (12, INF).
C ----------------------------------------------------------------------
      DATA C/-1.910444077728D-03,8.4171387781295D-04,
     *     -5.952379913043012D-04,7.93650793500350248D-04,
     *     -2.777777777777681622553D-03,8.333333333333333331554247D-02,
     *      5.7083835261D-03/
C ----------------------------------------------------------------------
C
      IF (XINF .EQ. ZERO) THEN
        EPS = D1MACH(4)
        XINF = D1MACH(2)
         if(D1MACH(1) * D1MACH(2) .ge. ONE) then
            XMININ = D1MACH(1)
         else
            XMININ = ONE / D1MACH(2)
         endif
c
c                         Compute XGBIG
c
c        XGBIG will satisfy Gamma(XGBIG) = 0.875 * XINF.
c        Use a Newton iteration and the following approximation for
c        the gamma function:
c        log(gamma(x)) ~ (x - .5)*log(x) - x + 0.5 * log(2 * PI)
c
         TEMP = log(0.875d0 * XINF)
         CONST = HALF * log(TWO * PI) - TEMP
         X1 = TEMP * 0.34d0
         do 40 J=1,7
            F = (X1-HALF)*log(X1) - X1 + CONST
            FP = ((X1-HALF)/X1)  + log(X1) - ONE
            DEL = -F/FP
            X2 = X1+DEL
            if(abs(DEL) .lt. 0.5d-5 * X2) go to 45
            X1 = X2
   40    continue
   45    continue
         XGBIG = X2
      END IF
      PARITY = .FALSE.
      FACT = ONE
      N = 0
      Y = X
      IF (Y .GT. ZERO) GO TO 200
C ----------------------------------------------------------------------
C  ARGUMENT IS NEGATIVE OR ZERO
C ----------------------------------------------------------------------
      Y = -X
      J = INT(Y)
      RES = Y - dble(J)
      IF (RES .EQ. ZERO) GO TO 700
      IF (J .NE. (J/2)*2) PARITY = .TRUE.
      FACT = -PI / sin(PI*RES)
      Y = Y + ONE
C ----------------------------------------------------------------------
C  ARGUMENT IS POSITIVE
C ----------------------------------------------------------------------
  200 IF (Y .LT. EPS) GO TO 650
      IF (Y .GE. TWELVE) GO TO 300
      Y1 = Y
      IF (Y .GE. ONE) GO TO 210
C ----------------------------------------------------------------------
C  0.0 .LT. ARGUMENT .LT. 1.0
C ----------------------------------------------------------------------
      Z = Y
      Y = Y + ONE
      GO TO 250
C ----------------------------------------------------------------------
C  1.0 .LT. ARGUMENT .LT. 12.0, REDUCE ARGUMENT IF NECESSARY
C ----------------------------------------------------------------------
  210 N= int(Y) - 1
      Y = Y - dble(N)
      Z = Y - ONE
C ----------------------------------------------------------------------
C  EVALUATE APPROXIMATION FOR 1.0 .LT. ARGUMENT .LT. 2.0
C ----------------------------------------------------------------------
  250 XNUM = ZERO
      XDEN = ONE
      DO 260 I = 1, 8
         XNUM = (XNUM + P(I)) * Z
         XDEN = XDEN * Z + Q(I)
  260 CONTINUE
      RES = (XNUM / XDEN + HALF) + HALF
      IF (Y .EQ. Y1) GO TO 900
      IF (Y1 .GT. Y) GO TO 280
C ----------------------------------------------------------------------
C  ADJUST RESULT FOR CASE 0.0 .LT. ARGUMENT .LT. 1.0
C ----------------------------------------------------------------------
      RES = RES / Y1
      GO TO 900
C ----------------------------------------------------------------------
C  ADJUST RESULT FOR CASE 2.0 .LT. 12.0
C ----------------------------------------------------------------------
  280 DO 290 I = 1, N
         RES = RES * Y
         Y = Y + ONE
  290 CONTINUE
      GO TO 900
C ----------------------------------------------------------------------
C  EVALUATE FOR ARGUMENT .GE. 12.0
C ----------------------------------------------------------------------
  300 IF (Y .GT. XGBIG) GO TO 720
      YSQ = Y * Y
      SUM = C(7)
      DO 350 I = 1, 6
         SUM = SUM / YSQ + C(I)
  350 CONTINUE
      SUM = ((SUM/Y + C1) - Y) + (Y - HALF) * log(Y)
      RES = exp(SUM)
      GO TO 900
C ----------------------------------------------------------------------
C  ARGUMENT .LT. EPS
C ----------------------------------------------------------------------
  650 IF (Y .LT. XMININ) GO TO 740
      RES = ONE / Y
      GO TO 900
C ----------------------------------------------------------------------
C  RETURN FOR SINGULARITIES,EXTREME ARGUMENTS, ETC.
C ----------------------------------------------------------------------
  700 CALL DERM1('DGAMMA',1,0,'POLE AT 0 AND NEG INTEGERS',
     *           'X',X,'.')
      GO TO 780
C
  720 CALL DERM1('DGAMMA',2,0,'X SO LARGE VALUE WOULD OVERFLOW',
     *           'X',X,',')
      CALL DERV1('LIMIT',XGBIG,'.')
      GO TO 780
C
  740 CALL DERM1('DGAMMA',3,0,'X TOO NEAR TO A SINGULARITY.'//
     *           'VALUE WOULD OVERFLOW.','X',X,'.')
C
  780 DGAMMA = XINF
      GO TO 950
C ----------------------------------------------------------------------
C  FINAL ADJUSTMENTS AND RETURN
C ----------------------------------------------------------------------
  900 IF (PARITY) RES = -RES
      IF (FACT .NE. ONE) RES = FACT / RES
      DGAMMA = RES
  950 RETURN
      END
