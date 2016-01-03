      real             function SLGAMA(X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 SLGAMA Krogh  Added external statement.
C>> 1995-11-16 SLGAMA Krogh  SFTRAN => Fortran 77
C>> 1995-10-24 SLGAMA Krogh  Removed blanks in numbers for C conversion.
C>> 1994-10-19 SLGAMA Krogh  Changes to use M77CON
C>> 1992-05-19 SLGAMA CLL Corrected computation of FRTBIG.
C>> 1991-10-21 SLGAMA CLL Eliminated DGAM1 as a separate subroutine.
C>> 1991-01-16 SLGAMA Lawson  Using DGAM1 in place of D2MACH/R2MACH
C>> 1986-03-18 SLGAMA Lawson  Initial code.
c--S replaces "?": ?LGAMA, ?GAMMA, ?RAT1, ?ERM1, ?ERV1
C
C     Designed and programmed by W.J.CODY, Argonne National Lab, 1982.
C     Minor changes for use in the JPL MATH77 library by C.L.LAWSON &
C     S.CHAN, JPL, 1983.
C  1992-05-19 CLL.  Noted that FRTBIG was being computed using XLBIG
c  before XLBIG was computed.  Thus branch on Y .GT. FRTBIG was
c  unreliable and was likely to cause wrong formulas to be used for
c  X between 12 and the correct value of FRTBIG.  Corrected this.
C ----------------------------------------------------------------------
C This routine calculated the LOG(GAMMA) function for a double
C     precision argument X. Computation is based on an algorithm
C     outlined in references 1 and 2. The program uses rational
C     functions that approximate LOG(GAMMA) to least 18
C     significant decimal digits. Approximations for X .LT. 12.0
C     are unpublished. Lower order approximations can be
C     substituted on machines with less precise arithmetic.
C
C  Explanaton of machine-dependent constants
C
C  XINF     The largest machine representable floating-point
C           number.
C
C  EPS      The smallest positive floating-point number such
C           that 1.0 + EPS .GT. 1.0
C
C  XGBIG  - A value such that    Gamma(XGBIG) = 0.875 * XINF.
c           (Computed and used in [D/S]GAMMA.)
C  XLBIG  - A value such that LogGamma(XLBIG) = 0.875 * XINF.
c           (Computed and used in [D/S]LGAMA.)
C
C  FRTBIG   The fourth root of XLBIG.
C
C-- Begin mask code changes
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
C-- End mask code changes
c
c ----------------------------------------------------------------------
C
C     Error Messages
C     1) X .LE. 0., setting result large.
C     2) X too large., setting result large.
C
c     ------------------------------------------------------------------
      external         SRAT1, SGAMMA, R1MACH, SERM1, SERV1
      real             SRAT1, SGAMMA, R1MACH
      real             ALN4, C,CORR,D2,D4, DEL
      real             EPS, FIVE, FRTBIG,FOUR,HALF
      real             LOMEGA, ONE
      real             P2,P4, P65,Q2,Q4,RES,SQRTPI
      real             T1, T5, THRHAL,TWELVE, X,XLBIG,XDEN
      real             XINF,XM1,XM2,XM4,XNUM
      real             Y, Y1, Y2, YSQ, ZERO
      integer I, J
      dimension C(7),P2(8),P4(8),Q2(8),Q4(8)
C
      save XLBIG,XINF,EPS,FRTBIG
C
      DATA XINF/0.E0/
      DATA ONE,HALF,TWELVE,ZERO/1.0E0,0.5E0,12.0E0,0.0E0/
      DATA FOUR,FIVE,THRHAL / 4.E0,5.E0,1.5E0 /
      DATA SQRTPI/0.9189385332046727417803297E0/
      DATA P65 / 0.65E0 /
C                                           T5 = LN(SQRT(4*PI)) - HALF
      DATA T5 /0.76551212348464539649E0/
C                                           ALN4 = LN(4)
      DATA ALN4/1.3862943611198906188E0/
c ----------------------------------------------------------------------
C  NUMERATOR AND DENOMINATOR COEFFICIENTS FOR RATIONAL MINIMAX
C     APPROXIMATION OVER (1.5,4.0).
c ----------------------------------------------------------------------
      DATA D2/4.227843350984671393993777E-1/
      DATA P2/4.974607845568932035012064E0,5.424138599891070494101986E2,
     *        1.550693864978364947665077E4,1.847932904445632425417223E5,
     *        1.088204769468828767498470E6,3.338152967987029735917223E6,
     *        5.106661678927352456275255E6,3.074109054850539556250927E6/
      DATA Q2/1.830328399370592604055942E2,7.765049321445005871323047E3,
     *        1.331903827966074194402448E5,1.136705821321969608938755E6,
     *        5.267964117437946917577538E6,1.346701454311101692290052E7,
     *        1.782736530353274213975932E7,9.533095591844353613395747E6/
c ----------------------------------------------------------------------
C  NUMERATOR AND DENOMINATOR COEFFICIENTS FOR RATIONAL MINIMAX
C     APPROXIMATION OVER (4.0,12.0)
c ----------------------------------------------------------------------
      DATA D4/1.791759469228055000094023E0/
      DATA P4/1.474502166059939948905062E4,2.426813369486704502836312E6,
     *        1.214755574045093227939592E8,2.663432449630976949898078E9,
     *      2.940378956634553899906876E10,1.702665737765398868392998E11,
     *      4.926125793377430887588120E11,5.606251856223951465078242E11/
      DATA Q4/2.690530175870899333379843E3,6.393885654300092398984238E5,
     *        4.135599930241388052042842E7,1.120872109616147941376570E9,
     *      1.488613728678813811542398E10,1.016803586272438228077304E11,
     *      3.417476345507377132798597E11,4.463158187419713286462081E11/
c ----------------------------------------------------------------------
C  COEFFICIENTS FOR MINIMAX APPROXIMATION OVER (12,INF).
c ----------------------------------------------------------------------
      DATA C/-1.910444077728E-03,8.4171387781295E-04,
     *     -5.952379913043012E-04,7.93650793500350248E-04,
     *     -2.777777777777681622553E-03,8.333333333333333331554247E-02,
     *      5.7083835261E-03/
c ----------------------------------------------------------------------
      IF (XINF .EQ. ZERO) THEN
        XINF = R1MACH(2)
        EPS  = R1MACH(4)
c                           Compute XLBIG
c
c        XLBIG must satisfy  LogGamma(XLBIG) = 0.875 * XINF.
c        Solve this equation by successive substitution using the
c        approximation: log(gamma(x)) ~ x*(log(x) - 1).
c        Scale this equation by substituting y * 0.875 * XINF for x,
c        and solve the scaled equation for y.  y will range from
c        0.0121 when XINF = 2**127 to 0.000176 when XINF = 2**8191.
c
         LOMEGA = log(0.875e0 * XINF)
         Y1 = 0.01e0
         do 10 J=1,7
            Y2 = 1.0e0 /(LOMEGA + log(Y1) - 1.0e0)
            DEL = Y2 - Y1
            if(abs(DEL) .lt. 0.5e-5 * Y2) go to 20
            Y1 = Y2
   10    continue
   20    continue
         XLBIG = 0.875e0 * XINF * Y2
         FRTBIG = sqrt(sqrt(XLBIG))
      ENDIF
C
      Y = X
      IF (Y .LE. ZERO) THEN
        CALL SERM1('SLGAMA',2,0,'X .LE. 0.,SETTING RESULT LARGE','X',
     *             X,'.')
        GO TO 700
      ENDIF
      IF (Y .GT. XLBIG) THEN
        CALL SERM1('SLGAMA',2,0,'X TOO LARGE,SETTING RESULT LARGE','X',
     *             X,',')
        CALL SERV1('LIMIT FOR X',XLBIG,'.')
        GO TO 700
      ENDIF
      IF (Y .GT. TWELVE) GO TO 400
        IF (Y .GT. FIVE) THEN
          RES = log(SGAMMA(Y))
          GO TO 900
        END IF
      IF (Y .GT. FOUR) GO TO 300
      IF (Y .GT. THRHAL) GO TO 200
C
      IF (Y .GE. P65) THEN
        XM1 = (Y-HALF) - HALF
        RES = SRAT1(XM1)
      ELSE IF (Y .GT. HALF) THEN
C
C *** Here for .5 < Y < .65 we use the formula
C     LGAM(Y) = LGAM(2Y) - LGAM(Y+ 1/2) - Y*LOG(4) + LOG(SQRT(4*PI))
C
        XM1 = Y - HALF
        T1 = -SRAT1(XM1)
        XM1 = XM1 + XM1
        RES = (((T1+SRAT1(XM1)) - Y * ALN4) + HALF) + T5
      ELSE IF (Y .GT. EPS) THEN
        XM1 = Y
        RES = SRAT1(XM1) - log(Y)
      ELSE
        RES = -log(Y)
      END IF
      GO TO 900
c ----------------------------------------------------------------------
C  1.5 .LT. X .LE. 4.0
c ----------------------------------------------------------------------
  200 CONTINUE
      XM2 = (Y - ONE) - ONE
      XDEN = ONE
      XNUM = ZERO
      DO 240 I = 1,8
      XNUM = XNUM*XM2 + P2(I)
      XDEN = XDEN*XM2 + Q2(I)
  240 CONTINUE
      RES = XM2 * (D2 + XM2*(XNUM/XDEN))
      GO TO 900
c ---------------------------------------------------------------------
C  4.0 .LT. X .LE. 5.0
c ---------------------------------------------------------------------
  300 CONTINUE
      XM4 = Y - FOUR
      XDEN = -ONE
      XNUM = ZERO
      DO 340 I = 1,8
         XNUM = XNUM*XM4 + P4(I)
         XDEN = XDEN*XM4 + Q4(I)
  340 CONTINUE
      RES = D4 + XM4*(XNUM/XDEN)
      GO TO 900
c ---------------------------------------------------------------------
C  EVALUATE FOR ARGUMENT .GE. 12.0
c ---------------------------------------------------------------------
  400 CONTINUE
      RES = ZERO
      IF (Y .GT. FRTBIG) GO TO 460
      RES = C(7)
      YSQ = Y * Y
      DO 450 I = 1, 6
         RES = RES / YSQ + C(I)
  450 CONTINUE
  460 CONTINUE
      RES = RES / Y
      CORR = log(Y)
      RES = RES + SQRTPI - HALF*CORR
      RES = RES + Y*(CORR-ONE)
      GO TO 900
c ---------------------------------------------------------------------
C  RETURN FOR BAD ARGUMENTS
c ---------------------------------------------------------------------
  700 CONTINUE
      RES = XINF
c ---------------------------------------------------------------------
C  FINAL ADJUSTMENTS AND RETURN
c ---------------------------------------------------------------------
  900 CONTINUE
      SLGAMA = RES
      RETURN
      END
c     ==================================================================
      real             function SRAT1(XM1)
C
C  Evaluate the rational function RAT1(XM1) which
C  with XM1 = X - 1 approximates LOG(GAMMA(X))
C  for .5 .LE. X .LE. 1.5 . This rational function
C  has poor error amplification properties for
C  .5 .le. X .le. 0.65 so we use it for
C  0.65 .le. X .le. 1.5.
c
c  D1, P1(), and Q1() are coefficients for a rational minimax
c  approximation over (0.5, 1.5).
c     ------------------------------------------------------------------
      integer I
      real             D1, P1(8), Q1(8), XDEN, XNUM, XM1
      DATA D1/-5.772156649015328605195174E-1/
      DATA P1/4.945235359296727046734888E0,2.018112620856775083915565E2,
     *        2.290838373831346393026739E3,1.131967205903380828685045E4,
     *        2.855724635671635335736389E4,3.848496228443793359990269E4,
     *        2.637748787624195437963534E4,7.225813979700288197698961E3/
      DATA Q1/6.748212550303777196073036E1,1.113332393857199323513008E3,
     *        7.738757056935398733233834E3,2.763987074403340708898585E4,
     *        5.499310206226157329794414E4,6.161122180066002127833352E4,
     *        3.635127591501940507276287E4,8.785536302431013170870835E3/
c     ------------------------------------------------------------------
      XDEN = 1.0e0
      XNUM = 0.0e0
      do 140 I = 1,8
         XNUM = XNUM*XM1 + P1(I)
         XDEN = XDEN*XM1 + Q1(I)
  140 continue
      SRAT1 = XM1 * (D1 + XM1*(XNUM/XDEN))
      return
      end
