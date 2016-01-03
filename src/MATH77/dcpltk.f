      DOUBLE PRECISION FUNCTION DCPLTK(EM)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2001-06-18 DCPLTK Krogh  Remove a ' ' inside a floating point num.
C>> 1998-10-29 DCPLTK Krogh  Moved external statement up for mangle.
c>> 1996-03-30 DCPLTK Krogh   Changed MAX to MAXT
C>> 1994-10-27 DCPLTK Snyder  Correct missing type declarations
C>> 1994-10-20 DCPLTK Krogh  Changes to use M77CON
C>> 1994-05-16 DCPLTK Snyder  Make SP and DP codes alike using CHGTYP
C>> 1989-06-16 DCPLTK Snyder  Remove implied DO from DATA stmts for CFT
C>> 1985-08-02 DCPLTK Lawson  Initial code.
C--D replaces "?": ?CPLTK, ?ERM1
C
C     Complete elliptic integral K(EM). Method given by
C     W.J.CODY, Math. of Comp.,Vol.19, (1965), pp. 105-111.
C     Data are taken from Cody's table in reverse order.
C     We are using N = 5 or 9 for single precision and
C     N = 10 for double precision.
C
C     C.L.LAWSON & STELLA CHAN,JPL,1983 JUNE.
C
C     ----------------------------------------------------
C
C     POLY DEGREE     NEGATIVE LOG BASE 10 OF MAX ABS
C                     ERROR OF APPROXIMATION
C
C         N           FOR K           FOR E
C         -           -----           -----
C         5            9.50            9.44
C         9           15.87           15.84
C        10           17.45           17.42
C
C     ----------------------------------------------------
C
      EXTERNAL D1MACH
      DOUBLE PRECISION A(10,3), B(10,3), D1MACH
      DOUBLE PRECISION EM, EMP, FAC, HALF, ONE, S1, S2, U, VL, ZERO
      INTEGER J, M, MAXT
C
      SAVE M, MAXT, FAC
C
C     VL = LOG(4)
      DATA VL/1.3862943611198906188344642429163531361510002687205D0/
C
      DATA ZERO,ONE,HALF / .0D0, 1.D0, .5D0 /
C
      DATA M / 0 /
C
      DATA A / 6.63980111468D-03, 2.59628884526D-02,
     *         2.37612248576D-02, 3.15594316275D-02,
     *         9.65786196226D-02, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,

     *         3.00725199036864838D-04, 3.96847090209897819D-03,
     *         1.07959904905916349D-02, 1.05899536209893585D-02,
     *         7.51938672180838102D-03, 8.92664629455646620D-03,
     *         1.49420291422820783D-02, 3.08851730018997099D-02,
     *         9.65735903017425285D-02, 0.0D0,

     *         1.3930878570066467279D-04,2.2966348983969586869D-03,
     *         8.0030039806499853708D-03,9.8489293221768937682D-03,
     *         6.8479092826245051197D-03,6.1796274460533176084D-03,
     *         8.7898018745550646778D-03,1.4938013532687165242D-02,
     *         3.0885146271305189866D-02,9.6573590280856255384D-02/
C
      DATA B / 1.84723416323D-03, 1.87516602769D-02,
     *         4.49838755399D-02, 7.01487577829D-02,
     *         1.24999295975D-01, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,

     *         6.66317524646073151D-05, 1.72161470979865212D-03,
     *         9.28116038296860419D-03, 2.06902400051008404D-02,
     *         2.95037293486887130D-02, 3.73355466822860296D-02,
     *         4.88271550481180099D-02, 7.03124954595466082D-02,
     *         1.24999999997640658D-01, 0.0D0,

     *         2.9700280966555612066D-05,9.2155463496324984638D-04,
     *         5.9739042991554291551D-03,1.5530941631977203877D-02,
     *         2.3931913323110790077D-02,3.0124849012898930266D-02,
     *         3.7377739758623604144D-02,4.8828041906862397978D-02,
     *         7.0312499739038352054D-02,1.2499999999990808051D-01/
C
      IF (M .EQ. 0) THEN
        IF (D1MACH(3) .LT. 6.31D-17) THEN
          M = 3
          MAXT = 10
C          -LOG10(6.31D-17) = 16.2
        ELSE IF (D1MACH(3) .LT. 6.31D-9) THEN
C          -LOG10(6.31D-9) = 8.2
          M = 2
          MAXT = 9
        ELSE
          M = 1
          MAXT = 5
        END IF
        FAC = ONE + 2 * D1MACH(3)
      END IF
      IF (EM .GE. ONE) THEN
        CALL DERM1('DCPLTK',1,0,'INFINITE VALUE WHEN EM .EQ. 1' //
     *             ' AND UNDEFINED FOR EM .GT. 1','EM',EM,'.')
        U = ZERO
      ELSE IF (EM .LT. ZERO) THEN
        CALL DERM1('DCPLTK',2,0,'UNDEFINED FOR EM .LT. ZERO',
     *             'EM',EM,'.')
        U = ZERO
      ELSE
        EMP = HALF + (HALF- EM)
        S1 = EMP * A(1,M)
        S2 = EMP * B(1,M)
        DO 100 J = 2, MAXT
          S1 = (S1 + A(J,M)) * EMP
          S2 = (S2 + B(J,M)) * EMP
  100   CONTINUE
        U = VL + S1 + LOG(ONE/EMP) * (HALF + S2)
      END IF
C
      DCPLTK = U * FAC
      RETURN
C
      END
