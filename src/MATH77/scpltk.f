      REAL             FUNCTION SCPLTK(EM)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2001-06-18 SCPLTK Krogh  Remove a ' ' inside a floating point num.
C>> 1998-10-29 SCPLTK Krogh  Moved external statement up for mangle.
c>> 1996-03-30 SCPLTK Krogh   Changed MAX to MAXT
C>> 1994-10-27 SCPLTK Snyder  Correct missing type declarations
C>> 1994-10-20 SCPLTK Krogh  Changes to use M77CON
C>> 1994-05-16 SCPLTK Snyder  Make SP and DP codes alike using CHGTYP
C>> 1989-06-16 SCPLTK Snyder  Remove implied DO from DATA stmts for CFT
C>> 1985-08-02 SCPLTK Lawson  Initial code.
C--S replaces "?": ?CPLTK, ?ERM1
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
      EXTERNAL R1MACH
      REAL             A(10,3), B(10,3), R1MACH
      REAL             EM, EMP, FAC, HALF, ONE, S1, S2, U, VL, ZERO
      INTEGER J, M, MAXT
C
      SAVE M, MAXT, FAC
C
C     VL = LOG(4)
      DATA VL/1.3862943611198906188344642429163531361510002687205E0/
C
      DATA ZERO,ONE,HALF / .0E0, 1.E0, .5E0 /
C
      DATA M / 0 /
C
      DATA A / 6.63980111468E-03, 2.59628884526E-02,
     *         2.37612248576E-02, 3.15594316275E-02,
     *         9.65786196226E-02, 0.0E0, 0.0E0, 0.0E0, 0.0E0, 0.0E0,

     *         3.00725199036864838E-04, 3.96847090209897819E-03,
     *         1.07959904905916349E-02, 1.05899536209893585E-02,
     *         7.51938672180838102E-03, 8.92664629455646620E-03,
     *         1.49420291422820783E-02, 3.08851730018997099E-02,
     *         9.65735903017425285E-02, 0.0E0,

     *         1.3930878570066467279E-04,2.2966348983969586869E-03,
     *         8.0030039806499853708E-03,9.8489293221768937682E-03,
     *         6.8479092826245051197E-03,6.1796274460533176084E-03,
     *         8.7898018745550646778E-03,1.4938013532687165242E-02,
     *         3.0885146271305189866E-02,9.6573590280856255384E-02/
C
      DATA B / 1.84723416323E-03, 1.87516602769E-02,
     *         4.49838755399E-02, 7.01487577829E-02,
     *         1.24999295975E-01, 0.0E0, 0.0E0, 0.0E0, 0.0E0, 0.0E0,

     *         6.66317524646073151E-05, 1.72161470979865212E-03,
     *         9.28116038296860419E-03, 2.06902400051008404E-02,
     *         2.95037293486887130E-02, 3.73355466822860296E-02,
     *         4.88271550481180099E-02, 7.03124954595466082E-02,
     *         1.24999999997640658E-01, 0.0E0,

     *         2.9700280966555612066E-05,9.2155463496324984638E-04,
     *         5.9739042991554291551E-03,1.5530941631977203877E-02,
     *         2.3931913323110790077E-02,3.0124849012898930266E-02,
     *         3.7377739758623604144E-02,4.8828041906862397978E-02,
     *         7.0312499739038352054E-02,1.2499999999990808051E-01/
C
      IF (M .EQ. 0) THEN
        IF (R1MACH(3) .LT. 6.31E-17) THEN
          M = 3
          MAXT = 10
C          -LOG10(6.31E-17) = 16.2
        ELSE IF (R1MACH(3) .LT. 6.31E-9) THEN
C          -LOG10(6.31E-9) = 8.2
          M = 2
          MAXT = 9
        ELSE
          M = 1
          MAXT = 5
        END IF
        FAC = ONE + 2 * R1MACH(3)
      END IF
      IF (EM .GE. ONE) THEN
        CALL SERM1('SCPLTK',1,0,'INFINITE VALUE WHEN EM .EQ. 1' //
     *             ' AND UNDEFINED FOR EM .GT. 1','EM',EM,'.')
        U = ZERO
      ELSE IF (EM .LT. ZERO) THEN
        CALL SERM1('SCPLTK',2,0,'UNDEFINED FOR EM .LT. ZERO',
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
      SCPLTK = U * FAC
      RETURN
C
      END
