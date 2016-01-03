      REAL             FUNCTION SCPLTE(EM)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 SCPLTE Krogh  Moved external statement up for mangle.
c>> 1996-03-30 SCPLTE Krogh   Changed MAX to MAXT
C>> 1994-10-27 SCPLTE Snyder  Correct missing type declarations
C>> 1994-10-20 SCPLTE Krogh  Changes to use M77CON
C>> 1994-05-16 SCPLTE Snyder  Make SP and DP codes alike using CHGTYP
C>> 1989-06-16 SCPLTE Snyder  Remove implied DO from DATA stmts for CFT
C>> 1985-08-02 SCPLTE Lawson  Initial code.
c--S replaces "?": ?CPLTE, ?ERM1
C
C     Complete elliptic integral E(EM). Method given by
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
      REAL             C(10,3), D(10,3), R1MACH
      REAL             EM, EMP, FAC, HALF, ONE, S3, S4, U, ZERO
      INTEGER J, M, MAXT
C
      SAVE M, MAXT, FAC
C
      DATA ZERO,ONE,HALF / 0.E0, 1.E0 ,.5E0 /
C
      DATA M / 0 /
c
      DATA C / 7.65296060328E-03, 3.06623474572E-02,
     *         3.17611455247E-02, 5.75669984841E-02,
     *         4.43152874726E-01, 0.0E0, 0.0E0, 0.0E0, 0.0E0, 0.0E0,

     *         3.25192015506390418E-04, 4.30253777479311659E-03,
     *         1.17858410087339355E-02, 1.18419259955012494E-02,
     *         9.03552773754088184E-03, 1.17167669446577228E-02,
     *         2.18361314054868967E-02, 5.68052233293082895E-02,
     *         4.43147180583368137E-01, 0.0E0,

     *         1.4946621757181326771E-04,2.4685033304607227339E-03,
     *         8.6384421736040744302E-03,1.0770635039866455473E-02,
     *         7.8204040609595541727E-03,7.5950934225594322802E-03,
     *         1.1569595745295402175E-02,2.1831811676130481568E-02,
     *         5.6805194567559156648E-02,4.4314718056088952648E-01/
C
      DATA D / 2.12479182845E-03, 2.18360211693E-02,
     *         5.42605244487E-02, 9.35649078307E-02,
     *         2.49999202736E-01, 0.0E0, 0.0E0, 0.0E0, 0.0E0, 0.0E0,

     *         7.20316963457154599E-05, 1.86453791840633632E-03,
     *         1.00879584943751004E-02, 2.26603098916041221E-02,
     *         3.28110691727210618E-02, 4.26725101265917523E-02,
     *         5.85927071842652739E-02, 9.37499951163670673E-02,
     *         2.49999999997461423E-01, 0.0E0,

     *         3.1859195655501571800E-05,9.8983328462253847867E-04,
     *         6.4321465864383017666E-03,1.6804023346363384981E-02,
     *         2.6145014700313878932E-02,3.3478943665761626232E-02,
     *         4.2717890547383095644E-02,5.8593661255531491732E-02,
     *         9.3749999721203140658E-02,2.4999999999990177208E-01/
C
      IF (M .EQ. 0) THEN
        IF (R1MACH(3) .LT. 6.31e-17) THEN
C         -LOG10(6.31E-17) = 17.2
          M = 3
          MAXT = 10
        ELSE IF (R1MACH(3) .LT. 6.31E-9) THEN
C         -LOG10(6.31E-9) = 8.2
          M = 2
          MAXT = 9
        ELSE
          M = 1
          MAXT = 5
        END IF
        FAC = ONE + 2 * R1MACH(3)
      END IF
      IF (EM .GT. ONE) THEN
        CALL SERM1('SCPLTE',1,0,'UNDEFINED FOR EM .GT. ONE',
     *             'EM',EM,'.')
        U = ZERO
      ELSE IF (EM .LT. ZERO) THEN
        CALL SERM1('SCPLTE',2,0,'UNDEFINED FOR EM .LT. ZERO',
     *             'EM',EM,'.')
        U = ZERO
      ELSE
        EMP = HALF + (HALF- EM)
        IF (EMP .EQ. ZERO) THEN
          U = ONE
          GO TO 10
        END IF
        S3 = EMP * C(1,M)
        S4 = EMP * D(1,M)
        DO 100 J = 2, MAXT
          S3 = (S3 + C(J,M)) * EMP
          S4 = (S4 + D(J,M)) * EMP
  100   CONTINUE
        U = ONE + S3 + LOG(ONE/EMP) * S4
      END IF
C
  10  SCPLTE = U * FAC
      RETURN
C
      END
