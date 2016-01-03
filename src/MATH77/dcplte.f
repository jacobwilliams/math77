      DOUBLE PRECISION FUNCTION DCPLTE(EM)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 DCPLTE Krogh  Moved external statement up for mangle.
c>> 1996-03-30 DCPLTE Krogh   Changed MAX to MAXT
C>> 1994-10-27 DCPLTE Snyder  Correct missing type declarations
C>> 1994-10-20 DCPLTE Krogh  Changes to use M77CON
C>> 1994-05-16 DCPLTE Snyder  Make SP and DP codes alike using CHGTYP
C>> 1989-06-16 DCPLTE Snyder  Remove implied DO from DATA stmts for CFT
C>> 1985-08-02 DCPLTE Lawson  Initial code.
c--D replaces "?": ?CPLTE, ?ERM1
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
      EXTERNAL D1MACH
      DOUBLE PRECISION C(10,3), D(10,3), D1MACH
      DOUBLE PRECISION EM, EMP, FAC, HALF, ONE, S3, S4, U, ZERO
      INTEGER J, M, MAXT
C
      SAVE M, MAXT, FAC
C
      DATA ZERO,ONE,HALF / 0.D0, 1.D0 ,.5D0 /
C
      DATA M / 0 /
c
      DATA C / 7.65296060328D-03, 3.06623474572D-02,
     *         3.17611455247D-02, 5.75669984841D-02,
     *         4.43152874726D-01, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,

     *         3.25192015506390418D-04, 4.30253777479311659D-03,
     *         1.17858410087339355D-02, 1.18419259955012494D-02,
     *         9.03552773754088184D-03, 1.17167669446577228D-02,
     *         2.18361314054868967D-02, 5.68052233293082895D-02,
     *         4.43147180583368137D-01, 0.0D0,

     *         1.4946621757181326771D-04,2.4685033304607227339D-03,
     *         8.6384421736040744302D-03,1.0770635039866455473D-02,
     *         7.8204040609595541727D-03,7.5950934225594322802D-03,
     *         1.1569595745295402175D-02,2.1831811676130481568D-02,
     *         5.6805194567559156648D-02,4.4314718056088952648D-01/
C
      DATA D / 2.12479182845D-03, 2.18360211693D-02,
     *         5.42605244487D-02, 9.35649078307D-02,
     *         2.49999202736D-01, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0,

     *         7.20316963457154599D-05, 1.86453791840633632D-03,
     *         1.00879584943751004D-02, 2.26603098916041221D-02,
     *         3.28110691727210618D-02, 4.26725101265917523D-02,
     *         5.85927071842652739D-02, 9.37499951163670673D-02,
     *         2.49999999997461423D-01, 0.0D0,

     *         3.1859195655501571800D-05,9.8983328462253847867D-04,
     *         6.4321465864383017666D-03,1.6804023346363384981D-02,
     *         2.6145014700313878932D-02,3.3478943665761626232D-02,
     *         4.2717890547383095644D-02,5.8593661255531491732D-02,
     *         9.3749999721203140658D-02,2.4999999999990177208D-01/
C
      IF (M .EQ. 0) THEN
        IF (D1MACH(3) .LT. 6.31d-17) THEN
C         -LOG10(6.31D-17) = 17.2
          M = 3
          MAXT = 10
        ELSE IF (D1MACH(3) .LT. 6.31D-9) THEN
C         -LOG10(6.31D-9) = 8.2
          M = 2
          MAXT = 9
        ELSE
          M = 1
          MAXT = 5
        END IF
        FAC = ONE + 2 * D1MACH(3)
      END IF
      IF (EM .GT. ONE) THEN
        CALL DERM1('DCPLTE',1,0,'UNDEFINED FOR EM .GT. ONE',
     *             'EM',EM,'.')
        U = ZERO
      ELSE IF (EM .LT. ZERO) THEN
        CALL DERM1('DCPLTE',2,0,'UNDEFINED FOR EM .LT. ZERO',
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
  10  DCPLTE = U * FAC
      RETURN
C
      END
