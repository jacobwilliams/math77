      SUBROUTINE SELEFI (PHI, K, F, E, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 SELEFI Krogh  Moved external statement up for mangle.
C>> 1995-11-17 SELEFI Krogh  Converted SFTRAN to Fortran 77.
C>> 1995-10-24 SELEFI Krogh  Removed blanks in numbers for C conversion.
C>> 1994-10-19 SELEFI Krogh  Changes to use M77CON
C>> 1990-11-27 SELEFI WV Snyder Convert code from NSWC to MATH77
C
C--S replaces "?": ?ELEFI, ?LNREL, ?ERM1
C
C-- Begin mask code changes
C          REAL ELLIPTIC INTEGRALS OF THE FIRST AND SECOND KINDS
C
C     PHI [IN]   = ARGUMENT                    (ABS(PHI) .LE. PI/2)
C     K [IN]     = MODULUS                       (ABS(K) .LE. 1.0)
C     F [OUT[    = ELLIPTIC INTEGRAL OF FIRST KIND = F(PHI, K)
C     E [OUT]    = ELLIPTIC INTEGRAL OF SECOND KIND = E(PHI, K)
C     IERR [OUT] = ERROR INDICATOR
C            0 = NO ERRORS
C            1 = ARGUMENT, PHI, > PI/2; SEE AMS 55 17.4.3 OR BYRD AND
C                 FRIEDMAN 113.02
C            2 = MODULUS, ABS(K), > 1; SEE AMS 55 17.4.15 OR BYRD AND
C                 FRIEDMAN 114.01;
C            3 = ARGUMENT, PHI, = PI/2 AND MODULUS, K = 1 (F INFINITE).
C-- End mask code changes
C
      REAL             PHI, K, F, E
      INTEGER IERR
C
C     *****     External References     ********************************
C
C R1MACH  Provides machine constants.
C SERM1   Displays an error message.
C SLNREL  Computes LOG(1+X) given X.
C
      EXTERNAL R1MACH, SLNREL
      REAL             R1MACH, SLNREL
C
C     *****     Local Variables     ************************************
C
      REAL             AN, CN, DN, HN, K2, L2, LN2, LN4, P, PH, PI2, PN
      REAL             PX, QN, QX, QXDR, R, R0, R2, RI, RJ, RLPX
      REAL             RK, RM, RN, S0, S1, S2, S3, S4, SGN, SI, SJ, SK
      REAL             SN, SS, TD, TH1, TR, TS
C
C     *****     Data Statements     ************************************
C
C     LN2 = LN(2)
C     LN4 = LN(4)
C     TH1 = TANH(1)
C     PI2 = PI/2
C
      DATA LN2 /0.693147180559945309417232121458176568075E0/
      DATA LN4 /1.386294361119890618834464242916353136151E0/
      DATA PI2 /1.570796326794896619231321691639751442099E0/
      DATA TH1 /.7615941559557648881194582826047935904128E0/
C
C     *****     Executable Statements     ******************************
C
      IF (PHI .LT. 0.0e0) THEN
         SGN = -1.0e0
         PH = -PHI
      ELSE
         SGN = 1.0e0
         PH = PHI
      END IF
      IF (PH .GT. PI2) THEN
         CALL SERM1 ('SELEFI',1,0,'ABS(PHI) .GT. PI/2','PHI',PHI,'.')
         IERR = 1
         RETURN
      END IF
      IF (ABS(K) .GT. 1.0e0) THEN
         CALL SERM1 ('SELEFI',1,0,'ABS(K) .GT. 1.0','K',K,'.')
         IERR = 2
         RETURN
      END IF
      IERR = 0
      IF (PH .EQ. 0.0e0) THEN
         F = 0.0e0
         E = 0.0e0
         RETURN
      END IF
C
      SN = SIN(PH)
      CN = SQRT(0.5e0 + (0.5e0 - SN*SN))
C
      K2 = K*K
      PX = ABS(K*SN)
      IF (PX .LT. TH1) THEN
C
C     SERIES EXPANSION FOR ABS(K*SIN(PHI)) .LE. TANH(1)
C
         SS = SN*SN
         PN = 1.0e0
         QN = 2.0e0
         AN = PH
         HN = 1.0e0
         S1 = 0.0e0
         S2 = 0.0e0
         TR = PH*SS
         TS = SN*CN
  100    CONTINUE
            AN = (PN*AN - TS)/QN
            R  = K2*HN/QN
            S2 = S2 + R*AN
            HN = PN*R
            S0 = S1
            S1 = S1 + HN*AN
            IF (ABS(TR) .LT. ABS(AN)) GO TO 120
            IF (ABS(S1) .LE. ABS(S0)) GO TO 120
            PN = QN + 1.0e0
            QN = PN + 1.0e0
            TR = SS*TR
            TS = SS*TS
            GO TO 100
  120    CONTINUE
         F = PH + S1
         E = PH - S2
      ELSE
C
C     SERIES EXPANSION FOR ABS(K*SIN(PHI)) .GT. TANH(1)
C
         R2 = 0.5e0 + (0.5e0 - PX*PX)
         IF (R2 .EQ. 0.0e0) THEN
            IERR = 3
            F = R1MACH(2)
            E = 1.0e0
            RETURN
         END IF
         L2 = 0.5e0 + (0.5e0 - K2)
         R = SQRT(R2)
         SI = 1.0e0
         SJ = L2
         SK = 0.0e0
         RM = 0.0e0
         RN = 0.0e0
         S1 = 0.0e0
         S2 = 0.0e0
         S3 = 0.0e0
         S4 = 0.0e0
         QX = ABS(K*CN)
         TD = QX*R
         DN = 2.0e0
  140    CONTINUE
            PN = (DN - 1.0e0)/DN
            QN = (DN + 1.0e0)/(DN + 2.0e0)
            RI = PN*SI
            RJ = PN*PN*SJ
            RK = SK + 2.0e0/(DN*(DN - 1.0e0))
            R0 = TD/DN
            RM = QN*QN*L2*(RM - R0*RI)
            RN = PN*QN*L2*(RN - R0*SI)
            R0 = S3
            S1 = S1 + RJ
            S2 = S2 + QN*RJ
            S3 = S3 + RM - RJ*RK
            S4 = S4 + RN - SJ*(PN*RK - 1.0e0/(DN*DN))
            IF (S3 .GE. R0) GO TO 160
            SI = RI
            SJ = RJ*L2
            SK = RK
            DN = DN + 2.0e0
            TD = R2*TD
            GO TO 140
  160    CONTINUE
         QXDR = QX/R
         RLPX = SLNREL(PX)
         P = LN4 - 0.5e0*(RLPX+SLNREL(-PX))-SLNREL(QXDR)
         F = (1.0e0 + S1)*P + QXDR*(RLPX-LN2) + S3
         E = (0.5e0 + S2)*L2*P + (1.0e0 - QXDR*(1.0e0-PX)) + S4
      END IF
      F = SGN*F
      E = SGN*E
      RETURN
      END
