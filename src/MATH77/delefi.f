      SUBROUTINE DELEFI (PHI, K, F, E, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 DELEFI Krogh  Moved external statement up for mangle.
C>> 1995-11-17 DELEFI Krogh  Converted SFTRAN to Fortran 77.
C>> 1995-10-24 DELEFI Krogh  Removed blanks in numbers for C conversion.
C>> 1994-10-19 DELEFI Krogh  Changes to use M77CON
C>> 1990-11-27 DELEFI WV Snyder Convert code from NSWC to MATH77
C
C--D replaces "?": ?ELEFI, ?LNREL, ?ERM1
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
      DOUBLE PRECISION PHI, K, F, E
      INTEGER IERR
C
C     *****     External References     ********************************
C
C D1MACH  Provides machine constants.
C DERM1   Displays an error message.
C DLNREL  Computes LOG(1+X) given X.
C
      EXTERNAL D1MACH, DLNREL
      DOUBLE PRECISION D1MACH, DLNREL
C
C     *****     Local Variables     ************************************
C
      DOUBLE PRECISION AN, CN, DN, HN, K2, L2, LN2, LN4, P, PH, PI2, PN
      DOUBLE PRECISION PX, QN, QX, QXDR, R, R0, R2, RI, RJ, RLPX
      DOUBLE PRECISION RK, RM, RN, S0, S1, S2, S3, S4, SGN, SI, SJ, SK
      DOUBLE PRECISION SN, SS, TD, TH1, TR, TS
C
C     *****     Data Statements     ************************************
C
C     LN2 = LN(2)
C     LN4 = LN(4)
C     TH1 = TANH(1)
C     PI2 = PI/2
C
      DATA LN2 /0.693147180559945309417232121458176568075D0/
      DATA LN4 /1.386294361119890618834464242916353136151D0/
      DATA PI2 /1.570796326794896619231321691639751442099D0/
      DATA TH1 /.7615941559557648881194582826047935904128D0/
C
C     *****     Executable Statements     ******************************
C
      IF (PHI .LT. 0.0d0) THEN
         SGN = -1.0d0
         PH = -PHI
      ELSE
         SGN = 1.0d0
         PH = PHI
      END IF
      IF (PH .GT. PI2) THEN
         CALL DERM1 ('DELEFI',1,0,'ABS(PHI) .GT. PI/2','PHI',PHI,'.')
         IERR = 1
         RETURN
      END IF
      IF (ABS(K) .GT. 1.0d0) THEN
         CALL DERM1 ('DELEFI',1,0,'ABS(K) .GT. 1.0','K',K,'.')
         IERR = 2
         RETURN
      END IF
      IERR = 0
      IF (PH .EQ. 0.0d0) THEN
         F = 0.0d0
         E = 0.0d0
         RETURN
      END IF
C
      SN = SIN(PH)
      CN = SQRT(0.5d0 + (0.5d0 - SN*SN))
C
      K2 = K*K
      PX = ABS(K*SN)
      IF (PX .LT. TH1) THEN
C
C     SERIES EXPANSION FOR ABS(K*SIN(PHI)) .LE. TANH(1)
C
         SS = SN*SN
         PN = 1.0d0
         QN = 2.0d0
         AN = PH
         HN = 1.0d0
         S1 = 0.0d0
         S2 = 0.0d0
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
            PN = QN + 1.0d0
            QN = PN + 1.0d0
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
         R2 = 0.5d0 + (0.5d0 - PX*PX)
         IF (R2 .EQ. 0.0d0) THEN
            IERR = 3
            F = D1MACH(2)
            E = 1.0d0
            RETURN
         END IF
         L2 = 0.5d0 + (0.5d0 - K2)
         R = SQRT(R2)
         SI = 1.0d0
         SJ = L2
         SK = 0.0d0
         RM = 0.0d0
         RN = 0.0d0
         S1 = 0.0d0
         S2 = 0.0d0
         S3 = 0.0d0
         S4 = 0.0d0
         QX = ABS(K*CN)
         TD = QX*R
         DN = 2.0d0
  140    CONTINUE
            PN = (DN - 1.0d0)/DN
            QN = (DN + 1.0d0)/(DN + 2.0d0)
            RI = PN*SI
            RJ = PN*PN*SJ
            RK = SK + 2.0d0/(DN*(DN - 1.0d0))
            R0 = TD/DN
            RM = QN*QN*L2*(RM - R0*RI)
            RN = PN*QN*L2*(RN - R0*SI)
            R0 = S3
            S1 = S1 + RJ
            S2 = S2 + QN*RJ
            S3 = S3 + RM - RJ*RK
            S4 = S4 + RN - SJ*(PN*RK - 1.0d0/(DN*DN))
            IF (S3 .GE. R0) GO TO 160
            SI = RI
            SJ = RJ*L2
            SK = RK
            DN = DN + 2.0d0
            TD = R2*TD
            GO TO 140
  160    CONTINUE
         QXDR = QX/R
         RLPX = DLNREL(PX)
         P = LN4 - 0.5d0*(RLPX+DLNREL(-PX))-DLNREL(QXDR)
         F = (1.0d0 + S1)*P + QXDR*(RLPX-LN2) + S3
         E = (0.5d0 + S2)*L2*P + (1.0d0 - QXDR*(1.0d0-PX)) + S4
      END IF
      F = SGN*F
      E = SGN*E
      RETURN
      END
