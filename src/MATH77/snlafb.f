      SUBROUTINE  SNLAFB(N, P, X, B, SCALCR, IV, LIV, LV, V)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2000-12-01 SNLAFB Krogh  Removed unused parameter REGD.
c>> 1996-04-27 SNLAFB Krogh  Changes to get desired C prototypes.
c>> 1994-10-20 SNLAFB Krogh  Changes to use M77CON
c>> 1990-06-29 SNLAFB CLL @ JPL
c>> 1990-06-12 CLL @ JPL
c>> 1990-02-16 CLL @ JPL
*** from netlib, Wed Feb  7 13:51:26 EST 1990 ***
C
C  ***  MINIMIZE A NONLINEAR SUM OF SQUARES USING RESIDUAL VALUES ONLY.
C  ***  This VERSION HANDLES SIMPLE BOUNDS ON X  ***
c
C  ***  PARAMETERS  ***
C
      EXTERNAL SCALCR
      INTEGER N, P, LIV, LV
      INTEGER IV(LIV)
      REAL             X(P), B(2,P), V(LV)
C
C ----------------------------  DISCUSSION  ----------------------------
C
C        THIS AMOUNTS TO SUBROUTINE NL2SNO (REF. 1) MODIFIED TO HANDLE
C     SIMPLE BOUNDS ON THE VARIABLES...
C           B(1,I) .LE. X(I) .LE. B(2,I), I = 1(1)P.
C        THE PARAMETERS FOR  SNLAFB ARE THE SAME AS THOSE FOR  DNLAGB
C     (WHICH SEE), EXCEPT THAT DCALCJ IS OMITTED.  INSTEAD OF CALLING
C     DCALCJ TO OBTAIN THE JACOBIAN MATRIX OF R AT X,  SNLAFB COMPUTES
C     AN APPROXIMATION TO IT BY FINITE (FORWARD) DIFFERENCES -- SEE
C     V(DLTFDJ) BELOW.   SNLAFB DOES NOT COMPUTE A COVARIANCE MATRIX.
C        THE NUMBER OF EXTRA CALLS ON SCALCR USED IN COMPUTING THE JACO-
C     BIAN APPROXIMATION ARE NOT INCLUDED IN THE FUNCTION EVALUATION
C     COUNT IV(NFCALL), BUT ARE RECORDED IN IV(NGCALL) INSTEAD.
C
C V(DLTFDJ)... V(43) HELPS CHOOSE THE STEP SIZE USED WHEN COMPUTING THE
C             FINITE-DIFFERENCE JACOBIAN MATRIX.  FOR DIFFERENCES IN-
C             VOLVING X(I), THE STEP SIZE FIRST TRIED IS
C                       V(DLTFDJ) * MAX(ABS(X(I)), 1/D(I)),
C             WHERE D IS THE CURRENT SCALE VECTOR (SEE REF. 1).  (IF
C             THIS STEP IS TOO BIG, I.E., IF SCALCR SETS NF TO 0, THEN
C             SMALLER STEPS ARE TRIED UNTIL THE STEP SIZE IS SHRUNK BE-
C             LOW 1000 * MACHEP, WHERE MACHEP IS THE UNIT ROUNDOFF.
C             DEFAULT = MACHEP**0.5.
C
C  ***  REFERENCE  ***
C
C 1.  DENNIS, J.E., GAY, D.M., AND WELSCH, R.E. (1981), AN ADAPTIVE
C             NONLINEAR LEAST-SQUARES ALGORITHM, ACM TRANS. MATH.
C             SOFTWARE, VOL. 7, NO. 3.
C
C  ***  GENERAL  ***
C
C     CODED BY DAVID M. GAY.
C
C ++++++++++++++++++++++++++  DECLARATIONS  +++++++++++++++++++++++++++
C
C  ***  EXTERNAL SUBROUTINES  ***
C
      EXTERNAL SIVSET, SRN2GB, SV7SCP
c--S replaces "?": ?NLAFB, ?IVSET, ?RN2GB, ?V7SCP, ?CALCR
C
C  SIVSET.... PROVIDES DEFAULT IV AND V INPUT COMPONENTS.
C  SRN2GB...  CARRIES OUT OPTIMIZATION ITERATIONS.
C  SV7SCP...  SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER D1, DK, DR1, I, IV1, J1K, K, N1, N2, NF, NG, RD1, R1, RN
      REAL             H, H0, HLIM, NEGPT5, ONE, T, XK, XK1, ZERO
C
C  ***  IV AND V COMPONENTS  ***
C
      INTEGER COVREQ, D, DINIT, DLTFDJ, J, MODE, NEXTV, NFCALL, NFGCAL,
     1        NGCALL, NGCOV, R, REGD0, TOOBIG, VNEED
      PARAMETER (COVREQ=15, D=27, DINIT=38, DLTFDJ=43, J=70, MODE=35,
     1           NEXTV=47, NFCALL=6, NFGCAL=7, NGCALL=30, NGCOV=53,
     2           R=61, REGD0=82, TOOBIG=2, VNEED=4)

      DATA HLIM/0.1E+0/, NEGPT5/-0.5E+0/, ONE/1.E+0/, ZERO/0.E+0/
C
C --------------------------------  BODY  ------------------------------
C
      IF (IV(1) .EQ. 0) CALL SIVSET(1, IV, LIV, LV, V)
      IV(COVREQ) = 0
      IV1 = IV(1)
      IF (IV1 .EQ. 14) GO TO 10
      IF (IV1 .GT. 2 .AND. IV1 .LT. 12) GO TO 10
      IF (IV1 .EQ. 12) IV(1) = 13
      IF (IV(1) .EQ. 13) IV(VNEED) = IV(VNEED) + P + N*(P+2)
      CALL SRN2GB(B, X, V, IV, LIV, LV, N, N, N1, N2, P, V, V, V, X)
      IF (IV(1) .NE. 14) GO TO 999
C
C  ***  STORAGE ALLOCATION  ***
C
      IV(D) = IV(NEXTV)
      IV(R) = IV(D) + P
      IV(REGD0) = IV(R) + N
      IV(J) = IV(REGD0) + N
      IV(NEXTV) = IV(J) + N*P
      IF (IV1 .EQ. 13) GO TO 999
C
 10   D1 = IV(D)
      DR1 = IV(J)
      R1 = IV(R)
      RN = R1 + N - 1
      RD1 = IV(REGD0)
C
 20   CALL SRN2GB(B, V(D1), V(DR1), IV, LIV, LV, N, N, N1, N2, P, V(R1),
     1           V(RD1), V, X)
      IF (IV(1)-2) 30, 50, 999
C
C  ***  NEW FUNCTION VALUE (R VALUE) NEEDED  ***
C
 30   NF = IV(NFCALL)
C%%    (*scalcr)( n, p, x, &nf, &V[r1] );
      CALL SCALCR(N, P, X, NF, V(R1))
      IF (NF .GT. 0) GO TO 40
         IV(TOOBIG) = 1
         GO TO 20
 40   IF (IV(1) .GT. 0) GO TO 20
C
C  ***  COMPUTE FINITE-DIFFERENCE APPROXIMATION TO DR = GRAD. OF R  ***
C
C     *** INITIALIZE D IF NECESSARY ***
C
 50   IF (IV(MODE) .LT. 0 .AND. V(DINIT) .EQ. ZERO)
     1        CALL SV7SCP(P, V(D1), ONE)
C
      J1K = DR1
      DK = D1
      NG = IV(NGCALL) - 1
      IF (IV(1) .EQ. (-1)) IV(NGCOV) = IV(NGCOV) - 1
      DO 120 K = 1, P
         IF (B(1,K) .GE. B(2,K)) GO TO 110
         XK = X(K)
         H = V(DLTFDJ) * MAX( ABS(XK), ONE/V(DK))
         H0 = H
         DK = DK + 1
         T = NEGPT5
         XK1 = XK + H
         IF (XK - H .GE. B(1,K)) GO TO 60
            T = -T
            IF (XK1 .GT. B(2,K)) GO TO 80
 60      IF (XK1 .LE. B(2,K)) GO TO 70
            T = -T
            H = -H
            XK1 = XK + H
            IF (XK1 .LT. B(1,K)) GO TO 80
 70      X(K) = XK1
         NF = IV(NFGCAL)
C%%        (*scalcr)( n, p, x, &nf, &V[j1k] );
         CALL SCALCR (N, P, X, NF, V(J1K))
         NG = NG + 1
         IF (NF .GT. 0) GO TO 90
              H = T * H
              XK1 = XK + H
              IF ( ABS(H/H0) .GE. HLIM) GO TO 70
 80                IV(TOOBIG) = 1
                   IV(NGCALL) = NG
                   GO TO 20
 90      X(K) = XK
         IV(NGCALL) = NG
         DO 100 I = R1, RN
              V(J1K) = (V(J1K) - V(I)) / H
              J1K = J1K + 1
 100          CONTINUE
         GO TO 120
C        *** SUPPLY A ZERO DERIVATIVE FOR CONSTANT COMPONENTS...
 110     CALL SV7SCP(N, V(J1K), ZERO)
         J1K = J1K + N
 120     CONTINUE
      GO TO 20
C
 999  RETURN
C
C  ***  LAST CARD OF  SNLAFB FOLLOWS  ***
      END
