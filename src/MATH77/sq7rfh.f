      SUBROUTINE SQ7RFH(IERR, IPIVOT, N, NN, NOPIVK, P, Q, R, RLEN, W)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c File: SQ7RFH.for      Four subrs used by the
c       David Gay & Linda Kaufman nonlinear LS package.
c             SQ7RFH, SV7PRM, SS7CPR, SV7SWP
c       Needed by versions that handle the Separable problem.
c       The first 3 are called by DRNSG & DRNSGB.
c       SV7SWP is called by SQ7RFH.
c
C>> 1998-10-29 SQ7RFH Krogh  Moved external statement up for mangle.
c>> 1996-05-15 SQ7RFH Krogh  Use M77CON for conversion to C.
c>> 1995-11-15 SQ7RFH Krogh  Moved formats up for C conversion.
c>> 1994-11-02 SQ7RFH Krogh  Changes to use M77CON
c>> 1992-04-27 SQ7RFH CLL Removed unreferenced stmt labels.
c>> 1992-04-13 CLL Change from Hollerith to '...' syntax in formats.
c>> 1990-03-30 CLL @ JPL
c>> 1990-02-21 CLL @ JPL
c--S replaces "?": ?Q7RFH,?D7TPR,?R7MDC,?V2AXY,?V2NRM,?V7SCL,?V7SCP
c--&               ?V7SWP,?S7CPR,?V7PRM
C
C  ***  COMPUTE QR FACTORIZATION VIA HOUSEHOLDER TRANSFORMATIONS
C  ***  WITH COLUMN PIVOTING  ***
C
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER IERR, N, NN, NOPIVK, P, RLEN
      INTEGER IPIVOT(P)
      REAL             Q(NN,P), R(RLEN), W(P)
C     DIMENSION R(P*(P+1)/2)
C
c ---------------------------  DESCRIPTION  ----------------------------
C
C    THIS ROUTINE COMPUTES A QR FACTORIZATION (VIA HOUSEHOLDER TRANS-
C FORMATIONS) OF THE MATRIX  A  THAT ON INPUT IS STORED IN Q.
C IF  NOPIVK  ALLOWS IT, THIS ROUTINE DOES COLUMN PIVOTING -- IF
C K .GT. NOPIVK,  THEN ORIGINAL COLUMN  K  IS ELIGIBLE FOR PIVOTING.
C THE  Q  AND  R  RETURNED ARE SUCH THAT COLUMN  I  OF  Q*R  EQUALS
C COLUMN  IPIVOT(I)  OF THE ORIGINAL MATRIX  A.  THE UPPER TRIANGULAR
C MATRIX  R  IS STORED COMPACTLY BY COLUMNS, I.E., THE OUTPUT VECTOR  R
C CONTAINS  R(1,1), R(1,2), R(2,2), R(1,3), R(2,3), ..., R(P,P) (IN
C THAT ORDER).  IF ALL GOES WELL, THEN THIS ROUTINE SETS  IERR = 0.
C BUT IF (PERMUTED) COLUMN  K  OF  A  IS LINEARLY DEPENDENT ON
C (PERMUTED) COLUMNS 1,2,...,K-1, THEN  IERR  IS SET TO  K AND THE R
C MATRIX RETURNED HAS  R(I,J) = 0  FOR  I .GE. K  AND  J .GE. K.
C    THE ORIGINAL MATRIX  A  IS AN N BY P MATRIX.  NN  IS THE LEAD
C DIMENSION OF THE ARRAY  Q  AND MUST SATISFY  NN .GE. N.  NO
C PARAMETER CHECKING IS DONE.
C    PIVOTING IS DONE AS THOUGH ALL COLUMNS OF Q WERE FIRST
C SCALED TO HAVE THE SAME NORM.  IF COLUMN K IS ELIGIBLE FOR
C PIVOTING AND ITS (SCALED) NORM**2 LOSS IS MORE THAN THE
C MINIMUM SUCH LOSS (OVER COLUMNS K THRU P), THEN COLUMN K IS
C SWAPPED WITH THE COLUMN OF LEAST NORM**2 LOSS.
C
C        CODED BY DAVID M. GAY (FALL 1979, SPRING 1984).
C
c -------------------------  LOCAL VARIABLES  --------------------------
C
      INTEGER I, II, J, K, KK, KM1, KP1, NK1
      REAL             AK, QKK, S, SINGTL, T, T1, WK
      EXTERNAL SD7TPR, SR7MDC,SV2AXY, SV7SCL, SV7SCP,SV7SWP, SV2NRM
      REAL             SD7TPR, SR7MDC, SV2NRM
      REAL             BIG, BIGRT, MEPS10, ONE, TEN, TINY, TINYRT,
     1                 WTOL, ZERO
      PARAMETER (ONE=1.0E+0, TEN=1.E+1, WTOL=0.75E+0, ZERO=0.0E+0)
      SAVE BIGRT, MEPS10, TINY, TINYRT
      DATA BIGRT/0.0E+0/, MEPS10/0.0E+0/, TINY/0.E+0/, TINYRT/0.E+0/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      IERR = 0
      IF (MEPS10 .GT. ZERO) GO TO 10
          BIGRT = SR7MDC(5)
          MEPS10 = TEN * SR7MDC(3)
          TINYRT = SR7MDC(2)
          TINY = SR7MDC(1)
          BIG = SR7MDC(6)
          IF (TINY*BIG .LT. ONE) TINY = ONE / BIG
 10   SINGTL = real(max(N,P)) * MEPS10
C
C  ***  INITIALIZE W, IPIVOT, AND DIAG(R)  ***
C
      J = 0
      DO 40 I = 1, P
         IPIVOT(I) = I
         T = SV2NRM(N, Q(1,I))
         IF (T .GT. ZERO) GO TO 20
              W(I) = ONE
              GO TO 30
 20      W(I) = ZERO
 30      J = J + I
         R(J) = T
 40      CONTINUE
C
C  ***  MAIN LOOP  ***
C
      KK = 0
      NK1 = N + 1
      DO 130 K = 1, P
         IF (NK1 .LE. 1) GO TO 999
         NK1 = NK1 - 1
         KK = KK + K
         KP1 = K + 1
         IF (K .LE. NOPIVK) GO TO 60
         IF (K .GE. P) GO TO 60
C
C        ***  FIND COLUMN WITH MINIMUM WEIGHT LOSS  ***
C
              T = W(K)
              IF (T .LE. ZERO) GO TO 60
              J = K
              DO 50 I = KP1, P
                   IF (W(I) .GE. T) GO TO 50
                        T = W(I)
                        J = I
 50                CONTINUE
              IF (J .EQ. K) GO TO 60
C
C             ***  INTERCHANGE COLUMNS K AND J  ***
C
                   I = IPIVOT(K)
                   IPIVOT(K) = IPIVOT(J)
                   IPIVOT(J) = I
                   W(J) = W(K)
                   W(K) = T
                   I = J*(J+1)/2
                   T1 = R(I)
                   R(I) = R(KK)
                   R(KK) = T1
                   CALL SV7SWP(N, Q(1,K), Q(1,J))
                   IF (K .LE. 1) GO TO 60
                        I = I - J + 1
                        J = KK - K + 1
                        CALL SV7SWP(K-1, R(I), R(J))
C
C        ***  COLUMN K OF Q SHOULD BE NEARLY ORTHOGONAL TO THE PREVIOUS
C        ***  COLUMNS.  NORMALIZE IT, TEST FOR SINGULARITY, AND DECIDE
C        ***  WHETHER TO REORTHOGONALIZE IT.
C
 60      AK = R(KK)
         IF (AK .LE. ZERO) GO TO 140
         WK = W(K)
C
C        *** SET T TO THE NORM OF (Q(K,K),...,Q(N,K))
C        *** AND CHECK FOR SINGULARITY.
C
         IF (WK .LT. WTOL) GO TO 70
            T = SV2NRM(NK1, Q(K,K))
            IF (T / AK .LE. SINGTL) GO TO 140
            GO TO 80
 70      T = sqrt(ONE - WK)
         IF (T .LE. SINGTL) GO TO 140
         T = T * AK
C
C        *** DETERMINE HOUSEHOLDER TRANSFORMATION ***
C
 80      QKK = Q(K,K)
         IF (T .LE. TINYRT) GO TO 90
         IF (T .GE. BIGRT) GO TO 90
            IF (QKK .LT. ZERO) T = -T
            QKK = QKK + T
            S = sqrt(T * QKK)
            GO TO 110
 90       S = sqrt(T)
          IF (QKK .LT. ZERO) GO TO 100
             QKK = QKK + T
             S = S * sqrt(QKK)
             GO TO 110
 100      T = -T
          QKK = QKK + T
          S = S * sqrt(-QKK)
 110      Q(K,K) = QKK
C
C         ***  SCALE (Q(K,K),...,Q(N,K)) TO HAVE NORM SQRT(2)  ***
C
          IF (S .LE. TINY) GO TO 140
          CALL SV7SCL(NK1, Q(K,K), ONE/S, Q(K,K))
C
          R(KK) = -T
C
C        ***  COMPUTE R(K,I) FOR I = K+1,...,P AND UPDATE Q  ***
C
         IF (K .GE. P) GO TO 999
         J = KK + K
         II = KK
         DO 120 I = KP1, P
              II = II + I
              CALL SV2AXY(NK1, Q(K,I), -SD7TPR(NK1,Q(K,K),Q(K,I)),
     1                   Q(K,K), Q(K,I))
              T = Q(K,I)
              R(J) = T
              J = J + I
              T1 = R(II)
              IF (T1 .GT. ZERO)  W(I) = W(I) + (T/T1)**2
 120          CONTINUE
 130     CONTINUE
C
C  ***  SINGULAR Q  ***
C
 140  IERR = K
      KM1 = K - 1
      J = KK
      DO 150 I = K, P
         CALL SV7SCP(I-KM1, R(J), ZERO)
         J = J + I
 150     CONTINUE
C
 999  RETURN
C  ***  LAST CARD OF SQ7RFH FOLLOWS  ***
      END
      SUBROUTINE SS7CPR(C, IV, L, LIV)
C
C  ***  PRINT C FOR   DNSG (ETC.)  ***
C
c     ------------------------------------------------------------------
      INTEGER L, LIV
      INTEGER IV(LIV)
      REAL             C(L)
C
      INTEGER PU
c%%   long int i;
      INTEGER I
C
      INTEGER PRUNIT, SOLPRT
C
      PARAMETER (PRUNIT=21, SOLPRT=22)
 10   FORMAT(/' LINEAR PARAMETERS...'//(1X,I5,g16.6))
C  ***  BODY  ***
C
      IF (IV(1) .GT. 11) GO TO 999
      IF (IV(SOLPRT) .EQ. 0) GO TO 999
      PU = IV(PRUNIT)
      IF (PU .EQ. 0) GO TO 999
c++ CODE for .C. is inactive
C%%    if( l > 0 ) {
C%%        printf( "\n LINEAR PARAMETERS...\n" );
C%%        for( i = 1; i <= l; i++ ) printf( "\n%5ld%16.6g", i, C[i] );
C%%        printf( "\n" );}
c++ CODE for ~.C. is active
      IF (L .GT. 0) WRITE(PU,10) (I, C(I), I = 1, L)
c++ END
C
 999  RETURN
C  ***  LAST LINE OF SS7CPR FOLLOWS  ***
      END
      SUBROUTINE SV7PRM(N, IP, X)
C
C     PERMUTE X SO THAT X.OUTPUT(IP(I)) = X.INPUT(I).
C     IP IS UNCHANGED ON OUTPUT.
C
c     ------------------------------------------------------------------
      INTEGER N
      INTEGER IP(N)
      REAL             X(N)
C
      INTEGER I, J, K
      REAL             S, T
      DO 30 I = 1, N
         J = IP(I)
         IF (J .EQ. I) GO TO 30
         IF (J .GT. 0) GO TO 10
            IP(I) = -J
            GO TO 30
 10      T = X(I)
 20      S = X(J)
         X(J) = T
         T = S
         K = J
         J = IP(K)
         IP(K) = -J
         IF (J .GT. I) GO TO 20
         X(J) = T
 30      CONTINUE
C  ***  LAST LINE OF SV7PRM FOLLOWS  ***
      END
      SUBROUTINE SV7SWP(N, X, Y)
C
C  ***  INTERCHANGE N-VECTORS X AND Y.  ***
C
c     ------------------------------------------------------------------
      INTEGER N
      REAL             X(N), Y(N)
C
      INTEGER I
      REAL             T
C
      DO 10 I = 1, N
         T = X(I)
         X(I) = Y(I)
         Y(I) = T
 10      CONTINUE
C  ***  LAST CARD OF SV7SWP FOLLOWS  ***
      END
