* File: DIVSET.for      28 subrs used by all main subrs of the
*       David Gay & Linda Kaufman nonlinear LS package.
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2001-06-18 DIVSET Krogh  Replaced ". AND." with " .AND."
C>> 1998-10-29 DIVSET Krogh  Moved external statements up for mangle.
C>> 1996-06-18 DIVSET Krogh  More work for for C conversion.
c>> 1995-11-15 DIVSET Krogh  Moved formats up for C conversion.
c>> 1994-11-02 DIVSET Krogh  Changes to use M77CON
c>> 1992-04-27 DIVSET CLL Removed unreferenced stmt labels.
c>> 1992-04-13 CLL Change from Hollerith to '...' syntax in formats.
c>> 1991-05-21 CLL JPL  Changing (1) to (*) in declarations.
c>> 1990-06-15 CLL JPL
c>> 1990-03-21 CLL JPL
c>> 1990-02-16 CLL JPL
      SUBROUTINE DIVSET(ALG, IV, LIV, LV, V)
C
C  ***  SUPPLY ***SOL (VERSION 2.3) DEFAULT VALUES TO IV AND V  ***
C
C  ***  ALG = 1 MEANS REGRESSION CONSTANTS.
C  ***  ALG = 2 MEANS GENERAL UNCONSTRAINED OPTIMIZATION CONSTANTS.
c     ------------------------------------------------------------------
c     1990-06-25 CLL changed default settings for printing so the
c     default is no printing.  This involved setting COVPRT, OUTLEV,
C     PARPRT, SOLPRT, STATPR, and X0PRT to zero.  Previously these were
c     all set to 1 except that COVPRT = 3.
c     ------------------------------------------------------------------
C
      INTEGER LIV, LV
      INTEGER ALG, IV(LIV)
      DOUBLE PRECISION V(LV)
C
      EXTERNAL DV7DFL, I1MACH
      INTEGER  I1MACH
c--D replaces "?": ?IVSET,?V7DFL,?R7MDC,?L7SVN,?D7TPR,?V2AXY,?V2NRM
c--& ?Q7APL,?D7UPD,?V7SCP,?Q7RAD,?V7SCL,?PARCK,?V7CPY,?S7LVM,?S7LUP
c--& ?L7MST,?L7ITV,?L7IVM,?G7QTS,?L7SRT,?L7SVX,?A7SST,?ITSUM,?L7SQR
c--& ?L7TVM,?L7VML,?RLDST
C I1MACH... RETURNS MACHINE-DEPENDENT INTEGER CONSTANTS.
C DV7DFL.... PROVIDES DEFAULT VALUES TO V.
C
      INTEGER ALG1, MIV, MV
      INTEGER MINIV(4), MINV(4)
C
C  ***  SUBSCRIPTS FOR IV  ***
C
      INTEGER ALGSAV, COVPRT, COVREQ, DRADPR, DTYPE, HC, IERR, INITH,
     1        INITS, IPIVOT, IVNEED, LASTIV, LASTV, LMAT, MXFCAL,
     2        MXITER, NFCOV, NGCOV, NVDFLT, NVSAVE, OUTLEV, PARPRT,
     3        PARSAV, PERM, PRUNIT, QRTYP, RDREQ, RMAT, SOLPRT, STATPR,
     4        VNEED, VSAVE, X0PRT
C
C  ***  IV SUBSCRIPT VALUES  ***
C
      PARAMETER (ALGSAV=51, COVPRT=14, COVREQ=15, DRADPR=101, DTYPE=16,
     1           HC=71, IERR=75, INITH=25, INITS=25, IPIVOT=76,
     2           IVNEED=3, LASTIV=44, LASTV=45, LMAT=42, MXFCAL=17,
     3           MXITER=18, NFCOV=52, NGCOV=53, NVDFLT=50, NVSAVE=9,
     4           OUTLEV=19, PARPRT=20, PARSAV=49, PERM=58, PRUNIT=21,
     5           QRTYP=80, RDREQ=57, RMAT=78, SOLPRT=22, STATPR=23,
     6           VNEED=4, VSAVE=60, X0PRT=24)
      DATA MINIV(1)/82/, MINIV(2)/59/, MINIV(3)/103/, MINIV(4)/103/,
     1     MINV(1)/98/, MINV(2)/71/, MINV(3)/101/, MINV(4)/85/
C
C ------------------------------  BODY  --------------------------------
C
C        I1MACH(2) returns the unit number for standard output.
c
      IF (PRUNIT .LE. LIV) IV(PRUNIT) = I1MACH(2)
      IF (ALGSAV .LE. LIV) IV(ALGSAV) = ALG
      IF (ALG .LT. 1 .OR. ALG .GT. 4) GO TO 40
      MIV = MINIV(ALG)
      IF (LIV .LT. MIV) GO TO 20
      MV = MINV(ALG)
      IF (LV .LT. MV) GO TO 30
      ALG1 = MOD(ALG-1,2) + 1
      CALL DV7DFL(ALG1, LV, V)
      IV(1) = 12
      IF (ALG .GT. 2) IV(DRADPR) = 1
      IV(IVNEED) = 0
      IV(LASTIV) = MIV
      IV(LASTV) = MV
      IV(LMAT) = MV + 1
      IV(MXFCAL) = 200
      IV(MXITER) = 150
      IV(OUTLEV) = 0
      IV(PARPRT) = 0
      IV(PERM) = MIV + 1
      IV(SOLPRT) = 0
      IV(STATPR) = 0
      IV(VNEED) = 0
      IV(X0PRT) = 0
C
      IF (ALG1 .GE. 2) GO TO 10
C
C  ***  REGRESSION  VALUES
C
      IV(COVPRT) = 0
      IV(COVREQ) = 1
      IV(DTYPE) = 1
      IV(HC) = 0
      IV(IERR) = 0
      IV(INITS) = 0
      IV(IPIVOT) = 0
      IV(NVDFLT) = 32
      IV(VSAVE) = 58
      IF (ALG .GT. 2) IV(VSAVE) = IV(VSAVE) + 3
      IV(PARSAV) = IV(VSAVE) + NVSAVE
      IV(QRTYP) = 1
      IV(RDREQ) = 3
      IV(RMAT) = 0
      GO TO 999
C
C  ***  GENERAL OPTIMIZATION VALUES
C
 10   IV(DTYPE) = 0
      IV(INITH) = 1
      IV(NFCOV) = 0
      IV(NGCOV) = 0
      IV(NVDFLT) = 25
      IV(PARSAV) = 47
      IF (ALG .GT. 2) IV(PARSAV) = 61
      GO TO 999
C
 20   IV(1) = 15
      GO TO 999
C
 30   IV(1) = 16
      GO TO 999
C
 40   IV(1) = 67
C
 999  RETURN
C  ***  LAST CARD OF DIVSET FOLLOWS  ***
      END
      SUBROUTINE DV7DFL(ALG, LV, V)
C
C  ***  SUPPLY ***SOL (VERSION 2.3) DEFAULT VALUES TO V  ***
C
C  ***  ALG = 1 MEANS REGRESSION CONSTANTS.
C  ***  ALG = 2 MEANS GENERAL UNCONSTRAINED OPTIMIZATION CONSTANTS.
C
      INTEGER ALG, LV
      DOUBLE PRECISION V(LV)
C
      EXTERNAL DR7MDC
      DOUBLE PRECISION DR7MDC
C DR7MDC... RETURNS MACHINE-DEPENDENT CONSTANTS
C
      DOUBLE PRECISION MACHEP, MEPCRT, ONE, SQTEPS, THREE
C
C  ***  SUBSCRIPTS FOR V  ***
C
      INTEGER AFCTOL, BIAS, COSMIN, DECFAC, DELTA0, DFAC, DINIT, DLTFDC,
     1        DLTFDJ, DTINIT, D0INIT, EPSLON, ETA0, FUZZ, HUBERC,
     2        INCFAC, LMAX0, LMAXS, PHMNFC, PHMXFC, RDFCMN, RDFCMX,
     3        RFCTOL, RLIMIT, RSPTOL, SCTOL, SIGMIN, TUNER1, TUNER2,
     4        TUNER3, TUNER4, TUNER5, XCTOL, XFTOL
C
      PARAMETER (ONE=1.D+0, THREE=3.D+0)
C
C  ***  V SUBSCRIPT VALUES  ***
C
      PARAMETER (AFCTOL=31, BIAS=43, COSMIN=47, DECFAC=22, DELTA0=44,
     1           DFAC=41, DINIT=38, DLTFDC=42, DLTFDJ=43, DTINIT=39,
     2           D0INIT=40, EPSLON=19, ETA0=42, FUZZ=45, HUBERC=48,
     3           INCFAC=23, LMAX0=35, LMAXS=36, PHMNFC=20, PHMXFC=21,
     4           RDFCMN=24, RDFCMX=25, RFCTOL=32, RLIMIT=46, RSPTOL=49,
     5           SCTOL=37, SIGMIN=50, TUNER1=26, TUNER2=27, TUNER3=28,
     6           TUNER4=29, TUNER5=30, XCTOL=33, XFTOL=34)
C ------------------------------  BODY  --------------------------------
C
      MACHEP = DR7MDC(3)
      V(AFCTOL) = 1.D-20
      IF (MACHEP .GT. 1.D-10) V(AFCTOL) = MACHEP**2
      V(DECFAC) = 0.5D+0
      SQTEPS = DR7MDC(4)
      V(DFAC) = 0.6D+0
      V(DTINIT) = 1.D-6
      MEPCRT = MACHEP ** (ONE/THREE)
      V(D0INIT) = 1.D+0
      V(EPSLON) = 0.1D+0
      V(INCFAC) = 2.D+0
      V(LMAX0) = 1.D+0
      V(LMAXS) = 1.D+0
      V(PHMNFC) = -0.1D+0
      V(PHMXFC) = 0.1D+0
      V(RDFCMN) = 0.1D+0
      V(RDFCMX) = 4.D+0
      V(RFCTOL) = max(1.D-10, MEPCRT**2)
      V(SCTOL) = V(RFCTOL)
      V(TUNER1) = 0.1D+0
      V(TUNER2) = 1.D-4
      V(TUNER3) = 0.75D+0
      V(TUNER4) = 0.5D+0
      V(TUNER5) = 0.75D+0
      V(XCTOL) = SQTEPS
      V(XFTOL) = 1.D+2 * MACHEP
C
      IF (ALG .GE. 2) GO TO 10
C
C  ***  REGRESSION  VALUES
C
      V(COSMIN) = max(1.D-6, 1.D+2 * MACHEP)
      V(DINIT) = 0.D+0
      V(DELTA0) = SQTEPS
      V(DLTFDC) = MEPCRT
      V(DLTFDJ) = SQTEPS
      V(FUZZ) = 1.5D+0
      V(HUBERC) = 0.7D+0
      V(RLIMIT) = DR7MDC(5)
      V(RSPTOL) = 1.D-3
      V(SIGMIN) = 1.D-4
      GO TO 999
C
C  ***  GENERAL OPTIMIZATION VALUES
C
 10   V(BIAS) = 0.8D+0
      V(DINIT) = -1.0D+0
      V(ETA0) = 1.0D+3 * MACHEP
C
 999  RETURN
C  ***  LAST CARD OF DV7DFL FOLLOWS  ***
      END
      DOUBLE PRECISION FUNCTION DR7MDC(K)
C
C  ***  RETURN MACHINE DEPENDENT CONSTANTS USED BY NL2SOL  ***
C
      INTEGER K
C
C  ***  THE CONSTANT RETURNED DEPENDS ON K...
C
C  ***        K = 1... SMALLEST POS. ETA SUCH THAT -ETA EXISTS.
C  ***        K = 2... SQUARE ROOT OF ETA.
C  ***        K = 3... UNIT ROUNDOFF = SMALLEST POS. NO. MACHEP SUCH
C  ***                 THAT 1 + MACHEP .GT. 1 .AND. 1 - MACHEP .LT. 1.
C  ***        K = 4... SQUARE ROOT OF MACHEP.
C  ***        K = 5... SQUARE ROOT OF BIG (SEE K = 6).
C  ***        K = 6... LARGEST MACHINE NO. BIG SUCH THAT -BIG EXISTS.
C
      DOUBLE PRECISION BIG, ETA, MACHEP
C
      EXTERNAL D1MACH
      DOUBLE PRECISION D1MACH, ZERO
      DATA BIG/0.D+0/, ETA/0.D+0/, MACHEP/0.D+0/, ZERO/0.D+0/
      IF (BIG .GT. ZERO) GO TO 1
         BIG = D1MACH(2)
         ETA = D1MACH(1)
         MACHEP = D1MACH(4)
 1    CONTINUE
C
C ------------------------------  BODY  --------------------------------
C
      GO TO (10, 20, 30, 40, 50, 60), K
C
 10   DR7MDC = ETA
      GO TO 999
C
 20   DR7MDC = sqrt(256.D+0*ETA)/16.D+0
      GO TO 999
C
 30   DR7MDC = MACHEP
      GO TO 999
C
 40   DR7MDC = sqrt(MACHEP)
      GO TO 999
C
 50   DR7MDC = sqrt(BIG/256.D+0)*16.D+0
      GO TO 999
C
 60   DR7MDC = BIG
C
 999  RETURN
C  ***  LAST CARD OF DR7MDC FOLLOWS  ***
      END
      DOUBLE PRECISION FUNCTION DL7SVN(P, L, X, Y)
C
C  ***  ESTIMATE SMALLEST SING. VALUE OF PACKED LOWER TRIANG. MATRIX L
C
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER P
      DOUBLE PRECISION L(*), X(P), Y(P)
C     DIMENSION L(P*(P+1)/2)
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  ***  PURPOSE  ***
C
C     THIS FUNCTION RETURNS A GOOD OVER-ESTIMATE OF THE SMALLEST
C     SINGULAR VALUE OF THE PACKED LOWER TRIANGULAR MATRIX L.
C
C  ***  PARAMETER DESCRIPTION  ***
C
C  P (IN)  = THE ORDER OF L.  L IS A  P X P  LOWER TRIANGULAR MATRIX.
C  L (IN)  = ARRAY HOLDING THE ELEMENTS OF  L  IN ROW ORDER, I.E.
C             L(1,1), L(2,1), L(2,2), L(3,1), L(3,2), L(3,3), ETC.
C  X (OUT) IF DL7SVN RETURNS A POSITIVE VALUE, THEN X IS A NORMALIZED
C             APPROXIMATE LEFT SINGULAR VECTOR CORRESPONDING TO THE
C             SMALLEST SINGULAR VALUE.  THIS APPROXIMATION MAY BE VERY
C             CRUDE.  IF DL7SVN RETURNS ZERO, THEN SOME COMPONENTS OF X
C             ARE ZERO AND THE REST RETAIN THEIR INPUT VALUES.
C  Y (OUT) IF DL7SVN RETURNS A POSITIVE VALUE, THEN Y = (L**-1)*X IS AN
C             UNNORMALIZED APPROXIMATE RIGHT SINGULAR VECTOR CORRESPOND-
C             ING TO THE SMALLEST SINGULAR VALUE.  THIS APPROXIMATION
C             MAY BE CRUDE.  IF DL7SVN RETURNS ZERO, THEN Y RETAINS ITS
C             INPUT VALUE.  THE CALLER MAY PASS THE SAME VECTOR FOR X
C             AND Y (NONSTANDARD FORTRAN USAGE), IN WHICH CASE Y OVER-
C             WRITES X (FOR NONZERO DL7SVN RETURNS).
C
C  ***  ALGORITHM NOTES  ***
C
C     THE ALGORITHM IS BASED ON (1), WITH THE ADDITIONAL PROVISION THAT
C     DL7SVN = 0 IS RETURNED IF THE SMALLEST DIAGONAL ELEMENT OF L
C     (IN MAGNITUDE) IS NOT MORE THAN THE UNIT ROUNDOFF TIMES THE
C     LARGEST.  THE ALGORITHM USES A RANDOM NUMBER GENERATOR PROPOSED
C     IN (4), WHICH PASSES THE SPECTRAL TEST WITH FLYING COLORS -- SEE
C     (2) AND (3).
C
C  ***  SUBROUTINES AND FUNCTIONS CALLED  ***
C
C        DV2NRM - FUNCTION, RETURNS THE 2-NORM OF A VECTOR.
C
C  ***  REFERENCES  ***
C
C     (1) CLINE, A., MOLER, C., STEWART, G., AND WILKINSON, J.H.(1977),
C         AN ESTIMATE FOR THE CONDITION NUMBER OF A MATRIX, REPORT
C         TM-310, APPLIED MATH. DIV., ARGONNE NATIONAL LABORATORY.
C
C     (2) HOAGLIN, D.C. (1976), THEORETICAL PROPERTIES OF CONGRUENTIAL
C         RANDOM-NUMBER GENERATORS --  AN EMPIRICAL VIEW,
C         MEMORANDUM NS-340, DEPT. OF STATISTICS, HARVARD UNIV.
C
C     (3) KNUTH, D.E. (1969), THE ART OF COMPUTER PROGRAMMING, VOL. 2
C         (SEMINUMERICAL ALGORITHMS), ADDISON-WESLEY, READING, MASS.
C
C     (4) SMITH, C.S. (1971), MULTIPLICATIVE PSEUDO-RANDOM NUMBER
C         GENERATORS WITH PRIME MODULUS, J. ASSOC. COMPUT. MACH. 18,
C         PP. 586-593.
C
C  ***  HISTORY  ***
C
C     DESIGNED AND CODED BY DAVID M. GAY (WINTER 1977/SUMMER 1978).
C
C  ***  GENERAL  ***
C
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324, DCR75-10143, 76-14311DSS, AND MCS76-11989.
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER I, II, IX, J, JI, JJ, JJJ, JM1, J0, PM1
      DOUBLE PRECISION B, SMINUS, SPLUS, T, XMINUS, XPLUS
C
C  ***  CONSTANTS  ***
C
      DOUBLE PRECISION HALF, ONE, R9973, ZERO
C
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***
C
      EXTERNAL DD7TPR, DV2NRM,DV2AXY
      DOUBLE PRECISION DD7TPR, DV2NRM
C
      PARAMETER (HALF=0.5D+0, ONE=1.D+0, R9973=9973.D+0, ZERO=0.D+0)
C
C  ***  BODY  ***
C
      IX = 2
      PM1 = P - 1
C
C  ***  FIRST CHECK WHETHER TO RETURN DL7SVN = 0 AND INITIALIZE X  ***
C
      II = 0
      J0 = P*PM1/2
      JJ = J0 + P
      IF (L(JJ) .EQ. ZERO) GO TO 110
      IX = MOD(3432*IX, 9973)
      B = HALF*(ONE + dble(IX)/R9973)
      XPLUS = B / L(JJ)
      X(P) = XPLUS
      IF (P .LE. 1) GO TO 60
      DO 10 I = 1, PM1
         II = II + I
         IF (L(II) .EQ. ZERO) GO TO 110
         JI = J0 + I
         X(I) = XPLUS * L(JI)
 10      CONTINUE
C
C  ***  SOLVE (L**T)*X = B, WHERE THE COMPONENTS OF B HAVE RANDOMLY
C  ***  CHOSEN MAGNITUDES IN (.5,1) WITH SIGNS CHOSEN TO MAKE X LARGE.
C
C     DO J = P-1 TO 1 BY -1...
      DO 50 JJJ = 1, PM1
         J = P - JJJ
C       ***  DETERMINE X(J) IN THIS ITERATION. NOTE FOR I = 1,2,...,J
C       ***  THAT X(I) HOLDS THE CURRENT PARTIAL SUM FOR ROW I.
         IX = MOD(3432*IX, 9973)
         B = HALF*(ONE + dble(IX)/R9973)
         XPLUS = (B - X(J))
         XMINUS = (-B - X(J))
         SPLUS = abs(XPLUS)
         SMINUS = abs(XMINUS)
         JM1 = J - 1
         J0 = J*JM1/2
         JJ = J0 + J
         XPLUS = XPLUS/L(JJ)
         XMINUS = XMINUS/L(JJ)
         IF (JM1 .EQ. 0) GO TO 30
         DO 20 I = 1, JM1
              JI = J0 + I
              SPLUS = SPLUS + abs(X(I) + L(JI)*XPLUS)
              SMINUS = SMINUS + abs(X(I) + L(JI)*XMINUS)
 20           CONTINUE
 30      IF (SMINUS .GT. SPLUS) XPLUS = XMINUS
         X(J) = XPLUS
C       ***  UPDATE PARTIAL SUMS  ***
         IF (JM1 .GT. 0) CALL DV2AXY(JM1, X, XPLUS, L(J0+1), X)
 50      CONTINUE
C
C  ***  NORMALIZE X  ***
C
 60   T = ONE/DV2NRM(P, X)
      DO 70 I = 1, P
 70      X(I) = T*X(I)
C
C  ***  SOLVE L*Y = X AND RETURN DL7SVN = 1/TWONORM(Y)  ***
C
      DO 100 J = 1, P
         JM1 = J - 1
         J0 = J*JM1/2
         JJ = J0 + J
         T = ZERO
         IF (JM1 .GT. 0) T = DD7TPR(JM1, L(J0+1), Y)
         Y(J) = (X(J) - T) / L(JJ)
 100     CONTINUE
C
      DL7SVN = ONE/DV2NRM(P, Y)
      GO TO 999
C
 110  DL7SVN = ZERO
 999  RETURN
C  ***  LAST CARD OF DL7SVN FOLLOWS  ***
      END
      SUBROUTINE DQ7APL(NN, N, P, J, R, IERR)
C     *****PARAMETERS.
      INTEGER NN, N, P, IERR
      DOUBLE PRECISION J(NN,P), R(N)
C
C     ..................................................................
C     ..................................................................
C
C     *****PURPOSE.
C     THIS SUBROUTINE APPLIES TO R THE ORTHOGONAL TRANSFORMATIONS
C     STORED IN J BY QRFACT
C
C     *****PARAMETER DESCRIPTION.
C     ON INPUT.
C
C        NN IS THE ROW DIMENSION OF THE MATRIX J AS DECLARED IN
C             THE CALLING PROGRAM DIMENSION STATEMENT
C
C        N IS THE NUMBER OF ROWS OF J AND THE SIZE OF THE VECTOR R
C
C        P IS THE NUMBER OF COLUMNS OF J AND THE SIZE OF SIGMA
C
C        J CONTAINS ON AND BELOW ITS DIAGONAL THE COLUMN VECTORS
C             U WHICH DETERMINE THE HOUSEHOLDER TRANSFORMATIONS
C             IDENT - U*U.TRANSPOSE
C
C        R IS THE RIGHT HAND SIDE VECTOR TO WHICH THE ORTHOGONAL
C             TRANSFORMATIONS WILL BE APPLIED
C
C        IERR IF NON-ZERO INDICATES THAT NOT ALL THE TRANSFORMATIONS
C             WERE SUCCESSFULLY DETERMINED AND ONLY THE FIRST
C             ABS(IERR) - 1 TRANSFORMATIONS WILL BE USED
C
C     ON OUTPUT.
C
C        R HAS BEEN OVERWRITTEN BY ITS TRANSFORMED IMAGE
C
C     *****APPLICATION AND USAGE RESTRICTIONS.
C     NONE
C
C     *****ALGORITHM NOTES.
C     THE VECTORS U WHICH DETERMINE THE HOUSEHOLDER TRANSFORMATIONS
C     ARE NORMALIZED SO THAT THEIR 2-NORM SQUARED IS 2.  THE USE OF
C     THESE TRANSFORMATIONS HERE IS IN THE SPIRIT OF (1).
C
C     *****SUBROUTINES AND FUNCTIONS CALLED.
C
C     DD7TPR - FUNCTION, RETURNS THE INNER PRODUCT OF VECTORS
C
C     *****REFERENCES.
C     (1) BUSINGER, P. A., AND GOLUB, G. H. (1965), LINEAR LEAST SQUARES
C        SOLUTIONS BY HOUSEHOLDER TRANSFORMATIONS, NUMER. MATH. 7,
C        PP. 269-276.
C
C     *****HISTORY.
C     DESIGNED BY DAVID M. GAY, CODED BY STEPHEN C. PETERS (WINTER 1977)
C     CALL ON DV2AXY SUBSTITUTED FOR DO LOOP, FALL 1983.
C
C     *****GENERAL.
C
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324, DCR75-10143, 76-14311DSS, AND MCS76-11989.
C
C     ..................................................................
C     ..................................................................
C
C     *****LOCAL VARIABLES.
      INTEGER K, L, NL1
C     *****FUNCTIONS.
      EXTERNAL DD7TPR,DV2AXY
      DOUBLE PRECISION DD7TPR
c     ------------------------------------------------------------------
C
C  ***  BODY  ***
C
      K = P
      IF (IERR .NE. 0) K = abs(IERR) - 1
      IF ( K .EQ. 0) GO TO 999
C
      DO 20 L = 1, K
         NL1 = N - L + 1
         CALL DV2AXY(NL1, R(L), -DD7TPR(NL1,J(L,L),R(L)), J(L,L), R(L))
 20   CONTINUE
C
 999  RETURN
C  ***  LAST LINE OF DQ7APL FOLLOWS  ***
      END
      DOUBLE PRECISION FUNCTION DV2NRM(P, X)
C
C  ***  RETURN THE 2-NORM OF THE P-VECTOR X, TAKING  ***
C  ***  CARE TO AVOID THE MOST LIKELY UNDERFLOWS.    ***
C
      INTEGER P
      DOUBLE PRECISION X(P)
C
      INTEGER I, J
      DOUBLE PRECISION ONE, R, SCALE, SQTETA, T, XI, ZERO
      EXTERNAL DR7MDC
      DOUBLE PRECISION DR7MDC
c     ------------------------------------------------------------------
      PARAMETER (ONE=1.D+0, ZERO=0.D+0)
      SAVE SQTETA
      DATA SQTETA/0.D+0/
c     ------------------------------------------------------------------
      IF (P .GT. 0) GO TO 10
         DV2NRM = ZERO
         GO TO 999
 10   DO 20 I = 1, P
         IF (X(I) .NE. ZERO) GO TO 30
 20      CONTINUE
      DV2NRM = ZERO
      GO TO 999
C
 30   SCALE = abs(X(I))
      IF (I .LT. P) GO TO 40
         DV2NRM = SCALE
         GO TO 999
 40   T = ONE
      IF (SQTETA .EQ. ZERO) SQTETA = DR7MDC(2)
C
C     ***  SQTETA IS (SLIGHTLY LARGER THAN) THE SQUARE ROOT OF THE
C     ***  SMALLEST POSITIVE FLOATING POINT NUMBER ON THE MACHINE.
C     ***  THE TESTS INVOLVING SQTETA ARE DONE TO PREVENT UNDERFLOWS.
C
      J = I + 1
      DO 60 I = J, P
         XI = abs(X(I))
         IF (XI .GT. SCALE) GO TO 50
              R = XI / SCALE
              IF (R .GT. SQTETA) T = T + R*R
              GO TO 60
 50           R = SCALE / XI
              IF (R .LE. SQTETA) R = ZERO
              T = ONE  +  T * R*R
              SCALE = XI
 60      CONTINUE
C
      DV2NRM = SCALE * sqrt(T)
 999  RETURN
C  ***  LAST LINE OF DV2NRM FOLLOWS  ***
      END
      DOUBLE PRECISION FUNCTION DD7TPR(P, X, Y)
C
C  ***  RETURN THE INNER PRODUCT OF THE P-VECTORS X AND Y.  ***
C
      INTEGER P
      DOUBLE PRECISION X(P), Y(P)
C
      INTEGER I
      DOUBLE PRECISION ONE, SQTETA, T, ZERO
      EXTERNAL DR7MDC
      DOUBLE PRECISION DR7MDC
C
C  ***  DR7MDC(2) RETURNS A MACHINE-DEPENDENT CONSTANT, SQTETA, WHICH
C  ***  IS SLIGHTLY LARGER THAN THE SMALLEST POSITIVE NUMBER THAT
C  ***  CAN BE SQUARED WITHOUT UNDERFLOWING.
C
      PARAMETER (ONE=1.D+0, ZERO=0.D+0)
      DATA SQTETA/0.D+0/
C
      DD7TPR = ZERO
      IF (P .LE. 0) GO TO 999
      IF (SQTETA .EQ. ZERO) SQTETA = DR7MDC(2)
      DO 20 I = 1, P
         T = max(abs(X(I)), abs(Y(I)))
         IF (T .GT. ONE) GO TO 10
         IF (T .LT. SQTETA) GO TO 20
         T = (X(I)/SQTETA)*Y(I)
         IF (abs(T) .LT. SQTETA) GO TO 20
 10      DD7TPR = DD7TPR + X(I)*Y(I)
 20   CONTINUE
C
 999  RETURN
C  ***  LAST LINE OF DD7TPR FOLLOWS  ***
      END
      SUBROUTINE DD7UPD(D, DR, IV, LIV, LV, N, ND, NN, N2, P, V)
C
C  ***  UPDATE SCALE VECTOR D FOR NL2IT  ***
C
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER LIV, LV, N, ND, NN, N2, P
      INTEGER IV(LIV)
      DOUBLE PRECISION D(P), DR(ND,P), V(LV)
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER D0, I, JCN0, JCN1, JCNI, JTOL0, JTOLI, K, SII
      DOUBLE PRECISION T, VDFAC
C
C     ***  CONSTANTS  ***
C
      DOUBLE PRECISION ZERO
C
C  ***  EXTERNAL SUBROUTINE  ***
C
      EXTERNAL DV7SCP
C
C DV7SCP... SETS ALL COMPONENTS OF A VECTOR TO A SCALAR.
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
      INTEGER DFAC, DTYPE, JCN, JTOL, NITER, S
      PARAMETER (DFAC=41, DTYPE=16, JCN=66, JTOL=59, NITER=31, S=62)
      PARAMETER (ZERO=0.D+0)
C ------------------------------  BODY  --------------------------------
C
      IF (IV(DTYPE) .NE. 1 .AND. IV(NITER) .GT. 0) GO TO 999
      JCN1 = IV(JCN)
      JCN0 = abs(JCN1) - 1
      IF (JCN1 .LT. 0) GO TO 10
         IV(JCN) = -JCN1
         CALL DV7SCP(P, V(JCN1), ZERO)
 10   DO 30 I = 1, P
         JCNI = JCN0 + I
         T  = V(JCNI)
         DO 20 K = 1, NN
 20           T = max(T, abs(DR(K,I)))
         V(JCNI) = T
 30      CONTINUE
      IF (N2 .LT. N) GO TO 999
      VDFAC = V(DFAC)
      JTOL0 = IV(JTOL) - 1
      D0 = JTOL0 + P
      SII = IV(S) - 1
      DO 50 I = 1, P
         SII = SII + I
         JCNI = JCN0 + I
         T = V(JCNI)
         IF (V(SII) .GT. ZERO) T = max(sqrt(V(SII)), T)
         JTOLI = JTOL0 + I
         D0 = D0 + 1
         IF (T .LT. V(JTOLI)) T = max(V(D0), V(JTOLI))
         D(I) = max(VDFAC*D(I), T)
 50      CONTINUE
C
 999  RETURN
C  ***  LAST CARD OF DD7UPD FOLLOWS  ***
      END
      SUBROUTINE DQ7RAD(N, NN, P, QTR, QTRSET, RMAT, W, Y)
C
C  ***  ADD ROWS W TO QR FACTORIZATION WITH R MATRIX RMAT AND
C  ***  Q**T * RESIDUAL = QTR.  Y = NEW COMPONENTS OF RESIDUAL
C  ***  CORRESPONDING TO W.  QTR, Y REFERENCED ONLY IF QTRSET = .TRUE.
C
      LOGICAL QTRSET
      INTEGER N, NN, P
      DOUBLE PRECISION QTR(P), RMAT(*), W(NN,P), Y(N)
C     DIMENSION RMAT(P*(P+1)/2)
      EXTERNAL DD7TPR, DR7MDC,DV2AXY, DV7SCL, DV2NRM
      DOUBLE PRECISION DD7TPR, DR7MDC, DV2NRM
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER I, II, IJ, IP1, J, K, NK
      DOUBLE PRECISION ARI, QRI, RI, SS, T, WI
      DOUBLE PRECISION BIG, BIGRT, ONE, TINY, TINYRT, ZERO
      SAVE BIGRT, TINY, TINYRT
      DATA BIG/-1.D+0/, BIGRT/-1.D+0/, ONE/1.D+0/, TINY/0.D+0/,
     1     TINYRT/0.D+0/, ZERO/0.D+0/
C
C ----------------------------- BODY -----------------------------------
C
      IF (TINY .GT. ZERO) GO TO 10
         TINY = DR7MDC(1)
         BIG = DR7MDC(6)
         IF (TINY*BIG .LT. ONE) TINY = ONE / BIG
 10   K = 1
      NK = N
      II = 0
      DO 180 I = 1, P
         II = II + I
         IP1 = I + 1
         IJ = II + I
         IF (NK .LE. 1) T = abs(W(K,I))
         IF (NK .GT. 1) T = DV2NRM(NK, W(K,I))
         IF (T .LT. TINY) GOTO  180
         RI = RMAT(II)
         IF (RI .NE. ZERO) GO TO 100
            IF (NK .GT. 1) GO TO 30
               IJ = II
               DO 20 J = I, P
                  RMAT(IJ) = W(K,J)
                  IJ = IJ + J
 20               CONTINUE
               IF (QTRSET) QTR(I) = Y(K)
               W(K,I) = ZERO
               GO TO 999
 30         WI = W(K,I)
            IF (BIGRT .GT. ZERO) GO TO 40
               BIGRT = DR7MDC(5)
               TINYRT = DR7MDC(2)
 40         IF (T .LE. TINYRT) GO TO 50
            IF (T .GE. BIGRT) GO TO 50
               IF (WI .LT. ZERO) T = -T
               WI = WI + T
               SS = sqrt(T * WI)
               GO TO 70
 50         SS = sqrt(T)
            IF (WI .LT. ZERO) GO TO 60
               WI = WI + T
               SS = SS * sqrt(WI)
               GO TO 70
 60         T = -T
            WI = WI + T
            SS = SS * sqrt(-WI)
 70         W(K,I) = WI
            CALL DV7SCL(NK, W(K,I), ONE/SS, W(K,I))
            RMAT(II) = -T
            IF (.NOT. QTRSET) GO TO 80
            CALL DV2AXY(NK, Y(K), -DD7TPR(NK,Y(K),W(K,I)), W(K,I), Y(K))
            QTR(I) = Y(K)
 80         IF (IP1 .GT. P) GO TO 999
            DO 90 J = IP1, P
               CALL DV2AXY(NK, W(K,J), -DD7TPR(NK,W(K,J),W(K,I)),
     1                    W(K,I), W(K,J))
               RMAT(IJ) = W(K,J)
               IJ = IJ + J
 90            CONTINUE
            IF (NK .LE. 1) GO TO 999
            K = K + 1
            NK = NK - 1
            GO TO 180
C
 100     ARI = abs(RI)
         IF (ARI .GT. T) GO TO 110
            T = T * sqrt(ONE + (ARI/T)**2)
            GO TO 120
 110     T = ARI * sqrt(ONE + (T/ARI)**2)
 120     IF (RI .LT. ZERO) T = -T
         RI = RI + T
         RMAT(II) = -T
         SS = -RI / T
         IF (NK .LE. 1) GO TO 150
         CALL DV7SCL(NK, W(K,I), ONE/RI, W(K,I))
         IF (.NOT. QTRSET) GO TO 130
            QRI = QTR(I)
            T = SS * ( QRI  +  DD7TPR(NK, Y(K), W(K,I)) )
            QTR(I) = QRI + T
 130     IF (IP1 .GT. P) GO TO 999
         IF (QTRSET) CALL DV2AXY(NK, Y(K), T, W(K,I), Y(K))
         DO 140 J = IP1, P
            RI = RMAT(IJ)
            T = SS * ( RI  +  DD7TPR(NK, W(K,J), W(K,I)) )
            CALL DV2AXY(NK, W(K,J), T, W(K,I), W(K,J))
            RMAT(IJ) = RI + T
            IJ = IJ + J
 140        CONTINUE
         GO TO 180
C
 150     WI = W(K,I) / RI
         W(K,I) = WI
         IF (.NOT. QTRSET) GO TO 160
            QRI = QTR(I)
            T = SS * ( QRI + Y(K)*WI )
            QTR(I) = QRI + T
 160     IF (IP1 .GT. P) GO TO 999
         IF (QTRSET) Y(K) = T*WI + Y(K)
         DO 170 J = IP1, P
            RI = RMAT(IJ)
            T = SS * (RI + W(K,J)*WI)
            W(K,J) = W(K,J) + T*WI
            RMAT(IJ) = RI + T
            IJ = IJ + J
 170        CONTINUE
 180     CONTINUE
C
 999  RETURN
C  ***  LAST LINE OF DQ7RAD FOLLOWS  ***
      END
      SUBROUTINE DPARCK(ALG, D, IV, LIV, LV, N, V)
C
C  ***  CHECK ***SOL (VERSION 2.3) PARAMETERS, PRINT CHANGED VALUES  ***
C
C  ***  ALG = 1 FOR REGRESSION, ALG = 2 FOR GENERAL UNCONSTRAINED OPT.
C
      INTEGER ALG, LIV, LV, N
      INTEGER IV(LIV)
      DOUBLE PRECISION D(N), V(LV)
C
      EXTERNAL DIVSET, DR7MDC,DV7CPY,DV7DFL
      DOUBLE PRECISION DR7MDC
C DIVSET  -- SUPPLIES DEFAULT VALUES TO BOTH IV AND V.
C DR7MDC -- RETURNS MACHINE-DEPENDENT CONSTANTS.
C DV7CPY  -- COPIES ONE VECTOR TO ANOTHER.
C DV7DFL  -- SUPPLIES DEFAULT PARAMETER VALUES TO V ALONE.
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER ALG1, I, II, IV1, J, K, L, M, MIV1, MIV2, NDFALT, PARSV1,
     1        PU
      INTEGER IJMP, JLIM(4), MINIV(4), NDFLT(4)
      CHARACTER*1 VARNM(2), SH(2)
      CHARACTER*4 CNGD(3), DFLT(3), VN(2,34), WHICH(3)
      DOUBLE PRECISION BIG, MACHEP, TINY, VK, VM(34), VX(34), ZERO
C
C  ***  IV AND V SUBSCRIPTS  ***
C
      INTEGER ALGSAV, DINIT, DTYPE, DTYPE0, EPSLON, INITS, IVNEED,
     1        LASTIV, LASTV, LMAT, NEXTIV, NEXTV, NVDFLT, OLDN,
     2        PARPRT, PARSAV, PERM, PRUNIT, VNEED
C
C
      PARAMETER (ALGSAV=51, DINIT=38, DTYPE=16, DTYPE0=54, EPSLON=19,
     1           INITS=25, IVNEED=3, LASTIV=44, LASTV=45, LMAT=42,
     2           NEXTIV=46, NEXTV=47, NVDFLT=50, OLDN=38, PARPRT=20,
     3           PARSAV=49, PERM=58, PRUNIT=21, VNEED=4)
      SAVE BIG, MACHEP, TINY
C
      DATA BIG/0.D+0/, MACHEP/-1.D+0/, TINY/1.D+0/, ZERO/0.D+0/
      DATA VN(1,1),VN(2,1)/'EPSL','ON..'/
      DATA VN(1,2),VN(2,2)/'PHMN','FC..'/
      DATA VN(1,3),VN(2,3)/'PHMX','FC..'/
      DATA VN(1,4),VN(2,4)/'DECF','AC..'/
      DATA VN(1,5),VN(2,5)/'INCF','AC..'/
      DATA VN(1,6),VN(2,6)/'RDFC','MN..'/
      DATA VN(1,7),VN(2,7)/'RDFC','MX..'/
      DATA VN(1,8),VN(2,8)/'TUNE','R1..'/
      DATA VN(1,9),VN(2,9)/'TUNE','R2..'/
      DATA VN(1,10),VN(2,10)/'TUNE','R3..'/
      DATA VN(1,11),VN(2,11)/'TUNE','R4..'/
      DATA VN(1,12),VN(2,12)/'TUNE','R5..'/
      DATA VN(1,13),VN(2,13)/'AFCT','OL..'/
      DATA VN(1,14),VN(2,14)/'RFCT','OL..'/
      DATA VN(1,15),VN(2,15)/'XCTO','L...'/
      DATA VN(1,16),VN(2,16)/'XFTO','L...'/
      DATA VN(1,17),VN(2,17)/'LMAX','0...'/
      DATA VN(1,18),VN(2,18)/'LMAX','S...'/
      DATA VN(1,19),VN(2,19)/'SCTO','L...'/
      DATA VN(1,20),VN(2,20)/'DINI','T...'/
      DATA VN(1,21),VN(2,21)/'DTIN','IT..'/
      DATA VN(1,22),VN(2,22)/'D0IN','IT..'/
      DATA VN(1,23),VN(2,23)/'DFAC','....'/
      DATA VN(1,24),VN(2,24)/'DLTF','DC..'/
      DATA VN(1,25),VN(2,25)/'DLTF','DJ..'/
      DATA VN(1,26),VN(2,26)/'DELT','A0..'/
      DATA VN(1,27),VN(2,27)/'FUZZ','....'/
      DATA VN(1,28),VN(2,28)/'RLIM','IT..'/
      DATA VN(1,29),VN(2,29)/'COSM','IN..'/
      DATA VN(1,30),VN(2,30)/'HUBE','RC..'/
      DATA VN(1,31),VN(2,31)/'RSPT','OL..'/
      DATA VN(1,32),VN(2,32)/'SIGM','IN..'/
      DATA VN(1,33),VN(2,33)/'ETA0','....'/
      DATA VN(1,34),VN(2,34)/'BIAS','....'/
C
      DATA VM(1)/1.0D-3/, VM(2)/-0.99D+0/, VM(3)/1.0D-3/, VM(4)/1.0D-2/,
     1     VM(5)/1.2D+0/, VM(6)/1.D-2/, VM(7)/1.2D+0/, VM(8)/0.D+0/,
     2     VM(9)/0.D+0/, VM(10)/1.D-3/, VM(11)/-1.D+0/, VM(13)/0.D+0/,
     3     VM(15)/0.D+0/, VM(16)/0.D+0/, VM(19)/0.D+0/, VM(20)/-10.D+0/,
     4     VM(21)/0.D+0/, VM(22)/0.D+0/, VM(23)/0.D+0/, VM(27)/1.01D+0/,
     5     VM(28)/1.D+10/, VM(30)/0.D+0/, VM(31)/0.D+0/, VM(32)/0.D+0/,
     6     VM(34)/0.D+0/
      DATA VX(1)/0.9D+0/, VX(2)/-1.D-3/, VX(3)/1.D+1/, VX(4)/0.8D+0/,
     1     VX(5)/1.D+2/, VX(6)/0.8D+0/, VX(7)/1.D+2/, VX(8)/0.5D+0/,
     2     VX(9)/0.5D+0/, VX(10)/1.D+0/, VX(11)/1.D+0/, VX(14)/0.1D+0/,
     3     VX(15)/1.D+0/, VX(16)/1.D+0/, VX(19)/1.D+0/, VX(23)/1.D+0/,
     4     VX(24)/1.D+0/, VX(25)/1.D+0/, VX(26)/1.D+0/, VX(27)/1.D+10/,
     5     VX(29)/1.D+0/, VX(31)/1.D+0/, VX(32)/1.D+0/, VX(33)/1.D+0/,
     6     VX(34)/1.D+0/
C
      DATA VARNM(1)/'P'/, VARNM(2)/'P'/, SH(1)/'S'/, SH(2)/'H'/
      DATA CNGD(1),CNGD(2),CNGD(3)/'---C','HANG','ED V'/,
     1     DFLT(1),DFLT(2),DFLT(3)/'NOND','EFAU','LT V'/
      DATA IJMP/33/, JLIM(1)/0/, JLIM(2)/24/, JLIM(3)/0/, JLIM(4)/24/,
     1     NDFLT(1)/32/, NDFLT(2)/25/, NDFLT(3)/32/, NDFLT(4)/25/
      DATA MINIV(1)/82/, MINIV(2)/59/, MINIV(3)/103/, MINIV(4)/103/
C
C...............................  BODY  ................................
C
 10   FORMAT(/' THE FIRST PARAMETER TO DIVSET SHOULD BE',I3,
     1       ' RATHER THAN',I3)
 40   FORMAT(/' /// BAD',A1,' =',I5)
 70   FORMAT(/' /// ',1A1,' CHANGED FROM ',I5,' TO ',I5)
 90   FORMAT(/' ///  IV(1) =',I5,' SHOULD BE BETWEEN 0 AND 14.')
 130  FORMAT(/' ///  ',2A4,'.. V(',I2,') =',g11.3,' should',
     1       ' be between',g11.3,' and',g11.3)
 160  FORMAT(/' IV(NVDFLT) =',I5,' RATHER THAN ',I5)
 180  FORMAT(/' ///  D(',I3,') =',g11.3,' SHOULD BE POSITIVE')
 220  FORMAT(/' NONDEFAULT VALUES....'/' INIT',A1,'..... IV(25) =',I3)
 240  FORMAT(' DTYPE..... IV(16) =',I3)
 260  FORMAT(/' ',3A4,'ALUES....'/)
 270  FORMAT(1X,2A4,'.. V(',I2,') =',g15.7)
 310  FORMAT(/' /// LIV =',I5,' MUST BE AT LEAST',I5)
 330  FORMAT(/' /// LV =',I5,' MUST BE AT LEAST',I5)
 350  FORMAT(/' /// ALG =',I5,' MUST BE 1 2, 3, OR 4')
 370  FORMAT(/' /// LIV =',I5,' MUST BE AT LEAST',I5,
     1       ' TO COMPUTE TRUE MIN. LIV AND MIN. LV')
c
c++(~.C.) Default UNITNO='(PU,'
c++(.C.) Default UNITNO='(*,'
c++ Replace "(PU," = UNITNO
      PU = 0
      IF (PRUNIT .LE. LIV) PU = IV(PRUNIT)
      IF (ALGSAV .GT. LIV) GO TO 20
      IF (ALG .EQ. IV(ALGSAV)) GO TO 20
         IF (PU .NE. 0) WRITE(PU,10) ALG, IV(ALGSAV)
         IV(1) = 67
         GO TO 999
 20   IF (ALG .LT. 1 .OR. ALG .GT. 4) GO TO 340
      MIV1 = MINIV(ALG)
      IF (IV(1) .EQ. 15) GO TO 360
      ALG1 = MOD(ALG-1,2) + 1
      IF (IV(1) .EQ. 0) CALL DIVSET(ALG, IV, LIV, LV, V)
      IV1 = IV(1)
      IF (IV1 .NE. 13 .AND. IV1 .NE. 12) GO TO 30
      IF (PERM .LE. LIV) MIV1 = max(MIV1, IV(PERM) - 1)
      IF (IVNEED .LE. LIV) MIV2 = MIV1 + max(IV(IVNEED), 0)
      IF (LASTIV .LE. LIV) IV(LASTIV) = MIV2
      IF (LIV .LT. MIV1) GO TO 300
      IV(IVNEED) = 0
      IV(LASTV) = max(IV(VNEED), 0) + IV(LMAT) - 1
      IV(VNEED) = 0
      IF (LIV .LT. MIV2) GO TO 300
      IF (LV .LT. IV(LASTV)) GO TO 320
 30   IF (IV1 .LT. 12 .OR. IV1 .GT. 14) GO TO 60
         IF (N .GE. 1) GO TO 50
              IV(1) = 81
              IF (PU .EQ. 0) GO TO 999
              WRITE(PU,40) VARNM(ALG1), N
              GO TO 999
 50      IF (IV1 .NE. 14) IV(NEXTIV) = IV(PERM)
         IF (IV1 .NE. 14) IV(NEXTV) = IV(LMAT)
         IF (IV1 .EQ. 13) GO TO 999
         K = IV(PARSAV) - EPSLON
         CALL DV7DFL(ALG1, LV-K, V(K+1))
         IV(DTYPE0) = 2 - ALG1
         IV(OLDN) = N
         WHICH(1) = DFLT(1)
         WHICH(2) = DFLT(2)
         WHICH(3) = DFLT(3)
         GO TO 110
 60   IF (N .EQ. IV(OLDN)) GO TO 80
         IV(1) = 17
         IF (PU .EQ. 0) GO TO 999
         WRITE(PU,70) VARNM(ALG1), IV(OLDN), N
         GO TO 999
C
 80   IF (IV1 .LE. 11 .AND. IV1 .GE. 1) GO TO 100
         IV(1) = 80
         IF (PU .NE. 0) WRITE(PU,90) IV1
         GO TO 999
C
 100  WHICH(1) = CNGD(1)
      WHICH(2) = CNGD(2)
      WHICH(3) = CNGD(3)
C
 110  IF (IV1 .EQ. 14) IV1 = 12
      IF (BIG .GT. TINY) GO TO 120
         TINY = DR7MDC(1)
         MACHEP = DR7MDC(3)
         BIG = DR7MDC(6)
         VM(12) = MACHEP
         VX(12) = BIG
         VX(13) = BIG
         VM(14) = MACHEP
         VM(17) = TINY
         VX(17) = BIG
         VM(18) = TINY
         VX(18) = BIG
         VX(20) = BIG
         VX(21) = BIG
         VX(22) = BIG
         VM(24) = MACHEP
         VM(25) = MACHEP
         VM(26) = MACHEP
         VX(28) = DR7MDC(5)
         VM(29) = MACHEP
         VX(30) = BIG
         VM(33) = MACHEP
 120  M = 0
      I = 1
      J = JLIM(ALG1)
      K = EPSLON
      NDFALT = NDFLT(ALG1)
      DO 150 L = 1, NDFALT
         VK = V(K)
         IF (VK .GE. VM(I) .AND. VK .LE. VX(I)) GO TO 140
              M = K
              IF (PU .NE. 0) WRITE(PU,130) VN(1,I), VN(2,I), K, VK,
     1                                    VM(I), VX(I)
 140     K = K + 1
         I = I + 1
         IF (I .EQ. J) I = IJMP
 150     CONTINUE
C
      IF (IV(NVDFLT) .EQ. NDFALT) GO TO 170
         IV(1) = 51
         IF (PU .EQ. 0) GO TO 999
         WRITE(PU,160) IV(NVDFLT), NDFALT
         GO TO 999
 170  IF ((IV(DTYPE) .GT. 0 .OR. V(DINIT) .GT. ZERO) .AND. IV1 .EQ. 12)
     1                  GO TO 200
      DO 190 I = 1, N
         IF (D(I) .GT. ZERO) GO TO 190
              M = 18
              IF (PU .NE. 0) WRITE(PU,180) I, D(I)
 190     CONTINUE
 200  IF (M .EQ. 0) GO TO 210
         IV(1) = M
         GO TO 999
C
 210  IF (PU .EQ. 0 .OR. IV(PARPRT) .EQ. 0) GO TO 999
      IF (IV1 .NE. 12 .OR. IV(INITS) .EQ. ALG1-1) GO TO 230
         M = 1
         WRITE(PU,220) SH(ALG1), IV(INITS)
 230  IF (IV(DTYPE) .EQ. IV(DTYPE0)) GO TO 250
         IF (M .EQ. 0) WRITE(PU,260) WHICH
         M = 1
         WRITE(PU,240) IV(DTYPE)
 250  I = 1
      J = JLIM(ALG1)
      K = EPSLON
      L = IV(PARSAV)
      NDFALT = NDFLT(ALG1)
      DO 290 II = 1, NDFALT
         IF (V(K) .EQ. V(L)) GO TO 280
              IF (M .EQ. 0) WRITE(PU,260) WHICH
              M = 1
              WRITE(PU,270) VN(1,I), VN(2,I), K, V(K)
 280     K = K + 1
         L = L + 1
         I = I + 1
         IF (I .EQ. J) I = IJMP
 290     CONTINUE
C
      IV(DTYPE0) = IV(DTYPE)
      PARSV1 = IV(PARSAV)
      CALL DV7CPY(IV(NVDFLT), V(PARSV1), V(EPSLON))
      GO TO 999
C
 300  IV(1) = 15
      IF (PU .EQ. 0) GO TO 999
      WRITE(PU,310) LIV, MIV2
      IF (LIV .LT. MIV1) GO TO 999
      IF (LV .LT. IV(LASTV)) GO TO 320
      GO TO 999
C
 320  IV(1) = 16
      IF (PU .NE. 0) WRITE(PU,330) LV, IV(LASTV)
      GO TO 999
C
 340  IV(1) = 67
      IF (PU .NE. 0) WRITE(PU,350) ALG
      GO TO 999
 360  IF (PU .NE. 0) WRITE(PU,370) LIV, MIV1
      IF (LASTIV .LE. LIV) IV(LASTIV) = MIV1
      IF (LASTV .LE. LIV) IV(LASTV) = 0
C
 999  RETURN
C  ***  LAST LINE OF DPARCK FOLLOWS  ***
      END
      SUBROUTINE DS7LVM(P, Y, SS, X)
C
C  ***  SET  Y = SS * X,  SS = P X P SYMMETRIC MATRIX.  ***
C  ***  LOWER TRIANGLE OF  SS  STORED ROWWISE.         ***
C
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER P
      DOUBLE PRECISION SS(*), X(P), Y(P)
C     DIMENSION SS(P*(P+1)/2)
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER I, IM1, J, K
      DOUBLE PRECISION XI
C
C  ***  EXTERNAL FUNCTION  ***
C
      EXTERNAL DD7TPR
      DOUBLE PRECISION DD7TPR
C
C ----------------------------------------------------------------------
C
      J = 1
      DO 10 I = 1, P
         Y(I) = DD7TPR(I, SS(J), X)
         J = J + I
 10      CONTINUE
C
      IF (P .LE. 1) GO TO 999
      J = 1
      DO 40 I = 2, P
         XI = X(I)
         IM1 = I - 1
         J = J + 1
         DO 30 K = 1, IM1
              Y(K) = Y(K) + SS(J)*XI
              J = J + 1
 30           CONTINUE
 40      CONTINUE
C
 999  RETURN
C  ***  LAST CARD OF DS7LVM FOLLOWS  ***
      END
      SUBROUTINE DS7LUP(A, COSMIN, P, SIZE, STEP, U, W, WCHMTD, WSCALE,
     1                  Y)
C
C  ***  UPDATE SYMMETRIC  A  SO THAT  A * STEP = Y  ***
C  ***  (LOWER TRIANGLE OF  A  STORED ROWWISE       ***
C
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER P
      DOUBLE PRECISION A(*), COSMIN, SIZE, STEP(P), U(P), W(P),
     1                 WCHMTD(P), WSCALE, Y(P)
C     DIMENSION A(P*(P+1)/2)
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER I, J, K
      DOUBLE PRECISION DENMIN, SDOTWM, T, UI, WI
C
C     ***  CONSTANTS  ***
      DOUBLE PRECISION HALF, ONE, ZERO
C
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***
C
      EXTERNAL DD7TPR, DS7LVM, DV2NRM
      DOUBLE PRECISION DD7TPR, DV2NRM
      PARAMETER (HALF=0.5D+0, ONE=1.D+0, ZERO=0.D+0)
C     ------------------------------------------------------------------
C
      SDOTWM = DD7TPR(P, STEP, WCHMTD)
      DENMIN = COSMIN * DV2NRM(P,STEP) * DV2NRM(P,WCHMTD)
      WSCALE = ONE
      IF (DENMIN .NE. ZERO) WSCALE = min(ONE, abs(SDOTWM/DENMIN))
      T = ZERO
      IF (SDOTWM .NE. ZERO) T = WSCALE / SDOTWM
      DO 10 I = 1, P
 10      W(I) = T * WCHMTD(I)
      CALL DS7LVM(P, U, A, STEP)
      T = HALF * (SIZE * DD7TPR(P, STEP, U)  -  DD7TPR(P, STEP, Y))
      DO 20 I = 1, P
 20      U(I) = T*W(I) + Y(I) - SIZE*U(I)
C
C  ***  SET  A = A + U*(W**T) + W*(U**T)  ***
C
      K = 1
      DO 40 I = 1, P
         UI = U(I)
         WI = W(I)
         DO 30 J = 1, I
              A(K) = SIZE*A(K) + UI*W(J) + WI*U(J)
              K = K + 1
 30           CONTINUE
 40      CONTINUE
C
C  ***  LAST CARD OF DS7LUP FOLLOWS  ***
      END
      SUBROUTINE DL7MST(D, G, IERR, IPIVOT, KA, P, QTR, R, STEP, V, W)
C
C  ***  COMPUTE LEVENBERG-MARQUARDT STEP USING MORE-HEBDEN TECHNIQUE  **
C  ***  NL2SOL VERSION 2.2.  ***
C
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER IERR, KA, P
      INTEGER IPIVOT(P)
      DOUBLE PRECISION D(P), G(P), QTR(P), R(*), STEP(P), V(21), W(*)
C     DIMENSION R(*), W(P*(P+5)/2 + 4)
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  ***  PURPOSE  ***
C
C        GIVEN THE R MATRIX FROM THE QR DECOMPOSITION OF A JACOBIAN
C     MATRIX, J, AS WELL AS Q-TRANSPOSE TIMES THE CORRESPONDING
C     RESIDUAL VECTOR, RESID, THIS SUBROUTINE COMPUTES A LEVENBERG-
C     MARQUARDT STEP OF APPROXIMATE LENGTH V(RADIUS) BY THE MORE-
C     TECHNIQUE.
C
C  ***  PARAMETER DESCRIPTION  ***
C
C      D (IN)  = THE SCALE VECTOR.
C      G (IN)  = THE GRADIENT VECTOR (J**T)*R.
C   IERR (I/O) = RETURN CODE FROM QRFACT OR DQ7RGS -- 0 MEANS R HAS
C             FULL RANK.
C IPIVOT (I/O) = PERMUTATION ARRAY FROM QRFACT OR DQ7RGS, WHICH COMPUTE
C             QR DECOMPOSITIONS WITH COLUMN PIVOTING.
C     KA (I/O).  KA .LT. 0 ON INPUT MEANS THIS IS THE FIRST CALL ON
C             DL7MST FOR THE CURRENT R AND QTR.  ON OUTPUT KA CON-
C             TAINS THE NUMBER OF HEBDEN ITERATIONS NEEDED TO DETERMINE
C             STEP.  KA = 0 MEANS A GAUSS-NEWTON STEP.
C      P (IN)  = NUMBER OF PARAMETERS.
C    QTR (IN)  = (Q**T)*RESID = Q-TRANSPOSE TIMES THE RESIDUAL VECTOR.
C      R (IN)  = THE R MATRIX, STORED COMPACTLY BY COLUMNS.
C   STEP (OUT) = THE LEVENBERG-MARQUARDT STEP COMPUTED.
C      V (I/O) CONTAINS VARIOUS CONSTANTS AND VARIABLES DESCRIBED BELOW.
C      W (I/O) = WORKSPACE OF LENGTH P*(P+5)/2 + 4.
C
C  ***  ENTRIES IN V  ***
C
C V(DGNORM) (I/O) = 2-NORM OF (D**-1)*G.
C V(DSTNRM) (I/O) = 2-NORM OF D*STEP.
C V(DST0)   (I/O) = 2-NORM OF GAUSS-NEWTON STEP (FOR NONSING. J).
C V(EPSLON) (IN) = MAX. REL. ERROR ALLOWED IN TWONORM(R)**2 MINUS
C             TWONORM(R - J*STEP)**2.  (SEE ALGORITHM NOTES BELOW.)
C V(GTSTEP) (OUT) = INNER PRODUCT BETWEEN G AND STEP.
C V(NREDUC) (OUT) = HALF THE REDUCTION IN THE SUM OF SQUARES PREDICTED
C             FOR A GAUSS-NEWTON STEP.
C V(PHMNFC) (IN)  = TOL. (TOGETHER WITH V(PHMXFC)) FOR ACCEPTING STEP
C             (MORE*S SIGMA).  THE ERROR V(DSTNRM) - V(RADIUS) MUST LIE
C             BETWEEN V(PHMNFC)*V(RADIUS) AND V(PHMXFC)*V(RADIUS).
C V(PHMXFC) (IN)  (SEE V(PHMNFC).)
C V(PREDUC) (OUT) = HALF THE REDUCTION IN THE SUM OF SQUARES PREDICTED
C             BY THE STEP RETURNED.
C V(RADIUS) (IN)  = RADIUS OF CURRENT (SCALED) TRUST REGION.
C V(RAD0)   (I/O) = VALUE OF V(RADIUS) FROM PREVIOUS CALL.
C V(STPPAR) (I/O) = MARQUARDT PARAMETER (OR ITS NEGATIVE IF THE SPECIAL
C             CASE MENTIONED BELOW IN THE ALGORITHM NOTES OCCURS).
C
C NOTE -- SEE DATA STATEMENT BELOW FOR VALUES OF ABOVE SUBSCRIPTS.
C
C  ***  USAGE NOTES  ***
C
C     IF IT IS DESIRED TO RECOMPUTE STEP USING A DIFFERENT VALUE OF
C     V(RADIUS), THEN THIS ROUTINE MAY BE RESTARTED BY CALLING IT
C     WITH ALL PARAMETERS UNCHANGED EXCEPT V(RADIUS).  (THIS EXPLAINS
C     WHY MANY PARAMETERS ARE LISTED AS I/O).  ON AN INTIIAL CALL (ONE
C     WITH KA = -1), THE CALLER NEED ONLY HAVE INITIALIZED D, G, KA, P,
C     QTR, R, V(EPSLON), V(PHMNFC), V(PHMXFC), V(RADIUS), AND V(RAD0).
C
C  ***  APPLICATION AND USAGE RESTRICTIONS  ***
C
C     THIS ROUTINE IS CALLED AS PART OF THE NL2SOL (NONLINEAR LEAST-
C     SQUARES) PACKAGE (REF. 1).
C
C  ***  ALGORITHM NOTES  ***
C
C     THIS CODE IMPLEMENTS THE STEP COMPUTATION SCHEME DESCRIBED IN
C     REFS. 2 AND 4.  FAST GIVENS TRANSFORMATIONS (SEE REF. 3, PP. 60-
C     62) ARE USED TO COMPUTE STEP WITH A NONZERO MARQUARDT PARAMETER.
C        A SPECIAL CASE OCCURS IF J IS (NEARLY) SINGULAR AND V(RADIUS)
C     IS SUFFICIENTLY LARGE.  IN THIS CASE THE STEP RETURNED IS SUCH
C     THAT  TWONORM(R)**2 - TWONORM(R - J*STEP)**2  DIFFERS FROM ITS
C     OPTIMAL VALUE BY LESS THAN V(EPSLON) TIMES THIS OPTIMAL VALUE,
C     WHERE J AND R DENOTE THE ORIGINAL JACOBIAN AND RESIDUAL.  (SEE
C     REF. 2 FOR MORE DETAILS.)
C
C  ***  FUNCTIONS AND SUBROUTINES CALLED  ***
C
C DD7TPR - RETURNS INNER PRODUCT OF TWO VECTORS.
C DL7ITV - APPLY INVERSE-TRANSPOSE OF COMPACT LOWER TRIANG. MATRIX.
C DL7IVM - APPLY INVERSE OF COMPACT LOWER TRIANG. MATRIX.
C DV7CPY  - COPIES ONE VECTOR TO ANOTHER.
C DV2NRM - RETURNS 2-NORM OF A VECTOR.
C
C  ***  REFERENCES  ***
C
C 1.  DENNIS, J.E., GAY, D.M., AND WELSCH, R.E. (1981), AN ADAPTIVE
C             NONLINEAR LEAST-SQUARES ALGORITHM, ACM TRANS. MATH.
C             SOFTWARE, VOL. 7, NO. 3.
C 2.  GAY, D.M. (1981), COMPUTING OPTIMAL LOCALLY CONSTRAINED STEPS,
C             SIAM J. SCI. STATIST. COMPUTING, VOL. 2, NO. 2, PP.
C             186-197.
C 3.  LAWSON, C.L., AND HANSON, R.J. (1974), SOLVING LEAST SQUARES
C             PROBLEMS, PRENTICE-HALL, ENGLEWOOD CLIFFS, N.J.
C 4.  MORE, J.J. (1978), THE LEVENBERG-MARQUARDT ALGORITHM, IMPLEMEN-
C             TATION AND THEORY, PP.105-116 OF SPRINGER LECTURE NOTES
C             IN MATHEMATICS NO. 630, EDITED BY G.A. WATSON, SPRINGER-
C             VERLAG, BERLIN AND NEW YORK.
C
C  ***  GENERAL  ***
C
C     CODED BY DAVID M. GAY.
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989, AND
C     MCS-7906671.
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER DSTSAV, I, IP1, I1, J1, K, KALIM, L, LK0, PHIPIN,
     1        PP1O2, RES, RES0, RMAT, RMAT0, UK0
      DOUBLE PRECISION A, ADI, ALPHAK, B, DFACSQ, DST, DTOL, D1, D2,
     1                 LK, OLDPHI, PHI, PHIMAX, PHIMIN, PSIFAC, RAD,
     2                 SI, SJ, SQRTAK, T, TWOPSI, UK, WL
C
C     ***  CONSTANTS  ***
      DOUBLE PRECISION DFAC, EIGHT, HALF, NEGONE, ONE, P001, THREE,
     1                 TTOL, ZERO
      DOUBLE PRECISION BIG
C
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***
C
      EXTERNAL DD7TPR, DL7ITV, DL7IVM, DL7SVN, DR7MDC,DV7CPY, DV2NRM
      DOUBLE PRECISION DD7TPR, DL7SVN, DR7MDC, DV2NRM
C
C  ***  SUBSCRIPTS FOR V  ***
C
      INTEGER DGNORM, DSTNRM, DST0, EPSLON, GTSTEP, NREDUC, PHMNFC,
     1        PHMXFC, PREDUC, RADIUS, RAD0, STPPAR
      PARAMETER (DGNORM=1, DSTNRM=2, DST0=3, EPSLON=19, GTSTEP=4,
     1           NREDUC=6, PHMNFC=20, PHMXFC=21, PREDUC=7, RADIUS=8,
     2           RAD0=9, STPPAR=5)
      PARAMETER (DFAC=256.D+0, EIGHT=8.D+0, HALF=0.5D+0, NEGONE=-1.D+0,
     1     ONE=1.D+0, P001=1.D-3, THREE=3.D+0, TTOL=2.5D+0,
     2     ZERO=0.D+0)
      SAVE BIG
      DATA BIG/0.D+0/
C
C  ***  BODY  ***
C
C     ***  FOR USE IN RECOMPUTING STEP, THE FINAL VALUES OF LK AND UK,
C     ***  THE INVERSE DERIVATIVE OF MORE*S PHI AT 0 (FOR NONSING. J)
C     ***  AND THE VALUE RETURNED AS V(DSTNRM) ARE STORED AT W(LK0),
C     ***  W(UK0), W(PHIPIN), AND W(DSTSAV) RESPECTIVELY.
      LK0 = P + 1
      PHIPIN = LK0 + 1
      UK0 = PHIPIN + 1
      DSTSAV = UK0 + 1
      RMAT0 = DSTSAV
C     ***  A COPY OF THE R-MATRIX FROM THE QR DECOMPOSITION OF J IS
C     ***  STORED IN W STARTING AT W(RMAT), AND A COPY OF THE RESIDUAL
C     ***  VECTOR IS STORED IN W STARTING AT W(RES).  THE LOOPS BELOW
C     ***  THAT UPDATE THE QR DECOMP. FOR A NONZERO MARQUARDT PARAMETER
C     ***  WORK ON THESE COPIES.
      RMAT = RMAT0 + 1
      PP1O2 = P * (P + 1) / 2
      RES0 = PP1O2 + RMAT0
      RES = RES0 + 1
      RAD = V(RADIUS)
      IF (RAD .GT. ZERO)
     1   PSIFAC = V(EPSLON)/((EIGHT*(V(PHMNFC) + ONE) + THREE) * RAD**2)
      IF (BIG .LE. ZERO) BIG = DR7MDC(6)
      PHIMAX = V(PHMXFC) * RAD
      PHIMIN = V(PHMNFC) * RAD
C     ***  DTOL, DFAC, AND DFACSQ ARE USED IN RESCALING THE FAST GIVENS
C     ***  REPRESENTATION OF THE UPDATED QR DECOMPOSITION.
      DTOL = ONE/DFAC
      DFACSQ = DFAC*DFAC
C     ***  OLDPHI IS USED TO DETECT LIMITS OF NUMERICAL ACCURACY.  IF
C     ***  WE RECOMPUTE STEP AND IT DOES NOT CHANGE, THEN WE ACCEPT IT.
      OLDPHI = ZERO
      LK = ZERO
      UK = ZERO
      KALIM = KA + 12
C
C  ***  START OR RESTART, DEPENDING ON KA  ***
C
      IF (KA) 10, 20, 370
C
C  ***  FRESH START -- COMPUTE V(NREDUC)  ***
C
 10   KA = 0
      KALIM = 12
      K = P
      IF (IERR .NE. 0) K = abs(IERR) - 1
      V(NREDUC) = HALF*DD7TPR(K, QTR, QTR)
C
C  ***  SET UP TO TRY INITIAL GAUSS-NEWTON STEP  ***
C
 20   V(DST0) = NEGONE
      IF (IERR .NE. 0) GO TO 90
      T = DL7SVN(P, R, STEP, W(RES))
      IF (T .GE. ONE) GO TO 30
         IF (DV2NRM(P, QTR) .GE. BIG*T) GO TO 90
C
C  ***  COMPUTE GAUSS-NEWTON STEP  ***
C
C     ***  NOTE -- THE R-MATRIX IS STORED COMPACTLY BY COLUMNS IN
C     ***  R(1), R(2), R(3), ...  IT IS THE TRANSPOSE OF A
C     ***  LOWER TRIANGULAR MATRIX STORED COMPACTLY BY ROWS, AND WE
C     ***  TREAT IT AS SUCH WHEN USING DL7ITV AND DL7IVM.
 30   CALL DL7ITV(P, W, R, QTR)
C     ***  TEMPORARILY STORE PERMUTED -D*STEP IN STEP.
      DO 60 I = 1, P
         J1 = IPIVOT(I)
         STEP(I) = D(J1)*W(I)
 60      CONTINUE
      DST = DV2NRM(P, STEP)
      V(DST0) = DST
      PHI = DST - RAD
      IF (PHI .LE. PHIMAX) GO TO 410
C     ***  IF THIS IS A RESTART, GO TO 110  ***
      IF (KA .GT. 0) GO TO 110
C
C  ***  GAUSS-NEWTON STEP WAS UNACCEPTABLE.  COMPUTE L0  ***
C
      DO 70 I = 1, P
         J1 = IPIVOT(I)
         STEP(I) = D(J1)*(STEP(I)/DST)
 70      CONTINUE
      CALL DL7IVM(P, STEP, R, STEP)
      T = ONE / DV2NRM(P, STEP)
      W(PHIPIN) = (T/RAD)*T
      LK = PHI*W(PHIPIN)
C
C  ***  COMPUTE U0  ***
C
 90   DO 100 I = 1, P
 100     W(I) = G(I)/D(I)
      V(DGNORM) = DV2NRM(P, W)
      UK = V(DGNORM)/RAD
      IF (UK .LE. ZERO) GO TO 390
C
C     ***  ALPHAK WILL BE USED AS THE CURRENT MARQUARDT PARAMETER.  WE
C     ***  USE MORE*S SCHEME FOR INITIALIZING IT.
C
      ALPHAK = abs(V(STPPAR)) * V(RAD0)/RAD
      ALPHAK = min(UK, max(ALPHAK, LK))
C
C
C  ***  TOP OF LOOP -- INCREMENT KA, COPY R TO RMAT, QTR TO RES  ***
C
 110  KA = KA + 1
      CALL DV7CPY(PP1O2, W(RMAT), R)
      CALL DV7CPY(P, W(RES), QTR)
C
C  ***  SAFEGUARD ALPHAK AND INITIALIZE FAST GIVENS SCALE VECTOR.  ***
C
      IF (ALPHAK .LE. ZERO .OR. ALPHAK .LT. LK .OR. ALPHAK .GE. UK)
     1             ALPHAK = UK * max(P001, sqrt(LK/UK))
      IF (ALPHAK .LE. ZERO) ALPHAK = HALF * UK
      SQRTAK = sqrt(ALPHAK)
      DO 120 I = 1, P
 120     W(I) = ONE
C
C  ***  ADD ALPHAK*D AND UPDATE QR DECOMP. USING FAST GIVENS TRANS.  ***
C
      DO 270 I = 1, P
C        ***  GENERATE, APPLY 1ST GIVENS TRANS. FOR ROW I OF ALPHAK*D.
C        ***  (USE STEP TO STORE TEMPORARY ROW)  ***
         L = I*(I+1)/2 + RMAT0
         WL = W(L)
         D2 = ONE
         D1 = W(I)
         J1 = IPIVOT(I)
         ADI = SQRTAK*D(J1)
         IF (ADI .GE. abs(WL)) GO TO 150
 130     A = ADI/WL
         B = D2*A/D1
         T = A*B + ONE
         IF (T .GT. TTOL) GO TO 150
         W(I) = D1/T
         D2 = D2/T
         W(L) = T*WL
         A = -A
         DO 140 J1 = I, P
              L = L + J1
              STEP(J1) = A*W(L)
 140          CONTINUE
         GO TO 170
C
 150     B = WL/ADI
         A = D1*B/D2
         T = A*B + ONE
         IF (T .GT. TTOL) GO TO 130
         W(I) = D2/T
         D2 = D1/T
         W(L) = T*ADI
         DO 160 J1 = I, P
              L = L + J1
              WL = W(L)
              STEP(J1) = -WL
              W(L) = A*WL
 160          CONTINUE
C
 170     IF (I .EQ. P) GO TO 280
C
C        ***  NOW USE GIVENS TRANS. TO ZERO ELEMENTS OF TEMP. ROW  ***
C
         IP1 = I + 1
         DO 260 I1 = IP1, P
              L = I1*(I1+1)/2 + RMAT0
              WL = W(L)
              SI = STEP(I1-1)
              D1 = W(I1)
C
C             ***  RESCALE ROW I1 IF NECESSARY  ***
C
              IF (D1 .GE. DTOL) GO TO 190
                   D1 = D1*DFACSQ
                   WL = WL/DFAC
                   K = L
                   DO 180 J1 = I1, P
                        K = K + J1
                        W(K) = W(K)/DFAC
 180                    CONTINUE
C
C             ***  USE GIVENS TRANS. TO ZERO NEXT ELEMENT OF TEMP. ROW
C
 190          IF (abs(SI) .GT. abs(WL)) GO TO 220
              IF (SI .EQ. ZERO) GO TO 260
 200          A = SI/WL
              B = D2*A/D1
              T = A*B + ONE
              IF (T .GT. TTOL) GO TO 220
              W(L) = T*WL
              W(I1) = D1/T
              D2 = D2/T
              DO 210 J1 = I1, P
                   L = L + J1
                   WL = W(L)
                   SJ = STEP(J1)
                   W(L) = WL + B*SJ
                   STEP(J1) = SJ - A*WL
 210               CONTINUE
              GO TO 240
C
 220          B = WL/SI
              A = D1*B/D2
              T = A*B + ONE
              IF (T .GT. TTOL) GO TO 200
              W(I1) = D2/T
              D2 = D1/T
              W(L) = T*SI
              DO 230 J1 = I1, P
                   L = L + J1
                   WL = W(L)
                   SJ = STEP(J1)
                   W(L) = A*WL + SJ
                   STEP(J1) = B*SJ - WL
 230               CONTINUE
C
C             ***  RESCALE TEMP. ROW IF NECESSARY  ***
C
 240          IF (D2 .GE. DTOL) GO TO 260
                   D2 = D2*DFACSQ
                   DO 250 K = I1, P
 250                    STEP(K) = STEP(K)/DFAC
 260          CONTINUE
 270     CONTINUE
C
C  ***  COMPUTE STEP  ***
C
 280  CALL DL7ITV(P, W(RES), W(RMAT), W(RES))
C     ***  RECOVER STEP AND STORE PERMUTED -D*STEP AT W(RES)  ***
      DO 290 I = 1, P
         J1 = IPIVOT(I)
         K = RES0 + I
         T = W(K)
         STEP(J1) = -T
         W(K) = T*D(J1)
 290     CONTINUE
      DST = DV2NRM(P, W(RES))
      PHI = DST - RAD
      IF (PHI .LE. PHIMAX .AND. PHI .GE. PHIMIN) GO TO 430
      IF (OLDPHI .EQ. PHI) GO TO 430
      OLDPHI = PHI
C
C  ***  CHECK FOR (AND HANDLE) SPECIAL CASE  ***
C
      IF (PHI .GT. ZERO) GO TO 310
         IF (KA .GE. KALIM) GO TO 430
              TWOPSI = ALPHAK*DST*DST - DD7TPR(P, STEP, G)
              IF (ALPHAK .GE. TWOPSI*PSIFAC) GO TO 310
                   V(STPPAR) = -ALPHAK
                   GO TO 440
C
C  ***  UNACCEPTABLE STEP -- UPDATE LK, UK, ALPHAK, AND TRY AGAIN  ***
C
 300  IF (PHI .LT. ZERO) UK = min(UK, ALPHAK)
      GO TO 320
 310  IF (PHI .LT. ZERO) UK = ALPHAK
 320  DO 330 I = 1, P
         J1 = IPIVOT(I)
         K = RES0 + I
         STEP(I) = D(J1) * (W(K)/DST)
 330     CONTINUE
      CALL DL7IVM(P, STEP, W(RMAT), STEP)
      DO 340 I = 1, P
 340     STEP(I) = STEP(I) / sqrt(W(I))
      T = ONE / DV2NRM(P, STEP)
      ALPHAK = ALPHAK + T*PHI*T/RAD
      LK = max(LK, ALPHAK)
      ALPHAK = LK
      GO TO 110
C
C  ***  RESTART  ***
C
 370  LK = W(LK0)
      UK = W(UK0)
      IF (V(DST0) .GT. ZERO .AND. V(DST0) - RAD .LE. PHIMAX) GO TO 20
      ALPHAK = abs(V(STPPAR))
      DST = W(DSTSAV)
      PHI = DST - RAD
      T = V(DGNORM)/RAD
      IF (RAD .GT. V(RAD0)) GO TO 380
C
C        ***  SMALLER RADIUS  ***
         UK = T
         IF (ALPHAK .LE. ZERO) LK = ZERO
         IF (V(DST0) .GT. ZERO) LK = max(LK, (V(DST0)-RAD)*W(PHIPIN))
         GO TO 300
C
C     ***  BIGGER RADIUS  ***
 380  IF (ALPHAK .LE. ZERO .OR. UK .GT. T) UK = T
      LK = ZERO
      IF (V(DST0) .GT. ZERO) LK = max(LK, (V(DST0)-RAD)*W(PHIPIN))
      GO TO 300
C
C  ***  SPECIAL CASE -- RAD .LE. 0 OR (G = 0 AND J IS SINGULAR)  ***
C
 390  V(STPPAR) = ZERO
      DST = ZERO
      LK = ZERO
      UK = ZERO
      V(GTSTEP) = ZERO
      V(PREDUC) = ZERO
      DO 400 I = 1, P
 400     STEP(I) = ZERO
      GO TO 450
C
C  ***  ACCEPTABLE GAUSS-NEWTON STEP -- RECOVER STEP FROM W  ***
C
 410  ALPHAK = ZERO
      DO 420 I = 1, P
         J1 = IPIVOT(I)
         STEP(J1) = -W(I)
 420     CONTINUE
C
C  ***  SAVE VALUES FOR USE IN A POSSIBLE RESTART  ***
C
 430  V(STPPAR) = ALPHAK
 440  V(GTSTEP) = min(DD7TPR(P,STEP,G), ZERO)
      V(PREDUC) = HALF * (ALPHAK*DST*DST - V(GTSTEP))
 450  V(DSTNRM) = DST
      W(DSTSAV) = DST
      W(LK0) = LK
      W(UK0) = UK
      V(RAD0) = RAD
C
C
C  ***  LAST CARD OF DL7MST FOLLOWS  ***
      END
      SUBROUTINE DG7QTS(D, DIG, DIHDI, KA, L, P, STEP, V, W)
C
C  *** COMPUTE GOLDFELD-QUANDT-TROTTER STEP BY MORE-HEBDEN TECHNIQUE ***
C  ***  (NL2SOL VERSION 2.2), MODIFIED A LA MORE AND SORENSEN  ***
C
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER KA, P
      DOUBLE PRECISION D(P), DIG(P), DIHDI(*), L(*), V(21), STEP(P),
     1                 W(*)
C     DIMENSION DIHDI(P*(P+1)/2), L(P*(P+1)/2), W(4*P+7)
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  ***  PURPOSE  ***
C
C        GIVEN THE (COMPACTLY STORED) LOWER TRIANGLE OF A SCALED
C     HESSIAN (APPROXIMATION) AND A NONZERO SCALED GRADIENT VECTOR,
C     THIS SUBROUTINE COMPUTES A GOLDFELD-QUANDT-TROTTER STEP OF
C     APPROXIMATE LENGTH V(RADIUS) BY THE MORE-HEBDEN TECHNIQUE.  IN
C     OTHER WORDS, STEP IS COMPUTED TO (APPROXIMATELY) MINIMIZE
C     PSI(STEP) = (G**T)*STEP + 0.5*(STEP**T)*H*STEP  SUCH THAT THE
C     2-NORM OF D*STEP IS AT MOST (APPROXIMATELY) V(RADIUS), WHERE
C     G  IS THE GRADIENT,  H  IS THE HESSIAN, AND  D  IS A DIAGONAL
C     SCALE MATRIX WHOSE DIAGONAL IS STORED IN THE PARAMETER D.
C     (DG7QTS ASSUMES  DIG = D**-1 * G  AND  DIHDI = D**-1 * H * D**-1.)
C
C  ***  PARAMETER DESCRIPTION  ***
C
C     D (IN)  = THE SCALE VECTOR, I.E. THE DIAGONAL OF THE SCALE
C              MATRIX  D  MENTIONED ABOVE UNDER PURPOSE.
C   DIG (IN)  = THE SCALED GRADIENT VECTOR, D**-1 * G.  IF G = 0, THEN
C              STEP = 0  AND  V(STPPAR) = 0  ARE RETURNED.
C DIHDI (IN)  = LOWER TRIANGLE OF THE SCALED HESSIAN (APPROXIMATION),
C              I.E., D**-1 * H * D**-1, STORED COMPACTLY BY ROWS., I.E.,
C              IN THE ORDER (1,1), (2,1), (2,2), (3,1), (3,2), ETC.
C    KA (I/O) = THE NUMBER OF HEBDEN ITERATIONS (SO FAR) TAKEN TO DETER-
C              MINE STEP.  KA .LT. 0 ON INPUT MEANS THIS IS THE FIRST
C              ATTEMPT TO DETERMINE STEP (FOR THE PRESENT DIG AND DIHDI)
C              -- KA IS INITIALIZED TO 0 IN THIS CASE.  OUTPUT WITH
C              KA = 0  (OR V(STPPAR) = 0)  MEANS  STEP = -(H**-1)*G.
C     L (I/O) = WORKSPACE OF LENGTH P*(P+1)/2 FOR CHOLESKY FACTORS.
C     P (IN)  = NUMBER OF PARAMETERS -- THE HESSIAN IS A  P X P  MATRIX.
C  STEP (I/O) = THE STEP COMPUTED.
C     V (I/O) CONTAINS VARIOUS CONSTANTS AND VARIABLES DESCRIBED BELOW.
C     W (I/O) = WORKSPACE OF LENGTH 4*P + 6.
C
C  ***  ENTRIES IN V  ***
C
C V(DGNORM) (I/O) = 2-NORM OF (D**-1)*G.
C V(DSTNRM) (OUTPUT) = 2-NORM OF D*STEP.
C V(DST0)   (I/O) = 2-NORM OF D*(H**-1)*G (FOR POS. DEF. H ONLY), OR
C             OVERESTIMATE OF SMALLEST EIGENVALUE OF (D**-1)*H*(D**-1).
C V(EPSLON) (IN)  = MAX. REL. ERROR ALLOWED FOR PSI(STEP).  FOR THE
C             STEP RETURNED, PSI(STEP) WILL EXCEED ITS OPTIMAL VALUE
C             BY LESS THAN -V(EPSLON)*PSI(STEP).  SUGGESTED VALUE = 0.1.
C V(GTSTEP) (OUT) = INNER PRODUCT BETWEEN G AND STEP.
C V(NREDUC) (OUT) = PSI(-(H**-1)*G) = PSI(NEWTON STEP)  (FOR POS. DEF.
C             H ONLY -- V(NREDUC) IS SET TO ZERO OTHERWISE).
C V(PHMNFC) (IN)  = TOL. (TOGETHER WITH V(PHMXFC)) FOR ACCEPTING STEP
C             (MORE*S SIGMA).  THE ERROR V(DSTNRM) - V(RADIUS) MUST LIE
C             BETWEEN V(PHMNFC)*V(RADIUS) AND V(PHMXFC)*V(RADIUS).
C V(PHMXFC) (IN)  (SEE V(PHMNFC).)
C             SUGGESTED VALUES -- V(PHMNFC) = -0.25, V(PHMXFC) = 0.5.
C V(PREDUC) (OUT) = PSI(STEP) = PREDICTED OBJ. FUNC. REDUCTION FOR STEP.
C V(RADIUS) (IN)  = RADIUS OF CURRENT (SCALED) TRUST REGION.
C V(RAD0)   (I/O) = VALUE OF V(RADIUS) FROM PREVIOUS CALL.
C V(STPPAR) (I/O) IS NORMALLY THE MARQUARDT PARAMETER, I.E. THE ALPHA
C             DESCRIBED BELOW UNDER ALGORITHM NOTES.  IF H + ALPHA*D**2
C             (SEE ALGORITHM NOTES) IS (NEARLY) SINGULAR, HOWEVER,
C             THEN V(STPPAR) = -ALPHA.
C
C  ***  USAGE NOTES  ***
C
C     IF IT IS DESIRED TO RECOMPUTE STEP USING A DIFFERENT VALUE OF
C     V(RADIUS), THEN THIS ROUTINE MAY BE RESTARTED BY CALLING IT
C     WITH ALL PARAMETERS UNCHANGED EXCEPT V(RADIUS).  (THIS EXPLAINS
C     WHY STEP AND W ARE LISTED AS I/O).  ON AN INITIAL CALL (ONE WITH
C     KA .LT. 0), STEP AND W NEED NOT BE INITIALIZED AND ONLY COMPO-
C     NENTS V(EPSLON), V(STPPAR), V(PHMNFC), V(PHMXFC), V(RADIUS), AND
C     V(RAD0) OF V MUST BE INITIALIZED.
C
C  ***  ALGORITHM NOTES  ***
C
C        THE DESIRED G-Q-T STEP (REF. 2, 3, 4, 6) SATISFIES
C     (H + ALPHA*D**2)*STEP = -G  FOR SOME NONNEGATIVE ALPHA SUCH THAT
C     H + ALPHA*D**2 IS POSITIVE SEMIDEFINITE.  ALPHA AND STEP ARE
C     COMPUTED BY A SCHEME ANALOGOUS TO THE ONE DESCRIBED IN REF. 5.
C     ESTIMATES OF THE SMALLEST AND LARGEST EIGENVALUES OF THE HESSIAN
C     ARE OBTAINED FROM THE GERSCHGORIN CIRCLE THEOREM ENHANCED BY A
C     SIMPLE FORM OF THE SCALING DESCRIBED IN REF. 7.  CASES IN WHICH
C     H + ALPHA*D**2 IS NEARLY (OR EXACTLY) SINGULAR ARE HANDLED BY
C     THE TECHNIQUE DISCUSSED IN REF. 2.  IN THESE CASES, A STEP OF
C     (EXACT) LENGTH V(RADIUS) IS RETURNED FOR WHICH PSI(STEP) EXCEEDS
C     ITS OPTIMAL VALUE BY LESS THAN -V(EPSLON)*PSI(STEP).  THE TEST
C     SUGGESTED IN REF. 6 FOR DETECTING THE SPECIAL CASE IS PERFORMED
C     ONCE TWO MATRIX FACTORIZATIONS HAVE BEEN DONE -- DOING SO SOONER
C     SEEMS TO DEGRADE THE PERFORMANCE OF OPTIMIZATION ROUTINES THAT
C     CALL THIS ROUTINE.
C
C  ***  FUNCTIONS AND SUBROUTINES CALLED  ***
C
C DD7TPR - RETURNS INNER PRODUCT OF TWO VECTORS.
C DL7ITV - APPLIES INVERSE-TRANSPOSE OF COMPACT LOWER TRIANG. MATRIX.
C DL7IVM - APPLIES INVERSE OF COMPACT LOWER TRIANG. MATRIX.
C DL7SRT  - FINDS CHOLESKY FACTOR (OF COMPACTLY STORED LOWER TRIANG.).
C DL7SVN - RETURNS APPROX. TO MIN. SING. VALUE OF LOWER TRIANG. MATRIX.
C DR7MDC - RETURNS MACHINE-DEPENDENT CONSTANTS.
C DV2NRM - RETURNS 2-NORM OF A VECTOR.
C
C  ***  REFERENCES  ***
C
C 1.  DENNIS, J.E., GAY, D.M., AND WELSCH, R.E. (1981), AN ADAPTIVE
C             NONLINEAR LEAST-SQUARES ALGORITHM, ACM TRANS. MATH.
C             SOFTWARE, VOL. 7, NO. 3.
C 2.  GAY, D.M. (1981), COMPUTING OPTIMAL LOCALLY CONSTRAINED STEPS,
C             SIAM J. SCI. STATIST. COMPUTING, VOL. 2, NO. 2, PP.
C             186-197.
C 3.  GOLDFELD, S.M., QUANDT, R.E., AND TROTTER, H.F. (1966),
C             MAXIMIZATION BY QUADRATIC HILL-CLIMBING, ECONOMETRICA 34,
C             PP. 541-551.
C 4.  HEBDEN, M.D. (1973), AN ALGORITHM FOR MINIMIZATION USING EXACT
C             SECOND DERIVATIVES, REPORT T.P. 515, THEORETICAL PHYSICS
C             DIV., A.E.R.E. HARWELL, OXON., ENGLAND.
C 5.  MORE, J.J. (1978), THE LEVENBERG-MARQUARDT ALGORITHM, IMPLEMEN-
C             TATION AND THEORY, PP.105-116 OF SPRINGER LECTURE NOTES
C             IN MATHEMATICS NO. 630, EDITED BY G.A. WATSON, SPRINGER-
C             VERLAG, BERLIN AND NEW YORK.
C 6.  MORE, J.J., AND SORENSEN, D.C. (1981), COMPUTING A TRUST REGION
C             STEP, TECHNICAL REPORT ANL-81-83, ARGONNE NATIONAL LAB.
C 7.  VARGA, R.S. (1965), MINIMAL GERSCHGORIN SETS, PACIFIC J. MATH. 15,
C             PP. 719-729.
C
C  ***  GENERAL  ***
C
C     CODED BY DAVID M. GAY.
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989, AND
C     MCS-7906671.
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  ***  LOCAL VARIABLES  ***
C
      LOGICAL RESTRT
      INTEGER DGGDMX, DIAG, DIAG0, DSTSAV, EMAX, EMIN, I, IM1, INC, IRC,
     1        J, K, KALIM, KAMIN, K1, LK0, PHIPIN, Q, Q0, UK0, X
      DOUBLE PRECISION ALPHAK, AKI, AKK, DELTA, DST, EPS, GTSTA, LK,
     1                 OLDPHI, PHI, PHIMAX, PHIMIN, PSIFAC, RAD, RADSQ,
     2                 ROOT, SI, SK, SW, T, TWOPSI, T1, T2, UK, WI
C
C     ***  CONSTANTS  ***
      DOUBLE PRECISION BIG, DGXFAC, EPSFAC, FOUR, HALF, KAPPA, NEGONE,
     1                 ONE, P001, SIX, THREE, TWO, ZERO
C
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***
C
      EXTERNAL DD7TPR, DL7ITV, DL7IVM,DL7SRT, DL7SVN, DR7MDC, DV2NRM
      DOUBLE PRECISION DD7TPR, DL7SVN, DR7MDC, DV2NRM
C
C  ***  SUBSCRIPTS FOR V  ***
C
      INTEGER DGNORM, DSTNRM, DST0, EPSLON, GTSTEP, STPPAR, NREDUC,
     1        PHMNFC, PHMXFC, PREDUC, RADIUS, RAD0
      PARAMETER (DGNORM=1, DSTNRM=2, DST0=3, EPSLON=19, GTSTEP=4,
     1           NREDUC=6, PHMNFC=20, PHMXFC=21, PREDUC=7, RADIUS=8,
     2           RAD0=9, STPPAR=5)
      PARAMETER (EPSFAC=50.0D+0, FOUR=4.0D+0, HALF=0.5D+0,
     1     KAPPA=2.0D+0, NEGONE=-1.0D+0, ONE=1.0D+0, P001=1.0D-3,
     2     SIX=6.0D+0, THREE=3.0D+0, TWO=2.0D+0, ZERO=0.0D+0)
      SAVE DGXFAC
      DATA BIG/0.D+0/, DGXFAC/0.D+0/
C
C  ***  BODY  ***
C
      IF (BIG .LE. ZERO) BIG = DR7MDC(6)
C
C     ***  STORE LARGEST ABS. ENTRY IN (D**-1)*H*(D**-1) AT W(DGGDMX).
      DGGDMX = P + 1
C     ***  STORE GERSCHGORIN OVER- AND UNDERESTIMATES OF THE LARGEST
C     ***  AND SMALLEST EIGENVALUES OF (D**-1)*H*(D**-1) AT W(EMAX)
C     ***  AND W(EMIN) RESPECTIVELY.
      EMAX = DGGDMX + 1
      EMIN = EMAX + 1
C     ***  FOR USE IN RECOMPUTING STEP, THE FINAL VALUES OF LK, UK, DST,
C     ***  AND THE INVERSE DERIVATIVE OF MORE*S PHI AT 0 (FOR POS. DEF.
C     ***  H) ARE STORED IN W(LK0), W(UK0), W(DSTSAV), AND W(PHIPIN)
C     ***  RESPECTIVELY.
      LK0 = EMIN + 1
      PHIPIN = LK0 + 1
      UK0 = PHIPIN + 1
      DSTSAV = UK0 + 1
C     ***  STORE DIAG OF (D**-1)*H*(D**-1) IN W(DIAG),...,W(DIAG0+P).
      DIAG0 = DSTSAV
      DIAG = DIAG0 + 1
C     ***  STORE -D*STEP IN W(Q),...,W(Q0+P).
      Q0 = DIAG0 + P
      Q = Q0 + 1
C     ***  ALLOCATE STORAGE FOR SCRATCH VECTOR X  ***
      X = Q + P
      RAD = V(RADIUS)
      RADSQ = RAD**2
C     ***  PHITOL = MAX. ERROR ALLOWED IN DST = V(DSTNRM) = 2-NORM OF
C     ***  D*STEP.
      PHIMAX = V(PHMXFC) * RAD
      PHIMIN = V(PHMNFC) * RAD
      PSIFAC = BIG
      T1 = TWO * V(EPSLON) / (THREE * (FOUR * (V(PHMNFC) + ONE) *
     1                       (KAPPA + ONE)  +  KAPPA  +  TWO) * RAD)
      IF (T1 .LT. BIG*min(RAD,ONE)) PSIFAC = T1 / RAD
C     ***  OLDPHI IS USED TO DETECT LIMITS OF NUMERICAL ACCURACY.  IF
C     ***  WE RECOMPUTE STEP AND IT DOES NOT CHANGE, THEN WE ACCEPT IT.
      OLDPHI = ZERO
      EPS = V(EPSLON)
      IRC = 0
      RESTRT = .FALSE.
      KALIM = KA + 50
C
C  ***  START OR RESTART, DEPENDING ON KA  ***
C
      IF (KA .GE. 0) GO TO 290
C
C  ***  FRESH START  ***
C
      K = 0
      UK = NEGONE
      KA = 0
      KALIM = 50
      V(DGNORM) = DV2NRM(P, DIG)
      V(NREDUC) = ZERO
      V(DST0) = ZERO
      KAMIN = 3
      IF (V(DGNORM) .EQ. ZERO) KAMIN = 0
C
C     ***  STORE DIAG(DIHDI) IN W(DIAG0+1),...,W(DIAG0+P)  ***
C
      J = 0
      DO 10 I = 1, P
         J = J + I
         K1 = DIAG0 + I
         W(K1) = DIHDI(J)
 10      CONTINUE
C
C     ***  DETERMINE W(DGGDMX), THE LARGEST ELEMENT OF DIHDI  ***
C
      T1 = ZERO
      J = P * (P + 1) / 2
      DO 20 I = 1, J
         T = abs(DIHDI(I))
         IF (T1 .LT. T) T1 = T
 20      CONTINUE
      W(DGGDMX) = T1
C
C  ***  TRY ALPHA = 0  ***
C
 30   CALL DL7SRT(1, P, L, DIHDI, IRC)
      IF (IRC .EQ. 0) GO TO 50
C        ***  INDEF. H -- UNDERESTIMATE SMALLEST EIGENVALUE, USE THIS
C        ***  ESTIMATE TO INITIALIZE LOWER BOUND LK ON ALPHA.
         J = IRC*(IRC+1)/2
         T = L(J)
         L(J) = ONE
         DO 40 I = 1, IRC
 40           W(I) = ZERO
         W(IRC) = ONE
         CALL DL7ITV(IRC, W, L, W)
         T1 = DV2NRM(IRC, W)
         LK = -T / T1 / T1
         V(DST0) = -LK
         IF (RESTRT) GO TO 210
         GO TO 70
C
C     ***  POSITIVE DEFINITE H -- COMPUTE UNMODIFIED NEWTON STEP.  ***
 50   LK = ZERO
      T = DL7SVN(P, L, W(Q), W(Q))
      IF (T .GE. ONE) GO TO 60
         IF (V(DGNORM) .GE. T*T*BIG) GO TO 70
 60   CALL DL7IVM(P, W(Q), L, DIG)
      GTSTA = DD7TPR(P, W(Q), W(Q))
      V(NREDUC) = HALF * GTSTA
      CALL DL7ITV(P, W(Q), L, W(Q))
      DST = DV2NRM(P, W(Q))
      V(DST0) = DST
      PHI = DST - RAD
      IF (PHI .LE. PHIMAX) GO TO 260
      IF (RESTRT) GO TO 210
C
C  ***  PREPARE TO COMPUTE GERSCHGORIN ESTIMATES OF LARGEST (AND
C  ***  SMALLEST) EIGENVALUES.  ***
C
 70   K = 0
      DO 100 I = 1, P
         WI = ZERO
         IF (I .EQ. 1) GO TO 90
         IM1 = I - 1
         DO 80 J = 1, IM1
              K = K + 1
              T = abs(DIHDI(K))
              WI = WI + T
              W(J) = W(J) + T
 80           CONTINUE
 90      W(I) = WI
         K = K + 1
 100     CONTINUE
C
C  ***  (UNDER-)ESTIMATE SMALLEST EIGENVALUE OF (D**-1)*H*(D**-1)  ***
C
      K = 1
      T1 = W(DIAG) - W(1)
      IF (P .LE. 1) GO TO 120
      DO 110 I = 2, P
         J = DIAG0 + I
         T = W(J) - W(I)
         IF (T .GE. T1) GO TO 110
              T1 = T
              K = I
 110     CONTINUE
C
 120  SK = W(K)
      J = DIAG0 + K
      AKK = W(J)
      K1 = K*(K-1)/2 + 1
      INC = 1
      T = ZERO
      DO 150 I = 1, P
         IF (I .EQ. K) GO TO 130
         AKI = abs(DIHDI(K1))
         SI = W(I)
         J = DIAG0 + I
         T1 = HALF * (AKK - W(J) + SI - AKI)
         T1 = T1 + sqrt(T1*T1 + SK*AKI)
         IF (T .LT. T1) T = T1
         IF (I .LT. K) GO TO 140
 130     INC = I
 140     K1 = K1 + INC
 150     CONTINUE
C
      W(EMIN) = AKK - T
      UK = V(DGNORM)/RAD - W(EMIN)
      IF (V(DGNORM) .EQ. ZERO) UK = UK + P001 + P001*UK
      IF (UK .LE. ZERO) UK = P001
C
C  ***  COMPUTE GERSCHGORIN (OVER-)ESTIMATE OF LARGEST EIGENVALUE  ***
C
      K = 1
      T1 = W(DIAG) + W(1)
      IF (P .LE. 1) GO TO 170
      DO 160 I = 2, P
         J = DIAG0 + I
         T = W(J) + W(I)
         IF (T .LE. T1) GO TO 160
              T1 = T
              K = I
 160     CONTINUE
C
 170  SK = W(K)
      J = DIAG0 + K
      AKK = W(J)
      K1 = K*(K-1)/2 + 1
      INC = 1
      T = ZERO
      DO 200 I = 1, P
         IF (I .EQ. K) GO TO 180
         AKI = abs(DIHDI(K1))
         SI = W(I)
         J = DIAG0 + I
         T1 = HALF * (W(J) + SI - AKI - AKK)
         T1 = T1 + sqrt(T1*T1 + SK*AKI)
         IF (T .LT. T1) T = T1
         IF (I .LT. K) GO TO 190
 180     INC = I
 190     K1 = K1 + INC
 200     CONTINUE
C
      W(EMAX) = AKK + T
      LK = max(LK, V(DGNORM)/RAD - W(EMAX))
C
C     ***  ALPHAK = CURRENT VALUE OF ALPHA (SEE ALG. NOTES ABOVE).  WE
C     ***  USE MORE*S SCHEME FOR INITIALIZING IT.
      ALPHAK = abs(V(STPPAR)) * V(RAD0)/RAD
      ALPHAK = min(UK, max(ALPHAK, LK))
C
      IF (IRC .NE. 0) GO TO 210
C
C  ***  COMPUTE L0 FOR POSITIVE DEFINITE H  ***
C
      CALL DL7IVM(P, W, L, W(Q))
      T = DV2NRM(P, W)
      W(PHIPIN) = RAD / T / T
      LK = max(LK, PHI*W(PHIPIN))
C
C  ***  SAFEGUARD ALPHAK AND ADD ALPHAK*I TO (D**-1)*H*(D**-1)  ***
C
 210  KA = KA + 1
      IF (-V(DST0) .GE. ALPHAK .OR. ALPHAK .LT. LK .OR. ALPHAK .GE. UK)
     1                      ALPHAK = UK * max(P001, sqrt(LK/UK))
      IF (ALPHAK .LE. ZERO) ALPHAK = HALF * UK
      IF (ALPHAK .LE. ZERO) ALPHAK = UK
      K = 0
      DO 220 I = 1, P
         K = K + I
         J = DIAG0 + I
         DIHDI(K) = W(J) + ALPHAK
 220     CONTINUE
C
C  ***  TRY COMPUTING CHOLESKY DECOMPOSITION  ***
C
      CALL DL7SRT(1, P, L, DIHDI, IRC)
      IF (IRC .EQ. 0) GO TO 240
C
C  ***  (D**-1)*H*(D**-1) + ALPHAK*I  IS INDEFINITE -- OVERESTIMATE
C  ***  SMALLEST EIGENVALUE FOR USE IN UPDATING LK  ***
C
      J = (IRC*(IRC+1))/2
      T = L(J)
      L(J) = ONE
      DO 230 I = 1, IRC
 230     W(I) = ZERO
      W(IRC) = ONE
      CALL DL7ITV(IRC, W, L, W)
      T1 = DV2NRM(IRC, W)
      LK = ALPHAK - T/T1/T1
      V(DST0) = -LK
      IF (ALPHAK .LT. LK) GO TO 210
C
C  ***  NASTY CASE -- EXACT GERSCHGORIN BOUNDS.  FUDGE LK, UK...
C
      T = P001 * ALPHAK
      IF (T .LE. ZERO) T = P001
      LK = ALPHAK + T
      IF (UK .LE. LK) UK = LK + T
      GO TO 210
C
C  ***  ALPHAK MAKES (D**-1)*H*(D**-1) POSITIVE DEFINITE.
C  ***  COMPUTE Q = -D*STEP, CHECK FOR CONVERGENCE.  ***
C
 240  CALL DL7IVM(P, W(Q), L, DIG)
      GTSTA = DD7TPR(P, W(Q), W(Q))
      CALL DL7ITV(P, W(Q), L, W(Q))
      DST = DV2NRM(P, W(Q))
      PHI = DST - RAD
      IF (PHI .LE. PHIMAX .AND. PHI .GE. PHIMIN) GO TO 270
      IF (PHI .EQ. OLDPHI) GO TO 270
      OLDPHI = PHI
      IF (PHI .LT. ZERO) GO TO 330
C
C  ***  UNACCEPTABLE ALPHAK -- UPDATE LK, UK, ALPHAK  ***
C
 250  IF (KA .GE. KALIM) GO TO 270
C     ***  THE FOLLOWING min IS NECESSARY BECAUSE OF RESTARTS  ***
      IF (PHI .LT. ZERO) UK = min(UK, ALPHAK)
C     *** KAMIN = 0 ONLY IFF THE GRADIENT VANISHES  ***
      IF (KAMIN .EQ. 0) GO TO 210
      CALL DL7IVM(P, W, L, W(Q))
C     *** THE FOLLOWING, COMMENTED CALCULATION OF ALPHAK IS SOMETIMES
C     *** SAFER BUT WORSE IN PERFORMANCE...
C     T1 = DST / DV2NRM(P, W)
C     ALPHAK = ALPHAK  +  T1 * (PHI/RAD) * T1
      T1 = DV2NRM(P, W)
      ALPHAK = ALPHAK  +  (PHI/T1) * (DST/T1) * (DST/RAD)
      LK = max(LK, ALPHAK)
      ALPHAK = LK
      GO TO 210
C
C  ***  ACCEPTABLE STEP ON FIRST TRY  ***
C
 260  ALPHAK = ZERO
C
C  ***  SUCCESSFUL STEP IN GENERAL.  COMPUTE STEP = -(D**-1)*Q  ***
C
 270  DO 280 I = 1, P
         J = Q0 + I
         STEP(I) = -W(J)/D(I)
 280     CONTINUE
      V(GTSTEP) = -GTSTA
      V(PREDUC) = HALF * (abs(ALPHAK)*DST*DST + GTSTA)
      GO TO 410
C
C
C  ***  RESTART WITH NEW RADIUS  ***
C
 290  IF (V(DST0) .LE. ZERO .OR. V(DST0) - RAD .GT. PHIMAX) GO TO 310
C
C     ***  PREPARE TO RETURN NEWTON STEP  ***
C
         RESTRT = .TRUE.
         KA = KA + 1
         K = 0
         DO 300 I = 1, P
              K = K + I
              J = DIAG0 + I
              DIHDI(K) = W(J)
 300          CONTINUE
         UK = NEGONE
         GO TO 30
C
 310  KAMIN = KA + 3
      IF (V(DGNORM) .EQ. ZERO) KAMIN = 0
      IF (KA .EQ. 0) GO TO 50
C
      DST = W(DSTSAV)
      ALPHAK = abs(V(STPPAR))
      PHI = DST - RAD
      T = V(DGNORM)/RAD
      UK = T - W(EMIN)
      IF (V(DGNORM) .EQ. ZERO) UK = UK + P001 + P001*UK
      IF (UK .LE. ZERO) UK = P001
      IF (RAD .GT. V(RAD0)) GO TO 320
C
C        ***  SMALLER RADIUS  ***
         LK = ZERO
         IF (ALPHAK .GT. ZERO) LK = W(LK0)
         LK = max(LK, T - W(EMAX))
         IF (V(DST0) .GT. ZERO) LK = max(LK, (V(DST0)-RAD)*W(PHIPIN))
         GO TO 250
C
C     ***  BIGGER RADIUS  ***
 320  IF (ALPHAK .GT. ZERO) UK = min(UK, W(UK0))
      LK = max(max(ZERO, -V(DST0)), T - W(EMAX))
      IF (V(DST0) .GT. ZERO) LK = max(LK, (V(DST0)-RAD)*W(PHIPIN))
      GO TO 250
C
C  ***  DECIDE WHETHER TO CHECK FOR SPECIAL CASE... IN PRACTICE (FROM
C  ***  THE STANDPOINT OF THE CALLING OPTIMIZATION CODE) IT SEEMS BEST
C  ***  NOT TO CHECK UNTIL A FEW ITERATIONS HAVE FAILED -- HENCE THE
C  ***  TEST ON KAMIN BELOW.
C
 330  DELTA = ALPHAK + min(ZERO, V(DST0))
      TWOPSI = ALPHAK*DST*DST + GTSTA
      IF (KA .GE. KAMIN) GO TO 340
C     *** IF THE TEST IN REF. 2 IS SATISFIED, FALL THROUGH TO HANDLE
C     *** THE SPECIAL CASE (AS SOON AS THE MORE-SORENSEN TEST DETECTS
C     *** IT).
      IF (PSIFAC .GE. BIG) GO TO 340
      IF (DELTA .GE. PSIFAC*TWOPSI) GO TO 370
C
C  ***  CHECK FOR THE SPECIAL CASE OF  H + ALPHA*D**2  (NEARLY)
C  ***  SINGULAR.  USE ONE STEP OF INVERSE POWER METHOD WITH START
C  ***  FROM DL7SVN TO OBTAIN APPROXIMATE EIGENVECTOR CORRESPONDING
C  ***  TO SMALLEST EIGENVALUE OF (D**-1)*H*(D**-1).  DL7SVN RETURNS
C  ***  X AND W WITH  L*W = X.
C
 340  T = DL7SVN(P, L, W(X), W)
C
C     ***  NORMALIZE W  ***
      DO 350 I = 1, P
 350     W(I) = T*W(I)
C     ***  COMPLETE CURRENT INV. POWER ITER. -- REPLACE W BY (L**-T)*W.
      CALL DL7ITV(P, W, L, W)
      T2 = ONE/DV2NRM(P, W)
      DO 360 I = 1, P
 360     W(I) = T2*W(I)
      T = T2 * T
C
C  ***  NOW W IS THE DESIRED APPROXIMATE (UNIT) EIGENVECTOR AND
C  ***  T*X = ((D**-1)*H*(D**-1) + ALPHAK*I)*W.
C
      SW = DD7TPR(P, W(Q), W)
      T1 = (RAD + DST) * (RAD - DST)
      ROOT = sqrt(SW*SW + T1)
      IF (SW .LT. ZERO) ROOT = -ROOT
      SI = T1 / (SW + ROOT)
C
C  ***  THE ACTUAL TEST FOR THE SPECIAL CASE...
C
      IF ((T2*SI)**2 .LE. EPS*(DST**2 + ALPHAK*RADSQ)) GO TO 380
C
C  ***  UPDATE UPPER BOUND ON SMALLEST EIGENVALUE (WHEN NOT POSITIVE)
C  ***  (AS RECOMMENDED BY MORE AND SORENSEN) AND CONTINUE...
C
      IF (V(DST0) .LE. ZERO) V(DST0) = min(V(DST0), T2**2 - ALPHAK)
      LK = max(LK, -V(DST0))
C
C  ***  CHECK WHETHER WE CAN HOPE TO DETECT THE SPECIAL CASE IN
C  ***  THE AVAILABLE ARITHMETIC.  ACCEPT STEP AS IT IS IF NOT.
C
C     ***  IF NOT YET AVAILABLE, OBTAIN MACHINE DEPENDENT VALUE DGXFAC.
 370  IF (DGXFAC .EQ. ZERO) DGXFAC = EPSFAC * DR7MDC(3)
C
      IF (DELTA .GT. DGXFAC*W(DGGDMX)) GO TO 250
         GO TO 270
C
C  ***  SPECIAL CASE DETECTED... NEGATE ALPHAK TO INDICATE SPECIAL CASE
C
 380  ALPHAK = -ALPHAK
      V(PREDUC) = HALF * TWOPSI
C
C  ***  ACCEPT CURRENT STEP IF ADDING SI*W WOULD LEAD TO A
C  ***  FURTHER RELATIVE REDUCTION IN PSI OF LESS THAN V(EPSLON)/3.
C
      T1 = ZERO
      T = SI*(ALPHAK*SW - HALF*SI*(ALPHAK + T*DD7TPR(P,W(X),W)))
      IF (T .LT. EPS*TWOPSI/SIX) GO TO 390
         V(PREDUC) = V(PREDUC) + T
         DST = RAD
         T1 = -SI
 390  DO 400 I = 1, P
         J = Q0 + I
         W(J) = T1*W(I) - W(J)
         STEP(I) = W(J) / D(I)
 400     CONTINUE
      V(GTSTEP) = DD7TPR(P, DIG, W(Q))
C
C  ***  SAVE VALUES FOR USE IN A POSSIBLE RESTART  ***
C
 410  V(DSTNRM) = DST
      V(STPPAR) = ALPHAK
      W(LK0) = LK
      W(UK0) = UK
      V(RAD0) = RAD
      W(DSTSAV) = DST
C
C     ***  RESTORE DIAGONAL OF DIHDI  ***
C
      J = 0
      DO 420 I = 1, P
         J = J + I
         K = DIAG0 + I
         DIHDI(J) = W(K)
 420     CONTINUE
C
C
C  ***  LAST CARD OF DG7QTS FOLLOWS  ***
      END
      SUBROUTINE DL7IVM(N, X, L, Y)
C
C  ***  SOLVE  L*X = Y, WHERE  L  IS AN  N X N  LOWER TRIANGULAR
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME
C  ***  STORAGE.  ***
C
      INTEGER N
      DOUBLE PRECISION X(N), L(*), Y(N)
      EXTERNAL DD7TPR
      DOUBLE PRECISION DD7TPR
      INTEGER I, J, K
      DOUBLE PRECISION T, ZERO
      PARAMETER (ZERO=0.D+0)
C
      DO 10 K = 1, N
         IF (Y(K) .NE. ZERO) GO TO 20
         X(K) = ZERO
 10      CONTINUE
      GO TO 999
 20   J = K*(K+1)/2
      X(K) = Y(K) / L(J)
      IF (K .GE. N) GO TO 999
      K = K + 1
      DO 30 I = K, N
         T = DD7TPR(I-1, L(J+1), X)
         J = J + I
         X(I) = (Y(I) - T)/L(J)
 30      CONTINUE
 999  RETURN
C  ***  LAST CARD OF DL7IVM FOLLOWS  ***
      END
      DOUBLE PRECISION FUNCTION DL7SVX(P, L, X, Y)
C
C  ***  ESTIMATE LARGEST SING. VALUE OF PACKED LOWER TRIANG. MATRIX L
C
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER P
      DOUBLE PRECISION L(*), X(P), Y(P)
C     DIMENSION L(P*(P+1)/2)
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  ***  PURPOSE  ***
C
C     THIS FUNCTION RETURNS A GOOD UNDER-ESTIMATE OF THE LARGEST
C     SINGULAR VALUE OF THE PACKED LOWER TRIANGULAR MATRIX L.
C
C  ***  PARAMETER DESCRIPTION  ***
C
C  P (IN)  = THE ORDER OF L.  L IS A  P X P  LOWER TRIANGULAR MATRIX.
C  L (IN)  = ARRAY HOLDING THE ELEMENTS OF  L  IN ROW ORDER, I.E.
C             L(1,1), L(2,1), L(2,2), L(3,1), L(3,2), L(3,3), ETC.
C  X (OUT) IF DL7SVX RETURNS A POSITIVE VALUE, THEN X = (L**T)*Y IS AN
C             (UNNORMALIZED) APPROXIMATE RIGHT SINGULAR VECTOR
C             CORRESPONDING TO THE LARGEST SINGULAR VALUE.  THIS
C             APPROXIMATION MAY BE CRUDE.
C  Y (OUT) IF DL7SVX RETURNS A POSITIVE VALUE, THEN Y = L*X IS A
C             NORMALIZED APPROXIMATE LEFT SINGULAR VECTOR CORRESPOND-
C             ING TO THE LARGEST SINGULAR VALUE.  THIS APPROXIMATION
C             MAY BE VERY CRUDE.  THE CALLER MAY PASS THE SAME VECTOR
C             FOR X AND Y (NONSTANDARD FORTRAN USAGE), IN WHICH CASE X
C             OVER-WRITES Y.
C
C  ***  ALGORITHM NOTES  ***
C
C     THE ALGORITHM IS BASED ON ANALOGY WITH (1).  IT USES A
C     RANDOM NUMBER GENERATOR PROPOSED IN (4), WHICH PASSES THE
C     SPECTRAL TEST WITH FLYING COLORS -- SEE (2) AND (3).
C
C  ***  SUBROUTINES AND FUNCTIONS CALLED  ***
C
C        DV2NRM - FUNCTION, RETURNS THE 2-NORM OF A VECTOR.
C
C  ***  REFERENCES  ***
C
C     (1) CLINE, A., MOLER, C., STEWART, G., AND WILKINSON, J.H.(1977),
C         AN ESTIMATE FOR THE CONDITION NUMBER OF A MATRIX, REPORT
C         TM-310, APPLIED MATH. DIV., ARGONNE NATIONAL LABORATORY.
C
C     (2) HOAGLIN, D.C. (1976), THEORETICAL PROPERTIES OF CONGRUENTIAL
C         RANDOM-NUMBER GENERATORS --  AN EMPIRICAL VIEW,
C         MEMORANDUM NS-340, DEPT. OF STATISTICS, HARVARD UNIV.
C
C     (3) KNUTH, D.E. (1969), THE ART OF COMPUTER PROGRAMMING, VOL. 2
C         (SEMINUMERICAL ALGORITHMS), ADDISON-WESLEY, READING, MASS.
C
C     (4) SMITH, C.S. (1971), MULTIPLICATIVE PSEUDO-RANDOM NUMBER
C         GENERATORS WITH PRIME MODULUS, J. ASSOC. COMPUT. MACH. 18,
C         PP. 586-593.
C
C  ***  HISTORY  ***
C
C     DESIGNED AND CODED BY DAVID M. GAY (WINTER 1977/SUMMER 1978).
C
C  ***  GENERAL  ***
C
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324, DCR75-10143, 76-14311DSS, AND MCS76-11989.
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER I, IX, J, JI, JJ, JJJ, JM1, J0, PM1, PPLUS1
      DOUBLE PRECISION B, BLJI, SMINUS, SPLUS, T, YI
C
C  ***  CONSTANTS  ***
C
      DOUBLE PRECISION HALF, ONE, R9973, ZERO
C
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***
C
      EXTERNAL DD7TPR, DV2NRM,DV2AXY
      DOUBLE PRECISION DD7TPR, DV2NRM
      PARAMETER (HALF=0.5D+0, ONE=1.D+0, R9973=9973.D+0, ZERO=0.D+0)
C
C  ***  BODY  ***
C
      IX = 2
      PPLUS1 = P + 1
      PM1 = P - 1
C
C  ***  FIRST INITIALIZE X TO PARTIAL SUMS  ***
C
      J0 = P*PM1/2
      JJ = J0 + P
      IX = MOD(3432*IX, 9973)
      B = HALF*(ONE + dble(IX)/R9973)
      X(P) = B * L(JJ)
      IF (P .LE. 1) GO TO 40
      DO 10 I = 1, PM1
         JI = J0 + I
         X(I) = B * L(JI)
 10      CONTINUE
C
C  ***  COMPUTE X = (L**T)*B, WHERE THE COMPONENTS OF B HAVE RANDOMLY
C  ***  CHOSEN MAGNITUDES IN (.5,1) WITH SIGNS CHOSEN TO MAKE X LARGE.
C
C     DO J = P-1 TO 1 BY -1...
      DO 30 JJJ = 1, PM1
         J = P - JJJ
C       ***  DETERMINE X(J) IN THIS ITERATION. NOTE FOR I = 1,2,...,J
C       ***  THAT X(I) HOLDS THE CURRENT PARTIAL SUM FOR ROW I.
         IX = MOD(3432*IX, 9973)
         B = HALF*(ONE + dble(IX)/R9973)
         JM1 = J - 1
         J0 = J*JM1/2
         SPLUS = ZERO
         SMINUS = ZERO
         DO 20 I = 1, J
              JI = J0 + I
              BLJI = B * L(JI)
              SPLUS = SPLUS + abs(BLJI + X(I))
              SMINUS = SMINUS + abs(BLJI - X(I))
 20           CONTINUE
         IF (SMINUS .GT. SPLUS) B = -B
         X(J) = ZERO
C        ***  UPDATE PARTIAL SUMS  ***
         CALL DV2AXY(J, X, B, L(J0+1), X)
 30      CONTINUE
C
C  ***  NORMALIZE X  ***
C
 40   T = DV2NRM(P, X)
      IF (T .LE. ZERO) GO TO 80
      T = ONE / T
      DO 50 I = 1, P
 50      X(I) = T*X(I)
C
C  ***  COMPUTE L*X = Y AND RETURN SVMAX = TWONORM(Y)  ***
C
      DO 60 JJJ = 1, P
         J = PPLUS1 - JJJ
         JI = J*(J-1)/2 + 1
         Y(J) = DD7TPR(J, L(JI), X)
 60      CONTINUE
C
C  ***  NORMALIZE Y AND SET X = (L**T)*Y  ***
C
      T = ONE / DV2NRM(P, Y)
      JI = 1
      DO 70 I = 1, P
         YI = T * Y(I)
         X(I) = ZERO
         CALL DV2AXY(I, X, YI, L(JI), X)
         JI = JI + I
 70      CONTINUE
      DL7SVX = DV2NRM(P, X)
      GO TO 999
C
 80   DL7SVX = ZERO
C
 999  RETURN
C  ***  LAST CARD OF DL7SVX FOLLOWS  ***
      END
c     ==================================================================
      SUBROUTINE DA7SST(IV, LIV, LV, V)
C
C  ***  ASSESS CANDIDATE STEP (***SOL VERSION 2.3)  ***
C
c     ------------------------------------------------------------------
      INTEGER LIV, LV
      INTEGER IV(LIV)
      DOUBLE PRECISION V(LV)
C
C  ***  PURPOSE  ***
C
C        THIS SUBROUTINE IS CALLED BY AN UNCONSTRAINED MINIMIZATION
C     ROUTINE TO ASSESS THE NEXT CANDIDATE STEP.  IT MAY RECOMMEND ONE
C     OF SEVERAL COURSES OF ACTION, SUCH AS ACCEPTING THE STEP, RECOM-
C     PUTING IT USING THE SAME OR A NEW QUADRATIC MODEL, OR HALTING DUE
C     TO CONVERGENCE OR FALSE CONVERGENCE.  SEE THE RETURN CODE LISTING
C     BELOW.
C
C -------------------------  PARAMETER USAGE  --------------------------
C
C  IV (I/O) INTEGER PARAMETER AND SCRATCH VECTOR -- SEE DESCRIPTION
C             BELOW OF IV VALUES REFERENCED.
C LIV (IN)  LENGTH OF IV ARRAY.
C  LV (IN)  LENGTH OF V ARRAY.
C   V (I/O) REAL PARAMETER AND SCRATCH VECTOR -- SEE DESCRIPTION
C             BELOW OF V VALUES REFERENCED.
C
C  ***  IV VALUES REFERENCED  ***
C
C    IV(IRC) (I/O) ON INPUT FOR THE FIRST STEP TRIED IN A NEW ITERATION,
C             IV(IRC) SHOULD BE SET TO 3 OR 4 (THE VALUE TO WHICH IT IS
C             SET WHEN STEP IS DEFINITELY TO BE ACCEPTED).  ON INPUT
C             AFTER STEP HAS BEEN RECOMPUTED, IV(IRC) SHOULD BE
C             UNCHANGED SINCE THE PREVIOUS RETURN OF DA7SST.
C                ON OUTPUT, IV(IRC) IS A RETURN CODE HAVING ONE OF THE
C             FOLLOWING VALUES...
C                  1 = SWITCH MODELS OR TRY SMALLER STEP.
C                  2 = SWITCH MODELS OR ACCEPT STEP.
C                  3 = ACCEPT STEP AND DETERMINE V(RADFAC) BY GRADIENT
C                       TESTS.
C                  4 = ACCEPT STEP, V(RADFAC) HAS BEEN DETERMINED.
C                  5 = RECOMPUTE STEP (USING THE SAME MODEL).
C                  6 = RECOMPUTE STEP WITH RADIUS = V(LMAXS) BUT DO NOT
C                       EVAULATE THE OBJECTIVE FUNCTION.
C                  7 = X-CONVERGENCE (SEE V(XCTOL)).
C                  8 = RELATIVE FUNCTION CONVERGENCE (SEE V(RFCTOL)).
C                  9 = BOTH X- AND RELATIVE FUNCTION CONVERGENCE.
C                 10 = ABSOLUTE FUNCTION CONVERGENCE (SEE V(AFCTOL)).
C                 11 = SINGULAR CONVERGENCE (SEE V(LMAXS)).
C                 12 = FALSE CONVERGENCE (SEE V(XFTOL)).
C                 13 = IV(IRC) WAS OUT OF RANGE ON INPUT.
C             RETURN CODE I HAS PRECDENCE OVER I+1 FOR I = 9, 10, 11.
C IV(MLSTGD) (I/O) SAVED VALUE OF IV(MODEL).
C  IV(MODEL) (I/O) ON INPUT, IV(MODEL) SHOULD BE AN INTEGER IDENTIFYING
C             THE CURRENT QUADRATIC MODEL OF THE OBJECTIVE FUNCTION.
C             IF A PREVIOUS STEP YIELDED A BETTER FUNCTION REDUCTION,
C             THEN IV(MODEL) WILL BE SET TO IV(MLSTGD) ON OUTPUT.
C IV(NFCALL) (IN)  INVOCATION COUNT FOR THE OBJECTIVE FUNCTION.
C IV(NFGCAL) (I/O) VALUE OF IV(NFCALL) AT STEP THAT GAVE THE BIGGEST
C             FUNCTION REDUCTION THIS ITERATION.  IV(NFGCAL) REMAINS
C             UNCHANGED UNTIL A FUNCTION REDUCTION IS OBTAINED.
C IV(RADINC) (I/O) THE NUMBER OF RADIUS INCREASES (OR MINUS THE NUMBER
C             OF DECREASES) SO FAR THIS ITERATION.
C IV(RESTOR) (OUT) SET TO 1 IF V(F) HAS BEEN RESTORED AND X SHOULD BE
C             RESTORED TO ITS INITIAL VALUE, TO 2 IF X SHOULD BE SAVED,
C             TO 3 IF X SHOULD BE RESTORED FROM THE SAVED VALUE, AND TO
C             0 OTHERWISE.
C  IV(STAGE) (I/O) COUNT OF THE NUMBER OF MODELS TRIED SO FAR IN THE
C             CURRENT ITERATION.
C IV(STGLIM) (IN)  MAXIMUM NUMBER OF MODELS TO CONSIDER.
C IV(SWITCH) (OUT) SET TO 0 UNLESS A NEW MODEL IS BEING TRIED AND IT
C             GIVES A SMALLER FUNCTION VALUE THAN THE PREVIOUS MODEL,
C             IN WHICH CASE DA7SST SETS IV(SWITCH) = 1.
C IV(TOOBIG) (IN)  IS NONZERO IF STEP WAS TOO BIG (E.G. IF IT CAUSED
C             OVERFLOW).
C   IV(XIRC) (I/O) VALUE THAT IV(IRC) WOULD HAVE IN THE ABSENCE OF
C             CONVERGENCE, FALSE CONVERGENCE, AND OVERSIZED STEPS.
C
C  ***  V VALUES REFERENCED  ***
C
C V(AFCTOL) (IN)  ABSOLUTE FUNCTION CONVERGENCE TOLERANCE.  IF THE
C             ABSOLUTE VALUE OF THE CURRENT FUNCTION VALUE V(F) IS LESS
C             THAN V(AFCTOL), THEN DA7SST RETURNS WITH IV(IRC) = 10.
C V(DECFAC) (IN)  FACTOR BY WHICH TO DECREASE RADIUS WHEN IV(TOOBIG) IS
C             NONZERO.
C V(DSTNRM) (IN)  THE 2-NORM OF D*STEP.
C V(DSTSAV) (I/O) VALUE OF V(DSTNRM) ON SAVED STEP.
C   V(DST0) (IN)  THE 2-NORM OF D TIMES THE NEWTON STEP (WHEN DEFINED,
C             I.E., FOR V(NREDUC) .GE. 0).
C      V(F) (I/O) ON BOTH INPUT AND OUTPUT, V(F) IS THE OBJECTIVE FUNC-
C             TION VALUE AT X.  IF X IS RESTORED TO A PREVIOUS VALUE,
C             THEN V(F) IS RESTORED TO THE CORRESPONDING VALUE.
C   V(FDIF) (OUT) THE FUNCTION REDUCTION V(F0) - V(F) (FOR THE OUTPUT
C             VALUE OF V(F) IF AN EARLIER STEP GAVE A BIGGER FUNCTION
C             DECREASE, AND FOR THE INPUT VALUE OF V(F) OTHERWISE).
C V(FLSTGD) (I/O) SAVED VALUE OF V(F).
C     V(F0) (IN)  OBJECTIVE FUNCTION VALUE AT START OF ITERATION.
C V(GTSLST) (I/O) VALUE OF V(GTSTEP) ON SAVED STEP.
C V(GTSTEP) (IN)  INNER PRODUCT BETWEEN STEP AND GRADIENT.
C V(INCFAC) (IN)  MINIMUM FACTOR BY WHICH TO INCREASE RADIUS.
C  V(LMAXS) (IN)  MAXIMUM REASONABLE STEP SIZE (AND INITIAL STEP BOUND).
C             IF THE ACTUAL FUNCTION DECREASE IS NO MORE THAN TWICE
C             WHAT WAS PREDICTED, IF A RETURN WITH IV(IRC) = 7, 8, 9,
C             OR 10 DOES NOT OCCUR, IF V(DSTNRM) .GT. V(LMAXS), AND IF
C             V(PREDUC) .LE. V(SCTOL) * ABS(V(F0)), THEN DA7SST RE-
C             TURNS WITH IV(IRC) = 11.  IF SO DOING APPEARS WORTHWHILE,
C             THEN DA7SST REPEATS THIS TEST WITH V(PREDUC) COMPUTED FOR
C             A STEP OF LENGTH V(LMAXS) (BY A RETURN WITH IV(IRC) = 6).
C V(NREDUC) (I/O)  FUNCTION REDUCTION PREDICTED BY QUADRATIC MODEL FOR
C             NEWTON STEP.  IF DA7SST IS CALLED WITH IV(IRC) = 6, I.E.,
C             IF V(PREDUC) HAS BEEN COMPUTED WITH RADIUS = V(LMAXS) FOR
C             USE IN THE SINGULAR CONVERVENCE TEST, THEN V(NREDUC) IS
C             SET TO -V(PREDUC) BEFORE THE LATTER IS RESTORED.
C V(PLSTGD) (I/O) VALUE OF V(PREDUC) ON SAVED STEP.
C V(PREDUC) (I/O) FUNCTION REDUCTION PREDICTED BY QUADRATIC MODEL FOR
C             CURRENT STEP.
C V(RADFAC) (OUT) FACTOR TO BE USED IN DETERMINING THE NEW RADIUS,
C             WHICH SHOULD BE V(RADFAC)*DST, WHERE  DST  IS EITHER THE
C             OUTPUT VALUE OF V(DSTNRM) OR THE 2-NORM OF
C             DIAG(NEWD)*STEP  FOR THE OUTPUT VALUE OF STEP AND THE
C             UPDATED VERSION, NEWD, OF THE SCALE VECTOR D.  FOR
C             IV(IRC) = 3, V(RADFAC) = 1.0 IS RETURNED.
C V(RDFCMN) (IN)  MINIMUM VALUE FOR V(RADFAC) IN TERMS OF THE INPUT
C             VALUE OF V(DSTNRM) -- SUGGESTED VALUE = 0.1.
C V(RDFCMX) (IN)  MAXIMUM VALUE FOR V(RADFAC) -- SUGGESTED VALUE = 4.0.
C  V(RELDX) (IN) SCALED RELATIVE CHANGE IN X CAUSED BY STEP, COMPUTED
C             (E.G.) BY FUNCTION  DRLDST  AS
C                 MAX (D(I)*ABS(X(I)-X0(I)), 1 .LE. I .LE. P) /
C                    MAX (D(I)*(ABS(X(I))+ABS(X0(I))), 1 .LE. I .LE. P).
C V(RFCTOL) (IN)  RELATIVE FUNCTION CONVERGENCE TOLERANCE.  IF THE
C             ACTUAL FUNCTION REDUCTION IS AT MOST TWICE WHAT WAS PRE-
C             DICTED AND  V(NREDUC) .LE. V(RFCTOL)*ABS(V(F0)),  THEN
C            DA7SST RETURNS WITH IV(IRC) = 8 OR 9.
C V(STPPAR) (IN)  MARQUARDT PARAMETER -- 0 MEANS FULL NEWTON STEP.
C V(TUNER1) (IN)  TUNING CONSTANT USED TO DECIDE IF THE FUNCTION
C             REDUCTION WAS MUCH LESS THAN EXPECTED.  SUGGESTED
C             VALUE = 0.1.
C V(TUNER2) (IN)  TUNING CONSTANT USED TO DECIDE IF THE FUNCTION
C             REDUCTION WAS LARGE ENOUGH TO ACCEPT STEP.  SUGGESTED
C             VALUE = 10**-4.
C V(TUNER3) (IN)  TUNING CONSTANT USED TO DECIDE IF THE RADIUS
C             SHOULD BE INCREASED.  SUGGESTED VALUE = 0.75.
C  V(XCTOL) (IN)  X-CONVERGENCE CRITERION.  IF STEP IS A NEWTON STEP
C             (V(STPPAR) = 0) HAVING V(RELDX) .LE. V(XCTOL) AND GIVING
C             AT MOST TWICE THE PREDICTED FUNCTION DECREASE, THEN
C            DA7SST RETURNS IV(IRC) = 7 OR 9.
C  V(XFTOL) (IN)  FALSE CONVERGENCE TOLERANCE.  IF STEP GAVE NO OR ONLY
C             A SMALL FUNCTION DECREASE AND V(RELDX) .LE. V(XFTOL),
C             THEN DA7SST RETURNS WITH IV(IRC) = 12.
C
C ------------------------------  NOTES  -------------------------------
C
C  ***  APPLICATION AND USAGE RESTRICTIONS  ***
C
C        THIS ROUTINE IS CALLED AS PART OF THE NL2SOL (NONLINEAR
C     LEAST-SQUARES) PACKAGE.  IT MAY BE USED IN ANY UNCONSTRAINED
C     MINIMIZATION SOLVER THAT USES DOGLEG, GOLDFELD-QUANDT-TROTTER,
C     OR LEVENBERG-MARQUARDT STEPS.
C
C  ***  ALGORITHM NOTES  ***
C
C        SEE (1) FOR FURTHER DISCUSSION OF THE ASSESSING AND MODEL
C     SWITCHING STRATEGIES.  WHILE NL2SOL CONSIDERS ONLY TWO MODELS,
C    DA7SST IS DESIGNED TO HANDLE ANY NUMBER OF MODELS.
C
C  ***  USAGE NOTES  ***
C
C        ON THE FIRST CALL OF AN ITERATION, ONLY THE I/O VARIABLES
C     STEP, X, IV(IRC), IV(MODEL), V(F), V(DSTNRM), V(GTSTEP), AND
C     V(PREDUC) NEED HAVE BEEN INITIALIZED.  BETWEEN CALLS, NO I/O
C     VALUES EXECPT STEP, X, IV(MODEL), V(F) AND THE STOPPING TOLER-
C     ANCES SHOULD BE CHANGED.
C        AFTER A RETURN FOR CONVERGENCE OR FALSE CONVERGENCE, ONE CAN
C     CHANGE THE STOPPING TOLERANCES AND CALL DA7SST AGAIN, IN WHICH
C     CASE THE STOPPING TESTS WILL BE REPEATED.
C
C  ***  REFERENCES  ***
C
C     (1) DENNIS, J.E., JR., GAY, D.M., AND WELSCH, R.E. (1981),
C        AN ADAPTIVE NONLINEAR LEAST-SQUARES ALGORITHM,
C        ACM TRANS. MATH. SOFTWARE, VOL. 7, NO. 3.
C
C     (2) POWELL, M.J.D. (1970)  A FORTRAN SUBROUTINE FOR SOLVING
C        SYSTEMS OF NONLINEAR ALGEBRAIC EQUATIONS, IN NUMERICAL
C        METHODS FOR NONLINEAR ALGEBRAIC EQUATIONS, EDITED BY
C        P. RABINOWITZ, GORDON AND BREACH, LONDON.
C
C  ***  HISTORY  ***
C
C        JOHN DENNIS DESIGNED MUCH OF THIS ROUTINE, STARTING WITH
C     IDEAS IN (2). ROY WELSCH SUGGESTED THE MODEL SWITCHING STRATEGY.
C        DAVID GAY AND STEPHEN PETERS CAST THIS SUBROUTINE INTO A MORE
C     PORTABLE FORM (WINTER 1977), AND DAVID GAY CAST IT INTO ITS
C     PRESENT FORM (FALL 1978).
C
C  ***  GENERAL  ***
C
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989, AND
C     MCS-7906671.
C
C -----------------------  EXTERNAL QUANTITIES  ------------------------
C
C  ***  NO EXTERNAL FUNCTIONS AND SUBROUTINES  ***
C
C -------------------------  LOCAL VARIABLES  --------------------------
C
      LOGICAL GOODX
      INTEGER I, NFC
      DOUBLE PRECISION EMAX, EMAXS, GTS, RFAC1, XMAX
      DOUBLE PRECISION HALF, ONE, ONEP2, TWO, ZERO
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
      INTEGER AFCTOL, DECFAC, DSTNRM, DSTSAV, DST0, F, FDIF, FLSTGD, F0,
     1        GTSLST, GTSTEP, INCFAC, IRC, LMAXS, MLSTGD, MODEL, NFCALL,
     2        NFGCAL, NREDUC, PLSTGD, PREDUC, RADFAC, RADINC, RDFCMN,
     3        RDFCMX, RELDX, RESTOR, RFCTOL, SCTOL, STAGE, STGLIM,
     4        STPPAR, SWITCH, TOOBIG, TUNER1, TUNER2, TUNER3, XCTOL,
     5        XFTOL, XIRC
C
C  ***  DATA INITIALIZATIONS  ***
C
      PARAMETER (HALF=0.5D+0, ONE=1.D+0, ONEP2=1.2D+0, TWO=2.D+0,
     1           ZERO=0.D+0)
      PARAMETER (IRC=29, MLSTGD=32, MODEL=5, NFCALL=6, NFGCAL=7,
     1           RADINC=8, RESTOR=9, STAGE=10, STGLIM=11, SWITCH=12,
     2           TOOBIG=2, XIRC=13)
      PARAMETER (AFCTOL=31, DECFAC=22, DSTNRM=2, DST0=3, DSTSAV=18,
     1           F=10, FDIF=11, FLSTGD=12, F0=13, GTSLST=14, GTSTEP=4,
     2           INCFAC=23, LMAXS=36, NREDUC=6, PLSTGD=15, PREDUC=7,
     3           RADFAC=16, RDFCMN=24, RDFCMX=25, RELDX=17, RFCTOL=32,
     4           SCTOL=37, STPPAR=5, TUNER1=26, TUNER2=27, TUNER3=28,
     5           XCTOL=33, XFTOL=34)
C
C ++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      NFC = IV(NFCALL)
      IV(SWITCH) = 0
      IV(RESTOR) = 0
      RFAC1 = ONE
      GOODX = .TRUE.
      I = IV(IRC)
      IF (I .GE. 1 .AND. I .LE. 12)
     1             GO TO (20,30,10,10,40,280,220,220,220,220,220,170), I
         IV(IRC) = 13
         GO TO 999
C
C  ***  INITIALIZE FOR NEW ITERATION  ***
C
 10   IV(STAGE) = 1
      IV(RADINC) = 0
      V(FLSTGD) = V(F0)
      IF (IV(TOOBIG) .EQ. 0) GO TO 110
         IV(STAGE) = -1
         IV(XIRC) = I
         GO TO 60
C
C  ***  STEP WAS RECOMPUTED WITH NEW MODEL OR SMALLER RADIUS  ***
C  ***  FIRST DECIDE WHICH  ***
C
 20   IF (IV(MODEL) .NE. IV(MLSTGD)) GO TO 30
C        ***  OLD MODEL RETAINED, SMALLER RADIUS TRIED  ***
C        ***  DO NOT CONSIDER ANY MORE NEW MODELS THIS ITERATION  ***
         IV(STAGE) = IV(STGLIM)
         IV(RADINC) = -1
         GO TO 110
C
C  ***  A NEW MODEL IS BEING TRIED.  DECIDE WHETHER TO KEEP IT.  ***
C
 30   IV(STAGE) = IV(STAGE) + 1
C
C     ***  NOW WE ADD THE POSSIBILTIY THAT STEP WAS RECOMPUTED WITH  ***
C     ***  THE SAME MODEL, PERHAPS BECAUSE OF AN OVERSIZED STEP.     ***
C
 40   IF (IV(STAGE) .GT. 0) GO TO 50
C
C        ***  STEP WAS RECOMPUTED BECAUSE IT WAS TOO BIG.  ***
C
         IF (IV(TOOBIG) .NE. 0) GO TO 60
C
C        ***  RESTORE IV(STAGE) AND PICK UP WHERE WE LEFT OFF.  ***
C
         IV(STAGE) = -IV(STAGE)
         I = IV(XIRC)
         GO TO (20, 30, 110, 110, 70), I
C
 50   IF (IV(TOOBIG) .EQ. 0) GO TO 70
C
C  ***  HANDLE OVERSIZE STEP  ***
C
      IF (IV(RADINC) .GT. 0) GO TO 80
         IV(STAGE) = -IV(STAGE)
         IV(XIRC) = IV(IRC)
C
 60      V(RADFAC) = V(DECFAC)
         IV(RADINC) = IV(RADINC) - 1
         IV(IRC) = 5
         IV(RESTOR) = 1
         GO TO 999
C
 70   IF (V(F) .LT. V(FLSTGD)) GO TO 110
C
C     *** THE NEW STEP IS A LOSER.  RESTORE OLD MODEL.  ***
C
      IF (IV(MODEL) .EQ. IV(MLSTGD)) GO TO 80
         IV(MODEL) = IV(MLSTGD)
         IV(SWITCH) = 1
C
C     ***  RESTORE STEP, ETC. ONLY IF A PREVIOUS STEP DECREASED V(F).
C
 80   IF (V(FLSTGD) .GE. V(F0)) GO TO 110
         IV(RESTOR) = 1
         V(F) = V(FLSTGD)
         V(PREDUC) = V(PLSTGD)
         V(GTSTEP) = V(GTSLST)
         IF (IV(SWITCH) .EQ. 0) RFAC1 = V(DSTNRM) / V(DSTSAV)
         V(DSTNRM) = V(DSTSAV)
         NFC = IV(NFGCAL)
         GOODX = .FALSE.
C
 110  V(FDIF) = V(F0) - V(F)
      IF (V(FDIF) .GT. V(TUNER2) * V(PREDUC)) GO TO 140
      IF (IV(RADINC) .GT. 0) GO TO 140
C
C        ***  NO (OR ONLY A TRIVIAL) FUNCTION DECREASE
C        ***  -- SO TRY NEW MODEL OR SMALLER RADIUS
C
         IF (V(F) .LT. V(F0)) GO TO 120
              IV(MLSTGD) = IV(MODEL)
              V(FLSTGD) = V(F)
              V(F) = V(F0)
              IV(RESTOR) = 1
              GO TO 130
 120     IV(NFGCAL) = NFC
 130     IV(IRC) = 1
         IF (IV(STAGE) .LT. IV(STGLIM)) GO TO 160
              IV(IRC) = 5
              IV(RADINC) = IV(RADINC) - 1
              GO TO 160
C
C  ***  NONTRIVIAL FUNCTION DECREASE ACHIEVED  ***
C
 140  IV(NFGCAL) = NFC
      RFAC1 = ONE
      V(DSTSAV) = V(DSTNRM)
      IF (V(FDIF) .GT. V(PREDUC)*V(TUNER1)) GO TO 190
C
C  ***  DECREASE WAS MUCH LESS THAN PREDICTED -- EITHER CHANGE MODELS
C  ***  OR ACCEPT STEP WITH DECREASED RADIUS.
C
      IF (IV(STAGE) .GE. IV(STGLIM)) GO TO 150
C        ***  CONSIDER SWITCHING MODELS  ***
         IV(IRC) = 2
         GO TO 160
C
C     ***  ACCEPT STEP WITH DECREASED RADIUS  ***
C
 150  IV(IRC) = 4
C
C  ***  SET V(RADFAC) TO FLETCHER*S DECREASE FACTOR  ***
C
 160  IV(XIRC) = IV(IRC)
      EMAX = V(GTSTEP) + V(FDIF)
      V(RADFAC) = HALF * RFAC1
      IF (EMAX .LT. V(GTSTEP)) V(RADFAC) = RFAC1 * max(V(RDFCMN),
     1                                           HALF * V(GTSTEP)/EMAX)
C
C  ***  DO FALSE CONVERGENCE TEST  ***
C
 170  IF (V(RELDX) .LE. V(XFTOL)) GO TO 180
         IV(IRC) = IV(XIRC)
         IF (V(F) .LT. V(F0)) GO TO 200
              GO TO 230
C
 180  IV(IRC) = 12
      GO TO 240
C
C  ***  HANDLE GOOD FUNCTION DECREASE  ***
C
 190  IF (V(FDIF) .LT. (-V(TUNER3) * V(GTSTEP))) GO TO 210
C
C     ***  INCREASING RADIUS LOOKS WORTHWHILE.  SEE IF WE JUST
C     ***  RECOMPUTED STEP WITH A DECREASED RADIUS OR RESTORED STEP
C     ***  AFTER RECOMPUTING IT WITH A LARGER RADIUS.
C
      IF (IV(RADINC) .LT. 0) GO TO 210
      IF (IV(RESTOR) .EQ. 1) GO TO 210
C
C        ***  WE DID NOT.  TRY A LONGER STEP UNLESS THIS WAS A NEWTON
C        ***  STEP.
C
         V(RADFAC) = V(RDFCMX)
         GTS = V(GTSTEP)
         IF (V(FDIF) .LT. (HALF/V(RADFAC) - ONE) * GTS)
     1            V(RADFAC) = max(V(INCFAC), HALF*GTS/(GTS + V(FDIF)))
         IV(IRC) = 4
         IF (V(STPPAR) .EQ. ZERO) GO TO 230
         IF (V(DST0) .GE. ZERO .AND. (V(DST0) .LT. TWO*V(DSTNRM)
     1             .OR. V(NREDUC) .LT. ONEP2*V(FDIF)))  GO TO 230
C             ***  STEP WAS NOT A NEWTON STEP.  RECOMPUTE IT WITH
C             ***  A LARGER RADIUS.
              IV(IRC) = 5
              IV(RADINC) = IV(RADINC) + 1
C
C  ***  SAVE VALUES CORRESPONDING TO GOOD STEP  ***
C
 200  V(FLSTGD) = V(F)
      IV(MLSTGD) = IV(MODEL)
      IF (IV(RESTOR) .NE. 1) IV(RESTOR) = 2
      V(DSTSAV) = V(DSTNRM)
      IV(NFGCAL) = NFC
      V(PLSTGD) = V(PREDUC)
      V(GTSLST) = V(GTSTEP)
      GO TO 230
C
C  ***  ACCEPT STEP WITH RADIUS UNCHANGED  ***
C
 210  V(RADFAC) = ONE
      IV(IRC) = 3
      GO TO 230
C
C  ***  COME HERE FOR A RESTART AFTER CONVERGENCE  ***
C
 220  IV(IRC) = IV(XIRC)
      IF (V(DSTSAV) .GE. ZERO) GO TO 240
         IV(IRC) = 12
         GO TO 240
C
C  ***  PERFORM CONVERGENCE TESTS  ***
C
 230  IV(XIRC) = IV(IRC)
 240  IF (IV(RESTOR) .EQ. 1 .AND. V(FLSTGD) .LT. V(F0)) IV(RESTOR) = 3
      IF (abs(V(F)) .LT. V(AFCTOL)) IV(IRC) = 10
      IF (HALF * V(FDIF) .GT. V(PREDUC)) GO TO 999
      EMAX = V(RFCTOL) * abs(V(F0))
      EMAXS = V(SCTOL) * abs(V(F0))
      IF (V(PREDUC) .LE. EMAXS .AND. (V(DSTNRM) .GT. V(LMAXS) .OR.
     1     V(STPPAR) .EQ. ZERO)) IV(IRC) = 11
      IF (V(DST0) .LT. ZERO) GO TO 250
      I = 0
      IF ((V(NREDUC) .GT. ZERO .AND. V(NREDUC) .LE. EMAX) .OR.
     1    (V(NREDUC) .EQ. ZERO .AND. V(PREDUC) .EQ. ZERO))  I = 2
      IF (V(STPPAR) .EQ. ZERO .AND. V(RELDX) .LE. V(XCTOL)
     1                        .AND. GOODX)                  I = I + 1
      IF (I .GT. 0) IV(IRC) = I + 6
C
C  ***  CONSIDER RECOMPUTING STEP OF LENGTH V(LMAXS) FOR SINGULAR
C  ***  CONVERGENCE TEST.
C
 250  IF (IV(IRC) .GT. 5 .AND. IV(IRC) .NE. 12) GO TO 999
      IF (V(STPPAR) .EQ. ZERO) GO TO 999
      IF (V(DSTNRM) .GT. V(LMAXS)) GO TO 260
         IF (V(PREDUC) .GE. EMAXS) GO TO 999
              IF (V(DST0) .LE. ZERO) GO TO 270
                   IF (HALF * V(DST0) .LE. V(LMAXS)) GO TO 999
                        GO TO 270
 260  IF (HALF * V(DSTNRM) .LE. V(LMAXS)) GO TO 999
      XMAX = V(LMAXS) / V(DSTNRM)
      IF (XMAX * (TWO - XMAX) * V(PREDUC) .GE. EMAXS) GO TO 999
 270  IF (V(NREDUC) .LT. ZERO) GO TO 290
C
C  ***  RECOMPUTE V(PREDUC) FOR USE IN SINGULAR CONVERGENCE TEST  ***
C
      V(GTSLST) = V(GTSTEP)
      V(DSTSAV) = V(DSTNRM)
      IF (IV(IRC) .EQ. 12) V(DSTSAV) = -V(DSTSAV)
      V(PLSTGD) = V(PREDUC)
      I = IV(RESTOR)
      IV(RESTOR) = 2
      IF (I .EQ. 3) IV(RESTOR) = 0
      IV(IRC) = 6
      GO TO 999
C
C  ***  PERFORM SINGULAR CONVERGENCE TEST WITH RECOMPUTED V(PREDUC)  ***
C
 280  V(GTSTEP) = V(GTSLST)
      V(DSTNRM) = abs(V(DSTSAV))
      IV(IRC) = IV(XIRC)
      IF (V(DSTSAV) .LE. ZERO) IV(IRC) = 12
      V(NREDUC) = -V(PREDUC)
      V(PREDUC) = V(PLSTGD)
      IV(RESTOR) = 3
 290  IF (-V(NREDUC) .LE. V(RFCTOL) * abs(V(F0))) IV(IRC) = 11
C
 999  RETURN
C
C  ***  LAST CARD OF DA7SST FOLLOWS  ***
      END
c     ==================================================================
      SUBROUTINE DITSUM(D, G, IV, LIV, LV, P, V, X)
C
C  ***  PRINT ITERATION SUMMARY FOR ***SOL (VERSION 2.3)  ***
C
c     6/28/90 CLL Added test before the GO TO (...), IV1
c     to avoid printing the termination message when IV1 = 1 to 4 and
c     no other printing has been requested.
C  ***  PARAMETER DECLARATIONS  ***
C
      INTEGER LIV, LV, P
      INTEGER IV(LIV)
      DOUBLE PRECISION D(P), G(P), V(LV), X(P)
c     ------------------------------------------------------------------
C  ***  LOCAL VARIABLES  ***
C
      INTEGER ALG, I, IV1, M, NF, NG, OL, PU
      CHARACTER*4 MODEL1(6), MODEL2(6)
      DOUBLE PRECISION NRELDF, OLDF, PRELDF, RELDF, ZERO
C
C  ***  NO EXTERNAL FUNCTIONS OR SUBROUTINES  ***
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
      INTEGER ALGSAV, DSTNRM, F, FDIF, F0, NEEDHD, NFCALL, NFCOV, NGCOV,
     1        NGCALL, NITER, NREDUC, OUTLEV, PREDUC, PRNTIT, PRUNIT,
     2        RELDX, SOLPRT, STATPR, STPPAR, SUSED, X0PRT
      integer COVPRT, PARPRT
C
C  ***  IV SUBSCRIPT VALUES  ***
C
      PARAMETER (ALGSAV=51, NEEDHD=36, NFCALL=6, NFCOV=52, NGCALL=30,
     1           NGCOV=53, NITER=31, OUTLEV=19, PRNTIT=39, PRUNIT=21,
     2           SOLPRT=22, STATPR=23, SUSED=64, X0PRT=24)
      parameter (COVPRT=14, PARPRT=20)
C
C  ***  V SUBSCRIPT VALUES  ***
C
      PARAMETER (DSTNRM=2, F=10, F0=13, FDIF=11, NREDUC=6, PREDUC=7,
     1           RELDX=17, STPPAR=5)
C
      PARAMETER (ZERO=0.D+0)
      DATA MODEL1/'    ','    ','    ','    ','  G ','  S '/,
     1     MODEL2/' G  ',' S  ','G-S ','S-G ','-S-G','-G-S'/
C
C ------------------------------  BODY  --------------------------------
C
 30   FORMAT(/'   IT   NF',6X,'F',7X,'RELDF',3X,'PRELDF',3X,'RELDX',
     1       2X,'MODEL  STPPAR')
 40   FORMAT(/'    IT   NF',7X,'F',8X,'RELDF',4X,'PRELDF',4X,'RELDX',
     1       3X,'STPPAR')
 70   FORMAT(/'    IT   NF',6X,'F',7X,'RELDF',3X,'PRELDF',3X,'RELDX',
     1       2X,'MODEL  STPPAR',2X,'D*STEP',2X,'NPRELDF')
 80   FORMAT(/'    IT   NF',7X,'F',8X,'RELDF',4X,'PRELDF',4X,'RELDX',
     1       3X,'STPPAR',3X,'D*STEP',3X,'NPRELDF')
 100  FORMAT(I6,I5,g10.3,2g9.2,g8.1,A3,A4,2g8.1,g9.2)
 110  FORMAT(I6,I5,g11.3,2g10.2,3g9.1,g10.2)
 140  FORMAT(/' ***** COEFFICIENT CONVERGENCE *****')
 160  FORMAT(/' ***** RELATIVE FUNCTION CONVERGENCE *****')
 200  FORMAT(/' ***** ABSOLUTE FUNCTION CONVERGENCE *****')
 220  FORMAT(/' ***** SINGULAR CONVERGENCE *****')
 240  FORMAT(/' ***** FALSE CONVERGENCE *****')
 260  FORMAT(/' ***** FUNCTION EVALUATION LIMIT *****')
 280  FORMAT(/' ***** ITERATION LIMIT *****')
 300  FORMAT(/' ***** STOPX *****')
 320  FORMAT(/' ***** INITIAL F(X) CANNOT BE COMPUTED *****')
 340  FORMAT(/' ***** BAD PARAMETERS (Internal error) *****')
C340  FORMAT(/' ***** BAD PARAMETERS TO ASSESS *****')
 360  FORMAT(/' ***** GRADIENT COULD NOT BE COMPUTED *****')
 380  FORMAT(/' ***** IV(1) =',I5,' *****')
 410  FORMAT(/'     0',I5,g10.3)
 420  FORMAT(/'     0',I5,g11.3)
 450  FORMAT(/' FUNCTION',g17.6,'   RELDX',g17.3/' FUNC. EVALS',
     1   I8,9X,'GRAD. EVALS',I8/' PRELDF',g16.3,6X,'NPRELDF',g15.3)
 470  FORMAT(/'     I      FINAL X(I)',8X,'D(I)',10X,'G(I)'/)
 490  FORMAT(1X,I5,g16.6,2g14.3)
 510  FORMAT(/' INCONSISTENT DIMENSIONS')
c
      PU = IV(PRUNIT)
      IF (PU .EQ. 0) GO TO 999
      IV1 = IV(1)
      IF (IV1 .GT. 62) IV1 = IV1 - 51
      OL = IV(OUTLEV)
      ALG = MOD(IV(ALGSAV)-1,2) + 1
      IF (IV1 .LT. 2 .OR. IV1 .GT. 15) GO TO 370
      IF (IV1 .GE. 12) GO TO 120
      IF (IV1 .EQ. 2 .AND. IV(NITER) .EQ. 0) GO TO 390
      IF (OL .EQ. 0) GO TO 120
      IF (IV1 .GE. 10 .AND. IV(PRNTIT) .EQ. 0) GO TO 120
      IF (IV1 .GT. 2) GO TO 10
         IV(PRNTIT) = IV(PRNTIT) + 1
         IF (IV(PRNTIT) .LT. abs(OL)) GO TO 999
 10   NF = IV(NFCALL) - abs(IV(NFCOV))
      IV(PRNTIT) = 0
      RELDF = ZERO
      PRELDF = ZERO
      OLDF = max(abs(V(F0)), abs(V(F)))
      IF (OLDF .LE. ZERO) GO TO 20
         RELDF = V(FDIF) / OLDF
         PRELDF = V(PREDUC) / OLDF
 20   IF (OL .GT. 0) GO TO 60
C
C        ***  PRINT SHORT SUMMARY LINE  ***
C
         IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 1) WRITE(PU,30)
         IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 2) WRITE(PU,40)
         IV(NEEDHD) = 0
         IF (ALG .EQ. 2) GO TO 50
         M = IV(SUSED)
         WRITE(PU,100) IV(NITER), NF, V(F), RELDF, PRELDF, V(RELDX),
     1                 MODEL1(M), MODEL2(M), V(STPPAR)
         GO TO 120
C
 50      WRITE(PU,110) IV(NITER), NF, V(F), RELDF, PRELDF, V(RELDX),
     1                 V(STPPAR)
         GO TO 120
C
C     ***  PRINT LONG SUMMARY LINE  ***
C
 60   IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 1) WRITE(PU,70)
      IF (IV(NEEDHD) .EQ. 1 .AND. ALG .EQ. 2) WRITE(PU,80)
      IV(NEEDHD) = 0
      NRELDF = ZERO
      IF (OLDF .GT. ZERO) NRELDF = V(NREDUC) / OLDF
      IF (ALG .EQ. 2) GO TO 90
      M = IV(SUSED)
      WRITE(PU,100) IV(NITER), NF, V(F), RELDF, PRELDF, V(RELDX),
     1             MODEL1(M), MODEL2(M), V(STPPAR), V(DSTNRM), NRELDF
      GO TO 120
C
 90   WRITE(PU,110) IV(NITER), NF, V(F), RELDF, PRELDF,
     1             V(RELDX), V(STPPAR), V(DSTNRM), NRELDF
C
 120  IF (IV1 .LE. 2) GO TO 999
      I = IV(STATPR)
      IF (I .EQ. (-1)) GO TO 460
      IF (I + IV1 .LT. 0) GO TO 460
c
c        The following test skips printing the termination message
c        when convergence is satisfactory and no other printing has been
c        requested.
c
      if(IV1 .ge. 3 .and. IV1 .le. 6 .and.
     *   IV(COVPRT) .eq. 0 .and. IV(OUTLEV) .eq. 0 .and.
     *   IV(PARPRT) .eq. 0 .and. IV(SOLPRT) .eq. 0 .and.
     *   IV(STATPR) .eq. 0 .and. IV(X0PRT)  .eq. 0) go to 430
      GO TO (999, 999, 130, 150, 170, 190, 210, 230, 250, 270, 290, 310,
     1       330, 350, 500),  IV1
C
 130  WRITE(PU,140)
      GO TO 430
C
 150  WRITE(PU,160)
      GO TO 430
C
 170  WRITE(PU,'(1x/a)')
     * ' ***** COEFFICIENT AND RELATIVE FUNCTION CONVERGENCE *****'
      GO TO 430
C
 190  WRITE(PU,200)
      GO TO 430
C
 210  WRITE(PU,220)
      GO TO 430
C
 230  WRITE(PU,240)
      GO TO 430
C
 250  WRITE(PU,260)
      GO TO 430
C
 270  WRITE(PU,280)
      GO TO 430
C
 290  WRITE(PU,300)
      GO TO 430
C
 310  WRITE(PU,320)
C
      GO TO 390
C
 330  WRITE(PU,340)
      GO TO 999
C
 350  WRITE(PU,360)
      IF (IV(NITER) .GT. 0) GO TO 460
      GO TO 390
C
 370  WRITE(PU,380) IV(1)
      GO TO 999
C
C  ***  INITIAL CALL ON DITSUM  ***
C
 390  IF (IV(X0PRT) .NE. 0) then
         WRITE(PU,'(/''     I     INITIAL X(I)'',8X,''D(I)''/)')
         DO 400 I = 1, P
 400          WRITE(PU,'(1X,I5,g17.6,g14.3)') I, X(I), D(I)
      END IF
C     *** THE FOLLOWING ARE TO AVOID UNDEFINED VARIABLES WHEN THE
C     *** FUNCTION EVALUATION LIMIT IS 1...
      V(DSTNRM) = ZERO
      V(FDIF) = ZERO
      V(NREDUC) = ZERO
      V(PREDUC) = ZERO
      V(RELDX) = ZERO
      IF (IV1 .GE. 12) GO TO 999
      IV(NEEDHD) = 0
      IV(PRNTIT) = 0
      IF (OL .EQ. 0) GO TO 999
      IF (OL .LT. 0 .AND. ALG .EQ. 1) WRITE(PU,30)
      IF (OL .LT. 0 .AND. ALG .EQ. 2) WRITE(PU,40)
      IF (OL .GT. 0 .AND. ALG .EQ. 1) WRITE(PU,70)
      IF (OL .GT. 0 .AND. ALG .EQ. 2) WRITE(PU,80)
      IF (ALG .EQ. 1) WRITE(PU,410) IV(NFCALL), V(F)
      IF (ALG .EQ. 2) WRITE(PU,420) IV(NFCALL), V(F)
      GO TO 999
C
C  ***  PRINT VARIOUS INFORMATION REQUESTED ON SOLUTION  ***
C
 430  IV(NEEDHD) = 1
      IF (IV(STATPR) .LE. 0) GO TO 460
         OLDF = max(abs(V(F0)), abs(V(F)))
         PRELDF = ZERO
         NRELDF = ZERO
         IF (OLDF .LE. ZERO) GO TO 440
              PRELDF = V(PREDUC) / OLDF
              NRELDF = V(NREDUC) / OLDF
 440     NF = IV(NFCALL) - IV(NFCOV)
         NG = IV(NGCALL) - IV(NGCOV)
         WRITE(PU,450) V(F), V(RELDX), NF, NG, PRELDF, NRELDF
C
 460  IF (IV(SOLPRT) .EQ. 0) GO TO 999
         IV(NEEDHD) = 1
         IF (IV(ALGSAV) .GT. 2) GO TO 999
         WRITE(PU,470)
         DO 480 I = 1, P
 480          WRITE(PU,490) I, X(I), D(I), G(I)
      GO TO 999
C
 500  WRITE(PU,510)
 999  RETURN
C  ***  LAST CARD OF DITSUM FOLLOWS  ***
      END
c     ==================================================================
      SUBROUTINE DL7ITV(N, X, L, Y)
C
C  ***  SOLVE  (L**T)*X = Y,  WHERE  L  IS AN  N X N  LOWER TRIANGULAR
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME
C  ***  STORAGE.  ***
c     ------------------------------------------------------------------
      INTEGER N
      DOUBLE PRECISION X(N), L(*), Y(N)
      INTEGER I, II, IJ, IM1, I0, J, NP1
      DOUBLE PRECISION XI, ZERO
      PARAMETER (ZERO=0.D+0)
C
      DO 10 I = 1, N
 10      X(I) = Y(I)
      NP1 = N + 1
      I0 = N*(N+1)/2
      DO 30 II = 1, N
         I = NP1 - II
         XI = X(I)/L(I0)
         X(I) = XI
         IF (I .LE. 1) GO TO 999
         I0 = I0 - I
         IF (XI .EQ. ZERO) GO TO 30
         IM1 = I - 1
         DO 20 J = 1, IM1
              IJ = I0 + J
              X(J) = X(J) - XI*L(IJ)
 20           CONTINUE
 30      CONTINUE
 999  RETURN
C  ***  LAST CARD OF DL7ITV FOLLOWS  ***
      END
c     ==================================================================
      SUBROUTINE DL7SQR(N, A, L)
C
C  ***  COMPUTE  A = LOWER TRIANGLE OF  L*(L**T),  WITH BOTH
C  ***  L  AND  A  STORED COMPACTLY BY ROWS.  (BOTH MAY OCCUPY THE
C  ***  SAME STORAGE.
C
C  ***  PARAMETERS  ***
C
c     ------------------------------------------------------------------
      INTEGER N
      DOUBLE PRECISION A(*), L(*)
C     DIMENSION A(N*(N+1)/2), L(N*(N+1)/2)
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER I, II, IJ, IK, IP1, I0, J, JJ, JK, J0, K, NP1
      DOUBLE PRECISION T
C
      NP1 = N + 1
      I0 = N*(N+1)/2
      DO 30 II = 1, N
         I = NP1 - II
         IP1 = I + 1
         I0 = I0 - I
         J0 = I*(I+1)/2
         DO 20 JJ = 1, I
              J = IP1 - JJ
              J0 = J0 - J
              T = 0.0D0
              DO 10 K = 1, J
                   IK = I0 + K
                   JK = J0 + K
                   T = T + L(IK)*L(JK)
 10                CONTINUE
              IJ = I0 + J
              A(IJ) = T
 20           CONTINUE
 30      CONTINUE
      END
c     ==================================================================
      SUBROUTINE DL7SRT(N1, N, L, A, IRC)
C
C  ***  COMPUTE ROWS N1 THROUGH N OF THE CHOLESKY FACTOR  L  OF
C  ***  A = L*(L**T),  WHERE  L  AND THE LOWER TRIANGLE OF  A  ARE BOTH
C  ***  STORED COMPACTLY BY ROWS (AND MAY OCCUPY THE SAME STORAGE).
C  ***  IRC = 0 MEANS ALL WENT WELL.  IRC = J MEANS THE LEADING
C  ***  PRINCIPAL  J X J  SUBMATRIX OF  A  IS NOT POSITIVE DEFINITE --
C  ***  AND  L(J*(J+1)/2)  CONTAINS THE (NONPOS.) REDUCED J-TH DIAGONAL.
C
C  ***  PARAMETERS  ***
C
c     ------------------------------------------------------------------
      INTEGER N1, N, IRC
      DOUBLE PRECISION L(*), A(*)
C     DIMENSION L(N*(N+1)/2), A(N*(N+1)/2)
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER I, IJ, IK, IM1, I0, J, JK, JM1, J0, K
      DOUBLE PRECISION T, TD, ZERO
C
      PARAMETER (ZERO=0.D+0)
C
C  ***  BODY  ***
C
      I0 = N1 * (N1 - 1) / 2
      DO 50 I = N1, N
         TD = ZERO
         IF (I .EQ. 1) GO TO 40
         J0 = 0
         IM1 = I - 1
         DO 30 J = 1, IM1
              T = ZERO
              IF (J .EQ. 1) GO TO 20
              JM1 = J - 1
              DO 10 K = 1, JM1
                   IK = I0 + K
                   JK = J0 + K
                   T = T + L(IK)*L(JK)
 10                CONTINUE
 20           IJ = I0 + J
              J0 = J0 + J
              T = (A(IJ) - T) / L(J0)
              L(IJ) = T
              TD = TD + T*T
 30           CONTINUE
 40      I0 = I0 + I
         T = A(I0) - TD
         IF (T .LE. ZERO) GO TO 60
         L(I0) = sqrt(T)
* DNSGB (9 of 10)
 50      CONTINUE
C
      IRC = 0
      GO TO 999
C
 60   L(I0) = T
      IRC = I
C
 999  RETURN
C
C  ***  LAST CARD OF DL7SRT  ***
      END
c     ==================================================================
      SUBROUTINE DL7TVM(N, X, L, Y)
C
C  ***  COMPUTE  X = (L**T)*Y, WHERE  L  IS AN  N X N  LOWER
C  ***  TRIANGULAR MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY
C  ***  OCCUPY THE SAME STORAGE.  ***
c     ------------------------------------------------------------------
      INTEGER N
      DOUBLE PRECISION X(N), L(*), Y(N)
C     DIMENSION L(N*(N+1)/2)
      INTEGER I, IJ, I0, J
      DOUBLE PRECISION YI, ZERO
      PARAMETER (ZERO=0.D+0)
C
      I0 = 0
      DO 20 I = 1, N
         YI = Y(I)
         X(I) = ZERO
         DO 10 J = 1, I
              IJ = I0 + J
              X(J) = X(J) + YI*L(IJ)
 10           CONTINUE
         I0 = I0 + I
 20      CONTINUE
C  ***  LAST CARD OF DL7TVM FOLLOWS  ***
      END
c     ==================================================================
      SUBROUTINE DL7VML(N, X, L, Y)
C
C  ***  COMPUTE  X = L*Y, WHERE  L  IS AN  N X N  LOWER TRIANGULAR
C  ***  MATRIX STORED COMPACTLY BY ROWS.  X AND Y MAY OCCUPY THE SAME
C  ***  STORAGE.  ***
C
c     ------------------------------------------------------------------
      INTEGER N
      DOUBLE PRECISION X(N), L(*), Y(N)
C     DIMENSION L(N*(N+1)/2)
      INTEGER I, II, IJ, I0, J, NP1
      DOUBLE PRECISION T, ZERO
      PARAMETER (ZERO=0.D+0)
C
      NP1 = N + 1
      I0 = N*(N+1)/2
      DO 20 II = 1, N
         I = NP1 - II
         I0 = I0 - I
         T = ZERO
         DO 10 J = 1, I
              IJ = I0 + J
              T = T + L(IJ)*Y(J)
 10           CONTINUE
         X(I) = T
 20      CONTINUE
C  ***  LAST CARD OF DL7VML FOLLOWS  ***
      END
c     ==================================================================
      DOUBLE PRECISION FUNCTION DRLDST(P, D, X, X0)
C
C  ***  COMPUTE AND RETURN RELATIVE DIFFERENCE BETWEEN X AND X0  ***
C  ***  NL2SOL VERSION 2.2  ***
c     ------------------------------------------------------------------
      INTEGER P
      DOUBLE PRECISION D(P), X(P), X0(P)
C
      INTEGER I
      DOUBLE PRECISION EMAX, T, XMAX, ZERO
      PARAMETER (ZERO=0.D+0)
C
C  ***  BODY  ***
C
      EMAX = ZERO
      XMAX = ZERO
      DO 10 I = 1, P
         T = abs(D(I) * (X(I) - X0(I)))
         IF (EMAX .LT. T) EMAX = T
         T = D(I) * (abs(X(I)) + abs(X0(I)))
         IF (XMAX .LT. T) XMAX = T
 10      CONTINUE
      DRLDST = ZERO
      IF (XMAX .GT. ZERO) DRLDST = EMAX / XMAX
C  ***  LAST CARD OF DRLDST FOLLOWS  ***
      END
c     ==================================================================
      SUBROUTINE DV2AXY(P, W, A, X, Y)
C
C  ***  SET W = A*X + Y  --  W, X, Y = P-VECTORS, A = SCALAR  ***
C
c     ------------------------------------------------------------------
      INTEGER P
      DOUBLE PRECISION A, W(P), X(P), Y(P)
C
      INTEGER I
C
      DO 10 I = 1, P
 10      W(I) = A*X(I) + Y(I)
      RETURN
      END
c     ==================================================================
      SUBROUTINE DV7CPY(P, Y, X)
C
C  ***  SET Y = X, WHERE X AND Y ARE P-VECTORS  ***
C
c     ------------------------------------------------------------------
      INTEGER P
      DOUBLE PRECISION X(P), Y(P)
C
      INTEGER I
C
      DO 10 I = 1, P
 10      Y(I) = X(I)
      RETURN
      END
c     ==================================================================
      SUBROUTINE DV7SCL(N, X, A, Y)
C
C  ***  SET X(I) = A*Y(I), I = 1(1)N  ***
C
c     ------------------------------------------------------------------
      INTEGER N
      DOUBLE PRECISION A, X(N), Y(N)
C
      INTEGER I
C
      DO 10 I = 1, N
 10       X(I) = A * Y(I)
C  ***  LAST LINE OF DV7SCL FOLLOWS  ***
      END
c     ==================================================================
      SUBROUTINE DV7SCP(P, Y, SS)
C
C  ***  SET P-VECTOR Y TO SCALAR SS  ***
C
c     ------------------------------------------------------------------
      INTEGER P
      DOUBLE PRECISION SS, Y(P)
C
      INTEGER I
C
      DO 10 I = 1, P
 10      Y(I) = SS
      RETURN
      END
