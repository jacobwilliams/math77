      REAL             FUNCTION SINTSM (SXMIN)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1996-03-31 SINTSM  Krogh  Removed unused variable in common.
c>> 1995-11-20 SINTSM  Krogh  Converted from SFTRAN to Fortran 77.
c>> 1994-10-19 SINTSM  Krogh  Changes to use M77CON
c>> 1994-07-07 SINTSM  Snyder set up for CHGTYP.
c>> 1994-07-05 SINTSM  Snyder  Corrected calculation
C>> 1993-05-18 SINTSM  Krogh -- Changed "END" to "END PROGRAM"
C>> 1987-11-19 SINTSM  Snyder  Initial code.
C
c--S replaces "?": ?intc, ?intec, ?intnc, ?INTSM
c
C     CALCULATE THE MINIMUM STEPSIZE TO USE IF ALOCAL WERE SET EQUAL TO
C     SXMIN.
C
C     WRITE X = TA + (T-TA)**2/TB.  IF WE LET X2 - X1 BE THE SMALLEST
C     ALLOWED STEP AT X1, SAY SMIN, THEN
C     TB*(X2-X1) = TB*SMIN = (T2-TA)**2 - (T1-TA)**2, OR
C     TB*SMIN = (T2-T1)*(T2-T1+2*(T1-TA)).  SOLVING FOR T2-T1 PROVIDES
C     THE EXPRESSIONS IN THE CODE BELOW.  THE ANALYSIS PROCEEDS
C     SIMILARLY WHEN X = TA + (T-TA)**4/TB**3.
C
      REAL             SXMIN
C
C     *****     LOCAL VARIABLES     ************************************
C
C SG      IS A TEMPORARY VARIABLE
      REAL             SG
C SMIN    IS THE VALUE THAT WILL BE RETURNED AS THE MINIMUM STEPSIZE.
      REAL             SMIN
C SOLVE   IS AN ARITHMETIC STATEMENT FUNCTION DEFINED BELOW.
      REAL             SOLVE
C SQRTTB  IS SQRT(ABS(TB))
      REAL             SQRTTB
C SX      IS A LOCAL COPY OF SXMIN.
      REAL             SX
C TDECR   IS AN ARITHMETIC STATEMENT FUNCTION DEFINED BELOW.
      REAL             TDECR
C
C     *****     COMMON VARIABLES     ***********************************
C
C     COMMON /SINTNC/ CONTAINS VARIABLES NOT SEPARATELY SAVED FOR
C     EACH DIMENSION OF A MULTIPLE QUADRATURE.  COMMON /SINTC/
C     CONTAINS VARIABLES THAT MUST BE SAVED FOR EACH DIMENSION OF THE
C     QUADRATURE.  THE VARIABLES IN EACH COMMON BLOCK ARE STORED IN THE
C     ORDER - ALWAYS DOUBLE, DOUBLE IF DOUBLE PRECISION PROGRAM, DOUBLE
C     IF DOUBLE PRECISION PROGRAM AND EXPONENT RANGE OF DOUBLE AND
C     SINGLE VERY DIFFERENT, SINGLE, INTEGER, LOGICAL.  A PAD OF LOGICAL
C     VARIABLES IS INCLUDED AT THE END OF /SINTC/.  THE DIMENSION OF
C     THE PAD MAY NEED TO BE VARIED SO THAT NO VARIABLES BEYOND THE END
C     OF THE COMMON BLOCK ARE ALTERED.
C
C     DECLARATIONS OF COMMON /SINTNC/ VARIABLES.
C
      REAL             AINIT, BINIT, FNCVAL, S, TP
      REAL             FER, FER1, RELOBT, TPS, XJ, XJP
      INTEGER     FEA,       FEA1,      INC,       INC2,      IPRINT,
     1 ISTOP(2,2),JPRINT,    KDIM,      KK,        KMAXF,     NDIM,
     2 NFINDX,    NFMAX,     NFMAXM,    RELTOL,    REVERM,    REVERS,
     3 WHEREM
      LOGICAL NEEDH
C
C     DECLARATIONS OF COMMON /SINTC/ VARIABLES.
C
c--D Next line special: S => D, X => Q, D => D, P => D
      DOUBLE PRECISION ACUM, PACUM, RESULT(2)
C     139 $.TYPE.$ VARIABLES
      REAL
     1 AACUM,     ABSCIS,    DELMIN,    DELTA,     DIFF,      DISCX(2),
     2 END(2),    ERRINA,    ERRINB,    FAT(2),    FSAVE,
     3 FUNCT(24), F1,        F2,        LOCAL(4),  PAACUM,    PF1,
     4 PF2,       PHISUM,    PHTSUM,    PX,        SPACE(6),
     5 STEP(2),   START(2),  SUM,       T,         TA,        TASAVE,
     6 TB,        TEND,      WORRY(2),  X,         X1,
     7 X2,        XT(17),    FT(17),    PHI(34)
c Note XT, FT, and PHI above are last, because they must be in adjacent
c locations in SINTC.
C     30 $DSTYP$ VARIABLES
      REAL
     1 ABSDIF,    COUNT,     EDUE2A,    EDUE2B,    EP,        EPNOIZ,
     2 EPS,       EPSMAX,    EPSMIN,    EPSO,      EPSR,      EPSS,
     3 ERR,       ERRAT(2),  ERRC,      ERRF,      ERRI,      ERRT(2),
     4 ESOLD,     EXTRA,     PEPSMN,    RE,        RELEPS,    REP,
     5 REPROD,    RNDC,      TLEN,      XJUMP
C     29 INTEGER VARIABLES
      INTEGER     DISCF,     DISCHK,    ENDPTS,    I,         INEW,
     1 IOLD,      IP,        IXKDIM,    J,         J1,        J1OLD,
     2 J2,        J2OLD,     K,         KAIMT,     KMAX,      KMIN,
     3 L,         LENDT,     NFEVAL,    NFJUMP,    NSUB,      NSUBSV,
     4 NXKDIM,    PART,      SEARCH,    TALOC,     WHERE,     WHERE2
C     11 TO 18 LOGICALS (7 ARE PADDING).
      LOGICAL     DID1,      FAIL,      FATS(2),   FSAVED,    HAVDIF,
     1 IEND,      INIT,      ROUNDF,    XCDOBT(2), PAD(7)
C
C     THE COMMON BLOCKS.
C
      COMMON /SINTNC/
c        1       2       3     4        5       6       7        8
     W AINIT,  BINIT,  FNCVAL, S,      TP,     FER,    FER1,   RELOBT,
c       9      10       11      12      13       1       2        3
     X TPS,    XJ,     XJP,    FEA,    FEA1,   KDIM,    INC,    INC2,
c     4 (2,2)    8       9     10       11      12       13      14
     Y ISTOP,  JPRINT, IPRINT, KK,     KMAXF,  NDIM,   NFINDX, NFMAX,
c        15     16       17      18      19      20
     Z NFMAXM, RELTOL, REVERM, REVERS, WHEREM, NEEDH
      COMMON /SINTC/
     1 ACUM,   PACUM,  RESULT
      COMMON /SINTC/
c        1     2 (4)     6      7        8       9      10     11 (2)
     1 AACUM,  LOCAL,  ABSCIS, TA,     DELTA,  DELMIN, DIFF,   DISCX,
c     13 (2)     15      16    17 (2)   19     20 (24) 44
     2 END,    ERRINA, ERRINB, FAT,    FSAVE,  FUNCT,  F2,
c       45      46     47       48      49     50      51 (6)
     3 PAACUM, PF1,    PF2,    PHISUM, PHTSUM, PX,     SPACE,
c      57 (2)  59 (2)   61     62        63    64       65
     4 STEP,   START,  SUM,    T,      TASAVE, TB,     TEND,
c      66 (2)  68      69      70      71       72
     5 WORRY,  X1,     X2,     X,      F1,     COUNT,
c      73 (17) 90 (17) 107 (34)
     6 XT,     FT,     PHI
      COMMON /SINTC/
c       141     142    143     144      145     146
     1 ABSDIF, EDUE2A, EDUE2B, EP,     EPNOIZ, EPSMAX,
c       147     148     149    150 (2)  152     153
     2 EPSO,   EPSR,   EPSS,   ERRAT,  ERRC,   ERRF,
c     154 (2)   156     157     158     159    160
     3 ERRT,   ESOLD,  EXTRA,  PEPSMN, RELEPS, REP,
c       161     162     163
     4 RNDC,   TLEN,   XJUMP,
c       164    165      166    167    168       169
     5 ERRI,   ERR,    EPSMIN, EPS,    RE,     REPROD
      COMMON /SINTC/
c       170     171     172
     1 DISCF,  DISCHK, ENDPTS, INEW,   IOLD,   IP,     IXKDIM,
     2 J,      J1,     J1OLD,  J2,     J2OLD,  KMAX,   KMIN,
     3 L,      LENDT,  NFJUMP, NSUBSV, NXKDIM, TALOC,  WHERE2,
c      1       2          3      4       5         6      7       8
     4 I,      K,      KAIMT,  NSUB,   PART,   SEARCH, WHERE, NFEVAL
      COMMON /SINTC/
     1 DID1,   FAIL,   FATS,   FSAVED, HAVDIF, IEND,   INIT,   ROUNDF,
     2 XCDOBT, PAD
      SAVE /SINTNC/, /SINTC/
C
C     THE VARIABLES HERE DEFINE THE MACHINE ENVIRONMENT.  ALL ARE SET
C     IN DINTOP.  THE MEANING ATTACHED TO THESE VARIABLES CAN BE
C     FOUND BY LOOKING AT THE DEFINITIONS IN DINTOP.
      REAL
     1  EMEPS,  EEPSM8, EDELM2, EDELM3, ESQEPS, ERSQEP, ERSQE6, EMINF,
     2  ESMALL, ENZER,  EDELM1, ENINF
      COMMON /SINTEC/
     1  EMEPS,  EEPSM8, EDELM2, EDELM3, ESQEPS, ERSQEP, ERSQE6, EMINF,
     2  ESMALL, ENZER,  EDELM1, ENINF
      SAVE /SINTEC/
C
C     *****     STATEMENT FUNCTIONS     ********************************
C
C SOLVE   PROVIDES THE SOLUTION OF A QUADRATIC EQUATION.
      SOLVE(SX,SG)=SQRTTB*SX/(SG*SQRTTB+SQRT(ABS(TB)*SG*SG+SX))
C TDECR   IS USED TO TRANSFORM AN ABSCISSA FROM THE CURRENT COORDINATE
C         SYSTEM TO ONE IN WHICH NSUB IS DECREMENTED BY A FACTOR OF 2.
C     TDECR(SX)=TA+(SX-TA)*((SX-TA)/TB)
      TDECR(SX)=TA*(1.0+TA/TB)+SX*((SX-TA)/TB-TA/TB)
C
C     *****     EXECUTABLE STATEMENTS     ******************************
C
      SX=SXMIN
      IF (NSUB .EQ. 0) THEN
         SG = SX
      ELSE
         SG = TDECR(SX)
         IF (NSUB .NE. 2) SG = TDECR(SG)
      END IF
      SMIN=EDELM3*MAX(EDELM1,ABS(SG))
      IF (NSUB .NE. 0) THEN
         SQRTTB=SQRT(ABS(TB))
         SG = ABS((SX-TA)/TB)
         SMIN=SOLVE(SMIN,SG)
         IF (NSUB .NE. 2) SMIN=SOLVE(SMIN,SG*SG)
      END IF
      SINTSM=SMIN
      RETURN
      END
