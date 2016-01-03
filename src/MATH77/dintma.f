      SUBROUTINE DINTMA (ANSWER,WORK,IFLAG)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2009-10-31 DINTMA Krogh  Initialized ACUM,PACUM,RESULT(2).
C>> 2008-01-11 DINTMA Krogh  Added new error message.
C>> 1996-03-31 DINTMA Krogh  Removed unused variable in common.
c>> 1995-11-20 DINTMA Krogh  Converted from SFTRAN to Fortran 77.
C>> 1995-11-16 DINTMA Krogh  Corrected comment.
C>> 1994-11-23 DINTMA Snyder Make recursion storage local
C>> 1994-11-14 DINTMA Krogh  Declared all vars.
c>> 1994-10-19 DINTMA Krogh  Changes to use M77CON
c>> 1994-07-07 DINTMA Snyder set up for CHGTYP.
C>> 1993-05-18 DINTMA Krogh  Changed "END" to "END PROGRAM"
c>> 1993-05-04 DINTMA Krogh  Additions for Conversion to C.
C>> 1992-04-09 DINTMA Krogh  added error message processing.
C>> 1991-09-20 DINTMA Krogh  converted '(1)' dimensioning to '(*)'.
C>> 1989-02-28 DINTMA Snyder Dimensioned MOVE
C>> 1988-04-28 DINTMA Snyder Initial code.
C
C     MULTIDIMENSIONAL QUADRATURE SUPERVISION PROGRAM.
c
c--D replaces "?": ?INTA,?INTC,?INTEC,?INTF,?INTM,?INTMA,?INTNC,?INTOP
C
C     *****     M77CON INFORMATION     *********************************
C
C     This program unit has two versions.  One will use ?COPY to
c     store the entire common block /DINTC/ in WORK.  This will work
c     in all environments where storage unit sizes follow the Fortran
c     standard: DOUBLE PRECISION uses twice as much storage as REAL, and
c     INTEGER and LOGICAL are the same size as real.  This version is
c     selected by setting STORE = WORK.
c
c     The other version has local SAVE arrays of types DOUBLE PRECISION,
c     REAL, INTEGER and LOGICAL, with enough space for DIMS+1 dimensions
c     of integration.  The default for DIMS is 5.  This version is
c     selected by setting STORE = LOCAL (or anything other than WORK).
c
c     One cannot automatically derive an extended precision version that
c     stores in WORK.
c
c++   Default STORE = LOCAL
c     DON'T SET SW, DW or SL in M77JOB !!!
c++   Default DW = {D} & STORE == WORK
c++   Default SW = {S} & STORE == WORK
c++   Default SL = ~(DW | SW)
c++   Default DIMS = 5
C
C     *****     PARAMETERS     *****************************************
c
c The following apply to the case when STORE = LOCAL:

c DIMS    is the number of dimensions for which to provide storage.
c         1 + DIMS dimensions may be integrated.
c KDBLE   is the number of double precision cells to copy.
c KINT    is the number of integer cells to copy.
c KLOG    is the number of logical cells to copy.
c KREAL   is the number of real or double precision cells to copy.
c
      integer DIMS, KDBLE, KINT, KLOG, KREAL
c++   Substitute for DIMS below
      parameter (DIMS=5, KDBLE=4, KINT=29, KLOG=11, KREAL=169)
c
C KWORK   IS THE AMOUNT OF WORKING STORAGE NEEDED FOR EVERY DIMENSION
C         OTHER THAN THE INNERMOST.  THE TOTAL SIZE OF WORK MUST BE AT
C         LEAST 3*NDIMI + KWORK*(NDIMI-1).  THE VALUE OF KWORK DEPENDS
C         ON THE MACHINE AND THE PRECISION OF THE PROGRAM, BUT FOR
C         PORTABILITY, THE USAGE SHOULD ALWAYS ASSUME THE WORST CASE FOR
C         EACH PRECISION.
C
c++   Code for ~SL is INACTIVE
c      INTEGER KWORK
c++   Code for DW is INACTIVE
c      parameter ( KWORK = KDBLE + KREAL + (KINT+KLOG+1)/2 )
c++   Code for SW is INACTIVE
c      parameter ( KWORK = 2*KDBLE + KREAL + KINT + KLOG )
c++   End
C
C     *****     FORMAL ARGUMENTS     ***********************************
C
C ANSWER  THE INTEGRAL OVER ALL DIMENSIONS WHEN THE INTEGRATION IS
C         COMPLETE.  THE INTEGRAL OVER THE INNER DIMENSIONS WHEN
C         THE INTEGRATION IS IN PROGRESS.
      DOUBLE PRECISION ANSWER
C WORK    WORKING STORAGE AS DESCRIBED IN DINTM.
      DOUBLE PRECISION WORK(*)
C IFLAG   INDICATES THE ACTION OR STATUS, AS DESCRIBED IN DINTM.
      INTEGER IFLAG(*)
C
C     *****     EXTERNAL REFERENCES     ********************************
C
c--       Begin mask code changes
c DCOPY   is used to copy DOUBLE PRECISION data.
c D1MACH  is used to get characteristics of DOUBLE PRECISION storage.
c I1MACH  is used to get characteristics of INTEGER storage.  We assume
c         INTEGER and LOGICAL occupy the same amount of storage.
c QCOPY   is used to copy EXTENDED PRECISION data.
c R1MACH  is used to get characteristics of REAL storage.
c SCOPY   is used to copy REAL data.
c--       End mask code changes
c
      external DCOPY
c++   Code for SL & {XQ} is INACTIVE
c      external QCOPY
c++   Code for SL & {SP} is INACTIVE
c      external SCOPY
c++   Code for ~SL is INACTIVE
cc--D Next line special: S => D, X => Q, D => D
c      double precision D1MACH
cc--D Next line special: S => D, X => Q, D => D
c      external D1MACH
c      integer I1MACH
c      external I1MACH
c++   Code for SW is INACTIVE
c      real R1MACH
c      external R1MACH
c++   End
C
C     *****     LOCAL VARIABLES     ************************************
C
c NWORK   is the necessary amount of work space per dimension.
      integer NWORK
c++   Code for SL is ACTIVE
c DSAVE   is used to save variables that are DOUBLE PRECISION in all
c         versions.
c ISAVE   is used to save variables that are INTEGER.
c LC      is used for loop control.
c LSAVE   is used to save variables that are LOGICAL.
c RSAVE   is used to save variables that are REAL or DOUBLE PRECISION
c         depending on the version.
c
c--D Next line special: S => D, X => Q, D => D
      double precision DSAVE(KDBLE,DIMS)
      integer LC, ISAVE(KINT,DIMS)
      logical LSAVE(KLOG,DIMS)
      double precision RSAVE(KREAL,DIMS)
      save DSAVE, ISAVE, LSAVE, RSAVE
c++   Code for ~SL is INACTIVE
cc DROUND  is the DOUBLE PRECISION round-off level.
cc FIRST   indicates whether the first call to DINTMA is in progress.
cc IBIG    is the biggest integer.
cc RROUND  is the REAL round-off level.
cc--D Next line special: S => D, X => Q, D => D
c      double precision DROUND
c      logical FIRST
c      save FIRST
c      integer IBIG
c++   Code for SW is INACTIVE
c      double precision RROUND
c++   END
C
C     *****     COMMON VARIABLES     ***********************************
C
C MOVE    IS EQUIVALENCED TO ACUM.  MOVE IS USED TO PASS THE CORRECT
C         TYPE VARIABLE TO DCOPY, AND TO MAKE DINTC THE CORRECT
C         LENGTH.
c++   Code for ~SL is INACTIVE
c      DOUBLE PRECISION MOVE(KWORK)
c++   Code for SL is ACTIVE
c DMOVE   is equivalenced to ACUM, to save DOUBLE PRECISION variables.
c IMOVE   is equivalenced to DISCF, to save INTEGER variables.
c LMOVE   is equivalenced to DID1, to save LOGICAL variables.
c RMOVE   is equivalenced to AACUM, to save variables that are REAL
c         or DOUBLE PRECISION depending on the version.
      integer IMOVE(KINT)
      logical LMOVE(KLOG)
      double precision RMOVE(KREAL)
c++   Code for SL & {XQ} is INACTIVE
cc--D Next line special: S => D, X => Q, D => D, P => D
c      double precision DMOVE(KDBLE)
c++   END
C
C     THE VARIABLES BELOW ARE NOT SEPARATELY SAVED FOR EACH DIMENSION
C     OF THE INTEGRATION, OR ARE ONLY USED FOR THE MULTIDIMENSIONAL
C     QUADRATURE.  SEE DINTA FOR DESCRIPTIONS OF VARIABLES NOT
C     EXPLAINED BELOW.
C
C ERRF    IS THE ERROR COMMITTED ON AN INNER INTEGRAL.  IT IS USED AS
C         THE ESTIMATED ERROR IN THE INTEGRAND FOR AN OUTER INTEGRAL.
C FEA1    STORES THE VALUE OF FEA FOR THE INNER INTEGRAL.
C FER1    STORES THE VALUE OF FER FOR THE INNER INTEGRAL.
C IPRINT  IS THE CURRENT DIAGNOSTIC PRINT LEVEL.  SEE DINTA.
C IXKDIM  INDEX IN IFLAG (=IOPT) IN WHICH UNUSUAL DIMENSION CHANGES ARE
C         TO BE STORED.  THIS IS SET BY OPTION 12, BUT THE DEFAULT IS 1.
C JPRINT  IS AN NDIM DIGIT INTEGER SPECIFYING ALL DIAGNOSTIC PRINT
C         LEVELS.  THE LOW ORDER DIGIT IS FOR THE INNER DIMENSION.
C         SEE DINTM.
C KDIM    IS THE CURRENT DIMENSION.
C NDIM    IS THE NUMBER OF DIMENSIONS.
C NFMAXM  IS THE TOTAL NUMBER OF FUNCTION VALUES ALLOWED IN THE INNER
C         INTEGRAL.  BY CONTRAST, NFMAX IS THE NUMBER OF FUNCTION VALUES
C         ALLOWED ON A SINGLE ITERATION OF THE INNER INTEGRAL.
C NXKDIM  INDEX-1 OF THE DIMENSION TO RESUME WHEN CALCULATION OF THE
C         INTEGRAL OVER THE CURRENT DIMENSION IS COMPLETE.  NXKDIM IS
C         USUALLY KDIM, BUT MAY BE DIFFERENT IF THE USER MAKES UNUSUAL
C         DIMENSION CHANGES.
C REVERM  IS THE USERS SELECTION OF REVERSE COMMUNICATION.
C REVERS  IS THE REVERSE COMMUNICATION FLAG FOR DINTA.
C WHEREM  IS USED AS A COMPUTED GO TO INDEX IF REVERM IS NON-ZERO.
C
C     COMMON /DINTNC/ CONTAINS VARIABLES NOT SEPARATELY SAVED FOR
C     EACH DIMENSION OF A MULTIPLE QUADRATURE.  COMMON /DINTC/
C     CONTAINS VARIABLES THAT MUST BE SAVED FOR EACH DIMENSION OF THE
C     QUADRATURE.  THE VARIABLES IN EACH COMMON BLOCK ARE STORED IN THE
C     ORDER - ALWAYS DOUBLE, DOUBLE IF DOUBLE PRECISION PROGRAM, DOUBLE
C     IF DOUBLE PRECISION PROGRAM AND EXPONENT RANGE OF DOUBLE AND
C     SINGLE VERY DIFFERENT, SINGLE, INTEGER, LOGICAL.  A PAD OF LOGICAL
C     VARIABLES IS INCLUDED AT THE END OF /DINTC/.  THE DIMENSION OF
C     THE PAD MAY NEED TO BE VARIED SO THAT NO VARIABLES BEYOND THE END
C     OF THE COMMON BLOCK ARE ALTERED.
C
C     DECLARATIONS OF COMMON /DINTNC/ VARIABLES.
C
      DOUBLE PRECISION AINIT, BINIT, FNCVAL, S, TP
      DOUBLE PRECISION FER, FER1, RELOBT, TPS, XJ, XJP
      INTEGER     FEA,       FEA1,      INC,       INC2,      IPRINT,
     1 ISTOP(2,2),JPRINT,    KDIM,      KK,        KMAXF,     NDIM,
     2 NFINDX,    NFMAX,     NFMAXM,    RELTOL,    REVERM,    REVERS,
     3 WHEREM
      LOGICAL NEEDH
C
C     DECLARATIONS OF COMMON /DINTC/ VARIABLES.
C
c--D Next line special: S => D, X => Q, D => D, P => D
      DOUBLE PRECISION ACUM, PACUM, RESULT(2)
C     139 $.TYPE.$ VARIABLES
      DOUBLE PRECISION
     1 AACUM,     ABSCIS,    DELMIN,    DELTA,     DIFF,      DISCX(2),
     2 END(2),    ERRINA,    ERRINB,    FAT(2),    FSAVE,
     3 FUNCT(24), F1,        F2,        LOCAL(4),  PAACUM,    PF1,
     4 PF2,       PHISUM,    PHTSUM,    PX,        SPACE(6),
     5 STEP(2),   START(2),  SUM,       T,         TA,        TASAVE,
     6 TB,        TEND,      WORRY(2),  X,         X1,
     7 X2,        XT(17),    FT(17),    PHI(34)
c Note XT, FT, and PHI above are last, because they must be in adjacent
c locations in DINTC.
C     30 $DSTYP$ VARIABLES
      DOUBLE PRECISION
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
      COMMON /DINTNC/
c        1       2       3     4        5       6       7        8
     W AINIT,  BINIT,  FNCVAL, S,      TP,     FER,    FER1,   RELOBT,
c       9      10       11      12      13       1       2        3
     X TPS,    XJ,     XJP,    FEA,    FEA1,   KDIM,    INC,    INC2,
c     4 (2,2)    8       9     10       11      12       13      14
     Y ISTOP,  JPRINT, IPRINT, KK,     KMAXF,  NDIM,   NFINDX, NFMAX,
c        15     16       17      18      19      20
     Z NFMAXM, RELTOL, REVERM, REVERS, WHEREM, NEEDH
      COMMON /DINTC/
     1 ACUM,   PACUM,  RESULT
      COMMON /DINTC/
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
      COMMON /DINTC/
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
      COMMON /DINTC/
c       170     171     172
     1 DISCF,  DISCHK, ENDPTS, INEW,   IOLD,   IP,     IXKDIM,
     2 J,      J1,     J1OLD,  J2,     J2OLD,  KMAX,   KMIN,
     3 L,      LENDT,  NFJUMP, NSUBSV, NXKDIM, TALOC,  WHERE2,
c      1       2          3      4       5         6      7       8
     4 I,      K,      KAIMT,  NSUB,   PART,   SEARCH, WHERE, NFEVAL
      COMMON /DINTC/
     1 DID1,   FAIL,   FATS,   FSAVED, HAVDIF, IEND,   INIT,   ROUNDF,
     2 XCDOBT, PAD
      SAVE /DINTNC/, /DINTC/
C
C     THE VARIABLES HERE DEFINE THE MACHINE ENVIRONMENT.  ALL ARE SET
C     IN DINTOP.  THE MEANING ATTACHED TO THESE VARIABLES CAN BE
C     FOUND BY LOOKING AT THE DEFINITIONS IN DINTOP.
      DOUBLE PRECISION
     1  EMEPS,  EEPSM8, EDELM2, EDELM3, ESQEPS, ERSQEP, ERSQE6, EMINF,
     2  ESMALL, ENZER,  EDELM1, ENINF
      COMMON /DINTEC/
     1  EMEPS,  EEPSM8, EDELM2, EDELM3, ESQEPS, ERSQEP, ERSQE6, EMINF,
     2  ESMALL, ENZER,  EDELM1, ENINF
      SAVE /DINTEC/
C
C     *****     EQUIVALENCE STATEMENTS     *****************************
C
c++   Code for ~SL is INACTIVE
c      EQUIVALENCE (ACUM, MOVE)
c++   Code for SL is ACTIVE
      equivalence (AACUM, RMOVE)
      equivalence (DISCF, IMOVE)
      equivalence (DID1, LMOVE)
c++   Code for SL & {XQ} is INACTIVE
c      equivalence (ACUM, DMOVE)
c++   End
C
C     *****    Statements for Processing Messages **********************
C
      INTEGER MERET, MEEMES
      PARAMETER (MERET  =51)
      PARAMETER (MEEMES =52)
      INTEGER MACT(5), IDAT(3)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA SINTMA$B
cAB NWORK = $I, needs to be at least $I, for NDIM = $I.$E
cAC Since you are using reverse communication you probably $C
c   forgot to exit when IOPT(1) + NDIM was .le. 0$E
cAD $I = IFLAG($I) should be an inner integral dimension $C
c   in the interval [1,$I].$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD
      parameter (LTXTAA=  1,LTXTAB=  9,LTXTAC= 62,LTXTAD=164)
      character MTXTAA(1) * (242)
      data MTXTAA/'SINTMA$BNWORK = $I, needs to be at least $I, for NDIM
     * = $I.$ESince you are using reverse communication you probably for
     *got to exit when IOPT(1) + NDIM was .le. 0$E$I = IFLAG($I) should$
     * be an inner integral dimension in the interval [1,$I].$E'/
c **** End of automatically generated text
      DATA MACT / MEEMES, 88, 0, 0, MERET /
c
c     *****     DATA Statement     *************************************
c
c++   Code for ~SL is INACTIVE
c      data FIRST /.TRUE./
c++   End
C
C     *****     PROCEDURES     *****************************************
C
      IF (WHEREM.NE.0) THEN
        GO TO (90,75,30), WHEREM
      end if
C
C     OUTERMOST DIMENSION.
C
c++   Code for SL is ACTIVE
      nwork = 0
c++   Code for ~SL is INACTIVE
c      if (first) then
c        first = .false.
cc--D Next line special: S => D, X => Q, D => D
c        dround = d1mach(4)
c        ibig = i1mach(9)
c++   Code for SW is INACTIVE
c        rround = r1mach(4)
c        if (rround .ge. 0.875*dround) then
c          nwork = kdble + kreal
c        else
c          nwork = 2*kdble + kreal
c        end if
c        if (ibig * rround .ge. 0.125) then
c          nwork = nwork + kint + klog
c        else
c          nwork = nwork + (kint + klog + 1) / 2
c        end if
c++   Code for DW is INACTIVE
c        if (ibig * dround .ge. 0.125) then
c          nwork = kdble + kreal + kint + klog
c        else
c          nwork = kdble + kreal + (kint + klog + 1) / 2
c        end if
c      end if
c++   End
      IDAT(2) = NWORK*(NDIM-1)+3*NDIM
      IF (KDIM.LT.IDAT(2)) THEN
         IFLAG(1)=-NDIM-3
         if ((REVERM .ne. 0) .and. (KDIM .ge. NDIM)) then
           MACT(4) = LTXTAC
         else
           MACT(4) = LTXTAB
         end if
         IDAT(1) = KDIM
         IDAT(3) = NDIM
         GO TO 118
      END IF
      KDIM=NDIM
      FEA1=FEA
      FER1=FER
      IF (NFINDX.NE.0) IFLAG(NFINDX)=0
      GO TO 25
C
C     ASK FOR LIMITS OF THE KDIMTH DIMENSION.
C
20    TALOC=0
25    WHEREM=3
      IFLAG(1)=KDIM
      IFLAG(IXKDIM)=KDIM
      WORK(1)=1.0d0
      ERRINA=0.0d0
      ERRINB=0.0d0
      NFEVAL=1
C     NFEVAL IS TESTED IN DINTOP.  DINTF MAY CALL DINTOP.
C  Intialize ACUM,PACUM,RESULT(2) to avoid diagnostics.
      ACUM = 0.D0
      PACUM = 0.D0
      RESULT(2) = 0.D0

      IF (REVERM.NE.0) RETURN
      CALL DINTF (ANSWER,WORK,IFLAG(1))
30    IF (ABS(IFLAG(1)).GT.NDIM) TALOC=IFLAG(1)
      WHERE=0
      IF (KDIM.NE.NDIM) THEN
C        WE CAN USE EPS HERE BECAUSE DINTA WILL CHANGE IT BEFORE USE
         EPS=ABS((WORK(NDIM+KDIM+1)-WORK(2*NDIM+KDIM+1))*WORK(1))
         EPS=MAX(EPS,MAX(EPSO*ESMALL,ESMALL))
         EPSO=EPSO/EPS
         IF (ABS(IFLAG(1)).LE.NDIM .AND. IFLAG(IXKDIM).LE.NDIM) THEN
            IF (IFLAG(IXKDIM).LT.1 .OR. IFLAG(IXKDIM).GT.KDIM) GO TO 115
C           INNER DIMENSION MAY BE LESS THAN EXPECTED.
            KDIM=IFLAG(IXKDIM)
         END IF
      END IF
50    IPRINT=MOD(JPRINT/10**(KDIM-1),10)
      AINIT=WORK(NDIM+KDIM)
      BINIT=WORK(2*NDIM+KDIM)
      IF (KDIM.EQ.1) THEN
         FEA=FEA1
         FER=FER1
         IF (NFINDX.NE.0) NFEVAL=IFLAG(NFINDX)
         NFMAX=NFMAXM
         REVERS=REVERM
      ELSE
         FEA=1
         FER=EMEPS
c        FER=EFERVL
c        EFERVL was EMEPS in single precision, and 8*EMEPS in double
         NFMAX=0
         REVERS=1
      END IF
      IFLAG(1)=0
C     TEST IFLAG(1) IN CASE REVERSE COMMUNICATION IS IN EFFECT, AND THE
C     CALLING PROGRAM IS ITERATING CALLS TO DINTA.
75    IF (IFLAG(1).LT.0) GO TO 80
      CALL DINTA (ANSWER,WORK(1),IFLAG)
      IF (IFLAG(1)) 80,100,110
C
C     FINISHED WITH INTEGRAL OF KDIMTH DIMENSION.
C
80    IF (KDIM.EQ.NDIM) GO TO 120
85    WHEREM=1
      IFLAG(1)=-KDIM
      IFLAG(IXKDIM)=-KDIM
      IF (REVERM.NE.0) RETURN
      CALL DINTF (ANSWER,WORK,IFLAG(1))
C     *****     ITERATE ON INNER INTEGRALS IF NECESSARY HERE     *******
90    CONTINUE
      KDIM=NXKDIM
      IF (IFLAG(IXKDIM).GE.0) THEN
         IF (IFLAG(IXKDIM).GT.NXKDIM) GO TO 115
         KDIM=IFLAG(IXKDIM)
C        INTEGRAND IS A FUNCTION OF MORE INTEGRALS.
         GO TO 20
      END IF
c++   Code for DW is INACTIVE
c      CALL DCOPY (KWORK,WORK(3*NDIM+KWORK*KDIM-KWORK+1),1,MOVE,1)
c++   Code for SW is INACTIVE
c      CALL SCOPY (KWORK,WORK(3*NDIM+KWORK*KDIM-KWORK+1),1,MOVE,1)
c++   Code for SL & {XQ} is INACTIVE
c      call qcopy (kdble, dsave(1,kdim), 1, dmove, 1)
c++   Code for SL & (~{XQ}) is ACTIVE
      acum = dsave(1,kdim)
      pacum = dsave(2,kdim)
      result(1) = dsave(3,kdim)
      result(2) = dsave(4,kdim)
c++   Code for SL & {Q} is INACTIVE
c      call qcopy (kreal, rsave(1,kdim), 1, rmove, 1)
c++   Code for SL & {XD} is ACTIVE
      call dcopy (kreal, rsave(1,kdim), 1, rmove, 1)
c++   Code for SL & {PS} is INACTIVE
c      call scopy (kreal, rsave(1,kdim), 1, rmove, 1)
c++   Code for SL is ACTIVE
      do 93 lc = 1, kint
        imove(lc) = isave(lc,kdim)
93    continue
      do 94 lc = 1, klog
        lmove(lc) = lsave(lc,kdim)
94    continue
c++   End
      KDIM=KDIM+1
      GO TO 50
C
C     NEED A FUNCTION VALUE FOR THE KDIMTH DIMENSION.
C
100   WHEREM=2
      IF (KDIM.EQ.1) RETURN
      KDIM=KDIM-1
c++   Code for DW is INACTIVE
c      CALL DCOPY (KWORK,MOVE,1,WORK(3*NDIM+KWORK*KDIM-KWORK+1),1)
c++   Code for SW is INACTIVE
c      CALL SCOPY (KWORK,MOVE,1,WORK(3*NDIM+KWORK*KDIM-KWORK+1),1)
c++   Code for SL & {XQ} is INACTIVE
c      call qcopy (kdble, dmove, 1, dsave(1,kdim), 1)
c++   Code for SL & (~{XQ}) is ACTIVE
      dsave(1,kdim) = acum
      dsave(2,kdim) = pacum
      dsave(3,kdim) = result(1)
      dsave(4,kdim) = result(2)
c++   Code for SL & {Q} is INACTIVE
c      call qcopy (kreal, rmove, 1, rsave(1,kdim), 1)
c++   Code for SL & {XD} is ACTIVE
      call dcopy (kreal, rmove, 1, rsave(1,kdim), 1)
c++   Code for SL & {PS} is INACTIVE
c      call scopy (kreal, rmove, 1, rsave(1,kdim), 1)
c++   Code for SL is ACTIVE
      do 103 lc = 1, kint
        isave(lc,kdim) = imove(lc)
103   continue
      do 104 lc = 1, klog
        lsave(lc,kdim) = lmove(lc)
104   continue
c++   End
      NXKDIM=KDIM
      GO TO 20
C
C     ERROR.
C
110   CONTINUE
      IF (KDIM.NE.NDIM) THEN
         IF (IFLAG(1).EQ.6) THEN
C           NON-INTEGRABLE SINGULARITY IN INNER INTEGRAL.  USE ALMOST
C           MACHINE INFINITY WITH THE SIGN OF ACUM FOR THE ANSWER, AND
C           ALMOST MACHINE INFINITY FOR THE ERROR ESTIMATE.
C           THE "SIGN" FUNCTION WANTS ITS ARGUMENTS TO BE THE SAME TYPE.
            ANSWER=ACUM
            ANSWER=SIGN(ENINF,ANSWER)
            WORK(1)=ENINF
            GO TO 85
         END IF
      END IF
      WHEREM=2
      IF (IFLAG(1).NE.5) WHEREM=0
      IFLAG(1)=-NDIM-IFLAG(1)
      GO TO 130
C
C     ERRONEOUS INNER INTEGRAL DIMENSIONALITY.
C
 115  MACT(4) = LTXTAD
      IDAT(3) = KDIM
      IDAT(2) = IXKDIM
      IDAT(1) = IFLAG(IXKDIM)
      IFLAG(1)=-NDIM-NDIM-KDIM-5
c                                  Print an error message and stop.
118   MACT(3) = IFLAG(1)
      call MESS(MACT, MTXTAA, IDAT)
      GO TO 130
C
C     NORMAL COMPLETION.
C
120   IFLAG(1)=-(NDIM-(IFLAG(1)+1))
      WHEREM=0
130   CONTINUE
      RETURN
C
      END
