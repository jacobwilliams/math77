      SUBROUTINE DINTOP (IOPT,WORK)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2000-12-01 DINTOP  Krogh  Removed unused variable MENTXT.
C>> 1996-03-31 DINTOP  Krogh  Removed unused variable in common.
c>> 1996-03-30 DINTOP  Krogh  Added external statement.
c>> 1995-11-20 DINTOP  Krogh  Converted from SFTRAN to Fortran 77.
C>> 1994-11-14 DINTOP  Krogh  Declared all vars.
c>> 1994-10-19 DINTOP  Krogh  Changes to use M77CON
c>> 1994-08-15 DINTOP  Snyder corrected mixed types in MAX and MIN
c>> 1994-07-07 DINTOP  Snyder set up for CHGTYP.
C>> 1993-05-18 DINTOP  Krogh -- Changed "END" to "END PROGRAM"
c>> 1993-04-29 DINTOP  Krogh  Additions for Conversion to C.
C>> 1992-03-03 DINTOP  Krogh added error messages.
C>> 1991-09-20 DINTOP  Krogh converted '(1)' dimensioning to '(*)'.
C>> 1987-12-07 DINTOP Snyder  Initial code.
c
c--D replaces "?": ?INT, ?INT1, ?intc, ?intec, ?INTM, ?INTNC, ?INTOP
C
C     ******************************************************************
C
C     THIS SUBROUTINE IS USED TO SPECIFY OPTIONS FOR DINT1 AND DINTM.
C     IT IS CALLED BY DINT1 AND DINTM.  IT MAY ALSO BE CALLED
C     BY THE USER DURING THE INTEGRATION TO CHANGE A RESTRICTED
C     SET OF THE OPTIONS.  IF IT IS CALLED WITH NFEVAL = 0, IT IS
C     ASSUMED THAT THE CALL CAME FROM DINT1 OR DINTM.  IN
C     THIS CASE, ALL OPTIONS ARE FIRST SET TO THEIR DEFAULT VALUES.
C     IF IT IS CALLED WITH NFEVAL .NE. 0, IT IS ASSUMED THAT THE
C     CALL CAME DIRECTLY FROM THE USER.  IN THIS CASE, ONLY A SUBSET
C     OF THE OPTIONS MAY BE CHANGED.  THE OPTIONS THAT MAY BE CHANGED
C     DO NOT REFER TO THE WORK VECTOR.  THUS WORK IS IN THIS CASE A
C     DUMMY ARGUMENT.  THE OPTIONS THAT MAY BE CHANGED CORRESPOND TO
C     NON-ZERO ELEMENTS OF THE VECTOR ICHG.
C     SEE DINT1 OR DINTM FOR A DESCRIPTION OF THE OPTIONS.
C
C     *****     FORMAL ARGUMENTS     ***********************************
C
C IOPT    IS THE OPTION VECTOR (SEE DINT1 OR DINTM).
      INTEGER IOPT(*)
C WORK    IS THE WORK VECTOR (SEE DINT1 OR DINTM).
      DOUBLE PRECISION WORK(*)
C
C     *****     EXTERNAL REFERENCES     ********************************
C
C D1MACH  FETCHES MACHINE PARAMETERS.
      EXTERNAL D1MACH
      DOUBLE PRECISION D1MACH
C
C     *****    LOCAL VARIABLES     *************************************
C
C ICHG    A VECTOR DEFINING WHICH OPTIONS MAY BE CHANGED WHEN NFEVAL
C         IS NON-ZERO.
      INTEGER ICHG(13)
C II JJ   INDICES.
      INTEGER II, JJ
C
C     *****    COMMON STORAGE ******************************************
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
C     *****    Statements for Processing Messages **********************
C
      INTEGER MEMDA1, MEMDA2, MERET, MEEMES, MEIVEC
      PARAMETER (MEMDA1 =27)
      PARAMETER (MEMDA2 =28)
      PARAMETER (MERET  =51)
      PARAMETER (MEEMES =52)
      PARAMETER (MEIVEC =57)
      INTEGER MACT(11)
c     *********     Error message text     *****************************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DINT$B
cAB Last value is IOPT specfies bad option, IOPT(1:$M) = $B
cAC Option $M, can not be changed while integrating, IOPT(1:$M) = $B
      integer LTXTAA,LTXTAB,LTXTAC
      parameter (LTXTAA=  1,LTXTAB=  7,LTXTAC= 62)
      character MTXTAA(1) * (125)
      data MTXTAA/'DINT$BLast value is IOPT specfies bad option, IOPT(1:
     *$M) = $BOption $M, can not be changed while integrating, IOPT(1:$M
     *) = $B'/
C                      1 2      3  4      5  6 7 8      9 10   11
      DATA MACT / MEMDA1,0,MEMDA2, 0,MEEMES,77,4,0,MEIVEC,0,MERET /
C
C     *****    DATA STATEMENTS   ***************************************
C
      DATA ICHG /1,1,0,0,0,1,1,1,1,0,1,1,0/
C
C     *****    PROCEDURES     ******************************************
C
C
C     SET OPTIONS TO DEFAULT VALUES.
C
      IOPT(1)=0
      IF (NFEVAL.EQ.0) THEN
         EMEPS = D1MACH(4)
         EMINF = D1MACH(2)
         ESMALL = 2.0 * MAX(D1MACH(1),1.0/EMINF)
         EDELM1 = ESMALL / EMEPS
         EDELM2 = 202.48
         EDELM3 = EMEPS * EDELM2
         EEPSM8 = 8.0 * EMEPS
         ENINF = 0.001 * EMINF
         ENZER = 50.0 * ESMALL
         ESQEPS = SQRT(EMEPS)
         ERSQEP = 1.0 / ESQEPS
         ERSQE6 = 0.001 * ERSQEP
         EPSO=0.0
         ERRINA=0.0
         ERRINB=0.0
         FEA=0
C        FER=EFERVL
C        EFERVL was EMEPS in single precision, and 8*EMEPS in double
         FER = EMEPS
         KMAXF=3
         NFINDX=0
         RELOBT=0.75
         RELTOL=0
         TALOC=0
         IF (NDIM.NE.1) THEN
            JPRINT=1
            DO 10 II=2,NDIM
10             JPRINT=10*JPRINT+1
            REVERM=0
            NFMAXM=0
            IXKDIM=1
         ELSE
            IPRINT=1
            REVERS=0
            NFMAX=0
         END IF
      END IF
C
C     SET OPTIONS SPECIFIED IN THE OPTION VECTOR.
C
      II=2
C     DO BLOCK
C        DO FOREVER
20       CONTINUE
            JJ=IOPT(II)
            IF (JJ.EQ.0) GO TO 300
            IF (JJ.LT.0 .OR. JJ.GT.13) THEN
               MACT(2) = II
               MACT(8) = LTXTAB
               GO TO 320
            END IF
            IF (NFEVAL.NE.0) THEN
               IF (ICHG(JJ).EQ.0) THEN
                  MACT(2) = JJ
                  MACT(4) = II
                  MACT(8) = LTXTAC
                  GO TO 320
               END IF
            END IF
C           DO CASE (JJ,13)
            GO TO (110,120,130,140,150,160,170,180,190,200,210,220,230),
     1         JJ
C           CASE 1
c                  No longer used.
110            continue
               go to 290
C           CASE 2
120            continue
               IF (NDIM.EQ.1) THEN
                  IPRINT=IOPT(II+1)
               ELSE
                  JPRINT=IOPT(II+1)
                  IF (NFEVAL.NE.0) IPRINT=MOD(JPRINT/10**(KDIM-1),10)
               END IF
               go to 290
C           CASE 3
130            continue
               RELTOL=IOPT(II+1)
               EPSO=WORK(RELTOL)
               RELOBT=1.0d0-MAX(0.0d0,MIN(1.0d0,WORK(RELTOL+1)))
               go to 290
C           CASE 4
140            continue
               FEA=IOPT(II+1)
               go to 290
C           CASE 5
150            continue
               JJ=IOPT(II+1)
C              FER=MAX(EFERVL,WORK(JJ))
C           EFERVL was EMEPS in single precision, and 8*EMEPS in double
               FER=MAX(EMEPS,WORK(JJ))
               go to 290
C           CASE 6
160            continue
               REVERM=1
               REVERS=1
               II=II-1
               go to 290
C           CASE 7
170            continue
               KMAXF=MAX(3,MIN(8,IOPT(II+1)))
               go to 290
C           CASE 8
180            continue
               go to 290
C           CASE 9
190            continue
               NFMAXM=IOPT(II+1)
               NFMAX=NFMAXM
               go to 290
C           CASE 10
200            continue
               NFINDX=II+1
               go to 290
C           CASE 11
210            continue
               TALOC=IOPT(II+1)
               go to 290
C           CASE 12
220            continue
               ERRINA=WORK(IOPT(II+1))
               ERRINB=WORK(IOPT(II+1)+1)
               go to 290
C           CASE 13
230            continue
               IXKDIM=II+1
C           END CASE
290         II=II+2
            go to 20
300      CONTINUE
C        END FOREVER
         RETURN
320   CONTINUE
C     END BLOCK
      MACT(10) = II
      IOPT(1)=4
      CALL MESS(MACT, MTXTAA, IOPT)
      RETURN
      END
