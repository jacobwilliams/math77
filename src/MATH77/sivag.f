      subroutine SIVAG(TSPECS, Y, F, KORD, IFLAG, NSTOP, GNEW, GT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2001-09-07 SIVAG  Krogh  Changes to allow user tol on G-Stops.
c>> 1995-06-20 SIVAG  Krogh  Fixed problem introduced with last change.
c>> 1995-05-09 SIVAG  Krogh  Fixed G-Stop/discontinuity code interaction
C>> 1994-11-11 SIVAG  Krogh  Declared all vars.
c>> 1994-10-20 SIVAG  Krogh  Changes to use M77CON
c>> 1994-09-12 SIVAG  Krogh  Added CHGTYP code.
c>> 1994-08-17 SIVAG  Krogh  Modified internal comment.
c>> 1994-03-07 SIVAG  Krogh  Allow larger order in single precision.
c>> 1993-04-27 SIVAG  Krogh  Additions for Conversion to C.
c>> 1992-10-12 SIVAG  Krogh  Fixed G-Stop/discontinuity code interaction
c>> 1992-09-17 SIVAG  Krogh  Slight change in check for sign change.
c>> 1992-04-08 SIVAG  Krogh  Unused labels 140,150,230, and 250 removed.
c>> 1992-03-10 SIVAG  Krogh  Fixed value for KDIM in single p. version.
c>> 1990-01-29 SIVAG  Krogh  Added arg to call to DERMN.
c>> 1988-03-04 SIVAG  Krogh  Initial code.
c
c--S replaces "?": ?IVAG,?IVABU,?IVAEV,?IVAIN,?IVAMC,?IVASC,?MESS,?ZERO
c
c     -XXXG(TSPECS, Y, F, KORD, IFLAG, NSTOP, GNEW, GT)
c
c  SUBROUTINE TO LOCATE OUTPUT POINTS AT ZEROS OF ARBITRARY
c  FUNCTIONS  **** GSTOPS **** FOR DIFFERENTIAL EQUATION
c  INTEGRATOR -ODE (OR -IVA).
c
      integer KORD(*), IFLAG, NSTOP
c--S Next line special: P=>D, X=>Q
      real             TSPECS(*), Y(*), TOLD, TSAVE
      real             F(*), GNEW(*), GT(*), GOLD
      save GOLD, TOLD, TSAVE
c
c++SP Default KDIM = 16
c++  Default KDIM = 20
c++  Default MAXORD = 2, MAXSTF = 1
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 16, MAXORD = 2, MAXSTF = 1)
c--S Next line special: P=>D, X=>Q
      real             TN
      real             XI(KDIM)
c
c--S Next line special: P=>D, X=>Q
      real             TG(2), TGSTOP(2), TMARK, TMARKX, TOUT, TOLG
      real             ALPHA(KDIM), BETA(KDIM+1)
      real              D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      real             V(KDIM+MAXORD)
      real             HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      real             FDAT(11)
c
      real             DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      real             SIGMA(KDIM), RBQ(KDIM), DNOISE
      real             EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      real             EEPS10, EEPS16, EROV10, EEPS2
      real             EEPT75, EOVEP2, OVTM75, OVD10
      common / SIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / SIVAEV /
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / SIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /SIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,TOLG,HC,HDEC,HINC,
     1   HINCC,HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / SIVAMC / , / SIVASC /
c
      integer I, IG, IGFLGS, IZFLAG, KEXIT, NGSTOP(2)
      real             HH
      equivalence (G(1,1), HH), (NGSTOP(1), IOP6), (KEXIT, IOP17),
     1   (IZFLAG, IY), (IGFLGS, ITOLEP)
c
c                      Declarations for error message processing.
c
      integer MEMDA1, MERET, MEEMES
      parameter (MEMDA1 =27)
      parameter (MERET  =51)
      parameter (MEEMES =52)
      integer MACT(7)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA SIVAG$B
cAB Call with bad values of KORD.  KORD(1)=$I, KORD(2)=$I, when $C
c   TSPECS(1)=$F and KSTEP=$M.$E
      integer LTXTAA,LTXTAB
      parameter (LTXTAA= 1,LTXTAB= 8)
      character MTXTAA(1) * (95)
      data MTXTAA/'SIVAG$BCall with bad values of KORD.  KORD(1)=$I, KOR
     *D(2)=$I, when TSPECS(1)=$F and KSTEP=$M.$E'/
      data MACT / MEMDA1, 0, MEEMES, 68, 24, 0, MERET /
c
c ****************** START OF EXECUTABLE CODE **************
c
      IFLAG = 1
      IG = KORD(2)
      if ((IG .ne. 0) .and. (IG .ne. 1)) go to 500
      if (IGFLG - 3) 10, 80, 70
   10 if (IGFLG - 1) 20, 210, 60
c
c ******************** INITIAL POINT ***********************
c
   20 if (IGFLG .eq. -3) then
         IGFLG = 5
         return
      end if
      IGTYPE(IG + 1) = 0
      if ((NGSTOP(IG + 1) .le. 0) .or. (IG + IGFLG .eq. -1)) go to 30
      IGFLG = IG - 2
      go to 40
   30 IGFLG = 5
   40 NG = NGSTOP(2 - IG)
      do 50 I = 1, NG
   50    GT(I) = GNEW(I)
      go to 480
c
c     **** USER HAS BEEN TOLD THAT A GSTOP WAS FOUND
c          TEST IF CALLED FROM OUTPUT WHEN CALL SHOULD
c          BE FROM DERIVS
   60 if ((IGTYPE(1) .eq. 0) .and. (IG .ne. 0)) go to 420
c     **** PROTECT AGAINST NOISEY G NEAR THE ZERO
      if (GNEW(INGS) .eq. 0.E0) GNEW(INGS) = GT(INGS)
c
c ****** TEST FOR CHANGE IN THE SIGN OF A G ****************
c
   70 NG = NGSTOP(2 - IG)
      INGS = 0
   80 INGS = INGS + 1
      if (INGS .gt. NG) if (IGFLG - 4) 400, 380, 480
      if (GNEW(INGS)) 90, 100, 110
   90 if (GT(INGS)) 120, 120, 130
  100 if (GT(INGS)) 130, 80, 130
  110 if (GT(INGS)) 130, 120, 120
  120 GT(INGS) = GNEW(INGS)
      go to 80
c
c ********* A SIGN CHANGE HAS BEEN FOUND *******************
c
  130 NSTOP = INGS
      if (IG .eq. 0) NSTOP = -INGS
      if (IGFLG .ne. 5) go to 200
c     **** USUAL CASE -- TEST IF OUTPUT POINT PRECEDES THE
c          SIGN CHANGE, OR IF PREDICTING, CORRECTING, OR
c          NOT THROUGH THE FIRST STEP.
c     **** TEST IF AN INTERPOLATED G WAS WHAT CHANGED SIGN
      if (IG .ne. 0) go to 180
c     **** BACK UP DIFFERENCES AND OTHER STUFF TO BEGINNING
c          OF THE STEP
      call SIVABU(F, KORD)
c     **** TEST IF CORRECTING
      if (KORD1I .eq. 2) go to 170
c     **** TEST IF THROUGH THE FIRST STEP
      if (LSC .lt. 4) go to 180
c     **** IF FIRST DERIVATIVE EVALUATION OF THE FIRST
c          STEP, FIND THE GSTOP, AND USE IT TO GET A NEW
c          INITIAL STEPSIZE
      if (LSC .eq. 7) go to 200
c     **** SET NEW STEPSIZE AFTER SIGN CHANGE WHEN STARTING
  160 HH = TSPECS(1) - TN
c     **** SET KEXIT TO TRY NEW STEPSIZE
  170 KEXIT = 1
      TSPECS(1) = TN
      go to 460
c     **** SET KEXIT FOR USUAL CASE
  180 KEXIT = IG + 2
c     **** TEST IF SIGN CHANGE IN G PRECEDES NEXT OUTPUT PT.
      if (HH * (TSPECS(1) - TMARK)) 200, 200, 190
c     **** SET UP TO EVALUATE G AT OUTPUT POINT
  190 IGFLG = 4
      TSPECS(1) = TMARK
      NSTOP = 0
      go to 240
c
c ***************** FIND THE ZERO OF G *********************
c
c     **** INITIALIZE ZERO FINDER
  200 TOLD = TG(2 - IG)
      GOLD = GT(INGS)
      TSAVE = TSPECS(1)
      IGFLGS = IGFLG
      IGFLG = 1
      IZFLAG = 0
      go to 220
c     **** TEST IF ZERO ALREADY FOUND
  210 if (IZFLAG - 1) 350, 220, 310
  220 continue
      call SZERO(TSPECS(1), GNEW(INGS), TOLD, GOLD, IZFLAG, TOLG)
c     **** TEST FOR CONVERGENCE
      if (IZFLAG .ne. 1) go to 260
c     **** INTERPOLATE NEW Y, AND GO COMPUTE G AGAIN
  240 call SIVAIN(TSPECS(1), Y, F, KORD)
      IFLAG = 4
      KORD2I = IG - 3
      return
c     **** CONVERGENCE -- CHOOSE TOLD TO GIVE A CHANGE
c          IN SIGN
  260 if (GNEW(INGS) .eq. 0.E0) go to 290
      if (TSPECS(1) - TOLD) 270, 300, 280
  270 if (HH) 290, 300, 300
  280 if (HH) 300, 300, 290
  290 TOLD = TSPECS(1)
c     **** CHECK IF SIGN CHANGE DUE TO NOISE
  300 TSPECS(1) = TOLD + XI(1)
      go to 240
  310 TSPECS(1) = TOLD
      if (GNEW(INGS)) 320, 340, 330
  320 if (GT(INGS)) 340, 360, 360
  330 if (GT(INGS)) 360, 360, 340
c     **** ZERO WAS EVIDENTLY DUE TO NOISE
  340 TSPECS(1) = TSAVE
      IZFLAG = 0
      go to 370
  350 IGFLG = IGFLGS
c     SET KORD2I TO INITIAL VALUE TO AVOID LOOP
      KORD2I = IG
      go to 80
c     **** SAVE INFORMATION ABOUT THE STOP
  360 IGFLG = 3
      TGSTOP(2 - IG) = TSPECS(1)
      IGTYPE(2 - IG) = IZFLAG + 3
      IGSTOP(2 - IG) = NSTOP
  370 NSTOP = 0
      go to 240
c
c ************** AFTER SEARCH FOR A SIGN CHANGE ************
c
c     **** NO SIGN CHANGE AT A T OUTPUT POINT
c     TEST IF CALLED FROM OUTPUT
  380 if (IG .ne. 0) go to 390
c     SET UP FOR CALL TO OUTPUT
      KORD1I = 7
      KORD2I = -2
      IFLAG = 3
      go to 480
c     **** ADJUST KEXIT AND SET UP TO GIVE OUTPUT
  390 KEXIT = KEXIT + 2
      KORD1I = min(KMARK, 5)
      KORD(3) = KMARK
      KORD(1) = KORD1I
      IGFLG = 5
      IFLAG = 2
      go to 470
c     **** TEST IF USER HAS BEEN TOLD OF GSTOP
  400 if (IGFLG .eq. 2) go to 450
c     **** A GSTOP HAS BEEN FOUND
c     TEST IF STARTING
      if (LSC .eq. 7) go to 160
      IFLAG = IGTYPE(2 - IG)
      NSTOP = IGSTOP(2 - IG)
      INGS = abs(NSTOP)
      if (INGS .eq. 0) go to 410
      GT(INGS) = -GT(INGS)
      if (IG .eq. 0) go to 430
      IGFLG = 2
c     If interpolated GSTOP was found set to check again in case of
c     multiple stops at exactly the same point.
      if (IGTYPE(1) .ne. 0) go to 440
c     **** TELL USER OF AN EXTRAPOLATED GSTOP
  410 IFLAG = IGTYPE(2)
      NSTOP = IGSTOP(2)
      INGS = abs(NSTOP)
c     **** SET SO DERIVS IS CALLED WITH KORD(1) = KPRED
  420 KORD1I = KPRED
      KORD2I = -3
      return
c     **** AN EXTRAPOLATED GSTOP WAS FOUND, SET UP TO CHECK
c          INTERPOLATED STOPS (IF ANY)
  430 NG = NGSTOP(1)
      INGS = 0
      IFLAG = 3
      NSTOP = IGSTOP(2)
c     **** SET SO OUTPUT IS CALLED WITH KORD(1) = 7
  440 KORD1I = 7
      KORD2I = -2
      go to 490
c     **** CHECK THAT AN EXTRAPOLATED G-STOP IS NOT MISSED
  450 if ((IG .eq. 0) .or. (IGTYPE(2) .eq. 0)) go to 460
c     SET TO CHECK FOR INTERPOLATED G-S.
      TG(1) = TSPECS(1)
      IGTYPE(1) = 0
      TSPECS(1) = TGSTOP(2)
      INGS = 0
      IGFLG = 3
      go to 240
c     **** SET SO INTEGRATOR GOES TO PLACE DIRECTED BY KEXIT
  460 NSTOP = 0
      IGFLG = 5
      IFLAG = 3
  470 KORD2I = -7
c     **** STORE INFO. ON LAST G COMPUTED
  480 IGTYPE(2 - IG) = 0
  490 TG(2 - IG) = TSPECS(1)
      return
c
c ********************** ERROR PROCESSING ******************
c
  500 MACT(2) = KSTEP
c--S Next line special: P=>S, X=>D
      call SMESS(MACT, MTXTAA, KORD, TSPECS)
      IFLAG = 8
      KEXIT = 6
      go to 470
      end
