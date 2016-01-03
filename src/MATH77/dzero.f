      subroutine DZERO(X1, F1, X2, F2, MODE, TOL)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2010-04-14 DZERO  Krogh  No discontinuity message if |F1-F2| small.
c>> 2010-04-12 DZERO  Krogh  Fixed KNKP to get discontinuity diagnostic.
c>> 2010-02-20 DZERO  Krogh  $G => $F for print out of iterations
c>> 2008-03-01 DZERO  Krogh  Minor change in diagnostic print.
c>> 2000-12-01 DZERO  Krogh  Removed unused variable C1P01.
c>> 1998-11-01 DZERO  Krogh  Set so errors stop less easily.
c>> 1998-11-01 DZERO  Krogh  For "mangle", INDIC replaced with MACT(3).
c>> 1996-03-30 DZERO  Krogh  Added external statement.
c>> 1995-11-09 DZERO  Krogh  Fixed so char. data at col. 72 is not ' '.
C>> 1994-11-11 DZERO  Krogh  Declared all vars.
c>> 1994-10-20 DZERO  Krogh  Changes to use M77CON
c>> 1994-09-08 DZERO  Krogh  Added CHGTYP code.
c>> 1993-04-27 DZERO  Krogh  Additions for Conversion to C.
c>> 1993-04-13 DZERO  Krogh  Minor change for new MESS.
c>> 1992-04-08 DZERO  Krogh  Unused label 400 removed.
c>> 1992-01-09 DZERO  Krogh  Moved calc. of XXMXO up (for error msg.)
c>> 1991-11-26 DZERO  Krogh  Converted to new error processor.
c>> 1988-08-14 DZERO  Krogh  Labels runumbered.
c>> 1988-03-07 DZERO  Krogh  Initial code.
c
c--D replaces "?": ?ZERO, ?MESS
c
c SUBROUTINE TO FIND A BOUNDED ZERO
c
c analysis and coding by Fred T.Krogh at the Jet Propulsion
c Laboratory, Pasadena, Calif.  April 25, 1972.
c Modified for portability, April 1984 by Krogh.
c Algorithmic changes, vars. added to save stmt., Sept. 1987 by Krogh
c
c Parameters in the calling sequence are defined as follows:
c
c  X1  = independent variable
c  F1  = dependent variable --  initially   F1=F(X1).
c        When MODE=1 (or 5) the user is to compute F(X1) given X1
c  X2  = second value of independent variable
c  F2  = F(X2) on the initial entry.  When MODE = 2-4, F2=F(X2) and
c        F1*F2 .le. 0.
c  MODE  is a parameter used for communication between this
c        subroutine and the user. (The user should set MODE
c        only to initialize it to 0 before the first call)
c      =1  compute F(X1) and call $ZERO
c      =2  F(X1) is approximately 0, the iteration is finished
c          and the error criterion is satisfied.
c      =3  same as MODE=2, except the error criterion can
c          not be satisfied.
c      =4  apparently the function has a discontinuity
c          between X1 and X2 -- No zero can be found
c      =5  F1*F2 was greater than zero on the first call, and an attempt
c          to bound the zero on both sides have failed.
c      =6  fatal error -- $ZERO was called after mode was set .ge.2.
c          If $ZERO is called again, the program will be stopped.
c          (Unless MODE is set to 0)
c      <0  If MODE is set <0 and $ZERO is called, no action is taken
c          except that print is turned on for -MODE calls to $ZERO.
c          This print gives all values of X and F used in the iteration.
c  TOL    is the error tolerance
c     TOL.GT.0  Iterate until values of X1 and X2 are known
c              for which abs(X1-X2) .le. tol and F1*F2 .le. 0.
c     TOL.LT.0  Iterate until a value of X1 is found for which
c              abs(F1) .le. abs(TOL).
c     TOL  = 0  Iterate until the zero is determined as
c              precisely as possible.  MODE = 3 is impossible
c              in this case.
c
c Parameters in the calling sequence have the following types
c
      integer MODE
      double precision X1, X2, F1, F2, TOL
c
c Usage is as follows (of course, variations are possible.)
c         Somehow one has available X1, F1, X2, and F2 such
c         that F1 = F(X1), F2 = F(X2) and F1*F2 .le. 0.
c         In addition, one should assign a value to TOL.
c     MODE = 0
c***  In the statement below, $ is replaced by an 'S' for single
c***  precision and a 'D' for double.
c XXX call $ZERO(X1,F1,X2,F2,MODE,TOL)
c     go to  (N1,N2,N3,N4,N5,N6), MODE
c  N1 COMPUTE  F1=F(X1)
c     go to XXX
c
c  N4 continue
c  N5 continue
c  N6 stop
c  N3 If you want to -- print results to note that error
c                       is too big.
c  N2 zero is found, do whatever you want to with it.
c
c End of comments explaining usage.
c
c ************************* Usage of internal variables ****************
c
c C0     Parameter = 0.
c C1     Parameter = 1.
c C1P25  Parameter = 1.25
c C2     Parameter = 2.
c C4     Parameter = 4.
c CP01   Parameter = 0.01
c CP125  Parameter = 1.25
c CP25   Parameter = 0.25
c CP5    Parameter = 0.5
c CP75   Parameter = 0.75
c CP99   Parameter = 0.99
c D1MACH Gets constants associated with floating point arithmetic.
c DFDXXX = (XXMXL/FFMFL) * (est. deriv. of f w.r.t. x at x = XX).  All
c   derivatives are base on a second degree polynonial that interpolates
c   the last three points generated.
c DFDXXX = (XXMXO/FFMFL) * (est. deriv. of f w.r.t. x at x = X0).
c DIV    If artificial subdivision of the interval is used, determines
c   the amount of the sudivision.  (-XXMXOO * DIV / (1. + DIV))
c DMESS  Prints error messages.
c DXDFFF = (FFMFL/XXMXL) * (est. deriv. of x w.r.t. f at f = FF).
c DXDFFO = (FFMFO/XXMXL) * (est. deriv. of x w.r.t. f at f = FO).
c F1     (formal arg.) The last value of F computed, on return the value
c   of F(X1).
c F2     (formal arg.) The other initial value provided for F.  Set to
c   the value of F(X2) on returns.
c FDAT   Temporary storage for floating point values for messages.
c FF     Value of F1 after F is computed.
c FFDFO  FF / FO
c FFMFB  FFMFL + FLMFB = FF - value of FF 2 iterations back.
c FFMFL  FF - FL
c FL     Value of FF from the previous iteration.
c FLMFB  Value of FFMFL from the previous iteration
c FO     F(XO)
c I      Comments for LCHNG define how I is set.
c IDAT   Temporary storage for integer values for messages.
c J      This is 1 if FF .le. 0., and is 2 if FF > 0.
c KNKP   KNKP(J) (see J above) is decreased by 3 when there are signs of
c   decent convergence.  It is counted up when convergence is slow.
c KS     =-1  initially,
c        = 0  whenever F changes sign, otherwise
c        = number of times the sign of F has remained the same
c KTYP   = 1 if interpolation was used to get the last iterate, = 0 if
c   an artificial subdivision was used.
c LCHG  the J-th continuation in the data statement for LCHG below gives
c new states for the case when the state number is J-1.  State 0 is the
c initial state.  The I-th entry on a row gives the state for case on I
c as follows:  (TP is the ratio (new f) / (last f of same sign)
c    I = 1   TP < 0.01
c    I = 2   .01 <= TP < 1
c    I = 3   TP = 1
c    I = 4   1 < TP <= 4
c    I = 5   TP > 4.
c States are as follows:
c    0   initial state, or big increase, or small increase in state 0
c    1   after big decrease, perhaps followed by small decreases
c    2   after one or more small decreases from state 0
c    3   one or more small increases from state 2
c    4   one or more small decreases from state 3
c    5   decision made that noise is a problem on this side of zero.
c LINIT  - the maximum number of iterations that can be taken with out
c   getting a sign change in F.
c LMODE  The value of MODE the last time in this routine.
c LNLP
c LTXTxx Names of this form are used in setting up data statements for
c   error messages.  These names are generated automatically by PMESS,
c   the program that makes up these messages.
c MACT   This array difines the actions to be taken by the error message
c   program.  See comments in MESS for details.  MODE is set to MACT(3)
c   on exit.
c MACT1  As for MACT except used for the diagnostic print.
c MExxxx Parameters defining constants used for interaction with the
c   error message program MESS.  See comments there for definitions.
c MLOC   Contains locations in MTXTAA for error messages.
c MODE   (formal) See comments above.
c MTXTAA Text for error messages.
c MTXTAB Text for diagnostic message.
c MTXTAC Text for diagnostic message.
c NP     If > 0, gives number of iterations till diagnostic print stops.
c QFM
c QXM
c RND    Largest relative difference between succesive floating point
c   numbers.
c SMALL  .5 / (RND * largest floating point number)
c TOL    (Formal) See description above.
c TOLX   Actually tolerance required for accuracy in X.  Usually =
c   max(TOL, XRND).  It can be cut by a factor of 2 for use in setting
c   bounds on an acceptable interval.
c TP     Ordinarily the ratio (FF / prev. FF of the same sign.
c TP1    Used for temporary storage.
c X1     (Formal) Value of x where F is to be computed, and value
c   returned for the zero after convergence.
c X2     (Formal) Initially other value of x where F is given.  After
c   convergence gives the other closest x which gives an F of opposite
c   sign from that given by x1.
c XL     Value of XX from the previous iteration.
c XLMXB  Value of XXMXL from the previous iteration.
c XO     Value of x on opposite side of the zero from the current x.
c XRND   Best accuracy that one could hope for based on the finite
c   precision of floating point numbers.
c XX     Current x, the last value of X1 where F was computed.
c XXMXL  XX - XL
c XXMXO  XX - XO = length of interval in which 0 lies.
c XXMXOL Value of XXMXO from a previous iteration.
c
      external D1MACH
      integer LINIT, KS, KTYP, J, I
      parameter (LINIT = -40)
      integer KNKP(2), LCHG(30), LMODE, LNLP(2), NP
      double precision XX, XO, XL, FF, FO, FL, FFDFO
      double precision DIV, QFM, QXM, TP, TP1, XXMXO, XXMXOL
      double precision RND, XRND, SMALL, TOLX
      double precision XXMXL, XLMXB, FFMFL, FFMFB, FLMFB
      double precision DXDFFF, DXDFFO, DFDXXX, DFDXXO
      double precision C0, C1, C2, C4, CP125, CP25, CP5, CP75, C1P25
      double precision C8, CP01, CP99, CP001, C1P031
      double precision D1MACH
c
      parameter (C0 = 0.D0, C1 = 1.D0, C2 = 2.D0, C4 = 4.D0)
      parameter (C8 = 8.D0)
      parameter (CP125 = 0.125D0, CP25 = 0.25D0, CP75 = 0.75D0)
      parameter (CP5 = 0.5D0)
      parameter (C1P25 = 1.25D0)
      parameter (CP01 = 0.01D0)
      parameter (CP001 = 0.001D0)
      parameter (CP99 = 0.99D0)
      parameter (C1P031 = 1.03125D0)
c
c                      Declarations for error message processing.
c
      integer MERET, MEEMES, METEXT
      double precision FDAT(4)
      integer MACT(5), MACT1(2), MLOC(4), IDAT(2)
      save DIV, FL, FLMFB, FO, KNKP, KS, KTYP, LCHG, LMODE,
     1   LNLP, MACT, NP, RND, SMALL, XL, XLMXB, XO, XX, XXMXOL
      parameter (MERET  =51)
      parameter (MEEMES =52)
      parameter (METEXT =53)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DZERO$B
cAB Best bound for zero is [$F, $F], but tolerance is $F.$E
cAC Apparent discontinuity in function near X = $F.$E
cAD Can not find a sign change: X1=$F, X2=$F, F1=$F, F2=$F$E
cAE Called with MODE = $I.$E
c   $
cAF In DZERO -- X1=$F F1=$F KTYP=$I DIV=$G KS=$I$E
c   $
cAG             X2=$F F2=$F$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG
      parameter (LTXTAA=  1,LTXTAB=  8,LTXTAC= 63,LTXTAD=112,LTXTAE=169,
     * LTXTAF=  1,LTXTAG=  1)
      character MTXTAA(1) * (193)
      character MTXTAB(1) * (46)
      character MTXTAC(1) * (25)
      data MTXTAA/'DZERO$BBest bound for zero is [$F, $F], but tolerance
     * is $F.$EApparent discontinuity in function near X = $F.$ECan not$
     * find a sign change: X1=$F, X2=$F, F1=$F, F2=$F$ECalled with MODE$
     * = $I.$E'/
      data MTXTAB/'In DZERO -- X1=$F F1=$F KTYP=$I DIV=$F KS=$I$E'/
      data MTXTAC/'            X2=$F F2=$F$E'/
c **** End of automatically generated text
c                      1  2  3  4      5
      data MACT / MEEMES, 0, 0, 0, MERET /
      data MACT1 / METEXT, MERET /
      data MLOC / LTXTAB, LTXTAC, LTXTAD, LTXTAE /
c
      data RND / C0 /
      data KS, KTYP, LMODE, DIV / 0, 0, 2, C0 /
      data LCHG /
     1   1, 2, 0, 0, 0,
     2   1, 1, 4, 5, 0,
     3   1, 2, 3, 3, 0,
     4   1, 4, 4, 3, 0,
     5   1, 4, 5, 5, 0,
     6   1, 5, 5, 5, 0 /
      data NP / 0 /

c
c INITIALIZE
c
      if (MODE .lt. 0) then
         NP = -1 - MODE
         return
      end if
      if (NP .gt. 0) then
         NP = NP - 1
         FDAT(1) = X1
         FDAT(2) = F1
         FDAT(3) = DIV
         IDAT(1) = KTYP
         IDAT(2) = KS
         call DMESS(MACT1, MTXTAB, IDAT, FDAT)
         if (MODE .ne. 0) if (LMODE - 1) 70, 80, 450
         FDAT(1) = X2
         FDAT(2) = F2
         call DMESS(MACT1, MTXTAC, IDAT, FDAT)
      else if (MODE .ne. 0) then
         if (LMODE - 1) 70, 80, 450
      end if
c
      if (RND .eq. C0) then
         RND = D1MACH(4)
         SMALL = CP5 / (RND * D1MACH(2))
      end if
      XL = X2
      FL = F2
   30 TP = C1
      MODE = 1
      MACT(3) = 2
      XXMXOL = C0
      KNKP(1) = 0
      KNKP(2) = 0
      LNLP(1) = 0
      LNLP(2) = 0
      KS = -1
      XX = X1
      FF = F1
      if (FL) 40, 75, 50
   40 if (FF) 60, 230, 100
   50 if (FF) 100, 230, 60
   60 LMODE = 0
c             Take care of points on same side of zero.
   70 FF = F1
      XX = X1
      TP = FF / FL
      if (TP .lt. C0) go to 30
      LMODE = LMODE - 1
      if (LMODE .lt. LINIT) then
         MACT(3) = 5
         FDAT(1) = XX
         FDAT(2) = XL
         FDAT(3) = FF
         FDAT(4) = FL
         go to 250
      end if
      if (TP .gt. C1) then
         FF = FL
         XX = XL
         FL = F1
         XL = X1
      end if
      if (abs(FF) .ge. C8 * abs(FL-FF)) then
         TP = C8
      else
         TP = max(-CP25*dble(LMODE), FF / (FL - FF))
      end if
      FL = FF
      XO = XL
      XL = XX
      if (XX .eq. XO) XO = C1P031 * XX + sign(CP001, XX)
      XX = XX + TP * (XX - XO)
      X1 = XX
      MODE = 1
      return
c
   75 X1 = XL
      F1 = FL
      go to 250
c END OF INITIALIZATION
c
c
c ENTRY AFTER COMPUTING F FOR THE LAST ITERATE
   80 FF = F1
      TP = FF / FL
      if (TP) 90, 230, 110
   90 TP = FF / FO
      KS = 0
  100 FO = FL
      XO = XL
      go to 120
  110 KS = KS + 1
  120 J = 1
      if (FF .gt. C0) J = 2
      if (TP - C1) 150, 140, 130
  130 I = 4
      if (TP .gt. C4) I = 5
      go to 160
  140 I = 3
      go to 160
  150 I = 2
      if (TP .lt. CP01) I = 1
      if (TP .lt. CP99) go to 170
  160 KNKP(J) = KNKP(J) + 1
      go to 180
  170 KNKP(J) = 0
  180 XXMXO = XX - XO
      LNLP(J) = LCHG(5*LNLP(J) + I)
      if (LNLP(J) .ge. 4) then
         if (LNLP(3 - J) .ge. 4) go to 210
      end if
c XXMXO GIVES THE LENGTH OF THE INTERVAL INSIDE WHICH
c THE ZERO IS KNOWN TO LIE.
      if (C2 * abs(XXMXO) .lt. abs(XXMXOL)) then
         KNKP(J) = max(0, KNKP(1) - 3)
      end if
      XXMXOL = XXMXO
      XRND = RND * (abs(XX) + abs(XO) + SMALL)
c
c TEST FOR CONVERGENCE
      if (TOL) 190, 200, 200
  190 continue
      if (abs(FF) .le. abs(TOL)) go to 220
  200 continue
      TOLX = max(TOL, XRND)
      if (abs(XXMXO) .gt. TOLX) go to 310
c
c CONVERGENCE -- PREPARE FOR FINAL EXIT
  210 if ((abs(XXMXO) .gt. TOL) .and. (TOL .ne. C0)) then
         MACT(3) = 3
         FDAT(3) = TOL
         if (XXMXO .gt. 0) then
            FDAT(2) = XX
            FDAT(1) = XO
         else
            FDAT(1) = XX
            FDAT(2) = XO
         end if
      end if
c SET FINAL VALUES FOR X1,F1,X2,AND F2
  220 continue
      if (abs(FF) .le. abs(FO)) go to 240
      F1 = FO
      X1 = XO
  230 FO = FF
      XO = XX
  240 X2 = XO
      F2 = FO
c TEST FOR DISCONTINUITY
      if ((KNKP(1) .gt. 5) .or. (KNKP(2) .gt. 5)) then
        if (abs(F1 - F2) .gt. RND * max(X1, 1.D0)) then
          MACT(3) = 4
          FDAT(1) = XX
        end if
      end if
  250 MODE = MACT(3)
      if (MACT(3) - 2) 420, 420, 430
c END OF CODE FOR FINAL EXIT
c
c F NOT DECREASING (OR THE FIRST ITERATE)
c PREPARE TO DIVIDE THE INTERVAL
  260 TP = C1
      if (KS) 370, 280, 270
  270 if (KTYP .eq. 0) go to 290
  280 DIV = C2
  290 continue
      DIV = max(DIV, FFDFO)
c KTYP=0 IF AND ONLY IF THE INTERVAL WAS DIVIDED (USING DIV)
c ON THE LAST ITERATION
      if (KTYP .eq. 0) DIV = DIV * (C1P25 / (C1P25 - TP))
c DIVIDE THE INTERVAL AS SPECIFIED BY DIV
  300 TP1 = -XXMXO * (DIV/(DIV+C1))
      KTYP = 0
      go to 410
c
  310 continue
      XXMXL = XX - XL
      FFMFL = FF - FL
      FFDFO = abs(FF / FO)
      TOLX = CP5 * TOLX
      if (TP .ge. C1) go to 260
c DIVIDE THE INTERVAL IF F HAS HAD THE SAME SIGN FOR
c FOUR OR MORE TIMES IN SUCCESSION
      if (KS - 4) 320, 340, 290
  320 continue
      if (FLMFB .eq. C0) go to 340
c BEGINNING OF CODE TO DETERMINE IF INVERSE QUADRATIC
c INTERPOLATION IS TO BE USED.
      FFMFB = FFMFL + FLMFB
      if (FFMFB .eq. C0) go to 330
      QFM = C1 - (FFMFL / FLMFB) * (XLMXB / XXMXL)
      QXM = C1 - (XXMXL / XLMXB) * (FLMFB / FFMFL)
      DXDFFF = C1 + (FFMFL / FFMFB) * QFM
      DXDFFO = DXDFFF + C2 * ((FO - FF) / FFMFB) * QFM
      TP1 = XXMXL + XLMXB
      DFDXXX = C1 + (XXMXL / TP1) * QXM
      DFDXXO = DFDXXX + C2 * ((XO - XX) / TP1) * QXM
      TP1 = DXDFFF * DFDXXX
      if ((TP1 .le. CP25) .or. (TP1 .ge. C4)) go to 330
      TP1 = DXDFFO * DFDXXO
      if ((TP1 .gt. CP25) .and. (TP1 .lt. C4)) go to 380
c
c DERIVATIVES DO NOT MATCH WELL ENOUGH
  330 continue
      if (KS .eq. 0) if (FFDFO - C1) 350, 370, 360
  340 continue
      if ((KTYP .eq. 0) .and. (TP .ge. CP75)) go to 290
      continue
      TP = C1 - TP
      if (TP .le. FFDFO) go to 280
      FFDFO = FFDFO / TP
      DIV = CP125
      go to 290
  350 continue
      DIV = CP5 * max(max(CP25, FFDFO), TP / (C1P25 - min(TP, C1)))
      go to 300
  360 continue
      DIV = min(C4, CP5 * FFDFO)
      go to 300
c INTERPOLATE WITH SECANT METHOD
  370 TP1 = -XXMXL
      go to 390
c
c DERIVATIVES MATCH UP PRETTY WELL.
  380 continue
c INTERPOLATE USING THE INVERSE QUADRATIC
      TP1 = XXMXL * (QFM * (FL / FFMFB) - C1)
  390 TP1 = (FF/FFMFL) * TP1
      KTYP = 1
c
c EXIT TO GET F(X)
  410 continue
      FL = FF
      FLMFB = FFMFL
      XLMXB = XXMXL
      XL = XX
c COMPUTE X1, INSURING THAT IT IS NOT TOO CLOSE TO THE
c ENDS OF THE INTERVAL
      XX = min(max(XL + TP1, min(XL, XO) + TOLX), max(XL, XO) - TOLX)
      X1 = XX
  420 LMODE = MODE
      return
c
  430 MACT(2) = 11*MACT(3)  - 19
  440 MACT(4) = MLOC(MACT(3)-2)
      call DMESS(MACT, MTXTAA, IDAT, FDAT)
      go to 420
c
c A CALL TO THE SUBROUTINE HAS BEEN MADE WITH MODE.NE.1
  450 IDAT(1) = MODE
      MACT(3) = 6
      MODE = 6
      if (LMODE .ne. 6) go to 430
      MACT(2) = 99
      go to 440
      end
