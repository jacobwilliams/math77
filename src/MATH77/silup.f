      subroutine SILUP (X, Y, NTAB, XT, YT, NDEG, LUP, IOPT, EOPT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2010-08-26 SILUP  Krogh  Dimension of IDAT increased to 4.
c>> 2010-03-05 SILUP  Krogh  Added more checks for NDEG too big.
c>> 2010-03-03 SILUP  Krogh  Fixed bug in flagging extrpolation.
c>> 2006-05-11 SILUP  Krogh  No flag of extrapolation on equality.
c>> 2006-04-01 SILUP  Krogh  Fixed minor bug just introduced.
c>> 2006-03-31 SILUP  Krogh  Don't flag extrap. when on end point.
c>> 2003-04-17 SILUP  Krogh  Fixed bugs in derivatives with vectors.
c>> 2000-12-03 SILUP  Krogh  Fixed so no reference to YT when LEXIT=0.
c>> 2000-12-01 SILUP  Krogh  Removed unused parameter MENTXT.
c>> 1998-01-26 SILUP  Krogh  Extrap. due to too many bad pts. flagged.
c>> 1997-09-30 SILUP  Krogh  Decremented NDEGI when LUP=4 & GETERR.
c>> 1996-04-08 SILUP  Krogh  With LUP=4 & GETERR, NDEGI was 1 too small.
c>> 1996-03-30 SILUP  Krogh  Added external statement.
C>> 1995-12-01 SILUP  Krogh  Fixed bugs connected with option 6.
C>> 1995-11-10 SILUP  Krogh  Fixed so char. data at col. 72 is not ' '.
C>> 1995-03-03 SILUP  Krogh  Bug in last change made SILUPM fail.
C>> 1994-12-22 SILUP  Krogh  Added Hermite interpolation.
C>> 1994-11-11 SILUP  Krogh  Declared all vars.
c>> 1994-10-20 SILUP  Krogh  Changes to use M77CON
c>> 1994-09-12 SILUP  Krogh  Added CHGTYP code.
c>> 1993-04-28 SILUP  Krogh  Additions for Conversion to C.
c>> 1992-05-27 SILUP  Krogh  Fixed bug in error estimate.
c>> 1992-04-08 SILUP  Krogh  Removed unused labels 510 and 2080.
c>> 1991-10-17 SILUP  Krogh  Initial Code.
c
c--S replaces "?": ?ILUP, ?ILUPM, C?ILUP, ?MESS
c
c Polynomial Interpolation with look up.
c Design/Code by  Fred T. Krogh, Jet Propulsion Laboratory, Pasadena, CA
c
c In addition to doing standard 1 dimensional interpolation, this
c subroutine supports efficient interpolation of several functions
c defined at the same values of the independent variable and supports
c SILUPM, a subroutine for doing multidimensional interpolation.  Error
c estimates, Hermite interpolation, and derivatives of the interpolant
c can be obtained via options.
c Algorithms used are described in "Efficient Algorithms for Polynomial
c Interpolation and Numerical Differentiation", by Fred T. Krogh, Math.
c of Comp. Vol. 24, #109 (Jan. 1970), pp. 185-190.
c
c     *************     Formal Arguments     ***************************
c
c X      Independent variable where value of interpolant is desired.
c Y      Value of interpolant, computed by this subroutine.  The
c        interpolant is always a piecewise polynomial.
c NTAB   Number of points in the table.
c XT     Array of independent variable values.  Must be monotone
c        increasing or monotone decreasing.  If XT(I) = XT(I+1) for some
c        I, then the XT(J)'s that are used in the interpolation will
c        either have all J's .le. I, or all J's .ge. I+1.  If the XT's
c        are equally spaced an option allows one to provide only XT(1),
c        and the increment between the XT's.
c YT     Array of dependent variable values.  Y(XT(I)) = YT(I).
c NDEG   If NDEG < 2 or odd, then it gives the degree of the polynomial
c        used.  Else the polynomial used is a linear combination of two
c        polynomials of degree NDEG, such that the resulting polynomial
c        is of degree NDEG+1 and has a continuous first derivative.
c        Typically accuracy will improve as NDEG increases up to some
c        point where it will start to get worse because of either
c        rounding errors or the inherant instability of high degree
c        polynoimial interpolation. If more than MAXDEG-th degree is
c        desired, parameter MAXDEG must be changed below.  It is
c        currently 15.  If X is so close to the end of the table that
c        the same number of points can not be selected on both sides of
c        x the degree of the interpolant is NDEG exactly.  When
c        extrapolating, the degree used is max(2, 2*(NDEG/2)), where the
c        divide is truncated to the nearest integer.
c LUP    Defines the type of look up method.  (Changed if LUP < 1.)
c    < 0   Use a sequential search starting with an index of -LUP, and
c          set LUP to -k on exit where k minimizes abs(X-XT(k)).
c    = 0   As for < 0, except start with a binary search.
c    = 1   Use a binary search.
c    = 2   Start a sequential search with an index = [1.5 +
c          (NTAB-1) * (X-XT(1)) / (XT(NTAB)-XT(1))].
c    = 3   YT(k) corresponds to XT(1) + (k-1)*XT(2), no search is needed
c          to do the look up and XT can have dimension 2.
c    = 4   Internal information connected with X-XT(k) values used in
c          in the last interpolation is reused.  (Only use if there are
c          no intervening calls to SILUP.)  No options should be
c          specified and only YT should be different.  Intended for
c          interpolating components after the first of a vector valued
c          function.
c IOPT   IOPT(1) is used to return a status as follows.
c    -10   An option index is out of range.
c    -9    NTAB is outside of allowed limits.
c    -8    NDEG is outside of allowed limits.
c    -7    LUP > 3 (use old parameters), used when not ready for it.
c    -6    Option 3 (compute derivatives), has requested more than
c          MAXDEG derivatives.
c    -5    LUP = 3 and XT(2) = 0.
c    -4    XT(1) = XT(NTAB), and NTAB is not 1.
c    -3    < 2 points available because of bad points.
c    -2    Only one table entry available, req. err. est. not computed.
c    -1    The accuracy requested was not obtained.
c     0    Nothing special to flag.
c     1    X was outside the domain of the table, extrapolation used.
c     2    NTAB is so small, it restricted the degree of the polynomial.
c
c        Starting with IOPT(2) options are specified by integers in the
c        range 0-6, followed in some cases by integers providing
c        argument(s).  Each option, together with its options its
c        arguments if any is followed in IOPT by the next option (or 0).
c     0    No more options; this must be last in the option list.
c     1    An error estimate is to be returned in EOPT(1).
c     2    (Argument: K2)  K2 gives the polynomial degree to use when
c          extrapolating.
c     3    (Argument: K3, L3) Save (k-th derivative of interpolating
c          polynomial) / (k!) in EOPT(K3+K-1) for k = 1, 2, ..., L3.
c          These values are the coefficients of the polynomial in the
c          monomial basis expanded about X.  One must have 0<L3<NDEG+1.
c     4    (Argument K4) The absolute and relative errors expected in
c          YT entries are specified in EOPT(K4) and EOPT(K4+1)
c          respectively.  The values provided here are used in
c          estimating the error in the interpolation which is stored
c          in EOPT(1).
c     5    (Argument K5, L5) Do the interpolation to the accuracy
c          requested by the absolute error tolerance specified in
c          EOPT(K5) and the relative error tolerance in EOPT(K5+1)
c          respectively.  An attempt is made to keep the final error
c          < EOPT(K5) + EOPT(K5+1) * (abs(YT(i1) +abs(YT(i2)), where
c          i1 and i2 are indices for table values close to X.
c          Interpolation is done as for the default case, except that
c          in this case NDEG gives the maximal degree polynomial to use
c          in the interpolation, and polynomial interpolation of even
c          degree can happen.  (The form that gives the C1 continuity
c          is not available in this case, and in fact continuity of the
c          interpolant itself is not to be expected.  The actual degree
c          used in doing the interpolation is stored in the space for
c          the argument L5.  An error estimate is returned in EOPT(1).
c     6    (Argument K6) Do not use point k in the interpolation if
c          YT(k) = EOPT(K6).
c     7    (Argument K7) YT(K7+i) gives the first derivative
c          corresponding to the function value in YT(i).  These
c          derivatives are to be used in doing the interpolation.  One
c          gets a continuous interpolant only for NDEG = 3, 7, 11, and
c          15.  The interpolating polynomial satisfies p(XT(i)) = YT(i),
c          p'(XT(i)) = YT(K7+i) for values of i that give values of XT
c          close to X.  (Subject to keeping within one of the same
c          number of XT's on either side of X, the maximum of |XT(i)-X|
c          is minimized.)  If NDEG is even a value of YT(i) is used
c          without using the corresponding value of YT(K7+i).
c  >253    Used for calls from SILUPM, a number of variables in the
c          common block have been set.  If this is used, it must be the
c          first option, and common variables must be set.  The rest
c          of IOPT is not examined for options.
c EOPT   Array used to return an error estimate and also used for
c        options.
c     EOPT(1)  if an error estimate is returned, this contains a crude
c              estimate of the error in the interpolation.
c
c      ************     External Procedures      ***********************
c
c R1MACH Returns system parameters.
c SMESS  Prints error messages.
c
c      ************     Variables referenced     ***********************
c
c ADJSAV Saved difference of XT values used to adjust error estimate on
c  variable order when derivatives are being computed.
c BADPT  In common CSILUP.  YT(K) is not to be used if YT(K) = BADPT,
c        and LBADPT = .true.
c CY     Array giving the YT values used to construct the interpolant.
c  Also used to store divided differences.
c R1MACH Function to get parameters of floating point arithmetic.
c SMESS  Program calling MESS to print error messages.
c DX     Array giving the differences X - XT(I), where I runs through
c  the points that are used in the interpolation.
c DXS    Saved value of DX when computing derivatives
c E1     Error estimate from one iteration ago (for variable order)
c E2     Error estimate from this iteration (for variable order)
c E2L    Used in computing error estimate for variable order.
c EBND   If estimated error is < EBND then order is suff. high.
c EBNDI  Internal value for bounding error.
c EBNDR  Used in getting part of EBND due to relative accuracy request.
c EF     Factor used in estimating errors for variable order.
c EN     Used in computing error estimate for variable order.
c EOPT   Formal argument, see above.
c EPSR   Relative error level (R1MACH(4)).
c ERRDAT Holds floating point data for error messages.
c ERREST Estimate of the error in the interpolation.
c GETERR Logical variable that is .true. if we are getting an error est.
c H      In indexed lookup contains the difference between XT values.
c I      Temporary index.
c IO     Index used in processing options.
c IDX    In common CSILUP.  Gives indices of points selected for the
c        interpolation in order selected.
c IIFLG  Internal value for IOPT(1).
c ILI    Lower (sometimes upper) bound on the XT indices that will be
c  used in the interpolation.
c INDXED Logical variable that is .true. if we have an indexed XT.
c IOPT   Formal argument, see above.
c IUI    As for ILI, except an upper (sometimes lower) bound.
c K      Index usually into YT and XT, into CY for derivatives.
c KAOS   In common CSILUP.  Keeps track of state on variable order.
c    = 0   No variable order -- this value only set in SILUPM.
c    = 1   First time
c    = 2   Second time
c    = 3   Just had an increase or a very weak decrease in the error.
c    = 4   Just had a strong decrease in the error.
c   On an error with IIFLG .lt. 0, this is set to -10 * stop level -
c   the print level.
c KDER   0 if not doing Hermite interpolation, else is < 0 if have next
c  higher order difference computed (in TP3), > 0 if it isn't.
c KEXTRP In common CSILUP.  Gives degree to use when extrapolating.
c KGO    Indicates type of interpolation as follows:
c    1     Standard polynomial interpolation.
c    2     Get error estimate after 1.
c    3     Compute an interpolant with some desired accuracy.
c    4     Compute an interpolant with a continuous derivative.
c  >10     Same as if KGO were 10 smaller, except XT values have already
c          been selected.
c KGOC   In common CSILUP.  Value saved for -KGO.  If YT contains values
c        of Y computed elsewhere in the order specified in IDX, then
c        KGOC should be set to abs(KGOC) before calling SILUP.  If -2,
c        an extra value was computed for an error estimate.
c KK     Used in selecting data points to use.
c     1  decrease ILI only
c     0  increase IUI
c    -1  decrease ILI
c   <-1, increase IUI only
c L      Temporary index used in searching the XT array.  Also one less
c  than the number of points used in the current interpolant.
c LBADPT Logical variable set = .true. if checking for bad points.
c LDER   Offset of y' values from the y values.
c LDERIV In common CSILUP. Loc. where first deriv. is stored in EOPT().
c LEXERR In common CSILUP. Loc. where absolute and relative error
c        information on Y is stored in EOPT.
c LEXIT  In common CSILUP.  Defines action after finding location in XT.
c    0     Don't compute anything for Y, just return. (Used for SILUPM.)
c    1     Usual case, just interpolate.
c   >1     Compute LEXIT-1 derivatives of the interpolant.
c LINC   Logical variable used in the sequential search.  Set .true. if
c  the values in XT are increasing with I, and set .false. otherwise.
c LNDEG  Location in IOPT to save degree when variable order is used.
c LOPT   Index of the last option.
c LUP    Formal argument, see above.
c MACT   Array used for error message actions, see SMESS for details.
c MACTV  Array used for part of error message for IOPT.
c MAXDEG Parameter giving the maximum degree polynomial interpolation
c  supported.  MAXDEG must be odd and > 2.
c MESS   Message program called from SMESS to print error messages.
c MEEMES Parameter giving value for printing an error message in MESS.
c MEIVEC Parameter giving value for printing an integer vector in MESS.
c MEMDA1 Parameter giving value for indicating value in MACT for MESS.
c MEMDA2 Parameter giving value for indicating value in MACT for MESS.
c MEMDAT Parameter giving value for specifying MACT index for MESS.
c MERET  Parameter giving value to indicate no more actions for MESS.
c METEXT Parameter giving value to tell MESS to print from MTXTAA.
c LTXTxx Parameter names of this form were generated by PMESS in making
c        up text for error messages and are used to locate various parts
c        of the message.
c MLOC   Array giving starting loctions for error message text.
c MTXTAA Character array holding error message text for MESS.
c N      Used in logic for deciding bounds.  If N = 0, ILI can not be
c  reduced further.  If N = 3*NTABI, IUI can not be increased further.
c NDEG   Formal argument, see above.
c NDEGE  Degree of polynomial to be used.  Starts out = NDEG.
c NDEGEC In common CSILUP.  Degree actually used, saved in common.
c NDEGI  Degree of polynomial up to which y & differences are computed.
c NDEGQ  Used when KGO > 10.  In this case If L .ge. NDEGQ special
c        action is needed.  (Usually means quitting.)
c NTAB   Formal argument, see above.
c NTABI  Internal value of NTAB, = number of points in XT and YT.
c PI     Array use to store interpolation coefficients
c PID    Temporary storage used when computing derivatives.
c SAVED  Logical variable set = .true. if a DX value needs to be
c  replaced and restored when computing derivatives.
c TP1    Used for temporary accumulation of values and temp. storage.
c TP2    Used for temporary storage.
c TP3    Used for divided difference when doing Hermite interpolation.
c X      Formal argument, see above.
c XI     Internal value for X, the place where interpolation is desired.
c XL     Value of "left hand" X when selecting indexed points.
c XT     Formal argument, see above.
c XU     Value of "right hand" X when selecting indexed points.
c Y      Formal argument, see above.
c YL     Last value of Y when selecting the order.
c YT     Formal argument, see above.
c YTORD  Logical variable set .true. when YT values are ordered on entry
c
c     *************     Formal Variable Declarations     ***************
c
      external         R1MACH
      integer          IOPT(*), LUP, NDEG, NTAB
      real             X, XT(*), Y, YT(*), EOPT(*)
c
c     *************     Common Block and Parameter     *****************
c
c                               MAXDEG must be odd and > 2.
      integer MAXDEG
      parameter (MAXDEG = 15)
c
      logical          GETERR
      integer          KAOS, KEXTRP, KGOC,LDERIV,LEXERR,LEXIT,NDEGEC,
     1                 IDX(0:MAXDEG+1)
      real             BADPT, DX(0:MAXDEG+1), EBND, EBNDR
      common / CSILUP / BADPT, DX, EBND, EBNDR, KAOS, KEXTRP, KGOC,
     1                  LDERIV, LEXERR,LEXIT,NDEGEC,IDX,GETERR
c
c     *************     Local Variables     ****************************
c
      logical          INDXED, LBADPT, LINC, YTORD, SAVED
      integer          I, IIFLG, ILI, IUI, IO, K, KDER, KGO, KK, L,
     1                LDER, LNDEG, LOPT, N, NDEGE, NDEGI, NDEGQ, NTABI
      real             ADJSAV, CY(0:MAXDEG+1), DXS, E1, E2, E2L, EBNDI,
     1                 EF, EM, EPSR, ERREST, H, PI(0:MAXDEG+1),
     2                 PID(0:MAXDEG), TP1, TP2, TP3, XI, XL, XU, YL
      real             R1MACH
      save             EPSR, /CSILUP/
c
c ************************ Error Message Stuff and Data ****************
c
c Parameter defined below are all defined in the error message program
c SMESS.
c
      integer MECONT,MEMDA1,MERET,MEEMES,METEXT,MEIVEC
      parameter (MEMDA1 =27)
      parameter (MECONT = 50)
      parameter (MERET =51)
      parameter (MEEMES =52)
      parameter (METEXT =53)
      parameter (MEIVEC =57)
c
      real             ERRDAT(2)
      integer MLOC(9), IDAT(4), MACT(5), MACTV(6)

c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA SILUP$B
cAB Estimated error = $F, Requested error = $F.$E
cAC NTAB = 1, no error estimate computed.$E
cAD Too many bad points, only $I point(s) available.$E
cAE XT(1) = XT(NTAB=$I) = $F ??$E
cAF LUP = 3, and XT(2) = 0.$E
cAG Order of requested derivative, $I, is > bound of $I.$E
cAH LUP > 3, with unprepared common block.$E
cAI NDEG = $I must be .le. $I or  NTAB = $I must be at $C
c   least $I.  You must either decrease NDEG, increase NTAB, $C
c   or if requesting an error estimate, turn off that request.$E
cAJ NDEG = $I is not in the allowed interval of [0, $I].$E
cAK NTAB = $I is not in the allowed interval of [1, $I].$E
cAL IOPT($I) = $I is not a valid option.$E
c   $
cAM IOPT(1:$M): $E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM
      parameter (LTXTAA=  1,LTXTAB=  8,LTXTAC= 53,LTXTAD= 92,LTXTAE=142,
     * LTXTAF=171,LTXTAG=196,LTXTAH=250,LTXTAI=290,LTXTAJ=458,
     * LTXTAK=512,LTXTAL=566,LTXTAM=  1)
      character MTXTAA(3) * (201)
      character MTXTAB(1) * (14)
      data MTXTAA/'SILUP$BEstimated error = $F, Requested error = $F.$EN
     *TAB = 1, no error estimate computed.$EToo many bad points, only $I
     * point(s) available.$EXT(1) = XT(NTAB=$I) = $F ??$ELUP = 3, and XT
     *(2) = 0.$EOrder ','of requested derivative, $I, is > bound of $I.$
     *ELUP > 3, with unprepared common block.$ENDEG = $I must be .le. $I
     * or  NTAB = $I must be at least $I.  You must either decrease NDEG
     *, increase NTAB, or if',' requesting an error estimate, turn off t
     *hat request.$ENDEG = $I is not in the allowed interval of [0, $I].
     *$ENTAB = $I is not in the allowed interval of [1, $I].$EIOPT($I) =
     * $I is not a valid option.$E'/
      data MTXTAB/'IOPT(1:$M): $E'/
c **** End of automatically generated text
c
c                      1 2 3 4      5
      data MACT / MEEMES,0,0,0, MECONT /
c                        2        3      4   5     6
      data MACTV /MEMDA1, 0, METEXT, MEIVEC, 0, MERET /

c
      data MLOC / LTXTAB, LTXTAC, LTXTAD, LTXTAE, LTXTAF, LTXTAG,
     1            LTXTAH, LTXTAI, LTXTAJ /
c
      data EPSR / 0.E0 /
c
c
c      ************     Start of executable code     *******************
c
      IIFLG = 0
      KDER = 0
      PI(0) = 1.E0
      LBADPT = .false.
      ILI = -LUP
      if (ILI .le. -4) go to 1400
      LEXERR = 0
      NTABI = NTAB
      if ((NTABI .le. 0) .or. (NTABI .gt. 9999999)) then
         IIFLG = -9
         IDAT(1) = NTABI
         IDAT(2) = 9999999
         go to 2120
      end if
      NDEGI = NDEG
      NDEGE = NDEGI
      XI = X
      KGO = 1
      IO = 1
      if (IOPT(2) .ge. 254) then
         LBADPT = IOPT(2) .eq. 255
         if (KAOS .le. 0) go to 50
         LNDEG = 0
         KGO = 3
         KAOS = 1
         NDEGI = min(MAXDEG, IOPT(3)+1)
         NDEGE = -NDEGI
         go to 60
      end if
      GETERR = .false.
      KEXTRP = -1
      LEXIT = 1
c                                      Loop to take care of options
   10 IO = IO + 1
      LOPT = IOPT(IO)
      if (LOPT .ne. 0) then
        go to (1930, 1500, 1600, 1900, 1950, 1970, 1980), LOPT
        IIFLG = -10
        IDAT(1) = IO
        IDAT(2) = LOPT
        go to 2120
      end if
   50 continue
      K = NDEGI + 1
      if (KDER .ne. 0) then
        K = K / 2
      else if (mod(NDEGI,2) .eq. 0) then
        K = K + 1
      end if
      if (GETERR) K = K + 1
      if ((NDEGI.lt.0) .or. (NDEGI.gt.MAXDEG) .or. (K.gt.NTAB)) then
         IIFLG = -8
         IDAT(1) = NDEGI
         IDAT(2) = MAXDEG
         IDAT(3) = NTAB
         IDAT(4) = K
         go to 2120
      end if
      if (KGO .eq. 1) then
         if (NDEGI .ge. 2) then
            if (mod(NDEGI, 2) .eq. 0) then
               if (KDER .eq. 0) then
                  NDEGE = NDEGE + 1
                  KGO = 4
               end if
            end if
         end if
      end if
   60 if (KEXTRP .lt. 0) then
         KEXTRP = max(NDEGE-1, min(NDEGE, 2))
         if (KEXTRP .lt. 0) KEXTRP = NDEGI
      end if
      if (ILI .gt. 0) go to 200
      if (ILI + 2) 1300, 1200, 100
c
c                                      Binary search, then sequential
  100    continue
c                         In binary search XT(ILI) .le. XI .le.  XT(IUI)
c                         or we are extrapolating
            ILI = 1
            IUI = NTABI
            if (XT(NTABI) - XT(1)) 110, 1220, 130
  110       ILI = NTABI
            L = 1
  120       IUI = L
  130       L = (IUI - ILI) / 2
            if (L .eq. 0) go to 210
            L = ILI + L
            if (XT(L) .gt. XI) go to 120
            ILI = L
            go to 130
c
c                                      Sequential search, then exit
  200    continue
            if (ILI .gt. NTABI) go to 100
            if (XT(NTABI) .eq. XT(1)) go to 1220
  210       INDXED = .false.
            LINC = XT(NTABI) .gt. XT(1)
            if ((XT(ILI) .gt. XI) .eqv. LINC) go to 230
  220       if (ILI .eq. NTABI) go to 1230
            ILI = ILI + 1
            if ((XT(ILI) .lt. XI) .eqv. LINC) go to 220
            N = 2*ILI
            if (abs(XT(ILI-1)-XI) .lt. abs(XT(ILI) - XI)) then
               ILI = ILI - 1
               N = N - 1
            end if
            go to 240
  230       if (ILI .eq. 1) go to 1230
            ILI = ILI - 1
            if ((XT(ILI) .gt. XI) .eqv. LINC) go to 230
            N = 2*ILI + 1
            if (abs(XT(ILI+1)-XI) .lt. abs(XT(ILI) - XI)) then
               ILI = ILI + 1
               N = N + 1
            end if
  240       if (LUP .le. 0) LUP = -ILI
  250       DX(0) = XI - XT(ILI)
c                                  Get bounding indices and interpolate
  260       IUI = ILI
            K = ILI
            L = -1
            if (LEXIT .eq. 0) then
               NDEGI = 0
               if (GETERR) then
                  if (KGO .ne. 3) then
                     if (KGO .eq. 1) NDEGE = NDEGE + 1
                     KEXTRP = min(KEXTRP+1, NDEGE)
                  else
                     NDEGE = -NDEGE
                  end if
               end if
            end if
c                                  Just got index for next XT
  270       if (LEXIT .eq. 0) then
               L = L + 1
               IDX(L) = K
               go to 350
            end if
            TP2 = YT(K)
            if (LBADPT) then
c                                  Check if point should be discarded
               if (TP2 .eq. BADPT) then
                  if (IUI .ne. ILI) then
                     if (abs(KK) .eq. 1) then
                        N = N - 1
                     else
                        N = N + 1
                     end if
                  end if
                  go to 1020
               end if
            end if
  280       L = L + 1
            IDX(L) = K
c
  290       if (KDER .ne. 0) then
c                           Hermite interpolation
               KDER = -KDER
               if (KDER .lt. 0) then
                  TP3 = YT(LDER + K)
                  if (L .eq. 0) then
                     TP1 = TP2
                  else
                     do 300 I = 1, L
                        TP2 = (TP2 - CY(I-1)) / (DX(I-1) - DX(L))
                        TP3 = (TP3 - TP2) / (DX(I-1) - DX(L))
  300                continue
                     PI(L) = PI(L-1) * DX(L-1)
                     TP1 = TP1 + PI(L) * TP2
                  end if
               else
                  DX(L) = DX(L-1)
                  PI(L) = PI(L-1) * DX(L)
                  TP2 = TP3
                  TP1 = TP1 + PI(L) * TP2
               end if
            else if (L .eq. 0) then
                  TP1 = TP2
            else
               PI(L) = PI(L-1) * DX(L-1)
               if (L .le. NDEGI) then
c                                 Get divided differences & interpolate.
                  do 320 I = 1, L
                     TP2 = (TP2 - CY(I-1)) / (DX(I-1) - DX(L))
  320             continue
                  TP1 = TP1 + PI(L) * TP2
               end if
            end if
            CY(L) = TP2
  350       if (L .lt. NDEGE) go to 1000
  360       if (LEXIT .eq. 0) go to 2110
            go to (500, 600, 900, 800), KGO
            if (L .ge. NDEGQ) go to (500,600,900,800), KGO-10
c                        Already got the points selected.
  400       L = L + 1
            if (YTORD) then
               TP2 = YT(L+1)
            else
               K = IDX(L)
               TP2 = YT(K)
            end if
            go to 290
c Got simple interpolated value.
  500       Y = TP1
            if (.not. GETERR) go to  2000
            NDEGI = NDEGI + 1
            KGO = KGO + 1
            if (NDEGE .ge. 0) go to 1000
            go to 400
c Got info. for error estimate.
  600       if (L .eq. 0) then
               IIFLG = -2
            else
               ERREST=1.5E0*(abs(TP1-Y)+.03125E0*abs(PI(L-1)*CY(L-1)))
               L = L - 1
            end if
            go to  2000
c C1 interpolant.
  800       do 810 I = 1, L-1
               TP2 = (TP2 - CY(I-1)) / (DX(I-1) - DX(L))
  810       continue
            TP2 = (TP2 - CY(L-1)) / (DX(0) - DX(1))
            CY(L) = TP2
            PI(L) = PI(L-1) * DX(0)
            Y = TP1 + PI(L) * TP2
            if (GETERR) ERREST = 1.5E0 * abs(PI(L-1)) * abs(((DX(L-1) *
     1         (DX(0) - DX(1)) / (DX(L-1) - DX(L)) - DX(0)) * TP2) +
     2         abs(.03125E0*CY(L-1)))
            go to 2000
c    Variable order, check estimated error and convergence.
  900       continue
            if (KAOS .ge. 3) go to 930
            if (KAOS .eq. 2) go to 920
c                                        First time
            if ((KDER.ne.0) .and. (LEXIT.gt.1) .and. (L.eq.0)) go to 970
            KAOS = 2
            E2L = abs(TP1)
            E2 = EBND + 1.E30
            go to 970
c                                        Second time
  920       KAOS = 4
            EBNDI = .66666E0*(EBND + EBNDR*(abs(CY(0))+abs(YT(IDX(1)))))
            EF = DX(0)
            if (LEXIT .gt. 1) then
               EF = DX(L) - DX(0)
               ADJSAV = EF
            end if
            E2 = abs(EF * TP2)
            EM = .75E0
            go to 950
c                                        Usual case
  930       E2L = E2
            E1 = E2L * (5.E0 * EM / real(L))
            EF = EF * DX(L-1)
            E2 = abs(EF*TP2)
            EM = 0.5E0 * EM + E2 / (E2L + E2 + 1.E-20)
            if (E2 .ge. E1) then
               if (KAOS .eq. 3) then
c                          Apparently diverging, so quit.
                  L = L - 1
                  go to 960
               else
                  KAOS = 3
               end if
            else
               KAOS = 4
            end if
  950       YL = TP1
            if (E2L + E2 .gt. EBNDI) then
               if ((L+NDEGE .lt. 0) .and. (IIFLG .ne. 2)) go to 970
               if (KGO .eq. 13) then
c                                 May need early exit to get next point.
                  IOPT(2) = 0
                  if (L .lt. NDEG) go to 2110
               end if
            end if
  960       TP2 = 1.5E0
            if (LEXIT .gt. 1) then
               if (KDER .eq. 0) TP2 = 1.5E0 * abs(DX(0) / ADJSAV)
            end if
            ERREST = TP2 * (E2  + .0625E0 * E2L)
            if (LNDEG .ne. 0) IOPT(LNDEG) = L
            Y = YL
            go to 2000
  970       if (KGO .gt. 10) go to 400
c
 1000       if (KDER .lt. 0) go to 280
 1020       KK = min(N - IUI - ILI, 2) - 1
c In this section of code, KK=: 1, decrease ILI only; 0, increase IUI;
c -1, decrease ILI; and <-1, increase IUI only
            if (abs(KK) .eq. 1) then
               ILI = ILI - 1
               K = ILI
               if (ILI .ne. 0) then
                  if (INDXED) then
                     XL = XL + H
                     DX(L+1) = XL
                     go to 270
                  end if
                  DX(L+1) = XI - XT(K)
                  if (XT(ILI+1) .ne. XT(ILI)) go to 270
               end if
               if (KK .eq. 1) go to 1100
               N = 0
c                If all points will be on one side, flag extrapolation.
c               if (L .le. 0) go to 1250
            else
               IUI = IUI + 1
               K = IUI
               if (IUI .le. NTABI) then
                  if (INDXED) then
                     XU = XU - H
                     DX(L+1) = XU
                     go to 270
                  end if
                  DX(L+1) = XI - XT(K)
                  if (XT(IUI-1) .ne. XT(IUI)) go to 270
               end if
               if (KK .ne. 0) go to 1100
               N = 3*NTABI
c                If all points will be on one side, flag extarpolation.
c               if (L .le. 0) go to 1250
            end if
            if (KGO .lt. 4) go to 1020
c If we can't get the same number of points on either side, the
c continuous derivative case becomes standard interpolation.
            KGO = 1
            NDEGE = NDEGE - 1
            if (L .lt. NDEGE) go to 1020
            go to 360
c                                     No more data accessible.
 1100       if (L .le. 0) then
c                             Too many bad points, couldn't find points.
                Y = TP1
                IDAT(1) = L + 1
                IIFLG = -3
                go to 2110
            end if
            NDEGI = min(NDEGI, L)
            NDEGE = 0
            IIFLG = 2
            go to 360
c
c                                     Secant start, then use sequential
 1200    continue
            if (XT(1) .eq. XT(NTABI)) go to 1220
            ILI = max(1, min(NTABI, int(1.5E0+real(NTABI-1)*(XI-XT(1))/
     1         (XT(NTABI) - XT(1)))))
         go to 210
c
c                         Special cases
c                                  1 entry in XT
 1220    ILI = 1
         IUI = 1
         KGO = 1
         K = 1
         if (NDEGE .ne. 0) IIFLG = 2
         NDEGE = 0
         if (NTABI .eq. 1) go to 250
c                         Error -- XT(1) .eq. XT(NTAB), and NTAB .ne. 1
         IIFLG = -4
         IDAT(1) = NTABI
         ERRDAT(1) = XT(1)
         KGO = 0
         go to 2120
c                  Check if really extrapolating
 1230    N = 6 * (ILI - 1)
         if ((XI - XT(1)) * (XT(NTABI) - XI) .ge. 0.E0) go to 240
c                                  Extrapolating
 1250    IIFLG = 1
         if (KGO .ge. 4) KGO = 1
         NDEGI = KEXTRP
         NDEGE = sign(NDEGI, NDEGE)
         if (INDXED) go to 260
         go to 240
c
c                                      Index search, then exit
 1300    continue
         INDXED = .true.
         H = XT(2)
         if (H .eq. 0) then
            IIFLG = -5
            go to 2120
         end if
         TP1 = 1.E0 + (XI - XT(1)) / H
         ILI = min(max(1, nint(TP1)), NTABI)
         XU = (TP1 - real(ILI)) * H
         XL = XU
         DX(0) = XU
         N = ILI + ILI
         if (TP1 .gt. real(ILI))  N = N + 1
         if ((XI - XT(1)) * ((XT(1) + H*real(NTABI-1) - XI)) .ge. 0.E0)
     1     go to 260
         N = 6 * (ILI - 1)
         go to 1250
c
c                                 Already set up
 1400    KGO = KGOC
         if (KGO .eq. 0) then
            IIFLG = -7
            go to 2120
         end if
         YTORD = KGO .gt. 0
         KGO = abs(KGO)
         if (KGO .lt. 10) KGO = KGO + 10
         if (KGO .eq. 12) KGO = 11
         NDEGI = NDEGEC
         NDEGQ = NDEGI
         if (KGO .eq. 13) NDEGQ = 0
         NDEGE = -NDEGI
         L = -1
         if (KGO .eq. 14) NDEGI = NDEGI - 1
         go to 400
c
c                                 Set number of points for extrapolation
 1500    continue
            I = I + 1
            KEXTRP = min(IOPT(I), NDEGE)
            go to 10
c
c                                 Get derivatives of interpolant
 1600    continue
            I = I + 2
            LDERIV = IOPT(I-1)
            LEXIT = IOPT(I) + 1
            go to 10
c
c                                     Get expected errors in YT.
1900     continue
            I = I + 1
            LEXERR = IOPT(I)
c
c                                     Set to get error estimate.
 1930    continue
            GETERR = .true.
            go to 10
c
c                                     Set for automatic order selection.
 1950    continue
            IO = IO + 2
            LNDEG = IO
            EBND = max(0.E0, EOPT(IOPT(IO-1)))
            EBNDR = max(0.E0, EOPT(IOPT(IO-1)+1))
            KGO = 3
            KAOS = 1
            NDEGI = min(MAXDEG, NDEGE+1)
            NDEGE = -NDEGI
         go to 1930
c
c                                    Set to ignore special points
 1970    continue
            LBADPT = .true.
            IO = IO + 1
            BADPT = EOPT(IOPT(IO))
            go to 10
c
c                                    Set up for Hermite interpolation.
 1980    continue
         IO = IO + 1
         LDER = IOPT(IO)
         KDER = abs(LDER)
         go to 10
c
c                                    Put any new options just above here
c                    End of the loop
c
 2000 if (LEXIT .lt. 2) go to 2100
c                       Compute derivatives of Y
      SAVED = mod(KGO, 10) .eq. 4
      if (SAVED) then
         DXS = DX(L-1)
         DX(L-1) = DX(0)
      end if
      N = LEXIT - 1
      if (N .gt. L) then
        if (N .gt. MAXDEG) then
           IDAT(1) = N
           IDAT(2) = MAXDEG
           N = MAXDEG
           IIFLG = -6
        end if
        do 2020 I = L+1, N
           EOPT(I+LDERIV-1) = 0.E0
 2020   continue
        N = L
      end if
      TP1 = CY(1)
      PID(0) = 1.E0
      do 2030 I = 1, L-1
         PID(I) = PI(I) + DX(I) * PID(I-1)
         TP1 = TP1 + PID(I) * CY(I+1)
 2030 continue
      EOPT(LDERIV) = TP1
      do 2070 K = 2, N
         TP1 = CY(K)
         do 2060 I = 1, L-K
            PID(I) = PID(I) + DX(I+K-1) * PID(I-1)
            TP1 = TP1 + PID(I) * CY(I+K)
 2060    continue
         EOPT(K+LDERIV-1) = TP1
 2070 continue
      if (SAVED) DX(L-1) = DXS
c                                    Save info. and return
 2100 continue
      if (GETERR) then
         if (EPSR .eq. 0.E0) then
            EPSR = R1MACH(4)
         end if
         TP1 = EPSR
         TP2 = 0.E0
         if (LEXERR .ne. 0) then
            TP2 = max(0.E0, EOPT(LEXERR))
            TP1 = max(TP1, EOPT(LEXERR+1))
         end if
         if (IIFLG .eq. 2) ERREST = 32.E0 * ERREST
         EOPT(1) = ERREST + TP2 + TP1*(abs(CY(0)) + abs(DX(0)*CY(1)))
         if ((KGO .eq. 3) .or. (KGO .eq. 13)) then
            if (EBNDI .ne. 0.E0) then
               if (EOPT(1) .gt. EBND) then
                  ERRDAT(1) = EOPT(1)
                  ERRDAT(2) = EBND
                  IIFLG = -1
               end if
            end if
         end if
      end if
 2110 KGOC = -KGO
      NDEGEC = L
 2120 IOPT(1) = IIFLG
      if (IIFLG .ge. 0) return
c Take care of error messages, IIFLG < -2 should stop.
      MACT(2) = 88
      if (IIFLG .lt. -1) KAOS = -MACT(2)
      if (IIFLG .ge. -3) MACT(2) = min(26, 24 - IIFLG)
      if (IOPT(2) .ge. 254)  then
         if (IIFLG .eq. -1) return
         MACT(2) = 28
      end if
      MACT(3) = -IIFLG
      MACT(4) = MLOC(-IIFLG)
      call SMESS(MACT, MTXTAA, IDAT, ERRDAT)
      MACTV(2) = IO
      MACTV(5) = IO
      K = 1
      if (IO .eq. 1) K = 6
      call MESS(MACTV(K), MTXTAB, IOPT)
      return
c                       End of Interpolation routine -- SILUP
      end
