      subroutine SILUPM (NDIM,X,Y,NTAB,XT,YT,NDEG,LUP,IOPT,EOPT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2006-03-28 SILUPM  Krogh  NDEG odd was giving bad error est.
c>> 1999-07-17 SILUPM  Krogh  Changed MAXDIM to 10.
c>> 1997-09-30 SILUPM  Krogh  Fixed bug on setting NT for ragged case.
C>> 1995-12-13 SILUPM  Krogh  Fixed bad Y value when getting derivative.
C>> 1995-12-04 SILUPM  Krogh  Set so SILUP errors keep stop/print level.
C>> 1995-11-10 SILUPM  Krogh  Fixed so char. data at col. 72 is not ' '.
C>> 1994-11-11 SILUPM  Krogh   Declared all vars.
c>> 1994-10-20 SILUPM  Krogh  Changes to use M77CON
c>> 1994-09-12 SILUPM  Krogh  Added CHGTYP code.
c>> 1993-04-28 SILUPM  Krogh  Additions for Conversion to C.
c>> 1992-04-08 SILUPM  Krogh  Removed unused label 350, add ID4.
c>> 1991-10-17 SILUPM  Krogh  Initial Code.
c
c--S replaces "?": ?ILUP, ?ILUPM, C?ILUP, ?MESS
c
c Multidimensional Polynomial Interpolation with Look Up
c Design/Code by Fred T. Krogh, Jet Propulsion Laboratory, Pasadena, CA
c Last Revision: March 1, 1991
c
c This subroutine works by making multiple calls to SILUP.  See comments
c there for the kind of look up options and the kind of interpolation
c options available.
c
c *****************     Formal Arguments     ***************************
c
c If the data is not defined on a grid of values of the independent
c variables, the table is said to be ragged.  In order to describe the
c situation for ragged tables, define
c   r = index of first NTAB(i) such that NTAB(i) < 0.
c   e = -NTAB(r) if r is defined, else e = 1000.
c If e = 1000, data is on a grid and one can ignore most of what is said
c in connection with ragged tables.  In the case of ragged tables,
c either r = NDIM, or for i .ge. r, NTAB(i) = -(i-1).
c Below we refer to lexicographic order as the order in which certain
c data is stored.  Let i.1, i.2, ... i.NDIM be indices associated with
c dimensions 1, to NDIM.  Data, D, which depends on these indices, is
c said to be in lexicographic order if for all valid values of the
c indices the data is stored in consecutive location in memory, and data
c for j.1, ..., j.NDIM precedes data for i.1, ..., i.NDIM if j.1 < i.1,
c and in the case of equality if j.2 < i.2, etc.  Thus for fixed values
c of the first NDIM-1 indices, successive values of the last index will
c be in successive memory locations.
c
c NDIM   Number of dimensions for the independent variable (1<NDIM<10).
c        The upper limit is artificial to catch bad values.
c X      Vector of dimension at least NDIM giving the place where the
c        value of the interpolant is desired.
c Y      Value of the interpolant.  The interpolating function is always
c        a piecewise polynomial of NDIM variables.
c NTAB   Defines organization of the table.  In the case of a grid,
c        NTAB(1 to NDIM) contain n.1, ..., n.NDIM, the number of points
c        with respect to the various dimensions.  NTAB(NDIM+1) must be
c        0 initially.  On the first call it is set to e as defined
c        above.  Also on the first call, values of NTAB(NDIM+2:2*NDIM+1)
c        are set to values that are used to find the XT values to pass
c        to SILUP.  In the case of ragged tables more space is required.
c        In this case NTAB must have dimension > 3*NDIM + 1 + space
c        needed to store information on the raggedness.  Information for
c        each dimension with tabular information that depends on indices
c        selected for earlier dimensions starts with a 0, then the
c        number of table entries for the first value of the indices from
c        lower dimensions, then the number of entries for the next value
c        of the indices, etc.  Here "first" and "next" means with the
c        indices ordered lexicographically, that is indices from the
c        lowest dimension vary first, and those from the highest
c        dimension last.  Thus if (j,i) represent indices from the
c        second and the first dimension, the order is (1,1), (1,2), ...,
c        (2,1), (2,2), ....  Information on raggedness is supplied first
c        for the first ragged dimension, then for the next, etc.
c        The 0 starting information for one dimension must follow
c        immediately after the last item from the previous dimension.
c        A -1 must follow the information on raggedness for the last
c        dimension.  This is used to enhance the error checks.
c XT     An array giving values of the independent variable where Y is
c        is known.  The organization of XT is defined by NTAB.
c        If the values of Y are known on a grid of points, this
c        organization is simple.
c YT     Array of dependent variable values. In the case of a grid, one
c        can think of YT as an NDIM dimensional array with YT(I(NDIM),
c        I(NDIM-1), ..., I(1)) being the value of Y at X = (XT(I(1)),
c        XT(I(2)), ..., XT(I(NDIM))).  In the case of general ragged
c        tables (as for a grid too), the YT values are stored in
c        lexicographic order.
c NDEG   NDEG(i) defines the nominal degree of the polynomial to be used
c        in the ith dimension, with definition just like that for NDEG
c        in SILUP.
c LUP    LUP(i) defines the type of look up method for the ith dimension
c        exactly as LUP does in SILUP.
c IOPT   IOPT(1) is used to return a status.  Values are as for SILUP,
c        with the addition of
c   - 1    Error estimate is > that requested error.
c   - 2    Bad value for NDIM.
c   - 3    Bad value for LUP(i).
c   - 4    Bad specification for a ragged table.
c   - 5    Ragged table does not start with a 0.
c   - 6    Bad value inside a ragged table.
c   - 7    Bad value at end of ragged table.
c   - 8    Bad option index.
c   - 9    In some dimension, the first and last XT values are equal.
c   -10    Too many derivatives requested.
c   -11    Bad value for number of derivatives in some dimension.
c   -12    Problem with storage use in EOPT.
c   <-20   Had an error in SILUP.  The value is the value returned
c          by SILUP - 20.
c
c        IOPT(2) gives the dimension of EOPT.  In addition to the first
c        location and the locations in EOPT used for options, SILUPM
c        needs a contiguous block of storage in EOPT of length >
c        2*(2*NDIM - 1 + sum of NDEG(i), i = 1 to NDIM - 1).  If
c        derivatives are being computed even more space is needed as is
c        described with option 3 below.
c        To get a message printed of the space required, set IOPT(2)=0.
c
c        IOPT(k), k > 3 is used for the specifiation of options.
c        Options are as follows:
c     0  End of the option list, must be the last item in IOPT.
c     1  An error estimate is to be returned in EOPT(1)
c     2  (Argument K2) K2 is a vector of length NDIM whose i-th entry
c        gives the polynomial degree to use when extrapolating with
c        respect to the i-th dimension.  The default for the i-th entry
c        is 2 * max(1, NDEG(i)/2)).
c     3  (Arguments K3, L3, M3)  Let D(i.1, i.2, ..., i.NDIM) denote the
c        result of differentiating the interpolating polynomial i.j
c        times with respect to i.j, j = 1, 2, ..., NDIM.  The i.j
c        satisfy the restriction: sum (i.j) .le. L3, and 0 .le. i.j .le.
c        M3.j, where M3 is a vector of length NDIM,  0 .le. M3.j .le.
c        min(NDEG(j), L3).  Subject to these restrictions, the D's are
c        stored in lexicographic order of i.NDIM, ..., i.1 starting in
c        EOPT(K3).  Thus for example if NDIM = 2, L3=2, and
c        M3.1 = M3.2 = 1, then EOPT(K3) = D(1, 0), EOPT(K3+1) = D(0, 1),
c        and EOPT(K3+2) = D(1, 1).  If L3 had been 1, nothing would be
c        stored in EOPT(K3+2).
c        Extra storage in EOPT is required when this option is used, as
c        mentioned in the description of IOPT(2).  The amount needed is
c        difficult to state for the general case.  Let n.j denote the
c        total number of derivatives that must be computed in dimension
c        j in order to get the final derivatives in dimension 1.  A
c        recurrence for computing the n.j is given in the subroutine
c        write-up.  The extra storage required is = sum for j = 2, NDIM
c        of (NDEG(j-1) + 2) * n.j
c        If one does not want to deal with the complexities of figuring
c        out the storage, set K3 = 0, and a suggested organization will
c        be printed out and the program stopped.  Or, set K3 = -1, and
c        the program will use the location it computes in the case K3 =
c        0, and return the value used in IOPT where K3 was stored
c        initially.
c     4  (Argument K4) The absolute and relative errors expected in YT
c        entries are specified in EOPT(K4) and EOPT(K4+1) respectively.
c        The values provided here are used in estimating the error in
c        the interpolation.  An error estimate is returned in EOPT(1).
c     5  (Argument K5) Do the interpolation to the accuracy requested
c        by the absolute error tolerance specified in EOPT(K5).  An
c        attempt is made to keep the final error < EOPT(K5).  Standard
c        polynomial interpolation is done, but here NDEG() gives the
c        maximum polynomial degree to use in the interpolation.  If
c        EOPT(K5) .le. 0., IOPT(1) is not set to -1, and no error
c        message is generated due to an unsatisfied accuracy request.
c        An error estimate is returned in EOPT(1).
c     6  (Argument K6) Do not use a point in the interpolation if the
c        corresponding value of YT = EOPT(K6).
c EOPT   Array used to return an error estimate, and used for options.
c     EOPT(1)  if an error estimate is returned, this contains a crude
c              estimate of the error in the interpolation.
c
c *****************     Variables Referenced     ***********************
c
c ABS    Fortran intrinsic -- absolute value.
c BADPT  In common CSILUP, see documentation in SILUP.
c DX     In common CSILUP, see documentation for SILUP.
c EBND   In common CSILUP, see documentation for SILUP.
c EBNDR  In common CSILUP, see documentation for SILUP.
c EOPT   Formal argument, see above.
c ERRDAT Place to store floating point for error messages.
c ERRSAV Place for saving error estimates.
c GETERP Usual value for GETERR.
c GETERR In common CSILUP, see documentation for SILUP.
c I      Temporary index.
c ID     Current place to pick up derivatives from higher dimension in
c        order to interpolate derivative in a lower dimension.
c ID4    = 4 defined in data.  To avoid passing literal to SILUP.
c IDX    In common CSILUP, see documentation for SILUP.
c IDXBAS Base location for storing values of DX in EOPT.  ( =IOPT(2)+1)
c        The first DX is stored in EOPT(IDXBAS+1).
c IEEOPT Index for absolute and relative error in EOPT.
c IIFLG  Value of flag to be returned in IOPT(1).
c IMAXD  IOPT(IMAXD+KDIM) gives the maximum derivative to be computed
c        with respect to dimension KDIM.
c INTCHK Array used for checking use of optional space in EOPT.
c IOP2N  Value to store in IOPTI(2) when interpolating in the last
c        dimension.  = 255 if bad points possible, else = 254.
c IOPT   Formal argument, see above.
c IOPTI  Internal array used to pass options to SILUP.
c IP     Array used to compute current indices into XT and YT.
c IPRAG  Part of a term used in computing IP when KDIM>IRAG, and
c        NTAB(KDIM) > 0.
c IRAG   = index of first ragged table, = 1000 if table not ragged.
c IX     Current point index w.r.t. the current dimension.
c IXT    Pointer to XT information for the current dimension.
c IYBAS  One less than first location for storing values interpolated
c        for y in EOPT.
c IYDSAV As for IYSAV below, but for saving a derivative value.
c IYSAV  Location in EOPT for saving interpolated value.
c IYT    Location in EOPT where vector of interpolated Y's are stored.
c J      Index of a 0 in NTAB which is followed by user information
c        specifying the number of data points as a function of
c        previously used values from XT.
c K      Temporary index.
c KAOS   In common CSILUP.   Keeps track of state on variable order.
c    = 0   No variable order -- this value only set in SILUPM.
c    = 1   First time
c    = 2   Second time
c    = 3   Just had an increase or a very weak decrease in the error.
c    = 4   Just had a strong decrease in the error.
c   On an error with IIFLG .lt. 0, in SILUP this is set to -10 * stop
c   level - the print level.
c KC     Array used for counters for computing derivatives.
c KDIM   Index of dimension currently active.
c KEXTRP In common CSILUP, see documentation in SILUP.
c KGOC   In common CSILUP, see documentation for SILUP.
c KL     Largest total derivative as starting from the highest dimension
c        and working to dimension 1 in getting storage needed for
c        derivatives.
c KPTD   KPTD(KDIM) = pointer to extra derivative information for
c        dimension KDIM.  KPTD(KDIM-1) is place to store deriv.
c        values for dim. KDIM.
c L      Temporary index.
c LCKEND End of space used for checking storage in EOPT.
c LDERIV In common CSILUP, see documentation for SILUP.
c LERTMP Location in EOPT where temp. error info. is stored (2 values).
c LEXERR In common CSILUP, see documentation for SILUP.
c LEXIT  In common CSILUP.  Defines action after finding location in XT.
c    0     Don't compute anything for Y, just return. (Used for SILUPM.)
c    1     Usual case, just interpolate.
c   >1     Compute LEXIT-1 derivatives of the interpolant.
c LI     Last location examined in IOPT().
c LICINF Base for indexing information that depends on the current
c        point index in the current dimension.
c LIDX   Array containing indices of the points selected for all
c        dimensions up to the current one.  Also used in computing
c        KPTD.
c LIDXSZ Parameter giving the amount of space allocated for LIDX.
c LINFO  LINFO(KDIM) contains a pointer to the index currently being
c        worked on in dimension KDIM.
c LINFO0 LINFO0(KDIM) gives value to use for LICINF when in dim. KDIM, =
c        1 + sum of (2 + NDEG(J)) for J = 1, 2, ..., KDIM-1.
c LKGOC  LKGOC(KDIM) contains value of KGOC for interp. in dim. KDIM.
c LNDEG  LNDEG(KDIM) contains value of NDEG for interp. in dim. KDIM.
c LOPT   Value from current location in IOPT.
c LT     As much of NTAB as is known to be safe for printing error info.
c LTXTxx Parameter names of this form were generated by PMESS in making
c        up text for error messages and are used to locate various parts
c        of the message.
c LUP    Formal argument, see above.
c LUPC   Value passed to SILUP for LUP, the look up method.
c MACT   Array giving actions for printing error messages, see MESS.
c MAX    Fortran intrinsic -- maximum
c MAXDEG Parameter -- Gives maximum degree of polynomial interpolation.
c MAXDIM Paramter giving the maximum number of dimensions allowed.
c MECONT Parameter telling MESS an error message is to be continued.
c MEEMES Parameter telling MESS to print an error message.
c MEIVEC Parameter telling MESS to print an integer vector.
c MEFVEC Parameter telling MESS to print an floating point vector.
c MEMDA1 Parameter telling MESS that next item is integer data to be
c        made available for output.
c MERET  Parameter telling MESS that this ends the error message.
c MESS   Subroutine for printing error messages.
c METEXT Parameter telling MESS that data from MTXTAA is to printed.
c MLOC   Array giving locations of start of text for error messages.
c MTOTD  Maximum order of total derivative to be computed.
c MTXTAx Character arrays holding error message text for MESS.
c MXD    Upper bound on number of derivatives to compute when computing
c        derivatives based on information from higher dimensions.
c NDEG   Formal argument, see above.
c NDEGEC In common CSILUP, see documentation for SILUP.
c NDIM   Formal argument, see above.
c NDIMI  Internal value for NDIM = number of dimensions.
c NT     Number of points for the current call to SILUP.
c NTAB   Formal argument, see above.
c NTABM  = NTABXT + NDIMI -- NTAB(NTABM+I) is 1 + the index of the last
c        word in NTAB required for ragged table storage through dim. I.
c NTABXT = NDIMI+1 -- NTAB(NTABXT) = index of first ragged table, =1000
c        if there is no ragged table.  NTABN(NTABXT+I) = base address
c        accessing XT information.
c NUMD   NUMD(j) gives the number of different derivatives for
c        dimension j.
c OPTCHK Subroutine for checking on space allocation for options.
c SETEXT If not 0, gives IOPT(SETEXT+KDIM) gives the value to use for
c        KEXTRP in dimension KDIM.
c SILUP  One dimensional interpolation subroutine.
c SMESS  Calls MESS and prints floating data in error messages.
c STOD   Index in EOPT where derivatives are to be stored.
c X      Formal argument, see above.
c XT     Formal argument, see above.
c Y      Formal argument, see above.
c YT     Formal argument, see above.
c
c     *************     Formal Variable Declarations     ***************
c
      integer          NDIM, NTAB(*), NDEG(*), LUP(*), IOPT(*)
      real             X(NDIM), Y, XT(*), YT(*), EOPT(*)
c
c     *************     Common Block and Parameter     *****************
c
c                               MAXDEG must be odd and > 2.
      integer MAXDEG
      parameter (MAXDEG = 15)
c
      logical          GETERR, GETERP
      integer          KAOS, KEXTRP, KGOC,LDERIV,LEXERR,LEXIT,NDEGEC,
     1                 IDX(0:MAXDEG+1)
      real             BADPT, DX(0:MAXDEG+1), EBND, EBNDR
      common / CSILUP / BADPT, DX, EBND, EBNDR, KAOS, KEXTRP, KGOC,
     1                  LDERIV, LEXERR,LEXIT,NDEGEC,IDX,GETERR
c
c     *************     Local Variables     ****************************
c
      INTEGER MAXDIM, LIDXSZ
      parameter (MAXDIM = 10)
      parameter (LIDXSZ = MAXDIM*(2 + MAXDEG))
      integer          IP(MAXDIM), LINFO(0:MAXDIM),
     1                 LINFO0(0:MAXDIM), LKGOC(MAXDIM), LNDEG(MAXDIM)
      integer          LIDX(LIDXSZ)
      integer          I, IDXBAS, IEEOPT, IIFLG, IMAXD, INTCHK(0:20),
     1 IOP2N, IOPTI(3), IPRAG, IRAG, IX, IYBAS, IYDSAV, IYSAV, IYT, J,
     2 K, KC(MAXDIM), KDIM, KPTD(0:MAXDIM), L, LERTMP, LI, LT,
     3 LICINF, LOPT, LUPC, MTOTD, NDIMI, NT, NTABM, NTABXT,
     4 NUMD(MAXDIM), SETEXT, STOD
      integer ID4, LCKEND, IXT, ID, MXD, KL
      equivalence (KPTD(0), STOD)
      real             ERRSAV(MAXDIM)
c
c ************************ Error Message Stuff and Data ****************
c
c Parameter defined below are all defined in the error message program
c MESS and SMESS.
c
      integer MEMDA1, MECONT, MERET, MEEMES, METEXT, MEIVEC, MEFVEC
      parameter (MEMDA1 =27)
      parameter (MECONT =50)
      parameter (MERET =51)
      parameter (MEEMES =52)
      parameter (METEXT =53)
      parameter (MEIVEC =57)
      parameter (MEFVEC =61)
c
      integer MLOC(12)
      integer MACT(12)
      real             ERRDAT(2)
c
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA SILUPM$B
cAB Estimated error = $F, Requested error = $F.$E
cAC Need NDIM in interval [1, $I] but NDIM = $I.$E
cAD LUP($I) = $I; it should be < 4.$E
cAE Ragged table badly specified in dimension $I.$E
cAF Start of ragged table, NTAB($I) = $I; it must be 0.$E
cAG Middle of ragged table, NTAB($I) = $I; it must be >0.$E
cAH End of ragged table, NTAB($I) = $I; it must be -1.$E
cAI IOPT($I) = $I is not a valid option.$E
cAJ In dimension $I, first XT = XT($I) = last XT = XT($I) = $F.$E
cAK A total of only $I derivatives allowed, but $I requested.$E
cAL Need derivative order in [0, $I], but it is = $I.$E
cAM Previous error from SILUP occurred in dimension $I.$E
c   $
cAN LUP(1:$M):$B
c   $
cAO NDEG(1:$M):$B
c   $
cAP NTAB(1:$M):$B
c   $
cAQ IOPT(1:$M):$B
c   $
cAR X((1:$M):$B
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM,LTXTAN,LTXTAO,LTXTAP,LTXTAQ,
     * LTXTAR
      parameter (LTXTAA=  1,LTXTAB=  9,LTXTAC= 54,LTXTAD=100,LTXTAE=133,
     * LTXTAF=180,LTXTAG=234,LTXTAH=289,LTXTAI=341,LTXTAJ=379,
     * LTXTAK=440,LTXTAL=499,LTXTAM=550,LTXTAN=  1,LTXTAO=  1,
     * LTXTAP=  1,LTXTAQ=  1,LTXTAR=  1)
      character MTXTAA(3) * (201)
      character MTXTAB(1) * (12)
      character MTXTAC(1) * (13)
      character MTXTAD(1) * (13)
      character MTXTAE(1) * (13)
      character MTXTAF(1) * (11)
      data MTXTAA/'SILUPM$BEstimated error = $F, Requested error = $F.$E
     *Need NDIM in interval [1, $I] but NDIM = $I.$ELUP($I) = $I; it sho
     *uld be < 4.$ERagged table badly specified in dimension $I.$EStart$
     * of ragged table',', NTAB($I) = $I; it must be 0.$EMiddle of ragge
     *d table, NTAB($I) = $I; it must be >0.$EEnd of ragged table, NTAB(
     *$I) = $I; it must be -1.$EIOPT($I) = $I is not a valid option.$EIn
     * dimension $I, first X','T = XT($I) = last XT = XT($I) = $F.$EA to
     *tal of only $I derivatives allowed, but $I requested.$ENeed deriva
     *tive order in [0, $I], but it is = $I.$EPrevious error from SILUP$
     * occurred in dimension $I.$E'/
      data MTXTAB/'LUP(1:$M):$B'/
      data MTXTAC/'NDEG(1:$M):$B'/
      data MTXTAD/'NTAB(1:$M):$B'/
      data MTXTAE/'IOPT(1:$M):$B'/
      data MTXTAF/'X((1:$M):$B'/
c
      data MLOC /LTXTAB, LTXTAC, LTXTAD, LTXTAE, LTXTAF, LTXTAG, LTXTAH,
     1           LTXTAI, LTXTAJ, LTXTAK, LTXTAL, LTXTAM/
c
c                      1 2 3 4       5       6 7       8       9 10
      data MACT / MEEMES,0,0,0, MECONT, MEMDA1,0, METEXT, MEIVEC, 0,
     1   MECONT,  MERET /
      data INTCHK(0) / 120 /
      data ID4 / 4 /
c
c *****************     Start of executable code     *******************
c
      LI = 2
      NDIMI = NDIM
      NTABXT = NDIMI+1
      NTABM = NTABXT + NDIMI
      LT = NTABM
      if (NTAB(NTABXT) .le. 0) then
c                            Fix NTAB on the first call.
         if (NDIMI .gt. MAXDIM) then
             INTCHK(1) = MAXDIM
             INTCHK(2) = NDIMI
             IIFLG = -2
             go to 400
         end if
         NTAB(NTABXT) = 1000
         NTAB(NTABXT+1) = 1
         do 40 I = 1, NDIMI
            if (LUP(I) .ge. 4) then
c                       LUP(I) is out of range -- fatal error.
               INTCHK(1) = I
               INTCHK(2) = LUP(I)
               IIFLG = -3
               go to 400
            end if
            L = NTAB(I)
            if (L .gt. 0) then
c                             Table is not ragged up to this point.
               if (I .eq. NDIMI) go to 50
c                       If table is ragged save pointer to ragged info.
               if (NTAB(NDIMI) .lt. 0)  NTAB(NTABM+I) = NTABM+NDIMI
c       Get pointer to start of XT data for next dimension
               if (LUP(I) .eq. 3) then
                  NTAB(NTABXT+I+1) = NTAB(NTABXT+I) + 2
               else
                  NTAB(NTABXT+I+1) = NTAB(NTABXT+I) + L
               end if
            else if ((L .eq. 0) .or. ((L .ne. 1-I) .and.
     1         ((I .ne. NDIMI) .or. (L .le. -I)))) then
c                                 Problem in specifying raggedness.
               LT = NDIMI
               INTCHK(1) = I
               IIFLG = -4
               go to 400
            else
               J = NTAB(NTABM+I-1)
               if (NTAB(I-1) .gt. 0) then
                  NTAB(NTABXT) = -NTAB(I)
c       Get K = number of NTAB entries of extra data
                  K = NTAB(1)
                  do 10 L = 2, -L
                     K = K * NTAB(L)
   10             continue
               else
                  K = NTAB(J-1)
               end if
               if (NTAB(J) .ne. 0) then
                  LT = J
                  INTCHK(1) = J
                  INTCHK(2) = NTAB(J)
                  IIFLG = -5
                  go to 400
               end if
c       Change data to be the partial sum of the original data.
               LT = J + K
               do 30 L = J + 1, LT
                  if (NTAB(L) .le. 0) then
c                     Error, bad value inside ragged table info.
                     INTCHK(1) = L
                     INTCHK(2) = NTAB(L)
                     IIFLG = -6
                     go to 400
                  end if
                  NTAB(L) = NTAB(L) + NTAB(L-1)
   30          continue
               if (I .eq. NDIMI) then
                  LT = LT + 1
                  if (NTAB(LT) .eq. -1) go to 50
c                    Error -- No end tag where needed.
                  INTCHK(1) = LT
                  INTCHK(2) = NTAB(LT)
                  IIFLG = -7
                  go to 400
               end if
c       Save index of 0th NTAB entry for extra data for next dim.
               NTAB(NTABM+I) = J + K + 1
c       Get pointer to start of XT data for next dimension
               if (LUP(I) .eq. 3) then
                  NTAB(NTABXT+I+1) = NTAB(NTABXT+I) + 2*K
               else
                  NTAB(NTABXT+I+1) = NTAB(NTABXT+I)+NTAB(J+K)
               end if
            end if
   40    continue
      end if
   50 continue
      IIFLG = 0
      LINFO(0) = 0
      LINFO0(0) = 0
      LINFO0(1) = 1
      do 60 I = 2, NDIMI
         LINFO0(I) = LINFO0(I-1) + NDEG(I-1) + 2
   60 continue
c                  Initialize option flags
      GETERP = .false.
      SETEXT = 0
      KAOS = 0
      IEEOPT = 0
      STOD = 0
      LDERIV = 0
      IOP2N = 254
      IRAG = NTAB(NTABXT)
      INTCHK(4) = 0
      INTCHK(6) = 0
      INTCHK(5) = -2 * (LINFO0(NDIMI) + 1)
      LCKEND = 7
      go to 70
c
   62 INTCHK(LCKEND+2) = 1
   64 INTCHK(LCKEND+1) = IOPT(LI)
   65 INTCHK(LCKEND) = LOPT
      LCKEND = LCKEND + 3
   70 continue
      LI = LI + 1
      LOPT = IOPT(LI)
      if (LOPT .ne. 0) then
         go to (310, 320, 330, 300, 340, 360), LOPT
         INTCHK(1) = LI
         INTCHK(2) = LOPT
         IIFLG = -8
         go to 400
      end if

      INTCHK(1) = LCKEND
      INTCHK(2) = IOPT(2)
      INTCHK(3) = 1
      INTCHK(LCKEND) = 1
      call OPTCHK(INTCHK, IOPT, 'SILUPM / EOPT$E')
      if (INTCHK(1) .lt. 0) then
         IOPT(1) = -12
         return
      end if
      J = LCKEND
c Store values for storage locations determined by OPTCHK.
   75 J = J + 1
      K = INTCHK(J)
      if (INTCHK(K) .eq. 0) then
         LERTMP = INTCHK(INTCHK(LCKEND+1)+1)
      else
         STOD = INTCHK(INTCHK(LCKEND+2)+1)
      end if
      if (J .ne. INTCHK(LCKEND)) go to 75
      IDXBAS = LERTMP+1
      IYBAS = IDXBAS + LINFO0(NDIMI)
      if (STOD .ne. 0) then
         K = IYBAS + LINFO0(NDIMI)
         do 80 J = 1, NDIMI
            KPTD(J) = KPTD(J) + K
   80    continue
         LDERIV = KPTD(NDIMI-1)
      end if
      IYSAV = IYBAS
      do 90 I = LERTMP, IYBAS
c Ensure in SILUP, YT will be defined. (It's ref. but not used.)
         EOPT(I) = 0
   90 continue
      ERRSAV(1) = 0.E0
      IYT = IDXBAS

      GETERR = GETERP
      KDIM = 1
      IXT = 1
      LUPC = LUP(1)
      IP(1) = 0
  100 NT = NTAB(KDIM)
  110 LEXIT = 0
c                 Setup for variable order.
      if (KAOS .ne. 0) then
         KAOS = 1
         IOPTI(3) = NDEG(KDIM)
      end if
c                 Set KEXTRP to indicate how to extrapolate
      KEXTRP = -1
      if (SETEXT .ne. 0) KEXTRP = IOPT(SETEXT + KDIM)
      if (KDIM .ne. NDIMI) go to 180
c                             Interpolate in the last dimension.
c                 Flag that want results on the exit
      LEXIT = 1
c                 Setup for getting derivatives.
      if (LDERIV .gt. 0) LEXIT = IOPT(IMAXD+KDIM) + 1
c                 Indicate where error info. on Y is found (if any)
      LEXERR = IEEOPT
c                 Indicate whether there are bad points.
      IOPTI(2) = IOP2N
c
      call SILUP(X(KDIM), EOPT(IYSAV), NT, XT(IXT), YT(IP(KDIM)+1),
     1   NDEG(KDIM), LUPC, IOPTI, EOPT)
      if (LUPC .lt. 0) LUP(KDIM) = LUPC
      if (KAOS .lt. 0) go to 390
      IIFLG = max(IIFLG, IOPTI(1))
  130 continue
c                             Back up to lower dimension
      KDIM = KDIM - 1
      if (KDIM .eq. 0) then
         Y = EOPT(IYSAV)
         IOPT(1) = IIFLG
         if (IOPTI(1) .ge. 0) return
         IIFLG = IOPTI(1)
         ERRDAT(1) = EOPT(1)
         ERRDAT(2) = EBND
         go to 400
      end if
      if (GETERP) ERRSAV(KDIM) = max(ERRSAV(KDIM), EOPT(1))
      NDEGEC = LNDEG(KDIM)
      if (KAOS .ne. 0) then
         if (LINFO(KDIM) .ge. LINFO0(KDIM) + 3) then
            NDEGEC = LINFO(KDIM) - LINFO0(KDIM)
            go to 140
         end if
      end if
      if (LINFO(KDIM) .lt. LINFO0(KDIM) + NDEGEC) go to 220
c                              Restore information in the common block
  140 KGOC = abs(LKGOC(KDIM))
      LICINF = LINFO0(KDIM)
      LINFO(KDIM+1) = LINFO0(KDIM+1)
      do 150 I = 0, NDEGEC
         IDX(I) = LIDX(I+LICINF)
         DX(I) = EOPT(I+IDXBAS+LICINF)
  150 continue
      IYSAV = IYBAS + LINFO(KDIM-1)
      IYT = IYBAS + LINFO0(KDIM)
c                 Flag that already have the Y's in order.
      LUPC = 4
c                 Flag that want results on the exit
      LEXIT = 1
c                 Setup for getting derivatives.
      if (LDERIV .ne. 0) then
         if (LDERIV .ge. -KDIM) then
            LDERIV = KPTD(KDIM-1)
            if (KDIM .ne. 1) LDERIV = LDERIV + NUMD(KDIM) *
     1          (LINFO(KDIM-1)-LINFO0(KDIM-1))
            LEXIT = IOPT(IMAXD+KDIM) + 1
         end if
      end if
c                 Indicate where error info. on Y is found (if any)
      if (GETERP) then
         LEXERR = LERTMP
         EOPT(LEXERR) = ERRSAV(KDIM)
         EOPT(LEXERR+1) = 0.E0
      end if
c                 Setup for variable order.
      if (KAOS .ne. 0) then
         KAOS = 1
         IOPTI(3) = NDEGEC
      end if
c                 Indicate no bad points.
  180 IOPTI(2) = 254
c
      if (mod(NDEG(KDIM), 2) .eq. 1) NDEGEC = NDEG(KDIM)
      call SILUP(X(KDIM), EOPT(IYSAV), NT, XT(IXT), EOPT(IYT),
     1   NDEG(KDIM), LUPC, IOPTI, EOPT)
      if (KAOS .lt. 0) go to 390
      IIFLG = max(IIFLG, IOPTI(1))
      if (LEXIT .eq. 0) then
c                            Just selected points for this dimension
         if (LUPC .lt. 0) LUP(KDIM) = LUPC
c                             Save info. from the common block.
         LKGOC(KDIM) = KGOC
         LNDEG(KDIM) = NDEGEC
         LICINF = LINFO0(KDIM)
         LINFO(KDIM) = LICINF
         do 210 I = 0, NDEGEC
            LIDX(I+LICINF) = IDX(I)
            EOPT(I+IDXBAS+LICINF) = DX(I)
  210    continue
         go to 230
      else
c                            Just got result for this dimension
c                  IOPTI(2) = 0, if variable order and need more points.
         if (IOPTI(2) .eq. 0) go to 220
         if (LDERIV .gt. 0) then
            GETERR = .false.
            ID = KPTD(KDIM)
            do 212 I = KDIM+1, NDIMI
               KC(I) = 0
  212       continue
            MXD = MTOTD
  214       IYDSAV = LDERIV + LEXIT - 1
            L = KDIM + 1
  215       if ((KC(L) .eq. IOPT(IMAXD+L)) .or. (MXD .eq. 0)) then
               MXD = MXD + KC(L)
               KC(L) = 0
               L = L + 1
               if (L .le. NDIMI) go to 215
               go to 218
            end if
            KC(L) = KC(L) + 1
            MXD = MXD - 1
            LEXIT = min(MXD, IOPT(IMAXD+KDIM)) + 1
            LDERIV = IYDSAV + 1
            do 216 I = 0, NDEGEC
               EOPT(IYT+I) = EOPT(ID + NUMD(KDIM+1)*I)
  216       continue
            ID = ID +1
            KGOC = abs(KGOC)
            if (mod(NDEG(KDIM), 2) .eq. 1) NDEGEC = NDEG(KDIM)
c                      Interpolate in an outer dimension.
            call SILUP(X(KDIM), EOPT(IYDSAV), NT, XT(IXT), EOPT(IYT),
     1         NDEG(KDIM), ID4, IOPTI, EOPT)
            go to 214
  218       GETERR = GETERP
         end if
         go to 130
      end if
  220 LINFO(KDIM) = LINFO(KDIM) + 1
      if (LDERIV .gt. 0) then
         if (LINFO(KDIM) .eq. LINFO0(KDIM) + NDEGEC) then
            if (LKGOC(KDIM) .eq. -2) then
               LDERIV = -KDIM
               go to 230
            end if
         end if
         if (KDIM .eq. NDIMI-1) then
            LDERIV = LDERIV + NUMD(NDIMI)
         else
            LDERIV = KPTD(NDIMI-1)
         end if
      end if
  230 IX = LIDX(LINFO(KDIM))
c          Get here after getting next point in this dimension
c               Compute IP() data for getting XT and YT indices
      if (KDIM .lt. IRAG) then
         IP(KDIM+1) = NTAB(KDIM+1) * (IX - 1 + IP(KDIM))
      else if (KDIM .eq. IRAG) then
         K = NTAB(NTABM+KDIM) + IP(KDIM) + IX
         IP(KDIM+1) = NTAB(K-1)
         IPRAG = NTAB(K) - NTAB(K-1)
      else if (NTAB(KDIM) .gt. 0) then
         IP(KDIM+1) = NTAB(KDIM) * IP(KDIM) + (IX -1) * IPRAG
      else
         IP(KDIM+1) = NTAB(NTAB(NTABM+KDIM)+IP(KDIM)+IX-1)
      end if
      IYSAV = IYBAS + LINFO(KDIM)
      KDIM = KDIM + 1
      ERRSAV(KDIM) = 0.E0
      if ((LEXIT .eq. 0) .or. (NTAB(KDIM) .lt. 0)) then
         IXT = NTAB(NTABXT+KDIM)
         LUPC = LUP(KDIM)
         if (NTAB(KDIM) .ge. 0) go to 100
         K = -NTAB(KDIM)
         K = NTAB(NTABM+K) + IP(K)
         NT = NTAB(K+IX) - NTAB(K+IX-1)
         if (LUPC .eq. 3) then
            IXT = IXT+2*(IP(-NTAB(KDIM))+LIDX(LINFO(-NTAB(KDIM)))-1)
         else
            IXT = IXT + IP(1-NTAB(KDIM))
         end if
         go to 110
      end if
      if (KDIM .eq. NDIM) then
         LUPC = LUP(KDIM)
         go to 110
      end if
      LINFO(KDIM) = LINFO0(KDIM)
      go to 230
c
c Process various options
c Specify error in Y.
  300 LI = LI + 1
      IEEOPT = IOPT(LI)
      INTCHK(LCKEND+2) = 2
      GETERP = .true.
      go to 64
c Return an error estimate
  310 GETERP = .true.
      go to 70
c Setup to set extrapolation degree
  320 SETEXT = LI
      LI = LI + NDIMI
      go to 70
c Setup for computing extra derivatives
  330 STOD = IOPT(LI+1)
      INTCHK(LCKEND+1) = STOD
      IMAXD = LI + 2
      MTOTD = IOPT(IMAXD)
      if (MTOTD .ge. LIDXSZ) then
         INTCHK(1) = LIDXSZ
         INTCHK(2) = MTOTD
         IIFLG = -10
         go to 400
      end if
      KL = IOPT(IMAXD+NDIMI)
      K = min(MTOTD, NDEG(NDIMI))
      if ((KL .lt. 0) .or. (KL .gt. K)) then
         INTCHK(1) = K
         INTCHK(2) = KL
         IIFLG = -11
         go to 400
      end if
      do 332 K = 0, MTOTD
         LIDX(K+1) = min(K, KL) + 1
  332 continue
      NUMD(NDIMI) = KL
      do 338 J = NDIMI-1, 1, -1
         L = IOPT(IMAXD+J)
         K = min(MTOTD, NDEG(J))
         if ((L .lt. 0) .or. (L .gt. K)) then
            INTCHK(1) = K
            INTCHK(2) = L
            IIFLG = -11
            go to 400
         end if
         do 334 K = MTOTD, L+1, -1
            LIDX(K+1) = LIDX(K+1) - LIDX(K-L)
  334    continue
         do 336 K = 1, MTOTD
            LIDX(K+1) = LIDX(K) + LIDX(K+1)
  336    continue
         KL = min (KL + L, MTOTD)
         NUMD(J) = LIDX(KL+1) - 1
  338 continue
      INTCHK(LCKEND+2) = NUMD(1)
      if (STOD .le. 0) then
         INTCHK(LCKEND+1) = -NUMD(1)
         INTCHK(LCKEND+2) = LI + 1
      end if
      KPTD(1) = 0
      do 339 J = 2, NDIMI
         KPTD(J) = KPTD(J-1) + NUMD(J) * (NDEG(J-1) + 2)
  339 continue
      INTCHK(5) = INTCHK(5) - KPTD(NDIMI)
      LI = IMAXD + NDIMI
      go to 65
c Setup for variable order.
  340 LI = LI + 1
      EBND = EOPT(IOPT(LI))
      EBNDR = 0.E0
      KAOS = 1
      GETERP = .true.
      go to 62
c Setup to indicate bad points
  360 LI = LI + 1
      BADPT = EOPT(IOPT(LI))
      IOP2N = 255
      go to 62
c Error Processing
  390 IIFLG = IOPTI(1) - 20
      INTCHK(1) = KDIM
      MACT(4) = MLOC(12)
      MACT(2) = -KAOS
      go to 410
  400 MACT(4) = MLOC(-IIFLG)
      MACT(2) = 88
      if (IIFLG .eq. -1) MACT(2) = 25
  410 IOPT(1) = IIFLG
      MACT(3) = -IIFLG
      call SMESS (MACT, MTXTAA, INTCHK(1), ERRDAT)
      if (IIFLG .lt. -2) then
         MACT(7) = NDIMI
         MACT(10) = NDIMI
         call MESS (MACT(6), MTXTAB, LUP)
         call MESS (MACT(6), MTXTAC, NDEG)
         MACT(9) = MEFVEC
         call SMESS (MACT(6), MTXTAF, NDEG, X)
         MACT(9) = MEIVEC
         MACT(7) = LT
         MACT(10) = LT
         call MESS (MACT(6), MTXTAD, NTAB)
         if (LI .gt. 3) then
            MACT(7) = LI
            MACT(10) = LI
            call MESS (MACT(6), MTXTAE, IOPT)
         end if
      end if
      call MESS (MACT(12), MTXTAA, INTCHK(1))
      return
      end
