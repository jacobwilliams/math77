      subroutine DSFITC(CCODE, XI, YI, SD, KORDER, NCOEF, TKNOTS,
     *                  BCOEF, RNORM, ISET, INFO, WORK)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 DSFITC Krogh   Added external statement.
c>> 1996-01-23 DSGITC Krogh  Changes to simplify conversion to C.
c>> 1995-11-21 DSFITC Krogh  Converted from SFTRAN to Fortran 77.
c>> 1994-11-16 CLL Add loops to zero debug arrays XT() and RT().
c>> 1994-10-19 DSFITC Krogh  Changes to use M77CON
c>> 1994-01-31 DSFITC CLL Added test for SD(i) .le. 0 when SD(1) > 0.
c>> 1992-12-16 CLL Corrected formula for NEED2 and comments re WORK().
c>> 1992-11-12 C. L. Lawson, JPL  Initializing LEFT, J1, J2.
c>> 1992-10-27 C. L. Lawson, JPL
c>> 1989-03-02 C. L. Lawson, JPL
c>> 1989-02-23 C. L. Lawson, JPL
c>> 1988-04-01 C. L. Lawson, JPL
c
c        Weighted least squares fit to discrete data by a polynomial
c     spline function of order KORDER.  The user can specify equality or
c     inequality constraints.  The fitting equations as well as the
c     constraints may involve the value, a derivative of specified
c     order, or a definite integral of the spline function.  A fitting
c     equation or constraint may involve function or derivative values
c     at different points, and relations between derivatives of
c     different orders.
c
c        The order of a polynomial spline function is one
c     greater than the degree of its polynomial pieces.
c     Example: KORDER = 4 specifies a cubic spline function.
c
c        The "proper fitting interval" is from A = TKNOTS(KORDER) to
c     B = TKNOTS(NT+1-KORDER).  Extrapolation outside this interval
c     is permitted, but one must expect diminished accuracy at
c     extrapolated points.  The given data or constraint
c     abcissas may be outside [A, B], and subsequent evaluations can be
c     done at points X outside [A, B].
c     ------------------------------------------------------------------
c           Specification of fitting and constraint equations.
c
c        Let F denote the polynomial spline to be determined.  Let the
c     quadruple (CCODE(i), X(i), Y(i), SD(i)) be called the ith
c     specification row.  For each desired fitting equation or
c     constraint equation, (either of which we call a relation) the user
c     must specify one or two (consecutive) specification rows.
c
c        CCODE(i) consists of 4 characters, that we call
c     KIND, DERIV, RELOP, and ACTIVE.
c
c        KIND may be '1', '2', '3', or '4'.  KIND determines the kind of
c     relation being specified.  When KIND = '1' or '2' all
c     information for the relation is given in a single specification
c     row.  We call this Row i in the following discussion.
c     When KIND = '3' or '4', two consecutive specification rows are use
c     We call these Rows i and i+1.  We will complete the explanation of
c     KIND after describing DERIV, RELOP, and ACTIVE.
c
c        DERIV may be '0', '1', ..., or '9'.  This selects the order of
c     derivative of F to appear in the relation.  '0' denotes the value
c     of F itself.
c
c        RELOP may be '~', '=', '<', or '>'.
c     RELOP = '~' means the relation is to be a least-squares fitting
c     equation, '=' means an equality constraint equation, '<' means a
c     less-than-or-equal constraint equation, and '>' means a
c     greater-than-or-equal constraint equation.  When RELOP = '~',
c     SD(i) specifies the a priori standard deviation of the right-side
c     member of the equation.  When RELOP indicates a constraint
c     equation, SD(i) will be ignored.
c
c        ACTIVE may be 'A', 'N', or '!'.  Lower case 'a' and 'n' are
c     also accepted.  ACTIVE = 'A' means the current specification row
c     is active, i.e., a relation will be generated from these
c     specifications.  ACTIVE = 'N' means the specifications are not
c     active, i.e., no relation will be generated.  ACTIVE = '!' means
c     the current row is inactive and there are no following rows,
c     i.e., this marks the end of the specification data.  The user must
c     provide this termination signal.
c     When KIND = '3', or '4', meaning two specification rows are to
c     be interpreted together, the setting of ACTIVE = 'A' or
c     ACTIVE = 'N' must be consistent in these two rows.
c     Setting ACTIVE = '!' in only the first row is permitted, however,
c     since then the second row will not be accessed anyway.
c
c        The forms of the relations selected by KIND are:
c
c     KIND = 1:      G(X(i)) RELOP Y(i)
c
c        where G is the derivative of F selected by DERIV.
c
c     KIND = 2:      G(X(i)) - G(Y(i)) RELOP 0
c
c        where G is the derivative of F selected by DERIV.
c
c     KIND = 3:      G(X(i)) - Y(i+1) * H(X(i+1)) RELOP Y(i)
c
c        where G is the derivative of F selected by DERIV(i) and
c        and H is the derivative of F selected by DERIV(i+1).  In this
c        case KIND(i+1) and RELOP(i+1) are not used.
c
c     KIND = 4:      Integral from X(i) to X(i+1) of F RELOP Y(i)
c
c        In this case DERIV(i), KIND(i+1), DERIV(i+1), RELOP(i+1), and
c        Y(i+1) are not used.
c     ------------------------------------------------------------------
C     The linear algebra methods were designed by C.L.Lawson and
c     R.J.Hanson.  The method of representing spline functions is due
c     to Carl de Boor.  References:
C     "SOLVING LEAST SQUARES PROBLEMS", by Lawson and Hanson,
C     Prentice-Hall, 1974.
c     "A PRACTICAL GUIDE TO SPLINES" by Carl de Boor,
c     Springer-Verlag, 1978.
c     The functionality and user interface of this subprogram are
c     modeled on the "French Curve" subroutine, FC, developed by Hanson
c     and Lawson at JPL in 1970, and the subsequent version developed by
c     Hanson at Sandia in 1979.
c     March 1988, CLL, JPL.  Revised to conform to the Fortran 77
c     standard.  Intended for inclusion in the JPL MATH77 math library.
c     Feb 1989, CLL, JPL.  Revised to use KIND = 3 to specify an
c     integral, and added new kind of relation using KIND = 4.
c     ------------------------------------------------------------------
c                     SUBROUTINE ARGUMENTS
c
c  CCODE()  [in, char*4]  CCODE(i) is regarded as consisting of four
c           single-character fields.
c        CCODE(i)(1:1) = KIND  =  '1',  '2', '3', '4'.
c        CCODE(i)(2:2) = DERIV   =  '0',  '1', ..., '9'.
c        CCODE(i)(3:3) = RELOP  =  '~',  '=',  '<',  '>'.
c        CCODE(i)(4:4) = ACTIVE =  'A',  'N',  '!'
c        Where alphabetic characters are shown, the corresponding
c        lower case character is also acceptable.
c
c  X()  [in]  Abcissas for specification of fitting or
c        constraint equations.
c
c  Y()  [in]  Values or abcissas for specification
c        of fitting or constraint equations.
c
c  SD() [in]  Specifies the a priori standard deviation of error in the
c       right-side value in each fitting equation.
c       The weighted fitting algorithm will take account of these.
c       Optionally, the user may set SD(1) to a negative value.
c       Then this subr will use abs(SD(1)) as the standard deviation
c       for the right-side value in each fitting equation.  In this
c       latter case the SD() array can be dimensioned SD(1).
c       Note that a negative value in SD(1) will always be interpreted
c       in this way by this subr, even if the associated RELOP is not
c       '~' or if ACTIVE is not 'A'.
c
c  KORDER  [in]  Order of the spline basis functions.  The
c        polynomial degree of the spline segments is one less than the
c        order.  Example:  the order of a cubic spline is 4.
c        Require KORDER .ge. 1.  Internal arrays in subroutines used put
c        an upper limit of 20 on KORDER.
c
c  NCOEF  [in]  No. of B-spline coefficients to be determined.
c
c  (TKNOTS(j),j=1,NT, where NT = NCOEF+KORDER)  [in]  This is the deBoor
c     knot sequence for definition of the spline basis functions.
c     These values must be nondecreasing.
c     Repeated values are permitted, but values at
c     an index spacing of KORDER must be strictly increasing.
c        The first and last KORDER-1 values in TKNOTS() are needed to
c     support the deBoor method of representing splines.
c        The "proper fitting interval" is from
c     A = TKNOTS(KORDER) to B = TKNOTS(NT+1-KORDER).  One acceptable and
c     convenient way to set the first and last KORDER-1 knots is to set
c     the first KORDER-1 to A and the last KORDER-1 to B.
c        Continuity of the spline at knots interior to (A, B) will be of
c     order KORDER-2, unless a knot is repeated, in which case the order
c     of continuity will be decreased at that knot.
c
c  BCOEF()  [out]  An array of length NCOEF into which the computed
c        coefficients defining the fitted curve will be stored.  These
c        are coeffients relative to B-spline basis functions.
c        For I = 1, ..., NCOEF, the coefficient BCOEF(I)
c        is associated with the basis function whose support interval
c        runs from TKNOTS(I) to TKNOTS(I+KORDER).
c
c  RNORM   [out]  RNORM := sqrt( sum over the indices i for which
c         fitting was requested of [( (yfit(i) - Y(i))/SD(i))**2])
c
c  ISET()  [in integer]  Array of length 3.
c        ISET(1) = NINFO, the dimension of INFO().  A sufficiently
c           large value for NINFO is 7 + 2*(NCOEF + NS).
c        ISET(2) = NWORK, the dimension of WORK().  A sufficiently
c           large value for NWORK can be computed as follows:
c           See definition of NS, M1, and MFIT below under INFO().
c           NTOT = NCOEF + NS
c           MTOT = M1 + MFIT
c           MINMN = min(MTOT, NTOT)
c           NWORK = MTOT*NTOT + 3*MTOT + 6*NTOT + 3*MINMN + M1
c        ISET(3) = KPRINT, a print flag in the range [0, 4].  It is
c           passed on to DBLSE.  Larger values produce more printing.
c
c  INFO()  [out and scratch integer]  The first 7 elements of INFO()
c        are used to return information about the problem.  The
c        following 2*(NCOEF+NS) locations are used as scratch.
c        INFO(1) = IERR5, the Error status indicator.
c        Note that IERR4 comes from DBLSE.  Possible values of IERR5 are
c        as follows:
c
c        =    0 means no errors detected.
c        =  100 means  NCOEF .lt. 1
c        =  200 means  TKNOTS(I) .gt. TKNOTS(I+1)
c        =  250 means  TKNOTS(I) .ge. TKNOTS(I+KORDER)
c        =  300 means  NINFO or NWORK is too small.
c        =  500 means  DERIV has bad value.
c        =  600 means  RELOP has bad value.
c        =  700 means  KIND has bad value.
c        =  800 means  ACTIVE has bad value.
c        = 1000 + IERR4 means IERR4 .ne. 0 due to error
c                       detected in _BLSE.
c        = 1100 means  SD(1) = zero.
c        = 1200 means  SD(1) > zero and SD(i) .le. zero for some i.
c
c        INFO(2)  = NEED1, the dimension needed for INFO().
c        INFO(3)  = NEED2, the dimension needed for WORK().
c        INFO(4)  = M1, the number of constraints rows in the matrix
c           representation of the problem.  This will be a count of
c           the number of nonignored instances of CCODE(i) having
c           RELOP = '=', '<', or '>, and ACTIVE = 'A'.
c        INFO(5)  = MFIT, the number of least-squares equations.
c           This will be a count of
c           the number of nonignored instances of CCODE(i) having
c           RELOP = '~' and ACTIVE = 'A'.
c        INFO(6)  = NS, the number of slack variables.  This will be a
c           count of the number of nonignored instances of CCODE(i)
c           having RELOP = '<' or '>, and ACTIVE = 'A'.
c        INFO(7)  = NSETP, the number of variables in Set P at
c           termination.  These variables are at values determined by
c           solution of a system of equations.  The other NCOEF + NS
c           - NSETP variables will be at fixed values, either at one of
c           their bounds or at zero.
c
c  WORK()  [scratch]  Work space dimensioned NWORK.
c     ------------------------------------------------------------------
c          Important internal variables.
c
c  BASIS()  Array in which values of KORDER basis functions or their
c           will be stored.  Dimensioned using the parameter KMAX.
c           This puts an upper limit on permissible KORDER.
c  JCOL     Column of matrix into which first element of current
c           set of basis function values will be placed.
c           JCOL = LEFT - KORDER + 1.
c  KINFO    Parameter specifying the number of locations at the
c           beginning of INFO() used for specific items of returned
c           information.  Space beyond these locations is used for
c           scratch.
c  KMAX     Intermal dimensioning parameter.  The input value of KORDER
c           must not exceed KMAX.
c  KORDP1   = KORDER+1
C  KSIZE    Number of rows in current block.
c  LEFT     Index of current spline segment.  LEFT will satisfy
c           KORDER .le. LEFT .le. NCOEF.
c           The knot interval associated with index LEFT is from
c           T(LEFT) to T(LEFT+1).
c           Note that the union of these
c           segments is the "proper fitting interval".
c  ELIMIT   Limit on number of errors in initial scan of specs before
c           quitting.
c     ------------------------------------------------------------------
c--D replaces "?": ?SFITC, ?BLSE, ?SBASI, ?SBASD, ?SFIND, ?ERV1
c     Both versions use      ERMOR, ERMSG, IERM1, IERV1
c     ------------------------------------------------------------------
      external D1MACH
      integer ELIMIT, KINFO
      parameter(ELIMIT = 9, KINFO = 7)
      integer ACTIVE, DERIV, FAC
      integer I, IC, IERR4, IERR5
      integer INFO(*), IRCON, IRLS, IROW, ISET(3)
      integer IWBND, IWBVEC, IWCC, IWDIFF, IWINDX, IWJSTA
      integer IWRT, IWSIZ, IWSS, IWTNRM, IWWRK, IWXS, IWXT, IWZ
      integer J, J1, J2, JCOL, JS, KMAX, KORDER, KPRINT, LEFT
      integer M1, MFIT, MINMN, MN, MODE, MTOT
      integer NBADCC, NCOEF, NEED1, NEED2, NINFO, NTOT
      integer NS, NSETP, NT, NWORK, RELOP, KIND
      parameter(KMAX=20)
      double precision D1MACH
      double precision BASIS(KMAX), BCOEF(NCOEF), ONE
      double precision RNORM, RTVAL, SD(*), SDIC
      double precision TKNOTS(NCOEF+KORDER), TOL, UNBND
      double precision WORK(*), WT, WT1
      double precision X, XI(*), YI(*), ZERO
      character ATAB*6, CCODE(*)*4, DTAB*10
      character RTAB*4, NVTAB*4
      character MSG1*19, MSG3*19, MSG4*19
      logical   USEWT1
      parameter( ONE = 1.0D0, UNBND = 99.0D0, ZERO = 0.0D0)
      parameter(DTAB='0123456789', RTAB='~=<>')
      parameter(NVTAB='1234', ATAB=' AaNn!')
      data MSG1 / 'CCODE(I)(1:1) =  ' /
      data MSG3 / 'CCODE(I)(3:3) =  ' /
      data MSG4 / 'CCODE(I)(4:4) =  ' /
c     ------------------------------------------------------------------
      NINFO  = ISET(1)
      NWORK  = ISET(2)
      KPRINT = ISET(3)
      NT = NCOEF + KORDER
      IERR5 = 0
C
C          Exit immediately if NCOEF .lt. 1  or if
C          the knots fail to be nondecreasing.
c
      if ( NCOEF .lt. 1 ) then
         IERR5 = 100
         call IERM1('DSFITC',IERR5,0,
     *   'Require NCOEF .ge. 1', 'NCOEF',NCOEF,'.')
         go to 500
      end if
      if(KORDER .gt. KMAX) then
         IERR5 = 150
         call IERM1('DSFITC',IERR5,0,'Require KORDER .le. KMAX.',
     *   'KORDER',KORDER,',')
         call IERV1('KMAX',KMAX,'.')
         go to 500
      end if
c
      do 20 I = 1,NT-1
        if (TKNOTS(I) .gt. TKNOTS(I+1)) then
          IERR5 = 200
          call IERM1('DSFITC',IERR5,0,
     *    'Require knots, TKNOTS(I), to be nondecreasing.',
     *    'I',I,',')
          call DERV1('TKNOTS(I)',TKNOTS(I),',')
          call DERV1('TKNOTS(I+1)',TKNOTS(I+1),'.')
          go to 500
        end if
   20 continue
c
      do 40 I = 1,NCOEF
        if (TKNOTS(I) .ge. TKNOTS(I+KORDER)) then
          IERR5 = 250
          call IERM1('DSFITC',IERR5,0,
     *    'Require TKNOTS(I) < TKNOTS(I+KORDER).',
     *    'I',I,',')
          call DERV1('TKNOTS(I)',TKNOTS(I),',')
          call DERV1('TKNOTS(I+KORDER)',TKNOTS(I+KORDER),'.')
          go to 500
        end if
   40 continue
C
c     ------------------------------------------------------------------
C                                       TEST SD(1)
      if(SD(1) .lt. ZERO) then
         WT1 = -ONE/SD(1)
         USEWT1 = .true.
      else if( SD(1) .gt. ZERO) THEN
         USEWT1 = .false.
      else
         IERR5 = 1100
         call ERMSG('DSFITC',IERR5,0,'Require SD(1) .ne. Zero','.')
         go to 500
      end if
c
c     .  Determine M1, MFIT, NS, MTOT, and NTOT.
c     .  M1 = number of non-ignored constraint specifications.
c     .  MFIT = number of non-ignored least-squates equations.
c     .  NS = number of non-ignored constraints that
c     .  are inequality constraints and thus require a slack variable.
c     .  MTOT = M1 + MFIT
c     .  NTOT = NCOEF + NS
c
      NBADCC = 0
      M1   = 0
      MFIT = 0
      NS   = 0
      I = 1
c     do forever
   60 continue
         KIND = index(NVTAB, CCODE(I)(1:1))
         DERIV  = index(DTAB, CCODE(I)(2:2)) - 1
         RELOP = index(RTAB, CCODE(I)(3:3))
         ACTIVE = index(ATAB, CCODE(I)(4:4))/2
         if(ACTIVE .eq. 3) go to 180
c
c        .  CCODE(I)(1:1) =  1  2  3  4
c        .           KIND =  1  2  3  4
c
c        .  CCODE(I)(2:2) =  0  1  2  3  4  5  6  7  8  9
c        .          DERIV =  0  1  2  3  4  5  6  7  8  9
c
c        .  CCODE(I)(3:3) =  ~  =  <  >
c        .          RELOP =  1  2  3  4
c
c        .  CCODE(I)(4:4) =  A  a  N  n  !
c        .         ACTIVE =  1  1  2  2  3
c
         if(ACTIVE .eq. 1) then
C           do case(RELOP, 4)
            go to (110, 120, 130, 140), RELOP
C           case other
               IERR5 = 600
               call IERM1('DSFITC',IERR5,0,
     *         'RELOP = CCODE(I)(3:3) has invalid value.','I',I,',')
               MSG3(19:19) = CCODE(I)(3:3)
               call ERMOR(MSG3, '.')
               NBADCC = NBADCC+1
               go to 150
C           case 1
  110          MFIT = MFIT+1
               go to 150
C           case 2
  120          M1 = M1+1
               go to 150
C           case 3
  130          M1 = M1+1
               NS = NS+1
               go to 150
C           case 4
  140          M1 = M1+1
               NS = NS+1
  150       continue
c           end case
         else if(ACTIVE .ne. 2) then
               IERR5 = 800
               call IERM1('DSFITC',IERR5,0,
     *         'ACTIVE = CCODE(I)(4:4) has invalid value.','I',I,',')
               MSG4(19:19) = CCODE(I)(4:4)
               call ERMOR(MSG4, '.')
               NBADCC = NBADCC+1
         end if
c
         if(KIND .eq. 1 .or. KIND .eq. 2) then
            I = I+1
         else if(KIND .eq. 3 .or. KIND .eq. 4) then
            I = I+2
         else
            IERR5 = 700
            call IERM1('DSFITC',IERR5,0,
     *      'KIND = CCODE(I)(1:1) has invalid value.','I',I,',')
            MSG1(19:19) = CCODE(I)(1:1)
            call ERMOR(MSG1, '.')
            NBADCC = ELIMIT+1
         end if
         if(NBADCC .gt. ELIMIT) then
            call ERMSG('DSFITC',IERR5,0,
     *      'Quitting on bad values in CCODE()','.')
            go to 500
         end if
         go to 60
  180 continue
C     end forever
c
      if(NBADCC .ne. 0) then
         call ERMSG('DSFITC',IERR5,0,
     *    'Quitting on bad values in CCODE()','.')
         go to 500
      end if
      MTOT = M1 + MFIT
      NTOT = NCOEF + NS
*     print*,'DSFITC.. M1, MFIT, NCOEF, NS =',M1, MFIT, NCOEF, NS
      MINMN = min(MTOT, NTOT)
      INFO(4) = M1
      INFO(5) = MFIT
      INFO(6) = NS
c
c     .  Set indices to partition the work arrays INFO() and WORK().
c
      IWINDX = 1+KINFO
      IWJSTA = IWINDX+NTOT
      NEED1  = IWJSTA+NTOT - 1
      INFO(2) = NEED1
c
      MN = MTOT*NTOT
      IWBVEC = MN+1
      IWBND  = IWBVEC+MTOT
      IWXS   = IWBND +2*NTOT
      IWWRK  = IWXS+NTOT
      IWSIZ  = IWWRK+NTOT
      IWTNRM = IWSIZ+M1
      IWZ    = IWTNRM+NTOT
      IWCC   = IWZ+MINMN
      IWSS   = IWCC+MINMN
      IWXT   = IWSS+MINMN
      IWRT   = IWXT+NTOT
      IWDIFF = IWRT+MTOT
      NEED2  = IWDIFF+MTOT - 1
      INFO(3) = NEED2
c     .                                     Check NINFO and NWORK.
c
      if(NINFO .lt. NEED1 .or. NWORK .lt. NEED2) then
              IERR5 = 300
         call IERM1('DSFITC',IERR5,0,
     *   'Require NINFO .ge. NEED1 and NWORK .ge. NEED2.',
     *   'NINFO',NINFO,',')
         call IERV1('NEED1',NEED1,',')
         call IERV1('NWORK',NWORK,',')
         call IERV1('NEED2',NEED2,'.')
         go to 500
      end if
c     .                       Zero an MTOT x NTOT space for the matrix.
      do 200 I = 1, MN
         WORK(I) = ZERO
  200 continue
c     .                       Zero debug arrays XT() and RT() in WORK().
      do 210 j = 1, ntot
         work(iwxt + j - 1) = ZERO
  210 continue
      do 220 j = 1, mtot
         work(iwrt + j - 1) = ZERO
  220 continue
c
c     .  Set bounds for variables.  Storing into a 2 x NTOT space.
c
      do 240 J = 1,2*NTOT
         WORK(IWBND-1+J) = UNBND
  240 continue
      do 250 J = 1,NS
         WORK(IWBND+(NCOEF+J-1)*2) = ZERO
  250 continue
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     Begin loop to form equations, both constraints and least-squares.
c
c     Initialize JS = column index of previous slack variable.
c             IRCON = row index of previous constraint equation.
c             IRLS  = row index of previous least-squares equation.
c               IC  = index of current specification data.
c             LEFT  = Arbitrary starting value for use by DSFIND.
c             J1,J2 = Arbitrary starting values for use by DSBASI.
c
      JS = NCOEF
      IRCON = 0
      IRLS  = M1
      IC    = 1
      J1    = 1
      J2    = 1
      LEFT  = 1
C     do forever
  300 continue
         ACTIVE = index(ATAB, CCODE(IC)(4:4))/2
         if(ACTIVE .eq. 3) go to 400
         if(ACTIVE .eq. 2) then
            IC = IC+1
            go to 300
         end if
         KIND= index(NVTAB, CCODE(IC)(1:1))
         RELOP = index(RTAB, CCODE(IC)(3:3))
c
c        .  CCODE(I)(1:1) =  1  2  3  4
c        .           KIND =  1  2  3  4
c
c        .  CCODE(I)(2:2) =  0  1  2  3  4  5  6  7  8  9
c        .          DERIV =  0  1  2  3  4  5  6  7  8  9
c
c        .  CCODE(I)(3:3) =  ~  =  <  >
c        .          RELOP =  1  2  3  4
c
c        .  CCODE(I)(4:4) =  A  a  N  n  !
c        .         ACTIVE =  1  1  2  2  3
c
c        .  Set matrix row index, IROW.
c        .  Set weight, WT, if RELOP = 1.
c        .  Store coefficient of +1 or -1 for slack variable if
c        .  RELOP is 3 or 4.
c
         if(RELOP .eq. 1) then
            IRLS = IRLS+1
            IROW = IRLS
            if( USEWT1) then
               WT = WT1
            else
               SDIC = SD(IC)
               if(SDIC .gt. ZERO) then
                  WT=ONE/SDIC
               else
                  IERR5 = 1200
                  call ERMSG('DSFITC',IERR5,0,
     *            'With SD(1) > 0  require all SD(I) > 0.', ',')
                  call DERV1('SD(1)',SD(1),',')
                  call IERV1('I',IC,',')
                  call DERV1('SD(I)',SDIC,'.')
                  go to 500
               end if
            end if
         else
            IRCON = IRCON+1
            IROW = IRCON
            if(RELOP .eq. 3) then
               JS = JS+1
               WORK(IROW+(JS-1)*MTOT) = ONE
            else if(RELOP .eq. 4) then
               JS = JS+1
               WORK(IROW+(JS-1)*MTOT) = -ONE
            end if
         end if
c
         if(KIND .eq. 4) then
c                        Setup for integral
            call DSBASI(KORDER, NCOEF, TKNOTS, XI(IC), XI(IC+1),
     *            J1, J2,  WORK(IWXS))
            do 320 J = J1, J2
               if(RELOP .eq. 1) then
                  WORK(IROW+(J-1)*MTOT) = WORK(IWXS-1+J) * WT
               else
                  WORK(IROW+(J-1)*MTOT) = WORK(IWXS-1+J)
               end if
  320       continue
c
            if(RELOP .eq. 1) then
               WORK(MN+IROW) = YI(IC) * WT
            else
               WORK(MN+IROW) = YI(IC)
            end if
c                        End of setup for integral
            IC = IC+2
         else
c                        Setup for value or derivative
c        .  DERIV = CCODE()(2:2) =  0  1  2  3  4  5  6  7  8  9
c
            DERIV  = index(DTAB, CCODE(IC)(2:2)) - 1
            X = XI(IC)
            FAC = ONE
c Negate KIND to flag that this is the first time accumulating values.
            KIND = -KIND
  340       continue
c                       Accumulate values into matrix
            call DSFIND(TKNOTS, KORDER, NCOEF+1, X, LEFT, MODE)
            call DSBASD(KORDER, LEFT, TKNOTS, X, DERIV, BASIS)
            JCOL = LEFT-KORDER+1
*           print*,'DSFITC..            X =',X
*           print*,'DSFITC.. IC,LEFT,JCOL =',IC,LEFT,JCOL
            if(RELOP .eq. 1) FAC = FAC * WT
            do 360 J = 1,KORDER
               WORK(IROW+(JCOL-1)*MTOT) =
     *               WORK(IROW+(JCOL-1)*MTOT) + FAC * BASIS(J)
               JCOL = JCOL + 1
  360       continue
c                       End of accumulate values into matrix
            if (KIND .lt. 0) then
c                       KIND is reset to positive here.
               KIND = -KIND
               if(KIND .eq. 1) then
                  RTVAL = YI(IC)
               else
c           .                              Here KIND = 2 or 3
                  if(KIND .eq. 2) then
                     FAC = -ONE
                     RTVAL = ZERO
                     X = YI(IC)
                  else
                     DERIV  = index(DTAB, CCODE(IC+1)(2:2)) - 1
                     FAC = -YI(IC+1)
                     RTVAL = YI(IC)
                     X = XI(IC+1)
                  end if
c Go back to accumulate values a second time and last time on this iter.
                  go to 340
               end if
            end if
c                                   Set right side of relation.
            if(RELOP .eq. 1) then
               WORK(MN+IROW) = RTVAL * WT
            else
               WORK(MN+IROW) = RTVAL
            end if
c                        End of setup for value or derivative
            IC = IC+1
            if(KIND .eq. 3) IC = IC+1
         end if
         go to 300
  400 continue
c     end forever
c     .                          End loop to form equations.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     print'(1x/1x,a,i5,a,i5)','DSFITC..  MTOT =',MTOT,',   NTOT =',NTOT
c     print'(1x/1x,a/1x)','DSFITC.. Matrix going to DBLSE:'
c     do for I = 1,MTOT
c        print'(1x/1x,i5,3x,5g13.5/(9x,5g13.5))',
c    *    I,(WORK(I+(J-1)*MTOT),J=1,NTOT+1)
c     end for
c
c     .          All points have been processed.  Call for the solution.
c
      TOL = D1MACH(4)**(0.75d0)
      call DBLSE(WORK, MTOT, MTOT, NTOT, M1, WORK(MN+1),
     *     WORK(IWBND), UNBND, KPRINT, TOL, IERR4, WORK(IWXS),
     *     RNORM, NSETP,
     *     WORK(IWWRK), WORK(IWSIZ), WORK(IWTNRM), WORK(IWZ),
     *     WORK(IWCC), WORK(IWSS), INFO(IWINDX), INFO(IWJSTA),
     *     WORK(IWXT), WORK(IWRT), WORK(IWDIFF))
c
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INFO(7) = NSETP
      if(IERR4 .ne. 0) then
         IERR5 = 1000 + IERR4
         call ERMSG('DSFITC',IERR5,0, 'Error noted in DBLSE.','.')
         go to 500
      end if
c
      do 440 I=1,NCOEF
         BCOEF(I) = WORK(IWXS-1+I)
  440 continue
      INFO(1) = IERR5
      return
c     ------------------------------------------------------------------
c                         ERROR RETURN
  500 do 520 I=1,NCOEF
         BCOEF(I)=ZERO
  520 continue
      INFO(1) = IERR5
      return
      end
