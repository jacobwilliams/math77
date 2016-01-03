      subroutine DSFIT(XI,YI,SDI,NXY, KORDER, NC, TKNOTS, BCOEF,
     *                  SIGFAC, IERR1, LDW, W)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2000-12-03 DSFIT Krogh  Declared SDI(*) so ref. with SD(1) o.k.
c>> 1995-11-21 DSFIT Krogh  Converted from SFTRAN to Fortran 77.
c>> 1994-10-19 DSFIT Krogh  Changes to use M77CON
c>> 1994-01-31 DSFIT CLL  Add test for SDI(i) .le. 0 when SDI(1) > 0.
c>> 1993-01-12 CLL  Using DSBASD in place of _SBAS.
c>> 1992-12-15 CLL. Change error message No. 600
c>> 1992-11-18 CLL. Change order of last four arguments.
c>> 1992-11-17 CLL. Permit XI() nondecreasing or nonincreasing.
c>> 1992-10-27 C. L. Lawson, JPL
c>> 1988-03-21 C. L. Lawson, JPL
c           Least squares fit to discrete data by a spline function of
c     order KORDER.  Note that the order is one greater than the degree
c     of the polynomial pieces.  Example: The order of a cubic spline
c     is 4.
c        TKNOTS() must contain NT values, called knots, indexed from
c     1 to NT, with NT = NC+KORDER.  These values must be nondecreasing.
c     Repeated values are permitted.  The first and last KORDER-1 values
c     in TKNOTS() are needed to support the deBoor method of
c     representing splines.  The "proper fitting interval" is from
c     A = TKNOTS(KORDER) to B = TKNOTS(NT+1-KORDER).  One acceptable way
c     to set the first and last KORDER-1 knots is to set the first
c     KORDER-1 to A and the last KORDER-1 to B.
c        Continuity of the spline at knots interior to (A, B) will be of
c     order KORDER-2, unless a knot is repeated, in which case the order
c     of continuity will be decreased at that knot.
c     ------------------------------------------------------------------
C     The linear algebra methods were designed by C.L.Lawson and
c     R.J.Hanson.  The method of representing spline functions is due
c     to Carl deBoor.  References:
C     "SOLVING LEAST SQUARES PROBLEMS", by Lawson and Hanson,
C     Prentice-Hall, 1974.
c     "A PRACTICAL GUIDE TO SPLINES" by Carl de Boor,
c     Springer-Verlag, 1978.
C     Programming and later changes and corrections by Lawson,Hanson,
C     T.Lang, and D.Campbell, Sept 1968, Nov 1969, and Aug 1970.
C  1974 5/21, C.L.Lawson, Fixed bug that caused ISEG to get too big.
C     Also changed to exit immidiately if TKNOTS() array is not
C     strictly increasing.
c  1984 July 10. Modified for improved portability and to conform
c          to Fortran 77.  C. L. Lawson, JPL.
c          Added calls to the error message subrs.
c  7/23/87 CLL.  Added the IERR1 argument.
c  March 1988, CLL.  Introduced the use of deBoor's spline methods.
c  1992-11-17 CLL Permit XI() to be either nondecreasing or
c     nonincreasing.
c     ------------------------------------------------------------------
c                     SUBROUTINE ARGUMENTS
c
c  (XI(i),i=1,NXY)  [in]  Abcissas of data to be fitted.  Require
c        this data be sorted: either nondecreasing or nonincreasing.
c
c  (YI(i),i=1,NXY)  [in]  Ordinates of data to be fitted.
c
c  (SDI(i),i=1,NXY) [in]  User may use this array to assign an
c                     a priori standard deviation of error to each
c       YI(i) value.  The weighted fitting algorithm will take
c       account of these.  Optionally the user may set SDI(1) to
c       a negative value.  Then this subr will use ABS(SDI(1)) as
c       the standard deviation for each YI(i) value.  In this case
c       the SDI() array can be dimensioned SDI(1).
c       If SDI(1) = 0., the subr issues an error message and returns.
c
c  NXY  [in]  No. of data pairs, (XI(i), YI(i)), and no. of elts
c          in SDI() if SDI(1) is positive.  Require NXY .ge. 4.
c
c  KORDER  [in]  Order of the spline basis functions.  Note that the
c        polynomial degree of the spline segments is one less than the
c        order.  Example:  the order of a cubic spline is 4.
c        Require KORDER .ge. 1.  Internal arrays in subroutines used put
c        an upper limit of 20 on KORDER.
c
c  NC  [in]  No. of coefficients to be determined.  This is the
c        number of degrees of freedom for the least squares problem.
c        To have a nonsingular problem one must have
c        NXY .ge. NC, and the distribution of the interior knots
c        must not be too skewed relative to the data abcissas.
c        If singularity is detected, an error message will be
c        issued by the subr that solves the band matrix.
c
c  (TKNOTS(j),j=1,NT, where NT = NC+KORDER)  [in]  This is the deBoor
c        knot sequence for definition of the spline basis functions.
c        See remarks above.
c
c  BCOEF()  [out]  An array of length NC into which the computed
c        coefficients defining the fitted curve will be stored.  These
c        are coeffients relative to B-spline basis functions.  BCOEF(I)
c        is associated with the basis function whose support interval
c        runs from TKNOTS(I) to TKNOTS(I+KORDER).
c
c  SIGFAC  [out]  The subr sets SIGFAC to RNORM / sqrt(DOF) where
c      RNORM = sqrt( sum over i of [( (yfit(i) - YI(i))/SDI(i))**2])
C          and DOF = max(1, NXY - NC)
C
c  IERR1  [out]  Error status indicator.  Note that IERR2 comes from
c         DBACC and IERR3 comes from DBSOL.
c
c        =    0 means no errors detected.
c        =  100 means  NC .lt. 1   .or.   NC .gt. NXY
c        =  200 means  TKNOTS(I) .gt. TKNOTS(I+1)
c        =  250 means  TKNOTS(I) .ge. TKNOTS(I+KORDER)
c        =  300 means  LDW .lt. NC+2
c        =  400 means  The XI's are not sorted.
c        =  600 means  Need larger dimension LDW.
c        =  700 + IERR2 means IERR2 .ne. 0
c        =  800 + IERR2 means IERR2 .ne. 0
c        =  900 + IERR2 means IERR2 .ne. 0
c        = 1000 + IERR3 means IERR3 .ne. 0 due to singularity
c                       detected in _BSOL.
c        = 1100 means SDI(1) = zero.
c        = 1200 means SDI(1) > zero and SDI(i) .le. zero for some i.
c
c  LDW  [in]  Leading dimension of W().   Must satisfy LDW .ge. NC + 2
c          Let IXMAX denote the max no. of data abcissas, XI(i),
c          in any one knot interval, i.e. between TKNOTS(j) and
c          TKNOTS(j+1) for some j.   The subr will be more efficient
c          if LDW is at least NC + 1 + IXMAX.
c
c  W()  [scratch]  Work space dimensioned W(LDW,KORDER+1).
c     ------------------------------------------------------------------
c          Important internal variables.
c
c  ISEG     Index of current spline segment.  ISEG runs from 1 to
c           NC+1-KORDER.  The knot interval associated with index ISEG
c           is T(ISEG+KORDER-1).  Note that the union of these
c           segments is the "proper fitting interval".
c           ISEG also tells the band matrix subroutine the column index
c           of the least squares matrix with which the first col
c           of the new block of data in G() is to be associated.
c           There will be KORDER basis functions that are nonzero on
c           segment ISEG.  They are indexed from ISEG to ISEG+KORDER-1.
c  KORDP1   = KORDER+1
C  KSIZE    Number of rows in current block.
C  JPOINT   Current data pointer.
c     ------------------------------------------------------------------
c--D replaces "?": ?SFIT, ?BACC, ?BSOL, ?SBASD, ?ERV1
c     Both versions use ERMSG, IERM1, IERV1
C     Other subrs needed: DHTCC, ERMSG, ERFIN
c     ------------------------------------------------------------------
      integer I, IERR1, IERR2, IERR3, IRNOW, ISEG, ISGMAX
      integer J, JPOINT, JPT, JTPREV
      integer K, KMAX, KORDER, KORDP1, KSIZE, LDW, LEFT, NXY
      integer NBAND, NC, NT, NXYP1
      parameter(KMAX=20)
      double precision BCOEF(NC), DOF, ONE, P(KMAX), RNORM
      double precision SDI(*), sdijp, SIGFAC
      double precision TKNOTS(NC+KORDER), W(LDW, KORDER+1), WT, WT1
      double precision XI(NXY), YI(NXY), ZERO
      logical  DIRECT, USEWT1
      parameter( ONE = 1.0D0, ZERO = 0.0D0)
c     ------------------------------------------------------------------
      NBAND = KORDER
      KORDP1 = KORDER+1
      NT = NC + KORDER
      ISGMAX = NC + 1 -KORDER
      IERR1 = 0
C
C          Exit immediately if NC .lt. 1  or  NXY .lt. NC  or if the
C          knots fail to be nondecreasing.
c
      IF ( NC .lt. 1   .or.   NXY .lt. NC) THEN
         IERR1 = 100
         call IERM1('DSFIT',IERR1,0,
     *   'Require NC .ge. 1 and NXY .ge. NC', 'NC',NC,',')
         call IERV1('NXY',NXY,'.')
         go to 200
      END IF
      if(KORDER .gt. KMAX) then
         IERR1 = 150
         call IERM1('DSFIT',IERR1,0,'Require KORDER .le. KMAX.',
     *   'KORDER',KORDER,',')
         call IERV1('KMAX',KMAX,'.')
         go to 200
      end if
c
      do 10 I = 1,NT-1
        IF (TKNOTS(I) .gt. TKNOTS(I+1)) THEN
          IERR1 = 200
          call IERM1('DSFIT',IERR1,0,
     *    'Require knots, TKNOTS(I), to be nondecreasing.',
     *    'I',I,',')
          call DERV1('TKNOTS(I)',TKNOTS(I),',')
          call DERV1('TKNOTS(I+1)',TKNOTS(I+1),'.')
          go to 200
        END IF
   10 continue
c
      do 20 I = 1,NC
        IF (TKNOTS(I) .ge. TKNOTS(I+KORDER)) THEN
          IERR1 = 250
          call IERM1('DSFIT',IERR1,0,
     *    'Require TKNOTS(I) < TKNOTS(I+KORDER).',
     *    'I',I,',')
          call DERV1('TKNOTS(I)',TKNOTS(I),',')
          call DERV1('TKNOTS(I+KORDER)',TKNOTS(I+KORDER),'.')
          go to 200
        END IF
   20 continue
C
C     Require LDW .ge. NC+2
C
      IF (LDW .lt. NC+2) THEN
        IERR1 = 300
        call IERM1('DSFIT',IERR1,0,'Require LDW .ge. NC+2',
     *             'LDW',LDW,',')
        call IERV1('NC',NC,'.')
        go to 200
      END IF
C
c     ------------------------------------------------------------------
C                                       TEST SDI(1)
      IF(SDI(1) .lt. ZERO) THEN
         WT1 = -ONE/SDI(1)
         USEWT1 = .true.
      ELSEIF( SDI(1) .gt. ZERO) THEN
         USEWT1 = .false.
      ELSE
         IERR1 = 1100
         call ERMSG('DSFIT',IERR1,0,'Require SD(1) .ne. Zero','.')
         return
      END IF
C
c                             Test ordering of XI() array.
c
      if(XI(NXY) .ge. XI(1)) then
         DIRECT = .true.
      else
         DIRECT = .false.
         NXYP1 = NXY+1
      end if
      IERR1 = 0
      do 40 I=2,NXY
         if(DIRECT) then
            if (XI(I-1) .gt. XI(I)) then
               IERR1 = 400
               go to 50
            end if
         else
            if (XI(I-1) .lt. XI(I)) then
               IERR1 = 400
               go to 50
            end if
         end if
   40 continue
   50 continue
      if(IERR1 .ne. 0) then
         call IERM1('DSFIT',IERR1,0,
     *   'Require abcissas, X(), to be sorted.',
     *   'I',I,',')
         call IERV1('NXY',NXY,',')
         call DERV1('  X(1)',XI(1),',')
         call DERV1('X(I-1)',XI(I-1),',')
         call DERV1('  X(I)',XI(I),',')
         call DERV1('X(NXY)',XI(NXY),'.')
         go to 200
      end if
C
C     Begin loop to form equations for least-squares spline fit.
C
      IRNOW = 1
      K = 1
      KSIZE = 0
      ISEG = 1
      LEFT = ISEG + KORDER - 1
      do 80 JPT = 1, NXY
         if(DIRECT) then
            JPOINT = JPT
         else
            JPOINT = NXYP1-JPT
         end if
         IF( K .gt. LDW ) THEN
            call DBACC(W, LDW, NBAND, IRNOW, KSIZE, ISEG, JTPREV, IERR2)
            if(IERR2 .ne. 0) then
               IERR1 = 700 + IERR2
               go to 100
            end if

            IF(IRNOW .gt. LDW) then
               IERR1 = 600
               call IERM1('DSFIT',IERR1,0,'Require LDW .ge. NC+2',
     *             'LDW',LDW,',')
               call IERV1('NC',NC,'.')
               go to 200
            END IF
            K = IRNOW
            KSIZE = 0
         END IF
c
C        DO WHILE(XI(JPOINT) .ge. TKNOTS(LEFT+1) .and. ISEG .lt. ISGMAX)
   60    if (XI(JPOINT) .ge. TKNOTS(LEFT+1) .and. ISEG .lt. ISGMAX) then
            call DBACC(W, LDW, NBAND, IRNOW, KSIZE, ISEG, JTPREV, IERR2)
            if(IERR2 .ne. 0) then
               IERR1 = 800 + IERR2
               go to 100
            end if
            KSIZE = 0
            K = IRNOW
            ISEG = ISEG + 1
            LEFT = LEFT + 1
            go to 60
         end if
C        END WHILE
c
c                                 Build one equation
         call DSBASD(KORDER, LEFT, TKNOTS, XI(JPOINT), 0, P)
         IF( USEWT1) THEN
            WT = WT1
         ELSE
            sdijp = sdi(jpoint)
            if(sdijp .gt. ZERO) then
               WT=ONE/sdijp
            else
               IERR1 = 1200
               call ERMSG('DSFIT',IERR1,0,
     *            'With SD(1) > 0  require all SD(I) > 0.', ',')
               call DERV1('SD(1)',SDI(1),',')
               call IERV1('I',jpoint,',')
               call DERV1('SD(I)',sdijp,'.')
               return
            end if
         END IF
         do 70 J = 1,KORDER
            W(K,J)=P(J)*WT
   70    continue
         W(K,KORDP1)=YI(JPOINT)*WT
c                                 End of build one equation
         K = K+1
         KSIZE = KSIZE + 1
c
   80 continue
      call DBACC(W, LDW, NBAND, IRNOW, KSIZE, ISEG, JTPREV, IERR2)
            if(IERR2 .ne. 0) then
               IERR1 = 900 + IERR2
               go to 100
            end if
C
C     ALL DATA POINTS HAVE BEEN PROCESSED.  CALL FOR SOLUTION.
C
      call DBSOL(1,W, LDW, NBAND, IRNOW, JTPREV, BCOEF,
     *           NC, RNORM, IERR3)
            if(IERR3 .ne. 0) then
               IERR1 = 1000 + IERR2
               call ERMSG('DSFIT',IERR1,0,
     *         'Singularity noted in DBSOL.','.')
               go to 200
            end if

      DOF = MAX(1, NXY - NC)
      SIGFAC = RNORM / sqrt(DOF)
      RETURN
C
C                                 ERROR IN _BACC
  100 call ERMSG('DSFIT',IERR1,0,
     *   'Error detected in subroutine DBACC','.')
C
C                                 ERROR RETURN
  200 do 220 I=1,NC
         BCOEF(I)=ZERO
  220 continue
      RETURN
      END
