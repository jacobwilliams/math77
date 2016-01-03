      subroutine SSFIND ( XT, IX1, IX2, X, LEFTI, MODE )
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 SSFIND Krogh Removed Fortran 90 comment.
c>> 1994-11-11 CLL Replaced Fortran 90 DO WHILE's with GOTO's for F77.
c>> 1994-10-20 SSFIND Krogh  Changes to use M77CON
c>> 1992-11-12 SSFIND C. L. Lawson, JPL
c>> 1992-10-23 C. L. Lawson, JPL
c>> 1988-03-16 C. L. Lawson, JPL
c
c     Require IX1 < IX2.
c     Require the values in XT() indexed from IX1 to IX2 to be
c     nondecreasing.
c     Require XT(IX1)   < XT(IX1+1).
c     Require XT(IX2-1) < XT(IX2).
c     Let A = XT(IX1) and B = XT(IX2).
c     The closed interval, [A, B], is regarded as being partitioned
c     partitioned into IX2-IX1 disjoint subintervals, with all but the
c     last being half-open, and the last one being closed:
c        [XT(I), XT(I+1)), I = IX1, ..., IX2-2
c        [XT(IX2-1), XT(IX2)]
c     Some of these intervals may have zero length, but not the first
c     and last ones.
c     This subroutine identifies the location of X with respect to these
c     subintervals by setting LEFTI and MODE as follows:
c     If X is contained in one of these segments of nonzero length,
c     set MODE = 0 and set LEFTI to be the index of the left end of the
c     segment containing X.  Thus LEFTI will satisfy
c     IX1 .le. LEFTI .le. IX2-1.
c     If X < A, set MODE = -1 and LEFTI = IX1.
c     If X > B, set MODE = +1 and LEFTI = IX2-1.
c     Issue an error message and stop if
c        X < XT(IX1+1) and XT(IX1) .ge. XT(IX1+1)  or if
c        X .ge. XT(IX2-1) and XT(IX2-1) .ge. XT(IX2)
c     ------------------------------------------------------------------
c                              Method
c
c        Saves the value of LEFTI returned on previous call in ILO.
c     Starts by checking to see if X is in this same segment.
c     If so, finishes quickly.  We assume this will be a frequently
c     occurring case.
c     If not, searches either to the left or right from this previous
c     segment, as appropriate.  During this search the stride is doubled
c     until a bracketing value is found.  Then we finish with a binary
c     search between the bracketing points.
c     ------------------------------------------------------------------
c     Based on subroutine INTERV on pp. 92-93 of A PRACTICAL GUIDE TO
c     SPLINES by Carl De Boor, Springer-Verlag, 1978.
c     Current version by C. L. Lawson, JPL, March 1988.
c     ------------------------------------------------------------------
C  XT()  [in]  This subr will access XT(i) for i = IX1, ..., IX2.  These
c        values must be nondecreasing.  Repeated values are permitted
c        except for the first two and last two.
c  IX1, IX2   [in]  Indices specifying the portion of the array XT() to
c        be considered by this subr.  Require IX1 < IX2.
c  X     [in]  Value to be looked up.
c  LEFTI [inout]  On entry LEFTI must contain an integer value.  If it
c       in the range [IX1, IX2-1] the search will start with this index.
c       Otherwise the search will start with IX1 or IX2-1.
c       On return LEFTI will be in [IX1, IX2-1], and identifies
c       the interval (of nonzero length) from X(LEFTI) to
c       X(LEFTI+1) as the reference subinterval for X.
c       LEFTI = IX1 means X < XT(IX1+1)
c       IX1 < LEFTI < IX2-1 means XT(LEFTI) .le. X .lt. XT(LEFTI+1)
c       LEFTI = IX2-1 means XT(IX2-1) .le. X
c  MODE [out]  Set to
c        -1 if X < XT(IX1)
c         0 if     XT(IX1) .le. X .le. XT(IX2)
c        +1 if                         XT(IX2) < X
c     ------------------------------------------------------------------
c--S replaces "?": ?SFIND, ?ERV1
c     Both use IERM1
c     ------------------------------------------------------------------
      integer IHI, ILO, LEFTI, IX1, IX2, MIDDLE, MODE, STEP
      real             X,  XT(1:IX2)
c     ------------------------------------------------------------------
      ILO = max(IX1, min(LEFTI, IX2-1))
      if(X .ge. XT(ILO))then
         if(X .lt. XT(ILO+1))then
            MODE = 0
            LEFTI = ILO
            return
         else
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     procedure( SEARCH TO RIGHT )

      STEP = 1
      IHI = ILO + 1
*     do while(.true.)              !  Using Fortran 90 "DO" syntax.
    5 continue
         if(IHI .eq. IX2) then
c                                            Here X .ge. XT(IX2)
            if(X .eq. XT(IX2)) then
               MODE = 0
            else
               MODE = +1
            endif
            ILO  = IX2-1
            LEFTI = ILO
            if(XT(ILO) .ge. XT(IX2)) then
               call IERM1('SSFIND',2,2,'Require T(IX2-1) < T(IX2)',
     *         'IX2',IX2,',')
               call SERV1('T(IX2-1)',XT(IX2-1),',')
               call SERV1('T(IX2)',  XT(IX2),  '.')
            endif
            return
         endif
         ILO = IHI
         IHI = min(IHI + STEP, IX2)
         if(X .lt. XT(IHI)) go to 10
         STEP = 2.0e0 * STEP
      go to 5
*     end do !while
   10 continue
c     end proc !( SEARCH TO RIGHT )
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
         endif
      else
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     procedure( SEARCH TO LEFT )

      STEP = 1
*     do while(.true.)
   15 continue
         if(ILO .eq. IX1) then
            MODE = -1
            LEFTI = IX1
            if(XT(IX1) .ge. XT(IX1+1)) then
               call IERM1('SSFIND',1,2,'Require T(IX1) < T(IX1+1)',
     *         'IX1',IX1,',')
               call SERV1('T(IX1)',  XT(IX1),  ',')
               call SERV1('T(IX1+1)',XT(IX1+1),'.')
            endif
            return
         endif
         IHI = ILO
         ILO = max(ILO - STEP, IX1)
         if(X .ge. XT(ILO)) go to 20
         STEP = 2.0e0 * STEP
      go to 15
*     end do !while
   20 continue
c     end proc !( SEARCH TO LEFT )
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      endif
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     procedure( BINARY SEARCH )

c                   Here XT(ILO) .le. X .lt. XT(IHI)
c
*     do while(.true.)
   25 continue
         MIDDLE = (ILO + IHI)/2
         if (MIDDLE .eq. ILO) go to 30
         if (X .ge. XT(MIDDLE)) then
            ILO = MIDDLE
         else
            IHI = MIDDLE
         endif
      go to 25
*     end do !while
   30 continue
      MODE = 0
      LEFTI = ILO
c     end proc !( BINARY SEARCH )
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
