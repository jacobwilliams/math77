      real             function SRANE(STDDEV)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 SRANE Krogh  Changes to use M77CON
c>> 1994-06-24 SRANE CLL Changed common to use RANC[D/S]1 & RANC[D/S]2.
c>> 1992-03-16 CLL
c>> 1991-11-26 CLL Reorganized common. Using RANCM[A/D/S].
c>> 1991-11-22 CLL Added call to RAN0, and SGFLAG in common.
c>> 1991-01-15 CLL Reordered common contents for efficiency.
C>> 1990-01-23 CLL  Making names in common same in all subprogams.
C>> 1987-04-22 SRANE  Lawson  Initial code.
c        Returns one pseudorandom number from the Exponential
c     distribution with standard deviation, STDDEV, which should be
c     positive.
c     If U is random, uniform on [0, 1], the Exponential variable is
c     given by             SRANE = -STDDEV * log(U)
c     This variable has    mean = STDDEV
c     and                  variance = STDDEV**2
c     Reference: NBS AMS 55, P. 930.
c     Code based on subprogram written for JPL by Stephen L. Ritchie,
c     Heliodyne Corp. and Wiley R. Bunton, JPL, 1969.
c     Adapted to Fortran 77 for the JPL MATH77 library by C. L. Lawson &
c     S. Y. Chiu, JPL, Apr 1987.
c     ------------------------------------------------------------------
c--S replaces "?": ?RANE, ?RANUA, RANC?1, RANC?2, ?PTR, ?NUMS, ?GFLAG
C     RANCS1 and RANCS2 are common blocks.
c     Calls RAN0 to initialize SPTR and SGFLAG.
c     ------------------------------------------------------------------
      integer M
      parameter(M = 97)
      real             SNUMS(M), STDDEV
      common/RANCS2/SNUMS
      integer SPTR
      logical SGFLAG
      common/RANCS1/SPTR, SGFLAG
      save  /RANCS1/, /RANCS2/, FIRST
      logical FIRST
      data    FIRST / .true. /
c     ------------------------------------------------------------------
      if(FIRST) then
         FIRST = .false.
         call RAN0
      endif
c
      SPTR = SPTR - 1
      if(SPTR .eq. 0) then
         call SRANUA(SNUMS, M)
         SPTR = M
      endif
      SRANE = -STDDEV * log(SNUMS(SPTR))
      return
      end
