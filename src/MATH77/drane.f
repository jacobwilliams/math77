      double precision function DRANE(STDDEV)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 DRANE Krogh  Changes to use M77CON
c>> 1994-06-24 DRANE CLL Changed common to use RANC[D/S]1 & RANC[D/S]2.
c>> 1992-03-16 CLL
c>> 1991-11-26 CLL Reorganized common. Using RANCM[A/D/S].
c>> 1991-11-22 CLL Added call to RAN0, and DGFLAG in common.
c>> 1991-01-15 CLL Reordered common contents for efficiency.
C>> 1990-01-23 CLL  Making names in common same in all subprogams.
C>> 1987-04-22 DRANE  Lawson  Initial code.
c        Returns one pseudorandom number from the Exponential
c     distribution with standard deviation, STDDEV, which should be
c     positive.
c     If U is random, uniform on [0, 1], the Exponential variable is
c     given by             DRANE = -STDDEV * log(U)
c     This variable has    mean = STDDEV
c     and                  variance = STDDEV**2
c     Reference: NBS AMS 55, P. 930.
c     Code based on subprogram written for JPL by Stephen L. Ritchie,
c     Heliodyne Corp. and Wiley R. Bunton, JPL, 1969.
c     Adapted to Fortran 77 for the JPL MATH77 library by C. L. Lawson &
c     S. Y. Chiu, JPL, Apr 1987.
c     ------------------------------------------------------------------
c--D replaces "?": ?RANE, ?RANUA, RANC?1, RANC?2, ?PTR, ?NUMS, ?GFLAG
C     RANCD1 and RANCD2 are common blocks.
c     Calls RAN0 to initialize DPTR and DGFLAG.
c     ------------------------------------------------------------------
      integer M
      parameter(M = 97)
      double precision DNUMS(M), STDDEV
      common/RANCD2/DNUMS
      integer DPTR
      logical DGFLAG
      common/RANCD1/DPTR, DGFLAG
      save  /RANCD1/, /RANCD2/, FIRST
      logical FIRST
      data    FIRST / .true. /
c     ------------------------------------------------------------------
      if(FIRST) then
         FIRST = .false.
         call RAN0
      endif
c
      DPTR = DPTR - 1
      if(DPTR .eq. 0) then
         call DRANUA(DNUMS, M)
         DPTR = M
      endif
      DRANE = -STDDEV * log(DNUMS(DPTR))
      return
      end
