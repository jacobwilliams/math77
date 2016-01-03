      integer function ISRANP(XMEAN)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2000-12-01 ISRANP Krogh  Removed unused parameter MONE.
c>> 1996-03-30 ISRANP Krogh  Added external statement, rem. F90 comment.
c>> 1995-11-16 ISRANP Krogh  Converted from SFTRAN to Fortran 77.
c>> 1994-10-19 ISRANP Krogh  Changes to use M77CON
c>> 1994-06-24 ISRANP CLL Changed common to use RANC[D/S]1 & RANC[D/S]2.
c>> 1994-06-23 CLL Changed name to I[D/S]RANP.
c>> 1992-03-16 CLL
c>> 1991-11-26 CLL Reorganized common. Using RANCM[A/D/S].
c>> 1991-11-22 CLL Added call to RAN0, and SGFLAG in common.
c>> 1991-01-15 CLL Reordered common contents for efficiency.
C>> 1990-01-23 CLL  Making names in common same in all subprogams.
C>> 1987-04-23 IRANP  Lawson  Initial code.
c        Returns one pseudorandom integer from the Poisson distribution
c     with mean and variance = XMEAN.
c        The probability of occurrence of the nonnegative integer k in
c     the Poisson distribution with mean XMEAN is

c                P(k) = exp(-XMEAN) * XMEAN**k / k!

c        Let SUM(n) denote the sum for k = 0 to n of P(k).
c     Let U be a random sample from a uniform
c     distribution on [0.0, 1.0].  The returned value of ISRANP will be
c     the smallest integer such that S(ISRANP) .ge. U.
c     This variable has mean and variance = XMEAN.
c     Reference: Richard H. Snow, Algorithm 342, Comm ACM, V. 11,
c     Dec 1968, p. 819.
c     Code based on subprogram written for JPL by Stephen L. Ritchie,
c     Heliodyne Corp. and Wiley R. Bunton, JPL, 1969.
c     Adapted to Fortran 77 for the JPL MATH77 library by C. L. Lawson &
c     S. Y. Chiu, JPL, Apr 1987.
c     ------------------------------------------------------------------
c                    Subprogram argument and result
c
c     XMEAN [in]  Mean value for the desired distribution.
c           Must be positive and not so large that exp(-XMEAN)
c           will underflow.  For example, on a machine with underflow
c           limit 0.14E-38, XMEAN must not exceed 88.
c
c     ISRANP [Returned function value]  Will be set to a nonnegative
c           integer value if computation is successful.
c           Will be set to -1 if XMEAN is out of range.
c     ------------------------------------------------------------------
c--S replaces "?": I?RANP, ?ERM1, ?ERV1, ?RANUA
c--&                 RANC?1, RANC?2, ?PTR, ?NUMS, ?GFLAG
c     Generic intrinsic functions referenced: EXP, LOG, MIN, NINT
c     Other MATH77 subprogram referenced: RAN0
c     Other MATH77 subprograms needed: ERMSG, ERFIN, AMACH
c     Common referenced: /RANCS1/, /RANCS2/
c     ------------------------------------------------------------------
      external R1MACH
      integer M, NMAX
      parameter(M = 97, NMAX = 84)
      real             ELIMIT, R1MACH
      real             S, SNUMS(M), SPREV, SUM(0:NMAX)
      real             TERM, TLAST, TWO
      real             U, XMEAN, XMSAVE, ZERO
      integer LAST, N, NMID
      logical FIRST
      parameter(ZERO = 0.0E0, TWO = 2.0E0)
      common/RANCS2/SNUMS
c
      integer SPTR
      logical SGFLAG
      common/RANCS1/SPTR, SGFLAG
c
      save   /RANCS1/, /RANCS2/
      save    ELIMIT, FIRST, LAST, NMID, SUM, TLAST, XMSAVE
      data    FIRST / .true. /
      data    ELIMIT / ZERO /
      data    XMSAVE / 0.E0 /
c     ------------------------------------------------------------------
      if(FIRST .or. (XMEAN .ne. XMSAVE)) then
c                                      Set for new XMEAN
c     R1MACH(1) gives the underflow limit.
c     ELIMIT gives a limit for arguments to the exponential function
c     such that if XMEAN .le. ELIMIT,  exp(-XMEAN) should not
c     underflow.
c
         if(ELIMIT .eq. ZERO) ELIMIT = -log(R1MACH(1) * TWO)
         if(XMEAN .le. ZERO  .or.  XMEAN .gt. ELIMIT) then
            call SERM1('ISRANP',1, 0,'Require 0 .lt. XMEAN .le. ELIMIT',
     *        'XMEAN',XMEAN,',')
            call SERV1('ELIMIT',ELIMIT,'.')
            ISRANP = -1
         else
            LAST = 0
            TLAST = exp(-XMEAN)
            SUM(0) = TLAST
            XMSAVE = XMEAN
            NMID = nint(XMEAN)
         end if
         if(FIRST) then
            FIRST = .false.
            call RAN0
         end if
      end if
c               SET U = UNIFORM RANDOM NUMBER
      SPTR = SPTR - 1
      if(SPTR .eq. 0) then
         call SRANUA(SNUMS,M)
         SPTR = M
      endif
      U = SNUMS(SPTR)
c
      if(U .gt. SUM(LAST)) then
c                          COMPUTE MORE SUMS
         N = LAST
         SPREV = SUM(LAST)
         TERM = TLAST
   50    continue
            N = N + 1
            TERM = TERM * XMEAN / real(N)
            S = SPREV + TERM
            if(S .eq. SPREV) then
               ISRANP = N
               return
            endif
            if(N .le. NMAX) then
               LAST = N
               SUM(LAST) = S
               TLAST = TERM
            endif
            if(U .le. S) then
               ISRANP = N
               return
            endif
            SPREV = S
         go to 50
      else
c                            SEARCH THROUGH STORED SUMS
c     Here we already know that U .le. SUM(LAST).
c     It is most likely that U will be near SUM(NMID).
c
         if(NMID .lt. LAST) then
            if( U .gt. SUM(NMID) ) then
               do 100 N = NMID+1, LAST
                  if(U .le. SUM(N)) then
                     ISRANP = N
                     return
                  endif
  100          continue
            endif
         endif
c
         do 150 N = min(NMID, LAST)-1, 0,-1
            if(U .gt. SUM(N)) then
               ISRANP = N+1
               return
            endif
  150    continue
         ISRANP = 0
      endif
      return
      end
