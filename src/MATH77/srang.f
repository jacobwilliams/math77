      real             function  SRANG ()
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-04-16 SRANG WVS SQRT(abs(TWO*log(U3))) avoids sqrt(-0.0)
c>> 1994-10-20 SRANG Krogh  Changes to use M77CON
c>> 1994-06-24 SRANG CLL Changed common to use RANC[D/S]1 & RANC[D/S]2.
c>> 1992-03-16 SRANG CLL
c>> 1991-11-26 SRANG CLL Reorganized common. Using RANCM[A/D/S].
c>> 1991-11-22 SRANG CLL Added call to RAN0, and SGFLAG in common.
c>> 1991-01-15 SRANG CLL Reordered common contents for efficiency.
C>> 1990-01-23 SRANG CLL Making names in common same in all subprogams.
C>> 1987-06-09 SRANG CLLawson  Initial code.
C
c     Returns one pseudorandom number from the Gausian (Normal)
c     distribution with zero mean and unit standard deviation.
c     Method taken from Algorithm 334, Comm. ACM, July 1968, p. 498.
c     Implemented at JPL in Univac Fortran V in 1969 by Wiley R. Bunton
c     of JPL and Stephen L. Richie of Heliodyne Corp.
c
c     Adapted to Fortran 77 for the MATH 77 library by C. L. Lawson and
c     S. Y. Chiu, JPL, April 1987, 6/9/87.
C     ------------------------------------------------------------------
c--S replaces "?": ?RANG, ?RANUA, RANC?1, RANC?2, ?PTR, ?NUMS, ?GFLAG
C     RANCS1 and RANCS2 are common blocks.
c     Uses intrinsic functions, log and sqrt.
C     Calls SRANUA to obtain an array of uniform random numbers.
c     Calls RAN0 to initialize SPTR and SGFLAG.
C     ------------------------------------------------------------------
c                        Important common variables
c
c     SPTR [integer] and SGFLAG [logical]
c
c          Will be set to SPTR = 1 and SGFLAG = .false.
c          when RAN0 is called from this subr if this
c          is the first call to RAN0.  Also set to these values when
c          RAN1 or RANPUT is called by a user.
c
c          SGFLAG will be set true on return from this subr when the
c          algorithm has values that it can save to reduce the amount
c          of computation needed the next time this function is
c          referenced.  Will be set false in the contrary case.
c
c          SPTR is the index of the last location used in the
c          common array SNUMS().  This index is counted down.
c
c     SNUMS() [floating point]  Buffer of previously computed uniform
c          random numbers.
C     ------------------------------------------------------------------
      integer M
      parameter(M = 97)
      real                ONE, TWO
      parameter(ONE = 1.0E0, TWO = 2.0E0)
      real                SNUMS(M), R, S, U3, X, XX, Y, YY
      logical FIRST
      common/RANCS2/SNUMS
      integer SPTR
      logical SGFLAG
      common/RANCS1/SPTR, SGFLAG
      save  /RANCS1/, /RANCS2/, FIRST
      save    R, X, Y
      data  FIRST/.true./
c     ------------------------------------------------------------------
      if(FIRST) then
         FIRST = .false.
         call RAN0
      endif
c
      if (.not. SGFLAG .or. SPTR .eq. 1) then
C
C     Use the Von Neuman rejection method for choosing a random point
c     (X,Y) in the unit circle, X**2 + Y**2 .le. 1.0.
c     Then the angle Theta = arctan(Y/X) is random, uniform in
c     (-Pi/2, Pi/2), and Phi = 2*Theta is random, uniform in (-Pi, Pi).
C     Define S = X**2 + Y**2, then
c     sin(Theta) = Y/sqrt(S),    cos(Theta) = X/sqrt(S),
c     sin(Phi) = 2*X*Y/S,    and cos(Phi) = (X**2 - Y**2)/S.
c
   10    continue
c                              Set X = random, uniform in [0., 1.]
      SPTR = SPTR - 1
      if(SPTR .eq. 0) then
         call SRANUA(SNUMS, M)
         SPTR = M
      endif
            X = SNUMS(SPTR)
c                              Set Y = random, uniform in [-1., 1.]
            SPTR = SPTR - 1
            if(SPTR .eq. 0) then
               call SRANUA(SNUMS,M)
               SPTR = M
            endif
            Y = TWO*SNUMS(SPTR) - ONE
c
            XX=X*X
            YY=Y*Y
            S=XX+YY
         if(S .gt. ONE) go to 10
C
C     Choose R randomly from Chi-squared distribution and
c     normalize with S.
C
c                              Set U3 = random, uniform in [0., 1.]
         SPTR = SPTR - 1
         if(SPTR .eq. 0) then
            call SRANUA(SNUMS,M)
            SPTR = M
         endif
         U3 = SNUMS(SPTR)
c        Changed -TWO*log(U3) to abs(TWO*log(U3)) because Lahey LF90
c        2.00 on a pentium produced -0.0 for -TWO*log(1.0), then got a
c        floating point exception on sqrt(-0.0).
         R = sqrt(abs(TWO*(log(U3))))/S
C
C                                Compute result as  R*Sin(PHI)
C
         SRANG = (XX-YY)*R
         SGFLAG = .true.
         return
      endif
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c        Come here when SGFLAG is true and SPTR is not 1.
c
C                                Compute result as  R*Cos(PHI)
      SRANG = TWO*X*Y*R
      SGFLAG=.false.
      return
      end
