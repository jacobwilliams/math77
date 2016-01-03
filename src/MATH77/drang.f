      double precision function  DRANG ()
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-04-16 DRANG WVS SQRT(abs(TWO*log(U3))) avoids sqrt(-0.0)
c>> 1994-10-20 DRANG Krogh  Changes to use M77CON
c>> 1994-06-24 DRANG CLL Changed common to use RANC[D/S]1 & RANC[D/S]2.
c>> 1992-03-16 DRANG CLL
c>> 1991-11-26 DRANG CLL Reorganized common. Using RANCM[A/D/S].
c>> 1991-11-22 DRANG CLL Added call to RAN0, and DGFLAG in common.
c>> 1991-01-15 DRANG CLL Reordered common contents for efficiency.
C>> 1990-01-23 DRANG CLL Making names in common same in all subprogams.
C>> 1987-06-09 DRANG CLLawson  Initial code.
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
c--D replaces "?": ?RANG, ?RANUA, RANC?1, RANC?2, ?PTR, ?NUMS, ?GFLAG
C     RANCD1 and RANCD2 are common blocks.
c     Uses intrinsic functions, log and sqrt.
C     Calls DRANUA to obtain an array of uniform random numbers.
c     Calls RAN0 to initialize DPTR and DGFLAG.
C     ------------------------------------------------------------------
c                        Important common variables
c
c     DPTR [integer] and DGFLAG [logical]
c
c          Will be set to DPTR = 1 and DGFLAG = .false.
c          when RAN0 is called from this subr if this
c          is the first call to RAN0.  Also set to these values when
c          RAN1 or RANPUT is called by a user.
c
c          DGFLAG will be set true on return from this subr when the
c          algorithm has values that it can save to reduce the amount
c          of computation needed the next time this function is
c          referenced.  Will be set false in the contrary case.
c
c          DPTR is the index of the last location used in the
c          common array DNUMS().  This index is counted down.
c
c     DNUMS() [floating point]  Buffer of previously computed uniform
c          random numbers.
C     ------------------------------------------------------------------
      integer M
      parameter(M = 97)
      double precision    ONE, TWO
      parameter(ONE = 1.0D0, TWO = 2.0D0)
      double precision    DNUMS(M), R, S, U3, X, XX, Y, YY
      logical FIRST
      common/RANCD2/DNUMS
      integer DPTR
      logical DGFLAG
      common/RANCD1/DPTR, DGFLAG
      save  /RANCD1/, /RANCD2/, FIRST
      save    R, X, Y
      data  FIRST/.true./
c     ------------------------------------------------------------------
      if(FIRST) then
         FIRST = .false.
         call RAN0
      endif
c
      if (.not. DGFLAG .or. DPTR .eq. 1) then
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
      DPTR = DPTR - 1
      if(DPTR .eq. 0) then
         call DRANUA(DNUMS, M)
         DPTR = M
      endif
            X = DNUMS(DPTR)
c                              Set Y = random, uniform in [-1., 1.]
            DPTR = DPTR - 1
            if(DPTR .eq. 0) then
               call DRANUA(DNUMS,M)
               DPTR = M
            endif
            Y = TWO*DNUMS(DPTR) - ONE
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
         DPTR = DPTR - 1
         if(DPTR .eq. 0) then
            call DRANUA(DNUMS,M)
            DPTR = M
         endif
         U3 = DNUMS(DPTR)
c        Changed -TWO*log(U3) to abs(TWO*log(U3)) because Lahey LF90
c        2.00 on a pentium produced -0.0 for -TWO*log(1.0), then got a
c        floating point exception on sqrt(-0.0).
         R = sqrt(abs(TWO*(log(U3))))/S
C
C                                Compute result as  R*Sin(PHI)
C
         DRANG = (XX-YY)*R
         DGFLAG = .true.
         return
      endif
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c        Come here when DGFLAG is true and DPTR is not 1.
c
C                                Compute result as  R*Cos(PHI)
      DRANG = TWO*X*Y*R
      DGFLAG=.false.
      return
      end
