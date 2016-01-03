      subroutine RN1
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2006-04-16 RANPK2 Krogh Fixed invalid mode message.
c>> 2005-12-21 RANPK2 Krogh Changec ivalid to invalid is stops
c>> 2005-12-07 RANPK2 Krogh Fixed bug in C conversion.
c>> 1997-12-17 RANPK2 Krogh Removed unreferenced labels
c>> 1995-11-22 RANPK2 Krogh Removed multiple entries.
c>> 1992-03-17 CLL Moved SAVE stmt ahead of DATA stmts.
c>> 1992-03-09 CLL Removed "save FIRST" because FIRST is not used.
c>> 1992-03-02 CLL Fix error: Set MODE = 1 in data stmt.
c>> 1991-11-21 CLL Add MODE with values 1, 2, 3, & 4.
c>> 1989-09-11 CLL Multiversion file. RANPK2 or RANPK3
c>> 1987-05-05 RANPK2 Lawson  Initial code.
c
c        This program unit, along with RANPK1, supports random number
c     generation.
c
c     The functionality of this random number package was modeled on the
c     specifications of the random number routines RANDOM and RANDOMSEED
c     in the February, 1987 working draft of the Fortran 8x language
c     standard.  This functionality remains similar to the later draft,
c     Fortran 90, S8.115, June 1990, in which the routine names have
c     been changed to RANDOM_MUMBER and RANDOM_SEED.  This should
c     facilitate replacement of use of this package by Fortran intrinsic
c     when and if Fortran 90 compilers come into widespread use.
c
c     The library user may call RANSIZ or RANGET in this prog unit,
c     or RAN1 or RANPUT in RANPK1 to obtain the functionality of
c     RANDOM_SEED of Fortran Fixed bug in C conversion.90.  This relates to sett
c     the seed.
c        Entries RN1 and RNPUT in this prog unit should not be called by
c     library users.  They are intended only to be called from RANPK1.
c        Entry RN2 returns the value of MODE.  This is a convenience in
c     case one is interested in knowing the value of MODE the package
c     has selected.
c        Entries SRANUA and DRANUA (s.p. and d.p. respectively)
c     generate arrays of pseudorandom numbers from the uniform
c     distribution in the range [0., 1.].  These may be called by users
c     and are called by other library routines.
c        Entries SRANUS and DRANUA (s.p. and d.p. respectively)
c     generate arrays of pseudorandom numbers from the uniform
c     distribution in the range [0., 1.] and then transformed to
c     A + B*U.  These are intended for direct use by users.
c     ------------------------------------------------------------------
c              Algorithm for generating pseudorandom numbers
c                     from the uniform distribution.
c
c  The current integer value in the random integer sequence is XCUR,
c  and the next is defined mathematically by the statement:
c
c                 XCUR = mod(AFAC * XCUR,  MDIV)
c  where
c                 MDIV = m = 6_87194_76503 = 2**36 - 233
c  and
c                 AFAC = a = 612_662 = (approx.) 0.58 * 2**20
c
c  XCUR may be any integer value in the range from 1 to m-1, and all
c  integer values in this range will be generated before the sequence
c  cycles.
c
c  We call the above computational algorithm for XCUR the "short"
c  algorithm.  There is also a "long" algorithm that produces exactly
c  the same sequence of values of XCUR:
c                  Q = aint(XCUR/B)
c                  R = XCUR - Q * B
c                  XCUR = AFAC * R - C * Q
c                  do while(XCUR .lt. 0.0)
c                     XCUR = XCUR + MDIV
c                  end do
c  where B and C are constants related to MDIV and AFAC by
c            MDIV = B * AFAC + C
c  We use B = 112165 and C = 243273.  The average number of executions
c  of the statement XCUR = XCUR + MDIV is 1.09 and the maximum number of
c  executions is 3.
c
c  The largest number that must be handled in the "short" algorithm
c  is the product of AFAC  with the max value of XCUR, i.e.,
c    612_662 * 6_87194_76502 = 42_10181_19126_68324 ~= 0.58 * 2**56.
c  Thus this algorithm requires arithmetic exact to at least 56 bits.
c
c  The largest number that must be handled in the "long" algorithm
c  is the product of C with the max value of aint(XCUR/B), i.e.,
c               243273 * 612664 ~= 0.14904e12 ~= 0.54 * 2**38.
c  Thus this algorithm requires arithmetic exact to at least 38 bits.
c
c  To accommodate different compiler/computer systems this program unit
c  contains code for 3 different ways of computing the new XCUR from the
c  old XCUR, each producing exactly the same sequence of of XCUR.
c
c  Initially we have MODE = 1.  When MODE = 1 the code does tests to
c  see which of three implementation methods will be used, and sets
c  MODE = 2, 3, or 4 to indicate the choice.
c
c  Mode 2 will be used in machines such as the Cray that have at
c  least a 38 bit significand in SP arithmetic.  XCUR will be advanced
c  using the "long" algorithm in SP arithmetic.
c
c  Mode 3 will be used on machines that don't meet the Mode 2 test,
c  but can maintain at least a 56 bits exactly in computing
c  mod(AFAC*XCUR, MDIV) in DP arithmetic.  This includes VAX, UNISYS,
c  IBM 30xx, and some IEEE machines that have clever compilers that
c  keep an extended precision representation of the product AFAC*XCUR
c  within the math processor for use in the division by MDIV.  XCUR will
c  be advanced using the "short" algorithm in DP arithmetic.
c
c  Mode 4 will be used on machines that don't meet the Mode 2 or 3
c  tests, but have at least a 38 bit significand in DP arithmetic.
c  This includes IEEE machines that have not-so-clever compilers.
c  XCUR is advanced using the "long" algorithm in DP arithmetic.
c  ---------------------------------------------------------------------
c               Properties of the generated sequence.
c
c        This m is one of the prime numbers cited in
c     Table 1, Page 390, of Knuth, Seminumerical Algorithms, Second
c     edition, 1981, Addison-Wesley.
c     The prime factorization of m-1 is
c           m-1 = p1 * p2 * p3 = 2 * 43801 * 784_451
c     The complementary factors are
c           q(1) = 3_43597_38251, q(2) = 15_68902, and q(3) = 87602.
c
c     The value a is a primitive root of m as is verified by
c     computing a**q(i) mod m for i = 1,3, and finding these values are
c     not 1.  These values are m-1, 2_49653_21011, and 1_44431_31136.
c     The fact that a is a primitive root of m assures that the period
c     of the generator is m-1, i.e. starting with any integer from 1
c     through m-1, all integers in this range will be produced.
c
c     The value a has relatively large values of the measures nu and mu
c     computed for the Spectral Test as described in Knuth, pp. 89-105.
c        (Log10(nu(i)), i=2,6) = 5.4, 3.6,  2.6,  2.2,  1.8
c        (mu(i), i=2,6)        = 3.0, 3.05, 3.39, 4.55, 6.01
c     This assures that the generated sequence will have relatively low
c     autocorrelation.
c     ------------------------------------------------------------------
c                          Alternative algorithm
c
c  An alternative set of constants that has been used widely in
c  commercial and public domain software packages is
c               m = 21474_83647 = 2**31 - 1
c               a = 16807       = 7**5 = (approx.) 0.513 * 2**15
c
c  The largest product that must be handled exactly is approximately
c  0.513 * 2**46 which is approximately  0.36E14.  This is within the
c  double-precision capability of most computer systems.
c
c  The sequence can be started with any integer from 1 through m-1
c  and will generate all integers in this range.  The autocorrelation
c  properties of the whole sequence will not be as good as with the
c  larger values for m and a.
c     ------------------------------------------------------------------
C     C. L. Lawson, F. T. Krogh & S. Chiu, JPL, July 1986, Apr 1987.
c     ------------------------------------------------------------------
c                    Common Block
      integer MODE
      real XCURSP
      double precision XCURDP
      common / RANCOM / XCURDP, XCURSP, MODE
c              These same parameters are also defined below in RANMOD.
      double precision X1DP
      real             X1SP
      parameter( X1DP=123456789.0D0, X1SP=123456789.0e0 )
c     ------------------------------------------------------------------
c                         Entered using CALL RN1
c              This entry should not be called by general users.
c              User should call RAN1 in RANPK1.
c
         XCURDP = X1DP
         XCURSP = X1SP
         return
         end
c     ------------------------------------------------------------------
      subroutine RANSIZ(KSIZE)
         integer KSIZE
         KSIZE = 2
         return
         end
c     ------------------------------------------------------------------
c                         Entered using CALL RNPUT(KSEED)
c              This entry should not be called by general users.
c              User should call RANPUT(KSEED) in RANPK1.
c
      subroutine RNPUT(KSEED)
      integer KSEED(2)
c                    Common Block
      integer MODE
      real XCURSP
      double precision XCURDP
      common / RANCOM / XCURDP, XCURSP, MODE
c
      double precision MDIVDP, SCALDP
      real             MDIVSP, SCALSP
      parameter( MDIVDP=68719476503.0D0, MDIVSP=68719476503.0e0 )
      parameter( SCALDP=100000.0D0, SCALSP=100000.0e0 )
      logical FIRST
      save FIRST
      data FIRST / .true. /
c
      if (FIRST) then
         FIRST = .false.
         call RANMOD
      end if
      if(MODE .eq. 2) then
         XCURSP = SCALSP * real(abs(KSEED(1))) + real(abs(KSEED(2)))
         XCURSP = max(1.e0, min(XCURSP, MDIVSP - 1.0e0))
      else
c                            Here for MODE = 3 or 4
         XCURDP = SCALDP * dble(abs(KSEED(1))) + dble(abs(KSEED(2)))
         XCURDP = max(1.D0, min(XCURDP, MDIVDP - 1.0D0))
      end if
      return
      end
c     ------------------------------------------------------------------
      subroutine RANGET(KSEED)
      integer KSEED(2)
c                    Common Block
      integer MODE
      real XCURSP
      double precision XCURDP
      common / RANCOM / XCURDP, XCURSP, MODE
c
      double precision SCALDP
      real             SCALSP
      parameter( SCALDP=100000.0D0, SCALSP=100000.0e0 )
      logical FIRST
      save FIRST
      data FIRST / .true. /
c
      if (FIRST) then
         FIRST = .false.
         call RANMOD
      end if
      if(MODE .eq. 2) then
         KSEED(1) = int(XCURSP/SCALSP)
         KSEED(2) = int(XCURSP - SCALSP * real(KSEED(1)))
      else
c                            Here for MODE = 3 or 4
c
         KSEED(1) = int(XCURDP/SCALDP)
         KSEED(2) = int(XCURDP - SCALDP * dble(KSEED(1)))
      end if
      return
      end
c     ------------------------------------------------------------------
      subroutine RN2(MODE1)
      integer MODE1
c                    Common Block
      integer MODE
      real XCURSP
      double precision XCURDP
      common / RANCOM / XCURDP, XCURSP, MODE
c
      logical FIRST
      save FIRST
      data FIRST / .true. /
c
      if (FIRST) then
         FIRST = .false.
         call RANMOD
      end if
      MODE1 = MODE
      return
      end
c     ------------------------------------------------------------------
      subroutine SRANUA(USP, N)
      integer N
      real USP(N)
c                    Common Block
      integer MODE
      real XCURSP
      double precision XCURDP
      common / RANCOM / XCURDP, XCURSP, MODE
c
      integer I
      double precision AFACDP, BFACDP, C2DP, MDIVDP, QDP, RDP
      real             AFACSP, BFACSP, C2SP, MDIVSP, QSP, RSP
      parameter( AFACDP=612662.0D0, BFACDP=112165.0d0,
     *   C2DP=243273.0d0, MDIVDP=68719476503.0D0 )
      parameter( AFACSP=612662.0e0, BFACSP=112165.0e0,
     *   C2SP=243273.0e0, MDIVSP=68719476503.0e0 )
      logical FIRST
      save FIRST
      data FIRST / .true. /
c
      if (FIRST) then
         FIRST = .false.
         call RANMOD
      end if
      go to (310, 320, 330, 340), MODE
  310 print '(''In SRANUA inside RANPK2 invalid MODE ='', I10)', MODE
      stop
c                                         Mode 2.
  320 continue
      do 325 I = 1,N
         QSP = aint(XCURSP / BFACSP)
         RSP = XCURSP - QSP * BFACSP
         XCURSP = AFACSP * RSP - C2SP * QSP
  322    if(XCURSP .lt. 0.0e0) then
            XCURSP = XCURSP + MDIVSP
            go to 322
         end if
         USP(I) =  XCURSP/MDIVSP
  325 continue
      go to 350
c                                         Mode 3.
  330   continue
      do 335 I = 1,N
         XCURDP = mod(AFACDP * XCURDP,  MDIVDP)
         USP(I) = real( XCURDP)/MDIVSP
  335 continue
      go to 350
c                                         Mode 4.
  340   continue
      do 345 I = 1,N
         QDP = aint(XCURDP / BFACDP)
         RDP = XCURDP - QDP * BFACDP
         XCURDP = AFACDP * RDP - C2DP * QDP
  342    if(XCURDP .lt. 0.0d0) then
            XCURDP = XCURDP + MDIVDP
            go to 342
         end if
         USP(I) = real( XCURDP)/MDIVSP
  345 continue
  350 continue
      return
      end
c     ------------------------------------------------------------------
      subroutine DRANUA(UDP, N)
      integer N
      double precision UDP(N)
c                    Common Block
      integer MODE
      real XCURSP
      double precision XCURDP
      common / RANCOM / XCURDP, XCURSP, MODE
c
      integer I
      double precision AFACDP, BFACDP, C2DP, MDIVDP, QDP, RDP
      real             AFACSP, BFACSP, C2SP, MDIVSP, QSP, RSP
      parameter( AFACDP=612662.0D0, BFACDP=112165.0d0,
     *   C2DP=243273.0d0, MDIVDP=68719476503.0D0 )
      parameter( AFACSP=612662.0e0, BFACSP=112165.0e0,
     *   C2SP=243273.0e0, MDIVSP=68719476503.0e0 )
      logical FIRST
      save FIRST
      data FIRST / .true. /
c
      if (FIRST) then
         FIRST = .false.
         call RANMOD
      end if
      go to (410, 420, 430, 440), MODE
  410 print '(''In DRANUA inside RANPK2 invalid MODE ='', I10)', MODE
      stop
c                                         Mode 2.
  420   continue
      do 425 I = 1,N
         QSP = aint(XCURSP / BFACSP)
         RSP = XCURSP - QSP * BFACSP
         XCURSP = AFACSP * RSP - C2SP * QSP
  422    if(XCURSP .lt. 0.0e0) then
            XCURSP = XCURSP + MDIVSP
            go to 422
         end if
         UDP(I) = dble(XCURSP) / MDIVDP
  425 continue
      go to 450
c                                         Mode 3.
  430   continue
      do 435 I = 1,N
         XCURDP = mod(AFACDP * XCURDP,  MDIVDP)
         UDP(I) =  XCURDP/MDIVDP
  435 continue
      go to 450
c                                         Mode 4.
  440   continue
      do 445 I = 1,N
         QDP = aint(XCURDP / BFACDP)
         RDP = XCURDP - QDP * BFACDP
         XCURDP = AFACDP * RDP - C2DP * QDP
  442    if(XCURDP .lt. 0.0d0) then
            XCURDP = XCURDP + MDIVDP
            go to 442
         end if
         UDP(I) =  XCURDP/MDIVDP
  445 continue
  450 continue
      return
      end
c     ------------------------------------------------------------------
      subroutine SRANUS(USP, N, ASP, BSP)
      integer N
      real ASP, BSP, USP(N)
c                    Common Block
      integer MODE
      real XCURSP
      double precision XCURDP
      common / RANCOM / XCURDP, XCURSP, MODE
c
      integer I
      double precision AFACDP, BFACDP, C2DP, MDIVDP, QDP, RDP
      real             AFACSP, BFACSP, C2SP, MDIVSP, QSP, RSP
      parameter( AFACDP=612662.0D0, BFACDP=112165.0d0,
     *   C2DP=243273.0d0, MDIVDP=68719476503.0D0 )
      parameter( AFACSP=612662.0e0, BFACSP=112165.0e0,
     *   C2SP=243273.0e0, MDIVSP=68719476503.0e0 )
      logical FIRST
      save FIRST
      data FIRST / .true. /
c
      if (FIRST) then
         FIRST = .false.
         call RANMOD
      end if
      go to (510, 520, 530, 540), MODE
  510 stop'In file RANPK2, subroutine SRANUS -- Invalid value for MODE'
c                                         Mode 2.
  520   continue
      do 525 I = 1,N
         QSP = aint(XCURSP / BFACSP)
         RSP = XCURSP - QSP * BFACSP
         XCURSP = AFACSP * RSP - C2SP * QSP
  522    if(XCURSP .lt. 0.0e0) then
            XCURSP = XCURSP + MDIVSP
            go to 522
         end if
         USP(I) = ASP + BSP * XCURSP/MDIVSP
  525 continue
      go to 550
c                                         Mode 3.
  530   continue
      do 535 I = 1,N
         XCURDP = mod(AFACDP * XCURDP,  MDIVDP)
         USP(I) = ASP + BSP * real( XCURDP)/MDIVSP
  535 continue
      go to 550
c                                         Mode 4.
  540   continue
      do 545 I = 1,N
         QDP = aint(XCURDP / BFACDP)
         RDP = XCURDP - QDP * BFACDP
         XCURDP = AFACDP * RDP - C2DP * QDP
  542    if(XCURDP .lt. 0.0d0) then
            XCURDP = XCURDP + MDIVDP
            go to 542
         end if
         USP(I) = ASP + BSP * real( XCURDP)/MDIVSP
  545 continue
  550 continue
      return
      end
c     ------------------------------------------------------------------
      subroutine DRANUS(UDP, N, ADP, BDP)
      integer N
      double precision ADP, BDP, UDP(N)
c                    Common Block
      integer MODE
      real XCURSP
      double precision XCURDP
      common / RANCOM / XCURDP, XCURSP, MODE
c
      integer I
      double precision AFACDP, BFACDP, C2DP, MDIVDP, QDP, RDP
      real             AFACSP, BFACSP, C2SP, MDIVSP, QSP, RSP
      parameter( AFACDP=612662.0D0, BFACDP=112165.0d0,
     *   C2DP=243273.0d0, MDIVDP=68719476503.0D0 )
      parameter( AFACSP=612662.0e0, BFACSP=112165.0e0,
     *   C2SP=243273.0e0, MDIVSP=68719476503.0e0 )
      logical FIRST
      save FIRST
      data FIRST / .true. /
c
      if (FIRST) then
         FIRST = .false.
         call RANMOD
      end if
      go to (610, 620, 630, 640), MODE
  610 stop'In file RANPK2, subroutine DRANUS -- Invalid value for MODE'
c                                         Mode 2.
  620   continue
      do 625 I = 1,N
         QSP = aint(XCURSP / BFACSP)
         RSP = XCURSP - QSP * BFACSP
         XCURSP = AFACSP * RSP - C2SP * QSP
  622    if(XCURSP .lt. 0.0e0) then
            XCURSP = XCURSP + MDIVSP
            go to 622
         end if
         UDP(I) = ADP + BDP * dble(XCURSP) / MDIVDP
  625 continue
      go to 650
c                                         Mode 3.
  630   continue
      do 635 I = 1,N
         XCURDP = mod(AFACDP * XCURDP,  MDIVDP)
         UDP(I) = ADP + BDP * XCURDP/MDIVDP
  635 continue
      go to 650
c                                         Mode 4.
  640   continue
      do 645 I = 1,N
         QDP = aint(XCURDP / BFACDP)
         RDP = XCURDP - QDP * BFACDP
         XCURDP = AFACDP * RDP - C2DP * QDP
  642    if(XCURDP .lt. 0.0d0) then
            XCURDP = XCURDP + MDIVDP
            go to 642
         end if
         UDP(I) = ADP + BDP * XCURDP/MDIVDP
  645 continue
  650 continue
      return
      end
c     ------------------------------------------------------------------
      subroutine RANMOD
c
c     Do tests to decide whether to set the MODE = 2, 3, or 4.
c     Outcome will depend on precision of SP and DP floating point
c     arithmetic on the host system.
c                    Common Block
      integer MODE
      real XCURSP
      double precision XCURDP
      common / RANCOM / XCURDP, XCURSP, MODE
c
      integer I, J, K
      double precision TEST(0:3,0:1), DIFF
      double precision QDP, RDP, XTSTDP
      double precision AFACDP, BFACDP, C2DP, MDIVDP, X1DP
      real             QSP, RSP, XTSTSP
      real             AFACSP, BFACSP, C2SP, MDIVSP, X1SP
      parameter( AFACDP=612662.0D0, MDIVDP=68719476503.0D0,
     1 X1DP=123456789.0D0, BFACDP=112165.0d0, C2DP=243273.0d0)
      parameter( AFACSP=612662.0e0, MDIVSP=68719476503.0e0,
     1  X1SP=123456789.0e0, BFACSP=112165.0e0, C2SP=243273.0e0)
      logical DONE
      save DONE
c
      data DONE / .false. /
      data TEST / 24997965550.0d0, 68719476502.0d0,
     *            68718863841.0d0, 36962132774.0d0,
     *            43721510953.0d0,           1.0d0,
     *                 612662.0d0, 31757343729.0d0/
C
      if (DONE) return
      DONE = .true.
      do 880 MODE = 2, 4
         do 870 J = 0,1
            XTSTDP = TEST(0,J)
            XTSTSP = real(XTSTDP)
            do 860 I = 1,3
               go to (820, 830, 840),MODE-1
c                                                Test of MODE 2
  820          continue
                  QSP = aint(XTSTSP / BFACSP)
                  RSP = XTSTSP - QSP * BFACSP
                  XTSTSP = AFACSP * RSP - C2SP * QSP
                  do 822 K = 1,3
                     if(XTSTSP .ge. 0.0e0) go to 825
                       XTSTSP = XTSTSP + MDIVSP
  822               continue
  825               continue
                  DIFF = dble(XTSTSP) - TEST(I,J)
               go to 850
c                                                Test of MODE 3
  830          continue
                  XTSTDP = mod(AFACDP * XTSTDP,  MDIVDP)
                  DIFF = XTSTDP - TEST(I,J)
               go to 850
c                                                Test of MODE 4
  840          continue
                  QDP = aint(XTSTDP / BFACDP)
                  RDP = XTSTDP - QDP * BFACDP
                  XTSTDP = AFACDP * RDP - C2DP * QDP
                  do 842 K = 1,3
                     if(XTSTDP .ge. 0.0d0) go to 845
                     XTSTDP = XTSTDP + MDIVDP
  842             continue
  845             continue
                  DIFF = XTSTDP - TEST(I,J)
  850          continue
*              print'(1x,a,3i3,g11.3)', 'RANPK2.. MODE,J,I,DIFF=',
*    *         MODE,J,I,DIFF                !****** For Testing ******
               if(DIFF .ne. 0.0d0) go to 880
c                            Following line ends I loop.
  860         continue
c                            Following line ends J loop.
  870      continue
c
c        Here the computations using the current value of MODE have
c        passed all tests, so we accept this value of MODE.
         XCURDP = X1DP
         XCURSP = X1SP
         return
c
*        print*,'From RANPK2.. MODE =',MODE  !***** For Testing *****
c                            Following line ends MODE loop.
  880 continue
c        The computations were unsuccessful for all values of MODE.
c        This means this random number package will not work on the
c        current host system.  ****** Fatal Error Stop ******
c
      call ERMSG('RANPK2',1, 2,
     *'This random no. code will not work on this computer system.','.')
      return
      end

