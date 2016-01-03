c     program DRSRFT1
c>> 1996-06-05 DRSRFT1 Krogh Fixes for conversion to C.
c>> 1996-05-28 DRSRFT1 Krogh Moved formats up.
c>> 1994-10-19 DRSRFT1 Krogh  Changes to use M77CON
c>> 1994-08-09 DRSRFT1 WVS Removed '0' from format
c>> 1992-04-22 DRSRFT1 CAO, commented program statement
c>> 1989-05-07 DRSRFT1 FTK, CLL
c>> 1989-05-04 DRSRFT1 FTK, CLL
c     Demo driver for SRFT1 -- One-dimensional real Fourier transform
c     ------------------------------------------------------------------
c--S replaces "?": DR?RFT1, ?RFT1
c     ------------------------------------------------------------------
      integer I, K, KLAST, M, MS, N, N2, N4
      parameter (M = 7)
c      parameter (N = 2**M)
      parameter (N = 128)
      parameter (N2 = N/2)
      parameter (N4 = N/4)
      real             F(N), S(N4-1), T, DELTAT, TTIME, SIG
      real             PI, TWOPI, RTWO, ZERO, ONE, FOUR, P1, P3
      real             PIN, TB
      parameter (PI = 3.1415926535897932384E0)
      parameter (TWOPI = 2.E0 * PI)
      parameter (RTWO = 1.4142135623730950488E0)
      parameter (ZERO = 0.E0)
      parameter (ONE = 1.E0)
      parameter (FOUR = 4.E0)
      parameter (P1 = .1E0)
      parameter (P3 = .3E0)
      data TTIME /25.E0/
 1111 format(/' Transform for K=21 to 41 using sigma factors')
 2222 format(' K=', I3, ' TO', I3, 3(1P,E12.2, E10.2))
c     ------------------------------------------------------------------
      KLAST = N2 - 1
      DELTAT = TTIME / REAL(N)
      T = ZERO
c                Get sine table for use in getting sigma factors
      MS = -1
      call SRFT1(F, 'A', M, MS, S)
      PIN = PI / REAL(N)
      do 100 K = 1, KLAST
         T = T + DELTAT
         TB = TTIME - T
c                               Get sigma factor
         if (K .GT. N4) then
            SIG = S(N2-K)
         ELSE if (K .LT. N4) then
            SIG = S(K)
         else
            SIG = ONE
         end if
         SIG = SIG / (PIN * REAL(N - 2*K))
c
c                         Compute F and apply sigma factors
c
         F(K+1) = SIG*(SIN(TWOPI*(T+P1)) + FOUR*COS(TWOPI*(RTWO*T+P3)))
         F(N-K+1)=SIG*(SIN(TWOPI*(TB+P1))+FOUR*COS(TWOPI*(RTWO*TB+P3)))
  100 continue
      F(1) = ZERO
      T = T + DELTAT
      F(N2+1) = SIN(TWOPI*(T+P1)) + FOUR*COS(TWOPI*(RTWO*T+P3))
      call SRFT1(F, 'A', M, MS, S)
      write (*, 1111)
      do 120 K = 21, 41, 3
         write (*, 2222) K, K+2, (F(I), I = 2*K-1, 2*K+4)
  120 continue
      stop
      end
