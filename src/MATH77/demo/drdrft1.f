c     program DRDRFT1
c>> 1996-06-05 DRDRFT1 Krogh Fixes for conversion to C.
c>> 1996-05-28 DRDRFT1 Krogh Moved formats up.
c>> 1994-10-19 DRDRFT1 Krogh  Changes to use M77CON
c>> 1994-08-09 DRDRFT1 WVS Removed '0' from format
c>> 1992-04-22 DRDRFT1 CAO, commented program statement
c>> 1989-05-07 DRDRFT1 FTK, CLL
c>> 1989-05-04 DRDRFT1 FTK, CLL
c     Demo driver for DRFT1 -- One-dimensional real Fourier transform
c     ------------------------------------------------------------------
c--D replaces "?": DR?RFT1, ?RFT1
c     ------------------------------------------------------------------
      integer I, K, KLAST, M, MS, N, N2, N4
      parameter (M = 7)
c      parameter (N = 2**M)
      parameter (N = 128)
      parameter (N2 = N/2)
      parameter (N4 = N/4)
      double precision F(N), S(N4-1), T, DELTAT, TTIME, SIG
      double precision PI, TWOPI, RTWO, ZERO, ONE, FOUR, P1, P3
      double precision PIN, TB
      parameter (PI = 3.1415926535897932384D0)
      parameter (TWOPI = 2.D0 * PI)
      parameter (RTWO = 1.4142135623730950488D0)
      parameter (ZERO = 0.D0)
      parameter (ONE = 1.D0)
      parameter (FOUR = 4.D0)
      parameter (P1 = .1D0)
      parameter (P3 = .3D0)
      data TTIME /25.D0/
 1111 format(/' Transform for K=21 to 41 using sigma factors')
 2222 format(' K=', I3, ' TO', I3, 3(1P,E12.2, E10.2))
c     ------------------------------------------------------------------
      KLAST = N2 - 1
      DELTAT = TTIME / DBLE(N)
      T = ZERO
c                Get sine table for use in getting sigma factors
      MS = -1
      call DRFT1(F, 'A', M, MS, S)
      PIN = PI / DBLE(N)
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
         SIG = SIG / (PIN * DBLE(N - 2*K))
c
c                         Compute F and apply sigma factors
c
         F(K+1) = SIG*(SIN(TWOPI*(T+P1)) + FOUR*COS(TWOPI*(RTWO*T+P3)))
         F(N-K+1)=SIG*(SIN(TWOPI*(TB+P1))+FOUR*COS(TWOPI*(RTWO*TB+P3)))
  100 continue
      F(1) = ZERO
      T = T + DELTAT
      F(N2+1) = SIN(TWOPI*(T+P1)) + FOUR*COS(TWOPI*(RTWO*T+P3))
      call DRFT1(F, 'A', M, MS, S)
      write (*, 1111)
      do 120 K = 21, 41, 3
         write (*, 2222) K, K+2, (F(I), I = 2*K-1, 2*K+4)
  120 continue
      stop
      end
