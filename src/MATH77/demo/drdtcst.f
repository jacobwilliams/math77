c     program DRDTCST
c>> 1996-06-05 DRDTCST Krogh Fixes for conversion to C.
c>> 1996-05-28 DRDTCST Krogh Moved formats up.
c>> 1994-10-19 DRDTCST Krogh  Changes to use M77CON
c>> 1994-08-09 DRDTCST WVS Remove '0' from format
c>> 1992-04-22 DRDTCST CAO commented program statement
c>> 1989-05-08 DRDTCST FTK, CLL
c>> 1989-05-04 DRDTCST FTK, CLL
c Driver to demonstrate DTCST
c     ------------------------------------------------------------------
c--D replaces "?": DR?TCST, ?TCST
c     ------------------------------------------------------------------
      double precision F(65), S(32), T, TTIME
      double precision DELTAT, DELTAO, OMEGA, FTRUE, SIN1,COS1
      double precision PI, ZERO, ONE
      integer    K, KSKIP, M, MS, N, ND, MA(1)
      parameter (M = 6)
c      parameter (N = 2 ** M + 1)
      parameter (N = 65)
      parameter (ND = 1)
      parameter (KSKIP = 10)
      parameter (PI = 3.1415926535897932384D0)
      parameter (ZERO = 0.D0)
      parameter (ONE = 1.D0)
      data TTIME / 10.D0 /
      data MA / M /
 1000 format (/'  K', 4X, 'OMEGA', 8X, 'COMPUTED', 9X, 'TRUE')
 1001 format (1X, I3, 1P,E13.5, 2E15.7)
c     ------------------------------------------------------------------
      SIN1 = SIN (ONE)
      COS1 = COS (ONE)
      DELTAT = TTIME / DBLE(N - 1)
      DELTAO = DBLE(KSKIP) * (PI / TTIME)
      T = ZERO
c                 Compute (TTIME / 2 ) * F(T)
      F(1) = TTIME / PI
      do 10 K = 2, N
         T = T + DELTAT
         F(K) = TTIME * SINH(T) / SINH(PI * T)
   10 continue
c
      MS = 0
      call DTCST (F, 'C', 'A', MA, ND, MS, S)
c
      OMEGA = ZERO
      write (*, 1000)
      do 20 K = 1, N, KSKIP
         FTRUE = SIN1 / (COSH(OMEGA) + COS1)
         write (*, 1001) K, OMEGA, F(K), FTRUE
         OMEGA = OMEGA + DELTAO
   20 continue
      stop
      end
