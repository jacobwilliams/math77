      program DRDCFT
c>> 1996-06-05 DRDCFT Krogh Fixes for conversion to C.
c>> 1994-10-19 DRDCFT Krogh  Changes to use M77CON
c>> 1989-05-07 DRDCFT FTK, CLL
c     Driver to demonstrate DCFT -- Complex Fast Fourier Transform
c     ------------------------------------------------------------------
c--D replaces "?": DR?CFT, ?CFT
c     ------------------------------------------------------------------
      double precision    F(256)
      double precision S(31), T, DELTAT, TTIME, PI, TWOPI, RTWO
      double precision  ZERO, FOUR, P1, P3
      integer    I, K, KF, M, MS, N, ND, MA(1)
      parameter (PI = 3.1415926535897932384D0)
      parameter (TWOPI = 2.D0 * PI)
      parameter (RTWO = 1.4142135623730950488D0)
      parameter (M = 7)
c      parameter (N = 2**M)
      parameter (N = 128)
      parameter (ND = 1)
      parameter (ZERO = 0.D0)
      parameter (FOUR = 4.D0)
      parameter (P1 = .1D0)
      parameter (P3 = .3D0)
      data  TTIME /25.D0/
      data MA / M /
c     ------------------------------------------------------------------
      print*,'Program DRDCFT..  Demonstrates DCFT'
      DELTAT = TTIME / N
      T = ZERO
      do 100 K = 1, 2*N, 2
         F(K)=SIN(TWOPI*(T+P1))+FOUR*COS(TWOPI*(RTWO*T+P3))
         F(K+1) = ZERO
         T = T + DELTAT
  100 continue
      MS = 0
      call DCFT (F, 'A', MA, ND, MS, S)
      print'(1X/A/1X)',' TRANSFORM FOR K=21 TO 41 (NO SIGMA FACTORS)'
      do 120 K = 21, 41, 3
         KF = 2*K-1
         print'(A,I3,A,I3,3(F11.5,F9.5))',
     *        ' K=',K,' TO',K+2,(F(I), I=KF, KF+5)
  120 continue
      stop
      end
