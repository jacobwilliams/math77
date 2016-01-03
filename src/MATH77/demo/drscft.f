      program DRSCFT
c>> 1996-06-05 DRSCFT Krogh Fixes for conversion to C.
c>> 1994-10-19 DRSCFT Krogh  Changes to use M77CON
c>> 1989-05-07 DRSCFT FTK, CLL
c     Driver to demonstrate SCFT -- Complex Fast Fourier Transform
c     ------------------------------------------------------------------
c--S replaces "?": DR?CFT, ?CFT
c     ------------------------------------------------------------------
      real                F(256)
      real             S(31), T, DELTAT, TTIME, PI, TWOPI, RTWO
      real              ZERO, FOUR, P1, P3
      integer    I, K, KF, M, MS, N, ND, MA(1)
      parameter (PI = 3.1415926535897932384E0)
      parameter (TWOPI = 2.E0 * PI)
      parameter (RTWO = 1.4142135623730950488E0)
      parameter (M = 7)
c      parameter (N = 2**M)
      parameter (N = 128)
      parameter (ND = 1)
      parameter (ZERO = 0.E0)
      parameter (FOUR = 4.E0)
      parameter (P1 = .1E0)
      parameter (P3 = .3E0)
      data  TTIME /25.E0/
      data MA / M /
c     ------------------------------------------------------------------
      print*,'Program DRSCFT..  Demonstrates SCFT'
      DELTAT = TTIME / N
      T = ZERO
      do 100 K = 1, 2*N, 2
         F(K)=SIN(TWOPI*(T+P1))+FOUR*COS(TWOPI*(RTWO*T+P3))
         F(K+1) = ZERO
         T = T + DELTAT
  100 continue
      MS = 0
      call SCFT (F, 'A', MA, ND, MS, S)
      print'(1X/A/1X)',' TRANSFORM FOR K=21 TO 41 (NO SIGMA FACTORS)'
      do 120 K = 21, 41, 3
         KF = 2*K-1
         print'(A,I3,A,I3,3(F11.5,F9.5))',
     *        ' K=',K,' TO',K+2,(F(I), I=KF, KF+5)
  120 continue
      stop
      end
