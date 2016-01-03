      program drdilu
c>> 2001-05-22 DRDILU Krogh Minor change for making .f90 version.
c>> 1994-12-21 DRDILU  Krogh  Latest version.
c--D replaces "?": DR?ILU, ?ILUP
c Demonstration driver for DILUP.
c Given table of sin(x), x = 0, .5, 1., ..., interpolates for x =
c -.1, .1, and, 8.3 using polynomial degrees 2, 3, 4, ..., 10.  Errors
c are estimated in all cases.
c
      integer NTAB, NX, NDEG1, NDEG2
      parameter (NTAB=40, NX=3, NDEG1=2, NDEG2=10)
      double precision XT(2), YT(NTAB), X(NX), Y, EOPT(1), H, ANS
      parameter (H = .5D0)
      integer I, NDEG, IOPT(3), LUP
      data X / -.1D0, .1D0, 8.3D0 /
c                       Set IOPT to get an error estimate.
      data IOPT / 0, 1, 0 /
      data LUP / 3 /
c
c Compute the XT and YT tables
      XT(1) = 0.D0
      XT(2) = H
      do 10 I = 1, NTAB
         YT(I) = sin(dble(I-1) * H)
   10 continue
      print *, 'IOP(1) NDEG    X         Y     Est. Error True Error'
      do 30 I = 1, NX
         ANS = sin(X(I))
         do 20 NDEG = NDEG1, NDEG2
            call DILUP (X(I), Y, NTAB, XT, YT, NDEG, LUP, IOPT, EOPT)
            print '(I5, I5, F9.4, F12.8, 1P,E10.2, E11.2)',
     1       IOPT(1), NDEG, X(I), Y, EOPT(1), Y - ANS
   20    continue
      print *
   30 continue
      stop
      end
