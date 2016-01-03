      program drsilu
c>> 2001-05-22 DRSILU Krogh Minor change for making .f90 version.
c>> 1994-12-21 DRSILU  Krogh  Latest version.
c--S replaces "?": DR?ILU, ?ILUP
c Demonstration driver for SILUP.
c Given table of sin(x), x = 0, .5, 1., ..., interpolates for x =
c -.1, .1, and, 8.3 using polynomial degrees 2, 3, 4, ..., 10.  Errors
c are estimated in all cases.
c
      integer NTAB, NX, NDEG1, NDEG2
      parameter (NTAB=40, NX=3, NDEG1=2, NDEG2=10)
      real             XT(2), YT(NTAB), X(NX), Y, EOPT(1), H, ANS
      parameter (H = .5E0)
      integer I, NDEG, IOPT(3), LUP
      data X / -.1E0, .1E0, 8.3E0 /
c                       Set IOPT to get an error estimate.
      data IOPT / 0, 1, 0 /
      data LUP / 3 /
c
c Compute the XT and YT tables
      XT(1) = 0.E0
      XT(2) = H
      do 10 I = 1, NTAB
         YT(I) = sin(real(I-1) * H)
   10 continue
      print *, 'IOP(1) NDEG    X         Y     Est. Error True Error'
      do 30 I = 1, NX
         ANS = sin(X(I))
         do 20 NDEG = NDEG1, NDEG2
            call SILUP (X(I), Y, NTAB, XT, YT, NDEG, LUP, IOPT, EOPT)
            print '(I5, I5, F9.4, F12.8, 1P,E10.2, E11.2)',
     1       IOPT(1), NDEG, X(I), Y, EOPT(1), Y - ANS
   20    continue
      print *
   30 continue
      stop
      end
