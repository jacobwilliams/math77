      program drsilm
c>> 2001-05-22 DRSILM  Krogh Minor change for making .f90 version.
c>> 1994-10-19 DRSILM  Krogh  Changes to use M77CON
c>> 1992-03-04 DRSILM  Krogh  Initial Code.
c--S replaces "?": DR?ILM, ?ILUPM
c Demonstration driver for SILUPM.
c Given a table of sin(xy), for x = 0, .08, .16, ..., and y = 0, .12, .2
c ... interpolates for (x,y) = (2.7, -.1), (.93, .05), (.765, .87) and
c obtains an error estimate.
c
      integer ND1, ND2, NDEG1, NDEG2, NUMTAB, NDIM, NX, NDIMT, NEOPT
      parameter (ND1=50, ND2=40, NDEG1=8, NDEG2=10, NUMTAB=ND1*ND2,
     1  NDIM=2, NX=3, NDIMT=2*NDIM+1, NEOPT=2*(2*NDIM+NDEG1)+1)
      integer  I, J, IOPT(4), NTAB(NDIMT), NDEG(NDIM), LUP(NDIM)
      real             X1(NX), X2(NX), YT(NUMTAB), YT2(ND2, ND1),
     1   XT(2*NDIM), X(NDIM), Y, EOPT(NEOPT), H1, H2, ANS
      parameter (H1 = .08E0, H2 = .12E0)
      equivalence (YT, YT2)
      data X1 / 2.7E0, .93E0, .765E0 /
      data X2 / -.1E0, .05E0, .87E0 /
c                       Set IOPT to get an error estimate.
      data IOPT / 0, NEOPT, 1, 0 /
      data NTAB(1), NTAB(2), NTAB(3) / ND1, ND2, 0 /
      data NDEG, LUP / NDEG1, NDEG2, 3, 3 /
c
c Compute the XT and YT tables
      XT(1) = 0.E0
      XT(2) = H1
      XT(3) = 0.E0
      XT(4) = H2
      do 20 I = 1, ND1
         do 10 J = 1, ND2
            YT2(J, I) = sin(real((I-1)*(J-1)) * H1 * H2)
   10    continue
   20 continue
      print *,
     1   'IOP(1)    X1      X2          Y     Est. Error True Error'
      do 50 I = 1, NX
         X(1) = X1(I)
         X(2) = X2(I)
         ANS = sin(X(1)*X(2))
            call SILUPM (NDIM,X,Y,NTAB,XT,YT,NDEG,LUP,IOPT,EOPT)
            print '(I5, 2F9.4, F12.8, 1P,E10.2, E11.2)',
     1       IOPT(1), X1(I), X2(I), Y, EOPT(1), Y - ANS
   50 continue
      stop
      end
