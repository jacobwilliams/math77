      program P0217
c>> 1999-11-24 Krogh Call in loop for metapost capacity prob.
c>> 1997-10-02 Krogh Generate plots for ch02-17.tex
      integer MAXPT, MAX1
      real RMAXPT, XA, XB, XSIZE, YSIZE
      parameter (MAX1=130, RMAX1=130.E0)
      parameter (XA = -3.0E0, XB = 5.0E0)
      parameter (XSIZE=3.2E0, YSIZE=2.5E0)
      real DX, OPT(28), OPTX(7), X0, XEND, X(MAX1), Y(MAX1, 4)
      external SFRENC, SFRENF, SFRENG, SFRENS
      real SFRENC, SFRENF, SFRENG, SFRENS
      integer I, K
      character COPT*46
      real PLOUNI, PLOTXT, PLONY, PLOBOR, PLOANL
      parameter (PLOUNI=1.E0, PLOTXT=14.E0, PLONY=2.E0, PLOBOR=4.E0,
     1   PLOANL=16.E0)
      data OPTX / 0.D0, PLONY, MAX1, 4.E0, PLOUNI, 3.E0, 0.D0 /
      data OPT / 0.E0, PLONY, RMAX1, 4.E0, PLOTXT, .1E0, .8E0, 0.E0,
     1   PLOTXT, 1.4E0, .75E0, 0.E0, PLOTXT, -1.2E0, -1.8E0, 0.E0,
     2   PLOTXT, -1.5E0, 1.3E0, 0.E0, PLOBOR, 2.E0, 6.E0, 1.E0,
     3   PLOANL, .5E0, 24.E0, 0.E0 /
c                          11111111112222222222333333333344444444
c                 12345678901234567890123456789012345678901234567
      data COPT /'F{pl02-17.tex}{C$(x)$}{S$(x)$}{$f(x)$}{$g(x)$}'/
c
      DX = (XB - XA) / 360.E0
      XEND = XB - .5E0 * DX
      X0 = XA - DX
      do 100 K = 1, 3
         do 50 I = 1, MAX1
            X(I) = X0 + real(I) * DX
            Y(I,1) = SFRENC(X(I))
            Y(I,2) = SFRENS(X(I))
            Y(I,3) = SFRENF(X(I))
            Y(I,4) = SFRENG(X(I))
            if (X(I) .ge. XEND) go to 100
 50      continue
         call SPLOT(XSIZE, YSIZE, X, MAX1, Y, OPTX, 'Q');
         X0 = X(MAX1) - 2.E0 * DX
 100  continue
      call SPLOT(XSIZE, YSIZE, X, I, Y, OPT, COPT)
      stop
      end
