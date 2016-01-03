      program P0203
c>> 1997-06-20 Krogh Generate plots for ch02-03.tex
      integer MAXPT
      real RMAXPT, XA, XB, XSIZE, YSIZE
      real YMAX
      parameter (MAXPT=101, RMAXPT=101.E0)
      parameter (XA = 0.0E0, XB = 8.0E0)
      parameter (XSIZE=3.0E0, YSIZE=2.5E0)
      parameter (YMAX=10.0E0)
      real DX, OPT(16), X(MAXPT), Y(MAXPT, 2)
      external SGAMMA, SLGAMA
      real SGAMMA, SLGAMA
      integer I
      character COPTA*46
      real PLOTXT, PLONY, PLOBOR, PLOYMX
      parameter (PLOTXT=14.E0, PLONY=2.E0, PLOBOR=4.E0,
     1   PLOYMX=9.E0 )
      data OPT / 0.E0, PLONY, RMAXPT, 2.E0, PLOTXT, 3.4E0, 8.0E0, 0.E0,
     1   PLOTXT, 6.0E0, 7.0E0, 0.E0, PLOYMX, -2.E0, 10.E0, 0.E0 /
c
      data COPTA /'F{pl02-03a.tex}{$\Gamma(x)$}{$\ln \Gamma(x)$}'/
c
      DX = (XB - XA) / (MAXPT)
      do 100 I = 1, MAXPT
         X(I) = real(I) * DX
         Y(I,1) = min(SGAMMA(X(I)), YMAX + 4.E0)
         Y(I,2) = SLGAMA(X(I))
  100 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT, COPTA)
      stop
      end
