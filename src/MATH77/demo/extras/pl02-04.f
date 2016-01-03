c      program P0204
c>> 1997-06-20 Krogh Generate plots for ch02-04.tex
      integer MAXPT
      real RMAXPT, XA, XB, XSIZE, YSIZE
      parameter (MAXPT=101, RMAXPT=101.E0)
      parameter (XA = -10.0E0, XB = 15.0E0)
      parameter (XSIZE=3.0E0, YSIZE=2.5E0)
      real YMIN
      parameter (YMIN = -1.5E0)
      real DX, OPT(23), OPT2(8), X(MAXPT), Y(MAXPT, 2)
      external SBESJ0, SBESJ1, SBESY0, SBESY1
      real SBESJ0, SBESJ1, SBESY0, SBESY1
      integer I
      character COPT*54
      real PLOUNI, PLOTXT, PLONY, PLOBOR, PLOYMX
      parameter (PLOUNI=1.E0, PLOTXT=14.E0, PLONY=2.E0, PLOBOR=4.E0,
     1  PLOYMX=9.E0)
      data OPT / 0.E0,  PLONY, RMAXPT, 2.E0, PLOTXT, -4.E0, .8E0, 0.E0,
     1   PLOTXT, -3.8E0, -.75E0, 0.E0, PLOTXT, -3.0E0, -1.25E0, 0.E0,
     2   PLOTXT, 1.E0, -1.4E0, 0.E0, PLOUNI, 3.E0, 0.E0 /
      data COPT /
     1  'F{pl02-04.tex}{$J_0(x)$}{$J_1(x)$}{$Y_0(x)$}{$Y_1(x)$}'/
      data OPT2 / 0.E0, PLONY, RMAXPT, 2.E0, PLOYMX, YMIN,
     1  1.0E0, 0.E0 /
c
      DX = (XB - XA) / (MAXPT - 1)
      do 100 I = 1, MAXPT
         X(I) = XA + real(I - 1) * DX
         Y(I,1) = SBESJ0(X(I))
         Y(I,2) = SBESJ1(X(I))
  100 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT, COPT)
      DX = (XB - 0.E0) / (MAXPT)
      do 200 I = 1, MAXPT
         X(I) = real(I) * DX
         Y(I,1) = max(SBESY0(X(I)), YMIN - .5E0)
         Y(I,2) = max(SBESY1(X(I)), YMIN - .5E0)
  200 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT2, 'Q')
      stop
      end
