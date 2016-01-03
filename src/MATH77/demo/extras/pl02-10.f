      program P0210
c>> 1997-06-20 Krogh Generate plots for ch02-02.tex
      integer MAXPT
      real RMAXPT, XA, XB, XSIZE, YSIZE
      real YMAX, YMIN
      parameter (MAXPT=101, RMAXPT=101.E0)
      parameter (XA = 0.0E0, XB = 1.6E0)
      parameter (XSIZE=3.2E0, YSIZE=2.5E0)
      parameter (YMAX=4.0E0, YMIN=-4.0E0)
      real DX, OPT(16), X(MAXPT), Y(MAXPT, 2)
      external SE1, SEI
      real SE1, SEI
      integer I
      character COPT*33
      real PLOUNI, PLOTXT, PLONY, PLOBOR, PLOYMX
      parameter (PLOUNI=1.E0, PLOTXT=14.E0, PLONY=2.E0, PLOBOR=4.E0,
     1   PLOYMX=9.E0 )
      data OPT / 0.E0, PLONY, RMAXPT, 2.E0, PLOTXT, 1.2E0, .5E0, 0.E0,
     1   PLOTXT, .8E0, 2.E0, 0.E0, PLOYMX, YMIN, YMAX,
     2   0.E0 /
      data COPT /'F{pl02-10.tex}{$E_1(x)$}{$Ei(x)$}'/
c
      DX = (XB - XA) / (MAXPT-1)
      do 100 I = 1, MAXPT
         X(I) = real(I-1) * DX
         if (X(I) .eq. 0) X(I) = 1.E-3
         Y(I,1) = min(SE1(X(I)), YMAX + 1.E0)
         Y(I,2) = max(SEI(X(I)), YMIN - 1.E0)
  100 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT, COPT)
      stop
      end
