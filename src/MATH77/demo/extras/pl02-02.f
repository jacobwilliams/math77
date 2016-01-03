      program P0202
c>> 1997-06-20 Krogh Generate plots for ch02-02.tex
      integer MAXPT
      real RMAXPT, XA, XB, XSIZE, YSIZE
      parameter (MAXPT=101, RMAXPT=101.E0)
      parameter (XA = 0.0E0, XB = 5.0E0)
      parameter (XSIZE=3.2E0, YSIZE=2.75E0)
      real DX, OPT(17), X(MAXPT), Y(MAXPT, 3)
      external SERF, SERFC, SERFCE
      real SERF, SERFC, SERFCE
      integer I
      character COPT*41
      real PLONY, PLOTXT
      parameter (PLONY=2.E0, PLOTXT=14.E0)
      data OPT / 0.E0, PLONY, RMAXPT, 3.E0, PLOTXT, 1.1E0, .8E0, 0.E0,
     1   PLOTXT, 1.2E0, .4E0, 0.E0, PLOTXT, 1.3E0, .1E0, 0.E0, 0.E0 /
      data COPT /
     1 'F{pl02-02.tex}{erf(x)}{erfce(x)}{erfc(x)}'/
c
      DX = (XB - XA) / (MAXPT - 1)
      do 100 I = 1, MAXPT
         X(I) = real(I - 1) * DX
         Y(I,1) = SERF(X(I))
         Y(I,2) = SERFC(X(I))
         Y(I,3) = SERFCE(X(I))
  100 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT, COPT)
      stop
      end
