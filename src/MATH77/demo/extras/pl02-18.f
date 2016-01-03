      program P0218
c>> 1997-10-02 Krogh Generate plots for ch02-18.tex
      integer MAXPT
      real XA, XB, XSIZE, YMAX, YMIN, YSIZE
      parameter (MAXPT=151)
      parameter (XA = -4.0E0, XB = 4.0E0)
      parameter (YMIN = -8.E0, YMAX = 8.E0)
      parameter (XSIZE=3.2E0, YSIZE=2.5E0)
      real DX, OPT(5), X(MAXPT), Y(MAXPT)
      external SPSI
      real SPSI
      integer I
      character COPT*40
      real PLOUNI, PLOTXT, PLONY, PLOBOR, PLOYMX
      parameter (PLOUNI=1.E0, PLOTXT=14.E0, PLONY=2.E0, PLOBOR=4.E0,
     1   PLOYMX=9.E0 )
      data OPT / 0.E0, PLOYMX, YMIN, YMAX, 0.E0 /
      data COPT / 'F{pl02-18.tex}1(F12){\large $\psi(x)$}Q' /
c
      DX = (XB - XA) / MAXPT
      do 100 I = 1, MAXPT
         X(I) = XA + real(I) * DX
         Y(I) = SPSI(X(I))
  100 continue
  150 call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT, COPT)
      stop
      end
