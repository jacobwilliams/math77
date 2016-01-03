      program P0214
c>> 1997-10-02 Krogh Generate plots for ch02-02.tex
      integer MAXPT
      real RMAXPT, XA, XB, XSIZE, YSIZE
      real YMIN
      real PI2
      parameter (MAXPT=101, RMAXPT=101.E0)
      parameter (XA = 0.0E0, XB = 12.0E0)
      parameter (XSIZE=3.2E0, YSIZE=2.5E0)
      parameter (YMIN=-2.0E0)
      parameter (PI2=1.57079632679489E0)
      real DX, OPT(23), X(MAXPT), Y(MAXPT, 3)
      external SCI, SCIN, SSI
      real SCI, SCIN, SSI
      integer I
      character COPT*63
      real PLOUNI, PLOTXT, PLONY, PLOBOR, PLOYMX, PLOANL
      parameter (PLOUNI=1.E0, PLOTXT=14.E0, PLONY=2.E0, PLOBOR=4.E0,
     1   PLOYMX=9.E0, PLOANL=16.E0)
      data OPT / 0.E0, PLONY, RMAXPT, 3.E0, PLOANL, PI2, 1924.E0,
     1   PLOTXT, 1.3E0, 1.9E0, 360.E0, PLOTXT, 4.3E0, -.6E0, 0.E0,
     1   PLOTXT, 7.E0, 2.8E0, 0.E0, PLOYMX, YMIN, 4.E0,
     2   0.E0 /
c               1111111111222222222233333333334444444444555555555556666
c      1234567890123456789012345678901234567890123456789012345678901234
      data COPT /
     1'F{pl02-14.tex}[cr]{$\frac{\pi}{2}$}{Si$(x)$}{Ci$(x)$}{Cin$(x)$}'
     2/
c
      DX = (XB - XA) / (MAXPT-1)
      do 100 I = 1, MAXPT
         X(I) = real(I-1) * DX
         if (X(I) .eq. 0.E0) X(I) = 1.E-3
         Y(I,1) = SSI(X(I))
         Y(I,2) = SCI(X(I))
         Y(I,3) = SCIN(X(I))
  100 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT, COPT)
      stop
      end
