      program P0208
c>> 1997-06-20 Krogh Generate plots for ch02-08.tex
      integer MAXPT
      real RMAXPT, XA, XB, XSIZE, YSIZE
      parameter (MAXPT=101, RMAXPT=101.E0)
      parameter (XA = 0.0E0, XB = 1.0E0)
      parameter (XSIZE=3.2E0, YSIZE=2.5E0)
      real DX, OPT(19), X(MAXPT), Y(MAXPT, 2)
      external SCPLTK, SCPLTE
      real SCPLTK, SCPLTE
      integer I
      character COPT*47
      real PLOUNI, PLOTXT, PLONY, PLOBOR, PLOYMX, PLOANL
      parameter (PLOUNI=1.E0, PLOTXT=14.E0, PLONY=2.E0, PLOBOR=4.E0,
     1   PLOYMX=9.E0, PLOANL=16.E0)
c                   1     2       3     4      5       6      7     8
c            9    10    11    12     13     14      15
      data OPT / 0.E0, PLONY, RMAXPT, 2.E0, PLOTXT, .5E0, 2.2E0, 0.E0,
     1  PLOTXT, .6E0, .9E0, 0.E0, PLOANL, 0.E0, 3120.E0,
     2  PLOYMX, 0.E0, 5.E0, 0.E0 /
c                          11111111112222222222333333333344444444
c                 12345678901234567890123456789012345678901234567
      data COPT /'F{pl02-08.tex}{$K(m)$}{$E(m)$}{$\frac{\pi}{2}$}'/
c
      DX = (XB - XA) / (MAXPT-1)
      do 100 I = 1, MAXPT
         X(I) = real(I - 1) * DX
         if (X(I) .ge. 1.E0) X(I) = .99999E0
         Y(I,1) = SCPLTK(X(I))
         Y(I,2) = SCPLTE(X(I))
  100 continue
      OPT(14) = Y(I, 1)
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT, COPT)
      stop
      end
