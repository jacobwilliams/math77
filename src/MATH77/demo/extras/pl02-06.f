      program P0206
c>> 1997-06-20 Krogh Generate plots for ch02-06.tex
      integer MAXPT
      real RMAXPT, XA, XB, XSIZE, YSIZE
      parameter (MAXPT=101, RMAXPT=101.E0)
      parameter (XA = -6.0E0, XB = 10.0E0)
      parameter (XSIZE=3.3E0, YSIZE=2.5E0)
      real YMAX
      parameter (YMAX = 2.0E0)
      real DX, OPT(26), OPT2(7), OPT3(2), TP, X(MAXPT), Y(MAXPT, 2)
      integer I, INFO
      character COPT*73
      real PLOUNI, PLOTXT, PLONY, PLOBOR, PLOYMX
      parameter (PLOUNI=1.E0, PLOTXT=14.E0, PLONY=2.E0, PLOBOR=4.E0,
     1  PLOYMX=9.E0)
      data OPT / 0.E0, PLONY, RMAXPT, 2.E0, PLOTXT, -2.5E0, 1.8E0, 0.E0,
     1   PLOTXT, 1.7E0, 1.5E0, 0.E0, PLOTXT, -2.7E0, .8E0, 0.E0,
     2   PLOTXT, -5.5E0, -.35E0, 0.E0,
     3   PLOUNI, 3.E0, PLOYMX, -.5E0 ,YMAX, 0.E0 /
      data COPT / 'F{pl02-06.tex}{$e^x K_0(x)$}{$e^x K_1(x)$}{$e^{-|x|}I
     1_0$}{$e^{2-|x|}I_1$}'/
c
      data OPT2 / 0.E0, PLONY, RMAXPT, 1.E0, PLOUNI, 3.E0, 0.E0 /
      data OPT3 / 0.E0, 0.E0 /

      DX = (XB - 0.E0) / (MAXPT)
      do 100 I = 1, MAXPT
         X(I) = real(I) * DX
         call SBI0K0 (X(I), TP, Y(I,1), -2, info)
         call SBI1K1 (X(I), TP, Y(I,2), -2, info)
  100 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT, COPT)
      DX = (XB - XA) / (MAXPT - 1)
      do 200 I = 1, MAXPT
         X(I) = XA + real(I - 1) * DX
         call SBI1K1 (X(I), Y(I,1), TP, -1, info)
  200 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT2, 'Q')
      DX = (XB - 0.E0) / (MAXPT - 1)
      do 300 I = 1, MAXPT
         X(I) = real(I-1) * DX
         call SBI0K0 (X(I), Y(I,1), TP, -1, info)
  300 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT2, 'Q')
      DX = (0.E0 - XA) / (MAXPT - 1)
      do 400 I = 1, MAXPT
         X(I) = real(I-1) * DX + XA
         call SBI0K0 (X(I), Y(I,1), TP, -1, info)
  400 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT3, 'Q')
      stop
      end
