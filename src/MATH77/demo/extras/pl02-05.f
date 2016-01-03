      program P0205
c>> 1997-06-20 Krogh Generate plots for ch02-05.tex
c Derived from an earlier code by Charles Lawson.  Working backwards
c from that code, the curve generated here for pl02-05c is the set of
c points (x,y) which satisfy:
c
c sqrt(2/pi y) [ 2 y / (e x) ] ** y = omega,
c
c where pi and e are the usual constants, and omega is the overflow limit.
c The plots are done for -.01 .le. x .le. .01.  The code avoids overflow
c and gets a smoother problem to solve by taking the base 10 logarithm of
c both side of this equation.
c
      integer MAXPT, MAXPTC
      real RMAXPT, XAC, XBC, XSIZE, YSIZE
      parameter (MAXPT=101, MAXPTC=41, RMAXPT=101.E0)
      parameter (XAC = .01E0, XBC = 100.0E0)
      parameter (XSIZE=3.3E0, YSIZE=3.0E0)
      real DX, OPTC(31), X(MAXPT), Y(MAXPT, 4)
      external SERF, SERFC, SERFCE
      real SERF, SERFC, SERFCE
      integer I, IV, MODE
      character COPTC*128
      real PLOUNI, PLOTXT, PLONY, PLOBOR, PLOYMX
      real             C1, C2, CC2, F1, F2, FCN, TOL, VALS(4),
     1   XX, Y1, Y2, ZZ
      parameter (TOL = 1.0E-5)

      parameter (PLOUNI=1.E0, PLOTXT=14.E0, PLONY=2.E0, PLOYMX=9.E0,
     1   PLOBOR=4.E0)
      data OPTC /0.E0, PLONY, RMAXPT, 4.E0, PLOTXT, .06E0, 12.5E0, 0.E0,
     1   PLOTXT, .015E0, 49.E0, 0.E0, PLOTXT, .2E0, 300.E0, 0.E0,
     2   PLOTXT, .03E0, 800.E0, 0.E0, PLOUNI, 3300.E0,
     3   PLOBOR, 34.E0, 0.E0, 0.E0, PLOBOR, 12.E0, 46.E0, 0.E0, 0.E0 /

      data COPTC /
     1'F{pl02-05c.tex}1[cr]{$x$}2[tc]{$\ \nu^*$}{IEEE SP, Unisys SP, VAX
     2 S2P \& DP}{IBM 30xx SP \& DP}{IEEE DP \& Unisys DP}{Cray J90}'/
      data VALS / 128.0E0, 252.0E0, 1024.0E0, 8190.0E0 /
c
      FCN(YY, C1, C2) = 0.5E0 * (C1 - YY) + 10.0E0**YY * (C2 + YY)
c
c        Plot the curves for Figure 3.

      C1 = log10(0.5E0 / atan(1.0E0))
      CC2 = log10(2.0E0 / exp(1.0E0))
      DX = 4.E0 / (MAXPTC-1)
      do 360 IV = 1, 4
         ZZ = VALS(IV) * log10(2.0E0)
         Y1 = 1.0E0
         do 340 I = 1, MAXPTC
            XX = DX*(I-1) - 2.0E0
            C2 = CC2 - XX
            if (I .eq. 1) then
               Y2 = log10(5000.0E0)
            else
               Y2 = Y1 + 0.5E0
            end if

            F1 = FCN(Y1, C1, C2) - ZZ
            F2 = FCN(Y2, C1, C2) - ZZ
            MODE = 0
c                    Loop to find the zero.
  320       continue
            call SZERO (Y1, F1, Y2, F2, MODE, TOL)
            if (MODE .eq. 1) then
               F1 = FCN(Y1, C1, C2) - ZZ
               go to 320
            end if
            Y(I, IV) = 10.E0 ** Y1
            X(I) = 10.E0 ** XX
  340    continue
  360 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPTC, Y, OPTC, COPTC)
      end
