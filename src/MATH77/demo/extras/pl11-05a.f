      program P1105A
c>> 1997-10-02 Krogh Generate plots for first two figures in ch11-05.tex
      integer MAXPT
      real RMAXPT, XA, XB, XSIZE, YSIZE
      parameter (MAXPT=101, RMAXPT=101.E0)
      parameter (XA = 0.0E0, XB = 6.0E0)
      parameter (XSIZE=3.2E0, YSIZE=2.5E0)
      real DX, X(MAXPT), Y(MAXPT,2)
      external SSVAL
      real SSVAL
      integer I, J
      real OPTA1(10), OPTA2(5), OPTA3(6), OPTB1(21), OPTB2(7)
      character COPTA1*17, COPTB1*50
c
      integer NCOEF, KORDER
      parameter (NCOEF = 9, KORDER = 4)
      real BCOEF(NCOEF), BVPOSA(7), BVPOSB(7), PKNOTS(7), TKNOTS(13),
     1  XPT(24), YPT(24)
c
      real PLOUNI,PLOTXT,PLONY,PLOBOR,PLOSYM,PLOYMX
      parameter (PLOUNI=1.E0, PLOTXT=14.E0, PLONY=2.E0, PLOBOR=4.E0,
     1  PLOSYM=10.E0, PLOYMX=9.E0)
c
      data OPTA1 / 0.E0, PLOSYM, 80603.E0, PLOBOR, 34.0E0, 0.E0, 0.E0,
     1   PLOUNI, 54.E0, 0.E0 /
      data COPTA1 /'F{pl11-05a.tex}Q'/
c
      data OPTA2 /0.E0, PLOUNI, 4.E0, 0.E0, 0.E0/
c
      data OPTA3 / 0.E0, PLOSYM, 1040000.E0, PLOUNI, 50.E0, 0.E0 /
c
      data OPTB1 / 0.E0, PLOSYM, 80603.E0, PLOBOR, 34.0E0, 0.E0, 0.E0,
     1   PLOUNI, 53.E0, PLOTXT, 3.5E0, 2.0E0, 0.E0,
     2   PLOTXT, 4.E0, -2.E0, 0.E0, PLOYMX, -4.E0, 4.E0, 0.E0 /
c                           111111111122222222223333333333444444444455
c                  123456789012345678901234567890123456789012345678901
      data COPTB1/'F{pl11-05b.tex}{$f^\prime$}{$f^{\prime \prime}$}'/
c
      data OPTB2 / 0.E0, PLOUNI, 3.E0, PLONY, MAXPT, 2.E0,
     1   0.E0 /
c
      data XPT / 0.000E0, 0.300E0, 0.700E0, 1.000E0, 1.300E0, 1.700E0,
     1  2.000E0, 2.300E0, 2.500E0, 2.600E0, 2.800E0, 2.900E0, 3.000E0,
     2  3.100E0, 3.200E0, 3.500E0, 3.700E0, 4.000E0, 4.300E0, 4.700E0,
     3  5.000E0, 5.300E0, 5.700E0, 6.000E0 /
      data YPT / 1.000E0, 1.100E0, 0.900E0, 1.020E0, 1.200E0, 1.000E0,
     1  1.200E0, 1.400E0, 1.760E0, 2.000E0, 2.400E0, 2.600E0, 3.000E0,
     2  3.400E0, 3.700E0, 4.300E0, 4.450E0, 4.760E0, 4.800E0, 5.000E0,
     3  4.960E0, 4.900E0, 4.900E0, 5.000E0 /
      data TKNOTS / 0.00000E0, 0.00000E0, 0.00000E0, 0.00000E0,
     1   1.50000E0, 2.50000E0, 3.30000E0, 4.00000E0, 4.70000E0,
     2   6.00000E0, 6.00000E0, 6.00000E0, 6.00000E0 /
      data BCOEF /  1.00000E0, 1.01613E0, 1.04300E0, 1.07848E0,
     1   4.07150E0, 4.87706E0, 4.92040E0, 4.96864E0, 5.00000E0 /
      data PKNOTS / 0.00000E0, 1.50000E0, 2.50000E0, 3.30000E0,
     1   4.00000E0, 4.70000E0, 6.00000E0 /
      data BVPOSA / 7*.3E0 /
      data BVPOSB / 7*-3.5E0 /
c
c Take care of the first figure for Chapter 11-05.
c
c Take care of the breakpoint marks
      call SPLOT(XSIZE, YSIZE, PKNOTS, 7, BVPOSA, OPTA1, COPTA1)
c
      DX = (XB - XA) / (MAXPT - 1)
      do 100 I = 1, MAXPT
         X(I) = real(I - 1) * DX
         Y(I,1) = SSVAL(KORDER, NCOEF, TKNOTS, BCOEF, X(I), 0)
  100 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPTA2, 'Q')
c Plot the data points
      call SPLOT(XSIZE, YSIZE, XPT, 24, YPT, OPTA3, 'Q')
c
c Take care of second figure for Chapter 11-05
c
c Take care of the breakpoint marks
      call SPLOT(XSIZE, YSIZE, PKNOTS, 7, BVPOSB, OPTB1, COPTB1)
      do 200 J = 1, 6
c                Loop over the intervals between breakpoints.
         DX = (PKNOTS(J+1) - PKNOTS(J)) / 19.E0
         do 150 I = 1, 20
            X(I) = PKNOTS(J) + real(I - 1) * DX
            Y(I,1) = SSVAL(KORDER, NCOEF, TKNOTS, BCOEF, X(I), 1)
            Y(I,2) = SSVAL(KORDER, NCOEF, TKNOTS, BCOEF, X(I), 2)
  150    continue
         if (J .eq. 6) OPTB2(3) = 0.E0
         call SPLOT(XSIZE, YSIZE, X, 20, Y, OPTB2, 'Q')
  200 continue
      stop
      end
