      program DRDPLO
c>> 1998-01-15 Krogh Generate the plot used in ch02-02.tex
c--D replaces "?": DR?PLO, DR?PL1, dr?plot, ?PLOT, ?ERF, ?ERFC, ?ERFCE
      integer MAXPT
      double precision RMAXPT, XA, XB, XSIZE, YSIZE
      parameter (MAXPT=101, RMAXPT=101.D0)
      parameter (XA = 0.0D0, XB = 5.0D0)
      parameter (XSIZE=3.2D0, YSIZE=2.75D0)
      double precision DX, OPT(17), X(MAXPT), Y(MAXPT, 3)
      external DERF, DERFC, DERFCE
      double precision DERF, DERFC, DERFCE
      integer I
      character COPT*41
      double precision PLONY, PLOTXT
      parameter (PLONY=2.D0, PLOTXT=14.D0)
      data OPT / 0.D0, PLONY, RMAXPT, 3.D0, PLOTXT, 1.1D0, .8D0, 0.D0,
     1   PLOTXT, 1.2D0, .4D0, 0.D0, PLOTXT, 1.3D0, .1D0, 0.D0, 0.D0 /
      data COPT /
     1 'F{drdplot.tex}{erf(x)}{erfce(x)}{erfc(x)}'/
c
      DX = (XB - XA) / (MAXPT - 1)
      do 100 I = 1, MAXPT
         X(I) = dble(I - 1) * DX
         Y(I,1) = DERF(X(I))
         Y(I,2) = DERFC(X(I))
         Y(I,3) = DERFCE(X(I))
  100 continue
      call DPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT, COPT)
c
c Don't use code below this point.  This code is included to make
c possible automated checks of the software in different environments.
c
      call DRDPL1
      stop
      end

      subroutine DRDPL1
c            Print .tex output for checkout purposes.
c++ CODE for ~.C. is active
      character BUF*80
      integer I
      open (unit=10, FILE='drdplot.tex', status='OLD')
   10 BUF = ' '
         read(10, '(A)', END=40) BUF
         do 20 I = 80, 2, -1
            if (BUF(I:I) .ne. ' ') go to 30
   20    continue
   30    print '(1X,A)', BUF(1:I)
      go to 10
   40 continue
      close(10)
c++ CODE for .C. is inactive
c%%   char  buf[81];
c%%   FILE  *iofil;
c%%   iofil = fopen("drdplot.tex", "r");
c%%   while (fscanf( iofil, "%[^\n]", buf ) != EOF){
c%%      printf("%s\n", buf);
c%%      if (fscanf( iofil, "%[\n]", buf) == EOF) break;}
c%%   fclose(iofil);
c++ END
      return
      end
