      program DRSPLO
c>> 1998-01-15 Krogh Generate the plot used in ch02-02.tex
c--S replaces "?": DR?PLO, DR?PL1, dr?plot, ?PLOT, ?ERF, ?ERFC, ?ERFCE
      integer MAXPT
      real             RMAXPT, XA, XB, XSIZE, YSIZE
      parameter (MAXPT=101, RMAXPT=101.E0)
      parameter (XA = 0.0E0, XB = 5.0E0)
      parameter (XSIZE=3.2E0, YSIZE=2.75E0)
      real             DX, OPT(17), X(MAXPT), Y(MAXPT, 3)
      external SERF, SERFC, SERFCE
      real             SERF, SERFC, SERFCE
      integer I
      character COPT*41
      real             PLONY, PLOTXT
      parameter (PLONY=2.E0, PLOTXT=14.E0)
      data OPT / 0.E0, PLONY, RMAXPT, 3.E0, PLOTXT, 1.1E0, .8E0, 0.E0,
     1   PLOTXT, 1.2E0, .4E0, 0.E0, PLOTXT, 1.3E0, .1E0, 0.E0, 0.E0 /
      data COPT /
     1 'F{drsplot.tex}{erf(x)}{erfce(x)}{erfc(x)}'/
c
      DX = (XB - XA) / (MAXPT - 1)
      do 100 I = 1, MAXPT
         X(I) = real(I - 1) * DX
         Y(I,1) = SERF(X(I))
         Y(I,2) = SERFC(X(I))
         Y(I,3) = SERFCE(X(I))
  100 continue
      call SPLOT(XSIZE, YSIZE, X, MAXPT, Y, OPT, COPT)
c
c Don't use code below this point.  This code is included to make
c possible automated checks of the software in different environments.
c
      call DRSPL1
      stop
      end
 
      subroutine DRSPL1
c            Print .tex output for checkout purposes.
c++ CODE for ~.C. is active
      character BUF*80
      integer I
      open (unit=10, FILE='drsplot.tex', status='OLD')
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
c%%   iofil = fopen("drsplot.tex", "r");
c%%   while (fscanf( iofil, "%[^\n]", buf ) != EOF){
c%%      printf("%s\n", buf);
c%%      if (fscanf( iofil, "%[\n]", buf) == EOF) break;}
c%%   fclose(iofil);
c++ END
      return
      end
