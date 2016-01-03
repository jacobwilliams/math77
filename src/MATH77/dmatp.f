      SUBROUTINE DMATP (A,LDA,M,N,TEXT)
c     DMATP..  Print a matrix.
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2001-05-25 DMATP Krogh Minor change for making .f90 version.
c>> 1996-07-02 DMATP Krogh  Changes to use .C. and C%%.
c>> 1996-03-30 DMATP Krogh  Added external statement.
c>> 1996-01-24 DMATP Krogh  M77CON instructions for conversion to C.
c>> 1994-10-20 DMATP Krogh  Changes to use M77CON
c>> 1994-08-08 DMATP CLL Special treatment for text(1:1) .eq. '0'
c>> 1994-04-20 CLL Making DP & SP codes similar.
C>> 1992-04-22 CLL
C>> 1990-01-23 CLL removed extraneous "60 continue"
C>> 1985-09-20 CLL
C>> 1983-07-05 Kris Stewart For MATH77
C>> 1981-07-23 Kris Stewart Improve portability.
C>> 1969-00-00 C. L. Lawson, JPL, Original code: MOUT/VOUT
C     ------------------------------------------------------------------
C  A(,)      Matrix to be output
C  LDA       Leading dimension of array A().
C  M         No. of rows to be printed from A().
C  N         No. of cols to be printed from A().
c  TEXT   Character string to be printed as a title.
c         First character in TEXT controls line spacing before title on
c         an impact printer.  For output to be viewed on a screen it is
c         safest to always use ' '.
c         ' ' = normal single space.
c         '0' = double space.
c         '1' = page advance.
c         '+' = suppress space, i.e., overprint.
C     ------------------------------------------------------------------
c  Method:  If the machine epsilon, is larger than 0.5*10**(-12), we set
c  MODE = 1 and print 8 numbers across a line, using a g15.7 format.
c  Otherwise we set MODE = 2 and print 6 numbers across a line, using a
c  g20.12 format.
C     ------------------------------------------------------------------
c--D replaces "?": ?MATP
C     ------------------------------------------------------------------
      external D1MACH
      integer iblock, j1, j2, lda, m, maxcol(2), mode, n, nblock
c%%   int i, j; /* Converter doesn't declare these for some reason. */
      integer i, j
      double precision a(lda,*), D1MACH
      character fmt1(2)*24, fmt2(2)*20, text*(*)
      data maxcol/8, 6/
c     ------------------------------------------------------------------
c++ CODE for ~.C. is active
      data fmt1 /'(/12x,8(4x,a3,i4,4x)/1x)','(/12x,6(6x,a3,i5,6x)/1x)'/
      data fmt2 /'(a,i4,4x,1p,8g15.7 )', '(a,i4,4x,1p,6g20.12)'/
      if(text(1:1) .eq. '0') then
         write(*,'(/1x,a)') text(2:)
      else
         write(*,'(a)') text
      endif
c++ CODE for .C. is inactive
C%%    if(text[0] == '0') printf("\n %s\n", &text[1]);
C%%    else  printf( "%s\n", text );
c++ END
      if(D1MACH(3) .gt. 0.5d-12) then
         mode = 1
      else
         mode = 2
      endif

      nblock=(n+maxcol(mode)-1)/maxcol(mode)
      j2 = 0

      do 70 iblock = 1,nblock
         j1 = j2 + 1
         j2 = min(j1+maxcol(mode)-1, n)
c++ CODE for ~.C. is active
         write(*,fmt1(mode)) ('COL',j,j=j1,j2)
         do 50 i=1,m
            write(*,fmt2(mode)) ' ROW',i,(a(i,j),j=j1,j2)
   50    continue
c++ CODE for .C. is inactive
C%%      printf("\n            ");
C%%      if (mode == 1) {
C%%        for (j = j1; j<= j2; j++) printf("    COL%4i    ", j);
C%%        printf("\n");
C%%        for (i = 1; i <= m; i++){
C%%           printf("ROW %4i", i);
C%%           for (j=j1; j <= j2; j++)
C%%                 printf("%15.7g", A(j-1, i-1));
C%%           printf("\n");}
C%%      }else {
C%%        for (j=j1;j<=j2;j++) printf("      COL%5i      ", j);
C%%        printf("\n");
C%%        for (i = 1; i <= m; i++){
C%%           printf("ROW %4i", i);
C%%           for (j=j1; j <= j2; j++)
C%%              printf("%20.12g", A(j-1, i-1));
C%%           printf("\n");}
C%%      }
c++ END
   70 continue
      end
