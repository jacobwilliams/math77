      SUBROUTINE IMATP (A,LDA,M,N,TEXT)
c     IMATP..  Print a matrix.
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-07-09 IMATP Krogh  Changes to use .C. and C%%.
c>> 1996-01-24 IMATP Krogh  M77CON instructions for conversion to C.
c>> 1994-08-08 CLL Special treatment for text(1:1) .eq. '0'
c>> 1994-04-20 CLL Making code in IMATP similar to DMATP and SMATP.
C>> 1992-04-22 CLL
C>> 1990-01-23 CLL removed extraneous "60 continue"
C>> 1985-09-20 CLL
C>> 1983-07-05 Kris Stewart For MATH77
C>> 1981-07-23 Kris Stewart Improve portability.
C>> 1969-00-00 C. L. Lawson, JPL, Original code: MOUT/VOUT
C     ------------------------------------------------------------------
C  A( )      MATRIX TO BE OUTPUT
C  LDA       Leading dimension of array A().
C  M         NO. OF ROWS IN A MATRIX
C  N         NO. OF COLS IN A MATRIX
c  TEXT   Character string to be printed as a title.
c         First character in TEXT controls line spacing before title on
c         an impact printer.  For output to be viewed on a screen it is
c         safest to always use ' '.
c         ' ' = normal single space.
c         '0' = double space.
c         '1' = page advance.
c         '+' = suppress space, i.e., overprint.
C     ------------------------------------------------------------------
      integer i, iblock, j1, j2, lda, m, maxcol, n, nblock

      integer a(lda,*)
      character*(*) text
      parameter(maxcol = 8)
c
c++ CODE for ~.C. is active
      integer j
      if(text(1:1) .eq. '0') then
         write(*,'(/1x,a)') text(2:)
      else
         write(*,'(a)') text
      endif
c++ CODE for .C. is inactive
C%%    long int j;
C%%    if(text[0] == '0') printf("\n %s\n", &text[1]);
C%%    else  printf( "%s\n", text );
c++ END
      nblock=(n+maxcol-1)/maxcol
      j2 = 0

      do 70 iblock = 1,nblock
         j1 = j2 + 1
         j2 = min(j1+maxcol-1, n)

C%%      printf( "\n               " );
C%%      for (j = j1; j <= j2; j++)
C%%         printf( "    COL%4ld    ", j );
C%%      printf( "\n" );
         write(*,'(/15x,8(4x,a3,i4,4x)/1x)') ('COL',j,j=j1,j2)
         do 50 i=1,m
C%%         printf( "ROW%4ld    ", i );
C%%         for (j = j1; j <= j2; j++)
C%%            printf( "%15ld", A(j-1, i-1) );
C%%         printf( "\n" );
            write(*,'(a,i4,4x,8i15)') ' ROW',i,(a(i,j),j=j1,j2)
   50    continue
   70 continue
      end
