      subroutine SPRTSV (A, MDA, M, N, NAMES, MODE, UNIT, WIDTH)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2008-11-26 SPRTSV Krogh Changed FMT2 for Fortran standard change
c>> 2008-11-26 SPRTSV Krogh in how P and F interact in formats.
c>> 2001-06-08 SPRTSV Krogh  Increased length of FMT1 and FMT2.
c>> 2001-05-25 SPRTSV Krogh  Added a comma in a format.
c>> 2001-01-16 SPRTSV Krogh  Minor fix for fussy C compilers.
c>> 1996-06-27 SPRTSV Krogh  Changes to use .C. and C%%.
c>> 1996-01-23 SPRTSV Krogh  Got in code for C conversion.
c>> 1994-10-20 SPRTSV Krogh  Changes to use M77CON
c>> 1992-03-22 SPRTSV CAO Deleted 4 debug statements
C>> 1992-03-18 CLL Allow user to choose size of names in NAMES().
C>> 1989-03-07 SPRTSV CLL Added arguments UNIT and WIDTH
C>> 1987-11-24 SPRTSV Lawson  Initial code.
c     Prints matrix with labeling, to be called by the Singular Value
c     Analysis subroutine, [D/S]SVA.
c     ------------------------------------------------------------------
c                           Subroutine Arguments
C     All are inout arguments.  None are modified by this subroutine.
c
C     A(,)     Array containing matrix to be output.
C     MDA      First dimension of the array, A(,).
C     M, N     No. of rows and columns, respectively in the matrix
c              contained in A(,).
C     NAMES()  [character array]  Array of names.
c              If NAMES(1) contains only blanks, the rest of the NAMES()
c              array will be ignored.
C     MODE     =1  Write header for V matrix and use an F format.
C              =2  Write header for the candidate solutions and use
c                  G format.
c     UNIT  [integer]   Selects output unit.  If UNIT .ge. 0 then UNIT
c              is the output unit number.  If UNIT = -1, output to
c              the '*' unit.
c     WIDTH [integer]   Selects width of output lines.
c              Each output line from this subroutine will have at most
c              max(26,min(124,WIDTH)) characters plus one additional
c              leading character for Fortran "carriage control".  The
c              carriage control character will always be a blank.
c     ------------------------------------------------------------------
c          This code was originally developed by Charles L. Lawson and
c     Richard J. Hanson at Jet Propulsion Laboratory in 1973.  The
c     original code was described and listed in the book,
c
c                  Solving Least Squares Problems
c                  C. L. Lawson and R. J. Hanson
c                  Prentice-Hall, 1974
c
c     Feb 1985, Mar 1987, C. L. Lawson & S. Y. Chan, JPL.
c     Adapted code from the Lawson & Hanson book to Fortran 77 for use
c     in the JPL MATH77 library.
c     Prefixing subprogram names with S or D for s.p. or d.p. versions.
c     Using generic names for intrinsic functions.
c     Adding calls to BLAS and MATH77 error processing subrs in some
c     program units.
c     ------------------------------------------------------------------
c--S replaces "?": ?PRTSV
c     ------------------------------------------------------------------
      integer MDA, M, N, MODE, UNIT, WIDTH
      integer I, J2, L, LENNAM, MAXCOL, NAMSIZ, NBLOCK
      real                A(MDA,N)
      character  NAMES(M)*(*)
      character*4  HEAD (2)
      character    HEAD1A*53, HEAD1B*47, HEAD2*55
      logical      BLKNAM
c++ Code for .C. is INACTIVE
C%%      long int j, j1, kblock;
c++ Code for ~.C. is ACTIVE
      integer J, J1, KBLOCK
      logical      STAR
      character*27 FMT1(2)
      character*26 FMT2(2)
      data FMT1 / '(/7x,00x,8(5x,a4,i4,1x)/)',
     *            '(/7x,00x,8(2x,a4,i4,4x)/)'/
      data FMT2 / '(1x,i4,1x,a00,1x,8f14.0)',
     *            '(1x,i4,1x,a00,1x,8g14.6  )'/
c++ End
      data HEAD1A /
     *   ' V-Matrix of the Singular Value Decomposition of A*D.'/
      data HEAD1B /' (Elements of V scaled up by a factor of 10**4)'/
      data HEAD2  /' Sequence of candidate solutions, X'/
      data HEAD/' COL', 'SOLN'/
c     ------------------------------------------------------------------
      if (M .le. 0 .or. N .le. 0) return
C
c     The LEN function returns the char length of a single element of
c     the NAMES() array.
c
      NAMSIZ = 1
C++ code for ~.C. is ACTIVE
      BLKNAM = NAMES(1) .eq. ' '
      LENNAM = len(NAMES(1))
C++ code for .C. is INACTIVE
c%%   lennam = names_s;
c%%   blknam = ((int)strspn(NAMES(0,0), " ") == lennam);
C++ End
      if(.not. BLKNAM) then
         do 30 I = 1,M
            do 10 L = LENNAM, NAMSIZ+1, -1
               if(NAMES(I)(L:L) .ne. ' ') then
                  NAMSIZ = L
                  go to 20
               endif
   10       continue
   20       continue
   30    continue
      endif
c
c++ Code for ~.C. is ACTIVE
      write(FMT1(MODE)(6:7),'(i2.2)') NAMSIZ
      write(FMT2(MODE)(12:13),'(i2.2)') NAMSIZ
      STAR = UNIT .lt. 0
      if(STAR) then
c++ End
         if (MODE .eq. 1) then
            write (*,'(/a/a)') HEAD1A, HEAD1B
         else
            write (*,'(/a)') HEAD2
         endif
c++ Code for ~.C. is ACTIVE
      else
         if (MODE .eq. 1) then
            write (UNIT,'(/a/a)') HEAD1A, HEAD1B
         else
            write (UNIT,'(/a)') HEAD2
         endif
      endif
c++ End
c
c     With NAMSIZ characters allowed for the "name" and MAXCOL
c     columns of numbers, the total line width, exclusive of a
c     carriage control character, will be 6 + LENNAM + 14 * MAXCOL.
c
      MAXCOL = max(1,min(8,(WIDTH - 6 - NAMSIZ)/14))
C
      NBLOCK = (N + MAXCOL -1) / MAXCOL
      J2 = 0
c++ Code for ~.C. is ACTIVE
      do 50 KBLOCK = 1, NBLOCK
         J1 = J2 + 1
         J2 = min(N, J2 + MAXCOL)
         if(STAR) then
            write (*,FMT1(MODE)) (HEAD(MODE),J,J=J1,J2)
         else
            write (UNIT,FMT1(MODE)) (HEAD(MODE),J,J=J1,J2)
         endif
C
         do 40 I=1,M
           if(STAR) then
             if(BLKNAM) then
               if (MODE .eq. 1) then
                 write (*,FMT2(1)) I,' ',(1.E4*A(I,J),J=J1,J2)
               else
                 write (*,FMT2(2)) I,' ',(A(I,J),J=J1,J2)
               end if
             else
               if (MODE .eq. 1) then
                 write (*,FMT2(1)) I,NAMES(I),(1.E4*A(I,J),J=J1,J2)
               else
                 write (*,FMT2(2)) I,NAMES(I),(A(I,J),J=J1,J2)
               end if
             endif
           else
             if(BLKNAM) then
               write (UNIT,FMT2(MODE)) I,' ',(A(I,J),J=J1,J2)
             else
               write (UNIT,FMT2(MODE)) I,NAMES(I),(1.E4*A(I,J),J=J1,J2)
             endif
           endif
 40      continue
   50 continue
C
c++ Code for .C. is INACTIVE
C%%  for( kblock = 1L; kblock <= nblock; kblock++ ){
C%%      j1 = j2 + 1L;
C%%      j2 = min( n, j2 + maxcol );
C%%      if( mode == 1L ){
C%%          printf( "\n        %*s", (int)namsiz, " " );
C%%          for( j = j1; j <= j2; j++ ){
C%%              printf( "     %4.4s%4ld ", head[0L], j );
C%%          }
C%%          printf( "\n" );
C%%          for( i = 1L; i <= m; i++ ){
C%%              if( blknam ){
C%%                  printf( " %4ld %*s ",i,(int)namsiz," " );
C%%                  for( j = j1; j <= j2; j++ ){
C%%                     printf( "%14.0f.", 1.0e4*A(j-1L,i-1L));
C%%                  }
C%%                  printf( "\n" );
C%%              } else{
C%%                  printf( " %4ld %-*.*s ",
C%%                   i, (int)namsiz, (int)namsiz, NAMES(i-1L,0L) );
C%%                  for( j = j1; j <= j2; j++ ){
C%%                     printf( "%14.0f.", 1.0e4*A(j-1L,i-1L));
C%%                  }
C%%                  printf( "\n" );
C%%              }
C%%          }
C%%      } else{
C%%          printf( "\n       %*s", (int)namsiz, " " );
C%%          for( j = j1; j <= j2; j++ ){
C%%              printf( "      %4.4s%4ld", head[1L], j );
C%%          }
C%%          printf( "\n" );
C%%          for( i = 1L; i <= m; i++ ){
C%%              if( blknam ){
C%%                  printf( " %4ld %*s ",i,(int)namsiz," " );
C%%                  for( j = j1; j <= j2; j++ ){
C%%                      printf( "%14.6g", A(j - 1L,i - 1L) );
C%%                  }
C%%                  printf( "\n" );
C%%              } else{
C%%                  printf( " %4ld %-*.*s",
C%%                   i, (int)namsiz, (int)namsiz, NAMES(i-1L,0L) );
C%%                  printf( " " );
C%%                  for( j = j1; j <= j2; j++ ){
C%%                      printf( "%14.6g", A(j - 1L,i - 1L) );
C%%                  }
C%%                  printf( "\n" );
C%%              }
C%%          }
C%%      }   /* endif !(MODE...) */
C%%  }   /* end for kblock  */
c++ End
      end
