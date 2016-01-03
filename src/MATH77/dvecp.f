      SUBROUTINE      DVECP  (V, N, TEXT)
c     DVECP..  Print a vector.
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2001-05-25 DVECP Krogh Minor change for making .f90 version.
c>> 1996-04-27 DVECP Krogh  Changes to use .C. and C%%.
c>> 1996-03-30 DVECP Krogh  Added external statement.
c>> 1996-01-24 DVECP Krogh  M77CON instructions for conversion to C.
c>> 1995-11-20 DVECP Krogh  Changes for conversion to C.
c>> 1994-10-20 DVECP Krogh  Changes to use M77CON
c>> 1994-08-08 DVECP CLL Special treatment for text(1:1) .eq. '0'
c>> 1994-04-20 CLL Making DP & SP codes similar.
C>> 1992-04-22 CLL
C>> 1985-09-20 CLL
C>> 1983-07-05 DVECP  K. Stewart   recoded for math 77 library
C>> 1981-07-23 DVECP  Kris Stewart RECODED FOR PORTABLE ENVIRONMENT
C>> 1969-00-00 C. L. Lawson, JPL, Original code: MOUT/VOUT
c     ------------------------------------------------------------------
c                  Subroutine arguments
C
C  V(I),I=1,N    Vector to be output.
C  N             Number of vector components.
c  TEXT   Character string to be printed as a title.
c         First character in TEXT controls line spacing before title on
c         an impact printer.  For output to be viewed on a screen it is
c         safest to always use ' '.
c         ' ' = normal single space.
c         '0' = double space.
c         '1' = page advance.
c         '+' = suppress space, i.e., overprint.
c     ------------------------------------------------------------------
c  Method:  If the machine epsilon, is larger than 0.5*10**(-12), we set
c    MODE = 1 and print 8 numbers across a line, using a g15.7 format.
c    Otherwise we set MODE = 2 and print 6 numbers across a line, using
c    a g20.12 format.
C     ------------------------------------------------------------------
c--D replaces "?": ?VECP
c     ------------------------------------------------------------------
      external D1MACH
      integer  iblock, j, j1, j2, maxcol(2), mode, n, nblock
      double precision D1MACH
      double precision v(*)
      character text*(*)
      data maxcol/8, 6/
c     ------------------------------------------------------------------
c++ CODE for ~.C. is active
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
         if (mode .eq. 1) then
            write(*, '(1x,i3,a,i3,1x,1p,8g15.7 )') j1, ' TO ', j2,
     1         (v(j), j=j1, j2)
         else
            write(*, '(1x,i3,a,i3,1x,1p,6g20.12)') j1, ' TO ', j2,
     1         (v(j), j=j1, j2)
         end if
   70 continue
      end
