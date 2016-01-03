      SUBROUTINE      IVECP  (V, N, TEXT)
c     IVECP..  Print a vector.
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-04-27 IVECP Krogh  Changes to use .C. and C%%.
c>> 1996-01-24 IVECP Krogh  M77CON instructions for conversion to C.
c>> 1994-08-08 CLL Special treatment for text(1:1) .eq. '0'
c>> 1994-04-20 CLL Making code in IVECP similar to DVECP and SVECP.
C>> 1992-04-22 CLL
C>> 1985-09-20 CLL
C>> 1983-07-05 IVECP  K. Stewart   recoded for math 77 library
C>> 1981-07-23 IVECP  Kris Stewart RECODED FOR PORTABLE ENVIRONMENT
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
C     ------------------------------------------------------------------
      integer  iblock, j, j1, j2, maxcol, n, nblock
      integer           v(*)
      character*(*)     text
      parameter(maxcol = 8)
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
      nblock=(n+maxcol-1)/maxcol
      j2 = 0

      do 70 iblock = 1,nblock
         j1 = j2 + 1
         j2 = min(j1+maxcol-1, n)
         write(*,'(1x,i3,a,i3,1x,8i15)') j1,' TO ',j2,(v(j),j=j1,j2)
   70 continue
      end
