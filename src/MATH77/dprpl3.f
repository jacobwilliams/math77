      SUBROUTINE DPRPL3(X1, X2, Y1, Y2, XMIN, XMAX, YMIN, YMAX,
     *           LEFT, RIGHT, BOTTOM, TOP,
     *           TITLE, XNAME, YNAME, NLINES, NCHARS, IMAGE, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2007-02/28 DRPRL3 Krogh  Fix for rounding problem in labels.
c>> 2005-12-06 DPRPL3 Krogh  Minor fixes for C conversion.
c>> 2001-10-05 DPRPL3 Krogh  Fixed formats to use "1p,e" not "1pe"
c>> 1996-06-21 DPRPL3 Krogh  Changes to use .C. and C%%.
C>> 1996-01-23 DPRPL3 Krogh  More changes to simplify conversion to C.
C>> 1995-11-14 DPRPL3 Krogh  Changes to simplify conversion to C.
c>> 1994-11-02 DPRPL3 Krogh  Changes to use M77CON
c>> 1992-09-29 DPRPL3 WVS Changed tabs to spaces
c>> 1992-04-29 DPRPL3 CAO Changed ' 's (from 0-length string correction)
c>> 1992-04-07 CAO Changed 0-length strings to spaces(error from VAX com
c>> 1992-03-17 DPRPL3 CLL Removed F90 syntax.
c>> 1992-02-24 DPRPL3 CLL Changed IMAGE*(NIMAGE) to IMAGE*(*) in -PRPL5
c>> 1992-02-21 DPRPL3 CLL Changed a > to .gt.
c>> 1992-02-14 DPRPL3 CLL
C>> 1992-01-29 SPRPL1 CLL Added choice of no. of rows & cols in output.
C>> 1990-10-29 PRPL1 CLL More changes to formatting of x-grid labels.
C>> 1990-10-22 PRPL1 CLL Added FAC, XSMALL, YSMALL.
C>> 1988-05-24 PRPL1  Lawson  Initial code.
C>> 1983-04-04 C.L.Lawson & Stella Chan,JPL, Coded for Fortran 77.
C     Subr DPRPL3 will build a grid and place numeric grid labels and
c     titles in IMAGE()().  Also returns values in
c     XMIN, XMAX, YMIN, YMAX,   LEFT, RIGHT, BOTTOM, TOP.
c     ------------------------------------------------------------------
c                             SUBROUTINE ARGUMENTS
c
c  X1, X2, Y1, Y2 [in, floating point]  Min and max values of x and y
c             values to be plotted.
c  XMIN, XMAX, YMIN, YMAX [out, floating point]  Values to be assigned
c     to the edges of the plot grid.
c  LEFT, RIGHT, BOTTOM, TOP [out, integer]  Indices for use in IMAGE()()
c     for locating the edges of the plot grid.
c  TITLE [in,character]  Character string to be printed above the plot
c        grid as a title for the graph.
c  XNAME [in,character]  Character string to be  printed below the plot
c        grid to identify the abscissa variable.
c  YNAME [in,character]  Character string to be printed in a vertical
c        column at the left of the plot grid to identify the ordinate
c        variable.
c  NLINES [in] Number of lines available in IMAGE()() for the output
c            image.
c  NCHARS [in]  Number of characters per line available in IMAGE()() for
c            the output image.
c  IMAGE() [out,array of chars]  Array of at least NLINES elements,
c      each being a character variable of length at least NCHARS.
c      This subr will place grid and labeling characters in this array.
c  IERR [out,integer]  Termination status indicator.  0 means ok.
c     1 means need larger NCHARS.  2 means need larger NLINES.
c     ------------------------------------------------------------------
c                  Descriptions of some of the internal variables.
c
c  NEEDX, NEEDY [integer parameters]  These establish the minimum size
c     of the plotting region.  We require at least NEEDX columns from
c     the leftmost to the rightmost grid line, including these grid
c     lines.  We require at least NEEDY lines from the top to the bottom
c     grid line, including these grid lines.  The nominal setting of
c     NEEDX and NEEDY is 11 each.  The code should function with any
c     setting greater than 1, but for any hope for a reasonably
c     useful plot these should should not be too small.
c     ------------------------------------------------------------------
c--D replaces "?": ?PRPL3, ?PRPL4, ?PRPL5
c     Also uses IERM1, IERV1
C     ------------------------------------------------------------------
c++(~.C.) Default SIZEX=',KSIZEX', SIZEY=',KSIZEY', SIZED=',KSIZED'
c++(.C.) Default SIZEX=',KSIZEX,KSIZ2X', SIZEY=',KSIZEY,KSIZ2Y'
c++(.C.) Default SIZED=',KSIZED,KSIZ2D'
c Note that in order for these replacements to work, the strings must be
c start with a ",", and KSIZE must not be be preceded immediately by a
c "," where the replacement is not desired.
c++ Replace ",KSIZEX" = SIZEX
c++ Replace ",KSIZEY" = SIZEY
c++ Replace ",KSIZED" = SIZED
      integer BOTTOM, DELX1, DELX2
      integer I, IERR, ILINE, INDEX, KMAJX, KMAJY, KMINX, KMINY
      integer KTESTX,KSIZEX,KSIZEY
      integer L0, L1, LCOUNT, LEFT, NCHARS, NEEDX, NEEDY, NLINES
      integer RIGHT, TLEN, TOP, TOTX, TOTY, TOTYM1
      integer XLEN, YLEN, YLAB1, YLAB2, YNSPCE
      double precision FKMAJX, FKMAJY, FYIND
      double precision X1, X2, XIFAC, XMAX, XMIN
      double precision Y1, YIFAC, Y2, YFAC, YMAX, YMIN, YSMALL, YVAL
      character*(*) TITLE, XNAME, YNAME
      character FMTY2*17, FMTX*15, FMTY*15, IMAGE(NLINES)*(*)
      parameter(NEEDX = 11, NEEDY = 11)
C     ------------------------------------------------------------------
c++ CODE for ~.C. is active
      do 10 I = 1,NLINES
         IMAGE(I) = ' '
   10 continue
c++ CODE for .C. is inactive
C%%    long int j;
C%%    char numstring[40];
C%%    for( i = 1L; i <= nlines; i++ ){
C%%       for( j = 1; j <= nchars; j++ ) *IMAGE(i-1L, j-1L) = ' ';
C%%       *IMAGE(i-1L, nchars) = '\0'; }
c++ END
C
C       Determine values for the first and last grid lines:
c                    XMIN, XMAX,    YMIN, YMAX,
c       the number of major grid intervals: KMAJX, KMAJY,
c       the space needed for grid line labels: KSIZEX, KSIZEY,
c       and the formats for grid line labels: FMTX, FMTY.
C
      call DPRPL4(X1,X2,XMIN,XMAX,KMAJX,KMINX,FMTX,KSIZEX)
      call DPRPL4(Y1,Y2,YMIN,YMAX,KMAJY,KMINY,FMTY,KSIZEY)
      YSMALL=max(abs(YMIN),abs(YMAX))*0.0001d0
      FKMAJX=dble(KMAJX)
c
c     YNSPCE is the no. of horizontal char positions we allocate for
c     the (vertical) YNAME.
c     The leftmost and rightmost grid lines are at char positions
c     LEFT and RIGHT.  We set RIGHT = NCHARS-1, but setting LEFT depends
c     on KSIZEX and KSIZEY to allow room for the y-grid labels and for
c     the leftmost x-grid label.
c     If an x-grid label is centered at char position IC it will begin
c     in position IC-DELX1 and end in position IC+DELX2.  The leftmost
c     x-grid label will be centered at LEFT.  The rightmost x-grid label
c     will end at position NCHARS.
c     The y-grid lables all start at char position YLAB1 and end at
c     YLAB2.
c
c++ CODE for ~.C. is active
      FMTY2 = '(' // FMTY // ')'
      if (YNAME .eq. ' ') then
         YNSPCE = 0
      else
         YLEN = len(YNAME)
         YNSPCE = 2
      end if
c++ CODE for .C. is inactive
C%%      ylen = strlen( yname );
C%%      ynspce = ((int)strspn(yname, " ") == ylen ) ? 0 : 2;
c++ END
      DELX1 = KSIZEX / 2
      DELX2 = KSIZEX -DELX1 - 1
      LEFT = YNSPCE + 1 +  max(KSIZEY + 1, DELX1)
      YLAB2 = LEFT - 2
      YLAB1 = YLAB2 - KSIZEY + 1
      RIGHT = NCHARS - 1
      TOTX = RIGHT - LEFT + 1
      KTESTX = max(NEEDX, DELX2 + 1)
      if (TOTX .lt. KTESTX) then
         IERR = 1
         call IERM1('DPRPL3',IERR,0,
     *      'Need larger NCHARS to plot and label the given data.',
     *      'Have NCHARS',NCHARS,',')
         call IERV1('Need at least NCHARS',NCHARS + KTESTX - TOTX,'.')
         return
      end if
c
c                                    Process TITLE if any, and set TOP.
c
c++ CODE for ~.C. is active
      if (TITLE .eq. ' ') then
         TOP = 1
      else
         TOP = 2
         TLEN = len(TITLE)
         if (TLEN .le. TOTX) then
            L1 = NCHARS - TLEN + 1 - (TOTX-TLEN)/2
            IMAGE(1)(L1:L1+TLEN-1) = TITLE
         else if (TLEN .le. NCHARS) then
            L1 = NCHARS - TLEN + 1
            IMAGE(1)(L1:L1+TLEN-1) = TITLE
         else
            IMAGE(1) = TITLE(1:NCHARS)
         end if
      end if
c++ CODE for .C. is inactive
C%%   tlen = strlen( title );
C%%   if ((int)strspn(title, " ") == tlen) {
C%%      *top = 1;
C%%   } else {
C         TOP = 2
C         if (TLEN .le. TOTX) then
C            L1 = NCHARS - TLEN - (TOTX-TLEN)/2
C%%            for (i = 0; i < tlen; i++) *IMAGE(0, l1+i) = title[i];
C         else if (TLEN .le. NCHARS) then
C            L1 = NCHARS - TLEN
C%%            for (i = 0; i < tlen; i++) *IMAGE(0, l1+i) = title[i];
C         else
c++ CODE for .C. is inactive
C%%            for (i = 0; i < nchars; i++) *IMAGE(0, i) = title[i];
C         end if
C%%    }
c++ END
c
c                                 Process XNAME if any, and set BOTTOM.
c
c++ CODE for ~.C. is active
      if (XNAME .eq. ' ') then
         BOTTOM = NLINES - 1
      else
         BOTTOM = NLINES - 2
         XLEN = len(XNAME)
         if (XLEN .le. TOTX) then
            L1 = NCHARS - XLEN + 1 - (TOTX-XLEN)/2
            IMAGE(NLINES)(L1:L1+XLEN-1) = XNAME
         else if (XLEN .le. NCHARS) then
            L1 = NCHARS - XLEN + 1
            IMAGE(NLINES)(L1:L1+XLEN-1) = XNAME
         else
            IMAGE(NLINES) = XNAME(1:NCHARS)
         end if
      end if
c++ CODE for .C. is inactive
C%%   xlen = strlen( xname );
C%%   if ((int)strspn(xname, " ") == xlen) {
C%%      *bottom = nlines - 1;
C%%   } else {
C         BOTTOM = NLINES - 2
C         if (XLEN .le. TOTX) then
C            L1 = NCHARS - XLEN  - (TOTX-XLEN)/2
C%%       for (i = 0; i < xlen; i++) *IMAGE(nlines-1, l1+i) = xname[i];
C         else if (XLEN .le. NCHARS) then
C            L1 = NCHARS - XLEN
C%%      for (i = 0; i < xlen; i++) *IMAGE(nlines-1, l1+i) = xname[i];
C         else
C%%      for (i = 0; i < nchars; i++) *IMAGE(nlines-1, l1+i) = xname[i];
C         end if
C%%    }
c++ END
      TOTY = BOTTOM - TOP + 1
      TOTYM1 = TOTY - 1
      if (TOTY .lt. NEEDY) then
         IERR = 2
         call IERM1('DPRPL3',IERR,0,
     *      'Need larger NLINES.',
     *      'Have NLINES',NLINES,',')
         call IERV1('Need at least NLINES',NLINES + NEEDY - TOTY,'.')
         return
      end if
c
c        Now have LEFT, RIGHT, TOP, and BOTTOM set.  Recall that
c        TOP is a smaller number than BOTTOM.
c        These are the indices in IMAGE()() of the edges of the plot
c        grid.
c
c                                          Process YNAME, if any.
      if (YNSPCE .ne. 0) then
         if (YLEN .le. NLINES) then
            L0 =  (NLINES-YLEN)/2
            LCOUNT = YLEN
         else
            L0 = 0
            LCOUNT = NLINES
         end if
         do 25 I = 1,LCOUNT
            IMAGE(L0+I)(1:1) = YNAME(I:I)
   25    continue
      end if
c
c                                          Draw grid box.
c
      do 26 I = TOP, BOTTOM
         IMAGE(I)(LEFT:LEFT) = '|'
         IMAGE(I)(RIGHT:RIGHT) = '|'
   26 continue
      do 27 I = LEFT+1, RIGHT-1
         IMAGE(TOP)(I:I) = '-'
         IMAGE(BOTTOM)(I:I) = '-'
   27 continue
c
c                       Place y grid line labels and "<" marks at
c                       right end of y grid lines.
c
      FKMAJY=dble(KMAJY)
      YFAC = (YMAX-YMIN)/FKMAJY
      YIFAC = dble(TOTYM1) / FKMAJY
      do 30 INDEX = 0,KMAJY
         FYIND=dble(INDEX)
         ILINE = BOTTOM - int( FYIND * YIFAC + 0.5d0 )
         YVAL = YMIN + FYIND * YFAC
         if (YVAL - ANINT(YVAL) .le. YSMALL) YVAL = ANINT(YVAL)
c++ CODE for ~.C. is active
         if ( abs(YVAL) .lt. YSMALL) then
            IMAGE(ILINE)(YLAB2-1:YLAB2) = '0.'
         else
           write(IMAGE(ILINE)(YLAB1:YLAB2), FMTY2) YVAL
         end if
         IMAGE(ILINE)(RIGHT+1:RIGHT+1) = '<'
c++ CODE for .C. is inactive
C%%     if( fabs( yval ) < ysmall ){
C%%         strncpy(IMAGE(iline - 1L, ylab2 - 2L), "0.", 2);
C%%     } else{
C%%/*             Here fmty is either "%*.*f" or "%*.*e"     */
C%%       sprintf( numstring, (const char*)fmty, ksizey, ksiz2y, yval);
C%%         strncpy(IMAGE(iline - 1L, ylab1 - 1L), numstring, ksizey);
C%%     }
C%%     IMAGE(iline - 1L,0)[(short)*right] = '<';
c++ END
   30 continue
c
c                       Place x grid line labels.
c
      call DPRPL5(XMIN,XMAX,KMAJX,FMTX,KSIZEX,LEFT,
     *            NCHARS,IMAGE(BOTTOM+1))
c
c            Place "|" marks at top and bottom of interior x grid lines.
c
      XIFAC = dble(TOTX-1)/FKMAJX
      do 35 INDEX = 1,KMAJX-1
        L1 = LEFT + int( dble(INDEX) * XIFAC + 0.5d0 )
        IMAGE(TOP)(L1:L1) = '|'
        IMAGE(BOTTOM)(L1:L1) = '|'
   35 continue
      return
      end
c     ==================================================================
      SUBROUTINE DPRPL4(A,B,C,D,KMAJOR,KMINOR,FMT,KSIZED)
C     .  Copyright (C) 1992, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1992-02-06 DPRPL4 CLL   Determine info for grid labeling.
C>> 1989-10-31 SCALK8 CLL   Force rounding of MKAJOR for Cray.
C>> 1985-08-02 SCALK8 Lawson  Initial code.
c  DPRPL4..   Select pleasant grid boundaries and build a format string.
c  C.L.LAWSON,JPL,1965 JUL  7
c  C.L.L.,JPL,1967 FEB 20 CHANGED TO MAKE C AND D ALWAYS
c  BE MULTIPLES OF UNIT.
c  MODIFIED BY CLL 7/14/72 FOR A,B CLOSE TO UNDER/OVER FLOW
c     ------------------------------------------------------------------
c                    Subroutine arguments
c
c  Input:   A, B    Output:  C, D, KMAJOR, KMINOR, FMT, KSIZED
C
c  A and B are min and max (or max and min) values
c     of a variable (either abcissa or ordinate) to be graphed.
c  KMAJOR is the recommended no. of major grid subdivisions.
c  KMINOR is the recommended no. of subdivisions within a major
c     subdivision.
c  C and D are pleasant values to be assigned to the leftmost and
c     rightmost grid boundaries.  Will satisfy C < D.
c     The closed interval [C,D] will generally contain the values A and
c     B, except that A and/or B may be outside [C,D] by a distance of
c     up to 0.0001 * (D-C).
c  FMT [out, char*15]  Recommended format for a single grid label value.
c     This will not contain parentheses.
c     Examples: 'ss,f05.01     ' or 'ss,1p,e15.07d02'
c  KSIZED [out, integer]  No. of char positions that will be used to
c     display a number when it is output using the format, FMT.
c     ------------------------------------------------------------------
c             Description of some of the internal variables.
c
c  ESIZE, EXSIZE, FSIZE [integers]  ESIZE and FSIZE give the total no.
c     of char positions needed for an E or F format, respectively.
c     EXSIZE is the no. of digit positions needed in the exponent part
c     of an E format.
c
c  HI, LO, COUNT [integers]  COUNT = HI - LO + 1.
c     HI and LO indicate the position of the most and
c     least significant digit that must be repesented in the printed
c     output.  Number template:    x   x   x   x . x   x   x
c               Digit position:    3   2   1   0  -1  -2  -3
c     COUNT is the number of digits needed in the output.
c     For example for the number 593.62 we would have
c     HI = 2, LO = -2, and COUNT = 5.
c
c  SPAN(), KMAJ(), EKMN() [integers]  These prestored tables are
c     related by SPAN(i) = KMAJ(i) * EKMN(i).
c     This subr chooses a pair of values
c     KMAJ(i) and EKMN(i) to return as KMAJOR and KMINOR.  The KMAJ()
c     table may be commented out, in which case its values will be
c     computed as needed by KMAJ(i) = SPAN(i) / EKMIN(i).
c     The prestored values in these tables are chosen so the values of
c     SPAN() are > 10, and .le. 100, and in increasing order, and give
c     somewhat uniform logorathmic coverage of the range from 10 to 100.
c     The values of EKMN() are limited to be 2, 5, or 10.
c     The values of KMAJ() are limited to be .ge. 3 and .le. 10.
c     ------------------------------------------------------------------
      integer COUNT, ESIZE, EXSIZE, FSIZE, HI
      integer I,IG,IMAX,K,KMAJOR,KMINOR,KSIZED,LO
      parameter(IMAX = 18)
      double precision A, A1, A2, B, B1, B2, BMA, C, D
      double precision EKMN(IMAX), F,  FRAC, P, SMALL, SPAN(IMAX), TEMP
      double precision UNIT, V, X
      character FMT*15
      data SPAN / 12.0d0, 14.0d0,15.0d0,16.0d0,18.0d0,20.0d0,20.0D0,
     1     25.0d0,30.0d0, 35.0d0,40.0d0,45.0d0,50.0d0,60.0d0,70.0d0,
     2     80.0d0,90.0d0,100.0d0 /
C     data KMAJ /  6.0d0,  7.0d0, 3.0d0, 8.0d0, 9.0d0,10.0d0, 4.0d0,
C    1      5.0d0, 3.0d0,  7.0d0, 4.0d0, 9.0d0, 5.0d0, 6.0d0, 7.0d0,
C    2      8.0d0, 9.0d0, 10.0d0 /
      data EKMN /  2.0d0,  2.0d0, 5.0d0, 2.0d0, 2.0d0, 2.0d0, 5.0d0,
     1      5.0d0,10.0d0,  5.0d0,10.0d0, 5.0d0,10.0d0,10.0d0,10.0d0,
     2     10.0d0,10.0d0, 10.0d0/

c     ------------------------------------------------------------------
C                          CHANGE A,B TO A1,B1, with A1 < B1
      if (A .lt. B) then
         A1 = A
         B1 = B
      else if (A .gt. B) then
         A1= B
         B1= A
      else if (A .eq. 0.0d0) then
         A1=-1.0d0
         B1=+1.0d0
      else
         SMALL= 0.01d0 * abs(A)
         A1=A - SMALL
         B1=B + SMALL
      end if
C                 Now we have A1 < B1
C
C                 PERTURB A1 and B1 TO AVOID BAD
C                 DECISIONS DUE TO ROUND-OFF.
      A2=A1
      B2=B1
      SMALL=(B1-A1)* 0.0001d0
      if (A1.ne.0.0d0) A1=A1+SMALL
      if (B1.ne.0.0d0) B1=B1-SMALL
      BMA=B1-A1
      if (BMA .le. 0.0d0) then
         A1=A2
         B1=B2
         BMA=B1-A1
      end if
C                Convert BMA to X*10**G = X*P with 10. < X .le. 100.
      V=log10(BMA)
      IG = int(V)
      F = V - dble(IG)
      if (F .le. 0.0d0) F=F+1.0d0
      X=10.0d0 ** (F+1.0d0)
      P=BMA/X
      if (X .le. 10.0d0) then
         X=X*10.0d0
         P=P/10.0d0
         IG = IG - 1
      end if
C                         ENTER SPAN( ) TABLE USING X
      do 90 I=1,IMAX
         if (X .le. SPAN(I))go to 95
   90 continue
      I=IMAX
   95 continue
C                     DETERMINE WHETHER SPAN(I) CAN BE USED
      do 105 K=1,3
  100    UNIT=EKMN(I)*P
         C=UNIT * aint(A1/UNIT)
         if (C .gt. A1) C=C-UNIT
         D=C+SPAN(I)*P
         if (B1 .le. D) go to 110
         I=I+1
         if (I .le. IMAX) go to 100
         I=1
         P=P*10.0d0
  105 continue
C                           TROUBLE: A or B close to UNDER/OVER FLOW
c
      print '('' DPRPL4 ERROR.. A,B,C,D,='',4e20.8)', A,B,C,D
  110 continue
      if ( abs(C) .lt. 0.0001d0 * BMA) then
         C = 0.0d0
      else if ( abs(D) .lt. 0.0001d0 * BMA) then
         D = 0.0d0
      end if
c
c     The ratio SPAN(I)/EKMN(I) is an exact integer value, however
c     the Cray X/MP sometimes returns a value less than the exact value,
c     so we take the nearest integer value.
c
      KMAJOR=nint( SPAN(I)/EKMN(I) )
      KMINOR=EKMN(I)
c
      TEMP = log10(max(abs(C), abs(D)))
      HI = int(TEMP)
      FRAC = TEMP - dble(HI)
      if (FRAC .lt. 0.0d0) HI = HI - 1
c                                     nint() rounds to nearest integer.
      LO = nint(log10(P))
      if (KMINOR .eq. 10.0d0) LO = LO + 1
*     print*,'DPRPL4..'
*     print'(/a,a/)','       A             B    KMAJOR KMINOR ',
*    *      ' C             D           HI    LO'
*     print'(/1x,2g14.6,2i3,2g14.6,2i6)',
*    *    A, B, KMAJOR, KMINOR, C, D, HI, LO
c
c        Set FSIZE to No. of char positions needed if F format is used.
c        First we assume C and D are each nonnegative.
c
      COUNT = HI - LO + 1
      if (HI .lt. 0) then
c                                       0.00xxx
         FSIZE = COUNT - HI + 1
      else if (LO .gt. -1) then
c                                       xxx00.
         FSIZE = COUNT + LO + 1
      else
c                                       x.xx
         FSIZE = COUNT + 1
      end if
c
c     Set ESIZE to No. of char positions needed if E format is used.
c     First we assume C and D are each nonnegative.
c     EXSIZE is the No. of digit positions needed in the exponent part.
c
      if (HI .eq. 0) then
         EXSIZE = 1
      else
         EXSIZE = 1 + int(log10(dble(abs(HI))))
      end if
c                                          x.xxE+yy
      ESIZE = COUNT + 3 + EXSIZE
c
c                     Adjust FSIZE and ESIZE if C or D is negative.
c
      if (C .lt. 0.0d0 .or. D .lt. 0.0D0) then
         FSIZE = FSIZE + 1
         ESIZE = ESIZE + 1
      end if
*     print'(a,i4,a,i4,a,i4)',
*    *   ' FSIZE=',FSIZE,',  ESIZE=',ESIZE,',  EXSIZE=',EXSIZE
c
c                            Build the format string.
c
      if (FSIZE .le. ESIZE) then
         KSIZED = FSIZE
c++ CODE for ~.C. is active
         FMT(1:15) = 'ss,f  .        '
         write(FMT(5:6),'(i2.2)') FSIZE
         write(FMT(8:9),'(i2.2)') max(0,-LO)
c++ CODE for .C. is inactive
C%%        strcpy(fmt, "%*.*f");
C          KSIZ2D = max(0, -LO)
c++ END
      else
         KSIZED = ESIZE
c++ CODE for ~.C. is active
         FMT(1:15) = 'ss,1p,e  .  e  '
         write(FMT(8:15),'(i2.2,''.'',i2.2,''e'',i2.2)')
     *     ESIZE, COUNT-1, EXSIZE
c++ CODE for .C. is inactive
C%%        strcpy(fmt, "%*.*e");
c          KSIZ2d = COUNT - 1
c++ END
      end if
      return
      end
c     ==================================================================
      SUBROUTINE DPRPL5(C,D,KMAJX,FMT,KSIZED,LEFT,NIMAGE,IMAGE)
C     .  Copyright (C) 1992, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1992-02-06 DPRPL5 CLL
c     DPRPL5 builds a print line of numeric grid labels for the x axis.
c     ------------------------------------------------------------------
c                    Subroutine arguments
c
c  Input:   C, D, KMAJX, FMT, KSIZED, LEFT, NIMAGE   Output:  IMAGE
C
c  C and D are pleasant values that have been assigned to the leftmost
c     and rightmost grid boundaries.  Will satisfy C < D.
c  KMAJX is the recommended no. of major grid subdivisions.
c  FMT [in, char*15]  Recommended format for a single grid label value.
c     This will not contain parentheses.
c     Examples: 'ss,f05.01     ' or 'ss,1p,e15.07d02'
c  KSIZED [in, integer]  No. of char positions that will be used to
c     display a number when it is output using the format, FMT.
c  LEFT [in, integer]  Index of position in IMAGE aligned with the
c     leftmost grid line.  Indexing is 1-based.
c  NIMAGE [in, integer]  Index of last useable position in IMAGE.
c     The rightmost grid line aligns with index IMAGE-1.
c  IMAGE [out, char*NIMAGE]  Character string in which this subr will
c     build a print line of numeric grid labels.
c     ------------------------------------------------------------------
c  We assume tests have been made in [D/S]PRPL3 to assure there is
c  enough space in IMAGE() to at least place the label for the leftmost
c  x grid line.  It extends from LEFT-DELX1 to LEFT+DELX2.  DELX1 and
c  DELX2 are computed in [D/S]PRPL3 for use in tests, and are computed
c  again in this subroutine for use in placing grid labels.
c  If there is not enough space for other grid labels we just omit them.
c     ------------------------------------------------------------------
      integer AVAIL1, AVAIL2, DELX1, DELX2
      integer I1,I2,IC,INDEX,KMAJX,KSIZED,LEFT,NIMAGE
      double precision C, D, FKMAJX, FXIND, IXFAC, XFAC, XSMALL, XVAL
      character IMAGE*(*)
c     ------------------------------------------------------------------
c++ CODE for ~.C. is active
      character FMT*15, FMT2*17
      IMAGE(1:NIMAGE) = ' '
      FMT2 = '(' // FMT // ')'
c++ CODE for .C. is inactive
c      character FMT*6
C%%    char numstring[40];
C%%   for (i1 = 0; i1 < nimage; i1++) image[i1] = ' ';
c++ END
      AVAIL1 = 1
      AVAIL2 = NIMAGE
      XSMALL = 0.0001d0 * (D-C)
      DELX1 = KSIZED / 2
      DELX2 = KSIZED -DELX1 - 1
c
c     When centering a label on position IC, the label will occupy
c     positions from IC - DELX1 through IC + DELX2.
c
c                       Try to place leftmost grid label.
c
      IC = LEFT
      if ( abs(C) .lt. XSMALL) then
         IMAGE(IC:IC) = '0'
         AVAIL1 = IC+2
      else
         I1 = IC - DELX1
         I2 = IC + DELX2
         if (I1 .ge. AVAIL1 .and. I2 .le. AVAIL2) then
C%%         sprintf( numstring, (const char*)fmt, ksized, ksiz2d, c);
C%%         strncpy( &image[(short)(i1-1)], numstring, ksized);
            write(IMAGE(I1:I2), FMT2) C
            AVAIL1 = I2+2
         end if
      end if
c
c                       Try to place rightmost grid label.
c
      IC = NIMAGE - 1
      if ( abs(D) .lt. XSMALL) then
         if (IC .ge. AVAIL1 ) then
            IMAGE(IC:IC) = '0'
            AVAIL2 = IC-2
         end if
      else
         I1 = NIMAGE - KSIZED + 1
         I2 = NIMAGE
         if (I1 .ge. AVAIL1 ) then
C%%          sprintf( numstring, (const char*)fmt, ksized, ksiz2d, d);
C%%          strncpy( &image[(short)(i1-1)], numstring, ksized);
            write(IMAGE(I1:I2), FMT2) D
            AVAIL2 = I1-2
         end if
      end if
c
c                       Try to place interior grid labels.
c
      FKMAJX=dble(KMAJX)
      XFAC = (D-C)/FKMAJX
      IXFAC = dble(NIMAGE - LEFT - 1) / FKMAJX
      do 30 INDEX = 1,KMAJX-1
         FXIND=dble(INDEX)
         IC = LEFT + nint( FXIND * IXFAC )
         XVAL = C + FXIND * XFAC
         if ( abs(XVAL) .lt. XSMALL) then
            if (IC .ge. AVAIL1 .and. IC .le. AVAIL2) then
               IMAGE(IC:IC) = '0'
               AVAIL1 = IC+2
            end if
         else
            I1 = IC - DELX1
            I2 = IC + DELX2
            if (I1 .ge. AVAIL1 .and. I2 .le. AVAIL2) then
              if (XVAL - ANINT(XVAL) .lt. XSMALL) XVAL = ANINT(XVAL)
C%%        sprintf( numstring, (const char*)fmt,ksized, ksiz2d, xval);
C%%        strncpy( &image[(short)(i1-1L)], numstring, ksized);
              write(IMAGE(I1:I2), FMT2) XVAL
              AVAIL1 = I2+2
            end if
         end if
   30 continue
      return
      end
