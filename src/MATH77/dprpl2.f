      SUBROUTINE DPRPL2(XY, IDIM, KC, JX, JY, NP, SYMBOL,
     *    TITLE, XNAME, YNAME, NLINES, NCHARS, IMAGE, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2011-11-29 DRPPL2 Krogh  Fixed comments for arguments.
C>> 1994-10-20 DPRPL2 Krogh  Changes to use M77CON
C>> 1994-08-05 DPRPL2 CLL Replaced 0.5 with 0.5001 for more consistent
c              rounding on different computers.
C>> 1992-02-14 DPRPL2  CLL
C>> 1988-05-24 CLL
C     1969 CLL and Jack Hatfield wrote KCPL & KCPLA for the JPL UNIVAC
c          LIB*JPL$ library.
c     1983 CLL and Stella Chan made Fortran 77 version, PRPL2.
c     1992-02-10 CLL.  Major changes to let user set NLINES and NCHAR.
c     ------------------------------------------------------------------
c                             SUBROUTINE ARGUMENTS
c
c  XY(,) Array of values from which $(x,\ y)$ coordinates of points
c        to be placed will be obtained under control of the parameters
c        JX(), JY(), and NP().
c
c  IDIM  Dimension of the first subscript in the XY array. Require IDIM
c        .ge. \max {NP(k), 1 .le. k .le. KC.
c
c  KC    Number of XY sets to be plotted. If KC .le. 0, the subroutine
c        will return taking no action.
c
c  JX()  JX(k) specifies the column (second subscript) of XY(,) to be
c        used as the x coordinates for the k-th xy set.
c
c  JY()  JY(k) specifies the column (second subscript) of XY(,) to be
c        used as the y coordinates for the k-th  xy set.
c
c  NP()  NP(k) specifies the number of xy pairs from XY(,) in the k-th
c        set to be plotted.
c
c  SYMBOL() SYMBOL(k) is the single character to be used for
c        point-plotting the k-th data set.
c
c   TITLE     Character string to be
c             printed above the plot grid as a title for the
c             graph.
C
c   XNAME     Character string to be
c             printed below the plot grid to identify the
c             abscissa variable.
C
c   YNAME     Character string to be
c             printed in a vertical column at the left of the
c             plot grid to identify the ordinate variable.
C
c   NLINES [in] Number of lines available for the output image.
c
c   NCHARS [in]  Number of characters per line available for the
c          output image.
c   IMAGE() [out,array of chars]  Array of at least NLINES elements,
c      each being a character variable of length at least NCHARS.
c      This subr will build the output plot image in this array.
c   IERR [out,integer]  Termination status indicator.  0 means ok.
c      1 means need larger NCHARS.  2 means need larger NLINES.
c     ------------------------------------------------------------------
c--D replaces "?": ?PRPL2, ?PRPL3
C     ------------------------------------------------------------------
      integer BOTTOM
      integer I, IDIM, IERR, ILINE, JX(*), JX1, JY(*), JY1, K, KC
      integer L1, LEFT, NCHARS, NLINES, NP(*), RIGHT, TOP
      double precision FACX, FACY
      double precision X1, X2, XMAX, XMIN, XY(IDIM,*)
      double precision Y1, Y2, YMAX, YMIN
      character*(*) TITLE, XNAME, YNAME
      character IMAGE(NLINES)*(*), SYMBOL(*)
C     ------------------------------------------------------------------
C
C                          Find min's and max's of data values.
C
      X1=XY(1,JX(1))
      X2=X1
      Y1=XY(1,JY(1))
      Y2=Y1
      do 20 K=1,KC
        JX1=JX(K)
        JY1=JY(K)
        do 15 I=1,NP(K)
          X2=max(X2,XY(I,JX1))
          X1=min(X1,XY(I,JX1))
          Y2=max(Y2,XY(I,JY1))
          Y1=min(Y1,XY(I,JY1))
   15   continue
   20 continue
C
C        Subroutine DPRPL3 determine data values for the first and last
c        grid lines:  XMIN, XMAX,    YMIN, YMAX,
c        and corresponding index values for use in the character array
c        IMAGE()():   LEFT, RIGHT,   BOTTOM, TOP,
c        and constructs grid lines, labels, and titles in IMAGE()().
c
      call DPRPL3(X1, X2, Y1, Y2, XMIN, XMAX, YMIN, YMAX,
     *           LEFT, RIGHT, BOTTOM, TOP,
     *           TITLE, XNAME, YNAME, NLINES, NCHARS, IMAGE, IERR)
C
c        Plot the xy data points.
c        Note: (BOTTOM - TOP) will be positive.
c
      FACX = dble(RIGHT - LEFT) / (XMAX-XMIN)
      FACY = dble(BOTTOM - TOP) / (YMAX-YMIN)
      DO 50 K=1,KC
         JX1=JX(K)
         JY1=JY(K)
         do 40 I=1,NP(K)
            ILINE = BOTTOM - int((XY(I,JY1)-YMIN) * FACY + 0.5001d0)
            L1    = LEFT +   int((XY(I,JX1)-XMIN) * FACX + 0.5001d0)
            IMAGE(ILINE)(L1:L1) = SYMBOL(K)
   40    continue
   50 continue
      return
      end
