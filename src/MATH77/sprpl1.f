      SUBROUTINE SPRPL1(X,Y,NP,TITLE,XNAME,YNAME,
     *  NLINES, NCHARS, IMAGE, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-10-20 SPRPL1 Krogh  Changes to use M77CON
C>> 1994-08-05 SPRPL1 CLL Replaced 0.5 with 0.5001 for more consistent
c              rounding on different computers.
C>> 1992-02-14 SPRPL1 CLL Added choice of no. of rows & cols in output.
C>> 1990-10-29 PRPL1 CLL More changes to formatting of x-grid labels.
C>> 1990-10-22 PRPL1 CLL Added FAC, XSMALL, YSMALL.
C>> 1989-07-20 PRPL1  WV Snyder JPL Change ROW1 and ROW2 dimensions to *
C>> 1988-05-24 PRPL1  Lawson  Initial code.
C     SPRPL1  Enhanced version of PRPL1.  1992-01-29, CLL
C     PRPL1       This replaces EZPLTA of JPL$.
C     Only first 51 characters of YNAME() are used.
C     C.L.Lawson & Stella Chan,JPL,April 4,1983
c     Coded for Fortran 77.
C     1990 Oct.  Reworked formatting of labels for the x-grid lines.
c     No. of major subdivisions, KMAJX, returned by SCALK8 will be
c     3, 4, 5, 6, 7, or 8.  Generally we print KMAJX+1 labels, but print
c     only 5 for KMAJX = 8.
c     Will print numbers in the form -1.2345E-201 except when KMAJX = 7
c     in which case we reduce to     -1.234E-201.  Provides one or more
c     spaces between numbers.
c     Also reduced spacing between the ROW1() & ROW2() items.
c     ------------------------------------------------------------------
c                             SUBROUTINE ARGUMENTS
c
c   X(),Y()   Arrays of (x,y) coordinate pairs defining the
c             curve to be plotted.
C
c   NP        Number of (x,y) points to be plotted.
C
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
c--S replaces "?": ?PRPL1, ?PRPL3
C     ------------------------------------------------------------------
      integer BOTTOM
      integer I, IERR, ILINE, J
      integer L1, LEFT, NCHARS, NLINES, NP, RIGHT, TOP
      real             FACX, FACY
      real             X(NP), X1, X2, XMAX, XMIN
      real             Y(NP), Y1, Y2, YMAX, YMIN
      character*(*) TITLE, XNAME, YNAME
      character IMAGE(NLINES)*(*)
C     ------------------------------------------------------------------
C
C                          Find min's and max's of data values.
C
      X1=X(1)
      Y1=Y(1)
      X2=X1
      Y2=Y1
      do 20 I=2,NP
        X2=max(X2,X(I))
        X1=min(X1,X(I))
        Y2=max(Y2,Y(I))
        Y1=min(Y1,Y(I))
   20 continue
C
C        Subroutine SPRPL3 determine data values for the first and last
c        grid lines:  XMIN, XMAX,    YMIN, YMAX,
c        and corresponding index values for use in the character array
c        IMAGE()():   LEFT, RIGHT,   BOTTOM, TOP,
c        and constructs grid lines, labels, and titles in IMAGE()().
c
      call SPRPL3(X1, X2, Y1, Y2, XMIN, XMAX, YMIN, YMAX,
     *           LEFT, RIGHT, BOTTOM, TOP,
     *           TITLE, XNAME, YNAME, NLINES, NCHARS, IMAGE, IERR)
C
c        Plot the xy data points.
c        Note:  (BOTTOM - TOP) will be positive.
c
      FACX = real(RIGHT - LEFT) / (XMAX-XMIN)
      FACY = real(BOTTOM - TOP) / (YMAX-YMIN)
      do 40 J=1,NP
        ILINE = BOTTOM - int((Y(J)-YMIN) * FACY + 0.5001e0)
        L1    = LEFT +   int((X(J)-XMIN) * FACX + 0.5001e0)
        IMAGE(ILINE)(L1:L1) = '*'
   40 continue
C
      return
      end
