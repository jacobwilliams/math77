      subroutine DSTAT1(XTAB, NX, STATS, IHIST, NCELLS, X1, X2)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1997-04-25 DSTAT1 Krogh Simplified code (WVS suggestion)
C>> 1994-11-11 DSTAT1 Krogh Declared all vars.
C>> 1994-10-20 DSTAT1 Krogh Changes to use M77CON
C>> 1989-10-20 DSTAT1 CLL
C>> 1987-05-01 DSTAT1 Lawson  Initial code.
c--D replaces "?": ?STAT1, ?STAT2
c
c        This subr computes basic statistics for X, storing them is
c     STATS() as follows:
c
c             STATS(1) = Total count
c             STATS(2) = Min
c             STATS(3) = Max
c             STATS(4) = Mean
c             STATS(5) = Standard deviation
c
c     This subr also accumulates counts in IHIST() to develop a
c     histogram of values of X.
c
c        The data to be treated is given in XTAB(1:NX).  If the
c     value of STATS(1) on entry is positive , say = COUNT, it is
c     assumed that COUNT data values have been processed previously
c     and results from that processing are in IHIST() and STATS().
c     These results will be updated to reflect the additional set of
c     NX values.
c
c        Alternatively, if STATS(1) is zero, the initial contents of
c     STATS(2:5) and IHIST() will be ignored and results will be
c     computed just for the present data set XTAB(1:NX).
c
c        The user must specify the range and resolution of the histogram
c     by setting X1, X2, and NCELLS.  The end cells, IHIST(1) and
c     IHIST(NCELLS) will be used to count occurences of X less than X1
c     or greater than X2 respectively.
c     The cells IHIST(2) through IHIST(NCELLS-1) will
c     be used to count occurences of X in NCELLS-2 equal-length
c     subintervals of [X1, X2].
c
c        Define h = (X2 - X1)/(NCELLS-2).  X-intervals will be
c     associated with elements of IHIST() as follows.
c
c          X interval                   Counting cell
c
c          (-Infinity, X1)              IHIST(1)
c          [X1+(i-2)*h, X1+(i-1)*h)     IHIST(i), i = 2,...,NCELLS-2
c          [X2-h, X2]                   IHIST(NCELLS-1)
c          (X2, Infinity)               IHIST(NCELLS)
c
c        After use of this subroutine, the user can call
c     DSTAT2, to produce a printer-plot of the histogram and print the
c     statistics.
c
c        Remark:  It is more efficient to call this subroutine one
c     time giving it N points rather than calling it N times giving it
c     one point each time, but the results will be the same to within
c     arithmetic limitations either way.
c     ------------------------------------------------------------------
c                    Subroutine arguments
c
c     XTAB()  [in]  Array of NX values whose statistics are to be
c           computed.
c     NX     [in]  Number of values given in XTAB().
c           Require NX .ge. 1.
c     STATS()  [inout]  Array of length 5 into which statistics are or
c           will be stored.  Initial value of STATS(1) must be positive
c           if IHIST() and STATS() contain prior results that are to be
c           updated.  Otherwise the initial value of STATS(1) must be
c           zero.
c     IHIST()  [inout]  Integer array of length at least NCELLS into
c           which counts will be accumulated.
c     NCELLS   [in]  Total number of classification regions.
c           Require NCELLS .ge. 3.
c     X1,X2  [in]  Lower and upper boundaries, respectively defining
c           the range of y values to be classified into NCELLS-2 equal
c           intervals.  Require X1 < X2.
c     ------------------------------------------------------------------
c        The value of FAC is not critical.  It should be greater than
c     one.  The program does less computation each time the test
c     (abs(DELTA) .lt. TEST) is satisfied.  It will be true more
c     frequently if FAC is larger.  There is probably not much advantage
c     in setting FAC larger than 4, so 64 is probably more than ample.
c     ------------------------------------------------------------------
c     C. L. Lawson and S. Y. Chiu, JPL, Apr 1987.
C     1989-10-20 CLL Moved integer declaration earlier to avoid warning
c     msg from Cray compiler.
c     ------------------------------------------------------------------
      integer NCELLS, NX
      integer IHIST(NCELLS)
      integer I, INDEX, J
      double precision COUNT, DELTA, FAC, PREV
      double precision SCALE, RSCALE, SCLNEW, STATS(5), SUMSCL
      double precision TEMP, TEST, X, X1, X2, XMAX, XMEAN, XMIN
      double precision XTAB(NX)
      parameter(FAC = 64.0D0)
c     ------------------------------------------------------------------
      if(NX .lt. 1) return
      COUNT = STATS(1)
      if(COUNT .eq. 0.D0) then
         do 10 I=1,NCELLS
            IHIST(I) = 0
   10    continue
c
c                    Begin: Special for first point
c
         PREV = 0.D0
         X = XTAB(1)
         XMIN = X
         XMAX = X
         XMEAN = 0.D0
         TEST = -1.0D0
         SUMSCL = 0.D0
c                    End: Special for first point
c
      else
c                    Here when COUNT is > zero on entry.
         XMIN = STATS(2)
         XMAX = STATS(3)
         XMEAN = STATS(4)
c
         if(STATS(5) .eq. 0.D0) then
            TEST = -1.0D0
            SUMSCL = 0.D0
         else
c
c              STATS(5) contains the old value of Sigma.  Since it is
c              nonzero (positive) here, COUNT must be at least 2.
c
            SCALE =  STATS(5)
            RSCALE = 1.0D0/SCALE
            TEST = FAC * SCALE
            SUMSCL = COUNT - 1.0D0
         endif
      endif
c
      do 30 J = 1, NX
         PREV = COUNT
         COUNT = COUNT + 1.0D0
         X = XTAB(J)
         XMIN = min(X, XMIN)
         XMAX = max(X, XMAX)
c        .                             Begin: Tally in histogram.
         if(X .lt. X1) then
            IHIST(1) = IHIST(1) + 1
         elseif(X .gt. X2) then
            IHIST(NCELLS) = IHIST(NCELLS) + 1
         else
c                          Following stmt converts integer to float.
            TEMP = NCELLS-2
c                          Following stmt converts float to integer.
            INDEX = TEMP*(X-X1)/(X2-X1)
            IHIST(INDEX + 2) = IHIST(INDEX + 2) + 1
         endif
c        .                             End: Tally in histogram.
c
         DELTA = X - XMEAN
c
c              Expect abs(DELTA) .le. TEST most of the time.
c
         if(abs(DELTA) .gt. TEST) then
            if( DELTA .eq. 0.D0 ) go to 20
c
c                     Here  abs(DELTA) .gt. TEST  and  DELTA .ne. 0.D0
c                     Must compute new SCALE, RSCALE and TEST
c                     and update SUMSCL if it is nonzero.
c
            SCLNEW = abs(DELTA)
            RSCALE = 1.0D0 / SCLNEW
            TEST = FAC * SCLNEW
            if(SUMSCL .ne. 0.D0) SUMSCL = SUMSCL * (SCALE * RSCALE)**2
            SCALE = SCLNEW
         endif
         XMEAN = XMEAN + DELTA / COUNT
         SUMSCL = SUMSCL + (PREV/COUNT) * (DELTA*RSCALE)**2
   20    continue
   30 continue
c
      STATS(1) = COUNT
      STATS(2) = XMIN
      STATS(3) = XMAX
      STATS(4) = XMEAN
      if(PREV .eq. 0.D0) then
         STATS(5) = 0.D0
      else
         STATS(5) = SCALE * sqrt( SUMSCL / PREV )
      endif
      return
      end
