      subroutine IDSTA1(ITAB, NI, ISTATS, XSTATS, IHIST, ILOW, NCELLS)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2000-12-01 IDSTA1 Krogh Removed unused parameter ONE.
c>> 1994-10-20 IDSTA1 Krogh Changes to use M77CON
c>> 1994-06-22 IDSTA1 CLL  Changed name to I[D/S]STA1.
C>> 1989-10-20 CLL
C>> 1987-05-01 ISTAT1 Lawson  Initial code.
c
c        This subr computes basic statistics for values of an integer
c     variable, say IVAL, storing the statistics as follows:
c
c             ISTATS(1) = Total count
c             ISTATS(2) = Min
c             ISTATS(3) = Max
c
c             XSTATS(1) = Mean
c             XSTATS(2) = Standard deviation
c
c     This subr also accumulates counts in IHIST() to develop a
c     histogram of values of IVAL.
c
c        The data to be treated is given in ITAB(1:NI).  If the
c     value of ISTATS(1) on entry is positive , say = NCOUNT, it is
c     assumed that NCOUNT data values have been processed previously
c     and results from that processing are in IHIST(), ISTATS(),
c     and XSTATS().  These results will be updated to reflect the
c     additional set of NI values.
c
c        Alternatively, if ISTATS(1) is zero, the initial contents of
c     ISTATS(2:3), XSTATS(), and IHIST() will be ignored and results
c     will be computed just for the present data set ITAB(1:NI).
c
c        The user must specify the range of the histogram
c     by setting ILOW and NCELLS.
c        The cells IHIST(2) through IHIST(NCELLS-1) will
c     be used to count occurences of IVAL = ILOW, ILOW+1, ..
c     ILOW + NCELLS - 3.
c        The end cells, IHIST(1) and IHIST(NCELLS) will be used to
c     count occurences of X less than ILOW or greater than
c     ILOW + NCELLS - 3, respectively.
c
c     The counting cells are associated with values of IVAL as follows:
c
c          Value of IVAL                   Counting cell
c
c          (-Infinity, ILOW-1]              IHIST(1)
c          [ILOW, ILOW+NCELLS-3]            IHIST(IVAL-ILOW +2)
c          [ILOW+NCELLS-2, Infinity)        IHIST(NCELLS)
c
c        After use of this subroutine, the user can call
c     ISTAT2, as appropriate, to produce a
c     printer-plot of the histogram and print the statistics.
c
c        Remark:  It is more efficient to call this subroutine one
c     time giving it N points rather than calling it N times giving it
c     one point each time, but the results will be the same to within
c     arithmetic limitations either way.
c     ------------------------------------------------------------------
c                    Subroutine arguments
c
c     ITAB()  [in]  Array of NI values whose statistics are to be
c           computed.
c     NI    [in]  Number of values given in ITAB().
c           Require NI .ge. 1.
c     ISTATS(), XSTATS()  [inout]  Arrays of length 3 and 2
c           respectively, into which statistics are or
c           will be stored.  Initial value of ISTATS(1) must be positive
c           if IHIST(), ISTATS() and XSTATS() contain prior results that
c           are to be updated.  Otherwise the initial value of STATS(1)
c           must be zero.
c     IHIST()  [inout]  Integer array of length at least NCELLS into
c           which counts will be accumulated.
c     ILOW    [in]  Value whose occurrences will be counted in IHIST(2).
c     NCELLS  [in]  Total number of classification regions.
c           Require NCELLS .ge. 3.
c     ------------------------------------------------------------------
c     C. L. Lawson and S. Y. Chiu, JPL, Apr 1987.
C     1989-10-20 CLL Moved integer declaration earlier to avoid warning
c     msg from Cray compiler.
c     ------------------------------------------------------------------
c--D replaces "?": I?STA1
c     ------------------------------------------------------------------
      integer NCELLS, NCOUNT, NI
      integer I, IHIST(NCELLS), ILOW, INDEX, ISTATS(3), ITAB(NI), IVAL
      integer J, NM1
      double precision DELTA, PREV, XSTATS(2)
      double precision SUMSQ, TERM, X, ZERO
      parameter(ZERO = 0.0D0)
c     ------------------------------------------------------------------
      if( NI .lt. 1) return
      NCOUNT = ISTATS(1)
      if(NCOUNT .eq. 0) then
         do 10 I=1,NCELLS
            IHIST(I) = 0
   10    continue
         ISTATS(2) = ITAB(1)
         ISTATS(3) = ISTATS(2)
         XSTATS(1) = ZERO
         SUMSQ     = ZERO
      else
         SUMSQ = (NCOUNT-1) * XSTATS(2)**2
      endif
c
         do 30 J=1,NI
            IVAL = ITAB(J)
            X = IVAL
            ISTATS(2) = min(IVAL, ISTATS(2))
            ISTATS(3) = max(IVAL, ISTATS(3))
            PREV = NCOUNT
            NCOUNT = NCOUNT + 1
            DELTA = X - XSTATS(1)
            TERM = DELTA / NCOUNT
            XSTATS(1) = XSTATS(1) + TERM
            SUMSQ = SUMSQ + PREV * DELTA * TERM
c
c           .                         Begin: Tally in histogram.
            INDEX = IVAL - ILOW + 2
            if(INDEX .lt. 1) then
               IHIST(1) = IHIST(1) + 1
            elseif(INDEX .gt. NCELLS) then
               IHIST(NCELLS) = IHIST(NCELLS) + 1
            else
               IHIST(INDEX) = IHIST(INDEX) + 1
            endif
c           .                         End: Tally in histogram.
   30    continue
      ISTATS(1) = NCOUNT
      NM1 = NCOUNT-1
      if(NM1 .gt. 0) then
         XSTATS(2) = sqrt(SUMSQ / NM1 )
      else
         XSTATS(2) = ZERO
      endif
      return
      end
