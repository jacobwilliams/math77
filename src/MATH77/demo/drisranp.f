c     program DRISRANP
c>> 1996-07-09 DRISRANP Krogh Added external statement.
c>> 1994-10-19 DRISRANP Krogh  Changes to use M77CON
c>> 1994-06-23 DRISRANP Changing name to DRI[D/S]RANP.
c>> 1987-12-09 DRISRANP  Lawson  Initial Code.
c
c     Driver to demonstrate use of ISRANP to generate random integers
c     from the Poisson distribution with mean, XMEAN.
c     Program computes histogram for N samples.
c     ------------------------------------------------------------------
c--S replaces "?": DRI?RANP, I?RANP, I?STA1, I?STA2
c     ------------------------------------------------------------------
      integer NCELLS, NI
      parameter(NCELLS = 7+2, NI = 100)
      external ISRANP
      integer I, IHIST(NCELLS), ISRANP, ISTATS(3), ITAB(NI), ILOW, J, N
      real             XMEAN, XSTATS(2)
c
      data N / 10000 /
      data ILOW / 0 /
      data XMEAN / 1.0e0 /
c     ------------------------------------------------------------------
      ISTATS(1) = 0
      do 30 J = 1, N/NI
         do 20 I = 1, NI
c                                   Get random integer.
           ITAB(I) =  ISRANP(XMEAN)
   20    continue
c                                 Accumulate statistics and histogram.
c
        call ISSTA1(ITAB, NI, ISTATS, XSTATS, IHIST, ILOW, NCELLS)
   30 continue
c                             Print the statistics and histogram.
c
      print '(11x,''Poisson random integers from ISRANP'')'
      print '(11x,''with XMEAN = '',g12.4,/1x)', XMEAN
      call ISSTA2(ISTATS, XSTATS, IHIST, ILOW, NCELLS)
      end
