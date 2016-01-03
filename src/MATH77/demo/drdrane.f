c     program DRDRANE
c>> 2001-05-22 DRDRANE  Krogh Minor change for making .f90 version.
c>> 1996-05-28 DRDRANE  Krogh Added external statement.
c>> 1994-10-19 DRDRANE  Krogh  Changes to use M77CON
c>> 1987-12-09 DRDRANE  Lawson  Initial Code.
c--D replaces "?": DR?RANE, ?RANE, ?STAT1, ?STAT2
c
c     Driver to demonstrate use of DRANE to generate random numbers
c     from the exponential distribution with standard deviation, STDDEV.
c     Program computes histogram for N numbers
c     ------------------------------------------------------------------
      integer I, NCELLS
      parameter(NCELLS = 12+2)
      integer IHIST(NCELLS), N
      external DRANE
      double precision  DRANE, STATS(5), STDDEV, Y1, Y2, YTAB(1), ZERO
c
      parameter(ZERO = 0.0D0)
      data N / 10000 /
      data Y1, Y2 / 0.0D0, 6.0D0 /
      data STDDEV / 1.0D0 /
c     ------------------------------------------------------------------
      STATS(1) = ZERO
      do 20 I = 1, N
c                             Get random number
        YTAB(1) = DRANE(STDDEV)
c                          Accumulate statistics and histogram.
c
        call DSTAT1(YTAB(1), 1, STATS, IHIST, NCELLS, Y1, Y2)
   20 continue
c
      print '(11x,a)','Exponential random numbers from DRANE'
      print '(11x,a,g12.4,/1x)','with STDDEV = ',STDDEV
c
c                          Print the statistics and histogram.
c
      call DSTAT2(STATS, IHIST, NCELLS, Y1, Y2)
      stop
      end
