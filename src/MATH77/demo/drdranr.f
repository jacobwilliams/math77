c     program DRDRANR
c>> 2001-05-22 DRDRANR Krogh Minor change for making .f90 version.
c>> 1996-05-28 DRDRANR  Krogh Added external statement.
c>> 1994-10-19 DRDRANR  Krogh  Changes to use M77CON
c>> 1987-12-09 DRDRANR  Lawson  Initial Code.
c--D replaces "?": DR?RANR, ?RANR, ?STAT1, ?STAT2
c
c     Driver to demonstrate use of  DRANR to generate random numbers
c     from the Rayleigh distribution with parameter, ALPHA.
c     Program computes histogram for N numbers
c
      integer NCELLS
      parameter(NCELLS = 13+2)
      external DRANR
      double precision  ALPHA, DRANR, ONE, PIOV2
      double precision  STATS(5), TWO, Y1, Y2, YTAB(1), ZERO
      integer  I, IHIST(NCELLS), N
c
      parameter(ONE = 1.0D0, TWO = 2.0D0, ZERO = 0.0D0)
      data       N / 10000/
      data  Y1, Y2 /  0.0D0,  4.33333D0/
      data   ALPHA / 1.0D0 /
c     ------------------------------------------------------------------
      PIOV2 = TWO * atan(ONE)
      STATS(1) = ZERO
      do 20 I=1,N
c                             Get random number
         YTAB(1) =  DRANR(ALPHA)
c
c                          Accumulate statistics and histogram.
c
        call DSTAT1(YTAB(1), 1, STATS, IHIST, NCELLS, Y1, Y2)
   20 continue
c                          Print the statistics and histogram.
c
      print '(13x,a)', 'Rayleigh random numbers from DRANR'
      print '(13x,a,g12.4)','with ALPHA = ',ALPHA
      print '(1x,a/13x,g13.5,a,g13.5/1x)',
     * 'The Mean and Std. Dev. of the theoretical distribution are',
     * ALPHA * sqrt(PIOV2), ' and ',ALPHA * sqrt(TWO - PIOV2)
      call DSTAT2(STATS, IHIST, NCELLS, Y1, Y2)
      stop
      end
