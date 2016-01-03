c     program DRSRANR
c>> 2001-05-22 DRSRANR Krogh Minor change for making .f90 version.
c>> 1996-05-28 DRSRANR  Krogh Added external statement.
c>> 1994-10-19 DRSRANR  Krogh  Changes to use M77CON
c>> 1987-12-09 DRSRANR  Lawson  Initial Code.
c--S replaces "?": DR?RANR, ?RANR, ?STAT1, ?STAT2
c
c     Driver to demonstrate use of  SRANR to generate random numbers
c     from the Rayleigh distribution with parameter, ALPHA.
c     Program computes histogram for N numbers
c
      integer NCELLS
      parameter(NCELLS = 13+2)
      external SRANR
      real              ALPHA, SRANR, ONE, PIOV2
      real              STATS(5), TWO, Y1, Y2, YTAB(1), ZERO
      integer  I, IHIST(NCELLS), N
c
      parameter(ONE = 1.0E0, TWO = 2.0E0, ZERO = 0.0E0)
      data       N / 10000/
      data  Y1, Y2 /  0.0E0,  4.33333E0/
      data   ALPHA / 1.0E0 /
c     ------------------------------------------------------------------
      PIOV2 = TWO * atan(ONE)
      STATS(1) = ZERO
      do 20 I=1,N
c                             Get random number
         YTAB(1) =  SRANR(ALPHA)
c
c                          Accumulate statistics and histogram.
c
        call SSTAT1(YTAB(1), 1, STATS, IHIST, NCELLS, Y1, Y2)
   20 continue
c                          Print the statistics and histogram.
c
      print '(13x,a)', 'Rayleigh random numbers from SRANR'
      print '(13x,a,g12.4)','with ALPHA = ',ALPHA
      print '(1x,a/13x,g13.5,a,g13.5/1x)',
     * 'The Mean and Std. Dev. of the theoretical distribution are',
     * ALPHA * sqrt(PIOV2), ' and ',ALPHA * sqrt(TWO - PIOV2)
      call SSTAT2(STATS, IHIST, NCELLS, Y1, Y2)
      stop
      end
