c     program DRSRANG
c>> 2001-05-22 DRSRANG  Krogh Minor change for making .f90 version.
c>> 1996-05-28 DRSRANG  Krogh Added external statement.
c>> 1994-10-19 DRSRANG  Krogh  Changes to use M77CON
c>> 1987-12-09 DRSRANG  Lawson  Initial Code.
c--S replaces "?": DR?RANG, ?RANG, ?STAT1, ?STAT2
c     Demonstration driver for SRANG.  Generates random numbers
c     from the Gaussian distribution.
c     ------------------------------------------------------------------
      integer NCELLS
      parameter(NCELLS = 12+2)
      external SRANG
      real                 SRANG, STATS(5), Y1, Y2, YTAB(1), ZERO
      integer  I, IHIST(NCELLS), N
c
      parameter(ZERO = 0.0E0)
      data       N / 10000/
      data  Y1, Y2 / -3.0E0, +3.0E0/
c     ------------------------------------------------------------------
      STATS(1) = ZERO
      do 20 I=1,N
c                          GET RANDOM NUMBER
         YTAB(1) = SRANG()
c
c                          Accumulate statistics and histogram.
c
        call SSTAT1(YTAB(1), 1, STATS, IHIST, NCELLS, Y1, Y2)
   20 continue
c                          Print the statistics and histogram.
c
      write(*,'(13x,a//)') 'Gaussian random numbers from SRANG'
      call SSTAT2(STATS, IHIST, NCELLS, Y1, Y2)
      stop
      end
