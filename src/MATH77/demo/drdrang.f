c     program DRDRANG
c>> 2001-05-22 DRDRANG  Krogh Minor change for making .f90 version.
c>> 1996-05-28 DRDRANG  Krogh Added external statement.
c>> 1994-10-19 DRDRANG  Krogh  Changes to use M77CON
c>> 1987-12-09 DRDRANG  Lawson  Initial Code.
c--D replaces "?": DR?RANG, ?RANG, ?STAT1, ?STAT2
c     Demonstration driver for DRANG.  Generates random numbers
c     from the Gaussian distribution.
c     ------------------------------------------------------------------
      integer NCELLS
      parameter(NCELLS = 12+2)
      external DRANG
      double precision     DRANG, STATS(5), Y1, Y2, YTAB(1), ZERO
      integer  I, IHIST(NCELLS), N
c
      parameter(ZERO = 0.0D0)
      data       N / 10000/
      data  Y1, Y2 / -3.0D0, +3.0D0/
c     ------------------------------------------------------------------
      STATS(1) = ZERO
      do 20 I=1,N
c                          GET RANDOM NUMBER
         YTAB(1) = DRANG()
c
c                          Accumulate statistics and histogram.
c
        call DSTAT1(YTAB(1), 1, STATS, IHIST, NCELLS, Y1, Y2)
   20 continue
c                          Print the statistics and histogram.
c
      write(*,'(13x,a//)') 'Gaussian random numbers from DRANG'
      call DSTAT2(STATS, IHIST, NCELLS, Y1, Y2)
      stop
      end
