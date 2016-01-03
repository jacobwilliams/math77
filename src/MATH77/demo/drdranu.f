c     program DRDRANU
c>> 1996-05-28 DRDRANU Krogh Added external statement.
c>> 1994-10-19 DRDRANU  Krogh  Changes to use M77CON
c>> 1992-03-13 DRDRANU  CLL
c>> 1987-12-09 DRDRANU  Lawson  Initial Code.
c
c     Driver to demonstrate use of DRANU to generate random numbers
c     from the uniform distribution on [0.0, 1.0].
c     Program computes histogram for N numbers
c     ------------------------------------------------------------------
c--D replaces "?": DR?RANU, ?RANU, ?STAT1, ?STAT2
c     ------------------------------------------------------------------
      integer N, NCELLS
      parameter(N = 10000, NCELLS = 10+2)
      external DRANU
      double precision DRANU, STATS(5), YTAB(1), Y1, Y2
      integer I, IHIST(NCELLS)
      data Y1, Y2 / 0.0D0, 1.0D0 /
c     ------------------------------------------------------------------
      STATS(1) = 0.0D0
      do 20 I = 1, N
c                          Get random number
         YTAB(1) = DRANU( )
c                          Accumulate statistics and histogram.
c
        call DSTAT1(YTAB(1), 1, STATS, IHIST, NCELLS, Y1, Y2)
  20  continue
c                          Print the statistics and histogram.
c
      write(*,'(13x,a//)') 'Uniform random numbers from DRANU'
      call DSTAT2(STATS, IHIST, NCELLS, Y1, Y2)
      stop
      end
