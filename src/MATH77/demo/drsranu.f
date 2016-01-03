c     program DRSRANU
c>> 1996-05-28 DRSRANU Krogh Added external statement.
c>> 1994-10-19 DRSRANU  Krogh  Changes to use M77CON
c>> 1992-03-13 DRSRANU  CLL
c>> 1987-12-09 DRSRANU  Lawson  Initial Code.
c
c     Driver to demonstrate use of SRANU to generate random numbers
c     from the uniform distribution on [0.0, 1.0].
c     Program computes histogram for N numbers
c     ------------------------------------------------------------------
c--S replaces "?": DR?RANU, ?RANU, ?STAT1, ?STAT2
c     ------------------------------------------------------------------
      integer N, NCELLS
      parameter(N = 10000, NCELLS = 10+2)
      external SRANU
      real             SRANU, STATS(5), YTAB(1), Y1, Y2
      integer I, IHIST(NCELLS)
      data Y1, Y2 / 0.0E0, 1.0E0 /
c     ------------------------------------------------------------------
      STATS(1) = 0.0E0
      do 20 I = 1, N
c                          Get random number
         YTAB(1) = SRANU( )
c                          Accumulate statistics and histogram.
c
        call SSTAT1(YTAB(1), 1, STATS, IHIST, NCELLS, Y1, Y2)
  20  continue
c                          Print the statistics and histogram.
c
      write(*,'(13x,a//)') 'Uniform random numbers from SRANU'
      call SSTAT2(STATS, IHIST, NCELLS, Y1, Y2)
      stop
      end
