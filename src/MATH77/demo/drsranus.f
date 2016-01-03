c     program DRSRANUS
c>> 1995-10-23 DRSRANUS Krogh  Reduced N to work on PC in D.P.
c>> 1994-10-19 DRSRANUS Krogh  Changes to use M77CON
c>> 1992-03-13 DRSRANUS CLL
c
c     Driver to demonstrate use of SRANUS to generate random numbers
c     from the uniform distribution between A and A+B.
c     Program computes histogram for N numbers
c     ------------------------------------------------------------------
c--S replaces "?": DR?RANUS, ?RANUS, ?STAT1, ?STAT2
c     ------------------------------------------------------------------
      integer N, NCELLS
      parameter(N = 8000, NCELLS = 10+2)
      real             A, B, STATS(5), YTAB(N)
      integer IHIST(NCELLS)
      data A, B / 20.0e0, 10.0e0/
c     ------------------------------------------------------------------
      STATS(1) = 0.0e0
c                          Get array of scaled random numbers.
c
      call SRANUS(YTAB, N, A, B)
c
c                          Compute statistics and histogram.
c
        call SSTAT1(YTAB, N, STATS, IHIST, NCELLS, A, A+B)
c
c                          Print the statistics and histogram.
c
      write(*,'(13x,a//)') 'Uniform random numbers from SRANUS'
      call SSTAT2(STATS, IHIST, NCELLS, A, A+B)
      end
