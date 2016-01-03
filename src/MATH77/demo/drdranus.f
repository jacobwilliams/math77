c     program DRDRANUS
c>> 1995-10-23 DRDRANUS Krogh  Reduced N to work on PC in D.P.
c>> 1994-10-19 DRDRANUS Krogh  Changes to use M77CON
c>> 1992-03-13 DRDRANUS CLL
c
c     Driver to demonstrate use of DRANUS to generate random numbers
c     from the uniform distribution between A and A+B.
c     Program computes histogram for N numbers
c     ------------------------------------------------------------------
c--D replaces "?": DR?RANUS, ?RANUS, ?STAT1, ?STAT2
c     ------------------------------------------------------------------
      integer N, NCELLS
      parameter(N = 8000, NCELLS = 10+2)
      double precision A, B, STATS(5), YTAB(N)
      integer IHIST(NCELLS)
      data A, B / 20.0d0, 10.0d0/
c     ------------------------------------------------------------------
      STATS(1) = 0.0d0
c                          Get array of scaled random numbers.
c
      call DRANUS(YTAB, N, A, B)
c
c                          Compute statistics and histogram.
c
        call DSTAT1(YTAB, N, STATS, IHIST, NCELLS, A, A+B)
c
c                          Print the statistics and histogram.
c
      write(*,'(13x,a//)') 'Uniform random numbers from DRANUS'
      call DSTAT2(STATS, IHIST, NCELLS, A, A+B)
      end
