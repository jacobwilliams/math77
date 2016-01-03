c     program DRXBLAS
c>> 1996-05-28 DRXBLAS Krogh Added external statement.
c>> 1992-03-13 CLL
c   DRXBLAS..  Demo driver for DSDOT and SDSDOT
c     ------------------------------------------------------------------
      external DSDOT, SDSDOT
      double precision DSDOT
      real    SDSDOT

      double precision D1
      real    S1, X(4), Y(4)
      integer N
      data X  /  4.0, -7.0, 3.0, -9.0 /
      data Y  /  -2.0, -8.0, 5.0, 6.0 /
c     ------------------------------------------------------------------
      print'(a)',
     * ' DRXBLAS..  Demo driver for DSDOT and SDSDOT',
     * ' These subprograms accept SP data but use DP arithmetic.'
      N = 4
      S1 = SDSDOT(N, -9.0e0, X, 1, Y, 1)
      D1 = -9.0d0 + DSDOT(N, X, 1, Y, 1)
      print'(/a)',' Using SDSDOT and DSDOT.  Expect S1 = D1 = 0.0'
      print'(a,f8.2,a,f8.2)',' Computed: S1 =',S1,', D1 =',D1
      end
