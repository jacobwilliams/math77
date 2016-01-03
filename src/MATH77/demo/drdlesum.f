c     DRDLESUM
c>> 1995-05-28 DRDLESUM Krogh  Changes to use M77CON
c>> 1994-08-09 DRDLESUM WVS  Set up for CHGTYP
c>> 1994-07-14 DRDLESUM CLL
c>> 1992-04-29 DRDLESUM CAO Replaced '1' in format.
c>> 1991-11-19 DRDLESUM CLL
c>> 1987-12-09 DRDLESUM Lawson  Initial Code.
c--D replaces "?": ?LESUM, DR?LESUM
c
c     Demonstration driver for evaluation of a Legendre series.
c     ------------------------------------------------------------------
      integer j
      double precision x,a(0:5),y,w,z
      data a/0.07d0, 0.27d0, 0.20d0, 0.28d0, 0.08d0, 0.08d0/
c     ------------------------------------------------------------------
      print '(1x,3x,a1,14x,a1,17x,a1/)','x','y','z'
      do 20 j = -10,10,2
        x = dble(j) /10.d0
        call dlesum (x,5,a,y)
        w = 0.35d0 * (x**4) + 0.63d0 * (x**5)
        z = y - w
        print '(1x,f5.2,5x,g15.7,g15.2)',x,y,z
   20 continue
      end
