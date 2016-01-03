c     DRSLESUM
c>> 1995-05-28 DRSLESUM Krogh  Changes to use M77CON
c>> 1994-08-09 DRSLESUM WVS  Set up for CHGTYP
c>> 1994-07-14 DRSLESUM CLL
c>> 1992-04-29 DRSLESUM CAO Replaced '1' in format.
c>> 1991-11-19 DRSLESUM CLL
c>> 1987-12-09 DRSLESUM Lawson  Initial Code.
c--S replaces "?": ?LESUM, DR?LESUM
c
c     Demonstration driver for evaluation of a Legendre series.
c     ------------------------------------------------------------------
      integer j
      real             x,a(0:5),y,w,z
      data a/0.07e0, 0.27e0, 0.20e0, 0.28e0, 0.08e0, 0.08e0/
c     ------------------------------------------------------------------
      print '(1x,3x,a1,14x,a1,17x,a1/)','x','y','z'
      do 20 j = -10,10,2
        x = real(j) /10.e0
        call slesum (x,5,a,y)
        w = 0.35e0 * (x**4) + 0.63e0 * (x**5)
        z = y - w
        print '(1x,f5.2,5x,g15.7,g15.2)',x,y,z
   20 continue
      end
