c     DRSLASUM
c>> 1994-10-19 DRSLASUM Krogh  Changes to use M77CON
c>> 1994-07-14 DRSLASUM CLL
c>> 1992-05-07 CLL
c>> 1992-04-28 DRSLASUM Replaced '1' in format.
c>> 1987-12-09 DRSLASUM Lawson  Initial Code.
c--S replaces "?": DR?LASUM, ?LASUM
c     Demonstration program for evaluation of a Laguerre series.
c     ------------------------------------------------------------------
      integer j
      real             x,a(0:6),y,w,dif,reldif,pn(6)
      data a / 7.2e0, -43.2e0, 108.0e0, -144.0e0,
     *         108.0e0, -43.2e0, 7.2e0 /
      data pn / 0.1e0, 0.3e0, 1.0e0, 3.0e0, 10.0e0, 30.0e0 /
c     ------------------------------------------------------------------
      print '(1x,3x,a1,12x,a1,14x,a3,9x,a6/)','x','y','dif','reldif'
      do 10 j = 1,6
        x = pn(j)
        call slasum (x, 6, a, y)
        w = 0.01e0 * (x**6)
        dif = y - w
        reldif = dif / w
        print '(1x,f6.2,3x,g15.8,2(3x,g10.3))',x,y,dif,reldif
   10 continue
      end
