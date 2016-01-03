c     DRDLASUM
c>> 1994-10-19 DRDLASUM Krogh  Changes to use M77CON
c>> 1994-07-14 DRDLASUM CLL
c>> 1992-05-07 CLL
c>> 1992-04-28 DRDLASUM Replaced '1' in format.
c>> 1987-12-09 DRDLASUM Lawson  Initial Code.
c--D replaces "?": DR?LASUM, ?LASUM
c     Demonstration program for evaluation of a Laguerre series.
c     ------------------------------------------------------------------
      integer j
      double precision x,a(0:6),y,w,dif,reldif,pn(6)
      data a / 7.2d0, -43.2d0, 108.0d0, -144.0d0,
     *         108.0d0, -43.2d0, 7.2d0 /
      data pn / 0.1d0, 0.3d0, 1.0d0, 3.0d0, 10.0d0, 30.0d0 /
c     ------------------------------------------------------------------
      print '(1x,3x,a1,12x,a1,14x,a3,9x,a6/)','x','y','dif','reldif'
      do 10 j = 1,6
        x = pn(j)
        call dlasum (x, 6, a, y)
        w = 0.01d0 * (x**6)
        dif = y - w
        reldif = dif / w
        print '(1x,f6.2,3x,g15.8,2(3x,g10.3))',x,y,dif,reldif
   10 continue
      end
