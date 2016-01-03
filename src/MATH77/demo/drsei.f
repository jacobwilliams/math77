      program DRSEI
c>> 1996-05-28 DRSEI    Krogh Added external statement.
c>> 1994-10-19 DRSEI    Krogh  Changes to use M77CON
c>> 1992-03-16 DRSEI    CLL
c>> 1990-11-29 CLL
c>> 1987-12-09 DRSEI    Lawson  Initial Code.
c     ------------------------------------------------------------------
c--S replaces "?": DR?EI, ?EI, ?E1
c     ------------------------------------------------------------------
      integer J
      external SEI, SE1
      real             X(14), Y, Z, SEI, SE1
c     ------------------------------------------------------------------
      data X / -80.E0, -20.E0, -5.E0, -1.E0, -.4E0, -.3E0, -.001E0,
     *         .001E0, .3E0, .4E0, 1.E0, 5.E0, 20.E0, 80.E0 /
c
      print '(1x,3X,A1,13X,A6,14X,A6/)','X','SEI(X)','SE1(X)'
c
      do 10 J = 1, 14
        Y = SEI(X(J))
        Z = SE1(X(J))
        print '(1x,F7.3,5X,2(G15.8,5X))',X(J),Y,Z
   10 continue
      end
