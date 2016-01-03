      program DRDEI
c>> 1996-05-28 DRDEI    Krogh Added external statement.
c>> 1994-10-19 DRDEI    Krogh  Changes to use M77CON
c>> 1992-03-16 DRDEI    CLL
c>> 1990-11-29 CLL
c>> 1987-12-09 DRDEI    Lawson  Initial Code.
c     ------------------------------------------------------------------
c--D replaces "?": DR?EI, ?EI, ?E1
c     ------------------------------------------------------------------
      integer J
      external DEI, DE1
      double precision X(14), Y, Z, DEI, DE1
c     ------------------------------------------------------------------
      data X / -80.D0, -20.D0, -5.D0, -1.D0, -.4D0, -.3D0, -.001D0,
     *         .001D0, .3D0, .4D0, 1.D0, 5.D0, 20.D0, 80.D0 /
c
      print '(1x,3X,A1,13X,A6,14X,A6/)','X','DEI(X)','DE1(X)'
c
      do 10 J = 1, 14
        Y = DEI(X(J))
        Z = DE1(X(J))
        print '(1x,F7.3,5X,2(G15.8,5X))',X(J),Y,Z
   10 continue
      end
