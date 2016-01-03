c     DRDBESJN
c>> 1999-01-07 DRDBESJN Krogh Added external statement.
c>> 1996-05-31 DRDBESJN Krogh  Changes to use M77CON
c>> 1994-09-01 DRDBESJN WVS Moved formats to top for C conversion
c>> 1992-04-29 DRDBESJN CAO Replaced '1' in format.
c>> 1987-12-09 DRDBESJN Lawson  Initial Code.
c--D replaces "?": DR?BESJN, ?BESJN, ?BESYN
c     DEMONSTRATION PROGRAM FOR BESSEL function.
c
      double precision X(3),ALPHA(3),BJ(2),BY(2),Z,PI2
      external DBESJN, DBESYN
      integer I, N
c
      data X / 0.5D0, 1.5D0, 3.2D0 /
      data ALPHA / 1.5D0, 3.0D0, 7.8D0 /
      data PI2 / 1.5707963267948966192313216D0 /
c
  100 format(' ',4X,A1,9X,A2,11X,A7,11X,A7,12X,A1)
  200 format(' ',26X,A9,9X,A9/' ')
  300 format(' ',F6.2,5X,F6.2,4X,G15.8,5X,G15.8,G13.2)
  400 format(' ',21X,2(G15.8,5X)/' ')
c
      print 100,'X','NU','J(NU,X)','Y(NU,X)','Z'
      print 200,'J(NU+1,X)','Y(NU+1,X)'
c
      do 500 I = 1,3
        N = 2
        call DBESJN(X(I),ALPHA(I),N,BJ)
        call DBESYN(X(I),ALPHA(I),N,BY)
        Z = PI2 * X(I) * (BJ(2)*BY(1) - BJ(1)*BY(2)) - 1.D0
        print 300,X(I),ALPHA(I),BJ(1),BY(1),Z
        print 400,BJ(2),BY(2)
  500 continue
c
      end
