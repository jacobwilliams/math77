c     DRDERFCE
c>> 1996-05-28 DRDERFCE Krogh Moved formats up.
c>> 1994-10-19 DRDERFCE Krogh  Changes to use M77CON
c>> 1992-04-29 DRDERFCE CAO Replaced '1' in format.
c>> 1992-01-29 DRDERFCE cao/cll eliminate underflow error message.
c>> 1992-01-29 DRDERFCE cao/cll better output labels.
c>> 1991-11-20 DRDERFCE CLL Edited for Fortran 90
c>> 1987-12-09 DRDERFCE Lawson  Initial Code.
c     DEMONSTRATION DRIVER FOR  DERFCE
c     ------------------------------------------------------------------
c--D replaces "?": DR?ERFCE, ?ERFC, ?ERFCE
c     ------------------------------------------------------------------
      external DERFC,DERFCE
      double precision DERFC, DERFCE
      double precision W, X, Y, Z
      integer J
c
  100 format(' ',3X,'X',8X,'DERFCE',7X,'DERFC*(X*X)',6X,'DIFF'/' ')
  200 format(1X,F5.1,3X,2(G15.8),1X,E9.2)
c     ------------------------------------------------------------------
c
      X = 0.0d0
      write(*,100)
      do 10 J=1,18
        W = DERFC(X) * EXP(X * X)
        Y = DERFCE(X)
        Z = Y - W
        write(*,200) X,Y,W,Z
        X = X + 0.5d0
   10 continue
      do 20 J=1,18
        Y = DERFCE(X)
        write(*,200) X,Y
        X = X + 1.0d0
   20 continue
      stop
      end
