c     DRSERFCE
c>> 1996-05-28 DRSERFCE Krogh Moved formats up.
c>> 1994-10-19 DRSERFCE Krogh  Changes to use M77CON
c>> 1992-04-29 DRSERFCE CAO Replaced '1' in format.
c>> 1992-01-29 DRSERFCE cao/cll eliminate underflow error message.
c>> 1992-01-29 DRSERFCE cao/cll better output labels.
c>> 1991-11-20 DRSERFCE CLL Edited for Fortran 90
c>> 1987-12-09 DRSERFCE Lawson  Initial Code.
c     DEMONSTRATION DRIVER FOR  SERFCE
c     ------------------------------------------------------------------
c--S replaces "?": DR?ERFCE, ?ERFC, ?ERFCE
c     ------------------------------------------------------------------
      external SERFC,SERFCE
      real             SERFC, SERFCE
      real             W, X, Y, Z
      integer J
c
  100 format(' ',3X,'X',8X,'SERFCE',7X,'SERFC*(X*X)',6X,'DIFF'/' ')
  200 format(1X,F5.1,3X,2(G15.8),1X,E9.2)
c     ------------------------------------------------------------------
c
      X = 0.0e0
      write(*,100)
      do 10 J=1,18
        W = SERFC(X) * EXP(X * X)
        Y = SERFCE(X)
        Z = Y - W
        write(*,200) X,Y,W,Z
        X = X + 0.5e0
   10 continue
      do 20 J=1,18
        Y = SERFCE(X)
        write(*,200) X,Y
        X = X + 1.0e0
   20 continue
      stop
      end
