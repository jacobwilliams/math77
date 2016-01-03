c     program DRSHINT
c>> 1996-05-28 DRSHINT  Krogh Added external statement.
c>> 1994-10-19 DRSHINT  Krogh  Changes to use M77CON
c>> 1987-12-09 DRSHINT  Lawson  Initial Code.
c--S replaces "?": DR?HINT, ?HINT
c
      integer I, J, NTAB
      real             X, XTAB(3), YTAB(3), YPTAB(3), YINT, YPINT, YTRUE
      external SHINT
      real             SHINT
c
      data XTAB / 0.E0, .5E0, .75E0 /
      data NTAB / 3 /
c
   10 format(1X,'Demonstration of SHINT by interpolating to the',
     *          ' exponential function,'/
     *          1X,'given values at X = 0.0, 0.5, and 0.75'//
     *          4X,'X',7X,'YTRUE',9X,
     *          'YINT',6X,'YINT-YTRUE',5X,'YPINT',6X,'YPINT-YTRUE'/1X)
   20 format(1X,F5.2,5F13.6)
c
      do 30 I = 1,3
        YTAB(I) = EXP(XTAB(I))
        YPTAB(I) = YTAB(I)
   30 continue
c
      print 10
c
      do 50 J = 10, 110, 5
        X = REAL(J)/100.E0 - 0.1E0
        YINT = SHINT(X,0,NTAB,XTAB,YTAB,YPTAB)
        YPINT = SHINT(X,1,NTAB,XTAB,YTAB,YPTAB)
        YTRUE = EXP(X)
        print 20, X, YTRUE, YINT, YINT-YTRUE, YPINT, YPINT-YTRUE
   50 continue
c
      end
