c     program DRDHINT
c>> 1996-05-28 DRDHINT  Krogh Added external statement.
c>> 1994-10-19 DRDHINT  Krogh  Changes to use M77CON
c>> 1987-12-09 DRDHINT  Lawson  Initial Code.
c--D replaces "?": DR?HINT, ?HINT
c
      integer I, J, NTAB
      double precision X, XTAB(3), YTAB(3), YPTAB(3), YINT, YPINT, YTRUE
      external DHINT
      double precision DHINT
c
      data XTAB / 0.D0, .5D0, .75D0 /
      data NTAB / 3 /
c
   10 format(1X,'Demonstration of DHINT by interpolating to the',
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
        X = DBLE(J)/100.D0 - 0.1D0
        YINT = DHINT(X,0,NTAB,XTAB,YTAB,YPTAB)
        YPINT = DHINT(X,1,NTAB,XTAB,YTAB,YPTAB)
        YTRUE = EXP(X)
        print 20, X, YTRUE, YINT, YINT-YTRUE, YPINT, YPINT-YTRUE
   50 continue
c
      end
