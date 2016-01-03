c     DRSGAMMA
c>> 1996-07-12 DRSGAMMA Krogh  Minor changes for conversion to C.
c>> 1994-10-19 DRSGAMMA Krogh  Changes to use M77CON
c>> 1992-06-11 DRSGAMMA Krogh   Changed "duplication" identity.
c>> 1991-10-21 DRSGAMMA CLL
c>> 1987-12-09 DRSGAMMA Lawson  Initial Code.
c
c--S replaces "?": DR?GAMMA, ?GAMMA, ?LGAMA
      external SGAMMA, SLGAMA
      real             SGAMMA, SLGAMA
      real             LGAM, GAM, PI, X(12), Z
      integer I
c
      data X /-1.5e0, -0.5e0, 0.5e0,  1.0e0,  1.5e0,  2.0e0,
     *          3.0e0, 4.0e0, 5.0e0, 10.0e0, 20.0e0, 30.0e0 /
      data PI / 3.1415926535897932385e0 /
c
      print '(''   X'',9X,''SGAMMA'',10X,''SLGAMA'',10X,''Z''/1x)'
c
      do 10 I = 1, 12
        GAM = SGAMMA(X(I))
        Z = (2.e0 * SQRT(PI) * GAM) / ((2.0e0**X(I)) *
     1    SGAMMA(0.5e0*X(I)) * SGAMMA(0.5e0*X(I)+.5e0)) - 1.e0
        if(X(I) .lt. 0.0e0) then
          print '(1x,F4.1,3X,G15.8, 13x ,1X,E13.2)', X(I), GAM,  Z
        else
          LGAM = SLGAMA(X(I))
          print '(1x,F4.1,3X,G15.8,F13.5,1X,E13.2)', X(I), GAM, LGAM, Z
        endif
   10 continue
      end
