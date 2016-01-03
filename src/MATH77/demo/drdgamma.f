c     DRDGAMMA
c>> 1996-07-12 DRDGAMMA Krogh  Minor changes for conversion to C.
c>> 1994-10-19 DRDGAMMA Krogh  Changes to use M77CON
c>> 1992-06-11 DRDGAMMA Krogh   Changed "duplication" identity.
c>> 1991-10-21 DRDGAMMA CLL
c>> 1987-12-09 DRDGAMMA Lawson  Initial Code.
c
c--D replaces "?": DR?GAMMA, ?GAMMA, ?LGAMA
      external DGAMMA, DLGAMA
      double precision DGAMMA, DLGAMA
      double precision LGAM, GAM, PI, X(12), Z
      integer I
c
      data X /-1.5d0, -0.5d0, 0.5d0,  1.0d0,  1.5d0,  2.0d0,
     *          3.0d0, 4.0d0, 5.0d0, 10.0d0, 20.0d0, 30.0d0 /
      data PI / 3.1415926535897932385d0 /
c
      print '(''   X'',9X,''DGAMMA'',10X,''DLGAMA'',10X,''Z''/1x)'
c
      do 10 I = 1, 12
        GAM = DGAMMA(X(I))
        Z = (2.d0 * SQRT(PI) * GAM) / ((2.0d0**X(I)) *
     1    DGAMMA(0.5d0*X(I)) * DGAMMA(0.5d0*X(I)+.5d0)) - 1.d0
        if(X(I) .lt. 0.0d0) then
          print '(1x,F4.1,3X,G15.8, 13x ,1X,E13.2)', X(I), GAM,  Z
        else
          LGAM = DLGAMA(X(I))
          print '(1x,F4.1,3X,G15.8,F13.5,1X,E13.2)', X(I), GAM, LGAM, Z
        endif
   10 continue
      end
