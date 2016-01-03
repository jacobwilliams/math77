c      program DRDSBASD
c>> 1996-07-09 DRDSBASD Krogh  Format changes for conversion to C.
c>> 1994-10-19 DRDSBASD Krogh  Changes to use M77CON
c>> 1993-01-12 DRDSBASD CLL @ JPL
c>> 1992-11-12 CLL @ JPL
c>> 1992-11-04 CLL @ JPL
c   Demo driver for DSBASD, DSBASI, DSDIF, DSFIND, DSVALA.
c     ------------------------------------------------------------------
c--D replaces "?": DR?SBASD, ?SBASD, ?SBASI, ?SDIF, ?SFIND, ?SVALA
c     ------------------------------------------------------------------
      integer I, IDERIV, J1, J2, KORDER, LEFT, MODE, NCOEF, NDERIV
      parameter(KORDER = 4, NCOEF = 6)
      parameter( IDERIV = 2, NDERIV = 3)
      double precision BASI(NCOEF), BCOEF(NCOEF), BDERIV(KORDER)
      double precision BDIF(NCOEF*(NDERIV+1)), BVALS(KORDER)
      double precision SVALUE(NDERIV+1)
      double precision TKNOTS(KORDER+NCOEF), X, X1, X2
      parameter( X = 0.4d0 , X1 = 0.1d0, X2 = 0.9d0)
      data TKNOTS / 4*0.0d0, 0.3d0, 0.8d0, 4*1.0d0 /
      data BCOEF / 0.1d0, 0.4d0, 0.2d0, -0.3d0, -0.5d0, -0.2d0 /
c     ------------------------------------------------------------------
      print '('' DRDSBASD..  Demo driver for''/
     *  ''             DSBASD, DSBASI, DSDIF, DSFIND, DSVALA.''/
     *  3x,'' KORDER ='',i2,'',  NCOEF ='',i2/ 3x,'' TKNOTS() ='',
     *  10f5.1)', KORDER, NCOEF, (TKNOTS(I),I=1,KORDER+NCOEF)
      print '('' Using DSFIND with X = '',f5.1)', X
      LEFT = 1
      call DSFIND(TKNOTS, KORDER, NCOEF+1, X, LEFT, MODE)
      print '(3x,'' LEFT = '',i2,'',  MODE ='',i2)', LEFT, MODE
      print '('' Using DSBASD with IDERIV = 0'')'
      call DSBASD( KORDER, LEFT, TKNOTS, X, 0, BVALS)
      print'(3x,'' Values at X of basis functions indexed from '',i2,
     *  '' to '',i2/6x,4f12.6)',  LEFT+1-KORDER, LEFT,
     *  (BVALS(I),I=1,KORDER)
      print'('' Using DSBASD with IDERIV ='',i2)', IDERIV
      call DSBASD(KORDER, LEFT, TKNOTS, X, IDERIV, BDERIV)
      print'(3x,
     * '' Values at X of 2nd deriv of basis functions indexed from '',
     * i2,'' to '',i2/6x,4f12.5)', LEFT+1-KORDER, LEFT,
     * (BDERIV(I),I=1,KORDER)
      print'('' Using DSBASI with X1 = '',f5.1,'' and X2 = '',f5.1)',
     *    X1,X2
      J1 = 1
      J2 = 1
      call DSBASI(KORDER, NCOEF, TKNOTS, X1, X2, J1, J2, BASI)
      print'(3x,'' J1 ='',i2,'',  J2 ='',i2/3x,
     * '' Integrals from X1 to X2 of basis functions:''/3x,6f11.6)',
     * J1, J2, (BASI(I),I=1,NCOEF)
      print'('' Using DSDIF and DSVALA with NDERIV ='',i2,'' and''/3x,
     * '' BCOEF() = '', 6f5.1)', NDERIV, (BCOEF(I),I=1,NCOEF)
      call DSDIF(KORDER, NCOEF, TKNOTS, BCOEF, NDERIV, BDIF)
      call DSVALA(KORDER, NCOEF, TKNOTS, NDERIV, BDIF, X, SVALUE)
      print'(3x,'' Values of derivs 0 through '',i2,'' at X: ''/
     *   6x,4f11.6)',  NDERIV, (SVALUE(I),I=1,NDERIV+1)
      end
