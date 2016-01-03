c     program DRDSFITC
c>> 2001-07-16 DRDSFITC Krogh  Added exponent 0 to some constants.
c>> 1996-07-11 DRDSFITC Krogh  Special code for C conversion.
c>> 1996-05-28 DRDSFITC Krogh  Changed Fortran 90 code & changes for C.
c>> 1994-10-19 DRDSFITC Krogh  Changes to use M77CON
c>> 1993-01-13 DRDSFITC C. L. Lawson, JPL
c>> 1992-11-10 C. L. Lawson, JPL
c>> 1992-11-04 C. L. Lawson, JPL
c>> 1989-03-02 C. L. Lawson, JPL
c>> 1988-04-01 C. L. Lawson, JPL
c     DRDSFITC..  Demo driver for DSFITC, Spline fit with constraints.
c     The problem has 24 data points and 10 constraints.
c     The spline is order 4 with 9 coefficients.
c     ------------------------------------------------------------------
c--D replaces "?": DR?SFITC, ?SFITC, ?SVAL, ?SVALA, ?PRPL, ?SDIF
c     ------------------------------------------------------------------
c++ Code for .C. is inactive
c%%   long int k;
c%%#define MT     (NCOEF+KORDER)
c++ End
      integer I, KORDER, KPRINT, MXY, MT
      integer NCOEF, NDATA, NINFO, NWORK
      parameter( NDATA = 24, MXY = NDATA+10)
      parameter( NINFO = 41, NWORK = 843)
      parameter( NCOEF=9, KORDER = 4, MT = NCOEF+KORDER)
      parameter( KPRINT = 0)
      integer INFO(NINFO), ISET(3)
      external DSVAL
      double precision DSVAL
      double precision BCOEF(NCOEF), BDIF(NCOEF*3), DELX, RNORM
      double precision SDI(MXY), SMAX, SMIN, SVALUE(0:2)
      double precision TKNOTS(NCOEF+KORDER)
      double precision WORK(NWORK), X, XI(MXY), YI(MXY), YFIT
      character CCODE(MXY+1)*4, IMAGE*49
      data TKNOTS / 4*0.0D0, 1.5D0, 2.5D0, 3.3D0, 4.0D0, 4.7D0, 4*6.0D0/
      data CCODE /  24*'10~a',
     *             '10=a', '11>a', '12>a', '12>a', '12>a',
     *             '12<a', '12<a', '12<a', '11>a', '10=a',
     *             '   !'/
      data XI / 0.0D0, 0.3D0, 0.7D0, 1.0D0, 1.3D0, 1.7D0, 2.0D0, 2.3D0,
     *          2.5D0, 2.6D0, 2.8D0, 2.9D0, 3.0D0, 3.1D0, 3.2D0, 3.5D0,
     *          3.7D0, 4.0D0, 4.3D0, 4.7D0, 5.0D0, 5.3D0, 5.7D0, 6.0D0,
     *          0.0D0, 0.0D0, 0.0D0, 1.5D0, 2.5D0,
     *          3.5D0, 4.5D0, 6.0D0, 6.0D0, 6.0D0/

      data YI /  1.0D0, 1.1D0,0.9D0,1.02D0, 1.2D0,1.0D0,1.2D0,1.4D0,
     *          1.76D0, 2.0D0,2.4D0, 2.6D0, 3.0D0,3.4D0,3.7D0,4.3D0,
     *          4.45D0,4.76D0,4.8D0, 5.0D0,4.96D0,4.9D0,4.9D0,5.0D0,
     *          1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     *          0.0D0,0.0D0,0.0D0,0.0D0,5.0D0/

      data SDI(1) / -1.0D0 /
      data ISET  / NINFO, NWORK, KPRINT /
c     ------------------------------------------------------------------
      print'('' DRDSFITC..  Demo driver for DSFITC''/'' Least-squares'',
     *  '' polynomial spline fit to data with constraints.'')'
c
      print'(/
     *  ''   I   kind   deriv   relop  active       X         Y'')'
      do 10 I = 1,MXY
         print'(1x,i3,3x,a1,7x,a1,7x,a1,7x,a1,f12.3,f10.3)', I,
     *      CCODE(I)(1:1),CCODE(I)(2:2),CCODE(I)(3:3),CCODE(I)(4:4),
     *      XI(I),YI(I)
   10 continue
      I = MXY+1
      print'(1x,i3,3x,a1,7x,a1,7x,a1,7x,a1)', I,
     *      CCODE(I)(1:1),CCODE(I)(2:2),CCODE(I)(3:3),CCODE(I)(4:4)
      print'(/''   KORDER ='',i3,'',  NCOEF ='',i3)', KORDER, NCOEF
c++ Code for ~.C. is active
      print'(''   TKNOTS() = '',4f10.5/(14x,4f10.5))',
     *   (TKNOTS(I), I = 1, MT)
c++ Code for .C. is inactive
c%%     printf( "\n   TKNOTS() = " );
c%%     for (i = 1; i <= MT+3; i+=4){
c%%        for (k = i; k <= min( i+3, MT ); k++)
c%%           printf( "%10.5f", Tknots[k] );
c%%        if (i + 3 < MT) printf( "\n              ");}
c%%     printf( "\n" );
c++ End
c
      call DSFITC(CCODE, XI, YI, SDI, KORDER, NCOEF, TKNOTS,
     *               BCOEF, RNORM, ISET, INFO, WORK)
c
      print'(/'' After call to DSFITC:'')'
      print'(/3x,''IERR5 ='',i6,'',   NEED1 ='',i7,'',   NEED2 ='',i7/
     * 3x,''M1    ='',i6,'',   MFIT  ='',i7,'',   NS    ='',i7/
     * 3x,''RNORM ='',f12.5)', INFO(1),INFO(2),INFO(3),INFO(4),INFO(5),
     * INFO(6), RNORM
c++ Code for ~.C. is active
      print'(/''   BCOEF() = '',4f10.5/(13x,4f10.5))',
     *   (BCOEF(I),I=1,NCOEF)
c++ Code for .C. is inactive
c%%     printf( "\n   BCOEF() = " );
c%%     for (i = 1; i <= NCOEF+3; i+=4){
c%%        for (k = i; k <= min( i+3, NCOEF ); k++)
c%%           printf( "%10.5f", Bcoef[k] );
c%%        if (i + 3 < NCOEF) printf( "\n             ");}
c%%     printf( "\n" );
c++ End
c
      call DSDIF(KORDER, NCOEF, TKNOTS, BCOEF, 2, BDIF)
      SMIN = 0.0d0
      SMAX = 0.0d0
      DELX = (XI(NDATA)-XI(1))/30.0D0
      X = XI(1)
      do 20 I = 0, 31
         call DSVALA(KORDER, NCOEF, TKNOTS, 2, BDIF, X, SVALUE)
         SMIN = min(min(SMIN, SVALUE(0)), min(SVALUE(1), SVALUE(2)))
         SMAX = max(max(SMAX, SVALUE(0)), max(SVALUE(1), SVALUE(2)))
         X = X + DELX
   20 continue
      print'(/a)',
     * '     X    YFIT  YFIT''  YFIT''''   '
      X = XI(1)
      do 40 I=0,31
         call DSVALA(KORDER, NCOEF, TKNOTS, 2, BDIF, X, SVALUE)
         IMAGE = ' '
         call DPRPL(SVALUE(0), '*', IMAGE, 49, SMIN, SMAX, .false.)
         call DPRPL(SVALUE(1), '1', IMAGE, 49, SMIN, SMAX, .false.)
         call DPRPL(SVALUE(2), '2', IMAGE, 49, SMIN, SMAX, .false.)
         print'(1x,f6.3,f7.3,f7.3,f7.3,1x,a49)',X, SVALUE(0), SVALUE(1),
     *      SVALUE(2), IMAGE
         X = X + DELX
   40 continue
c                           Compute and print residuals.
      print'(/'' Residuals at the data points:''//
     *   ''    I   XI(I)   YI(I)    YFIT   YFIT-YI(I)'',
     *   ''           YFIT-YI(I)'')'
      do 60 I = 1, NDATA
         YFIT = DSVAL(KORDER, NCOEF, TKNOTS, BCOEF, XI(I), 0)
         call DPRPL(YFIT-YI(I), '*', IMAGE, 39, -0.18d0, 0.18d0, .true.)
         print'(1x,i4,f8.3,f8.3, f8.3, f10.3,1x,a39)',
     *      I, XI(I), YI(I), YFIT, YFIT-YI(I), IMAGE(1:39)
   60 continue
      end
