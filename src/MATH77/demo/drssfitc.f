c     program DRSSFITC
c>> 2001-07-16 DRSSFITC Krogh  Added exponent 0 to some constants.
c>> 1996-07-11 DRSSFITC Krogh  Special code for C conversion.
c>> 1996-05-28 DRSSFITC Krogh  Changed Fortran 90 code & changes for C.
c>> 1994-10-19 DRSSFITC Krogh  Changes to use M77CON
c>> 1993-01-13 DRSSFITC C. L. Lawson, JPL
c>> 1992-11-10 C. L. Lawson, JPL
c>> 1992-11-04 C. L. Lawson, JPL
c>> 1989-03-02 C. L. Lawson, JPL
c>> 1988-04-01 C. L. Lawson, JPL
c     DRSSFITC..  Demo driver for SSFITC, Spline fit with constraints.
c     The problem has 24 data points and 10 constraints.
c     The spline is order 4 with 9 coefficients.
c     ------------------------------------------------------------------
c--S replaces "?": DR?SFITC, ?SFITC, ?SVAL, ?SVALA, ?PRPL, ?SDIF
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
      external SSVAL
      real             SSVAL
      real             BCOEF(NCOEF), BDIF(NCOEF*3), DELX, RNORM
      real             SDI(MXY), SMAX, SMIN, SVALUE(0:2)
      real             TKNOTS(NCOEF+KORDER)
      real             WORK(NWORK), X, XI(MXY), YI(MXY), YFIT
      character CCODE(MXY+1)*4, IMAGE*49
      data TKNOTS / 4*0.0E0, 1.5E0, 2.5E0, 3.3E0, 4.0E0, 4.7E0, 4*6.0E0/
      data CCODE /  24*'10~a',
     *             '10=a', '11>a', '12>a', '12>a', '12>a',
     *             '12<a', '12<a', '12<a', '11>a', '10=a',
     *             '   !'/
      data XI / 0.0E0, 0.3E0, 0.7E0, 1.0E0, 1.3E0, 1.7E0, 2.0E0, 2.3E0,
     *          2.5E0, 2.6E0, 2.8E0, 2.9E0, 3.0E0, 3.1E0, 3.2E0, 3.5E0,
     *          3.7E0, 4.0E0, 4.3E0, 4.7E0, 5.0E0, 5.3E0, 5.7E0, 6.0E0,
     *          0.0E0, 0.0E0, 0.0E0, 1.5E0, 2.5E0,
     *          3.5E0, 4.5E0, 6.0E0, 6.0E0, 6.0E0/
 
      data YI /  1.0E0, 1.1E0,0.9E0,1.02E0, 1.2E0,1.0E0,1.2E0,1.4E0,
     *          1.76E0, 2.0E0,2.4E0, 2.6E0, 3.0E0,3.4E0,3.7E0,4.3E0,
     *          4.45E0,4.76E0,4.8E0, 5.0E0,4.96E0,4.9E0,4.9E0,5.0E0,
     *          1.0E0,0.0E0,0.0E0,0.0E0,0.0E0,
     *          0.0E0,0.0E0,0.0E0,0.0E0,5.0E0/
 
      data SDI(1) / -1.0E0 /
      data ISET  / NINFO, NWORK, KPRINT /
c     ------------------------------------------------------------------
      print'('' DRSSFITC..  Demo driver for SSFITC''/'' Least-squares'',
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
      call SSFITC(CCODE, XI, YI, SDI, KORDER, NCOEF, TKNOTS,
     *               BCOEF, RNORM, ISET, INFO, WORK)
c
      print'(/'' After call to SSFITC:'')'
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
      call SSDIF(KORDER, NCOEF, TKNOTS, BCOEF, 2, BDIF)
      SMIN = 0.0e0
      SMAX = 0.0e0
      DELX = (XI(NDATA)-XI(1))/30.0E0
      X = XI(1)
      do 20 I = 0, 31
         call SSVALA(KORDER, NCOEF, TKNOTS, 2, BDIF, X, SVALUE)
         SMIN = min(min(SMIN, SVALUE(0)), min(SVALUE(1), SVALUE(2)))
         SMAX = max(max(SMAX, SVALUE(0)), max(SVALUE(1), SVALUE(2)))
         X = X + DELX
   20 continue
      print'(/a)',
     * '     X    YFIT  YFIT''  YFIT''''   '
      X = XI(1)
      do 40 I=0,31
         call SSVALA(KORDER, NCOEF, TKNOTS, 2, BDIF, X, SVALUE)
         IMAGE = ' '
         call SPRPL(SVALUE(0), '*', IMAGE, 49, SMIN, SMAX, .false.)
         call SPRPL(SVALUE(1), '1', IMAGE, 49, SMIN, SMAX, .false.)
         call SPRPL(SVALUE(2), '2', IMAGE, 49, SMIN, SMAX, .false.)
         print'(1x,f6.3,f7.3,f7.3,f7.3,1x,a49)',X, SVALUE(0), SVALUE(1),
     *      SVALUE(2), IMAGE
         X = X + DELX
   40 continue
c                           Compute and print residuals.
      print'(/'' Residuals at the data points:''//
     *   ''    I   XI(I)   YI(I)    YFIT   YFIT-YI(I)'',
     *   ''           YFIT-YI(I)'')'
      do 60 I = 1, NDATA
         YFIT = SSVAL(KORDER, NCOEF, TKNOTS, BCOEF, XI(I), 0)
         call SPRPL(YFIT-YI(I), '*', IMAGE, 39, -0.18e0, 0.18e0, .true.)
         print'(1x,i4,f8.3,f8.3, f8.3, f10.3,1x,a39)',
     *      I, XI(I), YI(I), YFIT, YFIT-YI(I), IMAGE(1:39)
   60 continue
      end
