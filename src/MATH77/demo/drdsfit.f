c     program DRDSFIT
c>> 1996-07-03 DRDSFIT Krogh Special code for C conversion.
c>> 1996-06-19 DRDSFIT Krogh Changes in formats for C conversion.
c>> 1996-05-28 DRDSFIT Krogh Added external & removed Fortran 90 syntax
c>> 1994-10-19 DRDSFIT Krogh Changes to use M77CON
c>> 1992-11-18 DRDSFIT CLL   Changed order of arguments in DSFIT.
c>> 1992-10-29 C. L. Lawson, JPL
c    Demonstration driver for DSFIT, DSVAL, DSQUAD, DSTOP, DPVAL, DPQUAD
c     ------------------------------------------------------------------
c--D replaces "?": DR?SFIT, ?SFIT, ?SVAL, ?SQUAD, ?STOP, ?PVAL, ?PQUAD
c--&                 ?PRPL
c     ------------------------------------------------------------------
c++ Code for .C. is inactive
c%%   long int k;
c++ End
      integer I, IERR, NXY, KORDER, MPC, NCOEF, NDERIV, NPC, NT, LDW
      parameter(NXY = 12, NCOEF=8, KORDER=4, NT = NCOEF+KORDER,LDW = 10)
      parameter(MPC = NCOEF-KORDER+1)
      external DSVAL, DSQUAD, DPVAL, DPQUAD
      double precision BDIF(NCOEF*KORDER), DSVAL, DSQUAD, DPVAL, DPQUAD
      double precision BCOEF(NCOEF), PCOEF(MPC*KORDER)
      double precision SD(1), SIGFAC, TKNOTS(NT), W(LDW,KORDER+1)
      double precision X(NXY), XI(MPC+1), Y(NXY), YFIT, Z
      character IMAGE*31
      data X / 2.D0, 4.D0, 6.D0, 8.D0,10.D0,12.D0,
     *        14.D0,16.D0,18.D0,20.D0,22.D0,24.D0/
      data Y /2.2D0,4.0D0,5.0D0,4.6D0,2.8D0,2.7D0,
     *        3.8D0,5.1D0,6.1D0,6.3D0,5.0D0,2.0D0/
      data TKNOTS / 4*2.0D0, 6.4D0, 10.8D0, 15.2D0, 19.6D0, 4*24.0D0 /
      data NDERIV / 0 /
      data SD(1) / -1.0D0 /
c     ------------------------------------------------------------------
      print'('' DRDSFIT''/ '' Demo driver for DSFIT, DSVAL, DSQUAD,'',
     *  '' DSTOP, DPVAL, DPQUAD'')'
      print'(/''   KORDER ='',i3,'',  NCOEF ='',i3)', KORDER, NCOEF
c++ Code for ~.C. is active
      print'(''   TKNOTS() = '',4f10.5/(14x,4f10.5))',
     *   (TKNOTS(I), I = 1, NT)
c++ Code for .C. is inactive
c%%     printf( "\n   TKNOTS() = ");
c%%     for (i = 1; i <= NT+3; i+=4){
c%%        for (k = i; k <= min( i+3, NT ); k++)
c%%           printf( "%10.5f", Tknots[k] );
c%%        if (i + 3 < NT) printf( "\n              ");}
c++ End
      call DSFIT(X, Y, SD, NXY, KORDER, NCOEF, TKNOTS, BCOEF,
     *           SIGFAC,IERR, LDW, W)
c++ Code for ~.C. is active
      print'(/'' After call to DSFIT:''/''   IERR ='',i5,
     *  '',  SIGFAC ='', f10.5 //''   BCOEF() = '',
     *  4f10.5/(13x,4f10.5))', IERR, SIGFAC, (BCOEF(I),I=1,NCOEF)
c++ Code for .C. is inactive
c%%     printf( "\n After call to DSFIT:\n   IERR =%5ld,  SIGFAC ="
c%%        "%10.5f\n\n   BCOEF() = ", ierr, sigfac);
c%%     for (i = 1; i <= NCOEF+3; i+=4){
c%%        for (k = i; k <= min( i+3, NCOEF ); k++)
c%%           printf( "%10.5f", Bcoef[k] );
c%%        if (i + 3 < NCOEF) printf( "\n             ");}
c++ End
      print'(/'' Evaluating fitted spline function using DSVAL:'')'
      print'(/
     * ''    I    X       Y       YFIT  R=Y-YFIT               YFIT''/)'

      do 20 I=1,NXY
       YFIT=  DSVAL(KORDER, NCOEF, TKNOTS, BCOEF, X(I), NDERIV)
       call DPRPL(YFIT, '*', IMAGE, 31, 1.9d0, 6.3d0, .true.)
       print'(3x,i2,f6.0,2f9.3,f10.3,3x,a31)',
     *    I, X(I), Y(I), YFIT, Y(I)-YFIT, IMAGE
   20 continue

      Z = DSQUAD(KORDER, NCOEF, TKNOTS, BCOEF, 5.0d0, 20.0d0)
      print'(/'' Integral from 5.0  to 20.0 using DSQUAD:'',f12.5)',Z

      call DSTOP(KORDER, NCOEF, TKNOTS, BCOEF, BDIF, NPC, XI, PCOEF)
      print'(/
     * '' Using DSTOP to convert from B-spline basis to power basis.'')'
      print'(''        NPC ='',i3)',NPC
c++ Code for ~.C. is active
      print'(''       XI() = '',4f10.5/(14x,4f10.5))',(XI(I),I=1,NPC+1)
      print'(''    PCOEF() = '',4f10.5/(14x,4f10.5))',
     *   (PCOEF(I),I=1,NPC*KORDER)
c++ Code for .C. is inactive
c%%   printf( "       XI() = ");
c%%   for (i = 1; i <= (npc + 1); i+=4){
c%%      for (k = i; k <= min( i+3, npc+1 ); k++)
c%%           printf( "%10.5f", Xi[k] );
c%%      if (i <= npc) printf( "\n              ");}
c%%   printf( "\n    PCOEF() = ");
c%%   for (i = 1; i <= (npc*KORDER); i+=4){
c%%      for (k = i; k <= min( i+3, npc*KORDER ); k++)
c%%           printf( "%10.5f", Pcoef[k] );
c%%      if (i < npc*KORDER) printf( "\n              ");}
c++ End
      print'(/'' Evaluating fitted spline function using DPVAL:'')'
      print'(/
     * ''    I    X       Y       YFIT  R=Y-YFIT               YFIT''/)'

      do 40 I=1,NXY
       YFIT=  DPVAL(KORDER, NPC, XI, PCOEF, X(I), NDERIV)
       call DPRPL(YFIT, '*', IMAGE, 31, 1.9d0, 6.3d0, .true.)
       print'(3x,i2,f6.0,2f9.3,f10.3,3x,a31)',
     *    I, X(I), Y(I), YFIT, Y(I)-YFIT, IMAGE
   40 continue

      Z = DPQUAD(KORDER, NPC, XI, PCOEF, 5.0d0, 20.0d0)
      print'(/'' Integral from 5.0  to 20.0 using DPQUAD:'',f12.5)',Z
      end
