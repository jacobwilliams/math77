c     program DRSSFIT
c>> 1996-07-03 DRSSFIT Krogh Special code for C conversion.
c>> 1996-06-19 DRSSFIT Krogh Changes in formats for C conversion.
c>> 1996-05-28 DRSSFIT Krogh Added external & removed Fortran 90 syntax
c>> 1994-10-19 DRSSFIT Krogh Changes to use M77CON
c>> 1992-11-18 DRSSFIT CLL   Changed order of arguments in SSFIT.
c>> 1992-10-29 C. L. Lawson, JPL
c    Demonstration driver for SSFIT, SSVAL, SSQUAD, SSTOP, SPVAL, SPQUAD
c     ------------------------------------------------------------------
c--S replaces "?": DR?SFIT, ?SFIT, ?SVAL, ?SQUAD, ?STOP, ?PVAL, ?PQUAD
c--&                 ?PRPL
c     ------------------------------------------------------------------
c++ Code for .C. is inactive
c%%   long int k;
c++ End
      integer I, IERR, NXY, KORDER, MPC, NCOEF, NDERIV, NPC, NT, LDW
      parameter(NXY = 12, NCOEF=8, KORDER=4, NT = NCOEF+KORDER,LDW = 10)
      parameter(MPC = NCOEF-KORDER+1)
      external SSVAL, SSQUAD, SPVAL, SPQUAD
      real             BDIF(NCOEF*KORDER), SSVAL, SSQUAD, SPVAL, SPQUAD
      real             BCOEF(NCOEF), PCOEF(MPC*KORDER)
      real             SD(1), SIGFAC, TKNOTS(NT), W(LDW,KORDER+1)
      real             X(NXY), XI(MPC+1), Y(NXY), YFIT, Z
      character IMAGE*31
      data X / 2.E0, 4.E0, 6.E0, 8.E0,10.E0,12.E0,
     *        14.E0,16.E0,18.E0,20.E0,22.E0,24.E0/
      data Y /2.2E0,4.0E0,5.0E0,4.6E0,2.8E0,2.7E0,
     *        3.8E0,5.1E0,6.1E0,6.3E0,5.0E0,2.0E0/
      data TKNOTS / 4*2.0E0, 6.4E0, 10.8E0, 15.2E0, 19.6E0, 4*24.0E0 /
      data NDERIV / 0 /
      data SD(1) / -1.0E0 /
c     ------------------------------------------------------------------
      print'('' DRSSFIT''/ '' Demo driver for SSFIT, SSVAL, SSQUAD,'',
     *  '' SSTOP, SPVAL, SPQUAD'')'
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
      call SSFIT(X, Y, SD, NXY, KORDER, NCOEF, TKNOTS, BCOEF,
     *           SIGFAC,IERR, LDW, W)
c++ Code for ~.C. is active
      print'(/'' After call to SSFIT:''/''   IERR ='',i5,
     *  '',  SIGFAC ='', f10.5 //''   BCOEF() = '',
     *  4f10.5/(13x,4f10.5))', IERR, SIGFAC, (BCOEF(I),I=1,NCOEF)
c++ Code for .C. is inactive
c%%     printf( "\n After call to SSFIT:\n   IERR =%5ld,  SIGFAC ="
c%%        "%10.5f\n\n   BCOEF() = ", ierr, sigfac);
c%%     for (i = 1; i <= NCOEF+3; i+=4){
c%%        for (k = i; k <= min( i+3, NCOEF ); k++)
c%%           printf( "%10.5f", Bcoef[k] );
c%%        if (i + 3 < NCOEF) printf( "\n             ");}
c++ End
      print'(/'' Evaluating fitted spline function using SSVAL:'')'
      print'(/
     * ''    I    X       Y       YFIT  R=Y-YFIT               YFIT''/)'
 
      do 20 I=1,NXY
       YFIT=  SSVAL(KORDER, NCOEF, TKNOTS, BCOEF, X(I), NDERIV)
       call SPRPL(YFIT, '*', IMAGE, 31, 1.9e0, 6.3e0, .true.)
       print'(3x,i2,f6.0,2f9.3,f10.3,3x,a31)',
     *    I, X(I), Y(I), YFIT, Y(I)-YFIT, IMAGE
   20 continue
 
      Z = SSQUAD(KORDER, NCOEF, TKNOTS, BCOEF, 5.0e0, 20.0e0)
      print'(/'' Integral from 5.0  to 20.0 using SSQUAD:'',f12.5)',Z
 
      call SSTOP(KORDER, NCOEF, TKNOTS, BCOEF, BDIF, NPC, XI, PCOEF)
      print'(/
     * '' Using SSTOP to convert from B-spline basis to power basis.'')'
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
      print'(/'' Evaluating fitted spline function using SPVAL:'')'
      print'(/
     * ''    I    X       Y       YFIT  R=Y-YFIT               YFIT''/)'
 
      do 40 I=1,NXY
       YFIT=  SPVAL(KORDER, NPC, XI, PCOEF, X(I), NDERIV)
       call SPRPL(YFIT, '*', IMAGE, 31, 1.9e0, 6.3e0, .true.)
       print'(3x,i2,f6.0,2f9.3,f10.3,3x,a31)',
     *    I, X(I), Y(I), YFIT, Y(I)-YFIT, IMAGE
   40 continue
 
      Z = SPQUAD(KORDER, NPC, XI, PCOEF, 5.0e0, 20.0e0)
      print'(/'' Integral from 5.0  to 20.0 using SPQUAD:'',f12.5)',Z
      end
