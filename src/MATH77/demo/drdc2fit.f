c     program DRDC2FIT
c>> 1996-07-03 DRDC2FIT Krogh  Changed formats for C conversion.
c>> 1994-10-19 DRDC2FIT Krogh  Changes to use M77CON
c>> 1992-04-27 DRDC2FIT CLL Declared all variables.
c>> 1987-12-09 DRDC2FIT Lawson  Initial Code.
c     Demonstration driver for DC2FIT.
c     C. L. Lawson, JPL, Apr 13 1987, 7/23/87
c     ------------------------------------------------------------------
c--D replaces "?": DR?C2FIT, ?C2FIT, ?HINT
c     ------------------------------------------------------------------
      integer I, NB, NDERIV, NW, NXY
      parameter(NXY = 12, NB = 6, NW = 10)
      external DHINT
      double precision DHINT, X(NXY), Y(NXY), SD(01), B(NB), W(NW,5)
      double precision YKNOT(NB), YPKNOT(NB), R, SIGFAC, YFIT
      integer IERR
      data X / 2.D0, 4.D0, 6.D0, 8.D0,10.D0,12.D0,
     *        14.D0,16.D0,18.D0,20.D0,22.D0,24.D0/
      data Y /2.2D0,4.0D0,5.0D0,4.6D0,2.8D0,2.7D0,
     *        3.8D0,5.1D0,6.1D0,6.3D0,5.0D0,2.0D0/
      data B / 2.0D0, 6.4D0, 10.8D0, 15.2D0, 19.6D0, 24.0D0 /
      data NDERIV / 0 /
      data SD(1) / -1.D0 /
c     ------------------------------------------------------------------
      call DC2FIT(X, Y, SD, NXY, B, NB, W, NW, YKNOT,YPKNOT,SIGFAC,IERR)
      print
     *   '('' DRDC2FIT..  Demo driver for DC2FIT.  Also uses DHINT.'')'
      print'(/5x,''IERR ='',i5,'',  SIGFAC ='',f10.5//''  YKNOT() = '',
     *   6f10.5)', IERR,SIGFAC, (YKNOT(I),I=1,NB)
      print'('' YPKNOT() = '',6f10.5)', (YPKNOT(I),I=1,NB)
      print'(/''  I    X       Y       YFIT  R=Y-YFIT''/)'
      do 10 I=1,NXY
         YFIT=  DHINT(X(I), NDERIV, NB, B, YKNOT, YPKNOT)
         R=Y(I)-YFIT
         print'(1x,i2,f6.0,2f9.3,f10.3)',I,X(I),Y(I),YFIT,R
   10 continue
      stop
      end
