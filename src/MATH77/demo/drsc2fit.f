c     program DRSC2FIT
c>> 1996-07-03 DRSC2FIT Krogh  Changed formats for C conversion.
c>> 1994-10-19 DRSC2FIT Krogh  Changes to use M77CON
c>> 1992-04-27 DRSC2FIT CLL Declared all variables.
c>> 1987-12-09 DRSC2FIT Lawson  Initial Code.
c     Demonstration driver for SC2FIT.
c     C. L. Lawson, JPL, Apr 13 1987, 7/23/87
c     ------------------------------------------------------------------
c--S replaces "?": DR?C2FIT, ?C2FIT, ?HINT
c     ------------------------------------------------------------------
      integer I, NB, NDERIV, NW, NXY
      parameter(NXY = 12, NB = 6, NW = 10)
      external SHINT
      real             SHINT, X(NXY), Y(NXY), SD(01), B(NB), W(NW,5)
      real             YKNOT(NB), YPKNOT(NB), R, SIGFAC, YFIT
      integer IERR
      data X / 2.E0, 4.E0, 6.E0, 8.E0,10.E0,12.E0,
     *        14.E0,16.E0,18.E0,20.E0,22.E0,24.E0/
      data Y /2.2E0,4.0E0,5.0E0,4.6E0,2.8E0,2.7E0,
     *        3.8E0,5.1E0,6.1E0,6.3E0,5.0E0,2.0E0/
      data B / 2.0E0, 6.4E0, 10.8E0, 15.2E0, 19.6E0, 24.0E0 /
      data NDERIV / 0 /
      data SD(1) / -1.E0 /
c     ------------------------------------------------------------------
      call SC2FIT(X, Y, SD, NXY, B, NB, W, NW, YKNOT,YPKNOT,SIGFAC,IERR)
      print
     *   '('' DRSC2FIT..  Demo driver for SC2FIT.  Also uses SHINT.'')'
      print'(/5x,''IERR ='',i5,'',  SIGFAC ='',f10.5//''  YKNOT() = '',
     *   6f10.5)', IERR,SIGFAC, (YKNOT(I),I=1,NB)
      print'('' YPKNOT() = '',6f10.5)', (YPKNOT(I),I=1,NB)
      print'(/''  I    X       Y       YFIT  R=Y-YFIT''/)'
      do 10 I=1,NXY
         YFIT=  SHINT(X(I), NDERIV, NB, B, YKNOT, YPKNOT)
         R=Y(I)-YFIT
         print'(1x,i2,f6.0,2f9.3,f10.3)',I,X(I),Y(I),YFIT,R
   10 continue
      stop
      end
