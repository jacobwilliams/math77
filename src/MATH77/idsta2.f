c++D Default ctypec = "double,"
c++S Default ctypec = "float, "
c++ Replace "double," = ctypec
c++ CODE for .C. is inactive
C%% void  dprpl(double,byte,byte[],long,double,double,LOGICAL32);
c++ END
      subroutine IDSTA2(ISTATS, XSTATS, IHIST, ILOW, NCELLS)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2001-03-30 IDSTA2 Krogh  Change for C conversion.
c>> 1995-07-09 IDSTA2 Krogh  Small modification for C conversion.
c>> 1994-10-20 IDSTA2 Krogh  Changes to use M77CON
c>> 1994-06-22 IDSTA2 CLL  Changed name to I[D/S]STA2.
c>> 1989-04-26 ISTAT2 CLL  Changed to use DPRPL instead of PRPL.
c>> 1987-11-24 ISTAT2 Lawson  Initial code.
c
c        This subr produces a printer-plot of a histogram and prints
c     statistics given in ISTATS() and XSTATS().  It is
c     intended for use following the building of a histogram and
c     computation of statistics by ISTAT1.
c     See ISTAT1 for description of the subroutine arguments.
c     C. L. Lawson and S. Y. Chiu, JPL, Apr 1987.
c     ------------------------------------------------------------------
c--D replaces "?": I?STA2, ?PRPL
c++D Default ctype = "(double)"
c++  Default ctype = " (float)"
c++ Replace "(double)" = ctype
c     ------------------------------------------------------------------
      double precision    FLMAXC, XSTATS(2)
      integer MAXCNT, NCELLS, NLEN
      integer I, IHIST(NCELLS), ILOW, ISTATS(3), IVAL
      parameter(NLEN=30)
      character*(NLEN) IMAGE
c     ------------------------------------------------------------------
C                        Get max count to scale the plot.
      MAXCNT=0
      do 25 I=1,NCELLS
         MAXCNT = max(IHIST(I),MAXCNT)
  25  continue
c                 The following stmt converts from integer to float.
      FLMAXC = MAXCNT
      write(*,'(2X,''   VALUE'',5X,''COUNT '',9X,''PLOT OF COUNT'')')
      if(IHIST(1) .ne. 0) then
c++ CODE for .C. is inactive
c%%         dprpl( (double)( Ihist[1] ), '*', image, NLEN, 0.e0,
c%%            flmaxc, TRUE );
c++ CODE for ~.C. is active
         call DPRPL(dble(IHIST(1)),'*',IMAGE,NLEN,0.D0,FLMAXC,.TRUE.)
c++ End
         write(*,'(1x,''<'',i8,i10,5x,a)') ILOW, IHIST(1), IMAGE
      endif
      IVAL = ILOW - 1
      do 30 I=2,NCELLS-1
         IVAL = IVAL + 1
c++ CODE for .C. is inactive
c%%         dprpl( (double)( Ihist[i] ), '*', image, NLEN, 0.e0,
c%%            flmaxc, TRUE );
c++ CODE for ~.C. is active
         call DPRPL(dble(IHIST(I)),'*',IMAGE,NLEN,0.D0,FLMAXC,.TRUE.)
c++ End
         write(*,'(2x,i8,i10,5x,a)') IVAL, IHIST(I), IMAGE
   30 continue
      if(IHIST(NCELLS) .ne. 0) then
c++ CODE for .C. is inactive
c%%         dprpl( (double)( Ihist[ncells] ), '*', image, NLEN,
c%%           0.e0, flmaxc, TRUE );
c++ CODE for ~.C. is active
         call DPRPL(dble(IHIST(NCELLS)),'*',IMAGE,NLEN,
     *             0.D0,FLMAXC,.TRUE.)
c++ End
         write(*,'(1x,''>'',i8,i10,5x,a)') IVAL, IHIST(NCELLS), IMAGE
      endif
c                           Finished with printer-plot of histogram.
c
      write(*,'(1x/1x,5a)')
     * '     Count','   Minimum','   Maximum',
     * '    Mean     ','Std. Deviation'
      write(*,'(1x/1x,3i10,g13.5,g14.5)')
     * ISTATS(1), ISTATS(2), ISTATS(3),XSTATS(1),XSTATS(2)
      return
      end
