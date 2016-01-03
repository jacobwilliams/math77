c++D Default ctypec = "double,"
c++S Default ctypec = "float, "
c++ Replace "float, " = ctypec
c++ CODE for .C. is inactive
C%% void  sprpl(float ,byte,byte[],long,float, float, LOGICAL32);
c++ END
      subroutine SSTAT2(STATS, IHIST, NCELLS, X1, X2)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2001-03-30 SSTAT2 Krogh  Change for C conversion.
c>> 1996-07-01 SSTAT2 Krogh  Added special code for C conversion.
c>> 1995-11-17 SSTAT2 Krogh  Moved formats up for C conversion.
c>> 1994-10-20 SSTAT2 Krogh  Changes to use M77CON
c>> 1989-04-26 SSTAT2 CLL  Changed to use SPRPL instead of PRPL.
c>> 1987-11-24 SSTAT2 Lawson  Initial code.
c--S replaces "?": ?STAT2, ?PRPL, ?STAT1
c++D Default ctype = "(double)"
c++  Default ctype = " (float)"
c++ Replace " (float)" = ctype
c
c-- Begin mask code changes
c        This subr produces a printer-plot of the histogram from
c     IHIST() and prints the statistics from STATS(1:5).
c     This subr is intended to be used following the computation of
c     statistics and the building of a histogram by subr DSTAT1.
c        On entry STATS(1:5) must contain:
c             STATS(1) = Total count.
c             STATS(2) = Min
c             STATS(3) = Max
c             STATS(4) = Mean
c             STATS(5) = Standard deviation
c-- End mask code changes
c
c     See subr SSTAT1 for description of the other
c     subroutine arguments.
c     C. L. Lawson and S. Y. Chiu, JPL, Apr 1987.
c     ------------------------------------------------------------------
      real             BRKPT, FLMAXC, STATS(5), STEP, TEMP, X1, X2
      integer NCELLS, I, IHIST(NCELLS), MAXCNT, NLEN
      parameter(NLEN=30)
c%%   char image[31];
      character*(NLEN) IMAGE
c     ------------------------------------------------------------------
  99  format(2X,'BREAK PT',5X,'COUNT ',9X,'PLOT OF COUNT')
 100  format(2X,F6.2,1X,
     *              '----------------------------------------------')
c
C                        Get max count to scale the plot.
      MAXCNT=0
      do 25 I=1,NCELLS
         MAXCNT = max(IHIST(I),MAXCNT)
  25  continue
c                      The following 2 stmts convert integer to float.
      FLMAXC = MAXCNT
      TEMP = NCELLS-2
      STEP = (X2-X1)/TEMP
      print 99
      if(IHIST(1) .ne. 0) then
C%%      sprpl( (float )( Ihist[1] ),'*',image,30,0.e0,flmaxc,TRUE );
         call SPRPL(real(IHIST(1)),'*',IMAGE,NLEN,0.E0,FLMAXC,.TRUE.)
C%%      printf( "             %7ld     %30.30s\n", Ihist[1], image );
         print '(13X,I7,5X,A)', IHIST(1),IMAGE
      end if
      BRKPT = X1
      do 30 I=2,NCELLS-1
C%%      sprpl( (float )( Ihist[i] ),'*',image,30,0.e0,flmaxc,TRUE );
         call SPRPL(real(IHIST(I)),'*',IMAGE,NLEN,0.E0,FLMAXC,.TRUE.)
         print 100, BRKPT
C%%      printf( "             %7ld     %30.30s\n", Ihist[i], image );
         print '(13X,I7,5X,A)', IHIST(I),IMAGE
         BRKPT = BRKPT + STEP
   30 continue
      print 100, BRKPT
      if(IHIST(NCELLS) .ne. 0) then
C%%   sprpl((float )( Ihist[ncells] ),'*',image,30,0.e0,flmaxc,TRUE);
      call SPRPL(real(IHIST(NCELLS)),'*',IMAGE,NLEN,0.E0,FLMAXC,.TRUE.)
C%%   printf("             %7ld     %30.30s\n", Ihist[ncells], image);
      print '(13X,I7,5X,A)', IHIST(NCELLS),IMAGE
      end if
c                           Finished with printer-plot of histogram.
c
      write(*,'(1x/1x,5a)')
     * '  Count','   Minimum   ','   Maximum   ',
     * '    Mean     ','Std. Deviation'
      write(*,'(1x/1x,i7,3g13.5,g14.5)')
     * int(STATS(1)),STATS(2),STATS(3),STATS(4),STATS(5)
      return
      end
