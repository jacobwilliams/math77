c     program DRZCOEF
c>> 1996-06-25 DRZCOEF Krogh Set for deriving C vers.
c>> 1994-07-15 CLL
c>> 1987-12-09 DRZCOEF  Lawson  Initial Code.
c Conversion should only be done from "Z" to "C" for processing to C.
c--Z replaces "?": DR?COEF, ?COEF
c     Demo driver for ZCOEF
c     C. L. Lawson & S. Chiu, JPL, 1987 Feb 17.
c     ------------------------------------------------------------------
      integer I, NDEG1, NDEG2
      double precision RT1(2,3), RT2(2,2)
      double precision ZC(2,4)
c     ------------------------------------------------------------------
c
      data (RT1(1,I),I=1,3) / 1.D0, 1.D0, 3.D0 /
      data (RT1(2,I),I=1,3) / 1.D0, -1.D0, 0.D0 /
      data (RT2(1,I),I=1,2) / 2.D0, 3.D0 /
      data (RT2(2,I),I=1,2) / 1.D0, 2.D0 /
      data NDEG1, NDEG2 / 3, 2 /
c
      call ZCOEF(NDEG1,RT1,ZC)
c++ CODE for ~.C. is active
  100 format(1X/1X,A,I3)
  200 format(1x,A/ (1X,'(',F12.9,',',F12.9,')':
     *                 ' (',F12.9,',',F12.9,')'))
      print 100,'Degree   =',NDEG1
      print 200,'Roots    =',(RT1(1,I),RT1(2,I),I=1,NDEG1)
      print 200,'Coeffs   =',(ZC(1,I),ZC(2,I),I=1,NDEG1+1)
      print '(/)'
c
      call ZCOEF(NDEG2,RT2,ZC)
      print 100,'Degree   =',NDEG2
      print 200,'Roots    =',(RT2(1,I),RT2(2,I),I=1,NDEG2)
      print 200,'Coeffs   =',(ZC(1,I),ZC(2,I),I=1,NDEG2+1)
c++ CODE for .C. is inactive
c%%   printf( " \n Degree   =%3ld", ndeg1 );
c%%   printf( " \n Roots    =\n" );
c%%   for (i = 0; i < ndeg1; i+=2){
c%%      printf( " (%12.9f,%12.9f )", rt1[i][0], rt1[i][1] );
c%%      if (i < ndeg1-1) printf( "  (%12.9f,%12.9f )", rt1[i+1][0],
c%%         rt1[i+1][1] );
c%%      printf( "\n");}
c%%   printf( " \n  Coeffs   =\n" );
c%%   for (i = 0; i <= ndeg1; i+=2){
c%%      printf( " (%12.9f,%12.9f )", zc[i][0], zc[i][1] );
c%%      if (i < ndeg1) printf( "  (%12.9f,%12.9f )", zc[i+1][0],
c%%         zc[i+1][1] );
c%%      printf( "\n");}
c%%
c%%   printf( "\n\n" );
c%%   zcoef( ndeg2, rt2, zc );
c%%   printf( " \n Degree   =%3ld", ndeg2 );
c%%   printf( " \n Roots    =\n" );
c%%   for (i = 0; i < ndeg2; i+=2){
c%%      printf( " (%12.9f,%12.9f )", rt2[i][0], rt2[i][1] );
c%%      if (i < ndeg2-1) printf( "  (%12.9f,%12.9f )", rt2[i+1][0],
c%%         rt2[i+1][1] );
c%%      printf( "\n");}
c%%   printf( " \n  Coeffs   =\n" );
c%%   for (i = 0; i <= ndeg2; i+=2){
c%%      printf( " (%12.9f,%12.9f )", zc[i][0], zc[i][1] );
c%%      if (i < ndeg2) printf( "  (%12.9f,%12.9f )", zc[i+1][0],
c%%         zc[i+1][1] );
c%%      printf( "\n");}
c++ END
      end
