c     Program DRZPOLZ
c>> 2009-10-27 DRZPOLZ Krogh/Snyder Added ',' in format stmt.
c>> 1996-06-14 DRZPOLZ Krogh Set for deriving C vers.
c>> 1992-03-06 DRZPOLZ CLL
c>> 1987-12-09 DRZPOLZ  Lawson  Initial Code.
c Conversion should only be done from "Z" to "C" for processing to C.
c--Z replaces "?": DR?POLZ, ?POLZ
c     Demonstration driver for ZPOLZ.
c     ------------------------------------------------------------------
      double precision A1(2,4), A2(2,6), A3(2,6), H(50)
      double precision Z(2,5)
      integer  N1, N2, N3, IERR
c%%   long int i;
      integer  I, K
c
      data A1 / 1.D0, 0.D0, -4.D0, 0.D0, 1.D0, 0.D0, -4.D0, 0.D0 /
      data A2 / 1.D0, 0.D0, -15.D0, 0.D0, 85.D0, 0.D0, -225.D0, 0.D0,
     *          274.D0, 0.D0, -120.D0, 0.D0 /
      data A3 / 1.0d0, 0.0d0,   -5.5d0, -1.5d0,
     *         10.00d0,   7.750d0,  -7.500d0,  -13.25d0,
     *         -1.000d0,   11.00d0,  3.000d0,  -1.500d0/
c     ------------------------------------------------------------------
      N1 = 3
      N2 = 5
      N3 = 5
c
      call ZPOLZ(A1,N1,Z,H,IERR)
c++ CODE for ~.C. is active
  100 format(' Degree =',I2//
     *       ' Coefficients ='/
     *       (2(1X:'(',1X,F14.9,',',1X,F14.9,1X,')',2X)))
  200 format(/' Roots ='/(2(1X,'(',1X,F14.9,',',1X,F14.9,1X,')':2X)))
c
      print 100, N1, ((A1(K,I),K=1,2),I=1,N1+1)
      print 200, ((Z(K,I),K=1,2),I=1,N1)
c
      print'(/)'
      print 100, N2, ((A2(K,I),K=1,2),I=1,N2+1)
      call ZPOLZ(A2,N2,Z,H,IERR)
      print 200, ((Z(K,I),K=1,2),I=1,N2)
c
      print'(/)'
      print 100, N3, ((A3(K,I),K=1,2),I=1,N3+1)
      call ZPOLZ(A3,N3,Z,H,IERR)
      print 200, ((Z(K,I),K=1,2),I=1,N3)
c++ CODE for .C. is inactive
c%%   printf( " Degree =%2ld\n\n Coefficients =\n", n1 );
c%%   for (i = 0; i <= n1; i+=2){
c%%     printf( " (%14.9f, %14.9f )", a1[i][0], a1[i][1] );
c%%     if (i < n1) printf( "  (%14.9f, %14.9f )", a1[i+1][0],
c%%        a1[i+1][1] );
c%%     printf( " \n" );}
c%%   printf( "\n Roots =\n" );
c%%   for (i = 0; i <= (n1-1); i+=2){
c%%     printf( " (%14.9f, %14.9f )", z[i][0], z[i][1] );
c%%     if (i < (n1-1)) printf( " (%14.9f, %14.9f )", z[i+1][0],
c%%        z[i+1][1] );
c%%     printf( " \n" );}
c%%
c%%   printf( "\n\n Degree =%2ld\n\n Coefficients =\n", n2 );
c%%   for (i = 0; i <= n2; i+=2){
c%%     printf( " (%14.9f, %14.9f )", a2[i][0], a2[i][1] );
c%%     if (i < n2) printf( "  (%14.9f, %14.9f )", a2[i+1][0],
c%%        a2[i+1][1] );
c%%     printf( "\n" );}
c%%   zpolz( a2, n2, z, h, &ierr );
c%%   printf( "\n Roots =\n" );
c%%   for (i = 0; i <= (n2-1); i+=2){
c%%     printf( " (%14.9f, %14.9f )", z[i][0], z[i][1] );
c%%     if (i < (n2-1)) printf( "  (%14.9f, %14.9f )", z[i+1][0],
c%%        z[i+1][1] );
c%%     printf( " \n" );}
c%%
c%%   printf( "\n\n Degree =%2ld\n\n Coefficients =\n", n3 );
c%%   for (i = 0; i <= n3; i+=2){
c%%     printf( " (%14.9f, %14.9f )", a3[i][0], a3[i][1] );
c%%     if (i < n3) printf( "  (%14.9f, %14.9f )", a3[i+1][0],
c%%        a3[i+1][1] );
c%%     printf( "\n" );}
c%%   zpolz( a3, n3, z, h, &ierr );
c%%   printf( "\n Roots =\n" );
c%%   for (i = 0; i <= (n3-1); i+=2){
c%%     printf( " (%14.9f, %14.9f )", z[i][0], z[i][1] );
c%%     if (i < (n3-1)) printf( "  (%14.9f, %14.9f )", z[i+1][0],
c%%        z[i+1][1] );
c%%     printf( " \n" );}
c++ END
      end
