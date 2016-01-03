c     Program DRSPOLZ
c>> 2009-10-28 DRZPOLZ Krogh Mods to get comples used in single prec.
c>> 1996-07-09 DRZPOLZ Krogh Set for deriving single precision C vers.
c>> 1994-08-09 DRSPOLZ WVS Remove '0' in format
c>> 1992-03-06 DRSPOLZ CLL
c>> 1987-12-09 DRSPOLZ  Lawson  Initial Code.
c Conversion should only be done from "D" to "S" for processing to C.
c--S replaces "?": DR?POLZ, ?POLZ
c     Demonstration driver for SPOLZ.
c     ------------------------------------------------------------------
      real             A1(4), A2(6), H(25)
 
c++ CODE for .D. | .C. is inactive 
C      real             Z1(2,3), Z2(2,5)
c++ CODE for .S. & ~.C. is active
      complex       Z1(3), Z2(5)
c++ END
      integer N1, N2, IERR
c++ CODE for ~.C. is active
      integer I, k
c++ CODE for .C. is inactive
c%%    long int i, k;
c++ END
c
      data A1 / 1.E0, -4.E0, 1.E0, -4.E0 /
      data A2 / 1.E0, -15.E0, 85.E0, -225.E0, 274.E0, -120.E0 /
c     ------------------------------------------------------------------
      N1 = 3
      N2 = 5
c
c++ CODE for ~.C. is active
  100 format(' ','Degree =',I2/' ', 'Coefficients =',(T20,4(F10.4,1X)))
  200 format(' ','Roots ='/(2(1X,'(',1X,F8.5,',',1X,F8.5,2X,')':2X)))
  300 format(//' ')
      print 100, N1, (A1(I),I=1,4)
c++ CODE for .C. is inactive
c%%   printf( " Degree =%2ld\n Coefficients =      ", n1 );
c%%   for (i = 0; i < 4; i++) printf( "%10.4f", a1[i] );
c++ End
      call SPOLZ(A1,N1,Z1,H,IERR)
c++ CODE for .D. & ~.C. is inactive 
C      print 200, ((Z1(K,I),K=1,2),I=1,3)
c++ CODE for .S. & ~.C. is active
      print 200, (Z1(I),I=1,3)
c++ CODE for ~.C. is active
      print 300
      print 100, N2, (A2(I),I=1,6)
c++ CODE for .C. is inactive
c%%   printf( "\n Roots =\n" );
c%%   for (i = 0; i < 3; i+=2) {
c%%      printf( " ( %8.5f, %8.5f  )", z1[i][0],z1[i][1] );
c%%      if (i<2) printf( " ( %8.5f, %8.5f  )", z1[i+1][0],z1[i+1][1] );
c%%      printf( "\n" );}
c%%   printf( "\n\n \n Degree =%2ld\n Coefficients =      ", n2 );
c%%   for (i = 0; i < 6; i+=4) {
c%%      for (k = i ; k < (i < 2 ? i + 4 : 6); k++)
c%%         printf( "%10.4f", a2[k] );
c%%         if (i < 4) printf( "\n                     " );}
c++ End
      call SPOLZ(A2,N2,Z2,H,IERR)
c++ CODE for .D. & ~.C. is inactive 
C      print 200,((Z2(K,I),K=1,2),I=1,5)
c++ CODE for .S. & ~.C. is active
      print 200,(Z2(I),I=1,5)
c++ CODE for .C. is inactive
c%%   printf( "\n Roots =\n" );
c%%   for (i = 0; i < 5; i+=2) {
c%%       printf( " ( %8.5f, %8.5f  )", z2[i][0],z2[i][1] );
c%%       if (i<4) printf(" ( %8.5f, %8.5f  )", z2[i+1][0],z2[i+1][1]);
c%%       printf( "\n" );}
c%%   printf( "\n" );
c++ END
      end
