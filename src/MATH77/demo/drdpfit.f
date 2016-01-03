c     program DRDPFIT
c>> 1996-06-21 DRDPFIT Krogh Special code for C conversion.
c>> 1996-05-28 DRDPFIT Krogh Added external statement.
c>> 1995-09-15 DRDPFIT Krogh Declare all variables.
c>> 1995-09-15 DRDPFIT Krogh Remove '0' in format (again?)
c>> 1994-10-19 DRDPFIT Krogh  Changes to use M77CON
c>> 1994-08-09 DRDPFIT WVS Remove '0' in format
c>> 1991-11-20 DRDPFIT CLL Edited for Fortran 90
c>> 1987-12-09 DRDPFIT  Lawson  Initial Code.
c--D replaces "?": DR?PFIT, ?PFIT, ?CPVAL
c     Demonstration driver for DPFIT.
c++ Code for .C. is inactive
c%%   long int j;
c++ End
      external DCPVAL
      double precision  X(12),Y(12),P(11),SIGFAC,W(121),DCPVAL
      double precision  SIG(1), R, YFIT
      integer I, M, NDEG, NP3
c
      data X / 2.D0, 4.D0, 6.D0, 8.D0, 10.D0, 12.D0, 14.D0,
     *         16.D0, 18.D0, 20.D0, 22.D0, 24.D0 /
      data Y / 2.2D0, 4.0D0, 5.0D0, 4.6D0, 2.8D0, 2.7D0,
     *         3.8D0, 5.1D0, 6.1D0, 6.3D0, 5.0D0, 2.0D0 /
      data SIG(1) / -1.D0 /
      data M      / 12 /
c
 1001 format(1X/'  I    X       Y       YFIT  R=Y-YFIT'/1X)
 1002 format(1X,I2,F6.0,2F9.3,F10.3)
c
      call DPFIT(M,X,Y,SIG,8, .TRUE. , .TRUE. , .TRUE. ,P,NDEG,SIGFAC,W)
      NP3 = NDEG+3

c++ Code for ~.C. is active
      print
     *'(/'' NDEG ='',I2,10X,''SIGFAC ='',F8.4//
     *'' P(1),P(2) ='',9X,2F15.8//'' P(3),...,P(NDEG+3) ='',3F15.8/
     *(21X,3F15.8))', NDEG,SIGFAC,(P(I),I=1,NP3)
c++ Code for .C. is inactive
c%%   printf(
c%%      "\n NDEG =%2ld          SIGFAC =%8.4f",ndeg,sigfac );
c%%   printf(
c%%   "\n\n P(1),P(2) =         %15.8f%15.5f\n\n P(3),...,P(NDEG+3) =",
c%%   p[0], p[1]);
c%%   for (i = 2; i < (np3); i+=3){
c%%      for (j = i; j < (i < np3-2 ? i+3 : np3); j++)
c%%           printf("%15.8f", p[j] );
c%%           if (i < np3-2) printf("\n                     ");}
c%%   printf( "\n" );
c++ End
      write(*,1001)
      do 20 I = 1,M
        YFIT = DCPVAL(P,NDEG,X(I))
        R = Y(I) - YFIT
        write(*,1002)I,X(I),Y(I),YFIT,R
   20 continue
      stop
      end
