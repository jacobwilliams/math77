c     program DRSPFIT
c>> 1996-06-21 DRSPFIT Krogh Special code for C conversion.
c>> 1996-05-28 DRSPFIT Krogh Added external statement.
c>> 1995-09-15 DRSPFIT Krogh Declare all variables.
c>> 1995-09-15 DRSPFIT Krogh Remove '0' in format (again?)
c>> 1994-10-19 DRSPFIT Krogh  Changes to use M77CON
c>> 1994-08-09 DRSPFIT WVS Remove '0' in format
c>> 1991-11-20 DRSPFIT CLL Edited for Fortran 90
c>> 1987-12-09 DRSPFIT  Lawson  Initial Code.
c--S replaces "?": DR?PFIT, ?PFIT, ?CPVAL
c     Demonstration driver for SPFIT.
c++ Code for .C. is inactive
c%%   long int j;
c++ End
      external SCPVAL
      real              X(12),Y(12),P(11),SIGFAC,W(121),SCPVAL
      real              SIG(1), R, YFIT
      integer I, M, NDEG, NP3
c
      data X / 2.E0, 4.E0, 6.E0, 8.E0, 10.E0, 12.E0, 14.E0,
     *         16.E0, 18.E0, 20.E0, 22.E0, 24.E0 /
      data Y / 2.2E0, 4.0E0, 5.0E0, 4.6E0, 2.8E0, 2.7E0,
     *         3.8E0, 5.1E0, 6.1E0, 6.3E0, 5.0E0, 2.0E0 /
      data SIG(1) / -1.E0 /
      data M      / 12 /
c
 1001 format(1X/'  I    X       Y       YFIT  R=Y-YFIT'/1X)
 1002 format(1X,I2,F6.0,2F9.3,F10.3)
c
      call SPFIT(M,X,Y,SIG,8, .TRUE. , .TRUE. , .TRUE. ,P,NDEG,SIGFAC,W)
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
        YFIT = SCPVAL(P,NDEG,X(I))
        R = Y(I) - YFIT
        write(*,1002)I,X(I),Y(I),YFIT,R
   20 continue
      stop
      end
