c     DEMONSTRATE DCPDRV, DCPINT, AND DCPVAL.
c>> 2001-05-22 DRDCPVAL Krogh Minor change for making .f90 version.
c>> 1997-05-29 DRDCPVAL Krogh Special code for C conversion.
c>> 1996-05-28 DRDCPVAL Krogh Added external statement.
c>> 1994-10-19 DRDCPVAL Krogh  Changes to use M77CON
c>> 1987-12-09 DRDCPVAL Lawson  Initial Code.
c--D replaces "?": DR?CPVAL, ?CPDRV, ?CPINT, ?CPVAL
c
      external DCPVAL
c++ Code for .C. is inactive
c%%   long int i;
c++ End
      integer NQ, NR
      double precision     P(6),Q(7),R(6),Z,DCPVAL
      data     P/ 5.D0, 2.D0, 10.D0, 8.D0, 6.D0, 4.D0 /
c
      call DCPINT(P,3,Q,NQ)
      call DCPDRV(Q,NQ,R,NR)
      Z=DCPVAL(Q,NQ,6.D0)-DCPVAL(Q,NQ,4.D0)
c++ Code for ~.C. is active
      write(*,1000) P,Q,R,Z
 1000 format(21X,'P =',2F4.0,2X,4F7.2//' INTEGRAL OF P.      Q ='
     *,2F4.0,2X,5F7.2//' DERIVATIVE OF Q.    R =',2F4.0,2X,4F7.2/
     */' DEFINITE INTEGRAL.  Z =',F20.8)
c++ Code for .C. is inactive
c%%   printf( "                     P =%4.0f%4.0f  ", p[0], p[1]);
c%%   for(i=2L; i < sizeof(p)/sizeof(p[1]); i++)
c%%           printf( "%7.2f", p[i] );
c%%   printf( "\n INTEGRAL OF P.      Q =%4.0f%4.0f  ", q[0], q[1]);
c%%   for(i=2L; i < sizeof(q)/sizeof(q[1]); i++)
c%%           printf( "%7.2f", q[i] );
c%%   printf( "\n DERIVATIVE OF Q.    R =%4.0f%4.0f  ", r[0], r[1]);
c%%   for(i=2L; i < sizeof(r)/sizeof(r[1]); i++)
c%%           printf( "%7.2f", r[i] );
c%%   printf( "\n DEFINITE INTEGRAL.  Z =%7.2f\n", z );
c++ End
      stop
      end
