c     program DRSMPVAL
c     Demonstrates SMPDRV, SMPINT, and SMPVAL.
c>> 1997-05-29 DRSMPVAL Krogh Special code for C conversion.
c>> 1996-05-28 DRSMPVAL Krogh Added external statement.
c>> 1994-10-19 DRSMPVAL Krogh  Changes to use M77CON
c>> 1993-02-04 DRSMPVAL CLL
c>> 1987-12-09 DRSMPVAL Lawson  Initial Code.
c     ------------------------------------------------------------------
c--S replaces "?": DR?MPVAL, ?MPDRV, ?MPINT, ?MPVAL
c     ------------------------------------------------------------------
c++ Code for .C. is inactive
c%%   long int n;
c++ End
      integer NQ, NR
      external SMPVAL
      real                 P(6),Q(7),R(6),Z,SMPVAL
      data     P/ 5.E0, 2.E0, 4.E0, -4.E0, 12.E0, 16.E0 /
c     ------------------------------------------------------------------
      call SMPINT(P, 3,Q,NQ)
      call SMPDRV(Q,NQ,R,NR)
      Z = SMPVAL(Q,NQ,6.E0) - SMPVAL(Q,NQ,4.E0)
c++ Code for ~.C. is active
      print '(21x,''P ='',2f4.0,2x,4f7.2/)', P
      print '('' INTEGRAL OF P.      Q ='',2f4.0,2x,5f7.2/)', Q
      print '('' DERIVATIVE OF Q.    R ='',2f4.0,2x,4f7.2/)', R
      print '('' DEFINITE INTEGRAL.  Z ='',f20.8)', Z
c++ Code for .C. is inactive
c%%   printf( "                     P =%4.0f%4.0f", p[0], p[1] );
c%%   for(n=2L; n < (long)(sizeof(p)/sizeof(p[1])); n++)
c%%           printf( "%7.2f", p[n] );
c%%   printf( "\n" );
c%%   printf( " INTEGRAL OF P.      Q =%4.0f%4.0f", q[0], q[1] );
c%%   for(n=2L; n < (long)(sizeof(q)/sizeof(q[1])); n++)
c%%           printf( "%7.2f", q[n] );
c%%   printf( "\n" );
c%%   printf( " DERIVATIVE OF Q.    R =%4.0f%4.0f", r[0], r[1] );
c%%   for(n=2L; n < (long)(sizeof(r)/sizeof(r[1])); n++)
c%%           printf( "%7.2f", r[n] );
c%%   printf( "\n" );
c%%   printf( " DEFINITE INTEGRAL.  Z =%20.8f\n", z );
c++ End
      stop
      end
