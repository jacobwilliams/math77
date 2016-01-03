      program DRDCON
c>> 2001-05-22 DRDCON Krogh Minor change for making .f90 version.
c>> 1996-06-25 DRDCON Krogh  Special code for C conversion.
c>> 1994-10-19 DRDCON Krogh  Changes to use M77CON
c>> 1994-08-09 DRDCON WVS    Remove '0' from formats
c>> 1992-03-09 DRDCON Krogh  Initial Code.
c Check program for converting between Chebyshev and monomial basis.
c--D replaces "?": DR?CON, ?CONCM, ?CONMC
c
      integer NMAX
      parameter (NMAX=6)
      integer          K, N
      double precision COEFF(0:NMAX)
c
c%%   printf( "       " );
c%%   for (k = 0; k <= NMAX; k++) printf( "   X**%1ld", k );
c%%   printf( "\n" );
      print '(7X, 9(:''    X**'',I1))', (K, K = 0, NMAX)
      do 20 N = 0, NMAX
         do 10 K = 0, N-1
            COEFF(K) = 0.D0
   10    continue
         COEFF(N) = 1.D0
         call DCONCM(N, COEFF)
         print '('' T'', I1, ''(X) ='', F7.3, 8F8.3)', N,
     1      (COEFF(K), K = 0, N)
   20 continue
c%%   printf( "\n      " );
c%%   for (k = 0; k <= NMAX; k++) printf( "   T%1ld(X)", k );
c%%   printf( "\n" );
      print '(/, 6X, 9(:''   T'', I1, ''(X)''))', (K, K = 0, NMAX)
      do 120 N = 0, NMAX
         do 110 K = 0, N-1
            COEFF(K) = 0.D0
  110    continue
         COEFF(N) = 1.D0
         call DCONMC(N, COEFF)
         print '('' X**'', I1, '' ='', 9F8.5)', N, (COEFF(K), K = 0, N)
  120 continue
      stop
      end
