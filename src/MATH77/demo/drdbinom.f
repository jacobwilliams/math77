c     program DRDBINOM
c>> 1998-05-12 DRDBINOM Krogh Ensure Pascal print output.
c>> 1995-12-15 DRDBINUM Krogh Initial Code.
c--D replaces "?": DR?BINOM, ?BINOM
c
c Test DBINOM for computing binomial coefficients.
c Checks a large value computed with factorials, one a little larger
c that is computed using log gamma, and then finds first that differs
c from that obtained using the Pascal triangle.
c
      integer K, N, NMAX
      parameter (NMAX = 120)
      external DBINOM
      double precision BC1, BC2, P(0:NMAX), TP, DBINOM
c            ( 150 )            ( 151 )
c      BC1 = (     )      BC2 = (     )
c            (  30 )            (  29 )
      parameter (BC1 = 32198785340494567031466236484400.D0)
      parameter (BC2 = 9880808670399701168713050486000.D0)
c
   10 format (/'  N    K',12X,'DBINOM',20X, A, 8X, ' Col. 2 - Col. 1')
   20 format (I4, I4,1P,E26.17,E26.17,1P,E16.7)
c
      print 10, ' True '
      TP =  DBINOM(150, 30)
      print 20, 150, 30, TP, BC1, BC1 - TP
      TP =  DBINOM(151, 29)
      print 20, 151, 29, TP, BC2, BC2 - TP
c
      print 10, 'Pascal'
      P(0) = 1.D0
      do 80 N = 0, NMAX
         do 40 K = 0, N
            TP = DBINOM(N, K)
            if (TP .ne. P(K)) go to 100
   40    continue
c                  Update the Pascal Triangle.
         P(N+1) = 1.D0
         do 60 K = N, 1, -1
            P(K) = P(K) + P(K-1)
   60    continue
   80 continue
  100 print 20, N, K, TP, P(K), P(K) - TP
      stop
      end
