c     program DRSBINOM
c>> 1998-05-12 DRSBINOM Krogh Ensure Pascal print output.
c>> 1995-12-15 DRDBINUM Krogh Initial Code.
c--S replaces "?": DR?BINOM, ?BINOM
c
c Test SBINOM for computing binomial coefficients.
c Checks a large value computed with factorials, one a little larger
c that is computed using log gamma, and then finds first that differs
c from that obtained using the Pascal triangle.
c
      integer K, N, NMAX
      parameter (NMAX = 120)
      external SBINOM
      real             BC1, BC2, P(0:NMAX), TP, SBINOM
c            ( 150 )            ( 151 )
c      BC1 = (     )      BC2 = (     )
c            (  30 )            (  29 )
      parameter (BC1 = 32198785340494567031466236484400.E0)
      parameter (BC2 = 9880808670399701168713050486000.E0)
c
   10 format (/'  N    K',12X,'SBINOM',20X, A, 8X, ' Col. 2 - Col. 1')
   20 format (I4, I4,1P,E26.17,E26.17,1P,E16.7)
c
      print 10, ' True '
      TP =  SBINOM(150, 30)
      print 20, 150, 30, TP, BC1, BC1 - TP
      TP =  SBINOM(151, 29)
      print 20, 151, 29, TP, BC2, BC2 - TP
c
      print 10, 'Pascal'
      P(0) = 1.E0
      do 80 N = 0, NMAX
         do 40 K = 0, N
            TP = SBINOM(N, K)
            if (TP .ne. P(K)) go to 100
   40    continue
c                  Update the Pascal Triangle.
         P(N+1) = 1.E0
         do 60 K = N, 1, -1
            P(K) = P(K) + P(K-1)
   60    continue
   80 continue
  100 print 20, N, K, TP, P(K), P(K) - TP
      stop
      end
