      program drdgami
c>>   2001-05-25 DRDGAMI Krogh Minor change for making .f90 version.
c>>   1996-05-28 DRDGAMI Krogh  Changed Fortran 90 code.
c>>   1994-10-19 DRDGAMI Krogh  Changes to use M77CON
c>>   1994-08-15 DRDGAMI WV Snyder JPL set up for chgtyp
c>>   1993-08-03 DRDGAMI WV Snyder JPL Original code
c--D replaces "?": DR?GAMI, ?ERF, ?ERFC, ?GAMI, ?GAMIK
c
c     Demo driver for incomplete gamma function.
c     Evaluate the identity P(1/2,x) = erf(sqrt(x)) and Q(1/2,x) =
c     erfc(sqrt(x)) for several values of x.
c
      double precision A, X, P, Q, DERF, DERFC, S, SC, SX
      external DERF, DERFC
      integer I, IND
      double precision XS(5)
      data XS /1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/
c
10    format ('          X         P(1/2,X)       ERF(SQRT(X))  ',
     1        '    Q(1/2,X)      ERFC(SQRT(X))')
20    format (1p,5e16.8)
      print 10
      a = 0.5d0
      ind = 0
      call dgamik (0.0d0, 0.0d0, 0.0d0, 0)
      do 30 i = 1, 5
         x = xs(i)
         call dgami (a, x, p, q, ind)
         sx = sqrt(x)
         s = derf(sx)
         sc = derfc(sx)
         print 20, x, p, s, q, sc
30    continue
      stop
      end
