      program drsgami
c>>   2001-05-25 DRSGAMI Krogh Minor change for making .f90 version.
c>>   1996-05-28 DRSGAMI Krogh  Changed Fortran 90 code.
c>>   1994-10-19 DRSGAMI Krogh  Changes to use M77CON
c>>   1994-08-15 DRSGAMI WV Snyder JPL set up for chgtyp
c>>   1993-08-03 DRSGAMI WV Snyder JPL Original code
c--S replaces "?": DR?GAMI, ?ERF, ?ERFC, ?GAMI, ?GAMIK
c
c     Demo driver for incomplete gamma function.
c     Evaluate the identity P(1/2,x) = erf(sqrt(x)) and Q(1/2,x) =
c     erfc(sqrt(x)) for several values of x.
c
      real             A, X, P, Q, SERF, SERFC, S, SC, SX
      external SERF, SERFC
      integer I, IND
      real             XS(5)
      data XS /1.0e0, 2.0e0, 3.0e0, 4.0e0, 5.0e0/
c
10    format ('          X         P(1/2,X)       ERF(SQRT(X))  ',
     1        '    Q(1/2,X)      ERFC(SQRT(X))')
20    format (1p,5e16.8)
      print 10
      a = 0.5e0
      ind = 0
      call sgamik (0.0e0, 0.0e0, 0.0e0, 0)
      do 30 i = 1, 5
         x = xs(i)
         call sgami (a, x, p, q, ind)
         sx = sqrt(x)
         s = serf(sx)
         sc = serfc(sx)
         print 20, x, p, s, q, sc
30    continue
      stop
      end
