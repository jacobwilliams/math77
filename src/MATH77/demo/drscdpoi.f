      program DRSPOI
c>> 2001-05-25 DRSCDPOI Krogh Minor change for making .f90 version.
c>> 1996-05-28 DRSCDPOI Krogh  Changed Fortran 90 code.
c>> 1994-10-19 DRSCDPOI Krogh  Changes to use M77CON
c>> 1994-07-06 DRSCDPOI WVS set up for chgtyp
c
c     Evaluate the Probability Function Q(n,lamda) of the Poisson
c     distribution by using SCDPOI.
c
c--S replaces "?": DR?POI, DR?CDPOI, ?cdpoi, ?gamik
      real             LAMDAS(4), P, Q(4)
      integer J, N, IERR
      data LAMDAS /0.5e0, 1.0e0, 1.5e0, 2.0e0/
c
10    format ('     Poisson Probability Function Q(lamda | n)'/
     1        '  N    LAMDA =',4f14.7/)
30    format (i3,15x,1p,4g14.7)
      call sgamik (0.0e0, 0.0e0, 0.0e0, 0)
      print 10, lamdas
      do 50 n = 1, 10
         do 40  j = 1, 4
            call scdpoi (n, lamdas(j), p, q(j), ierr)
40       continue
         print 30, n, q
50    continue
      stop
      end
