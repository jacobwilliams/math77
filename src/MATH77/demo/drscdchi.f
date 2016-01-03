      program DRSCHI
c>> 2001-05-25 DRSCDCHI Krogh Minor change for making .f90 version.
c>> 1996-06-17 DRSCDCHI Krogh  Minor format change for C conversion.
c>> 1996-05-28 DRSCDCHI Krogh  Changed Fortran 90 code.
c>> 1994-10-19 DRSCDCHI Krogh  Changes to use M77CON
c>> 1994-07-06 DRSCDCHI WVS set up for chgtyp
c
c     Evaluate the Probability Integral Q(chi-square,nu) of the Chi-
c     Square distribution by using SCDCHI.
c
c--S replaces "?": DR?CHI, DR?CDCHI, ?cdchi
      real             CHISQ, NU, P, Q(4)
      integer IERR, J
c
10    format ('     Probability Integral Q(chi**2 | nu)'/
     1 '    CHI**2        NU = 1',13x,'2',13x,'3',13x,'4')
20    format (1p,5g14.7)
      chisq = 0.5e0
      print 10
30    if (chisq .le. 6) then
         do 40 j = 1, 4
            nu = j
            call scdchi (chisq, nu, p, q(j), ierr)
40       continue
         print 20, chisq, q
         chisq = chisq + 0.5e0
         go to 30
      end if
      stop
      end
