      program DRDNML
c>> 2001-05-25 DRDCDNML Krogh Added comma to format.
c>> 1996-05-28 DRDCDNML Krogh  Changes to use M77CON
c>> 1994-07-06 DRDCDNML WV Snyder JPL set up for CHGTYP
c>> 1994-04-12 DRDCDNML WV Snyder JPL repair format to display sign
c
c     Evaluate the Cumulative Normal Distribution using DCDNML.
c
c--D replaces "?": DR?NML, DR?CDNML, ?CDNML, ?PPNML
      double precision DCDNML, DPPNML
      external DCDNML, DPPNML
      double precision X, C, P, MU, SIGMA
c
10    format ('        X       C=DCDNML(X)     DPPNML(C)')
20    format (1x,1p,2g14.7,2x,g14.7)
      x = -4.0d0
      mu = 0.0d0
      sigma = 1.0d0
      print 10
30    if (x .le. 4.0d0) then
         c = dcdnml(x,mu,sigma)
         p = dppnml(c,mu,sigma)
         print 20, x, c, p
         x = x + 0.5d0
         go to 30
      end if
      stop
      end
