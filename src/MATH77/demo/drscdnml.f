      program DRSNML
c>> 2001-05-25 DRSCDNML Krogh Added comma to format.
c>> 1996-05-28 DRSCDNML Krogh  Changes to use M77CON
c>> 1994-07-06 DRSCDNML WV Snyder JPL set up for CHGTYP
c>> 1994-04-12 DRSCDNML WV Snyder JPL repair format to display sign
c
c     Evaluate the Cumulative Normal Distribution using SCDNML.
c
c--S replaces "?": DR?NML, DR?CDNML, ?CDNML, ?PPNML
      real             SCDNML, SPPNML
      external SCDNML, SPPNML
      real             X, C, P, MU, SIGMA
c
10    format ('        X       C=SCDNML(X)     SPPNML(C)')
20    format (1x,1p,2g14.7,2x,g14.7)
      x = -4.0e0
      mu = 0.0e0
      sigma = 1.0e0
      print 10
30    if (x .le. 4.0e0) then
         c = scdnml(x,mu,sigma)
         p = sppnml(c,mu,sigma)
         print 20, x, c, p
         x = x + 0.5e0
         go to 30
      end if
      stop
      end
