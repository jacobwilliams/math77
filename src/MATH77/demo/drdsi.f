      program DRDSI
c>> 1994-10-19 DRDSI   Krogh  Changes to use M77CON
c>> 1991-11-19  DRDSI   CLL
c>> 1989-04-26  DRDSI   Snyder at JPL, Original Code
c--D replaces "?":  DR?SI, ?CI, ?CIN, ?SI
c
c     Demonstration driver for Sine and Cosine integrals.
c
      double precision DCI, DCIN, DSI, X
      integer I
      external DCI, DCIN, DSI
c
c     Print values of Si(x), Ci(x) and Cin(x)
c     for X = 1.0 (1.0) 15.0
c
      print '(''  X'',11x,''Si(x)'',11x,''Ci(x)'',11x,''Cin(x)''/)'
      do 10 i = 1, 15
         x = dble(i)
         print '(f4.0,3f16.7)', x, dsi(x), dci(x), dcin(x)
   10 continue
      stop
      end
