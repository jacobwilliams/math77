      program DRSSI
c>> 1994-10-19 DRSSI   Krogh  Changes to use M77CON
c>> 1991-11-19  DRSSI   CLL
c>> 1989-04-26  DRSSI   Snyder at JPL, Original Code
c--S replaces "?":  DR?SI, ?CI, ?CIN, ?SI
c
c     Demonstration driver for Sine and Cosine integrals.
c
      real             SCI, SCIN, SSI, X
      integer I
      external SCI, SCIN, SSI
c
c     Print values of Si(x), Ci(x) and Cin(x)
c     for X = 1.0 (1.0) 15.0
c
      print '(''  X'',11x,''Si(x)'',11x,''Ci(x)'',11x,''Cin(x)''/)'
      do 10 i = 1, 15
         x = real(i)
         print '(f4.0,3f16.7)', x, ssi(x), sci(x), scin(x)
   10 continue
      stop
      end
