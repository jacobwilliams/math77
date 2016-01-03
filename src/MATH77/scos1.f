      real             function SCOS1 (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>>   1994-10-20 SCOS1 Krogh  Changes to use M77CON
c>>   1993-07-21 SCOS1 WV Snyder JPL Original code
c
c     compute (1 - cos(x)) / x**2.
c
c--S replaces "?": ?COS1
c
      real             X
      real             C, SINC
c
      if (x .eq. 0.0e0) then
         scos1 = 0.5e0
      else
         c = cos(x)
         if (c .le. 0.0) then
            scos1 = (1.0e0 - c ) / x*x
         else
            sinc = sin(x) / x
            scos1 = sinc**2 / (1.0e0 + c)
         end if
      end if
      return
      end
