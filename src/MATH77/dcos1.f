      double precision function DCOS1 (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>>   1994-10-20 DCOS1 Krogh  Changes to use M77CON
c>>   1993-07-21 DCOS1 WV Snyder JPL Original code
c
c     compute (1 - cos(x)) / x**2.
c
c--D replaces "?": ?COS1
c
      double precision X
      double precision C, SINC
c
      if (x .eq. 0.0d0) then
         dcos1 = 0.5d0
      else
         c = cos(x)
         if (c .le. 0.0) then
            dcos1 = (1.0d0 - c ) / x*x
         else
            sinc = sin(x) / x
            dcos1 = sinc**2 / (1.0d0 + c)
         end if
      end if
      return
      end
