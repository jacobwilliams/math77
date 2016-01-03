      real             function SSIN1 (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>>   1998-10-29 SSIN1 Krogh  Moved external statement up for mangle.
c>>   1994-10-20 SSIN1 Krogh  Changes to use M77CON
c>>   1993-07-21 SSIN1 WV Snyder JPL Original code
c
c     Compute (X - SIN(X)) / X**2.
c     Use Taylor's series when |X| < CUT (an internal parameter).
c     Use the intrinsic function otherwise.
c
c--S replaces "?": ?SIN1
      real             X
      external R1MACH
      real             CUT, R1MACH, ROUND, S, X2
      parameter (CUT=0.125e0)
      integer M, N
      save N
      data N /-1/
c
      if (n .lt. 0) then
         round = r1mach(4)
         n = 1
         s = 1.0e0
10       if (s .gt. round) then
            s = s * cut * cut / ((2*n)*(2*n+1))
            n = n + 1
            go to 10
         end if
      end if
      if (abs(x) .lt. cut) then
         x2 = x*x
         m = 2*n
         s = 1.0e0 - x2/(m*(m+1))
20       if (m .gt. 4) then
            m = m - 2
            s = 1.0e0 - s*x2/(m*(m+1))
            go to 20
         end if
         ssin1 = s/6.0e0
      else
         ssin1 = (x - sin(x)) / x**3
      end if
      return
      end
