      double precision function DSINHM (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>>   1998-10-29 DSINHM Krogh  Moved external statement up for mangle.
c>>   1994-10-20 DSINHM Krogh  Changes to use M77CON
c>>   1994-05-22 DSINHM WV Snyder JPL Make SP and DP alike using CHGTYP
c>>   1993-07-21 DSINHM WV Snyder JPL Original code
c
c     Compute SINH(X) - X.
c
c--D replaces "?": ?SINHM
      double precision X
      external D1MACH
      double precision CUT, D1MACH, E, ROUND, X2
      parameter (CUT = 0.25d0)
      double precision SP5, SP4, SP3, SP2, SP1, SQ1
      parameter ( SP5 = .255251817302048D-09)
      parameter ( SP4 = .723809046696880D-07)
      parameter ( SP3 = .109233297700241D-04)
      parameter ( SP2 = .954811583154274D-03)
      parameter ( SP1 = .452867078563929D-01)
      parameter ( SQ1 =-.471329214363072D-02*6.0D0)
      integer M, N
      save M, ROUND
      data M /-1/
c
      if (m .lt. 0) then
         round = d1mach(4)
         if (round .lt. 5.0d-14) then
c           Compute appropriate value of M depending on round-off.
            m = 3
            e = cut/6.0d0
10          if (e .gt. round) then
               m = m + 2
               e = e*cut*cut/(m*(m-1))
               go to 10
            end if
         end if
      end if
c
      if (round .lt. 5.0d-14) then
         n = m
         x2 = x*x
c        We assume m > 1
         dsinhm = 1.0d0 + x2/(n*(n-1))
20       if (n .gt. 5) then
            n = n - 2
            dsinhm = 1.0d0 + dsinhm*x2/(n*(n-1))
            go to 20
         end if
         dsinhm = x * x2 * dsinhm / 6.0d0
         return
      end if
c
c     Use a rational approximation when ABS(X) is less than 1.65,
c     else use the Fortran intrinsic function.
c
      if (x .lt. 1.65d0) then
         x2 = x*x
         dsinhm = ((((((sp5*x2+sp4)*x2+sp3)*x2+sp2)*x2+sp1)*x2+1.0d0)
     *            *x2*x)/(sq1*x2+6.0d0)
      else
         dsinhm = sinh(x) - x
      end if
      return
      end
