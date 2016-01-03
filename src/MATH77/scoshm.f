      real             function SCOSHM (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 SCOSHM Krogh  Changes to use M77CON
c>> 1993-05-07 SCOSHM WVSnyder JPL Initial code
c
c     Compute cosh(x) - 1 using a rational approximation when abs(x)
c     is less than 1.2, else use the Fortran intrinsic function.
c
c--S replaces "?": ?COSHM
      real             CP5, CP4, CP3, CP2, CP1, CQ1, X, XS
      parameter ( CP5 = .116744361560051e-08)
      parameter ( CP4 = .280407224259429e-06 )
      parameter ( CP3 = .344417983443219e-04 )
      parameter ( CP2 = .232293648552398e-02 )
      parameter ( CP1 = .778752378267155e-01 )
      parameter ( CQ1 = -.545809550662099e-02*2.0e0)
      xs = x*x
      if (xs .lt. 1.44e0) then
         scoshm = ((((((cp5*xs+cp4)*xs+cp3)*xs+cp2)*xs+cp1)*xs+1.0e0)
     *                  *xs)/(cq1*xs+2.0e0)
      else
         scoshm = cosh(x) - 1.0e0
      end if
      return
      end
