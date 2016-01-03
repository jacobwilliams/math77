      double precision function DCOSHM (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 DCOSHM Krogh  Changes to use M77CON
c>> 1993-05-07 DCOSHM WVSnyder JPL Initial code
c
c     Compute cosh(x) - 1 using a rational approximation when abs(x)
c     is less than 1.2, else use the Fortran intrinsic function.
c
c--D replaces "?": ?COSHM
      double precision CP5, CP4, CP3, CP2, CP1, CQ1, X, XS
      parameter ( CP5 = .116744361560051d-08)
      parameter ( CP4 = .280407224259429d-06 )
      parameter ( CP3 = .344417983443219d-04 )
      parameter ( CP2 = .232293648552398d-02 )
      parameter ( CP1 = .778752378267155d-01 )
      parameter ( CQ1 = -.545809550662099d-02*2.0d0)
      xs = x*x
      if (xs .lt. 1.44d0) then
         dcoshm = ((((((cp5*xs+cp4)*xs+cp3)*xs+cp2)*xs+cp1)*xs+1.0d0)
     *                  *xs)/(cq1*xs+2.0d0)
      else
         dcoshm = cosh(x) - 1.0d0
      end if
      return
      end
