      double precision function DCSHMM (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 DCSHMM Krogh  Changes to use M77CON
c>> 1993-05-07 DCSHMM WVSnyder JPL Initial code
c
c     Compute cosh(x) - 1 - x**2 using a rational approximation when
c     abs(x) is less than 2.7, else use the Fortran intrinsic function.
c
c--D replaces "?": ?CSHMM
      double precision ZP3, ZP2, ZP1, ZQ4, ZQ3, ZQ2, ZQ1, X, XS
      parameter ( ZP3 = 5.59297116264720D-07 )
      parameter ( ZP2 = 1.77943488030894D-04 )
      parameter ( ZP1 = 1.69800461894792D-02 )
      parameter ( ZQ4 = 1.33412535492375D-09 * 24.0d0 )
      parameter ( ZQ3 = -5.80858944138663D-07 * 24.0d0 )
      parameter ( ZQ2 = 1.27814964403863D-04 * 24.0d0 )
      parameter ( ZQ1 = -1.63532871439181D-02 * 24.0d0 )
c     DATA ZP3/5.59297116264720D-07/,
c    *     ZP2/1.77943488030894D-04/,
c    *     ZP1/1.69800461894792D-02/,
c    *     ZQ4/1.33412535492375D-09/,
c    *     ZQ3/-5.80858944138663D-07/,
c    *     ZQ2/1.27814964403863D-04/,
c    *     ZQ1/-1.63532871439181D-02/
      xs = x * x
      if (xs .lt. 7.29d0) then
         dcshmm = ((((zp3*xs+zp2)*xs+zp1)*xs+1.0d0)*xs*xs)/
     *            ((((zq4*xs+zq3)*xs+zq2)*xs+zq1)*xs+24.0d0)
      else
         dcshmm = cosh(x) - 1.0d0 - 0.5d0*xs
      end if
      return
      end
