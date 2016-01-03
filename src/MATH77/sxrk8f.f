      subroutine SXRK8F(T, Y, F, IDAT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1997-12-18 SXRK8F  Krogh  Initial code.
c--S replaces "?": ?XRK8F
c
c  Dummy routine to print message if user has not setup correctly.
c  First two executable statements are for constructing prototypes.
c
      real             T, Y(*), F(*)
      integer IDAT(*)
      F(1) = T * Y(1)
      T = 0.E0
      PRINT *,'You have neglected either to write the SXRK8F subprogram'
      PRINT *,'or to select reverse communication before invoking DXRK8'
      stop
      end

