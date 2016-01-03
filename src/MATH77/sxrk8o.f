      subroutine SXRK8O(TS, Y, IDAT, DAT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1997-12-18 SXRK8O  Krogh  Initial code.
c--S replaces "?": ?XRK8O
c
c  Dummy routine to print message if user has not setup correctly.
c
      real             TS(*), Y(*), DAT(*)
      integer IDAT(*)
      PRINT *,'You have neglected either to write the SXRK8O subprogram'
      PRINT *,'or to select reverse communication before invoking DXRK8'
      stop
      end

