      DOUBLE PRECISION FUNCTION DXPARG (L)
      INTEGER L
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 DXPARG Krogh  Moved external statement up for mangle.
C>> 1994-10-20 DXPARG Krogh  Changes to use M77CON
C>> 1993-05-06 DXPARG WVS JPL Conversion from NSWC to Math 77
c--D replaces "?": ?XPARG
C -------------------------------------------------------------------
C     IF L = 0 THEN  DXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
C     EXP(W) CAN BE COMPUTED.
C
C     IF L IS NONZERO THEN  DXPARG(L) = THE LARGEST NEGATIVE W FOR
C     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO.
C
C     NOTE... ONLY AN APPROXIMATE VALUE FOR DXPARG(L) IS NEEDED.
C -------------------------------------------------------------------
      EXTERNAL D1MACH
      DOUBLE PRECISION D1MACH
      DOUBLE PRECISION FAC, XMAX, XMIN
      SAVE XMAX, XMIN
      DATA XMAX /-1.0D0/
C
      IF (XMAX .LT. 0.0) THEN
         FAC = 1.0D0 - D1MACH(3)
         XMAX = FAC * LOG(D1MACH(2))
         XMIN = FAC * LOG(D1MACH(1))
      END IF
C
      IF (L .NE. 0) THEN
         DXPARG = XMIN
      ELSE
         DXPARG = XMAX
      END IF
      RETURN
      END
