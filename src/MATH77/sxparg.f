      REAL             FUNCTION SXPARG (L)
      INTEGER L
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 SXPARG Krogh  Moved external statement up for mangle.
C>> 1994-10-20 SXPARG Krogh  Changes to use M77CON
C>> 1993-05-06 SXPARG WVS JPL Conversion from NSWC to Math 77
c--S replaces "?": ?XPARG
C -------------------------------------------------------------------
C     IF L = 0 THEN  SXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
C     EXP(W) CAN BE COMPUTED.
C
C     IF L IS NONZERO THEN  SXPARG(L) = THE LARGEST NEGATIVE W FOR
C     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO.
C
C     NOTE... ONLY AN APPROXIMATE VALUE FOR SXPARG(L) IS NEEDED.
C -------------------------------------------------------------------
      EXTERNAL R1MACH
      REAL             R1MACH
      REAL             FAC, XMAX, XMIN
      SAVE XMAX, XMIN
      DATA XMAX /-1.0E0/
C
      IF (XMAX .LT. 0.0) THEN
         FAC = 1.0E0 - R1MACH(3)
         XMAX = FAC * LOG(R1MACH(2))
         XMIN = FAC * LOG(R1MACH(1))
      END IF
C
      IF (L .NE. 0) THEN
         SXPARG = XMIN
      ELSE
         SXPARG = XMAX
      END IF
      RETURN
      END
