      INTEGER FUNCTION ISAMAX(N,X,INCX)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 ISAMAX  Krogh   Declared all vars.
C>> 1994-10-20 ISAMAX Krogh  Changes to use M77CON
C>> 1994-04-19 ISAMAX Krogh   Conveted to use generic intrinsics.
C>> 1987-12-09 ISAMAX Lawson  Initial code.
c--S replaces "?": I?AMAX
C
C     FIND SMALLEST INDEX OF MAXIMUM MAGNITUDE OF X.
C     ISAMAX =  FIRST I, I = 1 TO N, TO MINIMIZE  ABS(X(1-INCX+I*INCX))
C
      INTEGER N, INCX, NS, II, I
      REAL             X(*),XMAX,XMAG
      ISAMAX = 0
      IF(N.LE.0) RETURN
      ISAMAX = 1
      IF(N.LE.1)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      XMAX = ABS(X(1))
      NS = N*INCX
      II = 1
          DO 10 I = 1,NS,INCX
          XMAG = ABS(X(I))
          IF(XMAG.LE.XMAX) GO TO 5
          ISAMAX = II
          XMAX = XMAG
    5     II = II + 1
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
   20 XMAX = ABS(X(1))
      DO 30 I = 2,N
          XMAG = ABS(X(I))
          IF(XMAG.LE.XMAX) GO TO 30
          ISAMAX = I
          XMAX = XMAG
   30 CONTINUE
      RETURN
      END
