      REAL             FUNCTION SASUM(N,X,INCX)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 SASUM  Krogh  Declared all vars.
c>> 1994-10-20 SASUM  Krogh  Changes to use M77CON
c>> 1994-04-19 SASUM  Krogh  Minor -- Made diff. precision line up.
C>> 1985-08-02 SASUM  Lawson Initial code.
c--S replaces "?": ?ASUM
C
C     RETURNS SUM OF MAGNITUDES OF X.
C     SASUM = SUM FROM 0 TO N-1 OF ABS(X(1+I*INCX))
C
      INTEGER N, INCX, NS, I, M, MP1
      REAL             X(*)
      SASUM = 0.0E0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      NS = N*INCX
          DO 10 I=1,NS,INCX
          SASUM = SASUM + ABS(X(I))
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 6.
C
   20 M = MOD(N,6)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
         SASUM = SASUM + ABS(X(I))
   30 CONTINUE
      IF( N .LT. 6 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
         SASUM = SASUM + ABS(X(I)) + ABS(X(I + 1)) + ABS(X(I + 2))
     $   + ABS(X(I + 3)) + ABS(X(I + 4)) + ABS(X(I + 5))
   50 CONTINUE
      RETURN
      END
