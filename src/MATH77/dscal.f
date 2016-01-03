      SUBROUTINE DSCAL(N,A,X,INCX)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 DSCAL  Krogh   Declared all vars.
C>> 1994-10-20 DSCAL  Krogh  Changes to use M77CON
C>> 1985-08-02 DSCAL  Lawson  Initial code.
c--D replaces "?": ?SCAL
C
C     REPLACE X BY  A*X.
C     FOR I = 0 TO N-1, REPLACE X(1+I*INCX) WITH  A * X(1+I*INCX)
C
      INTEGER N, INCX, NS, I, M, MP1
      DOUBLE PRECISION A,X(*)
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      NS = N*INCX
          DO 10 I = 1,NS,INCX
          X(I) = A*X(I)
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        X(I) = A*X(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        X(I) = A*X(I)
        X(I + 1) = A*X(I + 1)
        X(I + 2) = A*X(I + 2)
        X(I + 3) = A*X(I + 3)
        X(I + 4) = A*X(I + 4)
   50 CONTINUE
      RETURN
      END
