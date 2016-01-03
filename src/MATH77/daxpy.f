      SUBROUTINE DAXPY(N,A,X,INCX,Y,INCY)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2006-06-07 DAXPY  Krogh  Removed arithmetic if
C>> 1994-11-11 DAXPY  Krogh  Declared all vars.
C>> 1994-10-20 DAXPY  Krogh  Changes to use M77CON
C>> 1985-08-02 DAXPY  Lawson Initial code.
c--D replaces "?": ?AXPY
C
C     OVERWRITE Y WITH A*X + Y.
C     FOR I = 0 TO N-1, REPLACE  Y(LY+I*INCY) WITH A*X(LX+I*INCX) +
C       Y(LY+I*INCY), WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N,
C       AND LY IS DEFINED IN A SIMILAR WAY USING INCY.
C
      INTEGER N, INCX, INCY, IX, IY, I, M, MP1, NS
      DOUBLE PRECISION X(*),Y(*),A
      IF(N.LE.0.OR.A.EQ.0.D0) RETURN
      IF ((INCX .NE. INCY) .OR. (INCX .LT. 0)) THEN
C        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS.
        IX = 1
        IY = 1
        IF(INCX.LT.0)IX = (-N+1)*INCX + 1
        IF(INCY.LT.0)IY = (-N+1)*INCY + 1
        DO 10 I = 1,N
          Y(IY) = Y(IY) + A*X(IX)
          IX = IX + INCX
          IY = IY + INCY
 10     CONTINUE
      ELSE IF (INCX .EQ. 1) THEN
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4.
        M = MOD(N,4)
        IF( M .EQ. 0 ) GO TO 40
        DO 30 I = 1,M
          Y(I) = Y(I) + A*X(I)
 30     CONTINUE
        IF( N .LT. 4 ) RETURN
 40     MP1 = M + 1
        DO 50 I = MP1,N,4
          Y(I) = Y(I) + A*X(I)
          Y(I + 1) = Y(I + 1) + A*X(I + 1)
          Y(I + 2) = Y(I + 2) + A*X(I + 2)
          Y(I + 3) = Y(I + 3) + A*X(I + 3)
 50     CONTINUE
      ELSE
C        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
        NS = N*INCX
        DO 70 I=1,NS,INCX
          Y(I) = A*X(I) + Y(I)
 70     CONTINUE
      END IF
      RETURN
      END
