      SUBROUTINE DCOPY (N,X,INCX,Y,INCY)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2006-06-07 DCOPY  Krogh   Removed arithmetic ifs
C>> 1994-11-11 DCOPY  Krogh   Declared all vars.
C>> 1994-10-20 DCOPY  Krogh   Changes to use M77CON
C>> 1989-05-12 DCOPY  Snyder  Clean up a little for F77
C>> 1985-08-02 DCOPY  Lawson  Initial code.
c--D replaces "?": ?COPY
C
C     COPY X TO Y.
C     FOR I = 0 TO N-1, COPY X(LX+I*INCX) TO Y(LY+I*INCY),
C     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
C     DEFINED IN A SIMILAR WAY USING INCY.
C
      INTEGER N, INCX, INCY, IX, IY, I, M, NS
      DOUBLE PRECISION X(*),Y(*)
C
      IF(N.LE.0)RETURN
      IF ((INCX .NE. INCY) .OR. (INCX .LT. 0)) THEN
C        CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
        IX = 1
        IY = 1
        IF(INCX.LT.0)IX = (-N+1)*INCX + 1
        IF(INCY.LT.0)IY = (-N+1)*INCY + 1
        DO 10 I = 1,N
          Y(IY) = X(IX)
          IX = IX + INCX
          IY = IY + INCY
 10     CONTINUE
      ELSE IF (INCX .EQ. 1) THEN
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 8.
        M = MOD(N,8)
        DO 30 I = 1,M
          Y(I) = X(I)
 30     CONTINUE
        DO 50 I = M+1,N,8
          Y(I)     = X(I)
          Y(I + 1) = X(I + 1)
          Y(I + 2) = X(I + 2)
          Y(I + 3) = X(I + 3)
          Y(I + 4) = X(I + 4)
          Y(I + 5) = X(I + 5)
          Y(I + 6) = X(I + 6)
          Y(I + 7) = X(I + 7)
 50     CONTINUE
      ELSE
C        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
        NS = N*INCX
        DO 70 I=1,NS,INCX
          Y(I) = X(I)
 70     CONTINUE
      END IF
      RETURN
      END
