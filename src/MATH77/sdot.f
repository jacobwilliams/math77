      REAL             FUNCTION SDOT(N,X,INCX,Y,INCY)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2006-06-07 SDOT   Krogh  Removed arithmetic if
C>> 1994-11-11 SDOT   Krogh  Declared all vars.
c>> 1994-10-20 SDOT   Krogh  Changes to use M77CON
c>> 1994-04-19 SDOT   Krogh  Minor -- Made code versions line up.
C>> 1985-08-02 SDOT   Lawson Initial code.
c--S replaces "?": ?DOT
C
C     RETURNS THE DOT PRODUCT OF X AND Y.
C     SDOT = SUM FOR I = 0 TO N-1 OF  X(LX+I*INCX) * Y(LY+I*INCY),
C     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
C     DEFINED IN A SIMILAR WAY USING INCY.
C
      INTEGER N, INCX, INCY, IX, IY, I, M, MP1, NS
      REAL             X(*),Y(*)
      SDOT = 0.0E0
      IF(N.LE.0)RETURN
      IF ((INCX .NE. INCY) .OR. (INCX .LT. 0)) THEN
C         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
        IX = 1
        IY = 1
        IF(INCX.LT.0)IX = (-N+1)*INCX + 1
        IF(INCY.LT.0)IY = (-N+1)*INCY + 1
        DO 10 I = 1,N
          SDOT = SDOT + X(IX)*Y(IY)
          IX = IX + INCX
          IY = IY + INCY
 10     CONTINUE
      ELSE IF (INCX .EQ. 1) THEN
C        CODE FOR BOTH INCREMENTS EQUAL TO 1.
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
        M = MOD(N,5)
        IF( M .EQ. 0 ) GO TO 40
        DO 30 I = 1,M
          SDOT = SDOT + X(I)*Y(I)
 30     CONTINUE
        IF( N .LT. 5 ) RETURN
 40     MP1 = M + 1
        DO 50 I = MP1,N,5
          SDOT = SDOT + X(I)*Y(I) + X(I+1)*Y(I+1) +
     $      X(I + 2)*Y(I + 2) + X(I + 3)*Y(I + 3) + X(I + 4)*Y(I + 4)
 50     CONTINUE
      ELSE
C         CODE FOR POSITIVE EQUAL INCREMENTS .NE.1.
        NS = N*INCX
        DO 70 I=1,NS,INCX
          SDOT = SDOT + X(I)*Y(I)
 70     CONTINUE
      END IF
      RETURN
      END
