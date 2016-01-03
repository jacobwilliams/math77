      DOUBLE PRECISION FUNCTION DSDOT(N,SX,INCX,SY,INCY)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 DSDOT  Krogh   Declared all vars.
C>> 1985-08-02 DSDOT  Lawson  Initial code.
C
C     RETURNS D.P. DOT PRODUCT ACCUMULATED IN D.P., FOR S.P. SX AND SY
C     DSDOT = SUM FOR I = 0 TO N-1 OF  SX(LX+I*INCX) * SY(LY+I*INCY),
C     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
C     DEFINED IN A SIMILAR WAY USING INCY.
C
      INTEGER N, INCX, INCY, KX, KY, I, NS
      REAL SX(*),SY(*)
C
      DSDOT = 0.D0
      IF(N .LE. 0)RETURN
      IF(INCX.EQ.INCY.AND.INCX.GT.0) GO TO 20
      KX = 1
      KY = 1
      IF(INCX.LT.0) KX = 1+(1-N)*INCX
      IF(INCY.LT.0) KY = 1+(1-N)*INCY
          DO 10 I = 1,N
          DSDOT = DSDOT + DBLE(SX(KX))*DBLE(SY(KY))
          KX = KX + INCX
          KY = KY + INCY
   10 CONTINUE
      RETURN
   20 CONTINUE
      NS = N*INCX
          DO 30 I=1,NS,INCX
          DSDOT = DSDOT + DBLE(SX(I))*DBLE(SY(I))
   30     CONTINUE
      RETURN
      END
