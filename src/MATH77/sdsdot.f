      REAL FUNCTION SDSDOT(N,SB,SX,INCX,SY,INCY)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 SDSDOT Krogh   SNGL => REAL.
C>> 1994-11-11 SDSDOT Krogh   Declared all vars.
C>> 1985-08-02 SDSDOT Lawson  Initial code.
C
C     RETURNS S.P. RESULT WITH DOT PRODUCT ACCUMULATED IN D.P.
C     SDSDOT = SB + SUM FOR I = 0 TO N-1 OF SX(LX+I*INCX)*SY(LY+I*INCY),
C     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
C     DEFINED IN A SIMILAR WAY USING INCY.
C
      INTEGER N, INCX, INCY, KX, KY, I, NS
      REAL              SX(*),SY(*),SB
      DOUBLE PRECISION DSDOT
C
      DSDOT = DBLE(SB)
      IF(N .LE. 0) GO TO 30
      IF(INCX.EQ.INCY.AND.INCX.GT.0) GO TO 40
      KX = 1
      KY = 1
      IF(INCX.LT.0) KX = 1+(1-N)*INCX
      IF(INCY.LT.0) KY = 1+(1-N)*INCY
          DO 10 I = 1,N
          DSDOT = DSDOT + DBLE(SX(KX))*DBLE(SY(KY))
          KX = KX + INCX
          KY = KY + INCY
   10     CONTINUE
   30 SDSDOT = REAL(DSDOT)
      RETURN
   40 CONTINUE
      NS = N*INCX
          DO 50 I=1,NS,INCX
          DSDOT = DSDOT + DBLE(SX(I))*DBLE(SY(I))
   50     CONTINUE
      SDSDOT = REAL(DSDOT)
      RETURN
      END
