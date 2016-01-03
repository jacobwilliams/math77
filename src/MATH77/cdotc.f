      COMPLEX FUNCTION CDOTC(N,CX,INCX,CY,INCY)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 CDOTC  Krogh   Declared all vars.
C>> 1985-08-02 CDOTC  Lawson  Initial code.
C
C     RETURNS THE DOT PRODUCT FOR COMPLEX CX AND CY, USES CONJUGATE(CX)
C     CDOTC = SUM FOR I = 0 TO N-1 OF CONJ(CX(LX+I*INCX))*CY(LY+I*INCY),
C     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
C     DEFINED IN A SIMILAR WAY USING INCY.
C
      INTEGER N, INCX, INCY, KX, KY, I, NS
      COMPLEX CX(*),CY(*)
C
      CDOTC = (0.,0.)
      IF(N .LE. 0)RETURN
      IF(INCX.EQ.INCY.AND.INCX.GT.0) GO TO 20
      KX = 1
      KY = 1
      IF(INCX.LT.0) KX = 1+(1-N)*INCX
      IF(INCY.LT.0) KY = 1+(1-N)*INCY
          DO 10 I = 1,N
          CDOTC = CDOTC + CONJG(CX(KX))*CY(KY)
          KX = KX + INCX
          KY = KY + INCY
   10     CONTINUE
      RETURN
   20 CONTINUE
      NS = N*INCX
          DO 30 I=1,NS,INCX
          CDOTC = CONJG(CX(I))*CY(I) + CDOTC
   30     CONTINUE
      RETURN
      END
