      SUBROUTINE CSWAP(N,CX,INCX,CY,INCY)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 CSWAP  Krogh   Declared all vars.
C>> 1985-08-02 CSWAP  Lawson  Initial code.
C
C     INTERCHANGE COMPLEX CX AND COMPLEX CY
C     FOR I = 0 TO N-1, INTERCHANGE  CX(LX+I*INCX) AND CY(LY+I*INCY),
C     WHERE LX = 1 IF INCX .GT. 0, ELSE LX = (-INCX)*N, AND LY IS
C     DEFINED IN A SIMILAR WAY USING INCY.
C
      INTEGER N, INCX, INCY, KX, KY, I, NS
      COMPLEX CX(*),CY(*),CTEMP
C
      IF(N .LE. 0)RETURN
      IF(INCX.EQ.INCY.AND.INCX.GT.0) GO TO 20
      KX = 1
      KY = 1
      IF(INCX.LT.0) KX = 1+(1-N)*INCX
      IF(INCY.LT.0) KY = 1+(1-N)*INCY
          DO 10 I = 1,N
          CTEMP = CX(KX)
          CX(KX) = CY(KY)
          CY(KY) = CTEMP
          KX = KX + INCX
          KY = KY + INCY
   10 CONTINUE
      RETURN
   20 CONTINUE
      NS = N*INCX
          DO 30 I=1,NS,INCX
          CTEMP = CX(I)
          CX(I) = CY(I)
          CY(I) = CTEMP
   30     CONTINUE
      RETURN
      END
