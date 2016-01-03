      SUBROUTINE CAXPY(N,CA,CX,INCX,CY,INCY)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 CLL Typing all variables.
C>> 1987-10-19 CAXPY  Lawson  Initial code.
C
C     OVERWRITE COMPLEX CY WITH COMPLEX  CA*CX + CY.
C     FOR I = 0 TO N-1, REPLACE  CY(LY+I*INCY) WITH CA*CX(LX+I*INCX) +
C       CY(LY+I*INCY), WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N,
C       AND LY IS DEFINED IN A SIMILAR WAY USING INCY.
C
      integer i, n, incx, incy, kx, ky
      real canorm
      COMPLEX CX(*),CY(*),CA
C
      CANORM = ABS(REAL(CA)) + ABS(AIMAG(CA))
      IF(N.LE.0.OR.CANORM.EQ.0.E0) RETURN
      IF(INCX.EQ.INCY.AND.INCX.GT.0) GO TO 20
      KX = 1
      KY = 1
      IF(INCX.LT.0) KX = 1+(1-N)*INCX
      IF(INCY.LT.0) KY = 1+(1-N)*INCY
          DO 10 I = 1,N
          CY(KY) = CY(KY) + CA*CX(KX)
          KX = KX + INCX
          KY = KY + INCY
   10 CONTINUE
      RETURN
   20 CONTINUE
          DO 30 I=1,N*INCX,INCX
          CY(I) = CA*CX(I) + CY(I)
   30     CONTINUE
      RETURN
      END
