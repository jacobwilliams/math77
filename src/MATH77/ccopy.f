      SUBROUTINE CCOPY (N,CX,INCX,CY,INCY)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 CCOPY  Krogh   Declared all vars.
C>> 1989-05-12 CCOPY  Snyder  Clean up a little for F77
C>> 1985-08-02 CCOPY  Lawson  Initial code.
C
C     COPY COMPLEX CX TO COMPLEX CY.
C     FOR I = 0 TO N-1, COPY CX(LX+I*INCX) TO CY(LY+I*INCY),
C     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
C     DEFINED IN A SIMILAR WAY USING INCY.
C
      INTEGER N, INCX, INCY, KX, KY, I, NS
      COMPLEX CX(*),CY(*)
C
      IF(N .LE. 0)RETURN
      IF(INCX.NE.INCY .OR. INCX.LE.0) THEN
         KX = 1
         KY = 1
         IF(INCX.LT.0) KX = 1+(1-N)*INCX
         IF(INCY.LT.0) KY = 1+(1-N)*INCY
         DO 10 I = 1,N
            CY(KY) = CX(KX)
            KX = KX + INCX
            KY = KY + INCY
   10       CONTINUE
      ELSE
         NS = N*INCX
         DO 30 I=1,NS,INCX
            CY(I) = CX(I)
   30       CONTINUE
      END IF
      RETURN
      END
