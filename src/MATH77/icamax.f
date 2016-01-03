      INTEGER FUNCTION ICAMAX(N,CX,INCX)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 CLL Typing all variables.
C>> 1985-08-02 ICAMAX Lawson  Initial code.
C
C      RETURNS THE INDEX OF THE COMPONENT OF CX HAVING THE
C      LARGEST SUM OF MAGNITUDES OF REAL AND IMAGINARY PARTS.
C     ICAMAX = FIRST I, I = 1 TO N, TO MINIMIZE
C        ABS(REAL(CX(1-INCX+I*INCX))) + ABS(IMAG(CX(1-INCX+I*INCX)))
C
      integer I, II, INCX, N
      real SUMMAX, SUMRI
      COMPLEX CX(*)
C
      ICAMAX = 0
      IF(N.LE.0) RETURN
      ICAMAX = 1
      IF(N .LE. 1) RETURN
      II = 1
      SUMMAX = ABS(REAL(CX(1))) + ABS(AIMAG(CX(1)))
      DO 20 I=1,N*INCX,INCX
         SUMRI = ABS(REAL(CX(I))) + ABS(AIMAG(CX(I)))
         IF(SUMMAX .lt. SUMRI) then
            SUMMAX = SUMRI
            ICAMAX = II
         endif
         II = II + 1
   20 CONTINUE
      RETURN
      END
