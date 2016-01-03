      REAL FUNCTION SCASUM(N,CX,INCX)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 CLL Typing all variables.
C>> 1985-08-02 SCASUM Lawson  Initial code.
C     RETURNS SUMS OF MAGNITUDES OF REAL AND IMAGINARY PARTS OF
C     COMPONENTS OF CX.  NOTE THAT THIS IS NOT THE L1 NORM OF CX.
C     CASUM = SUM FROM 0 TO N-1 OF ABS(REAL(CX(1+I*INCX))) +
C             ABS(IMAG(CX(1+I*INCX)))
C
      integer I, INCX, N
      COMPLEX CX(*)
C
      SCASUM=0.0e0
      IF(N .LE. 0) RETURN
         DO 10 I=1,N*INCX,INCX
            SCASUM = SCASUM + ABS(REAL(CX(I))) + ABS(AIMAG(CX(I)))
   10    CONTINUE
      RETURN
      END
