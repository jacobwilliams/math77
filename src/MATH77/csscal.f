      SUBROUTINE CSSCAL(N,SA,CX,INCX)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 CSSCAL  Krogh   Declared all vars.
C>> 1985-08-02 CSSCAL Lawson  Initial code.
C
C     REPLACE COMPLEX CX BY (SINGLE PRECISION SA) * (COMPLEX CX)
C     FOR I = 0 TO N-1, REPLACE CX(1+I*INCX) WITH  SA * CX(1+I*INCX)
C
      INTEGER N, NS, INCX, I
      COMPLEX CX(*)
      REAL    SA
C
      IF(N .LE. 0) RETURN
      NS = N*INCX
          DO 10 I = 1,NS,INCX
          CX(I) = SA*CX(I)
   10     CONTINUE
      RETURN
      END
