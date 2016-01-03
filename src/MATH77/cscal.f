      SUBROUTINE CSCAL(N,CA,CX,INCX)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 CSCAL  Krogh   Declared all vars.
C>> 1985-08-02 CSCAL  Lawson  Initial code.
C
C     REPLACE COMPLEX CX BY COMPLEX CA*CX.
C     FOR I = 0 TO N-1, REPLACE CX(1+I*INCX) WITH  CA * CX(1+I*INCX)
C
      INTEGER N, NS, INCX, I
      COMPLEX CA,CX(*)
C
      IF(N .LE. 0) RETURN
      NS = N*INCX
          DO 10 I = 1,NS,INCX
          CX(I) = CA*CX(I)
   10     CONTINUE
      RETURN
      END
