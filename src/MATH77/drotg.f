      SUBROUTINE DROTG(A,B,C,S)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-10-20 DROTG  Krogh  Changes to use M77CON
C>> 1994-04-19 DROTG  Krogh   Changed to use generic intrinsics.
C>> 1985-08-02 DROTG  Lawson  Initial code.
c--D replaces "?": ?ROTG, ?ROT
C
C     DESIGNED BY C.L.LAWSON, JPL, 1977 SEPT 08
C
C
C     CONSTRUCT THE GIVENS TRANSFORMATION
C
C         ( C  S )
C     G = (      ) ,    C**2 + S**2 = 1 ,
C         (-S  C )
C
C     WHICH ZEROS THE SECOND ENTRY OF THE 2-VECTOR  (A,B)**T .
C
C     THE QUANTITY R = (+/-)SQRT(A**2 + B**2) OVERWRITES A IN
C     STORAGE.  THE VALUE OF B IS OVERWRITTEN BY A VALUE Z WHICH
C     ALLOWS C AND S TO BE RECOVERED BY THE FOLLOWING ALGORITHM:
C           IF Z=1  SET  C=0.D0  AND  S=1.D0
C           IF ABS(Z) .LT. 1  SET  C=SQRT(1-Z**2)  AND  S=Z
C           IF ABS(Z) .GT. 1  SET  C=1/Z  AND  S=SQRT(1-C**2)
C
C     NORMALLY, THE SUBPROGRAM DROT(N,X,INCX,Y,INCY,C,S) WILL
C     NEXT BE CALLED TO APPLY THE TRANSFORMATION TO A 2 BY N MATRIX.
C
C ------------------------------------------------------------------
C
      DOUBLE PRECISION  A, B, C, S, U, V, R
      IF (ABS(A) .LE. ABS(B)) GO TO 10
C
C *** HERE ABS(A) .GT. ABS(B) ***
C
      U = A + A
      V = B / U
C
C     NOTE THAT U AND R HAVE THE SIGN OF A
C
      R = SQRT(.25D0 + V**2) * U
C
C     NOTE THAT C IS POSITIVE
C
      C = A / R
      S = V * (C + C)
      B = S
      A = R
      RETURN
C
C *** HERE ABS(A) .LE. ABS(B) ***
C
   10 IF (B .EQ. 0.D0) GO TO 20
      U = B + B
      V = A / U
C
C     NOTE THAT U AND R HAVE THE SIGN OF B
C     (R IS IMMEDIATELY STORED IN A)
C
      A = SQRT(.25D0 + V**2) * U
C
C     NOTE THAT S IS POSITIVE
C
      S = B / A
      C = V * (S + S)
      IF (C .EQ. 0.D0) GO TO 15
      B = 1.D0 / C
      RETURN
   15 B = 1.D0
      RETURN
C
C *** HERE A = B = 0.D0 ***
C
   20 C = 1.D0
      S = 0.D0
      RETURN
C
      END
