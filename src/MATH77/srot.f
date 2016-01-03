      SUBROUTINE SROT(N,X,INCX,Y,INCY,C,S)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 SROT  Krogh   Declared all vars.
C>> 1994-10-20 SROT   Krogh  Changes to use M77CON
C>> 1985-08-02 SROT   Lawson  Initial code.
c--S replaces "?": ?ROT
C
C     MULTIPLY THE 2 X 2 MATRIX  ( C S) TIMES THE 2 X N MATRIX (X**T)
C                                (-S C)                        (Y**T)
C     WHERE **T INDICATES TRANSPOSE.    THE ELEMENTS OF X ARE IN
C     X(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
C     LX = (-INCX)*N, AND SIMILARLY FOR Y USING LY AND INCY.
      INTEGER N, INCX, INCY, NSTEPS, I, KX, KY
      REAL             X,Y,C,S,ZERO,ONE,W,Z
      DIMENSION X(*),Y(*)
C
      DATA ZERO,ONE/0.E0,1.E0/
      IF(N .LE. 0 .OR. (S .EQ. ZERO .AND. C .EQ. ONE)) GO TO 40
      IF(.NOT. (INCX .EQ. INCY .AND. INCX .GT. 0)) GO TO 20
C
           NSTEPS=INCX*N
           DO 10 I=1,NSTEPS,INCX
                W=X(I)
                Z=Y(I)
                X(I)=C*W+S*Z
                Y(I)=-S*W+C*Z
   10           CONTINUE
           GO TO 40
C
   20 CONTINUE
           KX=1
           KY=1
C
           IF(INCX .LT. 0) KX=1-(N-1)*INCX
           IF(INCY .LT. 0) KY=1-(N-1)*INCY
C
           DO 30 I=1,N
                W=X(KX)
                Z=Y(KY)
                X(KX)=C*W+S*Z
                Y(KY)=-S*W+C*Z
                KX=KX+INCX
                KY=KY+INCY
   30           CONTINUE
   40 CONTINUE
C
      RETURN
      END
