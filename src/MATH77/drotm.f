      SUBROUTINE DROTM (N,DX,INCX,DY,INCY,DPARAM)

c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2006-06-07 DROTM  Krogh  Removed arithmetic ifs
C>> 1994-10-20 DROTM  Krogh  Changes to use M77CON
C>> 1994-04-29 DROTM  CLL Edited to make DP and SP codes similar.
C>> 1985-08-02 DROTM  Lawson  Initial code.
C
C     APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX
C
C     (DX**T) , WHERE **T INDICATES TRANSPOSE. THE ELEMENTS OF DX ARE IN
C     (DY**T)
C
C     DX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
C     LX = (-INCX)*N, AND SIMILARLY FOR SY USING LY AND INCY.
C     WITH DPARAM(1)=DFLAG, H HAS ONE OF THE FOLLOWING FORMS..
C
C     DFLAG=-1.D0     DFLAG=0.D0        DFLAG=1.D0     DFLAG=-2.D0
C
C       (DH11  DH12)    (1.D0  DH12)    (DH11  1.D0)    (1.D0  0.D0)
C     H=(          )    (          )    (          )    (          )
C       (DH21  DH22),   (DH21  1.D0),   (-1.D0 DH22),   (0.D0  1.D0).
C     SEE DROTMG FOR A DESCRIPTION OF DATA STORAGE IN DPARAM.
C     -----------------------------------------------------------------
c--D replaces "?": ?ROTM
C     -----------------------------------------------------------------
      integer i, incx, incy, kx, ky, n, nsteps
      DOUBLE PRECISION DFLAG,DH11,DH12,DH21,DH22, DX(*),DY(*),DPARAM(5)
      DOUBLE PRECISION TWO, W,Z, ZERO
      parameter(ZERO= 0.0d0, TWO= 2.0d0)
C     -----------------------------------------------------------------
C
      DFLAG=DPARAM(1)
      IF ((N .LE. 0) .OR. (DFLAG+TWO .EQ. ZERO)) RETURN
      IF ((INCX .NE. INCY) .OR. (INCX .LT. 0)) THEN
        KX=1
        KY=1
        IF(INCX .LT. 0) KX=1+(1-N)*INCX
        IF(INCY .LT. 0) KY=1+(1-N)*INCY
C
        IF (DFLAG .EQ. 0.D0) THEN
          DH12=DPARAM(4)
          DH21=DPARAM(3)
          DO 10 I=1,N
            W=DX(KX)
            Z=DY(KY)
            DX(KX)=W+Z*DH12
            DY(KY)=W*DH21+Z
            KX=KX+INCX
            KY=KY+INCY
 10       CONTINUE
        ELSE IF (DFLAG .GT. 0.D0) then
          DH11=DPARAM(2)
          DH22=DPARAM(5)
          DO 20 I=1,N
            W=DX(KX)
            Z=DY(KY)
            DX(KX)=W*DH11+Z
            DY(KY)=-W+DH22*Z
            KX=KX+INCX
            KY=KY+INCY
 20      CONTINUE
        ELSE
          DH11=DPARAM(2)
          DH12=DPARAM(4)
          DH21=DPARAM(3)
          DH22=DPARAM(5)
          DO 30 I=1,N
            W=DX(KX)
            Z=DY(KY)
            DX(KX)=W*DH11+Z*DH12
            DY(KY)=W*DH21+Z*DH22
            KX=KX+INCX
            KY=KY+INCY
 30       CONTINUE
        END IF
      ELSE
        NSTEPS=N*INCX
        IF (DFLAG .EQ. 0.D0) THEN
          DH12=DPARAM(4)
          DH21=DPARAM(3)
          DO 40 I=1,NSTEPS,INCX
            W=DX(I)
            Z=DY(I)
            DX(I)=W+Z*DH12
            DY(I)=W*DH21+Z
 40       CONTINUE
        ELSE IF (DFLAG .GT. 0.D0) THEN
          DH11=DPARAM(2)
          DH22=DPARAM(5)
          DO 50 I=1,NSTEPS,INCX
            W=DX(I)
            Z=DY(I)
            DX(I)=W*DH11+Z
            DY(I)=-W+DH22*Z
 50       CONTINUE
        ELSE
          DH11=DPARAM(2)
          DH12=DPARAM(4)
          DH21=DPARAM(3)
          DH22=DPARAM(5)
          DO 60 I=1,NSTEPS,INCX
            W=DX(I)
            Z=DY(I)
            DX(I)=W*DH11+Z*DH12
            DY(I)=W*DH21+Z*DH22
 60       CONTINUE
        END IF
      END IF
      RETURN
      END
