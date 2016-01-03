      SUBROUTINE SCOMQR(NM,N,LOW,IGH,HR,HI,Z,IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2001-01-24 SCOMQR Krogh  CSQRT -> CSQRTX to avoid C lib. conflicts.
c>> 1996-04-27 SCOMQR Krogh  Changes to use .C. and C%%.
c>> 1996-03-30 SCOMQR Krogh  Added external statement.
C>> 1996-01-18 SCOMQR Krogh  Added M77CON statements for conv. to C.
C>> 1995-01-03 SCOMQR WVS  Added EXTERNAL CQUO, CSQRT so VAX won't gripe
C>> 1992-03-13 SCOMQR FTK  Removed implicit statements.
C>> 1987-11-12 SCOMQR Lawson  Initial code.
c--S Replaces "?": ?COMQR
c--C (Type) Replaces "?": ?QUO, ?SQRTX
c     ------------------------------------------------------------------
c     Version of the Eispack subr, COMQR, for use in the JPL MATH77
c     library.  C. L. Lawson, JPL, 1987 Feb 17.
c     ------------------------------------------------------------------
      EXTERNAL R1MACH
      INTEGER EN,ENM1,I,IERR,IGH,ITS,J,L,LL,LOW,LP1,N,NM
      REAL             R1MACH,HI(NM,N),HR(NM,N),MACHEP
      REAL             NORM,SI,SR,TI,TR,XI,XR,YI,YR,ZZI,ZZR
c++ Default NO_COMPLEX = .C. | (.N. == 'D')
c++ Default COMPLEX = ~NO_COMPLEX
c++ CODE for COMPLEX is active
      COMPLEX Z(N), Z3
c++ CODE for NO_COMPLEX is inactive
C++ Replace "SCABS" = .N.//.Y.//"ABS"
C      EXTERNAL SCABS
C      REAL             SCABS, Z(2*N), Z1(2), Z2(2)
C      REAL             ZDENOM(2),ZIN(2),ZNUM(2),ZOUT(2),ZQUOT(2)
C      EXTERNAL CQUO, CSQRTX
c++ END
C
C     THIS SUBROUTINE IS A TRANSLATION OF A UNITARY ANALOGUE OF THE
C     ALGOL PROCEDURE  COMLR, NUM. MATH. 12, 369-376(1968) BY MARTIN
C     AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 396-403(1971).
C     THE UNITARY ANALOGUE SUBSTITUTES THE QR ALGORITHM OF FRANCIS
C     (COMP. JOUR. 4, 332-345(1962)) FOR THE LR ALGORITHM.
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES OF A COMPLEX
C     UPPER HESSENBERG MATRIX BY THE QR METHOD.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
C          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED,
C          SET LOW=1, IGH=N,
C
C        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS,
C          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX.
C          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN
C          INFORMATION ABOUT THE UNITARY TRANSFORMATIONS USED IN
C          THE REDUCTION BY  CORTH, IF PERFORMED.
C
C     ON OUTPUT-
C
C        THE UPPER HESSENBERG PORTIONS OF HR AND HI HAVE BEEN
C          DESTROYED.  THEREFORE, THEY MUST BE SAVED BEFORE
C          CALLING  COMQR  IF SUBSEQUENT CALCULATION OF
C          EIGENVECTORS IS TO BE PERFORMED,
C
C        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,
C          RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR
C          EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT
C          FOR INDICES IERR+1,...,N,
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C     ARITHMETIC IS REAL EXCEPT FOR THE REPLACEMENT OF THE ALGOL
C     PROCEDURE CDIV BY COMPLEX DIVISION AND USE OF THE SUBROUTINES
C     SQRT AND CMPLX IN COMPUTING COMPLEX SQUARE ROOTS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
C     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
C
C     ------------------------------------------------------------------
C
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
C                **********
      MACHEP = R1MACH(4)
C
      IERR = 0
      IF (LOW .EQ. IGH) GO TO 180
C     ********** CREATE REAL SUBDIAGONAL ELEMENTS **********
      L = LOW + 1
C
      DO 170 I = L, IGH
         LL = MIN(I+1,IGH)
         IF (HI(I,I-1) .EQ. 0.0) GO TO 170
c++ CODE for COMPLEX is active
         NORM = ABS(CMPLX(HR(I,I-1),HI(I,I-1)))
c++ CODE for NO_COMPLEX is inactive
C         Z1(1) = HR(I,I-1)
C         Z1(2) = HR(I,I-1)
C         NORM = SCABS(Z1)
c++ END
         YR = HR(I,I-1) / NORM
         YI = HI(I,I-1) / NORM
         HR(I,I-1) = NORM
         HI(I,I-1) = 0.0
C
         DO 155 J = I, IGH
            SI = YR * HI(I,J) - YI * HR(I,J)
            HR(I,J) = YR * HR(I,J) + YI * HI(I,J)
            HI(I,J) = SI
  155    CONTINUE
C
         DO 160 J = LOW, LL
            SI = YR * HI(J,I) + YI * HR(J,I)
            HR(J,I) = YR * HR(J,I) - YI * HI(J,I)
            HI(J,I) = SI
  160    CONTINUE
C
  170 CONTINUE
C     ********** STORE ROOTS ISOLATED BY CBAL **********
  180 DO 200 I = 1, N
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 200
c++ CODE for COMPLEX is active
         Z(I) = CMPLX(HR(I,I),HI(I,I))
c++ CODE for NO_COMPLEX is inactive
C         Z(2*I-1) = HR(I,I)
C         Z(2*I) = HI(I,I)
c++ END
  200 CONTINUE
C
      EN = IGH
      TR = 0.0
      TI = 0.0
C     ********** SEARCH FOR NEXT EIGENVALUE **********
  220 IF (EN .LT. LOW) GO TO 1001
      ITS = 0
      ENM1 = EN - 1
C     ********** LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
C                FOR L=EN STEP -1 UNTIL LOW  -- **********
  240 DO 260 LL = LOW, EN
         L = EN + LOW - LL
         IF (L .EQ. LOW) GO TO 300
         IF (ABS(HR(L,L-1)) .LE.
     X      MACHEP * (ABS(HR(L-1,L-1)) + ABS(HI(L-1,L-1))
     X             + ABS(HR(L,L)) +ABS(HI(L,L)))) GO TO 300
  260 CONTINUE
C     ********** FORM SHIFT **********
  300 IF (L .EQ. EN) GO TO 660
      IF (ITS .EQ. 30) GO TO 1000
      IF (ITS .EQ. 10 .OR. ITS .EQ. 20) GO TO 320
      SR = HR(EN,EN)
      SI = HI(EN,EN)
      XR = HR(ENM1,EN) * HR(EN,ENM1)
      XI = HI(ENM1,EN) * HR(EN,ENM1)
      IF (XR .EQ. 0.0 .AND. XI .EQ. 0.0) GO TO 340
      YR = (HR(ENM1,ENM1) - SR) / 2.0
      YI = (HI(ENM1,ENM1) - SI) / 2.0
c++ CODE for COMPLEX is active
      Z3 = SQRT(CMPLX(YR**2-YI**2+XR,2.0*YR*YI+XI))
      ZZR = REAL(Z3)
      ZZI = AIMAG(Z3)
c++ CODE for NO_COMPLEX is inactive
C      ZIN(1) = YR**2 - YI**2 + XR
C      ZIN(2) = 2.E0*YR*YI + XI
C      CALL CSQRTX(ZIN,ZOUT)
C      ZZR = ZOUT(1)
C      ZZI = ZOUT(2)
c++ END
      IF (YR * ZZR + YI * ZZI .GE. 0.0) GO TO 310
      ZZR = -ZZR
      ZZI = -ZZI
  310 CONTINUE
c++ CODE for COMPLEX is active
      Z3 = CMPLX(XR,XI) / CMPLX(YR+ZZR,YI+ZZI)
      SR = SR - REAL(Z3)
      SI = SI - AIMAG(Z3)
c++ CODE for NO_COMPLEX is inactive
C      ZNUM(1) = XR
C      ZNUM(2) = XI
C      ZDENOM(1) = YR + ZZR
C      ZDENOM(2) = YI + ZZI
Cc                      CQUO computes quotient of two complex numbers.
C      CALL CQUO(ZNUM,ZDENOM,ZQUOT)
C      SR = SR - ZQUOT(1)
C      SI = SI - ZQUOT(2)
c++ END
      GO TO 340
C     ********** FORM EXCEPTIONAL SHIFT **********
  320 SR = ABS(HR(EN,ENM1)) + ABS(HR(ENM1,EN-2))
      SI = 0.0
C
  340 DO 360 I = LOW, EN
         HR(I,I) = HR(I,I) - SR
         HI(I,I) = HI(I,I) - SI
  360 CONTINUE
C
      TR = TR + SR
      TI = TI + SI
      ITS = ITS + 1
C     ********** REDUCE TO TRIANGLE (ROWS) **********
      LP1 = L + 1
C
      DO 500 I = LP1, EN
         SR = HR(I,I-1)
         HR(I,I-1) = 0.0
         NORM = SQRT(HR(I-1,I-1)*HR(I-1,I-1)+HI(I-1,I-1)*HI(I-1,I-1)
     X               +SR*SR)
         XR = HR(I-1,I-1) / NORM
         XI = HI(I-1,I-1) / NORM
c++ CODE for COMPLEX is active
         Z(I-1) = CMPLX(XR,XI)
c++ CODE for NO_COMPLEX is inactive
C         Z(2*I-3) = XR
C         Z(2*I-2) = XI
c++ END
         HR(I-1,I-1) = NORM
         HI(I-1,I-1) = 0.0
         HI(I,I-1) = SR / NORM
C
         DO 490 J = I, EN
            YR = HR(I-1,J)
            YI = HI(I-1,J)
            ZZR = HR(I,J)
            ZZI = HI(I,J)
            HR(I-1,J) = XR * YR + XI * YI + HI(I,I-1) * ZZR
            HI(I-1,J) = XR * YI - XI * YR + HI(I,I-1) * ZZI
            HR(I,J) = XR * ZZR - XI * ZZI - HI(I,I-1) * YR
            HI(I,J) = XR * ZZI + XI * ZZR - HI(I,I-1) * YI
  490    CONTINUE
C
  500 CONTINUE
C
      SI = HI(EN,EN)
      IF (SI .EQ. 0.0) GO TO 540
c++ CODE for COMPLEX is active
      NORM = ABS(CMPLX(HR(EN,EN),SI))
c++ CODE for NO_COMPLEX is inactive
C      Z2(1) = HR(EN,EN)
C      Z2(2) = SI
C      NORM = SCABS(Z2)
c++ END
      SR = HR(EN,EN) / NORM
      SI = SI / NORM
      HR(EN,EN) = NORM
      HI(EN,EN) = 0.0
C     ********** INVERSE OPERATION (COLUMNS) **********
  540 DO 600 J = LP1, EN
c++ CODE for COMPLEX is active
         XR = REAL(Z(J-1))
         XI = AIMAG(Z(J-1))
c++ CODE for NO_COMPLEX is inactive
C         XR = Z(2*J-3)
C         XI = Z(2*J-2)
c++ END
C
         DO 580 I = L, J
            YR = HR(I,J-1)
            YI = 0.0
            ZZR = HR(I,J)
            ZZI = HI(I,J)
            IF (I .EQ. J) GO TO 560
            YI = HI(I,J-1)
            HI(I,J-1) = XR * YI + XI * YR + HI(J,J-1) * ZZI
  560       HR(I,J-1) = XR * YR - XI * YI + HI(J,J-1) * ZZR
            HR(I,J) = XR * ZZR + XI * ZZI - HI(J,J-1) * YR
            HI(I,J) = XR * ZZI - XI * ZZR - HI(J,J-1) * YI
  580    CONTINUE
C
  600 CONTINUE
C
      IF (SI .EQ. 0.0) GO TO 240
C
      DO 630 I = L, EN
         YR = HR(I,EN)
         YI = HI(I,EN)
         HR(I,EN) = SR * YR - SI * YI
         HI(I,EN) = SR * YI + SI * YR
  630 CONTINUE
C
      GO TO 240
C     ********** A ROOT FOUND **********
  660 continue
c++ CODE for COMPLEX is active
      Z(EN) = CMPLX(HR(EN,EN)+TR,HI(EN,EN)+TI)
c++ CODE for NO_COMPLEX is inactive
C      Z(2*EN-1) = HR(EN,EN) + TR
C      Z(2*EN) = HI(EN,EN) + TI
c++ END
      EN = ENM1
      GO TO 220
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
 1000 IERR = EN
 1001 RETURN
C     ********** LAST CARD OF COMQR **********
      END
