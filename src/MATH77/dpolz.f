      SUBROUTINE DPOLZ(A,NDEG,Z,H,IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2001-05-25 DPOLZ  Krogh Minor change for making .f90 version.
c>> 1996-04-27 DPOLZ  Krogh Changes to use .C. and C%%.
c>> 1996-03-30 DPOLZ  Krogh Added external statement, MIN0 => MIN.
C>> 1995-01-25 DPOLZ  Krogh Automate C conversion.
C>> 1995-11-17 DPOLZ  Krogh SFTRAN converted to Fortran 77
C>> 1994-10-19 DPOLZ  Krogh  Changes to use M77CON
C>> 1992-05-11 DPOLZ  CLL
C>> 1988-11-16        CLL More editing of spec stmts.
C>> 1988-06-07 DPOLZ  CLL Reordered spec stmts for ANSI standard.
C>> 1987-09-16 DPOLZ  Lawson  Initial code.
c--D replaces "?": ?POLZ,
C     ------------------------------------------------------------------
C
C     Given coefficients A(1),...,A(NDEG+1) this subr computes the
C     NDEG roots of the polynomial
C                 A(1)*X**NDEG + ... + A(NDEG+1)
C     storing the roots as complex numbers in the array Z( ).
C     Require NDEG .ge. 1 and A(1) .ne. 0.
C
C     ------------------------------------------------------------------
C-- Begin mask code changes
C
C     Argument Definitions
C     --------------------
C
C     A( )     (In) Contains the coefficients of a polynomial, high
C              order coefficient first with A(1).ne.0. The contents
C              of this array will not be modified by the subroutine.
C
C     NDEG     (In) Degree of the polynomial.
C
C     Z( )     (Out) Contains the polynomial roots stored as complex
C              numbers. The real and imaginary parts of the Jth roots
C              will be stored in Z(2*J-1) and Z(2*J) respectively.
C
C     H( )     (Scratch) Array of work space.
C
C     IERR     (Out) Error flag. Set by the subroutine to 0 on normal
C              termination. Set to -1 if A(1)=0. Set to -2 if NDEG
C              .le. 0. Set to  J > 0 if the iteration count limit
C              has been exceeded and roots 1 through J have not been
C              determined.
C
C     C.L.Lawson & S.Y.Chan, JPL, June 3,1986.
c     1992-05-11 CLL IERR was not being set when N = 0 or 1. Fixed this.
c                Added type stmts for all variables.
C-- End mask code changes
C     ------------------------------------------------------------------
C
      EXTERNAL D1MACH, I1MACH
      DOUBLE PRECISION C43, C75, C95, HALF, ONE, ZERO
      PARAMETER(ZERO=0.D0, ONE=1.D0, C75=.75D0, HALF=.5D0,
     *          C43=-.4375D0, C95=.95D0)
      INTEGER I,J,K,L,M,N,NDEG, EN,LL,MM,NA,ITS,LOW,MP2,
     *        ENM2,IERR,I1MACH
      DOUBLE PRECISION A(NDEG+1), H(NDEG,NDEG)
      DOUBLE PRECISION  P,Q,R,S,T,W,X,Y,ZZ,MACHEP,D1MACH
      DOUBLE PRECISION  C,F,G,B2,BASE
c++ Default NO_COMPLEX = .C. | (.N. == 'D')
c++ Default COMPLEX = ~NO_COMPLEX
c++ CODE for COMPLEX is inactive
C      COMPLEX Z(*)
c++ CODE for NO_COMPLEX is active
      DOUBLE PRECISION Z(*)
c++ END
      LOGICAL NOTLAS,MORE,FIRST
C
      SAVE FIRST, MACHEP, BASE, B2
C
      DATA FIRST /.TRUE./
C     ------------------------------------------------------------------
C
      IF (FIRST) THEN
C
C     Set MACHEP = machine dependent parameter specifying the
C                  relative precision of floating point arithmetic.
C         BASE  = machine dependent parameter specifying the base
C                  of the machine floating point representation.
C
        FIRST = .FALSE.
        MACHEP = D1MACH(4)
        BASE = I1MACH(10)
        B2 = BASE * BASE
      END IF
C
      IERR = 0
C
      IF (NDEG .LE. 0) THEN
        IERR = -2
        CALL ERMSG('DPOLZ',IERR,0,'NDEG .LE. 0','.')
        RETURN
      END IF
C
      IF (A(1) .EQ. ZERO) THEN
        IERR = -1
        CALL ERMSG('DPOLZ',IERR,0,'A(1) .EQ. ZERO','.')
        RETURN
      END IF
C
      N = NDEG
      IERR = 0
C
C     Build first row of companion matrix.
C
      DO 10 I = 2,NDEG+1
        H(1,I-1) = -(A(I) / A(1))
   10 CONTINUE
C
C     Extract any exact zero roots and set N = degree of
C     remaining polynomial.
C
      DO 20 J = NDEG,1,-1
        IF (H(1,J) .NE. ZERO) go to 30
c++  Code for COMPLEX is inactive
C        Z(J) = ZERO
c++  Code for NO_COMPLEX is active
        Z(2*J-1) = ZERO
        Z(2*J) = ZERO
c++  End
        N = N - 1
   20 CONTINUE
   30 CONTINUE
C
C     Special for N = 0 or 1.
C
      IF (N .EQ. 0) RETURN
      IF (N .EQ. 1) THEN
        Z(1) = H(1,1)
        RETURN
      END IF
C
C     Build rows 2 thru N of the companion matrix.
C
      DO 50 I = 2,N
        DO 40 J = 1,N
          H(I,J) = ZERO
   40   CONTINUE
        H(I,I-1) = ONE
   50 CONTINUE
C
C ***************** BALANCE THE MATRIX ***********************
C
C     This is an adaption of the EISPACK subroutine BALANC to
C     the special case of a companion matrix. The EISPACK BALANCE
C     is a translation of the ALGOL procedure BALANCE, NUM. MATH.
C     13, 293-304(1969) by Parlett and Reinsch.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).
C
C     ********** ITERATIVE LOOP FOR NORM REDUCTION **********
   60   CONTINUE
        MORE = .FALSE.
        DO 110 I = 1, N
C     Compute R = sum of magnitudes in row I skipping diagonal.
C             C = sum of magnitudes in col I skipping diagonal.
          IF (I .EQ. 1) THEN
            R = ABS(H(1,2))
            DO 70 J = 3,N
              R = R + ABS(H(1,J))
   70       CONTINUE
            C = ABS(H(2,1))
          ELSE
            R = ABS(H(I,I-1))
            C = ABS(H(1,I))
            IF (I .NE. N) THEN
              C = C + ABS(H(I+1,I))
            END IF
          END IF
C
C     Determine column scale factor, F.
C
          G = R / BASE
          F = ONE
          S = C + R
   80     IF (C .LT. G) THEN
            F = F * BASE
            C = C * B2
            GO TO 80
          END IF
          G = R * BASE
   90     IF (C .GE. G) THEN
            F = F / BASE
            C = C / B2
            GO TO 90
          END IF
C
C     Will the factor F have a significant effect ?
C
          IF ((C + R) / F .LT. C95 * S) THEN
C
C           Yes, so do the scaling.
C
            G = ONE / F
            MORE = .TRUE.
C
C     Scale Row I
C
            IF (I .EQ. 1) THEN
              DO 100 J = 1,N
                H(1,J) = H(1,J)*G
  100         CONTINUE
            ELSE
              H(I,I-1) = H(I,I-1)*G
            END IF
C
C     Scale Column I
C
            H(1,I) = H(1,I) * F
            IF (I .NE. N) H(I+1,I) = H(I+1,I) * F
C
          END IF
  110   CONTINUE
      if (MORE) go to 60
C
C ***************** QR EIGENVALUE ALGORITHM ***********************
C
C     This is the EISPACK subroutine HQR that uses the QR
C     algorithm to compute all eigenvalues of an upper
C     Hessenberg matrix. Original ALGOL code was due to Martin,
C     Peters, and Wilkinson, Numer. Math., 14, 219-231(1970).
C
      LOW = 1
      EN = N
      T = ZERO
C     ********** SEARCH FOR NEXT EIGENVALUES **********
  160 IF (EN .LT. LOW) GO TO 1001
      ITS = 0
      NA = EN - 1
      ENM2 = NA - 1
C     ********** LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
C                FOR L=EN STEP -1 UNTIL LOW DO -- **********
  170 DO 180 LL = LOW, EN
         L = EN + LOW - LL
         IF (L .EQ. LOW) GO TO 200
         IF (ABS(H(L,L-1)) .LE. MACHEP * (ABS(H(L-1,L-1))
     X      + ABS(H(L,L)))) GO TO 200
  180 CONTINUE
C     ********** FORM SHIFT **********
  200 X = H(EN,EN)
      IF (L .EQ. EN) GO TO 370
      Y = H(NA,NA)
      W = H(EN,NA) * H(NA,EN)
      IF (L .EQ. NA) GO TO 380
      IF (ITS .EQ. 30) GO TO 1000
      IF (ITS .NE. 10 .AND. ITS .NE. 20) GO TO 230
C     ********** FORM EXCEPTIONAL SHIFT **********
      T = T + X
C
      DO 220 I = LOW, EN
  220 H(I,I) = H(I,I) - X
C
      S = ABS(H(EN,NA)) + ABS(H(NA,ENM2))
      X = C75 * S
      Y = X
      W = C43 * S * S
  230 ITS = ITS + 1
C     ********** LOOK FOR TWO CONSECUTIVE SMALL
C                SUB-DIAGONAL ELEMENTS.
C                FOR M=EN-2 STEP -1 UNTIL L DO -- **********
      DO 240 MM = L, ENM2
         M = ENM2 + L - MM
         ZZ = H(M,M)
         R = X - ZZ
         S = Y - ZZ
         P = (R * S - W) / H(M+1,M) + H(M,M+1)
         Q = H(M+1,M+1) - ZZ - R - S
         R = H(M+2,M+1)
         S = ABS(P) + ABS(Q) + ABS(R)
         P = P / S
         Q = Q / S
         R = R / S
         IF (M .EQ. L) GO TO 250
         IF (ABS(H(M,M-1)) * (ABS(Q) + ABS(R)) .LE. MACHEP * ABS(P)
     X    * (ABS(H(M-1,M-1)) + ABS(ZZ) + ABS(H(M+1,M+1)))) GO TO 250
  240 CONTINUE
C
  250 MP2 = M + 2
C
      DO 260 I = MP2, EN
         H(I,I-2) = ZERO
         IF (I .EQ. MP2) GO TO 260
         H(I,I-3) = ZERO
  260 CONTINUE
C     ********** DOUBLE QR STEP INVOLVING ROWS L TO EN AND
C                COLUMNS M TO EN **********
      DO 360 K = M, NA
         NOTLAS = K .NE. NA
         IF (K .EQ. M) GO TO 270
         P = H(K,K-1)
         Q = H(K+1,K-1)
         R = ZERO
         IF (NOTLAS) R = H(K+2,K-1)
         X = ABS(P) + ABS(Q) + ABS(R)
         IF (X .EQ. ZERO) GO TO 360
         P = P / X
         Q = Q / X
         R = R / X
  270    S = SIGN(SQRT(P*P+Q*Q+R*R),P)
         IF (K .EQ. M) GO TO 280
         H(K,K-1) = -S * X
         GO TO 290
  280    IF (L .NE. M) H(K,K-1) = -H(K,K-1)
  290    P = P + S
         X = P / S
         Y = Q / S
         ZZ = R / S
         Q = Q / P
         R = R / P
C     ********** ROW MODIFICATION **********
         DO 310 J = K, EN
            P = H(K,J) + Q * H(K+1,J)
            IF (.NOT. NOTLAS) GO TO 300
            P = P + R * H(K+2,J)
            H(K+2,J) = H(K+2,J) - P * ZZ
  300       H(K+1,J) = H(K+1,J) - P * Y
            H(K,J) = H(K,J) - P * X
  310    CONTINUE
C
         J = MIN(EN,K+3)
C     ********** COLUMN MODIFICATION **********
         DO 330 I = L, J
            P = X * H(I,K) + Y * H(I,K+1)
            IF (.NOT. NOTLAS) GO TO 320
            P = P + ZZ * H(I,K+2)
            H(I,K+2) = H(I,K+2) - P * R
  320       H(I,K+1) = H(I,K+1) - P * Q
            H(I,K) = H(I,K) - P
  330    CONTINUE
C
  360 CONTINUE
C
      GO TO 170
C     ********** ONE ROOT FOUND **********
c++  Code for COMPLEX is inactive
C  370 Z(EN) = CMPLX(X+T,ZERO)
c++  Code for NO_COMPLEX is active
  370 Z(2*EN-1) = X+T
      Z(2*EN) = ZERO
c++  End
      EN = NA
      GO TO 160
C     ********** TWO ROOTS FOUND **********
  380 P = (Y - X) * HALF
      Q = P * P + W
      ZZ = SQRT(ABS(Q))
      X = X + T
      IF (Q .LT. ZERO) GO TO 420
C     ********** PAIR OF REALS **********
      ZZ = P + SIGN(ZZ,P)
c++  Code for COMPLEX is inactive
C      Z(NA) = CMPLX(X+ZZ,ZERO)
C      Z(EN) = Z(NA)
C      IF (ZZ .NE. ZERO) Z(EN) = CMPLX(X-W/ZZ,ZERO)
c++  Code for NO_COMPLEX is active
      Z(2*NA-1) = X + ZZ
      Z(2*NA) = ZERO
      Z(2*EN-1) = Z(2*NA-1)
      Z(2*EN) = Z(2*NA)
      IF (ZZ .NE. ZERO) THEN
        Z(2*EN-1) = X - W/ZZ
        Z(2*EN) = ZERO
      END IF
c++  End
      GO TO 430
C     ********** COMPLEX PAIR **********
c++  Code for COMPLEX is inactive
C  420 Z(NA) = CMPLX(X+P,ZZ)
C      Z(EN) = CMPLX(X+P,-ZZ)
c++  Code for NO_COMPLEX is active
  420 Z(2*NA-1) = X + P
      Z(2*NA) = ZZ
      Z(2*EN-1) = X + P
      Z(2*EN) = -ZZ
c++  End
  430 EN = ENM2
      GO TO 160
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
 1000 IERR = EN
 1001 CONTINUE
C
      IF (IERR .NE. 0) THEN
        CALL ERMSG('DPOLZ',IERR,0,'Convergence failure','.')
      END IF
      RETURN
      END
