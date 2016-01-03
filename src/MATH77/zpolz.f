      SUBROUTINE ZPOLZ(A,NDEG,Z,H,IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-04-27 ZPOLZ  Krogh Changes to use .C. and C%%.
c>> 1996-03-30 ZPOLZ  Krogh Added external statement, removed "!..."
C>> 1995-01-18 ZPOLZ  Krogh More M77CON for conversion to C.
C>> 1995-11-17 ZPOLZ  Krogh Added M77CON statements for conversion to C
C>> 1995-11-17 ZPOLZ  Krogh Converted SFTRAN to Fortran 77.
c>> 1992-05-11 CLL IERR was not being set when N = 0 or 1. Fixed this.
C>> 1989-10-20 CLL Delcared all variables.
C>> 1987-02-25 ZPOLZ  Lawson  Initial code.
c--Z Replaces "?": ?POLZ, ?QUO
C--D (Type) Replaces "?": ?COMQR
c++ Default NO_COMPLEX = .C. | (.N. == 'D')
c++ Default COMPLEX = ~NO_COMPLEX
C     ------------------------------------------------------------------
C
C In the discussion below, the notation A([*,],k} should be interpreted
c as the complex number A(k) if A is declared complex, and should be
c interpreted as the complex number A(1,k) + i * A(2,k) if A is not
c declared to be of type complex.  Similar statements apply for Z(k).
C     Given complex coefficients A([*,[1),...,A([*,]NDEG+1) this
C     subr computes the NDEG roots of the polynomial
C                 A([*,]1)*X**NDEG + ... + A([*,]NDEG+1)
C     storing the roots as complex numbers in the array Z( ).
C     Require NDEG .ge. 1 and A([*,]1) .ne. 0.
C
C     ------------------------------------------------------------------
C
C     Argument Definitions
C     --------------------
C
C     A( )     (In) Contains the complex coefficients of a polynomial
C              high order coefficient first, with A([*,]1).ne.0. The
C              real and imaginary parts of the Jth coefficient must
C              be provided in A([*],J). The contents of this array will
C              not be modified by the subroutine.
C
C     NDEG     (In) Degree of the polynomial.
C
C     Z( )     (Out) Contains the polynomial roots stored as complex
C              numbers.  The real and imaginary parts of the Jth root
C              will be stored in Z([*,]J).
C
C     H( )     (Scratch) Array of work space.
C
C     IERR     (Out) Error flag. Set by the subroutine to 0 on normal
C              termination. Set to -1 if A([*,]1)=0. Set to -2 if NDEG
C              .le. 0. Set to  J > 0 if the iteration count limit
C              has been exceeded and roots 1 through J have not been
C              determined.
C
C     ------------------------------------------------------------------
C     C.L.Lawson & S.Y.Chan, JPL, June 3,1986.
C     ------------------------------------------------------------------
      EXTERNAL I1MACH
      INTEGER I, IERR, I1MACH, J, NDEG, N
      DOUBLE PRECISION B2, C, C95, F, G, H(NDEG,NDEG,2)
      DOUBLE PRECISION ONE, R, BASE, S, ZERO
      LOGICAL MORE,FIRST
      PARAMETER(ZERO=0.D0, ONE=1.D0, C95=.95D0)
c++ CODE for COMPLEX is inactive
C      COMPLEX A(NDEG+1),TEMP,Z(NDEG)
c++ CODE for NO_COMPLEX is active
      DOUBLE PRECISION A(2,NDEG+1), TEMP(2), Z(2,NDEG)
c++ END
C
      SAVE FIRST,BASE,B2
C
      DATA FIRST /.TRUE./
C     ------------------------------------------------------------------
C
      IF (FIRST) THEN
C
C     Set BASE = machine dependent parameter specifying the base
C                 of the machine floating point representation.
C
        FIRST = .FALSE.
        BASE = I1MACH(10)
        B2 = BASE * BASE
      END IF
C
      IF (NDEG .LE. 0) THEN
        IERR = -2
        CALL ERMSG('ZPOLZ',IERR,0,'NDEG .LE. 0','.')
        RETURN
      END IF
C
c++ CODE for COMPLEX is inactive
c      IF (A(1) .EQ. CMPLX(ZERO, ZERO)) THEN
c++ CODE for NO_COMPLEX is active
      IF (A(1,1) .EQ. ZERO .AND. A(2,1) .EQ. ZERO) THEN
c++ END
        IERR = -1
        CALL ERMSG('ZPOLZ',IERR,0,'A(*,1) .EQ. ZERO','.')
        RETURN
      END IF
C
      N = NDEG
      IERR = 0
C
C     Build first row of companion matrix.
c
      DO 20 I = 2,N+1
c++ CODE for COMPLEX is inactive
C        TEMP = -(A(I)/A(1))
C        H(1,I-1,1) = REAL(TEMP)
C        H(1,I-1,2) = AIMAG(TEMP)
c++ CODE for NO_COMPLEX is active
        CALL ZQUO(A(1,I),A(1,1),TEMP)
        H(1,I-1,1) = -TEMP(1)
        H(1,I-1,2) = -TEMP(2)
c++ END
   20 CONTINUE
C
C     Extract any exact zero roots and set N = degree of
C     remaining polynomial.
C
      DO 30 J = NDEG,1,-1
c++ CODE for COMPLEX is inactive
C        IF (H(1,J,1).NE.ZERO .OR. H(1,J,2).NE.ZERO) go to 40
C        Z(J) = ZERO
c++ CODE for NO_COMPLEX is active
        IF (H(1,J,1).NE.ZERO .OR. H(1,J,2).NE.ZERO) GO TO 40
        Z(1,J) = ZERO
        Z(2,J) = ZERO
c++ END
        N = N - 1
   30 CONTINUE
   40 CONTINUE
C
C     Special for N = 0 or 1.
C
      IF (N .EQ. 0) RETURN
      IF (N .EQ. 1) THEN
c++ CODE for COMPLEX is inactive
C        Z(1) = CMPLX(H(1,1,1),H(1,1,2))
c++ CODE for NO_COMPLEX is active
        Z(1,1) = H(1,1,1)
        Z(2,1) = H(1,1,2)
c++ END
        RETURN
      END IF
C
C     Build rows 2 thru N of the companion matrix.
C
      DO 80 I = 2,N
        DO 60 J = 1,N
          IF (J .EQ. I-1) THEN
             H(I,J,1) = ONE
             H(I,J,2) = ZERO
          ELSE
            H(I,J,1) = ZERO
            H(I,J,2) = ZERO
          END IF
   60   CONTINUE
   80 CONTINUE
C
C ***************** BALANCE THE MATRIX ***********************
C
C     This is an adaption of the EISPACK subroutine BALANC to
C     the special case of a complex companion matrix. The EISPACK
C     BALANCE is a translation of the ALGOL procedure BALANCE,
C     NUM. MATH. 13, 293-304(1969) by Parlett and Reinsch.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).
C
C     ********** ITERATIVE LOOP FOR NORM REDUCTION **********
  100 CONTINUE
        MORE = .FALSE.
        DO 200 I = 1, N
C     Compute R = sum of magnitudes in row I skipping diagonal.
C             C = sum of magnitudes in col I skipping diagonal.
          IF (I .EQ. 1) THEN
            R = ABS(H(1,2,1)) + ABS(H(1,2,2))
            DO 120 J = 3,N
              R = R + ABS(H(1,J,1)) + ABS(H(1,J,2))
  120       CONTINUE
            C = ABS(H(2,1,1)) + ABS(H(2,1,2))
          ELSE
            R = ABS(H(I,I-1,1)) + ABS(H(I,I-1,2))
            C = ABS(H(1,I,1)) + ABS(H(1,I,2))
            IF (I .NE. N) THEN
              C = C + ABS(H(I+1,I,1)) + ABS(H(I+1,I,2))
            END IF
          END IF
C
C     Determine column scale factor, F.
C
          G = R / BASE
          F = ONE
          S = C + R
  140     IF (C .LT. G) THEN
            F = F * BASE
            C = C * B2
            GO TO 140
          END IF
          G = R * BASE
  160     IF (C .GE. G) THEN
            F = F / BASE
            C = C / B2
            GO TO 160
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
              DO 180 J = 1,N
                H(1,J,1) = H(1,J,1)*G
                H(1,J,2) = H(1,J,2)*G
  180         CONTINUE
            ELSE
              H(I,I-1,1) = H(I,I-1,1)*G
              H(I,I-1,2) = H(I,I-1,2)*G
            END IF
C
C     Scale Column I
C
            H(1,I,1) = H(1,I,1) * F
            H(1,I,2) = H(1,I,2) * F
            IF (I .NE. N) THEN
              H(I+1,I,1) = H(I+1,I,1) * F
              H(I+1,I,2) = H(I+1,I,2) * F
            END IF
C
          END IF
  200   CONTINUE
      IF (MORE) GO TO 100
C
      CALL DCOMQR(NDEG,N,1,N,H(1,1,1),H(1,1,2),Z,IERR)
C
      IF (IERR .NE. 0) THEN
        CALL ERMSG('ZPOLZ',IERR,0,'Convergence failure','.')
      END IF
      RETURN
      END
