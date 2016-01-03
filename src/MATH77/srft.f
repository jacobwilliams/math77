      SUBROUTINE SRFT (A, MODE, M, ND, MS, S)
c>> 1997-03-31 SFFT Krogh  Increased KEDIM, more sine table checks.
c>> 1996-01-23 SRFT Krogh  Changes to simplify conversion to C.
c>> 1994-10-20 SRFT Krogh  Changes to use M77CON
c>> 1990-01-23 SRFT CLL Removed extraneous label 80.
c>> 1989-06-16 FTK Fix error message on MODE.
c>> 1989-05-10 FTK & CLL
c>> 1989-05-07 FTK & CLL
c>> 1989-04-21 FTK & CLL
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c
c This subroutine computes Fourier transforms of real data in up to 6
c dimensions using the Cooley-Tukey fast Fourier transform.
c
c     Programmed by Fred F. Krogh at the Jet Propulsion Laboratory,
c     Pasadena, Calif.   August 1, 1969.
c     Revised for portability by Krogh -- February 2, 1988
c
c     Variables in the calling sequence have the following types
      REAL             A(*), S(*)
      INTEGER          ND, M(ND), MS
      CHARACTER        MODE
c
c     Values for A(), MODE, M(), ND, and MS must be specified before
c     calling the subroutine.
c
c     In describing the usage the following notation is used
c     NDA = ND
c     N(K) = 2 ** M(K)
c     MA = M(1) + M(2) + ... + M(ND)
c     NA = 2 ** MA
c     MX = MAX(M(1),M(2), ..., M(ND))
c     NX = 2**MX
c     WK = EXP(2*PI*I/N(K)), WHERE I = SQRT(-1) AND PI = 3.14159...
c
c     The usage is as follows
c
c A() on input is an array of function values, X, if one is doing
c   Fourier analysis, and is an array of Fourier coefficients, B, if one
c   is doing Fourier synthesis.  On output, A contains B if doing
c   analysis, and contains X if doing synthesis.  In our description
c   here, we assume that A has been declared by the user as a real array
c   with dimension A(N(1), N(2), ..., N(NDA)) when A contains the real
c   data, and A is a complex array, C, with dimension C(N(1)/2,N(2),...,
c   N(NDA)), when A contains complex Fourier coefficients.  (B(k1, k2,
c   ..., kNDA) for k1 > N(1)/2 need not be saved since B(N(1)-k1,
c   N(2)-k2, ..., N(NDA)-kNDA) = conjugate of B(k1, k2, ..., kNDA),
c   where the L-th subscripts are to be interpreted modulo N(L).  It is
c   assumed that the imaginary part of a complex number is stored
c   in the cell immediately following its real part, except that
c   if k1 = 0 and all of the remaining ki satisfy ki = 0 or ki = N(i)/2,
c   then B(0,k2,...,kNDA) and B(N(1)/2,k2,...,kNDA) are real, and
c   real part of C(1,k2+1,...,kNDA+1) = B(0,k2,...,kNDA) and
c   imag. part of C(0,k2+1,...,kNDA+1) = B(N(1)/2,k2,...,kNDA).
c   If k1 = 0 and kL is the first ki that does not satisfy ki = 0 or
c   ki = N(i)/2 then C(1,k2+1,...,kNDA+1) = B(0,k2,...,kNDA) for
c   kL .LT. N(L)/2, and C(1,k2+1,...,kNDA+1) = B(N(1)/2,k2,...,kNDA) for
c   kL.GT.N(L)/2.  Of course the storage required for A can be
c   reserved by the user in any way that works.
c
c MODE  A character variable to select Analysis or Synthesis.
c   'A' or 'a' selects Analysis, and 'S' or 's' selects Synthesis.
c   For Analysis the subroutine computes
c      B(k1,k2,...,kNDA) = sum over 0 .le. j1 .le. N(1)-1, 0 .le. j2
c      .le. N(2)-1,..., 0 .le. jNDA .le. N(NDA)-1, of X(j1,j2,...,jNDA)
c      * T(1,j1,k1) * T(1,j2,k2) * ... * T(NDA,jNDA,kNDA), 0 .le.
c      k1 .le. N(1)-1,..., 0 .le. kNDA .le. N(NDA)-1
c      with T(L,j,k) = (1/N(L)) * WL ** (-j*k).
c   For Synthesis the subroutine computes
c      X(j1,j2,...,jNDA) = sum over 0 .le. k1 .le. N(1)-1, 0 .le. k2
c      .le. N(2)-1,..., 0 .le. kNDA .le. N(NDA)-1, of B(k1,k2,...,kNDA)
c      * T(1,j1,k1) * T(1,j2,k2) * ... * T(NDA,jNDA,kNDA), 0 .le.
c      j1 .le. N(1)-1,..., 0 .le. jNDA .le. N(NDA)-1
c      with T(L,j,k) = WL**(j*k).
c
c M() is a vector used to indicate N(k) = 2**M(k), the number of
c  real points in the k-th variable.  M must be such that 0 .le. M(k)
c  .le. 21, and if M(1)=0, then M(k) must be 0 for k =1, ..., NDA.
c
c ND gives the dimension of (i.e. the number of subscripts in)
c    the array A.  ND must satisfy 1 .le. ND .le. 6.
c
c MS gives the state of the sine table.  If MS > 0, there are NT =
c    2 ** (MS-2) good entries in the sine table.  On the initial call,
c    MS must be set to 0, and when the return is made, it will be set
c    to MX, which is the value of MS required for computing a
c    transform of size NX.  If MS = -1, the sine table will be computed
c    as for the case MS = 0, and then a return to the user will be made
c    with MS set as before, but no transform will be computed.  This
c    option is useful if the user would like access to the sine table
c    before computing the FFT.
c
c S is a vector, S(j) = sin(pi*j/2*NT)), j = 1, 2, ..., NT-1, where
c  NT is defined in the description of MS above.  S is computed by the
c  subroutine if MX .gt. MS.  (If S is altered, set MS=0 so that S
c  is recomputed.)
c     ------------------------------------------------------------------
c                Notes on COMMON, PARAMETER's, and local variables
c
c     NDMAX = the maximum value for ABS(ND), and MAXM = the maximum
c     permitted for M(1), ..., M(ND)
c
c     The dimension of KE must be at least as large as MAXM-1.
c
c     The named common CSFFTC is used for communication between this
c     subroutine and the subroutine SFFT which computes a one
c     dimensional complex Fourier transform and computes the sine table.
c     The use of the variables in CSFFTC is contained in the listing
c     of SFFT.
c
c     NF(1) = 1, NF(K+1) = 2**(M(1) + M(2) + ... + M(K)), K = 1,...,NDA
c
c     MU is used in computing the index associated with a given index.
c     (The indices (k1,k2,...,kNDA) and (j1,j2,...,jNDA) are associated
c     if k1 = (N(1)/2)-j1 (modulo (N(1)/2)) and ki = N(i)-j1 (modulo ni)
c     i = 2,...,ND.)
c
c     ANAL = .TRUE. when doing Fourier analysis, and .FALSE. otherwise
c
c     N1 = 2**M(1)
c     ------------------------------------------------------------------
c--S replaces "?": ?RFT, ?FFT, C?FFTC
c     Both versions use IERM1
c     ------------------------------------------------------------------
      INTEGER MAXM, NDMAX
      PARAMETER (NDMAX = 6)
      INTEGER I, I1, I2, II, II1, II2, IIB, IIC
      INTEGER IR, IR1, IR2, IRB, IRC
      INTEGER J, JDIF, JJ, JL
      INTEGER K, K1, K1L, K1N, KB
      INTEGER KK, KN2, KN2S, KSM
      INTEGER L, LB
      INTEGER M1, MA, MMAX, MSI, MU(NDMAX-1)
      INTEGER N1, NDA, NF(NDMAX+1), NTOT2
      LOGICAL ANAL
      CHARACTER MSG1*19

      REAL             FN, HALF
      REAL             SPI4, T, TI, TT, TTI, TWO, WI, WR
c
      PARAMETER (SPI4 = .70710 67811 86547 52440 08443 62104 8490E0)
      EQUIVALENCE (NF(2), N1)
c
c Common variables
      INTEGER KEDIM
      PARAMETER (KEDIM=30)
      LOGICAL NEEDST
      INTEGER MT, NT, MM, KS, ILAST, KE(KEDIM), KEE(KEDIM+1)
c Note that KEE(1) is equivalent to ILAST.
      EQUIVALENCE (KE(1), KEE(2))
      COMMON /CSFFTC/ NEEDST, MT, NT, MM, KS, ILAST, KE
      SAVE /CSFFTC/

      PARAMETER(MAXM = KEDIM + 1)
      PARAMETER (HALF = .5E0)
      PARAMETER (TWO = 2.E0)
      DATA NF(1) / 1 /
      DATA MSG1 / 'Bad MODE.  MODE =  ' /
c     ------------------------------------------------------------------
c
      if( MODE .eq. 'A' .or. MODE .eq. 'a') then
         ANAL = .true.
      else if( MODE .eq. 'S' .or. MODE .eq. 's') then
         ANAL = .false.
      else
         MSG1(19:19) = MODE
         call ERMSG('SRFT', 2, 2, MSG1, '.')
         MS = -2
         return
      end if
      NDA = ND
      IF ((NDA .EQ. 0) .OR. (NDA .GT. NDMAX)) THEN
c                               Fatal error, default is to stop in IERM1
         CALL IERM1 ('SRFT', 1, 2, 'BAD ND', 'ND', ND, '.')
         MS = -2
         RETURN
      END IF
      MA = 0
      MMAX = -1
      DO 10 K = 1, NDA
         MM = M(K)
         IF (MM .LT. 0) GO TO 400
         IF (MM .GT. MMAX) THEN
c                if M(1)=0, then so must all of the rest.
            IF ((MMAX .EQ. 0) .OR. (MM .GT. MAXM)) GO TO 400
            MMAX = MM
         END IF
         MA = MA + MM
         NF(K+1) = NF(K) * 2 ** MM
   10 CONTINUE
      MSI = MS
      NEEDST = MMAX .GT. MSI
      if (.NOT. NEEDST) then
c  Check internal parameters to catch certain user errors.
         if (MT .lt. KEDIM) then
            if (MMAX .le. MT + 2) then
c              Skip sine table computation if all appears O.K.
               if (MT .le. 0) go to 20
               if (abs(S(NT/2) - SPI4) .le. 1.E-7) go to 20
            end if
         end if
         NEEDST = .true.
         call ERMSG('DRFT1', 3, 1,
     1      'Invalid sine table (re)computed', '.')
      end if
      MS = MMAX
      MT = MMAX - 2
      CALL SFFT (A, A, S)
      IF (MSI .EQ. -1) RETURN
c                   All setup for computation now
   20 IF (MMAX .EQ. 0) RETURN
      NTOT2 = NF(NDA+1)
      M1 = M(1)
c
c     Compute constants associated with N1.
      KN2 = N1 / 2
      KN2S = KN2
      IF (ANAL) KN2S = -KN2S
      K1L = KN2 - 1
      JDIF = (4 * NT) / N1
c
      IF (.NOT. ANAL) THEN
c                     Set flags for Fourier synthesis
         IRC = 0
         IIC = 1
         GO TO 160
      END IF
c
c     Set flags for Fourier analysis
      IRC = 1
      IIC = 0
      FN = HALF / REAL(NTOT2)
c     Doing Fourier analysis, so multiply by 2**(-M(1)-M(2)-...-M(ND)-1)
      DO 90 I = 1, NTOT2
         A(I) = A(I) * FN
   90 CONTINUE
c     Beginning of loop for computing multiple sum
c     multi-dimensional complex Fourier synthesis or analysis
  100 DO 140 K = 1, NDA
         MM = M(K)
         KS = NF(K)
         IF (K.NE.1) GO TO 110
         MM = MM - 1
         KS = 2
  110    KSM = KS - 1
         ILAST = NF(K+1)
         DO 120 L = 1, MM
            KEE(L+1) = KEE(L) / 2
  120    CONTINUE
         DO 130 J = 1, NTOT2, ILAST
            JL = J + KSM
            DO 125 I = J, JL, 2
               IR = I + IRC
               II = I + IIC
               CALL SFFT (A(IR), A(II), S)
  125       CONTINUE
  130    CONTINUE
  140 CONTINUE
c     End of loop for doing multi-dimensional complex Fourier transforms
c
      IF (.NOT. ANAL) RETURN
c
c
c     Beginning of calculations relating coefficients of real data
c     with coefficients of associated complex data
  160 KB = 0
      L = 1
      LB = 1
c     L = LB occurs when the coefficients of the real data which are
c     computed in the case k1 = 0, are real.
c
c     Coefficients with index L + k1 and LB + N1 - k1 (k1.gt.0) are
c     associated in the sense described above where MU is dimensioned.
c     When K.GT.1, LB = KB - L.
c
      K = 1
c
c     Special case -- K1 = N1/4  (L = LB)
  170 IF (M1.ne.1) then
         I1 = L + KN2
         A(I1) = TWO * A(I1)
         A(I1+1) = -TWO * A(I1+1)
      end if
c
c     Special case --  K1 = 0    (L = LB)
      T = A(L) + A(L+1)
      TI = A(L) - A(L+1)
      IF (ANAL) THEN
         T = TWO * T
         TI = TWO * TI
      END IF
      A(L) = T
      A(L+1) = TI
      GO TO 220
c
c     Special case -- K1 = N1/4  (L.NE.LB)
  200 IF (M1.NE.1) THEN
         I1 = L + KN2
         I2 = LB + KN2
         T = TWO * A(I2)
         A(I2) = TWO * A(I1)
         A(I1) = T
         T = -TWO * A(I2+1)
         A(I2+1) = -TWO * A(I1+1)
         A(I1+1) = T
      END IF
c        SET UP BASE INDICES
      IR = L
      IRB = LB
      IF (ANAL) THEN
         IR = IRB
         IRB = L
      END IF
      II = IR + 1
      IIB = IRB + 1
c
c     SPECIAL CASE-- K1 = 0     (L.NE.LB)
      T = A(IR) + A(IRB)
      TT = A(IIB) + A(II)
      TI = A(II) - A(IIB)
      TTI = A(IR) - A(IRB)
      A(IR) = T - TT
      A(IRB) = T + TT
      A(II) = TI + TTI
      A(IIB) = TTI - TI
c
  220 IF (M1.LE.2) GO TO 260
      J = 0
c
c     USUAL CASE -- K1.NE.0  AND  K1.NE.N1/4
      DO 250 K1 = 2, K1L, 2
         K1N = N1 - K1
         IF (ANAL) THEN
            IR1 = LB + K1N
            IR2 = L + K1
         ELSE
            IR1 = L + K1
            IR2 = LB + K1N
         END IF
         II2 = IR2 + 1
         II1 = IR1 + 1
         J = J + JDIF
         WI = S(J)
         JJ = NT - J
         WR = S(JJ)
         T = A(IR1) - A(IR2)
         TI = A(II1) + A(II2)
         TT = T * WI + TI * WR
         TTI = T * WR - TI * WI
         T = A(IR1) + A(IR2)
         TI = A(II1) - A(II2)
         A(IR1) = T - TT
         A(IR2) = T + TT
         A(II1) = TTI + TI
         A(II2) = TTI - TI
         IF (L .NE. LB) THEN
            IR1 = IR1 + KN2S
            IR2 = IR2 - KN2S
            II2 = IR2 + 1
            II1 = IR1 + 1
            T = A(IR1) - A(IR2)
            TI = A(II1) + A(II2)
            TT = T * WR - TI * WI
            TTI = T * WI + TI * WR
            T = A(IR1) + A(IR2)
            TI = A(II1) - A(II2)
            A(IR1) = T - TT
            A(IR2) = T + TT
            A(II1) = TI - TTI
            A(II2) = -TI - TTI
         END IF
  250 CONTINUE
c
  260 IF (K .LT. 2) GO TO 310
  270 L = L + N1
      KK = 1
      IF (K .GT. 2) THEN
c             Logic used to find index associated with a given index.
  280    IF (MU(KK).EQ.0) KB = KB + NF(KK+2)
         MU(KK) = MU(KK) + NF(KK+1)
         IF (MU(KK) .GE. NF(KK+2)) THEN
            MU(KK) = 0
            KK = KK + 1
            KB = KB - NF(KK+1)
            IF (KK.LE.(K - 2)) GO TO 280
         END IF
      END IF
  300 LB = KB - L
      IF (LB .GE. L) THEN
         IF (LB .GT. L) GO TO 200
         GO TO 170
      END IF
      IF (KK .LE. (K-2)) GO TO 270
  310 IF (K.GE.NDA) GO TO 330
      DO 320 I = 1, K
  320 MU(I) = 0
      K = K + 1
      KB = NF(K+1) + 2
      L = NF(K) + 1
      GO TO 300
c
  330 IF (.NOT. ANAL) GO TO 100
      RETURN
c
c               Fatal error, default is to stop in IERM1
  400 call ERMSG('SRFT',2,2,'M() specified improperly','.')
      MS = -2
      RETURN
c
      END
