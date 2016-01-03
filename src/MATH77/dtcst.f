      SUBROUTINE DTCST (A, TCS, MODE, M, ND, MS, S)
c>> 1997-03-31 DTCST Krogh  Increased KEDIM, more sine table checks.
c>> 1996-01-23 DTCST Krogh  Changes to simplify conversion to C.
C>> 1994-11-11 DTCST Krogh  Declared all vars.
c>> 1994-10-20 DTCST Krogh  Changes to use M77CON
c>> 1989-06-16 DTCST FTK Fix error message on MODE, and TCS.
c>> 1989-06-05 WVS Change length of MODE and TCS from (ND) to (*)
c>> 1989-05-08 FTK & CLL
c>> 1989-04-21 FTK & CLL
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c
c     This subroutine computes trigonometirc (sine-cosine), sine, or
c     cosine transforms of real data in up to 6 dimensions using the
c     Cooley-Tukey fast Fourier transform.
c
c     Variables in the calling sequence have the following types
      DOUBLE PRECISION A(*), S(*)
      INTEGER   ND, M(ND), MS, KEDIM
      CHARACTER*(*) TCS, MODE
c
c     Programmed by Fred F. Krogh at the Jet Propulsion Laboratory,
c     Pasadena, Calif.   August 1, 1969.
c     Revised for portability by Krogh -- January 29, 1988
c
c     Values for A, TCS, MODE, M, ND, and MS must be specified before
c     calling the subroutine.
c
c     In describing the usage the following notation is used
c     N(K) = 2 ** M(K)
c     MA = M(1) + M(2) + ... + M(ND)
c     NA = 2 ** MA
c
c     MTCS(K) = M(K)     TCS(K:K) = 'T'
c             = M(K)+1   otherwise
c
c     MX = MAX(MTCS(1), MTCS(2), ..., MTCS(ND))
c     NX = 2 ** MX
c
c     T(L,j,k) is defined differently for different values of TCS(L)
c
c       if TCS(L:L) = 'T' and MODE(L:L) = 'S', T(L,j,k)
c         =1/2                     if k = 0
c         =(1/2)*(-1)**j           if k = 1
c         =COS(j*k*PI/N(L))        if k IS EVEN  (k = 2, 4, ..., N(L)-2)
c         =SIN(j*(k-1)*PI/N(L))    if k IS ODD   (k = 3, 5, ..., N(L)-1)
c                    and if MODE(L:L) = 'A', T(L,j,k)
c         = (4/N) * (value of T(L,k,j) defined above)   If j<2
c         = (2/N) * (value of T(L,k,j) defined above)   Otherwise
c
c       if TCS(L:L) = 'C' and MODE(L:L) = 'S', T(L,j,k)
c         =1/2                     if k = 0
c         =COS(j*k*PI/N(L))           k = 1, 2, ..., N(L)-1
c         =(1/2)*(-1)**j           if k = N(L)
c                    and if MODE(L:L) = 'A', T(L,j,k)
c         = (2/N) * (value of T(L,j,k) defined above)
c
c       if TCS(L:L) = 'S' and MODE(L:L) = 'S', T(L,j,k)
c         =SIN(j*k*PI/N(L))           k = 0, 1, ..., N(L)-1
c                    and if MODE(L:L) = 'A', T(L,j,k)
c         = (2/N) * (value of T(L,j,k) defined above)
c
c     D(L) = N(L)     if TCS(L:L) .ne. 'C'
c          = N(L)+1   if TCS(L:L) = 'C'
c
c     The usage is as follows
c
c A() on input is an array of function values if one is doing Fourier
c   analysis, and is an array of Fourier coefficients if one is doing
c   Fourier synthesis.  On output, these are reversed.  In either case
c   A() is a real array with dimension A(D(1), D(2), ..., D(ND)).
c
c TCS  is a character variable of length ND.  The k-th character must be
c   'T' or 't' to select the general Trigonometric transform, or
c   'C' or 'c' to select the Cosine transform, or
c   'S' or 's' to select the Sine transform.
c     See the description of T(L,j,k) and M above.
c
c MODE  A character variable of length ND.  The k-th character must be
c   'A' or 'a' to select Analysis in the k-th dimension, or
c   'S' or 's' to select Synthesis in the k-th dimension.
c   One may be doing analysis, MODE(k:k) = 'A', with respect to one
c   dimension and synthesis, MODE(k:k) = 'S', with respect to
c   another.  A(j1+1, j2+1, ..., jND+1) is replaced by the sum over
c   0 .le. k1 .le. D(1)-1, 0 .le. k2 .le. D(2)-1, ..., 0 .le. kND .le.
c   D(ND)-1, of A(k1+1, k2+1, ..., kND+1) * T(1, k1, j1) * T(2, k2, j2)
c   ... * T(ND, kND, jND), 0 .le. j1 .le. D(1)-1, ..., 0 .le. jND .le.
c   D(ND)-1.

c M() is a vector used to indicate N(k) = 2**M(k)).  The number of
c   points in the k-th variable is then given by D(k) (see above).  M
c   must be specified in such a way that 0 < M(k) < 22
c   for k = 1, 2, ..., ND.
c
c ND is the dimension of (i.e. the number of subscripts in) the
c    array A.  ND must satisfy 1 .le. ND .le. 6.
c
c MS gives the state of the sine table.  If MS > 0, there are NT =
c    2 ** (MS-2) good entries in the sine table.  On the initial call,
c    MS must be set to 0, and when the return is made, it will be set
c    to MX, which is the value of MS required for computing a
c    transform of size N.  If MS = -1, the sine table will be computed
c    as for the case MS = 0, and then a return to the user will be made
c    with MS set as before, but no transform will be computed.  This
c    option is useful if the user would like access to the sine table
c    before computing the FFT.
c    On detected errors the error message subrs are called and
c    execution stops.  If the user overrides the stop to cause
c    continuation, then this subr will return with MS = -2.
c
c S() is a vector, S(j) = sin(pi*j/2*NT)), j = 1, 2, ..., NT-1, where
c  NT is defined in the description of MS above.  S is computed by the
c  subroutine if MX .gt. MS.  (If S is altered, set MS=0 so that S
c  is recomputed.)
c     -----------------------------------------------------------------
c                Notes on COMMON, PARAMETER's, and local variables
c
c     NDMAX = the maximum value for ND, and MAXMX = the maximum
c     permitted for MTCS(1), ..., MTCS(ND)
c
c     NF(1) = 1, NF(K+1) = NF(K) * D(K) K = 1, 2, ..., ND
c
c     MU is used in the process of eliminating transforms with respect
c     to the first subscript of transforms with TCS(:) = 'S'.
c     (This is only necessary if ND.GT.1.)
c
c     The dimension of KE must be at least as large as MAXMX-1.
c     The named common CDFFTC is used for communication between this
c     subroutine and the subroutine DFFT which computes a one
c     dimensional complex Fourier transform and computes the sine table.
c     The use of the variables in CDFFTC is contained in the listing
c     of DFFT.
c
c     The input character variable TCS is mapped to the internal
c     integer array ITCS() by mapping 'T' to 1, 'C' to 2, 'S' to 3.
c
c     -----------------------------------------------------------------
c--D replaces "?": ?TCST, ?FFT, C?FFTC
c     Both versions use IERM1
c     and need ERFIN, IERV1
c     -----------------------------------------------------------------
      INTEGER MAXMX, NDMAX
      PARAMETER (NDMAX = 6)

      INTEGER I, I1, II, IR, ITCS(NDMAX), ITCSK
      INTEGER J, JDIF, JJ, JK
      INTEGER K, KDR
      INTEGER KII, KIN, KK, KKI, KKL, KKN
      INTEGER L
      INTEGER MA, MI, MMAX, MSI, MU(NDMAX)
      INTEGER N, NDD, NDIV, NF(NDMAX+1)
      INTEGER NI, NI1, NI2, NI2I, NTOT2

      CHARACTER  MSG1*13, MSG2*12

      DOUBLE PRECISION FN, FOUR, ONE
      DOUBLE PRECISION SPI4, SUM, T, T1, TP, TPI, TS, TS1, TWO
      DOUBLE PRECISION WI, WR, ZERO

      PARAMETER (ZERO = 0.0D0)
      PARAMETER (ONE = 1.0D0)
      PARAMETER (TWO = 2.0D0)
      PARAMETER (FOUR = 4.0D0)
      PARAMETER (SPI4 = .70710 67811 86547 52440 08443 62104 8490D0)
c Common variables
      PARAMETER (KEDIM=30)
      LOGICAL NEEDST
      INTEGER MT, NT, MM, KS, ILAST, KE(KEDIM), KEE(KEDIM+1)
c Note that KEE(1) is equivalent to ILAST.
      EQUIVALENCE (KE(1), KEE(2))
      COMMON /CDFFTC/ NEEDST, MT, NT, MM, KS, ILAST, KE
      SAVE /CDFFTC/
      PARAMETER (MAXMX = KEDIM+1)
      DATA MSG1 / 'MODE(K:K) = ?' /
      DATA MSG2 / 'TCS(K:K) = ?' /
c     -----------------------------------------------------------------
c
      NDD = ND
      IF ((NDD .LE. 0) .OR. (NDD .GT. NDMAX)) THEN
C                               FATAL ERROR, DEFAULT IS TO STOP IN IERM1
         CALL IERM1 ('DTCST', 1, 2, 'BAD ND', 'ND', ND, '.')
         MS = -2
         RETURN
      END IF
      MA = 0
      MMAX = 0
      NDIV = 1
c Every element in the array A is divided by NDIV before computing
c the transform.  The value computed for NDIV depends on whether
c one is doing analysis or synthesis and on the type of
c transform being computed.
      DO 10 K = 1, NDD
         MM = M(K)
         IF ((MM .LT. 0) .OR. (MM .GT. MAXMX)) GO TO 200
         MA = MA + MM
         N = 2 ** MM
         IF (MODE(K:K) .eq. 'A' .or. MODE(K:K) .eq. 'a') THEN
            NDIV = NDIV * N
         ELSE IF (MODE(K:K) .eq. 'S' .or. MODE(K:K) .eq. 's') THEN
            NDIV = 2 * NDIV
         else
            MSG1(13:13) = MODE(K:K)
            CALL IERM1 ('DTCST',2,2, MSG1, 'for K =',  K, '.')
            MS = -2
            return
         END IF
         ITCSK = (index('TtCcSs', TCS(K:K)) + 1) / 2
         ITCS(K) = ITCSK
         if(ITCSK .eq. 0) then
            MSG2(12:12) = TCS(K:K)
            CALL IERM1 ('DTCST', 3, 2, MSG2, 'for K =', K, '.')
            return
         endif
         NF(1) = 1
         IF (ITCSK .GE. 2) THEN
            IF (ITCSK .EQ. 2) N = N + 1
            NDIV = 2 * NDIV
            MM = MM + 1
         END IF
         NF(K+1) = NF(K) * N
         IF (MM .GT. MMAX) THEN
            MMAX = MM
         END IF
   10 CONTINUE
c
      MSI = MS
      NEEDST = MMAX .GT. MSI

      if (.NOT. NEEDST) then
c  Check internal parameters to catch certain user errors.
         if (MT .lt. KEDIM) then
            if (MMAX .le. MT + 2) then
c              Skip sine table computation if all appears O.K.
               if (MT .le. 0) go to 15
               if (abs(S(NT/2) - SPI4) .le. 1.D-7) go to 15
            end if
         end if
         NEEDST = .true.
         call ERMSG('DTCST', 3, 1,
     1      'Invalid sine table (re)computed', '.')
      end if
      MS = MMAX
      MT = MMAX - 2
      CALL DFFT (A, A, S)
      IF (MSI .EQ. -1) RETURN
c                   All setup for computation now
   15 NTOT2 = NF(NDD+1)
c
      FN = ONE / DBLE(NDIV)
c     Divide every element of A by NDIV
      DO 20 I = 1, NTOT2
         A(I) = A(I) * FN
   20 CONTINUE
c
c     Beginning of loop for computing multiple sum
      DO 170 K = 1, NDD
         ITCSK = ITCS(K)
         MI = M(K)
         MM = MI - 1
         if(MODE(K:K) .eq. 'A' .or. MODE(K:K) .eq. 'a') MI = -MI
         KDR = NF(K)
         KS = KDR + KDR
         ILAST = NF(K+1)
         IF (ITCSK .EQ. 2) ILAST = ILAST - KDR
         DO 30 L = 1, MM
            KEE(L+1) = KEE(L) / 2
   30    CONTINUE
c
         I = 1
         J = NDD
   40    DO 50 L = 1, J
            MU(L) = 0
            IF ((L .NE. K) .AND. (ITCS(L) .GT. 2)) THEN
c           Skip the part of the array left empty by the sine transform
               MU(L) = NF(L)
               I = I + NF(L)
            END IF
   50    CONTINUE
c
c        Compute indices associated with the current value of I (and K)
   60    I1 = I + KDR
         NI1 = I + NF(K+1)
         IF (ITCSK .EQ. 2) NI1 = NI1 - KDR
         NI = NI1 - KDR
         NI2 = (NI1 + I) / 2
         NI2I = NI2 + KDR
         IF (ITCSK .NE. 1) THEN
c                Doing a cosine or a sine transform -- set MI = 0 and do
c                calculations not required for sine-cosine transforms
            MI = 0
            J = NI
            SUM = A(I1)
            T = A(J)
   70       JK = J - KS
               IF (JK .GE.I1) THEN
                  SUM = SUM + A(J)
                  A(J) = A(JK) - A(J)
                  J = JK
                  GO TO 70
               END IF
            IF (ITCSK .NE. 2) THEN
c                                Calculations for the sine transform
               A(I) = TWO * A(I1)
               A(I1) = -TWO * T
               IF (MM.EQ.0) GO TO 90
               T = TWO * A(NI2)
               A(NI2) = -TWO * A(NI2I)
               A(NI2I) = T
               GO TO 90
            END IF
c                               Set for cosine transform
            A(I1) = A(NI1)
         END IF
         IF (MM.EQ.0) GO TO 90
         if (MI .lt. 0) go to 160
c        Begin calculations for the sine-cosine transform
   80    A(NI2) = TWO * A(NI2)
         A(NI2I) = TWO * A(NI2I)
   90    T = A(I) + A(I1)
         A(I1) = A(I) - A(I1)
         IF (MI .LT. 0) THEN
            IF (ITCSK .EQ. 1) THEN
               A(I1) = TWO * A(I1)
               T = TWO * T
            END IF
         END IF
         A(I) = T
         J = 0
         JDIF = 2 ** (MT - MM + 1)
         KKL = KE(1) - KDR
         IF (MM .GT. 1) THEN
            DO 100 KK = KS, KKL, KS
               KKI = I + KK
               KII = KKI + KDR
               KKN = NI1 - KK
               KIN = KKN + KDR
               J = J + JDIF
               WI = S(J)
               JJ = NT - J
               WR = S(JJ)
               T = A(KKI) + A(KKN)
               TS = A(KKN) - A(KKI)
               T1 = A(KIN) - A(KII)
               TS1 = A(KIN) + A(KII)
               IF (ITCSK .GT. 2) THEN
c                         The sine-cosine transform must be computed
c                         differently in the case of the sine transform
c                         because the input data is stored differently.
                  TP = WR * T - WI * T1
                  TPI = WI * T + WR * T1
                  A(KKI) = TP - TS1
                  A(KKN) = -TP - TS1
                  A(KII) = TPI + TS
                  A(KIN) = TPI - TS
               ELSE
                  TP = WR * TS1 + WI * TS
                  TPI = WI * TS1 - WR * TS
                  A(KKI) = T + TP
                  A(KKN) = T - TP
                  A(KII) = T1 + TPI
                  A(KIN) = TPI - T1
               END IF
  100       CONTINUE
         ELSE IF (MM .EQ. 0) THEN
            GO TO 120
         END IF
c        End of computing sine-cosine transform
c
         IF (MI .LT. 0) GO TO 140
         IR = I
         II = IR + KDR
c
c        Compute a one-dimensional complex Fourier transform
  110    CALL DFFT (A(IR), A(II), S)
         IF (MI .LT. 0) GO TO 80
         IF (MI .NE. 0) GO TO 140
  120    IF (ITCSK .EQ. 1) GO TO 140
         IF (ITCSK .EQ. 3) THEN
            A(I) = ZERO
         ELSE
c                Compute first and last elements of the cosine transform
            SUM = FOUR * SUM
            T = TWO * A(I)
            A(NI1) = T - SUM
            A(I) = T + SUM
            IF (MM.GE.0) A(NI2) = TWO * A(NI2)
         END IF
         IF (MM .GT. 0) THEN
c               Extra calculations required by sine and cosine transform
            J = 0
            JDIF = JDIF / 2
            DO 130 KK = KDR, KKL, KDR
               KKI = I + KK
               KKN = NI1 - KK
               J = J + JDIF
               WI = TWO * S(J)
               T = A(KKI) + A(KKN)
               TS = A(KKI) - A(KKN)
               IF (ITCSK .NE. 2) THEN
                  T = T / WI
               ELSE
                  TS = TS / WI
               END IF
               A(KKI) = T + TS
               A(KKN) = T - TS
  130       CONTINUE
         END IF
c
c        Logic for deciding which one-dimensional transform to do next
  140    J = 0
  150    J = J + 1
            IF (J .EQ. K) THEN
               J = J + 1
               I = I + NF(J) - NF(J-1)
            END IF
            IF (J.GT.NDD) GO TO 170
            MU(J) = MU(J) + NF(J)
            IF (MU(J).GE.NF(J+1)) GO TO 150
         I = I + NF(1)
         J = J - 1
         IF (J .NE. 0) GO TO 40
         GO TO 60
c
c        Set for Fourier analysis
  160    II = I
         IR = II + KDR
         GO TO 110
c
  170 CONTINUE
c     End of K loop
      RETURN
c
c                               Fatal error, default is to stop in IERM1
  200 CALL IERM1 ('DTCST', 4, 2, 'Require 0 .le. max(M(K)) .le. 31',
     1  'M', MM, '.')
      MS = -2
      RETURN
c
      END
