      SUBROUTINE DCFT (A, MODE, M, ND, MS, S)
c>> 1997-03-31 DCFT Krogh  Increased KEDIM, more sine table checks.
C>> 1994-10-20 DCFT Krogh  Change CERROR to simplify conversion to C.
C>> 1994-10-20 DCFT Krogh  Changes to use M77CON
C>> 1994-04-19 DCFT CLL Edited to make DP & SP files similar.
c>> 1989-06-16 FTK Fix error message on MODE.
c>> 1989-06-05 WVS Change length of MODE from *(ND) to *(*).
c>> 1989-05-07 FTK & CLL
c>> 1989-04-21 FTK & CLL
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c
c     This subroutine computes complex Fourier transforms in up to 6
c     dimensions using the Cooley-Tukey fast Fourier transform.
c
c     Variables in the calling sequence have the following types.
      DOUBLE PRECISION A(*), S(*)
      INTEGER  ND, M(ND), MS
      CHARACTER MODE*(*)
c
c     Programmed by Fred T. Krogh at the Jet Propulsion Laboratory,
c     Pasadena, Calif.   August 1, 1969.
c
c     Values for A, MODE, M, ND, and MS must be specified before
c     calling the subroutine.
c
c     In describing the usage the following notation is used
c     N(K) = 2**M(K)
c     WK = EXP(2*PI*I/N(K)), where I = sqrt(-1) and PI = 3.14159...
c     MA = M(1) + M(2) + ... + M(ND)
c     NA = 2**MA
c     MX = MAX(M(1), M(2), ..., M(ND))
c     NX = 2**MX
c
c The usage is as follows
c
c A() on input is an array of function values, X, if one is doing
c   Fourier analysis,
c   and is an array of Fourier coefficients, C, if one is
c   doing Fourier synthesis.  On output, A contains C if doing analysis,
c   and contains X if doing synthesis.  In our description here, we
c   assume that A has been declared by the user as a complex array with
c   dimension A(N(1),N(2),...,N(ND)). (Of course this is not necessary
c   as long as one declares enough storage for A, and keeps in mind that
c   the array is treated in this subroutine as if it had been specified
c   in this way.)  It is assumed that the imaginary part of a complex
c   number is stored in the cell immediately following its real part.
c
c MODE  A character variable of length at least ND.
c   The k-th character must be 'A' or 'a' to select Analysis in the
c   k-th dimension, or 'S' or 's' to select Synthesis in the
c   k-th dimension.
c   The subroutine sets  A(j1+1, j2+1, ..., jND+1) to the sum
c  over 0 .le. k1 .le. N(1)-1, 0 .le. k2 .le. N(2)-1, ...,
c  0 .le. kND .le. N(ND)-1 of A(k1+1, k2+1, ..., kND+1) * T(1,j1,k1) *
c  T(2,j2,k2) * ... * T(ND,jND,kND), 0 .le. j1 .le. N(1)-1, ...,
c  0 .le. jnd .le. N(ND)-1
c  with T(L,j,k) =
c        WL ** (j*k)               if MODE(L:L) = 'S' or 's'
c        (1/N(L)) * WL ** (-j*k)   if MODE(L:L) = 'A' or 'a'
c
c M() is a vector used to indicate N(k) = 2**M(k), the number of
c  complex points in the k-th variable.  M must be such that MX < 21.
c  If M(L)=0, no change is made with respect to the L-th dimension.
c
c ND is the dimension of (i.e. the number of subscripts in) the
c    array A.  ND must satisfy 1 .le. ND .le. 6
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
c S is a vector, S(j) = sin(pi*j/(2*NT)), j = 1, 2, ..., NT-1, where
c  NT is defined in the description of MS above.  S is computed by the
c  subroutine if MX .gt. MS.  (If S is altered, set MS=0 so that S
c  is recomputed.)
c     ------------------------------------------------------------------
c                Notes on COMMON, PARAMETER's, and local variables
C
c     NDMAX = the maximum value for ND, and MAXM = the maximum
c     permitted for M(1), ..., M(ND)
c     The dimension of KE must be at least as large as MAXM.
c     The named common CDFFTC is used for communication between this
c     subroutine and the subroutine DFFT which computes a one
c     dimensional complex Fourier transform and computes the sine table.
c     The use of the variables in CDFFTC is contained in the listing
c     of DFFT.
c
c     NF(1) = 2, NF(K+1) = 2**(1+M(1)+...+M(K)), K = 1,..., ND
c     ------------------------------------------------------------------
c--D replaces "?": ?CFT, ?FFT, C?FFTC
c     Both versions use IERM1
c     ------------------------------------------------------------------
      INTEGER MAXM, NDMAX
      PARAMETER (NDMAX = 6)

      INTEGER I, II, IIC, IR, IRC
      INTEGER J, JL
      INTEGER K, KEDIM, KSM
      INTEGER L
      INTEGER MA, MMAX, MSI
      INTEGER NDD, NF(NDMAX+1), NTOT2

      CHARACTER*13  CERROR

      DOUBLE PRECISION FN, ONE, SPI4

      PARAMETER (ONE = 1.D0)
      PARAMETER (SPI4 = .70710 67811 86547 52440 08443 62104 8490D0)

c Common variables
      PARAMETER (KEDIM=30)
      LOGICAL NEEDST
      INTEGER MT, NT, MM, KS, ILAST, KE(KEDIM), KEE(KEDIM+1)
c Note that KEE(1) is equivalent to ILAST.
      EQUIVALENCE (KE(1), KEE(2))
      COMMON /CDFFTC/ NEEDST, MT, NT, MM, KS, ILAST, KE
      SAVE /CDFFTC/
      PARAMETER (MAXM = KEDIM)
      DATA CERROR / 'MODE(K:K) =  ' /
c     ------------------------------------------------------------------
c
      NDD = ND
      IF ((NDD .LE. 0) .OR. (NDD .GT. NDMAX)) THEN
c                               Fatal error, default is to stop in IERM1
         CALL IERM1 ('DCFT', 1, 2, 'BAD ND', 'ND', ND, '.')
         MS = -2
         RETURN
      END IF
      NF(1) = 2
      MA = 0
      MMAX = 0
      FN = ONE
      DO 10 K = 1, NDD
         MM = M(K)
         MMAX = MAX(MM, MMAX)
         MA = MA + MM
         I = 2**MM
         IF ((MM .LT. 0) .OR. (MM .GT. MAXM)) THEN
c                               Fatal error, default is to stop in IERM1
         CALL IERM1('DCFT',2,2,'Need 0 .le. M(K) .le. 30','M(K)',MM,'.')
         MS = -2
         RETURN
         END IF
         NF(K+1) = NF(K) * I
         if (MODE(K:K) .eq. 'A' .or. MODE(K:K) .eq. 'a') then
            FN = FN / I
         else if (MODE(K:K) .ne. 'S' .and. MODE(K:K) .ne. 's') then
            CERROR(13:13) = MODE(K:K)
            CALL IERM1 ('DCFT',3,2,CERROR, 'for K =',K,'.')
            MS = -2
            RETURN
         end if
   10 CONTINUE
      MSI = MS
      NEEDST = MMAX .GT. MSI
      if (.NOT. NEEDST) then
c  Check internal parameters to catch certain user errors.
         if (MT .lt. KEDIM) then
            if (MMAX .le. MT + 2) then
c              Skip sine table computation if all appears O.K.
               if (MT .le. 0) go to 20
               if (abs(S(NT/2) - SPI4) .le. 1.D-7) go to 20
            end if
         end if
         NEEDST = .true.
         call ERMSG('DRFT1', 3, 1,
     1      'Invalid sine table (re)computed', '.')
      end if
      MS = MMAX
      MT = MMAX - 2
      CALL DFFT (A, A, S)
      IF (MSI .EQ. -1) RETURN
c                   All setup for computation now
   20 NTOT2 = NF(NDD+1)
      IF (FN .NE. ONE) THEN
         DO 30 I = 1, NTOT2
            A(I) = A(I) * FN
   30    CONTINUE
      END IF
c     Beginning of loop for computing multiple sum
      DO 70 K = 1, NDD
         if(MODE(K:K) .eq. 'A' .or. MODE(K:K) .eq. 'a') then
            IRC = 1
            IIC = 0
         ELSE
            IRC = 0
            IIC = 1
         END IF
         MM = M(K)
         KS = NF(K)
         KSM = KS - 1
         ILAST = NF(K+1)
         DO 40 L = 1, MM
            KEE(L+1) = KEE(L) / 2
   40    CONTINUE
         DO 60 J = 1, NTOT2, ILAST
            JL = J + KSM
            DO 50 I = J, JL, 2
               IR = I + IRC
               II = I + IIC
               CALL DFFT (A(IR), A(II), S)
   50       CONTINUE
   60    CONTINUE
   70 CONTINUE
      RETURN
c        End of loop for computing multiple sum
c
      END
