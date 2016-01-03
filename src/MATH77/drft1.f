      SUBROUTINE DRFT1 (A, MODE, M, MS, S)
c>> 1997-03-31 DFFT1 Krogh  Increased KEDIM, more sine table checks.
c>> 1996-01-23 DRFT1 Krogh  Changes to simplify conversion to C.
C>> 1994-11-11 DRFT1 Krogh  Declared all vars.
c>> 1994-10-20 DRFT1 Krogh  Changes to use M77CON
c>> 1989-05-07 DRFT1 FTK & CLL
c>> 1989-04-21 FTK & CLL
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c
c     This subroutine computes Fourier transforms of real data using
c     the Cooley-Tukey fast Fourier transform.
c
c     Variables in the calling sequence have the following types
      DOUBLE PRECISION A(*), S(*)
      INTEGER          M, MS
      CHARACTER        MODE
c
c     Programmed by Fred T. Krogh at the Jet Propulsion Laboratory,
c     Pasadena, Calif.   August 1, 1969.
c     Revised for portability by Krogh -- January 22, 1988
c
c     In describing the usage the following notation is used
c     N = 2 ** M
c     W = EXP(2*PI*I/N), where I = SQRT(-1) and PI = 3.14159...
c
c     The usage is as follows
c
c A() is an array of function values if one is doing Fourier analysis,
c  and is an array of Fourier coefficients if one is doing Fourier
c  synthesis.  In our description here we assume that A is a real
c  array with dimension A(N) when A contains the real data, X, and
c  that A is a complex array with dimension A(N/2) when A contains
c  complex Fourier coefficients, C.  (C(k) for k > N/2 need not be
c  saved since for 0 < k < N, C(N-k) = conjugate of C(k).  It is
c  assumed that the imaginary part of a complex number is stored
c  in the cell immediately following its real part, except that
c  A(1) = C(0), and A(2) = C(N/2).  This is possible since these
c  values of C are real and by doing this both X and C require the
c  same storage in A. Of course the storage required for A can be
c  reserved by the user in any way that works.
c
c MODE  Selects Synthesis or Analysis.
c  If MODE = 'A' or 'a', do Fourier analysis, which amounts to setting
c  C(k) = sum for j=0 to N-1 of X(j)*T(M,j,k), for k = 0, N/2
c  with  T(M,j,k) = (1/N) * W ** (-j*k).
c  If MODE = 'S' or 's', do Fourier synthesis, which amounts to setting
c  X(j) = sum for k=0 to N-1 of C(k)*T(M,j,k), for j = 0, N - 1
c  with  T(M,j,k) = W ** (j*k)
c  (Recall that C(N-k) is the conjugate of C(k).)
c
c M is used to indicate N = 2**M, the number of real points.  The
c  number of points must satisfy 1 .le. N .le. 2**21.
c  M = 0 gives an immediate return.
c
c MS gives the state of the sine table.  If MS > 0, there are NT =
c    2 ** (MS-2) good entries in the sine table.  On the initial call,
c    MS must be set to 0, and when the return is made, it will be set
c    to M, which is the value of MS required for computing a
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
c  subroutine if M .gt. MS.  (If S is altered, set MS=0 so that S
c  is recomputed.)
c
c     ------------------------------------------------------------------
c                Notes on COMMON, PARAMETER's, and local variables
c
c     MMAX is the largest value allowed for M
c     The dimension of KE must be at least as large as MMAX-1.
c     The named common CDFFTC is used for communication between this
c     subroutine and the subroutine DFFT which computes a one
c     dimensional complex Fourier transform and computes the sine table.
c     The use of the variables in CDFFTC is contained in the listing
c     of DFFT.
c
c     ANAL = .TRUE. when doing Fourier analysis, and .false. otherwise.
c
c     N1 = 2 ** M
c     ------------------------------------------------------------------
c--D replaces "?": ?RFT1, ?FFT, C?FFTC
c     Both versions use ERMSG, IERM1
c     and need ERFIN, IERV1
c     ------------------------------------------------------------------
      INTEGER    MMAX
      INTEGER I, II, II1, II2, IR, IR1, IR2
      INTEGER J, JDIF, JJ
      INTEGER K1, K1N, KN2
      INTEGER L
      INTEGER MA, MSI
      INTEGER N1, N1P, KEDIM

      DOUBLE PRECISION FN, HALF
      DOUBLE PRECISION SPI4, T, TI, TT, TTI, TWO, WI, WR

      LOGICAL ANAL
      CHARACTER MSG1*19

      PARAMETER (TWO = 2.D0)
      PARAMETER (HALF = .5D0)
      PARAMETER (SPI4 = .70710 67811 86547 52440 08443 62104 8490D0)
      EQUIVALENCE (ILAST, N1)
c Common variables
      PARAMETER (KEDIM=30)
      LOGICAL NEEDST
      INTEGER MT, NT, MM, KS, ILAST, KE(KEDIM), KEE(KEDIM+1)
c Note that KEE(1) is equivalent to ILAST.
      EQUIVALENCE (KE(1), KEE(2))
      COMMON /CDFFTC/ NEEDST, MT, NT, MM, KS, ILAST, KE
      SAVE /CDFFTC/
      PARAMETER (MMAX = KEDIM+1)
      DATA MSG1 / 'Bad MODE.  MODE =  ' /


c     ------------------------------------------------------------------
c
      if( MODE .eq. 'A' .or. MODE .eq. 'a') then
         ANAL = .true.
      else if( MODE .eq. 'S' .or. MODE .eq. 's') then
         ANAL = .false.
      else
         MSG1(19:19) = MODE
         call ERMSG('DRFT1', 2, 2, MSG1, '.')
         MS = -2
         return
      end if
      MA = M
      IF (MA .GT. MMAX .or. MA .lt. 0) THEN
c                            Fatal error, default is to stop in IERM1
         CALL IERM1 ('DRFT1', 1, 2,
     *   'Require 0 .le. M .le. 31', 'M', M, '.')
         MS = -2
         RETURN
      END IF
      MSI = MS
      NEEDST = MA .GT. MSI
      if (.NOT. NEEDST) then
c  Check internal parameters to catch certain user errors.
         if (MT .lt. KEDIM) then
            if (MA .le. MT + 2) then
c              Skip sine table computation if all appears O.K.
               if (MT .le. 0) go to 10
               if (abs(S(NT/2) - SPI4) .le. 1.D-7) go to 10
            end if
         end if
         NEEDST = .true.
         call ERMSG('DRFT1', 3, 1,
     1      'Invalid sine table (re)computed', '.')
      end if
      MS = MA
      MT = MA - 2
      CALL DFFT (A, A, S)
c                   Return if user requested it.
      IF (MSI .EQ. -1) RETURN
c                   All setup for computation now
   10 IF (MA .NE. 0) THEN
         MM = MA - 1
         N1 = 2 ** MA
         N1P = N1 + 2
         KN2 = N1 / 2
         JDIF = (4 * NT) / N1
         KS = 2
         IF (ANAL) THEN
c                               Set flags for Fourier analysis
            IR = 2
            II = 1
            FN = HALF / DBLE(N1)
c           Doing Fourier analysis, so multiply by 2 ** M
            DO 20 I = 1, N1
               A(I) = A(I) * FN
   20       CONTINUE
         ELSE
c                              Set flags for Fourier synthesis
            IR = 1
            II = 2
            GO TO 50
         END IF

c                              Compute complex Fourier transform
   30    DO 40 L = 1, MM
            KEE(L+1) = KEE(L) / 2
   40    CONTINUE
         CALL DFFT (A(IR), A(II), S)
c                              End of computing complex transform
c
         IF (.NOT. ANAL) GO TO 70
c
c        Beginning of calculations relating coefficients of real data
c        with coefficients of associated complex data
c
c        Special case --  K1 = 0
   50    T = A(1) + A(2)
         TI = A(1) - A(2)
         IF (ANAL) THEN
            T = TWO * T
            TI = TWO * TI
         END IF
         A(1) = T
         A(2) = TI
         IF (MM .GT. 0) THEN
c                           Special kase -- K1 = N1 / 4
            A(KN2+1) = TWO * A(KN2+1)
            A(KN2+2) = -TWO * A(KN2+2)
            IF (MM .GT. 1) THEN
               J = 0
               DO 60 K1 = 3, KN2, 2
                  K1N = N1P - K1
                  IF (ANAL) THEN
                     IR1 = K1N
                     IR2 = K1
                  ELSE
                     IR1 = K1
                     IR2 = K1N
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
   60          CONTINUE
            END IF
         END IF
         IF (.NOT. ANAL) GO TO 30
      END IF
   70 RETURN
c
      END
