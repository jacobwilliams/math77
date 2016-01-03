      real             function SBINOM(N, K)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1998-01-23 SBINOM  Krogh  Work around for HP compiler bug.
c>> 1997-04-14 SBINOM  Krogh  Added external statement, aint => anint.
c>> 1995-12-14 SBINOM  Krogh  Initial Code
c--S replaces "?": ?BINOM, ?LGAMA
c
c                                    ( N }
c  Computes the binomial coefficient (   }.  Require 0 .le. K .le. N.
c                                    ( K )
c
c **************** Variable Definitions ********************************
c
c R1MACH R1MACH returns the largest floating point number
c SBINOM The result returned for the Binomial Coefficient.  If an error
c    returns, this is set to -1.
c SLGAMA MATH77 library for computing the log of the gamma function.
c BIG    Used in testing for possible overflow.
c BIGTST BIG / MAXN, used to test for overflow when computing factorials
c BIGLOG Used to checking for overflow before exponentiating.
c EXERR  If the result is bigger than EXERR some extra work is done
c    to eliminate the effect of small round off errors.
c FAC    Array of factorials.  For N .le. LBND(1), N! = FAC(N). For
c    LBND(1) .lt. N .le. LBND(2), N! = FAC(LBND(1)) * FAC(N).  For
c    LBND(j) .lt. N .le. LBND(j+1), N! = FAC(LBND(1)) * ... *
c    FAC(LBND(j)) * FAC(N).]
c FNI    Floating value for NI.
c I      Temporary index.
c IERR   Index for error message.
c K      Formal argumtent, see description above.
c K1,K2,K3  Indices used in untangling factorials.  K1 is originally for
c        NI, K2 for NMK, and K3 for KI.
c KI     Intenal value for K = min (K, N - K)
c LBND   Array used to obtain large factorials, see FAC above.
c KBND   Last index used in LBND.
c MAXL   Size of the array LBND.  This is a little larger than needed
c        to handle N = MAXN in single precision IEEE arithmetic.  May
c        need to be bigger if MAXN is increased.
c MAXN   Size of the array FAC.  This is probably larger than needed for
c        single precision in most cases.
c N      Formal argumtent, see description above.
c NC     Highest factorial that has been computed.
c NI     Internal value for N.
c NMK    NI - KI
c PRIMES A table of primes.  The last entry must be equal to the third
c        to the last entry.
c TP     Used for temporary storage.
c TP1    Used for temporary storage.
c
c **************** Variable Declarations *******************************
c
      external R1MACH, SLGAMA
      integer N, K
      integer MAXN, MAXL
      parameter (MAXN = 150, MAXL = 12)
      integer I, IERR, K1, K2, K3, KBND, KI, LBND(MAXL), NC, NI, NMK,
     2  PRIMES(3:33)
      real             BIG, BIGTST, BIGLOG, EXERR, FAC(MAXN), FNI,
     1  R1MACH, TP, TP1, SLGAMA
      character ERMSG(2)*32
      save BIG, BIGTST, BIGLOG, EXERR, FAC, KBND, LBND, NC
      data NC, KBND / 0, 1 /
      data ERMSG / 'Bad value for N or K, ', 'Result would overflow,' /
      data PRIMES / 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
     1  67, 71, 73, 79, 83, 89, 97,101,103,107,109,113,127,131,137,139,
     2 149, 139 /
c      151,157,163,167, 173,179,181,191,193,197,199
c
c **************** Start of Executable Code ****************************
c
      NI = N
      if (NI .lt. 0) go to 300
      KI = min(K, NI - K)
      if (KI .le. 2) then
         if (KI .lt. 1) then
            TP = 1.E0
            if (KI .ne. 0) go to 300
         else
            FNI = NI
            if (KI .eq. 1) then
               TP = FNI
            else
               TP = .5E0 * (FNI * (FNI - 1.E0))
            end if
         end if
      else
         NMK = NI - KI
         if (NI .gt. NC) then
            if (NC .eq. 0) then
               BIG = R1MACH(2)
               BIGTST = BIG / real(MAXN)
               BIGLOG = log(BIG) - .1E0
               EXERR = .01E0 / R1MACH(4)
               FAC(1) = 1.E0
               FAC(2) = 2.E0
               NC = 2
            end if
            if (NI .gt. MAXN) then
                TP = SLGAMA(real(NI+1)) - SLGAMA(real(KI+1)) -
     1              SLGAMA(real(NMK+1))
                if (TP .gt. BIGLOG) go to 320
                TP = exp(TP)
                go to 250
            end if
            TP = NC
            do 100 I = NC, NI - 1
               TP = TP + 1.E0
               if (FAC(I).ge.BIGTST) then
                  LBND(KBND) = I
                  KBND = KBND + 1
                  FAC(I+1) = TP
               else
                  FAC(I+1) = TP * FAC(I)
               end if
  100       continue
            NC = NI
            LBND(KBND) = NI
         end if
         if (NI .le. LBND(1)) then
            TP = FAC(NI) / (FAC(KI) * FAC(NMK))
         else if (NMK .le. LBND(1)) then
            TP = FAC(NI) * ((FAC(LBND(1)) / (FAC(KI)) / FAC(NMK)))
            if (NI .gt. LBND(2)) TP = TP * FAC(LBND(2))
         else
            K2 = 1
            K3 = 1
            I = 1
            do 120 K1 = 1, MAXL
               if (NI .le. LBND(K1)) go to 140
               if (NMK .gt. LBND(K1)) then
                  K2 = K2 + 1
                  if (KI .gt. LBND(K1)) K3 = K3 + 1
               end if
  120       continue
  140       if (K2 .eq. K1) then
               TP = FAC(NI) / (FAC(NMK) * FAC(KI))
            else
               TP = FAC(LBND(K2)) / FAC(NMK)
  150          K2 = K2 + 1
  160          if (TP .gt. 1.E0) then
                  if (I .lt. K3) then
                     TP = TP / FAC(LBND(I))
                     I = I + 1
                     go to 160
                  end if
                  TP = TP / FAC(KI)
                  do 180 I = K2, K1 - 1
                     if (TP .gt. BIG / FAC(LBND(I))) go to 320
                     TP = TP * FAC(LBND(I))
  180             continue
                  if (TP .gt. BIG / FAC(NI)) go to 320
                  TP = TP * FAC(NI)
               else if (K2 .ne. K1) then
                  TP = TP * FAC(LBND(K2))
                  go to 150
               else
                  TP = TP * FAC(NI) / FAC(KI)
               end if
            end if
         end if
         if (TP .ge. EXERR) then
            I = .2199E0 * real(NI)
  200       if (PRIMES(I) .le. NMK) then
               I = I + 1
               go to 200
            end if
            if (PRIMES(I) .le. NI) then
               TP1 = PRIMES(I)
               if (PRIMES(I+1) .le. NI) TP1 = TP1 * PRIMES(I+1)
c    anint(TP) avoided below due to bugs in recent HP Fortran compilers.
c               TP = TP1 * anint(TP / TP1)
                TP = .5E0 + (TP / TP1)
                TP = TP1 * (TP - mod(TP, 1.E0))
            end if
         end if
      end if
  250 continue
c     TP = anint(TP)
      TP = (TP + .5E0) - mod(TP + .5E0, 1.E0)
      SBINOM = TP
      return
c
c           Error processing  -- Default is to stop in IERV1.
  300 IERR = 1
      go to 340
  320 IERR = 2
  340 call IERM1('SBINOM', IERR, 2, ERMSG(IERR), ' N ', N, ',')
      call IERV1(' K ', K, '.')
      SBINOM = -1.E0
      return
      end
