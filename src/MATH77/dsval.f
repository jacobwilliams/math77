      double precision function DSVAL ( K, NC, T, BCOEF, X, IDERIV )
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 DSVAL Krogh  Changes to use M77CON
c>> 1994-09-26 DSVAL CLL Moved "DSVAL = ZERO" to be 1st executable stmt.
c>> 1992-11-12 C. L. Lawson, JPL  Saving LEFTI.
c>> 1992-10-27 C. L. Lawson, JPL
c>> 1988-03-15 C. L. Lawson, JPL
C     Calculates the value at X of the derivative of order IDERIV
c     of the spline function represented in B-spline form by
c     K, NC, T(), and BCOEF().
c
c     Based on subroutine BVALUE on pp. 144-145 of A PRACTICAL GUIDE TO
c     SPLINES by Carl De Boor, Springer-Verlag, 1978.
c     Current version by C. L. Lawson, JPL, March 1988.
c     ------------------------------------------------------------------
c  K     [in]  Order of the spline functions.  Note that the polynomial
c        degree of the segments of the spline is one less than the
c        order.  Example:  Cubic splines have order K = 4.
c  NC     [in]  Number of B-spline coefficients.  Require NC .ge. 1.
C  T()   [in]  Knot sequence, indexed from 1 to NT, with
c        NT = NC + K.  Knot values must be nonincreasing.
c        Repetition of values is permitted and has the effect of
c        decreasing the order of contimuity at the repeated knot.
c        Proper function representation by splines of order K is
c        supported on the interval from T(K) to T(NC+1).
c        Extrapolation can be done outside this interval.
c  BCOEF()  [in]  Coefficients of B-spline basis functions, indexed from
c        1 to NC.
c  X     [in]  Abcissa at which the spline function or one of its
c        derivatives is to be evaluated.  The evaluation will use one
c        of the polynomial pieces of the spline as follows:
c        If X .lt. T(K+1) use the piece associated with [T(K),T(K+1))
c        If T(L) .le. X .lt. T(L+1) for some L in [K+1, NC-1] use the
c        piece associated with [T(L),T(L+1)).
c        If T(NC) .le. X use the piece associated with
c        [T(NC),T(NC+1)].
c  IDERIV   [in]  Order of derivative to be evaluated.  IDERIV = 0
c        selects evaluation of the spline function.  Require
c        IDERIV .ge. 0.  All derivatives of orders .ge. K will be zero.
c  DSVAL [out] Returned value of the spline function or its requested
c        derivative.
c     ------------------------------------------------------------------
c--D replaces "?": ?SVAL, ?SFIND
c     ------------------------------------------------------------------
      integer KMAX
      parameter(KMAX = 20)
      integer IDERIV, IDERP1, IHI, ILO, IMK, IP1
      integer J, JJ, K, KM1, KMIDER, KMJ, LEFTI, MODE, NC
      double precision AJ(KMAX), BCOEF(NC)
      double precision DP(KMAX), DM(KMAX), FKMJ, T(NC+K), X, ZERO
      parameter(ZERO=0.0D0)
      save LEFTI
      data LEFTI / 1 /
c     ------------------------------------------------------------------
      DSVAL = ZERO
      if(K .gt. KMAX) then
         call IERM1('DSVAL',1,2,'Require KORDER .le. KMAX.',
     *   'KORDER',K,',')
         call IERV1('KMAX',KMAX,'.')
         return
      endif
      KMIDER = K - IDERIV
      if (KMIDER .LE. 0)               GO TO 99
C
c     If T(K) .le. X .lt. NC+1, DSFIND will return LEFTI such that
C     T(LEFTI) .le. X .lt. T(LEFTI+1).
c     Otherwise if X .lt. T(K), sets LEFTI := K
c            or if X .ge. T(NC+1), sets LEFTI := NC.
c
      KM1 = K-1
      call DSFIND (T, K, NC+1, X, LEFTI, MODE )
c
c        MODE =  -1 if X < T(K)
c                  0 if T(1) .le. X .le. T(NC+1)
c                 +1 if T(NC+1) .lt. X
c
C               *** DIFFERENCE THE COEFFICIENTS *IDERIV* TIMES .
      IMK = LEFTI-K
      do 21 J=1,K
         AJ(J) = BCOEF(IMK+J)
   21 continue
      do 25 J=1,IDERIV
         KMJ = K-J
         FKMJ = dble(KMJ)
         do 23 JJ=1,KMJ
            IHI = LEFTI + JJ
            AJ(JJ) = (AJ(JJ+1) - AJ(JJ))/(T(IHI) - T(IHI-KMJ))*FKMJ
   23    continue
   25 continue
C
C  *** COMPUTE VALUE AT *X* IN (T(I),T(I+1)) OF IDERIV-TH DERIVATIVE,
C      GIVEN ITS RELEVANT B-SPLINE COEFF. IN AJ(1),...,AJ(K-IDERIV).
c
      if (IDERIV .ne. KM1) then
         IP1 = LEFTI+1
         do 32 J=1,KMIDER
            DP(J) = T(LEFTI+J) - X
            DM(J) = X - T(IP1-J)
   32    continue
         IDERP1 = IDERIV+1
         do 35 J=IDERP1,KM1
            KMJ = K-J
            ILO = KMJ
            do 33 JJ=1,KMJ
               AJ(JJ) = (AJ(JJ+1)*DM(ILO) + AJ(JJ)*DP(JJ))/
     *                  (DM(ILO)+DP(JJ))
               ILO = ILO - 1
   33       continue
   35    continue
      endif
      DSVAL = AJ(1)
C
   99 continue
      return
      end
