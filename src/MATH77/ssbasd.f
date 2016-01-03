      subroutine SSBASD(KORDER, LEFT, T, X, IDERIV, BDERIV)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 SSBASD Krogh  Changes to use M77CON
c>> 1993-01-12 SSBASD CLL  Bringing all computation inline.
c>> 1992-11-02 C. L. Lawson, JPL
c>> 1988-03-22 C. L. Lawson, JPL
c
c     Given the knot-set T(i), i = 1, ..., NT, this subr computes
c     values at X of the derivative of order IDERIV of each of the
c     the KORDER B-spline basis functions of order KORDER
c     that are nonzero on the open interval (T(LEFT), T(LEFT+1).
c     Require T(LEFT) < T(LEFT+1) and KORDER .le. LEFT .le.
c     NT-KORDER.
c     For proper evaluation, X must lie in the closed interval
c     [T(LEFT), T(LEFT+1], otherwise the computation constitutes
c     extrapolation.
c     The basis functions whose derivatives are returned will be those
c     indexed from LEFT+1-KORDER through LEFT, and their values will be
c     stored in BDERIV(I), I = 1, ..., KORDER.
c     ------------------------------------------------------------------
c  Method:
c
c  In general there are two stages: First compute values of basis
c  functions of order KORDER-IDERIV.  Then, if IDERIV > 0, transform
c  these values to produce values of higher order derivatives of
c  higher order basis functions, ultimately obtaining values of
c  the IDERIV derivative of basis functions of order KORDER.
c
c  The first stage uses the recursion:
C
C                       X - T(I)              T(I+J+1) - X
C     B(I,J+1)(X)  =  -----------B(I,J)(X) + ---------------B(I+1,J)(X)
C                     T(I+J)-T(I)            T(I+J+1)-T(I+1)
C
c  where B(I,J) denotes the basis function of order J and index I.
c  For order J, the only basis functions that can be nonzero on
c  [T(LEFT), T(LEFT+1] are those indexed from LEFT+1-J to LEFT.
c  For order 1, the only basis function nonzero on this interval is
c  is B(LEFT,1), whose value is 1.  The organization of the calculation
c  using the above recursion follows Algorithm (8) in Chapter X of the
c  book by DeBoor.
c
c  For the second stage, let B(ID, K, J) denote the value at X of the
c  IDth derivative of the basis function of order K, with index J.
c  From the first stage we will have values of B(0, KORDER-IDERIV, j)
c  for j = LEFT+1-KORDER+IDERIV), ..., LEFT, stored in BDERIV(i), for
c  i = 1+IDERIV, ..., KORDER.
c
c     Loop for ID = 1, ..., IDERIV
c        Replace the contents of BDERIV() by values of
c        B(ID, KORDER-IDERIV+ID, j) for
c        j = LEFT+1-KORDER+IDERIV-ID), ..., LEFT, storing these in
c        BDERIV(i), i = 1+IDERIV-ID, ..., KORDER.
c     End loop
c
c  The above loop uses formula (10), p. 138, of
c  A PRACTICAL GUIDE TO SPLINES by Carl DeBoor, Springer-Verlag,
c  1978, when ID = 1, and successive derivatives of that formula
c  when ID > 1.  Note that we are using Formula (10) in the special
c  case in which (Alpha sub i) = 1, and all other Alpha's are zero.
c
c  This approach and implementation by C. L. Lawson, JPL, March 1988.
C     ------------------------------------------------------------------
C  KORDER [in]  Gives both the order of the B-spline basis functions
c        whose derivatives are to be evaluated,
c        and the number of such functions to be evaluated.
c        Note that the polynomial degree of these functions is one less
c        than the order.  Example:  For cubic splines the order is 4.
c        Require 1 .le. KORDER .le. KMAX, where KMAX is an internal
c        parameter.
c  LEFT  [in]  Index identifying the left end of the interval
c        [T(LEFT), T(LEFT+1)] relative to which the B-spline basis
c        functions will be evaluated.
c        Require        KORDER .le. LEFT .le. NT-KORDER
c        and            T(LEFT) < T(LEFT+1)
c        The evaluation is proper if T(LEFT) .le. X .le. T(LEFT+1), and
c        otherwise constitutes extrapolation.
C        DIVISION BY ZERO  will result if T(LEFT) = T(LEFT+1)
C  T()   [in]  Knot sequence, indexed from 1 to NT, with
c        NT .ge. LEFT+KORDER.  Knot values must be nonincreasing.
c        Repetition of values is permitted and has the effect of
c        decreasing the order of contimuity at the repeated knot.
c        Proper function representation by splines of order K is
c        supported on the interval from T(K) to T(NT+1-K).
c        Extrapolation can be done outside this interval.
C  X     [in]  The abcissa at which the B-spline basis functions are to
c        be evaluated.  The evaluation is proper if T(KORDER) .le. X .le
c        T(NT+1-KORDER), and constitutes extrapolation otherwise.
c  IDERIV [in]  Order of derivative requested.  Require IDERIV .ge. 0.
c        Derivatives of order .ge. KORDER are zero.
C  BDERIV()  [out]  On normal return, this array will contain in
c        BDERIV(i), i = 1, ...,KORDER, the values computed by this subr.
c        These are values at X of the derivative of order IDERIV of each
c        of the basis functions indexed LEFT+1-KORDER through LEFT.
C     ------------------------------------------------------------------
c--S replaces "?": ?SBASD
c     Both versions use IERM1, IERV1
C     ------------------------------------------------------------------
      integer KMAX
      parameter(KMAX = 20)
      integer I, I1, ID, IDERIV, ISTART, IT1, IT2
      integer J, JP1, K1, KORDER, LEFT
      real             BDERIV(KORDER), DELTAL(KMAX), DELTAR(KMAX), FLK1
      real             SAVED, T(LEFT+KORDER), TERM, X
C     ------------------------------------------------------------------
      ID = IDERIV
      if(ID .lt. 0 ) then
         call IERM1('SSBASD',2,2,'Require IDERIV .ge. 0',
     *   'IDERIV',ID,'.')
         return
      endif
      if(ID .ge. KORDER) then
         do 5 I = 1, KORDER
            BDERIV(I) = 0.0e0
    5    continue
         return
      endif
      if(KORDER .gt. KMAX) then
         call IERM1('SSBASD',1,2,'Require KORDER .le. KMAX.',
     *   'KORDER',KORDER,',')
         call IERV1('KMAX',KMAX,'.')
         return
      endif
      ISTART = 1+ID
      K1 = KORDER - ID
c
c        Evaluate K1 basis functions of order K1.  Store values in
c        BDERIV(ISTART:ISTART+K1-1)
c
      BDERIV(ISTART) = 1.0e0
      do 30 J = 1, K1-1
         JP1 = J + 1
         DELTAR(J) = T(LEFT+J) - X
         DELTAL(J) = X - T(LEFT+1-J)
         SAVED = 0.0e0
         do 26 I=1,J
            TERM = BDERIV(ID+I)/(DELTAR(I) + DELTAL(JP1-I))
            BDERIV(ID+I) = SAVED + DELTAR(I)*TERM
            SAVED = DELTAL(JP1-I)*TERM
   26    continue
         BDERIV(ID+JP1) = SAVED
   30 continue
c
c     Loop IDERIV times, each time advancing the order of the
c     derivative by one, so final results are values of derivatives
c     of order IDERIV.
c
      do 70 I1 = ISTART, 2, -1
         FLK1 = real(K1)
         IT1 = LEFT+1-I1
         IT2 = IT1 - K1
         do 50 I = I1, KORDER
            BDERIV(I) = BDERIV(I) * FLK1 /(T(IT1+I) - T(IT2+I))
   50    continue
c
         BDERIV(I1-1) = -BDERIV(I1)
         do 60 I = I1, KORDER-1
            BDERIV(I) = BDERIV(I) - BDERIV(I+1)
   60    continue
         K1 = K1 + 1
   70 continue
      return
      end
