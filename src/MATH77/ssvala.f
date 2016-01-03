      subroutine SSVALA ( K, N, T, NDERIV, BDIF, X, SVALUE)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 SSVALA Krogh  Changes to use M77CON
c>> 1993-01-12 SSVALA CLL Bringing basis computation inline.
c>> 1992-11-12 C. L. Lawson, JPL Saving LEFTI.
c>> 1992-11-02 C. L. Lawson, JPL
c>> 1988-03-16 C. L. Lawson, JPL
c
c     A spline function is defined by the contents of K, N, and T().
c     Given array BDIF(,) containing B-spline coefficients and
c     differences of these, this subroutine computes and stores into
c     SVALUE(1:NDERIV+1) the value and derivatives through order NDERIV
c     of the spline function evaluated at the abcissa, X.
c     Derivatives of order .ge. K will be zero.
c
c     The subroutine, BSPLPP, given on pp. 140-141 of
c     A PRACTICAL GUIDE TO SPLINES by Carl De Boor, Springer-Verlag,
c     1978, has been recoded as separate subroutines: DSTOT (or DSTOP)
c     calling DSDIF and SSVALA.  This subroutine has the functionality
c     of lines 72-103 of BSPLPP in the book.
c     ------------------------------------------------------------------
c--S replaces "?": ?SVALA, ?SFIND
c     Both versions use IERM1, IERV1
c     ------------------------------------------------------------------
      integer KMAX
      parameter(KMAX=20)
      integer I, ID, J, JP1, K, KP1MN, L, LEFT, LEFTI, LEFTPL
      integer MODE, N, NDER, NDERIV
      real             BDIF(N,NDERIV+1), DELTAL(KMAX), DELTAR(KMAX)
      real             ONE, SAVED, SVALUE(NDERIV+1)
      real             T(N+K), TERM, VNIKX(KMAX), X, ZERO
      parameter(ONE = 1.0e0, ZERO = 0.0e0)
      save LEFTI
      data LEFTI / 1/
c     ------------------------------------------------------------------
      if(K .gt. KMAX) then
         call IERM1('SSVALA',1,2,'Require KORDER .le. KMAX.',
     *   'KORDER',K,',')
         call IERV1('KMAX',KMAX,'.')
         return
      endif
      do 5 I=1,NDERIV+1
         SVALUE(I) = ZERO
    5 continue
      call SSFIND(T, K, N+1, X, LEFTI, MODE)
C
c     LEFTI has been found so that [T(LEFTI), T(LEFTI+1)] is the
c     reference interval for the polynomial piece to be used in
c     evaluating at X.
c
      NDER = min(NDERIV, K-1)
      KP1MN = K-NDER
c
c        Compute values of KP1MN basis functions of order KP1MN at X.
c        Store these in VNIKX(1:KP1MN).  Here KP1MN .ge. 1.
c
      VNIKX(1) = ONE
      do 27 J = 1, KP1MN-1
         JP1 = J + 1
         DELTAR(J) = T(LEFTI+J) - X
         DELTAL(J) = X - T(LEFTI+1-J)
         SAVED = ZERO
         do 26 I=1,J
            TERM = VNIKX(I)/(DELTAR(I) + DELTAL(JP1-I))
            VNIKX(I) = SAVED + DELTAR(I)*TERM
            SAVED = DELTAL(JP1-I)*TERM
   26    continue
         VNIKX(JP1) = SAVED
   27 continue
c
c        Loop on ID.  For each value of ID, evaluate the derivative of
c        order ID-1 at X, and store it in SVALUE(ID).
c        Then, if ID > 1, update basis function values
c        in VNIKX() to the next higher order. (From order J to JP1).
c
      do 55 ID = NDER+1, 1, -1
         LEFT = LEFTI - KP1MN
         do 52 L=1,KP1MN
            LEFTPL = LEFT+L
            SVALUE(ID) = VNIKX(L)*BDIF(LEFTPL,ID) + SVALUE(ID)
   52    continue
         if (ID .gt. 1) then
            J = KP1MN
            JP1 = J + 1
            DELTAR(J) = T(LEFTI+J) - X
            DELTAL(J) = X - T(LEFTI+1-J)
            SAVED = ZERO
            do 54 I=1,J
               TERM = VNIKX(I)/(DELTAR(I) + DELTAL(JP1-I))
               VNIKX(I) = SAVED + DELTAR(I)*TERM
               SAVED = DELTAL(JP1-I)*TERM
   54       continue
            VNIKX(JP1) = SAVED
         endif
         KP1MN = KP1MN+1
   55 continue
      return
      end
