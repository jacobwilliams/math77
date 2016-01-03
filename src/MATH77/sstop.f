      subroutine SSTOP(K, N, T, BCOEF, BDIF, NPC, XI, PC)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 SSTOP Krogh  Changes to use M77CON
c>> 1992-10-27 SSTOP C. L. Lawson, JPL
c>> 1988-03-16 C. L. Lawson, JPL
c
c     Given coeffs, BCOEF(), rel to the B-spline basis, this subr
c     computes coeffs, PC(,), rel to the Power basis.
c     This code is an adaptation of lines 35-51 of
c     the subroutine, BSPLPP, given on pp. 140-141 of
c     A PRACTICAL GUIDE TO SPLINES by Carl De Boor, Springer-Verlag,
c     1978, however BSPLPP uses the Taylor basis and this subr uses
c     the Power basis.
c     ------------------------------------------------------------------
c     K     [in]  Order of the spline function.  Note that the
c           polynomial degree of the pieces of the spline function is
c           one less than the order.
c     N     [in]  Number of B-spline coefficients.
c     T()   [in]  Knot sequence defining the B-spline basis functions.
c           Contains N+K values, nondecreasing.  The "proper
c           interpolation interval" is from T(K) to T(N+1).
c     BCOEF()  [in]  N coefficients, defining a spline function relative
c           to the B-spline basis.
c     BDIF(,)  [scratch, out]  Array in which differences of the
c           B-spline coeffs will be stored.
c     NPC   [out]  NPC+1 is the number of distinct values among T(K)
c           through T(N+1).  NPC is the number of polynomial pieces
c           needed to define the spline function.  The number of
c           breakpoints returned in XI() will be NPC.
c     (XI(j), j = 1, ..., NPC+1)  [out]  Breakpoints for the Power
c           representation of the piecewise polynomial.
c           These will be the distinct values from among the knots,
c           T(K) through T(N+1).
c     ((PC(i,j), i = 1, ..., K), j = 1, ..., NPC)  [out]
c           PC(i,j) will be the (i-1)st derivative of the spline
c           function at XI(j).  Thus PC(i,j)/(factorial(i-1)) is the
c           coefficient of (x-XI(j))**(i-1)
c           in the polynomial piece defined over the interval from
c           XI(j) to XI(j+1).
c     ------------------------------------------------------------------
c--S replaces "?": ?STOP, ?SDIF, ?SVALA
c     ------------------------------------------------------------------
      integer I, ILEFT, NPC, K, N, NDERIV
      real             BCOEF(N), BDIF(N,K), DENOM, FAC
      real             ONE, PC(K,*), T(N+K), XI(*)
      parameter(ONE = 1.0e0)
c     ------------------------------------------------------------------
      NDERIV = K-1
      call SSDIF ( K, N, T, BCOEF, NDERIV, BDIF)
      NPC = 0
      XI(1) = T(K)
      do 50 ILEFT=K,N
         if (T(ILEFT+1) .ne. T(ILEFT)) then
            NPC = NPC + 1
            XI(NPC+1) = T(ILEFT+1)
c
c              SSVALA sets PC(I,NPC) to be the (I-1)st derivative of
c              the curve at XI(NPC) for I = 1, ..., K.
c              The subsequent loop divides the Jth derivative by
c              factorial(J).
c
            call SSVALA ( K, N, T, NDERIV, BDIF, XI(NPC), PC(1,NPC))
            DENOM = ONE
            FAC = ONE
            do 40 I = 3,K
               FAC = FAC + ONE
               DENOM = DENOM * FAC
               PC(I,NPC) = PC(I,NPC) / DENOM
   40       continue
         endif
   50 continue
      return
      end
