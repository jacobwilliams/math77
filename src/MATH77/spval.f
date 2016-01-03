      real             function SPVAL(KORDER, NPC, XI, PC, X, IDERIV)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 SPVAL Krogh  Changes to use M77CON
c>> 1992-10-27 SPVAL C. L. Lawson, JPL  Saving LEFTI
c>> 1988-03-16 C. L. Lawson, JPL
c
c     Computes value at X of the derivative of order IDERIV of a
c     piecewise polynomial given by coeffs relative to the Power basis.
c     The piecewise polynomial is specified by the arguments
c     XI, PC, NPC, KORDER.
c     The "proper interpolation interval" for X is from XI(1) to
c     XI(NPC+1), but extrapolation will be used outside this interval.
c     Based on subroutine PPVALU given on pp. 89-90 of
c     A PRACTICAL GUIDE TO SPLINES by Carl De Boor, Springer-Verlag,
c     1978, however PPVALU uses the Taylor basis and this subr uses
c     the Power basis.
c     The cases of IDERIV = 0, 1, or 2 are coded individually for
c     best efficiency.  The cases of 3 .le. IDERIV .lt. KORDER are
c     treated in a general way.
c     The result is zero when IDERIV .ge. KORDER.
c     ------------------------------------------------------------------
c     KORDER   [in]  Order of the polynomial pieces.  This is one
c           greater than the degree of the pieces.  KORDER is also the
c           leading dimension of the array PC(,).
c     NPC   [in]  Number of polynomial pieces.
c     (XI(j), j = 1, ..., NPC+1)  [in]  Breakpoints defining the
c           subintervals over which the polynomial pieces are defined.
c     ((PC(i,j), i = 1, ..., KORDER), j = 1, ..., NPC)  [in]
c           PC(*,j) contains the coefficients to be used to evaluate
c           the polynomial piece between XI(j) and XI(j+1).
c           PC(i,j) is the coefficient to be multiplied times
c           (X - X(j))**(i-1).
c     X  [in]  Abcissa at which function is to be evaluated.
c     IDERIV   [in]  Order of derivative to be evaluated.
c           Require IDERIV .ge. 0.  IDERIV = 0 means to evaluate the
c           function itself.  Derivatives of order .ge. KORDER will
c           be zero.
c     SPVAL [out]    The computed value or derivative value.
c     ------------------------------------------------------------------
c--S replaces "?": ?PVAL, ?SFIND
c     ------------------------------------------------------------------
      integer I, IDERIV, LEFTI, KORDER, MODE, NPC
      real             DX, FAC, FAC1, FAC2
      real             ONE, X, XI(NPC+1), PC(KORDER,NPC), VAL, ZERO
      parameter(ONE=1.0E0, ZERO=0.0E0)
      save LEFTI
      data LEFTI / 1/
c     ------------------------------------------------------------------
      call SSFIND ( XI, 1, NPC+1, X, LEFTI, MODE )
      DX = X - XI(LEFTI)

      if(IDERIV .eq. 0) then
         VAL = PC(KORDER,LEFTI)
         do 10  I  = KORDER-1, 1, -1
            VAL = VAL * DX + PC(I,LEFTI)
   10    continue
      elseif(IDERIV .eq. 1) then
         FAC1 = real(KORDER-1)
         VAL = FAC1 * PC(KORDER,LEFTI)
         do 20 I = KORDER-1, 2, -1
            FAC1 = FAC1 - ONE
            VAL = VAL * DX + FAC1 * PC(I,LEFTI)
   20    continue
      elseif(IDERIV .eq. 2) then
         FAC1 = real(KORDER-1)
         FAC2 = FAC1 - ONE
         VAL = FAC1 * FAC2 * PC(KORDER,LEFTI)
         do 30 I = KORDER-1, 3, -1
            FAC1 = FAC2
            FAC2 = FAC2 - ONE
            VAL = VAL * DX + FAC1 * FAC2 * PC(I,LEFTI)
   30    continue
      elseif(IDERIV .ge. KORDER) then
         VAL = ZERO
      else
c                           Here when 3 .le. IDERIV .lt. KORDER
c
         FAC1 = real(KORDER-1)
         FAC2 = FAC1
         FAC = FAC1
         do 40 I = 2, IDERIV
            FAC2 = FAC2 - ONE
            FAC = FAC * FAC2
   40    continue
         VAL = FAC * PC(KORDER,LEFTI)
         do 60 I = KORDER-1, IDERIV+1, -1
            FAC2 = FAC2 - ONE
            FAC = (FAC / FAC1) * FAC2
            FAC1 = FAC1 - ONE
            VAL = VAL * DX + FAC * PC(I,LEFTI)
   60    continue
      endif
      SPVAL = VAL
      return
      end
