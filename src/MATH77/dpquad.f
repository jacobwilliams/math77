      double precision function DPQUAD(KORDER, NPC, XI, PC, X1, X2)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 DPQUAD Krogh  Changes to use M77CON
c>> 1992-11-12 DPQUAD C. L. Lawson, JPL  Saving IL1 and IL2.
c>> 1992-10-27 C. L. Lawson, JPL
c>> 1988-03-16 C. L. Lawson, JPL
C
C   This subroutine computes the integral over [X1,X2] of a
c   piecewise polynomial using the piecewise Power
c   representation given by [XI, PC, NPC, KORDER].
c   The degrees of the polynomial pieces are KORDER-1.
c   Require  2 .le. KORDER .le. 20.
c   Permits X1 .le. X2 or X1 gt. X2.
C   The "proper interpolation interval" is from XI(1) to XI(NPC+1).
c   For most reliable results, X1 and X2 should lie in this
c   interval, however this subr will give a result even if this
c   is not the case by use of extrapolation.
c     ------------------------------------------------------------------
C  DESCRIPTION OF ARGUMENTS
C  INPUT:
c    KORDER   [in]  Order of the polynomial pieces.  This is one
c           greater than the degree of the pieces.  KORDER is also the
c           leading dimension of the array PC(,).
c           Require  2 .le. KORDER .le. 20.
c    NPC    - NUMBER OF POLYNOMIAL PIECES
C    XI()    - Breakpoint array of length NPC+1
C    PC(i,j) - Coeffs for the Power representation of a piecewise
c              polynomial.   i=1,KORDER , j=1,NPC
c              The coeffs PC(*,j) are used at XI(j) and on the
c              interval between XI(j) and XI(j+1).
c              PC(i,j) is the coefficient to be multiplied times
c              (X - X(j))**(i-1).
C    X1,X2  - END POINTS OF QUADRATURE INTERVAL, NORMALLY IN
C                XI(1) .LE. X .LE. XI(NPC+1) but extrapolation is
c                permitted.
C
c  OUTPUT:
c     DPQUAD - Integral of the piecewise polynomial from X1 to X2.
c     ------------------------------------------------------------------
c     Adapted from subroutine PPQUAD due to D. E. Amos,
c     Sandia, June, 1979.  Documented in SAND79-1825.  PPQUAD uses the
c     Taylor basis, whereas this subprogram uses the Power basis.
c     Uses the [XI, PC, NPC, KORDER] representation of a piecewise
c     polynomial as presented by Carl De Boor in
c     A PRACTICAL GUIDE TO SPLINES, Springer-Verlag, 1978.
c     (Called [XI, C, LXI, K] in the book.)
c     ------------------------------------------------------------------
c--D replaces "?": ?PQUAD, ?SFIND
c     ------------------------------------------------------------------
      integer II, IL1, IL2, IM, KORDER, LEFT, MODE, NPC
      double precision A, AA, B, BB, DENOM, DX, ONE
      double precision Q, S, SS(2), PC(KORDER,NPC)
      double precision X, X1, X2, XI(NPC+1)
      double precision ZERO
      parameter(ONE = 1.0D0, ZERO = 0.0D0)
      save IL1, IL2
      data IL1, IL2 / 1,1 /
c     ------------------------------------------------------------------
C
      AA= min(X1,X2)
      BB= max(X1,X2)
      Q = ZERO
      if(AA .eq. BB) go to 90
      call DSFIND ( XI, 1, NPC+1, AA, IL1, MODE )
      call DSFIND ( XI, 1, NPC+1, BB, IL2, MODE )
      do 50 LEFT=IL1,IL2
         if(LEFT .eq. IL1) then
            A=AA
         else
            A= XI(LEFT)
         endif
         if(LEFT .eq. IL2) then
            B = BB
         else
            B = XI(LEFT+1)
         endif
         X = A
         do 40 II=1,2
            SS(II) = ZERO
            DX=X-XI(LEFT)
            if(DX .ne. ZERO) then
               DENOM = dble(KORDER)
               S=PC(KORDER,LEFT)/DENOM
               do 30 IM = KORDER-1, 1, -1
                  DENOM = DENOM - ONE
                  S = S * DX + PC(IM,LEFT) / DENOM
   30          continue
               SS(II)=S*DX
            endif
            X = B
   40    continue
         Q=Q+(SS(2)-SS(1))
   50 continue
      if(X1 .gt. X2) Q=-Q
   90 continue
      DPQUAD = Q
      return
      end
