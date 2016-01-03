      subroutine DUREV( XT, TX, NDIM, RCOND, IWORK, WORK)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 DUREV Krogh  Added external statement.
c>> 1994-10-20 DUREV Krogh  Changes to use M77CON
c>> 1994-08-04 DUREV CLL Changed name of common from UCOM to UCOM1.
c>> 1992-02-17 CLL
c>> 1990-12-12 CLL DUREV  Initial code.
c     This subr does what is sometimes called series reversion.
c     Regarding N variables x sub j as functions of
c     N variables t sub i, and given a set of values of the t's and
c     the values of the x's and the 1st and 2nd partial derivatives of
c     the x's with respect to the t's evaluated at this set of t values,
c     this subr computes values at this point of the 1st and 2nd partial
c     derivatives of the t's with respect to the x's.
c     It is required that the matrix of 1st partials of the x's with
c     respect to the t's must be nonsingular.
c     ------------------------------------------------------------------
c                   Subroutine Arguments
c
c  XT(,) [in float]  Array containing values of N variables x sub 1
c     through x sub N, along with their partial derivatives of orders
c     1 and 2 with respect to N variables t sub 1 through t sub N, all
c     evaluated at the set of t values given in TX().
c     Data for x sub i is in XT(1:(N+2)*(N+1)/2),i).
c
c  TX(,) [inout float]  On entry TX(1,i), i = 1,..., N, must contain
c     values of the N variables t sub 1 thru t sub N.
c     Other values in the array TX() on entry are irrelevant.
c     This subr will compute 1st and 2nd partials of the t's with
c     respect to the x's and store these results in
c     TX(2:(N+2)*(N+1)/2),1:N).
c
c  NDIM  [in, integer]  Leading dimension for the arrays XT() and TX().
c     Require NDIM .ge. (N+2)*(N+1)/2.
c
c  RCOND  [out float]  Estimate of the reciprocal condition number of
c     the matrix of 1st partials of x's with respect to the t's.
c     RCOND will satisfy 0.0 .le. RCOND .le. 1.0.
c     Values near 1.0 indicate a well conditioned matrix, small values
c     indicate poor conditioning, zero indicates a singular matrix.  In
c     this latter case the subr will return without computing partials
c     of the t's with respect to the x's.
c
c  IWORK()  [scratch, integer]  Integer work space for this subroutine.
c
c  WORK(N,N,3) [scratch, float]  Floating-point work space for this
c     subroutine.  We use this as 3 NxN arrays.  In the calls to DGECO
c     and DGEI the last argument needs only N scratch locations.
c     ------------------------------------------------------------------
c              N, M1, M2 in common /UCOM1/
c
c  N [in]  Number of components in the (conceptual) x and t vectors.
c  M1, M2 [in]  We assume 0 .le. M1 .le. M2 .le. 2.  In other subrs of
c     the [D/S]UCOMP package, M1 and M2 select computation of partial
c     derivatives of orders M1 through M2, assuming all needed partial
c     derivatives of orders less than M1 are available.
c     This subr differs from others in the [D/S]UCOMP package in that
c     derivs of order 0, i.e., the values of t sub i for i = 1, ..., N,
c     associated with x sub j for j = 1, ..., N, must always be input to
c     this subr.  Thus this subr treats M1 = 0 like M1 = 1.
c     If M2 .eq. 0 this subr does nothing.
c     If M2 .eq. 1 this subr computes first partials of t w.r.t. x.
c     If M1 .le. 1 and M2 .eq. 2 this subr computes first and second
c     partials of t w.r.t. x.
c     If M1 .eq. 2 and M2 .eq. 2 this subr assumes first partials of t
c     w.r.t. x are available and computes second partials of t w.r.t. x.
c     ------------------------------------------------------------------
c--D replaces "?": ?UREV, ?DOT, ?GECO, ?GEI
c     Also uses ERMSG
c     ------------------------------------------------------------------
      external DDOT
      double precision DDOT
      integer N, M1, M2
      common/UCOM1/N,M1,M2
      save /UCOM1/
      integer I, IT, ITXX, IX, IXTT, IWORK(N), J, K, NDIM
      double precision RCOND, TX(NDIM,N), XT(NDIM,N), WORK(N,N,3)
c     ------------------------------------------------------------------
      if(M2 .eq. 0) return
      if(M1 .le. 1) then
c          Copy the matrix of 1st partials of x w.r.t. t to WORK2(*,*,1)
c
      do 20 IX = 1,N
         do 10 IT = 1,N
            WORK(IX,IT,1) = XT(1+IT,IX)
   10    continue
   20 continue
c
c        Compute an LU factorization and RCOND for this Jacobian matrix.
c
      call DGECO(WORK(1,1,1), N, N, IWORK, RCOND, WORK(1,1,3))
      if(RCOND .eq. 0.0d0) then
         call ERMSG('DUREV', 6, 0, 'Singular Jacobian matrix.','.')
         return
      endif
c
c        Compute the inverse Jacobian matrix in WORK(*,*,1) and copy
c        its elements into TX().  These elements are the 1st partials of
c        the t's w.r.t. the x's.
c
      call DGEI(WORK(1,1,1), N, N, IWORK, WORK(1,1,3))
c
      do 40 IT = 1,N
         do 30 IX = 1,N
            TX(1+IX,IT) = WORK(IT,IX,1)
   30    continue
   40 continue
      else
c
c          Copy 1st partials of t's w.r.t. x's from TX() to WORK(*,*,1).
c
      do 60 IT = 1,N
         do 50 IX = 1,N
            WORK(IT,IX,1) = TX(1+IX,IT)
   50    continue
   60 continue
      endif
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(M2 .lt. 2) return
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                 Main loop on K to compute 2nd partials
c                 of t sub K w.r.t. the x's.
c
      do 200 K = 1,N
c
c           Compute weighted sum of the Hessian matrices of the x's
c           using the 1st partial of t sub K w.r.t. x sub i as the
c           weight for the Hessian matrix of x sub i.
c           Store the resulting symmetric matrix in WORK(*,*,2).
c
         IXTT = 1 + N
         do 80 I = 1,N
            do 70 J = 1,I
               IXTT = IXTT + 1
               WORK(I,J,2) = DDOT(N, WORK(K,1,1),N, XT(IXTT,1),NDIM)
               WORK(J,I,2) = WORK(I,J,2)
   70       continue
   80    continue
c
c           Multiply the matrix in WORK(*,*,2) by WORK(*,*,1) from the
c           right and by the transpose of WORK(*,*,1) from the left and
c           invert the sign.  Put the result of the first multiplication
c           into WORK(*,*,3) and the lower triangle of the (symmetric)
c           final result into TX(*,K).
c
         do 100 I = 1,N
            do 90 J = 1,N
               WORK(I,J,3) = DDOT(N, WORK(I,1,2),N, WORK(1,J,1),1)
   90       continue
  100    continue
c
         ITXX = 1 + N
         do 120 I = 1,N
            do 110 J = 1,I
               ITXX = ITXX + 1
               TX(ITXX,K) = -DDOT(N, WORK(1,I,1),1, WORK(1,J,3),1)
  110       continue
  120    continue
  200 continue
      return
      end
