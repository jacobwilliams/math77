c     program DRDSVDRS
c>> 1996-06-25 DRDSVDRS   Krogh Changes for conversion to C.
c>> 1996-05-28 DRDSVDRS   Krogh Added external statement.
c>> 1994-10-19 DRDSVDRS   Krogh  Changes to use M77CON
c>> 1992-03-18 DRDSVDRS   CLL Added "c" to "program" line above.
c>> 1992-03-12 DRDSVDRS   CLL
c     Demo driver for DSVDRS and DCOV3, Singular Value Decomposition
c     and Covariance matrix.
c
c     The sample data was computed as
c     y = 0.5 + 0.25 * sin(2*pi*x) + 0.125 * exp(-x)
c     rounded to four decimal places.
c     ------------------------------------------------------------------
c--D replaces "?": DR?SVDRS, ?SVDRS, ?COV3, ?DOT, ?NRM2
c     ------------------------------------------------------------------
      external DDOT, DNRM2
      double precision DDOT, DNRM2
      integer MMAX, NMAX
      parameter(MMAX = 11, NMAX = 3)
      double precision A(MMAX,NMAX), B(MMAX), C(NMAX), DOF, PI, RNORM
      double precision SING(NMAX), STDDEV
      double precision VAR, WORK(2*NMAX), X(MMAX),Y(MMAX)
      integer I, IERR, J, M, N
c
      data X / 0.0d0,  0.1d0,  0.2d0,  0.3d0,  0.4d0,  0.5d0,
     *                 0.6d0,  0.7d0,  0.8d0,  0.9d0,  1.0d0/
      data Y / 0.6250d0,0.7601d0,0.8401d0,0.8304d0,0.7307d0,0.5758d0,
     *                0.4217d0,0.3243d0,0.3184d0,0.4039d0,0.5460d0/
c     ------------------------------------------------------------------
      M = MMAX
      N = NMAX
      print*,' DRDSVDRS..    Demo driver for DSVDRS and DCOV3.'
c
      PI = 4.0d0 * atan(1.0d0)
      do 10 I = 1, M
         A(I,1) = 1.0d0
         A(I,2) = sin(2.0d0 * PI * X(I))
         A(I,3) = exp(-X(I))
         B(I) = Y(I)
   10 continue

      call DSVDRS(A, MMAX, M, N, B, MMAX, 1, SING, WORK)
c
      print '(/'' Computed singular values:''/(1x,5g14.6))',
     *   (SING(I),I=1,N)
      print '(/'' V matrix:'')'
      do 15 I = 1, N
         print '(1x,3f12.6)', (A(I,J),J=1,N)
   15 continue
c
      RNORM = DNRM2(M-N, B(N+1), 1)
      DOF = M - N
      VAR = RNORM**2 / DOF
      STDDEV = sqrt(VAR)
      print '(/a,f10.6)', ' Estimated Std. Dev. of data error =',STDDEV
c
c     Writing the singular value decomposition of A as
c              A = U * S * Transpose(V),
c     the solution of the least-squares problem A*c ~ b will be
c              c  = V * Inverse(S) *Transpose(U) * b.
c     We now have V in A(), S in SING(), and the product, Transpose(U)*b
c     in B().  So we can now compute c, as long as all singular values
c     are nonzero.
c     SING() contains the singular values as nonnegative
c     numbers in nonincreasing order, so we only need to test SING(N).
c
      if(SING(N) .ne. 0.0d0) then
         do 20 I = 1,N
            B(I) = B(I) / SING(I)
   20    continue
         do 30 I = 1,N
            C(I) = DDOT(N, A(I,1), MMAX, B, 1)
   30    continue

         print '(1x,''Solution coefficients   ='',3f10.6)',
     *      (C(J),J=1,N)
      end if
c
c                      Compute covariance matrix.
c
      call DCOV3(A, MMAX, N, SING, VAR, WORK, IERR)
      print '(/'' After DCOV3, IERR = '',i4)',IERR
      if(IERR .eq. 0) then
         print '(/'' Covariance matrix:'')'
         do 40 I = 1, N
            print '(1x,3g14.6)', (A(I,J),J=1,N)
   40    continue
      end if
      end
