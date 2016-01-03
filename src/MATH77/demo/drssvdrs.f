c     program DRSSVDRS
c>> 1996-06-25 DRSSVDRS   Krogh Changes for conversion to C.
c>> 1996-05-28 DRSSVDRS   Krogh Added external statement.
c>> 1994-10-19 DRSSVDRS   Krogh  Changes to use M77CON
c>> 1992-03-18 DRSSVDRS   CLL Added "c" to "program" line above.
c>> 1992-03-12 DRSSVDRS   CLL
c     Demo driver for SSVDRS and SCOV3, Singular Value Decomposition
c     and Covariance matrix.
c
c     The sample data was computed as
c     y = 0.5 + 0.25 * sin(2*pi*x) + 0.125 * exp(-x)
c     rounded to four decimal places.
c     ------------------------------------------------------------------
c--S replaces "?": DR?SVDRS, ?SVDRS, ?COV3, ?DOT, ?NRM2
c     ------------------------------------------------------------------
      external SDOT, SNRM2
      real             SDOT, SNRM2
      integer MMAX, NMAX
      parameter(MMAX = 11, NMAX = 3)
      real             A(MMAX,NMAX), B(MMAX), C(NMAX), DOF, PI, RNORM
      real             SING(NMAX), STDDEV
      real             VAR, WORK(2*NMAX), X(MMAX),Y(MMAX)
      integer I, IERR, J, M, N
c
      data X / 0.0e0,  0.1e0,  0.2e0,  0.3e0,  0.4e0,  0.5e0,
     *                 0.6e0,  0.7e0,  0.8e0,  0.9e0,  1.0e0/
      data Y / 0.6250e0,0.7601e0,0.8401e0,0.8304e0,0.7307e0,0.5758e0,
     *                0.4217e0,0.3243e0,0.3184e0,0.4039e0,0.5460e0/
c     ------------------------------------------------------------------
      M = MMAX
      N = NMAX
      print*,' DRSSVDRS..    Demo driver for SSVDRS and SCOV3.'
c
      PI = 4.0e0 * atan(1.0e0)
      do 10 I = 1, M
         A(I,1) = 1.0e0
         A(I,2) = sin(2.0e0 * PI * X(I))
         A(I,3) = exp(-X(I))
         B(I) = Y(I)
   10 continue
 
      call SSVDRS(A, MMAX, M, N, B, MMAX, 1, SING, WORK)
c
      print '(/'' Computed singular values:''/(1x,5g14.6))',
     *   (SING(I),I=1,N)
      print '(/'' V matrix:'')'
      do 15 I = 1, N
         print '(1x,3f12.6)', (A(I,J),J=1,N)
   15 continue
c
      RNORM = SNRM2(M-N, B(N+1), 1)
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
      if(SING(N) .ne. 0.0e0) then
         do 20 I = 1,N
            B(I) = B(I) / SING(I)
   20    continue
         do 30 I = 1,N
            C(I) = SDOT(N, A(I,1), MMAX, B, 1)
   30    continue
 
         print '(1x,''Solution coefficients   ='',3f10.6)',
     *      (C(J),J=1,N)
      end if
c
c                      Compute covariance matrix.
c
      call SCOV3(A, MMAX, N, SING, VAR, WORK, IERR)
      print '(/'' After SCOV3, IERR = '',i4)',IERR
      if(IERR .eq. 0) then
         print '(/'' Covariance matrix:'')'
         do 40 I = 1, N
            print '(1x,3g14.6)', (A(I,J),J=1,N)
   40    continue
      end if
      end
