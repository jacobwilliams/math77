c     program DRSHFTI
c>> 2001-05-22 DRSHFTI  Krogh Minor change for making .f90 version.
c>> 1996-07-03 DRSHFTI  Krogh  Special code for C conversion.
c>> 1994-10-19 DRSHFTI  Krogh  Changes to use M77CON
c>> 1987-12-09 DRSHFTI  Lawson  Initial Code.
c                       Demo driver for SHFTI and SCOV2
c--S replaces "?": DR?HFTI, ?HFTI, ?COV2
c
c     The sample data was computed as
c     y = 0.5 + 0.25 * sin(2*pi*x) + 0.125 * exp(-x)
c     rounded to four decimal places.
c     ------------------------------------------------------------------
c++ Code for .C. is inactive
c%%   long int k;
c++ End
      integer MMAX, NMAX
      real             ZERO, ONE, TWO, FOUR
      parameter(MMAX=11, NMAX=3)
      parameter(ZERO = 0.0E0, ONE = 1.0E0, TWO = 2.0E0, FOUR = 4.0E0)
      real             X(MMAX),Y(MMAX),A(MMAX,NMAX),C(MMAX)
      real             WORK(NMAX), RNORM(1)
      real             DOF, PI, STDDEV, TAU, VAR
      integer          IP(NMAX), KRANK, I, IERR, J, NC, M, N
c     ------------------------------------------------------------------
      data X / 0.0E0,  0.1E0,  0.2E0,  0.3E0,  0.4E0,  0.5E0,
     *                 0.6E0,  0.7E0,  0.8E0,  0.9E0,  1.0E0/
      data Y / 0.6250E0,0.7601E0,0.8401E0,0.8304E0,0.7307E0,0.5758E0,
     *                0.4217E0,0.3243E0,0.3184E0,0.4039E0,0.5460E0/
      data M, N, NC, TAU/ MMAX, NMAX, 1, 0.0E0/
c     ------------------------------------------------------------------
      PI = FOUR * atan(ONE)
      do 20 I = 1, M
         A(I,1) = ONE
         A(I,2) = sin(TWO * PI * X(I))
         A(I,3) = exp(-X(I))
         C(I) = Y(I)
   20 continue
 
      call SHFTI (A,MMAX,M,N,C,MMAX,NC,TAU,KRANK,RNORM,WORK,IP)
      DOF = M - N
      STDDEV = RNORM(1) / sqrt(DOF)
      VAR = STDDEV**2
      print '(1x,''Rank of linear system   ='',i4)', KRANK
      print '(1x,''Std. Dev. of data error ='',f10.6)', STDDEV
      print '(1x,''Solution coefficients   ='',3f10.6)', (C(J),J=1,N)
 
      call SCOV2( A, MMAX, N, IP, VAR, IERR)
      print '(1x,''Error flag from SCOV2   ='',i4)', IERR
      print '('' Covariance matrix of computed coefficients:'')'
      print '(1X)'
c++ Code for ~.C. is active
      do 30 I = 1,N
         print '(1x,3(3x,2i3,g16.8))', (I,J,A(I,J),J=I,N)
   30 continue
c++ Code for .C. is inactive
c%%   for (i = 0; i < n; i++){
c%%      for (j = i; j < n; j+=3){
c%%         for (k = j; k < (j < n - 3 ? j+3 : n); k++)
c%%              printf( "   %3ld%3ld%16.8g", i+1, k+1, a[k][i] );
c%%         printf( "\n" );}
c%%      }
c++ End
      stop
      end
