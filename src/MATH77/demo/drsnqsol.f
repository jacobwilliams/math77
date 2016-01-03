c     program DRSNQSOL
c>> 1996-06-21 DRSNQSOL Krogh  Changes for C conversion.
c>> 1994-11-02 DRSNQSOL Krogh  Changes to use M77CON
c>> 1992-04-15 DRSNQSOL CLL.
c>> 1992-01-14 CLL.
c     Demo driver for SNQSOL.  Also using SCKDER.
c     Expected solution vector:  0.9000518     1.0001835     1.0945009
c     ------------------------------------------------------------------
c--S replaces "?": DR?NQSOL, ?NQSOL, ?NRM2, ?CKDER, ?NQFJ
c     ------------------------------------------------------------------
      external R1MACH, SNQFJ, SNRM2
      integer I, IMAX, IOPT(5), J, JMAX, LWA, M, MODE, N, NMAX
      parameter(NMAX = 3, LWA = 3+(15*NMAX+3*NMAX*NMAX)/2 )
      real             R1MACH, SNRM2
      real             FJAC(NMAX,NMAX), FNORM, FVEC(NMAX)
      real             TEST(NMAX,NMAX), TOL, TSTMAX, WA(LWA), X(NMAX)
      data N / NMAX /
c     ------------------------------------------------------------------
      IOPT(4) = 0
      TOL = sqrt(R1MACH(4))
      X(1) = 3.0E0
      X(2) = 3.0E0
      X(3) = 3.0E0
      print*,
     *  'Program DRSNQSOL.  Demo driver for SNQSOL.  Also using SCKDER.'
 
c     ------------------------------------------------------------------
c               Using SCKDER to check derivative computation.
c     ------------------------------------------------------------------
      print'(/'' Using SCKDER to check derivative computation.'')'
      M  = N
      call SNQFJ(N, X, FVEC ,FJAC, 2)
      MODE = 1
   10 continue
      call SCKDER(MODE, M, N, X, FVEC, FJAC, NMAX, TEST,
     *            IMAX, JMAX, TSTMAX)
      if(MODE .eq. 2) then
         call SNQFJ(N, X, FVEC ,FJAC, 1)
         go to 10
      endif
      call SNQFJ(N, X, FVEC ,FJAC, 1)
      print'(/11x,''X(J) ='',5g11.3:/(17x,5g11.3))',(X(J),J=1,N)
      print'(/1x,''  I    FVEC(I)  '',
     *   '' .....................FJAC(I,J)........................''/)'
      do 20 I = 1,M
         print'(1x,i3,1x,g11.3,1x,5g11.3:/(17x,5g11.3))',
     *         I,FVEC(I),(FJAC(I,J),J=1,N)
   20 continue
      print'(/1x,''TEST(,):''/)'
      do 30 I = 1,M
         print'(1x,i3,13x,5g11.3:/(17x,5g11.3))',I,(TEST(I,J),J=1,N)
   30 continue
      print'(/1x,''IMAX ='',i3,'',    JMAX ='',i3,'',    TSTMAX ='',
     *   g11.3)', IMAX,JMAX,TSTMAX
 
c     ------------------------------------------------------------------
c           Using SNQSOL to solve system of nonlinear equations.
c     ------------------------------------------------------------------
      print
     *  '(/'' Using SNQSOL to solve system of nonlinear equations.'')'
 
      call SNQSOL(SNQFJ, N, X, FVEC, TOL, IOPT, WA, LWA)
 
      FNORM = SNRM2(N,FVEC,1)
c++ CODE for ~.C. is active
      print'('' Termination status:  '',i6/'' NFEV, NJEV:          '',
     * 2i6/ '' Final residual norm: '',g14.3/'' Final X():           ''
     * /(8x,4f14.7))',  IOPT(1), IOPT(2), IOPT(3),
     * FNORM, (X(J), J = 1, N)
c++ CODE for .C. is inactive
c%%   printf( " Termination status:  %6ld\n NFEV, NJEV:          "
c%%   "%6ld%6ld\n Final residual norm: %14.3g\n Final X():           ",
c%%      Iopt[1], Iopt[2], Iopt[3], fnorm);
c%%   for (j = 0; j < n; j+=4){
c%%      printf( "\n        " );
c%%      for (i = j; i < (j < n - 3 ? j + 4 : n); i++)
c%%         printf( "%14.7f", x[i] );}
c%%   printf( "\n" );
c++ END
      stop
      end
c     ==================================================================
      subroutine SNQFJ(N, X, FVEC ,FJAC, IFLAG)
c>> 1992-01-14 CLL.
c     Sample 3-dimensional function of 3 variables for demo of solution
c     of a system of nonlinear equations.
c     ------------------------------------------------------------------
      integer I, IFLAG, N
      real             C1(3), C2(3), C3(3), FJAC(N,N), FVEC(N)
      real             TERM(3), X(N)
      data C1 / -1.0e0,  2.0e0,  2.0e0 /
      data C2 /  2.0e0, -1.0e0,  2.0e0 /
      data C3 /  2.0e0,  2.0e0, -1.0e0 /
      data TERM / 5.01e0, 5.85e0, 8.88e0 /
c     ------------------------------------------------------------------
      if (IFLAG .eq. 1) then
c                                        Compute function vector.
         do 10 I = 1,N
            FVEC(I) = exp(C1(I)*X(1)) + sinh(C2(I)*X(2)) +
     *                tanh(C3(I)*X(3)) - TERM(I)
   10    continue
      elseif (IFLAG .eq. 2) then
c                                        Compute Jacobian matrix.
         do 40 I = 1, N
            FJAC(I,1) = exp(C1(I)*X(1)) * C1(I)
            FJAC(I,2) = cosh(C2(I)*X(2)) * C2(I)
            FJAC(I,3) = (1.0e0/cosh(C3(I)*X(3)))**2 * C3(I)
  40     continue
      endif
      return
      end
