c     program DRDMLC01
c>> 2001-05-22 DRDMLC01 Krogh Minor change for making .f90 version.
c>> 1996-07-08 DRDMLC01 Krogh  Minor format change for C conversion.
c>> 1994-11-02 DRDMLC01 Krogh  Changes to use M77CON
c>> 1994-09-13 DRDMLC01 CLL
c>> 1992-05-13 CLL
c>> 1992-04-15 CLL
c>> 1992-01-21 CLL
c>> 1991-06-10 CLL
c>> 1992-01-16 CLL
c>> 1990-07-12 CLL
c     Demo driver for DMLC01.  Also uses DCKDER to check gradient.
c     Minimization with linear constraints.
c     The constraints are Ax .le. b with equality required in the first
c     two rows.
c     ------------------------------------------------------------------
c--D replaces "?": DR?MLC01, ?MLC01, ?XLOGX, ?CKDER
c     ------------------------------------------------------------------
      external DXLOGX
      integer MMAX, NMAX, LIW, LW
      parameter(MMAX = 3, NMAX = 4)
      parameter(LIW = 4 + MMAX + 2*NMAX)
      parameter(LW  = 3 + MMAX + NMAX*(16 + NMAX))
      integer I, IMAX, IPRINT, IW(LIW), J, JMAX
      integer M, MEQ, MODE, MXEVAL, N
      logical HAVEG
      double precision A(MMAX, NMAX), ACC, B(MMAX), FVAL(1), GRAD(NMAX)
      double precision GDUMMY(NMAX), TEST(NMAX), TSTMAX, W(LW)
      double precision X(NMAX), XL(NMAX), XU(NMAX)
      data M, MEQ, N / 3, 2, 4 /
      data ACC, IPRINT, MXEVAL / 0.0D0, 3, 0 /
      data ((A(I,J),J=1,4),I=1,3) / 1.0D0,  1.0D0, 1.0D0, 1.0D0 ,
     *                              1.0D0, -1.0D0, 0.0D0, 0.0D0,
     *                              0.0D0, -1.0D0, 1.0D0, 0.0D0 /
      data (B(I),I=1,3) / 1.0D0, 0.25D0, -0.1D0 /
      data (XL(J),J=1,4) / 4*1.0D-6 /
      data (XU(J),J=1,4) / 4*1.0D0 /
      data (X(J),J=1,4) / 0.7d0, 0.6d0, 0.5d0, 0.4d0 /
c     ------------------------------------------------------------------
      print*,
     * 'Program DRDMLC01..  Demo driver for DMLC01.  Also uses DCKDER.'
c
c                    Using DCKDER to check the gradient calculation.
c
      MODE = 1
      call DXLOGX( N, X, FVAL(1), GRAD, HAVEG)
   10 continue
      call DCKDER(MODE, 1, N, X, FVAL, GRAD, 1, TEST, IMAX, JMAX,TSTMAX)
         call DXLOGX( N, X, FVAL(1), GDUMMY, HAVEG)
      if(MODE .eq. 2) go to 10
      print'(/'' Using DCKDER to check the gradient calculation.'')'
      print'(/11x,''X(J) ='',5g11.3:/(17x,5g11.3))',(X(J),J=1,N)
      print'(11x,''FVAL ='',5g11.3)',FVAL(1)
      print'(7x,''Gradient ='',5g11.3:/(17x,5g11.3))',(GRAD(J),J=1,N)
      print'(9x,''TEST() ='',5g11.3/(17x,5g11.3))',(TEST(J),J=1,N)
      print'(11x,''JMAX ='',i3,'',          TSTMAX ='',g11.3)',JMAX,
     *     TSTMAX
c
c                Using DMLC01 to solve the minimization problem.
c
      print'(/'' Using DMLC01 to solve the minimization problem.'')'
      call DMLC01 (DXLOGX, N, M, MEQ, A, MMAX, B, XL, XU, X, ACC,
     *             IPRINT, MXEVAL, IW, LIW, W, LW)
      stop
      end
c     ==================================================================
      subroutine DXLOGX( N, X, F, G, HAVEG)
c     ------------------------------------------------------------------
      integer I, N
      double precision F, G(N), X(N)
      logical HAVEG
c     ------------------------------------------------------------------
      HAVEG = .true.
      F = 0.0D0
      do 10 I = 1,N
         F = F + X(I) * log(X(I))
         G(I) = 1.0D0 + log(X(I))
   10 continue
      return
      end
