c     program DRSMLC01
c>> 2001-05-22 DRSMLC01 Krogh Minor change for making .f90 version.
c>> 1996-07-08 DRSMLC01 Krogh  Minor format change for C conversion.
c>> 1994-11-02 DRSMLC01 Krogh  Changes to use M77CON
c>> 1994-09-13 DRSMLC01 CLL
c>> 1992-05-13 CLL
c>> 1992-04-15 CLL
c>> 1992-01-21 CLL
c>> 1991-06-10 CLL
c>> 1992-01-16 CLL
c>> 1990-07-12 CLL
c     Demo driver for SMLC01.  Also uses SCKDER to check gradient.
c     Minimization with linear constraints.
c     The constraints are Ax .le. b with equality required in the first
c     two rows.
c     ------------------------------------------------------------------
c--S replaces "?": DR?MLC01, ?MLC01, ?XLOGX, ?CKDER
c     ------------------------------------------------------------------
      external SXLOGX
      integer MMAX, NMAX, LIW, LW
      parameter(MMAX = 3, NMAX = 4)
      parameter(LIW = 4 + MMAX + 2*NMAX)
      parameter(LW  = 3 + MMAX + NMAX*(16 + NMAX))
      integer I, IMAX, IPRINT, IW(LIW), J, JMAX
      integer M, MEQ, MODE, MXEVAL, N
      logical HAVEG
      real             A(MMAX, NMAX), ACC, B(MMAX), FVAL(1), GRAD(NMAX)
      real             GDUMMY(NMAX), TEST(NMAX), TSTMAX, W(LW)
      real             X(NMAX), XL(NMAX), XU(NMAX)
      data M, MEQ, N / 3, 2, 4 /
      data ACC, IPRINT, MXEVAL / 0.0E0, 3, 0 /
      data ((A(I,J),J=1,4),I=1,3) / 1.0E0,  1.0E0, 1.0E0, 1.0E0 ,
     *                              1.0E0, -1.0E0, 0.0E0, 0.0E0,
     *                              0.0E0, -1.0E0, 1.0E0, 0.0E0 /
      data (B(I),I=1,3) / 1.0E0, 0.25E0, -0.1E0 /
      data (XL(J),J=1,4) / 4*1.0E-6 /
      data (XU(J),J=1,4) / 4*1.0E0 /
      data (X(J),J=1,4) / 0.7e0, 0.6e0, 0.5e0, 0.4e0 /
c     ------------------------------------------------------------------
      print*,
     * 'Program DRSMLC01..  Demo driver for SMLC01.  Also uses SCKDER.'
c
c                    Using SCKDER to check the gradient calculation.
c
      MODE = 1
      call SXLOGX( N, X, FVAL(1), GRAD, HAVEG)
   10 continue
      call SCKDER(MODE, 1, N, X, FVAL, GRAD, 1, TEST, IMAX, JMAX,TSTMAX)
         call SXLOGX( N, X, FVAL(1), GDUMMY, HAVEG)
      if(MODE .eq. 2) go to 10
      print'(/'' Using SCKDER to check the gradient calculation.'')'
      print'(/11x,''X(J) ='',5g11.3:/(17x,5g11.3))',(X(J),J=1,N)
      print'(11x,''FVAL ='',5g11.3)',FVAL(1)
      print'(7x,''Gradient ='',5g11.3:/(17x,5g11.3))',(GRAD(J),J=1,N)
      print'(9x,''TEST() ='',5g11.3/(17x,5g11.3))',(TEST(J),J=1,N)
      print'(11x,''JMAX ='',i3,'',          TSTMAX ='',g11.3)',JMAX,
     *     TSTMAX
c
c                Using SMLC01 to solve the minimization problem.
c
      print'(/'' Using SMLC01 to solve the minimization problem.'')'
      call SMLC01 (SXLOGX, N, M, MEQ, A, MMAX, B, XL, XU, X, ACC,
     *             IPRINT, MXEVAL, IW, LIW, W, LW)
      stop
      end
c     ==================================================================
      subroutine SXLOGX( N, X, F, G, HAVEG)
c     ------------------------------------------------------------------
      integer I, N
      real             F, G(N), X(N)
      logical HAVEG
c     ------------------------------------------------------------------
      HAVEG = .true.
      F = 0.0E0
      do 10 I = 1,N
         F = F + X(I) * log(X(I))
         G(I) = 1.0E0 + log(X(I))
   10 continue
      return
      end
