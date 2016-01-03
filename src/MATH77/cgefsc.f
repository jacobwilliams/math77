      subroutine CGEFSC(A,LDA,N,B,LDB,NB,IPVT,RCOND,Z)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 CGEFSC  Krogh   Declared all vars.
C>> 1988-05-11 CGEFSC Lawson  Initial code.
c
c     Solves a system of linear equations,  A * X = B,
c     where A is a square nonsingular matrix of order N and B is an
c     N by NB matrix.  The solution is the N by NB matrix X that will
c     be stored on return in place of B in the array B().
c     Sets RCOND to an estimate of the reciprocal of the condition
c     number of A.
c     ------------------------------------------------------------------
c     Uses subroutines derived from LINPACK.
c     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
c     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
c     Math, Philadelphia, 1979.
c     Adapted for the JPL Math77 library by C. L. Lawson, JPL, Aug 1987.
c     CLL May 1988 Changed RCOND1 to RCOND in type stmt.
c     ------------------------------------------------------------------
c                    Subroutine arguments
c
c     A(,)  [inout]  On entry contains the N by N matrix A.
c           On return contains the LU factorization of A as computed by
c           LINPACK subroutines.
c
c     LDA  [in]  Leading dimensioning parameter for the array A(,).
c           Require LDA .ge. N.
c
c     N  [in]  The order of the matrix A and number of rows in the
c           matrices B and X.
c
c     B(,)  [inout]  On entry contains the N by NB matrix, B. The array
c           B() could be a singly subscripted array if NB .eq. 1.
c           On return, if RCOND .ne. ZERO, B() will contain the N by NB
c           solution matrix, X.   If RCOND .eq. ZERO the solution will
c           not be computed and B() will be unaltered.
c
c     LDB  [in]  Leading dimensioning parameter for the array B(,).
c           Require LDB .ge. N.
c
c     NB  [in]  Number of columns in the matrices B and X.  If NB .lt. 1
c           the matrix, A, will be factored but no reference will be
c           made to the array B(,).
c
c     IPVT()  [out]  Integer array of length at least N.  On return will
c           contain a record of the row interchanges done during
c           factorization of A.
c
c     RCOND  [out]  On return contains an estimate of the reciprocal of
c           the condition number of the given matrix, A.  Will be in the
c           range from zero to one.  Zero indicates an exactly singular
c           matrix.  A larger RCOND indicates a better conditioned
c           matrix.  If RCOND .eq. ZERO, the solution X will not be
c           computed.
c
c     Z()  [scratch]  Array of length at least N.  Used as work space.
c     ------------------------------------------------------------------
c     Subroutines called: CGECO, CGESLD
c     ------------------------------------------------------------------
      integer LDA, N, LDB, NB, IPVT(N), J
      complex A(LDA,N), B(LDB,*), Z(N)
      real RCOND, ZERO
      parameter(ZERO = 0.0E0)
c     ------------------------------------------------------------------
c                                    Compute LU factorization of A
c                                and compute recip. cond. no. of A.
      call CGECO(A,LDA,N,IPVT,RCOND,Z)
      if( RCOND .eq. ZERO ) return
c                                    Solve equations.
      do 10 J = 1,NB
         call CGESLD(A,LDA,N,IPVT,B(1,J))
   10 continue
      return
      END
