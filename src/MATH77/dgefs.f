      subroutine DGEFS(A,LDA,N,B,LDB,NB,IPVT,INFO)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 DGEFS  Krogh   Declared all vars.
C>> 1994-10-20 DGEFS  Krogh  Changes to use M77CON
C>> 1987-08-18 DGEFS  Lawson  Initial code.
c--D replaces "?": ?GEFS, ?GEFA, ?GESLD
c
c     Solves a system of linear equations,  A * X = B,
c     where A is a square nonsingular matrix of order N and B is an
c     N by NB matrix.  The solution is the N by NB matrix X that will
c     be stored on return in place of B in the array B().
c     ------------------------------------------------------------------
c     Uses subroutines derived from LINPACK.
c     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
c     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
c     Math, Philadelphia, 1979.
c     Adapted for the JPL Math77 library by C. L. Lawson, JPL, Aug 1987.
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
c     B(,)  [inout]  On entry contains the N by NB matrix, B.  On return
c           contains the N by NB solution matrix, X.  Could be a
c           Singly subscripted array if NB .eq. 1.
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
c     INFO  [out]  Set to zero if all diagonal elements in the U matrix
c           of the LU factorization are found to be nonzero.  If nonzero
c           it is the index of the first diagonal element of U that was
c           found to be zero.  In this latter case the solution X will
c           not be computed.
c     ------------------------------------------------------------------
c     Subprograms called: DGEFA, DGESLD
c     ------------------------------------------------------------------
      integer LDA, N, LDB, NB, IPVT(N), INFO, J
      double precision A(LDA,N), B(LDB,*)
c     ------------------------------------------------------------------
      call DGEFA(A,LDA,N,IPVT,INFO)
      if( INFO .ne. 0) return
      do 10 J = 1,NB
         call DGESLD(A,LDA,N,IPVT,B(1,J))
   10 continue
      return
      END
