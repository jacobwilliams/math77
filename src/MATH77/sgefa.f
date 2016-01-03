      subroutine SGEFA(A,LDA,N,IPVT,INFO)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 SGEFA  Krogh  Added external statement.
C>> 1994-10-20 SGEFA  Krogh  Changes to use M77CON
C>> 1987-08-18 SGEFA  Lawson  Initial code.
c--S replaces "?": ?GEFA, ?SCAL, ?AXPY, I?AMAX
C
C     SGEFA computes the LU factorization of the N x N matrix A by
c     Gaussian elimination.  This produces matrices, L and U, that
c     satisfy L * U = A, where U is an upper triangular matrix and
c     L is a permutation of a lower triangular matrix. Use of this
c     subroutine would typically be followed by use of other
c     subroutines that would use this factorization to solve a
c     system of linear equations, or to compute the inverse matrix
c     or the determinant of A.
C
C     SGEFA may be referenced indirectly via _GECO, but it can be called
C     directly with a saving in time if the reciprocal condition number
C     RCOND is not needed.
C     (Time for _GECO) = (1 + 9/N)*(Time for SGEFA) .
C
c     ------------------------------------------------------------------
c                        Subroutine arguments
C
C     A(,)  [inout]  An array of size at least N x N.  On entry must
c            contain an N x N matrix, A, to be factored.  On return will
c            contain the LU factors of A.
C
C     LDA  [in]  Leading dimensioning parameter for the array A(,).
C
C     N  [in]  The order of the matrix, A.
C
C     IPVT()  [in]  An integer array of length at least N, containg a
c           record of the row interchanges made during factorization of
c           A.
c
C      INFO  [out]  Indicate status on return
C              = 0  NORMAL VALUE.
C              = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
C              CONDITION FOR THIS SUBROUTINE, but it does
C              indicate that the usual following steps to solve
c              equations or compute an inverse matrix cannot
c              be done, at least by the usual straightforward
c              algorithms.  Use RCOND in _GECO for a more reliable
C              indication of singularity.
c     ------------------------------------------------------------------
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
c     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
c     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
c     Math, Philadelphia, 1979.
c     Adapted from LINPACK for the JPL Math77 library by
c     C. L. Lawson, JPL, Aug 1987.
c     ------------------------------------------------------------------
C     Subprograms referenced: SAXPY,SSCAL,ISAMAX
c     ------------------------------------------------------------------
      external ISAMAX
      integer ISAMAX
      integer LDA,N,IPVT(N),INFO, J,K,KP1,L,NM1
      real             A(LDA,N), T, ONE, ZERO
      parameter(ONE=1.E0, ZERO=0.E0)
c     ------------------------------------------------------------------
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
      INFO = 0
      NM1 = N - 1
      DO 60 K = 1, NM1
         KP1 = K + 1
C
C        FIND L = PIVOT INDEX
C
         L = ISAMAX(N-K+1,A(K,K),1) + K - 1
         IPVT(K) = L
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
C
         IF (A(L,K) .EQ. ZERO) THEN
           INFO = K
         ELSE
C           INTERCHANGE IF NECESSARY
           IF (L .NE. K) THEN
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
           END IF
C
C           COMPUTE MULTIPLIERS
C
            T = -ONE/A(K,K)
            CALL SSCAL(N-K,T,A(K+1,K),1)
C
C           ROW ELIMINATION WITH COLUMN INDEXING
C
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .NE. K) THEN
                  A(L,J) = A(K,J)
                  A(K,J) = T
               END IF
               CALL SAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30       CONTINUE
         ENDIF
   60 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. ZERO) INFO = N
      RETURN
      END
