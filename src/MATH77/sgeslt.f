      subroutine SGESLT(A,LDA,N,IPVT,B)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 SGESLT Krogh  Added external statement.
C>> 1994-10-20 SGESLT Krogh  Changes to use M77CON
C>> 1987-08-18 SGESLT Lawson  Initial code.
c--S replaces "?": ?GESLT, ?DOT
C
C     This subroutine solves the system of equations  (A**t) * X = B
C     using the LU factorization of A given in the array A().
c     Here A**t denotes the transpose of A.
c     ------------------------------------------------------------------
c                        Subroutine arguments
C
C     A(,)  [in]  An array of size at least N x N.  On entry must
c            contain the LU factors of an N x N  matrix, A.  It is
c            expected that this factorization will have been computed by
c            use of _GEFA, either directly or indirectly via use of
c            _GEFS or _GEFSC.  This subr will not alter the contents of
c            A(,)
C
C     LDA  [in]  Leading dimensioning parameter for the array A(,).
C
C     N  [in]  The order of the original matrix, A.
C
C     IPVT()  [in]  An integer array of length at least N, containg a
c           record of the row interchanges made during factorization of
c           A.
C
c     B()  [inout]  On entry contains the right-side N-vector for the
c           problem, (A**t) * X = B.  On return contains the solution
c           N-vector, X.
c     ------------------------------------------------------------------
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
C        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
C        setting of LDA.  The user can avoid sending a singular matrix
c        to this subr by testing INFO (set by _GEFS or _GEFA) or
c        RCOND (set by _GEFSC or _GERC) before calling this subr.
c        Nonsingularity is indicated by INFO .eq. 0 or RCOND .ne. ZERO.
c     ------------------------------------------------------------------
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
c     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
c     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
c     Math, Philadelphia, 1979.
c     Adapted from LINPACK for the JPL Math77 library by
c     C. L. Lawson, JPL, Aug 1987.
c     ------------------------------------------------------------------
c     Subprograms referenced: SDOT
C     ------------------------------------------------------------------
      external SDOT
      integer LDA, N, IPVT(N), K, KB, L, NM1
      real             A(LDA,N), B(N), SDOT, T, ZERO
      parameter (ZERO = 0.E0)
C     ------------------------------------------------------------------
      NM1 = N - 1
C
C        SOLVE  (A**t) * X = B
C        FIRST SOLVE  (U**t)*Y = B
C
         DO 60 K = 1, N
            T = SDOT(K-1,A(1,K),1,B(1),1)
            IF (A(K,K) .NE. ZERO) THEN
              B(K) = (B(K) - T)/A(K,K)
            ELSE
              CALL ERMSG('SGESLT',1,0,'A diagonal element is zero','.')
              RETURN
            END IF
   60    CONTINUE
C
C        NOW SOLVE (L**t)*X = Y
C
         DO 80 KB = 1, NM1
            K = N - KB
            B(K) = B(K) + SDOT(N-K,A(K+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF (L .NE. K) THEN
               T = B(L)
               B(L) = B(K)
               B(K) = T
            END IF
   80    CONTINUE
      RETURN
      END
