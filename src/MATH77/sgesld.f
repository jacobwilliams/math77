      subroutine SGESLD(A,LDA,N,IPVT,B)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-10-20 SGESLD Krogh  Changes to use M77CON
C>> 1987-08-18 SGESLD Lawson  Initial code.
c--S replaces "?": ?GESLD, ?AXPY

C     This subroutine solves the system of equations  A * X = B
C     using the LU factorization of A given in the array A().
c     ------------------------------------------------------------------
c           Subroutine arguments
C
C     A(,)  [in]  An array of size at least N x N.  On entry must
c         contain the LU factors of an N x N  matrix, A.  It is
c         expected that this factorization will have been computed by
c         use of _GEFA, either directly or indirectly via use of
c         _GEFS or _GEFSC.  This subr will not alter the contents of
c         A(,)
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
c           problem, A * X = B.  On return contains the solution
c           N-vector, X.
c     ------------------------------------------------------------------
C     ERROR CONDITION
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
C        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
C        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
C        setting of LDA.  The user can avoid sending a singular matrix
c        to this subr by testing INFO (set by _GEFS or _GEFA) or
c        RCOND (set by _GEFSC or _GERC) before calling this subr.
c        Nonsingularity is indicated by INFO .eq. 0 or RCOND .ne. ZERO.
C     ------------------------------------------------------------------
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
c     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
c     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
c     Math, Philadelphia, 1979.
c     Adapted from LINPACK for the JPL Math77 library by
c     C. L. Lawson, JPL, Aug 1987.
C     ------------------------------------------------------------------
c     Subprograms referenced: SAXPY
C     ------------------------------------------------------------------
      integer LDA,N,IPVT(N), K, KB, L, NM1
      real             A(LDA,N), B(N), T, ZERO
      parameter ( ZERO = 0.E0 )
C     ------------------------------------------------------------------
      NM1 = N - 1

C        SOLVE  A * X = B
C        FIRST SOLVE  L*Y = B

         DO 20 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L .ne. K) THEN
              B(L) = B(K)
              B(K) = T
            END IF
            CALL SAXPY(N-K,T,A(K+1,K),1,B(K+1),1)
   20    CONTINUE
C
C        NOW SOLVE  U*X = Y
C
         DO 40 KB = 1, N
            K = N + 1 - KB
            IF (A(K,K) .NE. ZERO) THEN
              B(K) = B(K)/A(K,K)
            ELSE
              CALL ERMSG('SGESLD',1,0,'A diagonal element is zero','.')
              RETURN
            END IF
            T = -B(K)
            CALL SAXPY(K-1,T,A(1,K),1,B(1),1)
   40    CONTINUE
      RETURN
      END
