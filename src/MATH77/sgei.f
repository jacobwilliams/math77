      subroutine SGEI(A,LDA,N,IPVT,WORK)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-10-20 SGEI   Krogh  Changes to use M77CON
C>> 1987-08-18 SGEI   Lawson  Initial code.
c--S replaces "?": ?GEI, ?AXPY, ?SCAL, ?SWAP
C
c     This subroutine computes the inverse of the N x N matrix, A,
C     using the LU factorization of A given in the array A().
c     ------------------------------------------------------------------
c                        Subroutine arguments
C
C     A(,)  [inout]  An array of size at least N x N.  On entry must
c            contain the LU factors of an N x N  matrix, A.  It is
c            expected that this factorization will have been computed by
c            use of _GEFA, either directly or indirectly via use of
c            _GEFS or _GEFSC.
c            On return contains the N x N inverse matrix of A.
C
C     LDA  [in]  Leading dimensioning parameter for the array A(,).
C
C     N  [in]  The order of the original matrix, A.
C
C     IPVT()  [in]  An integer array of length at least N, containg a
c           record of the row interchanges made during factorization of
c           A.
C
c     WORK()  [scratch]  An array of length at least N used by this
c        subroutine as working space.
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
c     Subprograms referenced: SAXPY, SSCAL, SSWAP
C     ------------------------------------------------------------------
      integer LDA,N,IPVT(N), I, J, K, KB, KP1, L, NM1
      real             A(LDA,N), WORK(N), T, ONE, ZERO
      parameter(ONE = 1.0E0, ZERO = 0.0E0)
C     ------------------------------------------------------------------
C     COMPUTE INVERSE(U)
C
         DO 100 K = 1, N
            IF (A(K,K) .NE. ZERO) THEN
              A(K,K) = ONE/A(K,K)
            ELSE
              CALL ERMSG('SGEI',1,0,'A diagonal element is zero','.')
              RETURN
            END IF
            T = -A(K,K)
            CALL SSCAL(K-1,T,A(1,K),1)
            KP1 = K + 1
            DO 80 J = KP1, N
               T = A(K,J)
               A(K,J) = ZERO
               CALL SAXPY(K,T,A(1,K),1,A(1,J),1)
   80       CONTINUE
  100    CONTINUE
C
C        FORM INVERSE(U)*INVERSE(L)
C
         NM1 = N - 1
         DO 130 KB = 1, NM1
            K = N - KB
            KP1 = K + 1
            DO 110 I = KP1, N
               WORK(I) = A(I,K)
               A(I,K) = ZERO
  110       CONTINUE
            DO 120 J = KP1, N
               T = WORK(J)
               CALL SAXPY(N,T,A(1,J),1,A(1,K),1)
  120       CONTINUE
            L = IPVT(K)
            IF (L .NE. K) CALL SSWAP(N,A(1,K),1,A(1,L),1)
  130    CONTINUE
      RETURN
      END
