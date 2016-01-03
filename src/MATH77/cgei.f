      subroutine CGEI(A,LDA,N,IPVT,WORK)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1992-05-18 CLL
C>> 1987-08-14 CGEI   Lawson  Initial code.
C
c     This subroutine computes the inverse of the N x N matrix, A,
C     using the LU factorization of A given in the array A().
c     ------------------------------------------------------------------
c                        Subroutine arguments
C
C     A(,)  [inout]  An array of size at least N x N.  On entry must
c            contain the LU factors of an N x N  matrix, A.  It is
c            expected that this factorization will have been computed by
c            use of _GEFA, either directly or indirectly via use of _GEC
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
c        RCOND (set by _GEFSC or _GECO) before calling this subr.
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
c     Subprograms referenced: CAXPY, CSCAL, CSWAP
C     ------------------------------------------------------------------
      integer LDA,N,IPVT(N),I,J,K,KB,KP1,L,NM1
      complex A(LDA,N), WORK(N), T, ONE, ZERO
      parameter(ONE = (1.E0,0.E0), ZERO = (0.E0,0.E0))
C     ------------------------------------------------------------------
C     COMPUTE INVERSE(U)
C
         DO 100 K = 1, N
c           5/18/92 CLL The following stmt rewritten to avoid a bug in
c           the current SUN Fortran compiler.
c           IF (A(K,K) .NE. ZERO) THEN
            if(real(A(K,K)) .ne. 0.0e0 .or.
     *          aimag(A(K,K)) .ne. 0.0e0) then
              A(K,K) = ONE/A(K,K)
            ELSE
              CALL ERMSG('CGEI',1,0,'A diagonal element is zero','.')
              RETURN
            END IF
            T = -A(K,K)
            CALL CSCAL(K-1,T,A(1,K),1)
            KP1 = K + 1
            DO 80 J = KP1, N
               T = A(K,J)
               A(K,J) = ZERO
               CALL CAXPY(K,T,A(1,K),1,A(1,J),1)
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
               CALL CAXPY(N,T,A(1,J),1,A(1,K),1)
  120       CONTINUE
            L = IPVT(K)
            IF (L .NE. K) CALL CSWAP(N,A(1,K),1,A(1,L),1)
  130    CONTINUE
      RETURN
      END
