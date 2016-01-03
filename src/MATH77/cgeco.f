      SUBROUTINE CGECO(A,LDA,N,IPVT,RCOND,Z)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 CGECO  Krogh   Added external statement.
C>> 1992-05-18 CLL
C>> 1987-08-20 CGECO  Lawson  Initial code.
C
c     Given an Nth order matrix, A, this subroutine does the following
c     three steps:
c     1. Compute a norm of A for internal use at Step 3.
c     2. Call _GEFA to compute an LU factorization of A that overwrites
c        A in the array A().
c     3. Compute RCOND, an estimate of the reciprocal of the condition
c        number of A.
C     The total computing time for Steps 1 and 3 is relatively
c     economical, being about (9/N) times the time needed for Step 2.
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
C     IPVT()  [out]  An integer array of length at least N into which
c           will be stored record of the row interchanges made during
c           factorization of A.
c
C     RCOND  [out]  An estimate of the reciprocal of the condition
c           number of the original matrix, A.
c           Will satisfy ZERO .le. RCOND .le. ONE.
C           For the system  A*X = B , relative perturmations
C           in  A  and  B  of size  EPSILON  may cause
C           relative perturbations in  X  of size  EPSILON/RCOND .
C           If RCOND is so small that the expression
C                           ONE + RCOND
c           truncated to working precision is not distinguishable from
c           ONE, then A is singular to working precision.
C           In particular, RCOND will be returned as ZERO if  exact
c           singularity is detected or if the estimate underflows.
C
C     Z()  [out]  An array of length at least N.  Needed in this
c           subroutine as work space.  On return will contain a vector
c           Z satisfying NORM(A*Z) = RCOND * NORM(A) * NORM(Z) .
C           If A is close to a singular matrix, then Z will be
C           an approximate null vector.
c     ------------------------------------------------------------------
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
c     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
c     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
c     Math, Philadelphia, 1979.
c     Adapted from LINPACK for the JPL Math77 library by
c     C. L. Lawson, JPL, Aug 1987.
c     ------------------------------------------------------------------
C     Subprograms referenced directly:  CGEFA, CAXPY,CDOTC,CSSCAL,SCASUM
c     ------------------------------------------------------------------
      EXTERNAL CDOTC, SCASUM
      INTEGER LDA,N,IPVT(N)
      COMPLEX A(LDA,N),Z(N),CZERO,CONE
      PARAMETER(CZERO=(0.E0,0.E0),CONE=(1.E0,0.E0))
      REAL RCOND
      COMPLEX CDOTC,EK,T,WK,WKM
      REAL ANORM,S,SCASUM,SM,YNORM,ZERO,ONE
      INTEGER INFO,J,K,KP1,L
      PARAMETER(ZERO=0.E0, ONE=1.E0)
C
      COMPLEX ZDUM,ZDUM1,ZDUM2,CSIGN1
      REAL CABS1
c     ------------------------------------------------------------------
      CABS1(ZDUM) = ABS(REAL(ZDUM)) + ABS(AIMAG(ZDUM))
      CSIGN1(ZDUM1,ZDUM2) = CABS1(ZDUM1)*(ZDUM2/CABS1(ZDUM2))
c     ------------------------------------------------------------------
C
C     COMPUTE 1-NORM OF A
C
      ANORM = ZERO
      DO 10 J = 1, N
         ANORM = MAX(ANORM,SCASUM(N,A(1,J),1))
   10 CONTINUE
C
C     FACTOR
C
      CALL CGEFA(A,LDA,N,IPVT,INFO)
C
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  CTRANS(A)*Y = E .
C     CTRANS(A)  IS THE CONJUGATE TRANSPOSE OF A .
C     THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL
C     GROWTH IN THE ELEMENTS OF W  WHERE  CTRANS(U)*W = E .
C     THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW.
C
C     SOLVE CTRANS(U)*W = E
C
      EK = CONE
      DO 20 J = 1, N
         Z(J) = CZERO
   20 CONTINUE
      DO 100 K = 1, N
c        5/18/92 CLL The following stmt rewritten to avoid a bug in
c        the current SUN Fortran compiler.
c        IF (Z(K) .NE. CZERO) EK = CSIGN1(EK,-Z(K))
         if(real(Z(K)) .ne. 0.0e0 .or. aimag(Z(K)) .ne. 0.0e0)
     *      EK = CSIGN1(EK,-Z(K))
          IF (CABS1(EK-Z(K)) .GT. CABS1(A(K,K))) THEN
            S = CABS1(A(K,K))/CABS1(EK-Z(K))
            CALL CSSCAL(N,S,Z,1)
            EK = S * EK
         END IF
         WK = EK - Z(K)
         WKM = -EK - Z(K)
         S = CABS1(WK)
         SM = CABS1(WKM)
c        5/18/92 CLL The following stmt rewritten to avoid a bug in
c        the current SUN Fortran compiler.
c        IF (A(K,K) .NE. CZERO) THEN
         if(real(A(K,K)) .ne. 0.0e0 .or. aimag(A(K,K)) .ne. 0.0e0) then
            WK = WK/CONJG(A(K,K))
            WKM = WKM/CONJG(A(K,K))
         ELSE
            WK = CONE
            WKM = CONE
         END IF
         KP1 = K + 1
            DO 60 J = KP1, N
               SM = SM + CABS1(Z(J)+WKM*CONJG(A(K,J)))
               Z(J) = Z(J) + WK*CONJG(A(K,J))
               S = S + CABS1(Z(J))
   60       CONTINUE
            IF (S .LT. SM) THEN
               T = WKM - WK
               WK = WKM
               DO 70 J = KP1, N
                  Z(J) = Z(J) + T*CONJG(A(K,J))
   70          CONTINUE
            END IF
         Z(K) = WK
  100 CONTINUE
      S = ONE/SCASUM(N,Z,1)
      CALL CSSCAL(N,S,Z,1)
C
C     SOLVE CTRANS(L)*Y = W
C
      DO 120 K = N,1,-1
         IF (K .LT. N) Z(K) = Z(K) + CDOTC(N-K,A(K+1,K),1,Z(K+1),1)
         IF (CABS1(Z(K)) .GT. ONE) THEN
            S = 1.0E0/CABS1(Z(K))
            CALL CSSCAL(N,S,Z,1)
         END IF
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
  120 CONTINUE
      S = ONE/SCASUM(N,Z,1)
      CALL CSSCAL(N,S,Z,1)
C
      YNORM = ONE
C
C     SOLVE L*V = Y
C
      DO 140 K = 1, N
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
         IF (K .LT. N) CALL CAXPY(N-K,T,A(K+1,K),1,Z(K+1),1)
         IF (CABS1(Z(K)) .GT. ONE) THEN
            S = 1.0E0/CABS1(Z(K))
            CALL CSSCAL(N,S,Z,1)
            YNORM = S*YNORM
         END IF
  140 CONTINUE
      S = ONE/SCASUM(N,Z,1)
      CALL CSSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
C     SOLVE  U*Z = V
C
      DO 160 K = N,1,-1
         IF (CABS1(Z(K)) .GT. CABS1(A(K,K))) THEN
            S = CABS1(A(K,K))/CABS1(Z(K))
            CALL CSSCAL(N,S,Z,1)
            YNORM = S*YNORM
         END IF
c        5/18/92 CLL The following stmt rewritten to avoid a bug in
c        the current SUN Fortran compiler.
c        IF (A(K,K) .NE. CZERO) THEN
         if(real(A(K,K)) .ne. 0.0e0 .or. aimag(A(K,K)) .ne. 0.0e0) then
           Z(K) = Z(K)/A(K,K)
         ELSE
           Z(K) = CONE
         END IF
         T = -Z(K)
         CALL CAXPY(K-1,T,A(1,K),1,Z(1),1)
  160 CONTINUE
C     MAKE ZNORM = 1.0
      S = ONE/SCASUM(N,Z,1)
      CALL CSSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
      IF (ANORM .EQ. 0.0E0) THEN
        RCOND = 0.0E0
      ELSE
        RCOND = YNORM/ANORM
      END IF
      RETURN
      END
