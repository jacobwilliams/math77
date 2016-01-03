      subroutine DGECO(A,LDA,N,IPVT,RCOND,Z)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 DGECO  Krogh  Added external statement.
C>> 1994-10-20 DGECO  Krogh  Changes to use M77CON
C>> 1987-08-18 DGECO  Lawson  Initial code.
c--D replaces "?": ?GECO, ?GEFA, ?AXPY,?DOT,?SCAL,?ASUM
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
C     IPVT()  [out]  An integer array of length at least N, into which
c           will be stored a record of the row interchanges made during
c           factorization of A.
c
C     RCOND  [out]  An estimate of the reciprocal of the condition
c           number of the matrix, A.
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
C     Subprograms referenced directly:  DGEFA, DAXPY,DDOT,DSCAL,DASUM
c     ------------------------------------------------------------------
      external DASUM, DDOT
      integer LDA,N,IPVT(N)
      integer J,K,KB,KP1,L, INFO
      double precision A(LDA,N),Z(N), RCOND
      double precision DDOT, DASUM
      double precision EK,T,WK,WKM, ANORM,S,SM,YNORM,ZERO,ONE
      parameter(ZERO = 0.0D0, ONE = 1.0D0)
c     ------------------------------------------------------------------
C     COMPUTE 1-NORM OF A
C
      ANORM = ZERO
      do 10 J = 1, N
         ANORM = max(ANORM,DASUM(N,A(1,J),1))
   10 continue
c
c                      Replace A by its LU factorization
c
      call DGEFA(A, LDA, N, IPVT, INFO)
C
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .
C     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE
C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
C     OVERFLOW.
C
C     SOLVE TRANS(U)*W = E
C
      EK = ONE
      do 20 J = 1, N
         Z(J) = ZERO
   20 continue
      do 100 K = 1, N
         if (Z(K) .ne. ZERO) EK = sign(EK,-Z(K))
         if (abs(EK-Z(K)) .gt. abs(A(K,K))) then
            S = abs(A(K,K))/abs(EK-Z(K))
            CALL DSCAL(N,S,Z,1)
            EK = S*EK
         endif
         WK = EK - Z(K)
         WKM = -EK - Z(K)
         S = abs(WK)
         SM = abs(WKM)
         if (A(K,K) .ne. ZERO) then
            WK = WK/A(K,K)
            WKM = WKM/A(K,K)
         else
            WK = ONE
            WKM = ONE
         endif
         KP1 = K + 1
            do 60 J = KP1, N
               SM = SM + abs(Z(J)+WKM*A(K,J))
               Z(J) = Z(J) + WK*A(K,J)
               S = S + abs(Z(J))
   60       continue
            if (S .lt. SM) then
               T = WKM - WK
               WK = WKM
               do 70 J = KP1, N
               Z(J) = Z(J) + T*A(K,J)
   70          continue
            endif
         Z(K) = WK
  100 continue
      S = ONE/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
C
C     SOLVE TRANS(L)*Y = W
C
      do 120 KB = 1, N
         K = N + 1 - KB
         if (K .lt. N) Z(K) = Z(K) + DDOT(N-K,A(K+1,K),1,Z(K+1),1)
         if (abs(Z(K)) .gt. ONE) then
            S = ONE/abs(Z(K))
            CALL DSCAL(N,S,Z,1)
         endif
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
  120 continue
      S = ONE/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
C
      YNORM = ONE
C
C     SOLVE L*V = Y
C
      do 140 K = 1, N
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
         if (K .lt. N) CALL DAXPY(N-K,T,A(K+1,K),1,Z(K+1),1)
         if (abs(Z(K)) .gt. ONE) then
            S = ONE/abs(Z(K))
            CALL DSCAL(N,S,Z,1)
            YNORM = S*YNORM
         endif
  140 continue
      S = ONE/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
C     SOLVE  U*Z = V
C
      do 160 KB = 1, N
         K = N + 1 - KB
         if (abs(Z(K)) .gt. abs(A(K,K))) then
            S = abs(A(K,K))/abs(Z(K))
            CALL DSCAL(N,S,Z,1)
            YNORM = S*YNORM
         endif
         if (A(K,K) .ne. ZERO) then
            Z(K) = Z(K)/A(K,K)
         else
            Z(K) = ONE
         endif
         T = -Z(K)
         CALL DAXPY(K-1,T,A(1,K),1,Z(1),1)
  160 continue
C                              MAKE ZNORM = 1.0
      S = ONE/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
      if (ANORM .eq. ZERO) THEN
        RCOND = ZERO
      else
        RCOND = YNORM/ANORM
      endif
      return
      end
