      subroutine DHFTI (A,LDA,M1,N1,B,LDB,KB,TAU,KRANK,RNORM,WORK,IP)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2006-03-20 DHFTI Krogh  Changed LDB to LDA in an error message.
c>> 1996-03-30 DHFTI Krogh  Added external statement.
C>> 1994-10-20 DHFTI Krogh  Changes to use M77CON
C>> 1994-04-20 DHFTI CLL Edited to make DP & SP files similar.
c>> 1993-02-09 CLL.  Fixed index in 1st reference to [D/S]NRM2.
C>> 1992-03-13 DHFTI  FTK  Removed implicit statements.
C>> 1987-11-24 DHFTI  Lawson  Initial code.
c--D replaces "?": ?HFTI, ?HTCC, ?HTGEN, ?DOT, ?NRM2
c
c     ------------------------------------------------------------------
c          This subr solves the least squares problem
c
c                          A * X  ~=~  B
c
c     where A is a given M x N matrix, B is a given M x KB matrix and
c     X is the N x KB solution matrix to be determined.  This includes
c     the usual special case of KB = 1 where B is an M-vector and the
c     solution, X, is an N-vector.
c
c          This subr permits M > N, M = N, or M < N.  This subr
c     determines the "pseudorank", i.e. the estimated rank, of A based
c     on a user-provided tolerance.  If the pseudorank is less than N,
c     the minimal length solution, i.e. the pseudoinverse solution, to
c     the problem is computed.
c
c          Note that this subr can be used to compute the pseudoinverse
c     of a matrix, A.  Set B to the M x M identity matrix and the
c     solution matrix, X, will be the pseudoinverse of A.
c
c          The algorithm is HFTI from the L & H book.  This method does
c     a Householder QR decomposition from the left.  Then if the
c     pseudorank is less than N it does a second Householder QR
c     decomposition from the right.
c
c          The results returned in A(,), RNORM(), and IP() can be used
c     by subroutine SCOV1 or DCOV1 to compute the covariance matrix of
c     the solution vectors.
c     ------------------------------------------------------------------
c                     SUBROUTINE ARGUMENTS
c
c     A(,)     (In/Out)  On input, contains the M x N matrix, A.  Permit
c              M > N, M = N, or M < N.  On return A(,) will contain an
c              upper triangular matrix of order KRANK that can be used
c              by subr _COV2 to compute a covariance matrix when
c              KRANK = N.
c
c     LDA      (In)  The first dimension of the array A(,).
c              Require LDA .ge. M.
c
c     M        (In)  No. of rows of matrices A and B.  Require M .ge. 1.
c
c     N        (In)  No. of columns of matrix A, and rows of matrix X.
c              Require N .ge. 0.
c
c     B(,)     (In/Out)  If KB > 0, the array B(,) must initially
c              contain the right-side matrix, B, having M rows and KB
c              columns.  On return the array B(,) will contain the
c              N x KB solution matrix X.
c              If KB = 0, this subr will not reference the array B(,).
c
c     LDB      (In)  First dimensioning parameter for the array B(,).
c              If KB > 0, require LDB .ge. Max( M, N).
c              If KB = 0, require LDB .ge. 1.
c
c     KB       (In)  No. of columns of the matrices B and X.
c              Require KB .ge. 0.
c              If KB = 0, this subr will not reference the array B(,).
c
c     TAU      (In)  Absolute tolerance parameter provided by user for
c              pseudorank determination.
c
c     KRANK    (Out)  Set by subr to indicate the pseudorank of A.
c              This means that the first KRANK diagonal elements in the
c              the upper triangular factor matrix derived from A each
c              exceed TAU in magnitude.  Either KRANK = Min( M, N), or
c              the the magnitude of the diagonal element in position
c              KRANK + 1 is less than or equal to TAU.
c
c     RNORM()  (Out)  On return, RNORM(J) will contain the euclidean
c              norm of the residual vector for the problem defined by
c              the Jth column vector of the input matrix, B, for
c              J = 1, ..., KB.
c
c     WORK()  (Scratch)  Array used for work space by this subr.
c             Must be of length at least N.
c
c     IP()    (Work/Out)  Integer array of length at least N in which
c              the subr will store column permutation information.
c     -----------------------------------------------------------------
c     Subprograms referenced directly: ERMSG, ERMOR, IERM1, IERV1
c          D1MACH, DHTCC, DHTGEN, DDOT, DNRM2
c     Other subprograms needed: ERFIN
c     -----------------------------------------------------------------
c          This code was originally developed by Charles L. Lawson and
c     Richard J. Hanson at Jet Propulsion Laboratory in 1973.  The
c     original code was described and listed in the book,
c
c                  Solving Least Squares Problems
c                  C. L. Lawson and R. J. Hanson
c                  Prentice-Hall, 1974
c
c     Feb 1985, Mar 1987, June 1987.  C. L. Lawson & S. Y. Chiu, JPL.
c     Adapted code from the Lawson & Hanson book to Fortran 77 for use
c     in the JPL MATH77 library.
c     Changed code to provide oveflow avoidance.
c     Replaced previous scratch arrays H() and G() by WORK().
c     Prefixing subprogram names with S or D for s.p. or d.p. versions.
c     Using generic names for intrinsic functions.
c     Adding calls to BLAS and MATH77 error processing subrs in some
c     program units.
c     ------------------------------------------------------------------
c     1983 Sept 22. CLL added computation of RNORM() for the
c     exceptional case of N = 0.
c     -----------------------------------------------------------------
      EXTERNAL D1MACH, DDOT, DNRM2
      INTEGER LDA,M1,N1,LDB,KB,KRANK,IP(N1)
      INTEGER I,II,J,JB,K,KP1,L,LDIAG,LMAX,M,N,NTERMS
      DOUBLE PRECISION D1MACH,DDOT,DNRM2
      DOUBLE PRECISION A(LDA,N1),B(LDB,*),FACTOR,HFAC,ONE
      DOUBLE PRECISION RNORM(KB),SM1,SMALL,TAU,TMP,UPARAM,WORK(N1),ZERO
      logical COMSQR, COL, ROW
      parameter(ONE = 1.0D0, ZERO=0.0D0, FACTOR = 1000.0D0)
      parameter(COL = .true., ROW = .false.)
c     -----------------------------------------------------------------
C
      M = M1
      N = N1
      if( M .lt. 1 .or. N .lt. 0 .or. KB .lt. 0 .or. LDA .lt. M ) then
         call ERMSG('DHFTI',1,0,
     *   'Bad argument values.  Require M .ge. 1, N .ge. 0,', ',')
         call ERMOR('KB .ge. 0, and LDA .ge. M', ',')
         call IERV1('M',  M,   ',')
         call IERV1('N',  N,   ',')
         call IERV1('KB',  KB,   ',')
         call IERV1('LDA',LDA, '.')
         KRANK = 0
         return
      elseif( KB .eq. 0) then
         if(LDB .le. 0) then
            call IERM1('DHFTI',2,0,
     *         'Require LDB .ge. 1 when KB .eq. 0', 'LDB', LDB, '.')
            KRANK = 0
            return
         endif
      elseif(LDB .lt. max(M,N)) then
         call IERM1('DHFTI',3,0,
     *      'Require LDB .ge. max(M,N) when KB .ge. 1', 'KB',  KB, ',')
         call IERV1('LDB',LDB, '.')
         KRANK = 0
         return
      endif
c
      if (N .eq. 0) then
         do 10 J = 1, KB
            RNORM(J) = DNRM2(M, B(1,J), 1)
  10     continue
         KRANK = 0
         return
      endif
c                                 Here we have M > 0 and N > 0.
      SMALL = FACTOR * D1MACH(4)
      LDIAG = MIN(M,N)
C
      DO 80 J = 1,LDIAG
       if(J .eq. N) then
c      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c           Special for J = N.  This case is much simpler than J < N
c           since there are no more columns of A beyond the jth to be
c           considered for interchange or to be triangularized.
c
         IP(N) = N
         CALL DHTCC (1,N,N+1,M,A(1,N),UPARAM,B,LDB,KB)
c      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       else
c      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                                   Here we have J < N.
         IF (J .EQ. 1) THEN
            COMSQR = .TRUE.
         ELSE
c                  Update scaled squared column lengths and set LMAX.
c
            LMAX = J
            DO 20 L = J,N
               WORK(L) = WORK(L) - (HFAC * A(J-1,L))**2
               IF (WORK(L) .GT. WORK(LMAX)) LMAX = L
   20       continue
            COMSQR =  WORK(LMAX) .LE. SMALL
         ENDIF
C
         IF( COMSQR ) THEN
C
C              Compute scaled squared column lengths and set LMAX.
c              Scaling using HFAC protects against overflow of squared
c              numbers.
c
            NTERMS = M - J + 1
            LMAX = J
            DO 40 L = J,N
               WORK(L) = DNRM2(NTERMS, A(J,L), 1)
               IF (WORK(L) .GT. WORK(LMAX)) LMAX = L
   40       continue
            if(WORK(LMAX) .eq. ZERO) then
               HFAC = ONE
            else
               HFAC = ONE/WORK(LMAX)
            endif
            do 45 L = J,N
               WORK(L) = (HFAC * WORK(L))**2
   45       continue
         ENDIF
C
C                               DO COLUMN INTERCHANGES IF NEEDED.
C
         IP(J) = LMAX
         IF (IP(J) .NE. J) THEN
            DO 60 I = 1,M
               TMP = A(I,J)
               A(I,J) = A(I,LMAX)
   60          A(I,LMAX) = TMP
            WORK(LMAX) = WORK(J)
         ENDIF
C
C          Compute the J-th transformation and apply it to A and B.
C          Since we treated J = N as a special case we here have J < N
c          so the reference to A(1,J+1) is valid.
c
         CALL DHTCC (1,J,J+1,M,A(1,J),UPARAM,A(1,J+1),LDA,N-J)
         CALL DHTCC (2,J,J+1,M,A(1,J),UPARAM,B,LDB,KB)
c      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       endif
   80 continue
C
C              DETERMINE THE PSEUDORANK, K, USING THE TOLERANCE, TAU.
C
      K = LDIAG
      DO 90 J = 1,LDIAG
         IF (ABS(A(J,J)).LE.TAU) THEN
            K = J - 1
            GO TO 100
         ENDIF
   90 continue
  100 continue
      KP1 = K + 1
C
C                         COMPUTE THE NORMS OF THE RESIDUAL VECTORS.
C
      DO 130 JB = 1,KB
         TMP = ZERO
         DO 120 I = KP1,M
  120       TMP = TMP + B(I,JB)**2
         RNORM(JB) = SQRT(TMP)
  130 continue
C                          Special termination when Pseudorank = 0
      IF (K .EQ. 0) THEN
         DO 155 JB = 1,KB
            DO 150 I = 1,N
              B(I,JB) = ZERO
  150       continue
  155    continue
         KRANK = 0
         RETURN
      ENDIF
C
C               IF THE PSEUDORANK IS LESS THAN N COMPUTE HOUSEHOLDER
C               DECOMPOSITION OF FIRST K ROWS.
C
      IF (K .NE. N) THEN
         DO 170 II = 1,K
            I = KP1-II
            CALL DHTGEN(1,I,KP1,N,A(I,1),LDA,ROW,WORK(I),A,LDA,I-1,ROW)
  170    continue
      ENDIF
C
      DO 260 JB = 1,KB
C
C                        SOLVE THE K BY K TRIANGULAR SYSTEM.
C
         DO 210 L = 1,K
            I = KP1 - L
            IF (I .LT. K) THEN
               SM1 = DDOT(K-I,A(I,I+1),LDA,B(I+1,JB),1)
            ELSE
               SM1 = ZERO
            END IF
            B(I,JB) = (B(I,JB)-SM1) / A(I,I)
  210    continue
C
C     COMPLETE COMPUTATION OF SOLUTION VECTOR.
C    ..
         IF (K .NE. N) THEN
            DO 220 J = KP1,N
  220          B(J,JB) = ZERO
            DO 230 I = 1,K
  230          CALL DHTGEN(2,I,KP1,N,A(I,1),LDA,ROW,WORK(I),
     *                    B(1,JB),LDB,1,COL)
         ENDIF
C                    RE-ORDER THE SOLUTION VECTOR TO COMPENSATE FOR THE
C                    COLUMN INTERCHANGES.
C
         DO 250 J = LDIAG, 1, -1
            IF (IP(J) .NE. J) THEN
               L = IP(J)
               TMP = B(L,JB)
               B(L,JB) = B(J,JB)
               B(J,JB) = TMP
            ENDIF
  250    continue
  260 continue
      KRANK = K
      return
      end
