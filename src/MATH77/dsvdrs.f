      SUBROUTINE DSVDRS (A,LDA,M1,N1,B,LDB,NB,S,WORK)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2001-05-25 DSVDRS Krogh Minor change for making .f90 version.
C>> 1999-08-12 DSVDRS Krogh  Loop at 180 => 2 loops for Sun optimizer.
C>> 1994-10-20 DSVDRS Krogh  Changes to use M77CON
C>> 1992-06-16 DSVDRS CLL
C>> 1992-03-13 DSVDRS FTK  Removed implicit statements.
C>> 1989-10-20 CLL
C>> 1989-06-05 DSVDRS Snyder  Add END statement, polish a little.
C>> 1987-11-24 DSVDRS Lawson  Adaptation to Fortran 77 and MATH77.
c>> 1974-01-01 Lawson and Hanson initial code.
c
C          This subr computes the singular value decomposition of the
c     given M1 x N1 matrix, A, and optionally applys the transformations
c     from the left to the NB column vectors of the M1 x NB matrix B.
c     Either M1 .ge. N1  or  M1 .lt. N1 is permitted.
c
c          The singular value decomposition of A is of the form
c
C                     A  =  U * S * V**t
c
c     where U is M1 x M1 orthogonal, S is M1 x N1 diagonal with the
c     diagonal terms nonnegative and ordered from large to small, and
c     V is N1 x N1 orthogonal.  Note that these matrices also satisfy
c
c                     S = (U**t) * A * V
c
c          The matrix V is returned in the leading N1 rows and
c     columns of the array A(,).
c
c          The singular values, i.e. the diagonal terms of the matrix S,
c     are returned in the array S().  If M1 .lt. N1, positions M1+1
c     through N1 of S() will be set to zero.
c
c          The product matrix  G = U**t * B replaces the given matrix B
c     in the array B(,).
c
c          If the user wishes to obtain a minimum length least squares
c     solution of the linear system
c
c                           A * X ~=~ B
c
c     the solution X can be constructed, following use of this subr,
c     by computing the sum for i = 1, ..., R of the outer products
c
c             (Col i of V) * (1/S(i)) * (Row i of G)
c
c     Here R denotes the pseudorank of A which the user may choose
c     in the range 0 through Min(M1, N1) based on the sizes of the
c     singular values.
C     ------------------------------------------------------------------
C          Subroutine Arguments
c
c     A(,)     (In/Out)  On input contains the M1 x N1 matrix A.
c              On output contains the N1 x N1 matrix V.
c
c     LDA      (In)  First dimensioning parameter for A(,).
c              Require LDA .ge. Max(M1, N1).
c
c     M1       (In)  No. of rows of matrices A, B, and G.
c              Require M1 > 0.
c
c     N1       (In)  No. of cols of matrix A, No. of rows and cols of
c              matrix V.  Permit M1 .ge. N1  or  M1 .lt. N1.
c              Require N1 > 0.
c
c     B(,)     (In/Out)  If NB .gt. 0 this array must contain an
C              M1 x NB matrix on input and will contain the
c              M1 x NB product matrix, G = (U**t) * B on output.
c
c     LDB      (In)  First dimensioning parameter for B(,).
c              Require LDB .ge. M1.
c
c     NB       (In)  No. of cols in the matrices B and G.
c              Require NB .ge. 0.
c
c     S()      (Out)  Must be dimensioned at least N1.  On return will
c              contain the singular values of A, with the ordering
c                   S(1) .ge. S(2) .ge. ... .ge. S(N1) .ge. 0.
c              If M1 .lt. N1 the singular values indexed from M1+1
c              through N1 will be zero.
c              If the given integer arguments are not consistent, this
c              subr will return immediately, setting S(1) = -1.0.
c
c     WORK()  (Scratch)  Work space of total size at least 2*N1.
c              Locations 1 thru N1 will hold the off-diagonal terms of
c              the bidiagonal matrix for subr _QRBD.  Locations N1+1
c              thru 2*N1 will save info from one call to the next of
c              _HTGEN.
c     ------------------------------------------------------------------
c     Subprograms referenced directly: DCOPY, DHTCC, DHTGEN,DQRBD,DSWAP,
c        ERMSG, ERMOR, IERV1
c     Other subprograms needed: ERFIN, *1MACH (Fortran 77 only), *ROT,
c     *ROTG (* = S or D)
C     ------------------------------------------------------------------
c          This subr gives special treatment to the cases of entire rows
c     and/or columns of A being zero. This provides the feature that
c     singular values that are zero because of the zero structure of A
c     will be produced as exact zeros rather than as relatively small
c     nonzero values.  This is convenient for cases in which a user
c     wishes to remove a variable from a problem by simply zeroing the
c     corresponding column of A.
c
c          Method for this special feature:
c
C      1. EXCHANGE COLS OF A TO PACK NONZERO COLS TO THE LEFT.
C         SET NWORK  =  NO. OF NONZERO COLS.
C         USE LOCATIONS A(1,NWORK+1),...,A(1,N1) TO RECORD THE
C         COL PERMUTATIONS.
C      2. EXCHANGE ROWS OF A TO PACK NONZERO ROWS TO THE TOP.
C         QUIT PACKING IF FIND NWORK NONZERO ROWS.  MAKE SAME ROW
C         EXCHANGES IN B.  SET MWORK SO THAT ALL NONZERO ROWS OF THE
C         PERMUTED A ARE IN FIRST MWORK ROWS.  IF MWORK .LE. NWORK THEN
C         ALL MWORK ROWS ARE NONZERO.  IF MWORK .GT. NWORK THEN THE
C         FIRST NWORK ROWS ARE KNOWN TO BE NONZERO, AND ROWS NWORK+1
C         THRU MWORK MAY BE ZERO OR NONZERO.
C      3. APPLY THE BASIC SVD ALGORITHM TO THE MWORK BY NWORK PROBLEM.
C      4. MOVE PERMUTATION RECORD FROM A(,) TO S(I),I = NWORK+1,...,N1.
C      5. BUILD V UP FROM  NWORK BY NWORK TO N1 BY N1 BY PLACING ONES ON
C         THE DIAGONAL AND ZEROS ELSEWHERE.  THIS IS ONLY PARTLY DONE
C         EXPLICITLY.  IT IS COMPLETED DURING STEP 6.
C      6. EXCHANGE ROWS OF V TO COMPENSATE FOR COL EXCHANGES OF STEP 2.
C      7. PLACE ZEROS IN  S(I),I = NWORK+1,...,N1  TO REPRESENT ZERO
C         SING VALS.
C
c     ------------------------------------------------------------------
c          This code was originally developed by Charles L. Lawson and
c     Richard J. Hanson at Jet Propulsion Laboratory in 1973.  The
c     original code was described and listed in the book,
c
c                  Solving Least Squares Problems
c                  C. L. Lawson and R. J. Hanson
c                  Prentice-Hall, 1974
c
c     Feb 1985, Mar 1987, C. L. Lawson & S. Y. Chiu, JPL.  Adapted code
c     from the Lawson & Hanson book to Fortran 77 for use in the
c     JPL MATH77 library.
c     Prefixing subprogram names with S or D for s.p. or d.p. versions.
c     Using generic names for intrinsic functions.
c     Adding calls to BLAS and MATH77 error processing subrs in some
c     program units.
c     1989-10-20 CLL  Moved integer declaration to earlier position to
c     avoid warning msg from Cray compiler.
C     1992-06-16 CLL.  Using "min" fcn in arg list of calls to _HTCC
c     and _HTGEN to avoid "index out of range" when using a bounds
c     checker, even though no reference would be made to the "out of
c     range" location.
c     ------------------------------------------------------------------
C--D replaces "?": ?SVDRS, ?COPY, ?HTCC, ?HTGEN, ?QRBD, ?SWAP
c     ------------------------------------------------------------------
      integer I,IPASS,J,K,L,LBASE,LDA,LDB,M1,MWORK,N1,NB,NP1,NS,NWORK
      double precision A(LDA,N1),B(LDB,*),ONE,S(N1),UPARAM,WORK(2*N1)
      double precision ZERO
      logical COL, ROW
      parameter( ZERO = 0.0D0, ONE = 1.0D0, COL = .true., ROW = .false.)
c     ------------------------------------------------------------------
      NWORK = N1
      LBASE = NWORK
      if(LDA .lt. max(M1,NWORK) .or. M1 .le. 0 .or. NWORK .le. 0 .or.
     *   (NB .gt. 0 .and. LDB .lt. M1)) then
         call ERMSG('DSVDRS',1,0,
     *   'Bad argument values.  Require LDA .ge. max(M,N), M > 0,',
     *   ',')
         call ERMOR('N > 0, LDB .ge. M if NB > 0', ',')
         call IERV1('LDA',LDA,   ',')
         call IERV1('M',  M1,    ',')
         call IERV1('N',  NWORK, ',')
         call IERV1('LDB',LDB,   ',')
         call IERV1('NB', NB,    '.')
         S(1) = -ONE
         return
      endif
C                             BEGIN.. SPECIAL FOR ZERO ROWS AND COLS.
C
C                             PACK THE NONZERO COLS TO THE LEFT
C
      DO 50 J = NWORK, 1, -1
         DO 20 I = 1,M1
            IF (A(I,J) .NE. ZERO) GO TO 50
   20    CONTINUE
C
C         COL J  IS ZERO. EXCHANGE IT WITH COL NWORK.
C
         IF (J .ne. NWORK) then
            DO 30 I = 1, M1
   30       A(I,J) = A(I,NWORK)
         end if
         A(1,NWORK) = J
         NWORK = NWORK - 1
   50 CONTINUE
C                          IF NWORK = 0 THEN A IS ENTIRELY ZERO AND SVD
C                          COMPUTATION CAN BE SKIPPED
      NS = 0
      IF (NWORK .ne. 0) then
C        ---------------------------------------------------------------
C                             PACK NONZERO ROWS TO THE TOP
C                             QUIT PACKING IF FIND NWORK NONZERO ROWS
         I = 1
         MWORK = M1
   60    IF (I.le.NWORK .and. I.lt.MWORK) then
            DO 80 J = I, NWORK
               IF (A(I,J) .NE. ZERO) GO TO 145
   80       CONTINUE
            DO 85 J = 1, I-1
               IF(A(I,J) .NE. ZERO) GO TO 145
   85       CONTINUE
C                             ROW I IS ZERO
C                             EXCHANGE ROWS I AND MWORK
            IF (NB .GT. 0)
     *         CALL DSWAP(   NB, B(MWORK,1), LDB, B(I,1), LDB)
               CALL DCOPY(NWORK, A(MWORK,1), LDA, A(I,1), LDA)
            IF (MWORK .LE. NWORK) THEN
               DO 130 J = 1,NWORK
  130             A(MWORK,J) = ZERO
            ENDIF
C                             EXCHANGE IS FINISHED
            MWORK = MWORK - 1
            GO TO 60
c           Here when row I is not all zero.  No exchange needed.
  145       I = I + 1
            GO TO 60
         end if
C                             END.. SPECIAL FOR ZERO ROWS AND COLUMNS
c        ---------------------------------------------------------------
C                             BEGIN.. SVD ALGORITHM
C     METHOD..
C     (1)     REDUCE THE MATRIX TO UPPER BIDIAGONAL FORM WITH
C     HOUSEHOLDER TRANSFORMATIONS.
C          H(NWORK)...H(1)AQ(1)...Q(NWORK-2)  =  (D**T,0)**T
C     WHERE D IS UPPER BIDIAGONAL.
C
C     (2)     APPLY H(NWORK)...H(1) TO B.  HERE H(NWORK)...H(1)*B REPLAC
C     IN STORAGE.
C
C     (3)     THE MATRIX PRODUCT W =  Q(1)...Q(NWORK-2) OVERWRITES THE F
C     NWORK ROWS OF A IN STORAGE.
C
C     (4)     AN SVD FOR D IS COMPUTED.  HERE K ROTATIONS RI AND PI ARE
C     COMPUTED SO THAT
C          RK...R1*D*P1**(T)...PK**(T)  =  DIAG(S1,...,S.NS)
C     TO WORKING ACCURACY.  THE SI ARE NONNEGATIVE AND NONINCREASING.
C     HERE RK...R1*B OVERWRITES B IN STORAGE WHILE
C     A*P1**(T)...PK**(T)  OVERWRITES A IN STORAGE.
C
C     (5)     IT FOLLOWS THAT,WITH THE PROPER DEFINITIONS,
C     U**(T)*B OVERWRITES B, WHILE V OVERWRITES THE FIRST NWORK ROW AND
C     COLUMNS OF A.
C     ------------------------------------------------------------------
         L = MIN(MWORK,NWORK)
C             THE FOLLOWING LOOP REDUCES A TO UPPER BIDIAGONAL AND
C             ALSO APPLIES THE PREMULTIPLYING TRANSFORMATIONS TO B.
C
         DO 170 J = 1,L
            IF(J .LT. MWORK) THEN
               CALL DHTCC(1,J,J+1,MWORK,A(1,J),UPARAM,
     *            A(1,min(J+1,NWORK)),LDA,NWORK-J)
               CALL DHTCC(2,J,J+1,MWORK,A(1,J),UPARAM,B,LDB,NB)
            ENDIF
            IF (J .LT. NWORK-1) THEN
               CALL DHTGEN (1,J+1,J+2,NWORK,A(J,1),LDA,ROW,
     *            WORK(LBASE + J),A(min(J+1,MWORK),1),LDA,MWORK-J,ROW)
            ENDIF
  170    CONTINUE
C                            Copy the bidiagonal matrix into the arrays
c                            S() and WORK(1:N1) for _QRBD.
         DO 180 J = 1, L
  180    S(J) = A(J,J)
         DO 190 J = 2, L
  190    WORK(J) = A(J-1,J)
C
         NS = NWORK
         IF (MWORK .LT. NWORK) THEN
            NS = MWORK + 1
            S(NS) = ZERO
            WORK(NS) = A(MWORK,MWORK+1)
         ENDIF
C
C     CONSTRUCT THE EXPLICIT NWORK BY NWORK PRODUCT MATRIX,
C     W = Q1*Q2*...*QL*I IN THE ARRAY A().
C
         DO 230 I = NWORK, 1, -1
            IF (I .LE. MIN(MWORK,NWORK-2))
     *         CALL DHTGEN(2,I+1,I+2,NWORK,A(I,1),LDA,ROW,
     *            WORK(LBASE+I),A(1,min(I+1,NWORK)),LDA,NWORK-I,COL)
C
            DO 220 J = 1, NWORK
  220          A(I,J) = ZERO
  230    A(I,I) = ONE
C
C        COMPUTE THE SVD OF THE BIDIAGONAL MATRIX
C
         CALL DQRBD (IPASS,S,WORK(1),NS,A,LDA,NWORK,B,LDB,NB)
C
C          If the above subr has a convergence failure it will report
c          the error via the MATH77 error message subrs and return
c          with IPASS = 2.  Since the error has been reported, we don't
c          report it again here but just return.
c
         IF (IPASS .EQ. 2) RETURN
C        ---------------------------------------------------------------
      end if
      DO 250 J = NS+1, NWORK
  250    S(J) = ZERO
      IF (NWORK .EQ. N1) RETURN
      NP1 = NWORK + 1
C                                  MOVE RECORD OF PERMUTATIONS
C                                  AND STORE ZEROS
      DO 280 J = NP1, N1
         S(J) = A(1,J)
         DO 270 I = 1,NWORK
  270       A(I,J) = ZERO
  280 CONTINUE
C                             PERMUTE ROWS AND SET ZERO SINGULAR VALUES.
      DO 300 K = NP1, N1
         I = S(K)
         S(K) = ZERO
         DO 290 J = 1,N1
            A(K,J) = A(I,J)
  290       A(I,J) = ZERO
         A(I,K) = ONE
  300 CONTINUE
c          End.. Special for zero rows and columns.
      RETURN
      END
