      subroutine SCOV2( A, MDIM, N, IP, VAR, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2001-02-13 SCOV2  Krogh   Fixed a comment.
c>> 1996-03-30 SCOV2  Krogh   Added external statement.
C>> 1994-11-11 SCOV2  Krogh   Declared all vars.
C>> 1994-10-20 SCOV2  Krogh  Changes to use M77CON
C>> 1989-10-20 SCOV2  CLL
C>> 1987-11-24 SCOV2  Lawson  Initial code.
c--S replaces "?": ?COV2, ?DOT, ?SWAP
C     Computes upper triangle of covariance matrix,
c     beginning with the triangular matrix, and permutation record, and
c     data error variance computed by _HFTI.
c     Thus, given a matrix, A, represented by the upper triangular
c     matrix in A(), and the permutation record in IP(), and the data
c     error variance, VAR, we compute the upper triangle of the
c     symmetric matrix, C = VAR * ((A**t)*A)**(-1).
C     Adapted from PROG2 in L & H book.
c     ------------------------------------------------------------------
c     Subprograms referenced directly: SDOT, SSWAP, IERM1
c     Other subprograms needed: ERMSG, IERV1, ERFIN, AMACH
c     ------------------------------------------------------------------
c                 Subroutine Arguments
c
c     A(,) [inout] On entry, contains the upper triangular matrix, A,
c                  in standard, not packed, storage.  This matrix could
c                  have been produced by _HFTI.  On return, contains the
c                  upper triangle of the symmetric unscaled covariance
c                  matrix.  Elements below the diagonal in A(,) will
c                  not be referenced.
c     MDIM [in]    First dimension of A(,).  Require MDIM .ge. N.
c     N [in]       Order of the matrix in A(,)
c     IP() [in]    Permutation record produced by _HFTI.
c     VAR  [in]    Estimate of variance of data error.
c     IERR [out]   Set to 0 if ok.  Set to J > 0 if the (J,J) element
c                  of the given matrix is zero.  In this latter case
c                  the covariance matrix cannot be computed and the
c                  contents of A(,) on return will be meaningless.
C     ------------------------------------------------------------------
c          This code was originally developed by Charles L. Lawson and
c     Richard J. Hanson at Jet Propulsion Laboratory in 1973.  The
c     original code was described and listed in the book,
c
c                  Solving Least Squares Problems
c                  C. L. Lawson and R. J. Hanson
c                  Prentice-Hall, 1974
c
c     Feb, 1985, C. L. Lawson & S. Y. Chan, JPL.  Adapted code from the
c     Lawson & Hanson book to Fortran 77 for use in the JPL MATH77
c     library.
c     Prefixing subprogram names with S or D for s.p. or d.p. versions.
c     Using generic names for intrinsic functions.
c     Adding calls to BLAS and MATH77 error processing subrs in some
c     program units.
C     1989-10-20 CLL Moved integer declaration earlier to avoid warning
c     msg from Cray compiler.
C     ------------------------------------------------------------------
      external SDOT
      integer I, IERR, J, N, MDIM, K, IP1, KP1
      integer IP(N)
      real             A(MDIM,N), SDOT, ONE, TMP, VAR, ZERO
C
      parameter(ZERO = 0.0E0, ONE = 1.0E0)
C     ------------------------------------------------------------------
C     Replace upper triangular matrix U by its inverse, Inv(U)
c
      do 40 J = 1,N
         if(A(J,J) .eq. ZERO) then
            call IERM1('SCOV2',1,0,'Jth diagonal elt is zero',
     *      'J',J,'.')
            IERR = J
            return
         end if
         A(J,J) = ONE / A(J,J)
   40 continue

      do 62 I = 1,N-1
         do 60 J = I+1,N
            A(I,J) = -A(J,J) * SDOT(J-I,A(I,I),MDIM,A(I,J),1)
   60    continue
   62 continue
C
C     Replace Inv(U) by upper triangle of Inv(u) * Trans(Inv(U))
c     multiplied by VAR.
c
      do 92 I = 1,N
         do 90 J = I,N
            A(I,J) = VAR * SDOT(N-J+1,A(I,J),MDIM,A(J,J),MDIM)
   90    continue
   92 continue
C                                 Permute rows & columns
      do 150 I = N-1, 1, -1
         if (IP(I) .ne. I) then
            K = IP(I)
            TMP = A(I,I)
            A(I,I) = A(K,K)
            A(K,K) = TMP
            if (I .gt. 1) THEN
              CALL SSWAP(I-1,A(1,I),1,A(1,K),1)
            end if
            IP1 = I + 1
            if (IP1 .lt. K) THEN
              CALL SSWAP(K-I-1,A(I,IP1),MDIM,A(IP1,K),1)
            end if
            KP1 = K + 1
            if (KP1 .le. N) THEN
              CALL SSWAP(N-K,A(I,KP1),MDIM,A(K,KP1),MDIM)
            end if
         end if
  150                     continue
      IERR = 0
      end
