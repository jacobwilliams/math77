      SUBROUTINE SACCUM (A,LDA,N,B,LDB,NB,IR1, NROWS, NCOUNT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 SACCUM Krogh   Added external statement.
C>> 1994-11-11 SACCUM Krogh   Declared all vars.
C>> 1994-10-20 SACCUM Krogh  Changes to use M77CON
C>> 1987-11-24 SACCUM Lawson  Initial code.
c--S replaces "?": ?ACCUM, ?ROTG, ?HTCC, ?NRM2
c     Sequential accumulation of rows of data for a linear least
c     squares problem.  Using Givens orthogonal transformations when
c     NROWS is small and Householder orthogonal transformations when
c     NROWS is larger.
c
c        On first call user must set IR1 = 1.  On each return this subr
c     will update IR1 to min(IR1+NROWS, N+2).  The user should not alter
c     IR1 while processing data for the same case.
c
c        On all calls, including the first, the user sets NROWS to
c     indicate the number of rows of new data being provided.  The user
c     must put the new data in rows IR1 through IR1 + NROWS - 1 of the
c     arrays A() and B().  On return, with IR1 updated to
c     min(IR1+NROWS, N+2), the first IR1-1 rows of A() will contain
c     transformed data on and to the right of the diagonal, and zeros
c     to the left of the diagonal.  The first IR1-1 rows of B() will
c     also contain transformed data.
c     ------------------------------------------------------------------
c     Reference: C. L. Lawson & R. J. Hanson,
c                Solving Least Squares Problems, Prentice-Hall, 1974.
c     Original code by R. J. Hanson, JPL, Sept 11, 1968.
c     Adapted to Fortran 77 for the JPL MATH77 library
c     by Lawson, 6/2/87.
c     ------------------------------------------------------------------
c                      Subroutine arguments
c
c     A(,)  [inout]
c     LDA   [in]
c     N     [in]
c     B(,)  [inout]
c     LDB   [in]
c     NB    [in]
c     IR1   [inout]
c     NROWS  [in]
c     NCOUNT  [out]
c     ------------------------------------------------------------------
c     Subprograms called directly: SHTCC, SNRM2, SROTG, IERM1, IERV1
c     ------------------------------------------------------------------
      external SNRM2
      integer    IR1, NB, L1, LAST, LDA, LDB, N, NROWS, MARK
      integer    M, IP, IROW, J, I, NCOUNT
      real              SNRM2
      real              A(LDA,N), B(LDB,NB), UPARAM, ZERO
      real             CFAC, SFAC, TEMP
      parameter(ZERO = 0.0E0, MARK = 2)
c     ------------------------------------------------------------------
C
      if (NROWS .le. 0) return
C
      M=IR1+NROWS-1
      if(M .gt. LDA .or. M .gt. LDB) then
         call IERM1('SACCUM',1,0,
     *   'Require LDA .ge. M and LDB .ge M where M = IR1+NROWS-1',
     *   'IR1',IR1, ',')
         call IERV1('NROWS',NROWS, ',')
         call IERV1('LDA',LDA, ',')
         call IERV1('LDB',LDB, '.')
         return
      endif
      if(M .le. N) then
         LAST = M-1
      else
         LAST = N
      endif
c
      if(NROWS .le. MARK) then
c      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c           Use Givens transformation for better efficiency when the
c           number of rows being introduced is .le. MARK.
c
         do 50 IP = 1, LAST
            L1 = max(IR1, IP+1)
            do 30 IROW = L1, M
               call SROTG(A(IP,IP), A(IROW,IP), CFAC, SFAC)
               A(IROW,IP) = ZERO
               do 10 J = IP+1,N
                  TEMP = A(IP,J)
                  A(IP,J)   =   CFAC * TEMP + SFAC * A(IROW,J)
                  A(IROW,J) =  -SFAC * TEMP + CFAC * A(IROW,J)
   10          continue
               do 20 J = 1,NB
                  TEMP = B(IP,J)
                  B(IP,J)   =   CFAC * TEMP + SFAC * B(IROW,J)
                  B(IROW,J) =  -SFAC * TEMP + CFAC * B(IROW,J)
   20          continue
   30       continue
   50    continue
c        - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      else
c        - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                 Use Householder transformations
c                 when the number of rows being introduced exceeds MARK.
c
         do 80 IP = 1, LAST
            L1 = max(IR1, IP+1)
            call SHTCC (1,IP,L1,M,A(1,IP),UPARAM,
     *                           A(1,min(IP+1,N)),LDA,N-IP)
            call SHTCC (2, IP, L1, M, A(1,IP),UPARAM,
     *                           B(1,1),LDB,NB)
C
C                      Clear elements just made implicitly zero.
C
            do 60 I = L1, min(M,N+1)
               A(I,IP)=ZERO
   60       continue
   80    continue
c        - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      endif
c
      if(IR1 .eq. 1) then
         NCOUNT = NROWS
      else
         NCOUNT = NCOUNT + NROWS
      endif
      IR1=min(IR1 + NROWS, N+2)
      if (M .le. N+1) return
C
C     Pack lengths of B() column vectors below row N
C     to single locations each.
C
      do 90 J=1,NB
         B(N+1,J) = SNRM2 (M-N, B(N+1,J), 1)
   90 continue
      return
      end
