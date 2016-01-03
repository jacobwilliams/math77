c     program DRDSYMQL
c>> 1996-05-28 DRDSYMQL  Krogh Added external statement.
c>> 1994-10-19 DRDSYMQL  Krogh  Changes to use M77CON
c>> 1994-09-22 DRDSYMQL  CLL
c>> 1992-04-23 CLL
c>> 1992-03-04 DRDSYMQL  Krogh Initial version.
c     Demonstrate symmetric eigenvalue/eigenvector subroutine DSYMQL.
c     ------------------------------------------------------------------
c--D replaces "?": DR?SYMQL, ?SYMQL, ?VECP, ?MATP, ?DOT
c     ------------------------------------------------------------------
      integer I, IERR, J, LDA, N
      parameter (LDA = 4)
      double precision A(LDA,LDA), ASAV(LDA,LDA), ANORM, D(LDA,LDA)
      external DDOT
      double precision DDOT, EVAL(LDA), WORK(LDA)
      data A(1,1)          /  5.0d0 /
      data (A(2,J), J=1,2) /  4.0d0,  5.0d0 /
      data (A(3,J), J=1,3) /  1.0d0,  1.0d0, 4.0d0 /
      data (A(4,J), J=1,4) /  1.0d0,  1.0d0, 2.0d0, 4.0d0 /
      data ANORM / 11.0d0 /
      data N /LDA/
c     ------------------------------------------------------------------
      print*,'DRDSYMQL..  Demo driver for DSYMQL.'
c
c     First copy A() to ASAV() for later residual check.
c
      do 20 I = 1,N
         do 10 J = 1,I
            ASAV(I,J) = A(I,J)
            ASAV(J,I) = ASAV(I,J)
   10    continue
   20 continue
      call DSYMQL(A, LDA, N, EVAL, WORK, IERR)
      if (IERR .eq. 0) then
         call DVECP(EVAL, N, '0 Eigenvalues')
         call DMATP(A, LDA, N, N,  '0 Eigenvectors as column vectors')
c
c        As a check compute D = (ASAV*EVEC - EVEC*EVAL) / ANORM.
c        The EVEC's are in the array A().
c        Expect D to be close to machine precision.
c
         do 40 J = 1, N
            do 30 I = 1, N
               D(I, J) = (DDOT(N, ASAV(I,1), LDA, A(1,J), 1) -
     *                  A(I,J) * EVAL(J)) / ANORM
   30       continue
   40    continue
         call DMATP(D, LDA, N, N,
     *         '0 Residual matrix D = (A*EVEC - EVEC*EVAL) / ANORM')

      else
         print '(/a,i5)',' Convergence failure in DSYMQL, IERR =',IERR
      end if
      stop
      end
