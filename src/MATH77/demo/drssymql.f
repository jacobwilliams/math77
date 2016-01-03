c     program DRSSYMQL
c>> 1996-05-28 DRSSYMQL  Krogh Added external statement.
c>> 1994-10-19 DRSSYMQL  Krogh  Changes to use M77CON
c>> 1994-09-22 DRSSYMQL  CLL
c>> 1992-04-23 CLL
c>> 1992-03-04 DRSSYMQL  Krogh Initial version.
c     Demonstrate symmetric eigenvalue/eigenvector subroutine SSYMQL.
c     ------------------------------------------------------------------
c--S replaces "?": DR?SYMQL, ?SYMQL, ?VECP, ?MATP, ?DOT
c     ------------------------------------------------------------------
      integer I, IERR, J, LDA, N
      parameter (LDA = 4)
      real             A(LDA,LDA), ASAV(LDA,LDA), ANORM, D(LDA,LDA)
      external SDOT
      real             SDOT, EVAL(LDA), WORK(LDA)
      data A(1,1)          /  5.0e0 /
      data (A(2,J), J=1,2) /  4.0e0,  5.0e0 /
      data (A(3,J), J=1,3) /  1.0e0,  1.0e0, 4.0e0 /
      data (A(4,J), J=1,4) /  1.0e0,  1.0e0, 2.0e0, 4.0e0 /
      data ANORM / 11.0e0 /
      data N /LDA/
c     ------------------------------------------------------------------
      print*,'DRSSYMQL..  Demo driver for SSYMQL.'
c
c     First copy A() to ASAV() for later residual check.
c
      do 20 I = 1,N
         do 10 J = 1,I
            ASAV(I,J) = A(I,J)
            ASAV(J,I) = ASAV(I,J)
   10    continue
   20 continue
      call SSYMQL(A, LDA, N, EVAL, WORK, IERR)
      if (IERR .eq. 0) then
         call SVECP(EVAL, N, '0 Eigenvalues')
         call SMATP(A, LDA, N, N,  '0 Eigenvectors as column vectors')
c
c        As a check compute D = (ASAV*EVEC - EVEC*EVAL) / ANORM.
c        The EVEC's are in the array A().
c        Expect D to be close to machine precision.
c
         do 40 J = 1, N
            do 30 I = 1, N
               D(I, J) = (SDOT(N, ASAV(I,1), LDA, A(1,J), 1) -
     *                  A(I,J) * EVAL(J)) / ANORM
   30       continue
   40    continue
         call SMATP(D, LDA, N, N,
     *         '0 Residual matrix D = (A*EVEC - EVEC*EVAL) / ANORM')
 
      else
         print '(/a,i5)',' Convergence failure in SSYMQL, IERR =',IERR
      end if
      stop
      end
