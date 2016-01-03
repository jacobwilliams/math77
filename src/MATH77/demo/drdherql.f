c     program DRDHERQL
c>> 1996-05-28 DRDHERQL  Krogh Added external statement.
c>> 1994-10-19 DRDHERQL  Krogh  Changes to use M77CON
c>> 1994-09-23 DRDHERQL  CLL
c>> 1992-04-23 CLL
c>> 1992-03-04 DRDHERQL  Krogh Initial version.
c     Demonstrate Hermitian eigenvalue/eigenvector subroutine DHERQL.
c     ------------------------------------------------------------------
c--D replaces "?": DR?HERQL, ?HERQL, ?VECP, ?MATP, ?DOT
c     ------------------------------------------------------------------
      integer I, IERR, J, LDA, LDA3, N
      parameter (LDA = 4)
      parameter (LDA3 = 3*LDA)
      double precision AR(LDA, LDA), AI(LDA, LDA)
      double precision ARSAV(LDA, LDA), AISAV(LDA, LDA), ANORM
      external DDOT
      double precision DDOT, DR(LDA,LDA), DI(LDA,LDA), EVAL(LDA)
      double precision VR(LDA, LDA), VI(LDA, LDA), WORK(LDA3)
      data AR(1,1)          /  25.0d0 /
      data (AR(2,I), I=1,2) /  -3.0d0,  25.0d0 /
      data (AR(3,I), I=1,3) /  -08.0d0,  0.0d0,  25.0d0 /
      data (AR(4,I), I=1,4) /  0.0d0,  -08.0d0,  3.0d0, 25.0d0 /
      data AI(1,1)          /  0.0d0 /
      data (AI(2,I), I=1,2) /  4.0d0,  0.0d0 /
      data (AI(3,I), I=1,3) /  -06.0d0,  0.0d0, 0.0d0 /
      data (AI(4,I), I=1,4) / 0.0d0,  06.0d0, 4.0d0, 0.0d0 /
      data ANORM / 46.0d0 /
      data N /LDA/
c     ------------------------------------------------------------------
      print*,'DRDHERQL..  Demo driver for DHERQL.'
c
c     First copy AR() and AI() to ARSAV() and AISAV() for later
c     residual check.
c
      do 20 I = 1,N
         do 10 J = 1,I
            ARSAV(I,J) = AR(I,J)
            ARSAV(J,I) = ARSAV(I,J)
            AISAV(I,J) = AI(I,J)
            AISAV(J,I) = -AISAV(I,J)
   10    continue
   20 continue

      call DHERQL(AR, AI, LDA, N, EVAL, VR, VI, WORK, IERR)
      if (IERR .eq. 0) then
         call DVECP(EVAL, N, '0 Eigenvalues')
         call DMATP(VR,LDA,N,N,
     *     '0 Real parts of eigenvectors as column vectors')
         call DMATP(VI,LDA,N,N,
     *     '0 Imaginary parts of eigenvectors as column vectors')
c
c        As a check compute D = (ASAV*EVEC - EVEC*EVAL) / ANORM.
c        Expect D to be close to the machine precision.
c
         do 40 J = 1, LDA
            do 30 I = 1, LDA
               DR(I, J) = (DDOT(LDA,ARSAV(I,1),LDA,VR(1,J),1) -
     *                     DDOT(LDA,AISAV(I,1),LDA,VI(1,J),1) -
     *                     VR(I,J)*EVAL(J))/ANORM
               DI(I, J) = (DDOT(LDA,ARSAV(I,1),LDA,VI(1,J),1) +
     *                     DDOT(LDA,AISAV(I,1),LDA,VR(1,J),1) -
     *                     VI(I,J)*EVAL(J))/ANORM
   30       continue
   40    continue
       call DMATP(DR, LDA, N, N,
     * '0Real part of residual matrix D = (A*EVEC - EVEC*EVAL) / ANORM')
       call DMATP(DI, LDA, N, N,
     * '0Imag part of residual matrix D = (A*EVEC - EVEC*EVAL) / ANORM')
      else
         print '(/a,i5)',' Convergence failure in DHERQL, IERR =',IERR
      end if
      stop
      end
