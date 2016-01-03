c     program DRDEVVUN
c>> 1996-05-28 DRDEVVUN  Krogh Added external statement.
c>> 1994-10-19 DRDEVVUN  Krogh  Changes to use M77CON
c>> 1994-09-22 DRDEVVUN  CLL
c>> 1992-04-23 CLL
c>> 1992-03-04 DRDEVVUN  Krogh Initial version.
c     Demonstrate unsymmetric eigenvalue/eigenvector subroutine DEVVUN.
c     ------------------------------------------------------------------
c--D replaces "?": DR?EVVUN, ?EVVUN, ?VECP, ?MATP, ?DOT
c     ------------------------------------------------------------------
      integer I, J, LDA, N
      parameter (LDA = 3)
      integer IFLAG(LDA)
      double precision A(LDA,LDA), VR(LDA), VI(LDA), VEC(LDA, LDA)
      double precision WORK(LDA), ASAV(LDA, LDA), D(LDA, LDA), ANORM
      external DDOT
      double precision DDOT
      data (A(1,I), I=1,3) /  8.0d0, -1.0d0, -5.0d0 /
      data (A(2,I), I=1,3) / -4.0d0,  4.0d0, -2.0d0 /
      data (A(3,I), I=1,3) / 18.0d0, -5.0d0, -7.0d0 /
      data ANORM / 30.0d0 /
      data N / LDA /
c     ------------------------------------------------------------------
      print*,'DRDEVVUN..  Demo driver for DEVVUN.'
c
c     First copy A() to ASAV() for later residual check.
c
         do 20 J = 1, N
            do 10 I = 1, N
               ASAV(I, J) = A(I, J)
   10       continue
   20    continue

         call DEVVUN(A(1, 1), LDA, N, VR, VI, VEC, IFLAG, WORK)

         print '(a, I2)',' IFLAG(1) =',IFLAG(1)
         if (IFLAG(1) .le. 2) then
            call DVECP(VR, N, ' Real part of the eigenvalues')
            call DVECP(VI, N, ' Imaginary part of the eigenvalues')
            call DMATP(VEC, LDA, N, N,
     *         '0 Eigenvectors as columns or pairs of columns')
c
c           As a check compute D = (ASAV*VEC - VEC*EVAL) / ANORM.
c           Expect D to be close to the machine precision.
c
            do 50 J = 1, N
               if (VI(J) .eq. 0.0d0) then
c                 Compute residual for a real eigenvalue and eigenvector
                  do 30 I = 1, N
                     D(I, J) = (DDOT(N, ASAV(I,1), LDA, VEC(1,J), 1) -
     *                  VEC(I,J) * VR(J)) / ANORM
   30             continue
               else if (VI(J) .gt. 0.0d0) then
                  do 40 I = 1, N
                     D(I, J) = (DDOT(N,ASAV(I,1),LDA,VEC(1,J),1) -
     *                  VEC(I,J)*VR(J)+VEC(I,J+1)*VI(J))/ANORM
                     D(I, J+1) = (DDOT(N,ASAV(I,1),LDA,VEC(1,J+1),1) -
     *                  VEC(I,J)*VI(J)-VEC(I,J+1)*VR(J))/ANORM
   40             continue
               end if
   50       continue
            call DMATP(D, LDA, N, N,
     *      '0 Packed residual matrix D = (A*EVEC - EVEC*EVAL) / ANORM')
         else
            print'(/a)', ' Failure in DEVVUN. '
         end if
      stop
      end
