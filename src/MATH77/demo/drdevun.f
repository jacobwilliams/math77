c     program DRDEVUN
c>> 1994-10-19 DRDEVUN  Krogh  Changes to use M77CON
c>> 1992-04-23 DRDEVUN  CLL   Declaring all variables.
c>> 1992-03-04 DRDEVUN  Krogh Initial version.
c     Demonstrate unsymmetric eigenvalue subroutine DEVUN.
c     ------------------------------------------------------------------
c--D replaces "?": DR?EVUN, ?EVUN, ?VECP
c     ------------------------------------------------------------------
      integer I, KASE, LDA, N
      parameter (LDA = 3)
      integer IFLAG(LDA)
      double precision A(LDA,LDA,3), VR(LDA), VI(LDA)
c
      data (A(1,I,1), I=1,3) / -954.0d0, -464.0d0, -2088.0d0 /
      data (A(2,I,1), I=1,3) /  792.0d0,  387.0d0,  1728.0d0 /
      data (A(3,I,1), I=1,3) /  264.0d0,  128.0d0,   579.0d0 /
c
      data (A(1,I,2), I=1,3) / 4.0d0, 1.0d0, 1.0d0 /
      data (A(2,I,2), I=1,3) / 2.0d0, 4.0d0, 1.0d0 /
      data (A(3,I,2), I=1,3) / 0.0d0, 1.0d0, 4.0d0 /
c
      data (A(1,I,3), I=1,3) /  8.0d0, -1.0d0, -5.0d0 /
      data (A(2,I,3), I=1,3) / -4.0d0,  4.0d0, -2.0d0 /
      data (A(3,I,3), I=1,3) / 18.0d0, -5.0d0, -7.0d0 /
      data N / LDA /
c     ------------------------------------------------------------------
      print*,'DRDEVUN..  Demo driver for DEVUN.'
      do 10 KASE = 1, 3
         print '(/a,i2)',' Beginning Case =', KASE
         call DEVUN(A(1, 1, KASE), LDA, N, VR, VI, IFLAG)
         print '(a,i2)',' IFLAG(1) =', IFLAG(1)
         if (IFLAG(1) .le. 2) then
            call DVECP(VR, N, ' Real part of the eigenvalues')
            call DVECP(VI, N, ' Imaginary part of the eigenvalues')
         else
            print '(/a)',' Failure in DEVUN.'
         end if
   10 continue
      stop
      end
