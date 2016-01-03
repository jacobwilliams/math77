c     program DRSEVUN
c>> 1994-10-19 DRSEVUN  Krogh  Changes to use M77CON
c>> 1992-04-23 DRSEVUN  CLL   Declaring all variables.
c>> 1992-03-04 DRSEVUN  Krogh Initial version.
c     Demonstrate unsymmetric eigenvalue subroutine SEVUN.
c     ------------------------------------------------------------------
c--S replaces "?": DR?EVUN, ?EVUN, ?VECP
c     ------------------------------------------------------------------
      integer I, KASE, LDA, N
      parameter (LDA = 3)
      integer IFLAG(LDA)
      real             A(LDA,LDA,3), VR(LDA), VI(LDA)
c
      data (A(1,I,1), I=1,3) / -954.0e0, -464.0e0, -2088.0e0 /
      data (A(2,I,1), I=1,3) /  792.0e0,  387.0e0,  1728.0e0 /
      data (A(3,I,1), I=1,3) /  264.0e0,  128.0e0,   579.0e0 /
c
      data (A(1,I,2), I=1,3) / 4.0e0, 1.0e0, 1.0e0 /
      data (A(2,I,2), I=1,3) / 2.0e0, 4.0e0, 1.0e0 /
      data (A(3,I,2), I=1,3) / 0.0e0, 1.0e0, 4.0e0 /
c
      data (A(1,I,3), I=1,3) /  8.0e0, -1.0e0, -5.0e0 /
      data (A(2,I,3), I=1,3) / -4.0e0,  4.0e0, -2.0e0 /
      data (A(3,I,3), I=1,3) / 18.0e0, -5.0e0, -7.0e0 /
      data N / LDA /
c     ------------------------------------------------------------------
      print*,'DRSEVUN..  Demo driver for SEVUN.'
      do 10 KASE = 1, 3
         print '(/a,i2)',' Beginning Case =', KASE
         call SEVUN(A(1, 1, KASE), LDA, N, VR, VI, IFLAG)
         print '(a,i2)',' IFLAG(1) =', IFLAG(1)
         if (IFLAG(1) .le. 2) then
            call SVECP(VR, N, ' Real part of the eigenvalues')
            call SVECP(VI, N, ' Imaginary part of the eigenvalues')
         else
            print '(/a)',' Failure in SEVUN.'
         end if
   10 continue
      stop
      end
