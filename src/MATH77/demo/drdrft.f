      program DRDRFT
c>> 1996-06-19 DRDRFT Krogh  Minor change for C conversion.
c>> 1994-10-19 DRDRFT Krogh  Changes to use M77CON
c>> 1994-08-09 DRDRFT WVS Remove '0' from format
c>> 1993-02-04 DRDRFT CLL
c>> 1989-05-07 DRDRFT FTK, CLL
c>> 1989-05-04 DRDRFT FTK, CLL
c     Demo driver for DRFT -- Multi-dimensional real Fourier transform
c     ------------------------------------------------------------------
c--D replaces "?": DR?RFT, ?RFT
c     ------------------------------------------------------------------
      integer J, J1, J2, K, L, M(2), MS, N, N2, N4, ND
      double precision A(16, 16), ONE, PI, S(3), SIG, SIGD, TEMP, ZERO
      parameter (PI = 3.1415926535897932384D0)
      parameter (ONE = 1.D0)
      parameter (ZERO = 0.D0)
      data M / 4, 4 /, ND / 2 /
c     ------------------------------------------------------------------
c                         Start of code -- Construct A
      N = 2 ** M(1)
      N2 = N /2
      N4 = N2 / 2
      SIGD = PI / N2
      do 20 J1 = 1, N
         do 10 J2 = 1, N
            A(J1, J2) = ZERO
            if (abs(J1-N2-1) + abs(J2-N2-1) .le. N4) A(J1, J2) = ONE
   10    continue
   20 continue
c     ------------------------------------------------------------------
c                Compute Fourier transform and apply sigma factors
      MS = 0
      call DRFT (A, 'A', M, ND, MS, S)
      do 50 J1 = 1, N, 2
         A(J1, N2+1) = ZERO
         A(J1+1, N2+1) = ZERO
         do 40 J2 = 1, N2
            SIG = ONE
            if (J1 .EQ. 1) then
c                            No change in SIG due to J
               if (J2 .NE. 1) then
                  J = J2 - 1
                  K = 1
               else
                  A(2, 1) = ZERO
                  go to 40
               end if
            else
               J = J1 / 2
               K = 0
            end if
c                         Get nontrivial sigma factors * SIG
   30       continue
            if (J .LT. N4) then
               TEMP = S(J)
            else if (J .EQ. N4) then
               TEMP = ONE
            else
               TEMP = S(N2-J)
            end if
            SIG = SIG * TEMP / (SIGD * dble(J))
            if (K .EQ. 0) then
               if (J2 .NE. 1) then
                  J = J2 - 1
                  K = 1
                  go to 30
               end if
            else
c                                       Apply sigma factors
               if (J1 .EQ. 1) then
                  A(1, N-J2+2) = ZERO
                  A(2, N-J2+2) = ZERO
               else
                  A(J1, N-J2+2) = SIG * A(J1, N-J2+2)
                  A(J1+1, N-J2+2) = SIG * A(J1+1, N-J2+2)
               end if
            end if
            A(J1, J2) = SIG * A(J1, J2)
            A(J1+1, J2) = SIG * A(J1+1, J2)
   40    continue
   50 continue
      call DRFT (A, 'S', M, ND, MS, S)
      print '(/'' Smoothed A'')'
      do 60 L = 1, 9
         print'(9f8.4)', (A(L,N), N = 1, 9)
   60 continue
      stop
      end
