c     program DRDCHOL
c>> 1996-06-17 DRDCHOL Krogh  Minor format change for C conversion.
c>> 1996-05-28 DRDCHOL Krogh Added external statement.
c>> 1994-10-19 DRDCHOL Krogh  Changes to use M77CON
c>> 1994-08-09 DRDCHOL WVS remove '0' from formats
c>> 1992-03-04 DRDCHOL Krogh Initial version.
c  Demonstration driver for DCHOL
c     ------------------------------------------------------------------
c--D replaces "?": DR?CHOL, ?CHOL, ?DOT
c     ------------------------------------------------------------------
      integer LDP, M, N
      parameter (M = 3, N = 2, LDP = 2)
      integer I, IERR, J
      double precision A(M,N), B(M)
      external DDOT
      double precision P(LDP,LDP), D(LDP), U, DDOT
      data A(1,1), A(1,2), B(1) /  0.7D0,  0.6D0,  1.726D0 /
      data A(2,1), A(2,2), B(2) / -0.8D0,  0.5D0, -5.415D0 /
      data A(3,1), A(3,2), B(3) /  0.6D0, -0.7D0,  5.183D0 /
c     ------------------------------------------------------------------
      U = DDOT(M, B, 1, B, 1)
      do 20 I = 1, N
         D(I) = DDOT(M, A(1,I), 1, B, 1)
         do 10 J = 1, N
            P(I,J) = DDOT(M, A(1,I), 1, A(1, J), 1)
   10    continue
   20 continue
      call DCHOL(P, LDP, N, D, U, 0.0d0, IERR)
      print '('' X()   = '',2f15.6)', D(1), D(2)
      print '('' RNORM = '',f15.6)', U
      if (IERR .ne. 0) print '(
     *  '' Matrix failed conditioning test in DCHOL, IERR = '',I3)',IERR
      end
