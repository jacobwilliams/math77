c     program DRSCHOL
c>> 1996-06-17 DRSCHOL Krogh  Minor format change for C conversion.
c>> 1996-05-28 DRSCHOL Krogh Added external statement.
c>> 1994-10-19 DRSCHOL Krogh  Changes to use M77CON
c>> 1994-08-09 DRSCHOL WVS remove '0' from formats
c>> 1992-03-04 DRSCHOL Krogh Initial version.
c  Demonstration driver for SCHOL
c     ------------------------------------------------------------------
c--S replaces "?": DR?CHOL, ?CHOL, ?DOT
c     ------------------------------------------------------------------
      integer LDP, M, N
      parameter (M = 3, N = 2, LDP = 2)
      integer I, IERR, J
      real             A(M,N), B(M)
      external SDOT
      real             P(LDP,LDP), D(LDP), U, SDOT
      data A(1,1), A(1,2), B(1) /  0.7E0,  0.6E0,  1.726E0 /
      data A(2,1), A(2,2), B(2) / -0.8E0,  0.5E0, -5.415E0 /
      data A(3,1), A(3,2), B(3) /  0.6E0, -0.7E0,  5.183E0 /
c     ------------------------------------------------------------------
      U = SDOT(M, B, 1, B, 1)
      do 20 I = 1, N
         D(I) = SDOT(M, A(1,I), 1, B, 1)
         do 10 J = 1, N
            P(I,J) = SDOT(M, A(1,I), 1, A(1, J), 1)
   10    continue
   20 continue
      call SCHOL(P, LDP, N, D, U, 0.0e0, IERR)
      print '('' X()   = '',2f15.6)', D(1), D(2)
      print '('' RNORM = '',f15.6)', U
      if (IERR .ne. 0) print '(
     *  '' Matrix failed conditioning test in SCHOL, IERR = '',I3)',IERR
      end
