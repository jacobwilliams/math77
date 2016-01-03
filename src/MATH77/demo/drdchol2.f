c     program DRDCHOL2
c>> 1996-06-17 DRDCHOL2  Krogh  Minor format change for C conversion.
c>> 1996-05-28 DRDCHOL2  Krogh Added external statement.
c>> 1994-10-19 DRDCHOL2  Krogh  Changes to use M77CON
c>> 1993-02-18 DRDCHOL2  CLL.
c>> 1992-03-04 DRDCHOL2  Krogh Initial version.
c  Demonstration driver for DCHOL
c     ------------------------------------------------------------------
c--D replaces "?": DR?CHOL2, ?CHOL, ?DOT
c     ------------------------------------------------------------------
      integer LDPDU, M, N, NP1
      parameter (M = 3, N = 2, NP1 = N+1, LDPDU = 3)
      integer I, IERR, J
      double precision AB(M,NP1)
      external DDOT
      double precision PDU(LDPDU,LDPDU), DDOT
      data AB(1,1), AB(1,2), AB(1,3) /  0.7D0,  0.6D0,  1.726D0 /
      data AB(2,1), AB(2,2), AB(2,3) / -0.8D0,  0.5D0, -5.415D0 /
      data AB(3,1), AB(3,2), AB(3,3) /  0.6D0, -0.7D0,  5.183D0 /
c     ------------------------------------------------------------------
      do 20 I = 1, NP1
         do 10 J = 1, NP1
            PDU(I,J) = DDOT(M, AB(1,I), 1, AB(1, J), 1)
   10    continue
   20 continue
      call DCHOL(PDU, LDPDU, N, PDU(1,NP1), PDU(NP1,NP1), 0.0d0, IERR)
      print '('' X()   = '',2f15.6)', PDU(1,NP1), PDU(2,NP1)
      print '('' RNORM = '',f15.6)', PDU(NP1,NP1)
      if (IERR .ne. 0) print '(
     *  '' Matrix failed conditioning test in DCHOL, IERR = '',I3)',IERR
      end
