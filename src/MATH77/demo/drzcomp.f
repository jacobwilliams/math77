c     program DRZCOMP
c>> 2001-01-24 DRZCOMP  ZSQRT -> ZSQRTX to fix C lib. problems.
c>> 1998-01-22 DRZCOMP  Krogh Added ZDIF,..., ZSUM to exernal statement.
c>> 1996-05-28 DRZCOMP  Krogh Added external statement.
c>> 1993-02-04 CLL Added call to DZABS.
c>> 1987-12-09 DRZCOMP  Lawson  Initial Code.
c     Demo driver for DZABS, ZSUM, ZDIF, ZPRO, ZQUO, and ZSQRTX.
c     C. L. Lawson, JPL, 1987 Feb 17.
c     ------------------------------------------------------------------
      external DZABS, ZDIF, ZPRO, ZQUO, ZSQRTX, ZSUM
      double precision DZABS
      double precision A(2), B(2), C(2), DMAG, U(2), U2(2), V(2), V2(2)
      double precision W(2), W2(2), TEST(2), Z(2)
c     ------------------------------------------------------------------
      A(1) = 6.0D0/7.0D0
      A(2) = -14.0D0/15.0D0
      B(1) = -29.0D0/31.0D0
      B(2) = 47.0D0/43.0D0
      U(1) = 51.0D0/53.0D0
      U(2) = 73.0D0/71.0D0
      call ZSUM(U, A, V)
      call ZPRO(V, B, W)
      call ZSQRTX(W,   Z)
      call ZPRO(Z, Z, W2)
      call ZQUO(W2, B, V2)
      call ZDIF(V2, A, U2)
      call ZDIF(U2, U, TEST)
      C(1) =  3.0d0/7.0d0
      C(2) = -4.0d0/7.0d0
      DMAG = DZABS(C)
      print'(a/)','                             ODZCOMP'
      print '(1x,a,f19.15,a,f19.15,a)',
     *   'A        = (',A(1),',',A(2), ')',
     *   'B        = (',B(1),',',B(2), ')',
     *   'U        = (',U(1),',',U(2), ')',
     *   'V = U+A  = (',V(1),',',V(2), ')',
     *   'W = V*B  = (',W(1),',',W(2), ')',
     *   'Z=sqrt(W)= (',Z(1),',',Z(2), ')',
     *   'W2 = Z*Z = (',W2(1),',',W2(2), ')',
     *   'V2= W2/B = (',V2(1),',',V2(2), ')',
     *   'U2= V2-A = (',U2(1),',',U2(2), ')'
      print '(1x,a,g19.3,a,g19.3,a)',
     *   'TEST=U2-U= (',TEST(1),',',TEST(2), ')'
      C(1) =  3.0d0/7.0d0
      C(2) = -4.0d0/7.0d0
      DMAG = DZABS(C)
      print '(/1x,a,f19.15,a,f19.15,a)',
     *   'C        = (',C(1),',',C(2), ')'
      print '(1x,a,g11.3)',
     *   'TEST2 = DZABS(C)-(5/7) = ',DMAG-(5.0d0/7.0d0)
      stop

      end
