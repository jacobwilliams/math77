c     DRDINT1F
c>> 2000-12-03 DRDINT1F Krogh  Declared WORK as WORK(1) in DINTF.
c>> 1994-11-02 DRDINT1F Krogh  Changes to use M77CON
c>> 1994-08-08 DRDINT1F Snyder Took '0' out of formats for C conversion
c>> 1993-05-05 DRDINT1F Krogh  Adjusted to simplify conversion to C.
c>> 1987-12-09 DRDINT1F Snyder Initial Code.
c--D replaces "?": DR?INT1f, ?INT1, ?INTF
c
c     DEMO DRIVER for 1 dimensional quadrature subprogram DINT1.
c
c     Compute the integral for X = 0.0 to X = PI of SIN(X), then
c     subtract 2.0 from the ANSWER.  The result should be zero.
c
c     The integrand is evaluated by the subprogram DINTF.
c
c
      double precision A, B, ANSWER, WORK(1)
      integer IOPT(10)
10    format (/
     1' DRDINT1F:'/
     2' Compute the integral for X = 0.0 to X = PI of SIN(X), then'/
     3' subtract 2.0 from the ANSWER.  The result should be zero.')
20    format (/' ANSWER          =',G15.8/
     1         ' ERROR ESTIMATE  =',G15.8/
     2         ' STATUS FLAG     =',I6/
     3         ' FUNCTION VALUES =',I6)
c
      print 10
      A = 0.0D0
      B = 4.0D0 * ATAN(1.0D0)
      IOPT(2) = 10
      IOPT(3) = 0
      IOPT(4) = 0
c
      call DINT1 (A, B, ANSWER, WORK, IOPT)
      ANSWER = ANSWER - 2.0d0
      print 20, ANSWER, WORK(1), IOPT(1), IOPT(3)
      stop
      end
c   End of DRDINT1F

      subroutine DINTF (ANSWER, WORK, IFLAG)
c
c     Subroutine to provide integrand for DINT1.
c
      double precision ANSWER, WORK(1)
      integer IFLAG
c
c     IFLAG is not used in this example.
c
      answer = sin(work(1))
      return
c
      end
