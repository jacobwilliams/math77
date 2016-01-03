c     DRSINT1F
c>> 2000-12-03 DRSINT1F Krogh  Declared WORK as WORK(1) in SINTF.
c>> 1994-11-02 DRSINT1F Krogh  Changes to use M77CON
c>> 1994-08-08 DRSINT1F Snyder Took '0' out of formats for C conversion
c>> 1993-05-05 DRSINT1F Krogh  Adjusted to simplify conversion to C.
c>> 1987-12-09 DRSINT1F Snyder Initial Code.
c--S replaces "?": DR?INT1f, ?INT1, ?INTF
c
c     DEMO DRIVER for 1 dimensional quadrature subprogram SINT1.
c
c     Compute the integral for X = 0.0 to X = PI of SIN(X), then
c     subtract 2.0 from the ANSWER.  The result should be zero.
c
c     The integrand is evaluated by the subprogram SINTF.
c
c
      real             A, B, ANSWER, WORK(1)
      integer IOPT(10)
10    format (/
     1' DRSINT1F:'/
     2' Compute the integral for X = 0.0 to X = PI of SIN(X), then'/
     3' subtract 2.0 from the ANSWER.  The result should be zero.')
20    format (/' ANSWER          =',G15.8/
     1         ' ERROR ESTIMATE  =',G15.8/
     2         ' STATUS FLAG     =',I6/
     3         ' FUNCTION VALUES =',I6)
c
      print 10
      A = 0.0E0
      B = 4.0E0 * ATAN(1.0E0)
      IOPT(2) = 10
      IOPT(3) = 0
      IOPT(4) = 0
c
      call SINT1 (A, B, ANSWER, WORK, IOPT)
      ANSWER = ANSWER - 2.0e0
      print 20, ANSWER, WORK(1), IOPT(1), IOPT(3)
      stop
      end
c   End of DRSINT1F
 
      subroutine SINTF (ANSWER, WORK, IFLAG)
c
c     Subroutine to provide integrand for SINT1.
c
      real             ANSWER, WORK(1)
      integer IFLAG
c
c     IFLAG is not used in this example.
c
      answer = sin(work(1))
      return
c
      end
