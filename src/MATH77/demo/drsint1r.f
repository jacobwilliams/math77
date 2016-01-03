c     program DRSINT1R
c>> 1994-10-19 DRSINT1R Krogh  Changes to use M77CON
c>> 1994-08-08 DRSINT1R Snyder  Took '0' out of formats for C conversion
c>> 1991-11-20 CLL Edited for Fortran 90.
c>> 1987-12-09 DRSINT1R Snyder  Initial Code.
c--S replaces "?": DR?INT1R, ?INT1, ?INTA
c
c     DEMO DRIVER for 1 dimensional quadrature subprogram SINT1.
c
c     Compute the integral for X = 0.0 to X = PI of SIN(X), then
c     subtract 2.0 from the ANSWER.  The result should be zero.
c
c     The integrand is evaluated using reverse communication.
c
      real             A, B, ANSWER, WORK(1)
      integer IOPT(10)
c
10    format (/' DRSINT1R:'/
     1' Compute the integral for X = 0.0 to X = PI of SIN(X), then'/
     2' subtract 2.0 from the ANSWER.  The result should be zero.')
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
      IOPT(4) = 6
      IOPT(5) = 0
      call SINT1 (A, B, ANSWER, WORK, IOPT)
 30   continue
      call SINTA (ANSWER, WORK, IOPT)
      if (IOPT(1) .eq. 0) then
         ANSWER = SIN(WORK(1))
         go to 30
      end if
      print 20, ANSWER, WORK(1), IOPT(1), IOPT(3)
      stop
      end
