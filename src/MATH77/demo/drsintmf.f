c     DRSINTM
c>> 2003-03-30 DRSINTMF Krogh Changed way of dealing with 0 denominator.
c>> 2001-05-22 DRSINTMF Krogh Minor change for making .f90 version.
c>> 1994-11-02 DRSINTMF Krogh  Changes to use M77CON
c>> 1994-08-08 DRSINTMF Snyder  Took '0' out of formats for C conversion
c>> 1993-05-05 DRSINTMF Krogh   Adjusted to simplify conversion to C.
c>> 1987-12-09 DRSINTMF Snyder  Initial Code.
c--S replaces "?": DR?INTM, DR?INTMF, ?INTM, ?INTF
c
c     DEMO DRIVER for multi-dimensional quadrature subprogram SINTM.
c
c     Compute the integral for Y = 0.0 to Y = PI of
c             the integral for X = 0.0 to X = Y of
c             X * COS(Y) /(X*X+Y*Y).  The ANSWER should be zero.
c
c     The integrand and all limits are provided by the subprogram SINTF,
c     although the limits of the outer dimension, and the lower limit of
c     the inner dimension, being constants, could have been provided
c     here.
c
      integer NDIMI, NWPD, NWORK, IOPT(10)
      parameter (NDIMI=2, NWPD=217, NWORK=3*NDIMI+NWPD*(NDIMI-1))
      real             ANSWER, WORK(NWORK)
10    format (/
     1' DRSINTMF:'/
     2' Compute the integral for Y = 0.0 to Y = PI of'/
     3'         the integral for X = 0.0 to X = Y of'/
     4'         X * COS(Y) /(X*X+Y*Y).  The ANSWER should be zero.')
20    format (/' ANSWER =        ',G15.8/
     1         ' ERROR ESTIMATE =',G15.8/
     2         ' STATUS FLAG =   ',I3/
     3         ' FUNCTION VALUES (INNERMOST INTEGRALS) =',I6)
c
      print 10
      IOPT(2) = 10
      IOPT(3) = 0
      IOPT(4) = 0
      call SINTM (NDIMI, ANSWER, WORK, NWORK, IOPT)
      print 20, ANSWER, WORK(1), IOPT(1), IOPT(3)
      stop
      end
c   End of DRSINTMF
 
      subroutine SINTF (ANSWER, WORK, IFLAG)
c
c     Subroutine to provide integrand and limits for SINTM.
c
c     This sample subroutine demonstrates the use of functional
c     transformation of the inner integral to reduce the cost of the
c     overall computation.  The factor "cos y" does not depend on the
c     variable of integration for the inner integral, and therefore is
c     factored out.  Similarly, the term y*y in the denominator of the
c     inner integrand is pre-computed when the limits of the inner
c     integral are requested.
c
c     This example also demonstrates the necessity to cope with overflow
c     of the integrand, even though the integral is well behaved.
c
      real             ANSWER, WORK(*)
      integer IFLAG
      integer NDIMI
      real             COSY, DENOM, XDY, YSQ
      save COSY, YSQ
      data NDIMI /2/
c
      if (IFLAG .EQ. 0) then
c
c       IFLAG = 0, compute innermost integrand.
c
        DENOM = WORK(1) * WORK(1) + YSQ
        if (DENOM .NE. 0.0E0) then
c        This test will not detect overflow of the integrand if the
c        arithmetic underflows gradually.
           ANSWER = WORK(1) / DENOM
        else
c     Special care to avoid a zero denominator.
           XDY = WORK(1) / WORK(2)
           ANSWER = XDY / (WORK(2) * (1.E0 + XDY**2))
        end if
c
      else if (IFLAG .EQ. 1) then
c
c       Compute limits of inner dimension.
c
c   Set WORK(1) = COS(WORK(2)) = Partial derivative, with respect to
c   the integral over the inner dimension, of the transformation
c   applied when integration over the inner dimension is complete.
c   This is so sintm knows how much accuracy it needs for this integral.
c
        WORK(NDIMI+IFLAG) = 0.0E0
        WORK(2*NDIMI+IFLAG) = WORK(2)
        YSQ = WORK(2) * WORK(2)
        COSY = COS(WORK(2))
        WORK(1) = COSY
c
      else if (IFLAG .EQ. 2) then
c
c       Compute limits of outer dimension.
c
        WORK(NDIMI+IFLAG) = 0.0E0
        WORK(2*NDIMI+IFLAG) = 4.0E0*ATAN(1.0E0)
c
      else
c
c       IFLAG < 0, transform inner integrand.
c
        ANSWER = ANSWER * COSY
        WORK(1) = WORK(1) * COSY
c
      end if
c
      return
c
      end
