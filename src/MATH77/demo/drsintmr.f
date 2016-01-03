c     program DRSINTMR
c>> 2003-03-30 DRSINTMR Krogh Changed way of dealing with 0 denominator.
c>> 2001-05-22 DRSINTMR Krogh Minor change for making .f90 version.
c>> 1994-10-19 DRSINTMR Krogh  Changes to use M77CON
c>> 1994-08-31 DRSINTMR Snyder  Moved formats for C conversion
c>> 1994-08-08 DRSINTMR Snyder  Took '0' out of formats for C conversion
c>> 1991-11-20 CLL Edited for Fortran 90.
c>> 1987-12-09 DRSINTMR Snyder  Initial Code.
c--S replaces "?": DR?INTMR, ?INTM, ?INTMA, ?INTA
c
c     DEMO DRIVER for multi-dimensional quadrature subprogram SINTM.
c
c     Compute the integral for Y = 0.0 to Y = PI of
c             the integral for X = 0.0 to X = Y of
c             X * COS(Y) /(X*X+Y*Y).  The ANSWER should be zero.
c
c     The integrand and all limits are provided by reverse
c     communication.
c
c     This sample demonstrates the use of functional transformation of
c     the inner integral to reduce the cost of the overall computation.
c     The factor "cos y" does not depend on the variable of integration
c     for the inner integral, and therefore is factored out.  Similarly,
c     the term y*y in the denominator of the inner integrand is pre-
c     computed when the limits of the inner integral are requested.
c
c     This example also demonstrates the necessity to cope with overflow
c     of the integrand, even though the integral is well behaved.
c
      integer IFLAG, IOPT(10), NDIMI, NWPD, NWORK
      parameter (NDIMI=2, NWPD=217, NWORK=3*NDIMI+NWPD*(NDIMI-1))
      real             ANSWER, WORK(NWORK)
      real             COSY, DENOM, XDY, YSQ
c
10    format (/' DRSINTMR:'/
     1' Compute the integral for Y = 0.0 to Y = PI of'/
     2'         the integral for X = 0.0 to X = Y of'/
     3'         X * COS(Y) /(X*X+Y*Y).  The ANSWER should be zero.')
20    format (/' ANSWER =        ',G15.8/
     1         ' ERROR ESTIMATE =',G15.8/
     2         ' STATUS FLAG =   ',I3/
     3         ' FUNCTION VALUES (INNERMOST INTEGRALS) =',I6)
c
      print 10
      IOPT(2) = 10
      IOPT(3) = 0
      IOPT(4) = 6
      IOPT(5) = 0
      call SINTM (NDIMI, ANSWER, WORK, NWORK, IOPT)
 30   continue
      call SINTMA (ANSWER, WORK, IOPT)
      IFLAG = IOPT(1)
      if (IFLAG .eq. 0) then
c
c     IFLAG = 0, compute innermost integrand.
c
c     Iterate on the innermost integrand by calling SINTA here.
c     The code would be slightly simpler, but slightly slower,
c     if control of the iteration were relegated to SINTMA.
c
 40      continue
         DENOM = WORK(1) * WORK(1) + YSQ
         if (DENOM .ne. 0.0E0) then
c     This test will not detect overflow of the integrand if
c     the arithmetic underflows gradually.
            ANSWER = WORK(1) / DENOM
         else
c     Special care to avoid a zero denominator.
            XDY = WORK(1) / WORK(2)
            ANSWER = XDY / (WORK(2) * (1.E0 + XDY**2))
         end if
         call SINTA (ANSWER, WORK, IOPT)
         if(IOPT(1) .eq. 0) goto 40
c
         if (IOPT(1) .gt. 0) then
c                                 Done with the integration
            IOPT(1) = -(IOPT(1)+NDIMI)
            go to 60
         end if
c
      else if (IFLAG .eq. 1) then
c
c     Compute limits of inner dimension.
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
      else if (IFLAG .eq. 2) then
c
c     Compute limits of outer dimension.
c
         WORK(NDIMI+IFLAG) = 0.0E0
         WORK(2*NDIMI+IFLAG) = 4.0E0*ATAN(1.0E0)
c
      else
         if (IFLAG+NDIMI .le. 0) goto 60
c
c     IFLAG < 0, transform inner integrand.
c
         ANSWER = ANSWER * COSY
         WORK(1) = WORK(1) * COSY
c
      end if
      go to 30
c
60    continue
 
      print 20, ANSWER, WORK(1), IOPT(1), IOPT(3)
      stop
      end
