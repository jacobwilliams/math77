c      program DRSLNREL
c>> 1999-07-27 DRSLNREL Krogh analyz => SANAL for global analysis.
c>> 1996-07-09 DRSLNREL Krogh Moved formats up.
c>> 1994-11-02 DRSLNREL Krogh  Changes to use M77CON
c>> 1994-07-06 DRSLNREL WVS set up for chgtyp
c
c     Demonstration driver for more accurate routines from chapter 2-15.
c     To illustrate worst-case, expressions are computed little-by-
c     little.  If possible, the routine should be compiled with an
c     option that causes results not to be stored in registers from one
c     assignment to the next.  For Lahey Fortran on a PC, use /7.
c
c--S replaces "?": DR?LNREL, ?GAMMA, ?LNREL, ?RLOG1, ?RLOG, ?REXP
c--   &              ?SINPX, ?COSPX, ?SINHM, ?COSHM, ?CSHMM, ?GAM1
c--   &              ?COS1, ?SIN1, ?ANAL
      real             DPI
      parameter (DPI = 3.141592653589793238462643383279502884197E0)
      real             D, R1MACH, SGAMMA, U, W
      external R1MACH, SGAMMA
      real             SLNREL, SRLOG1, SRLOG, SREXP, SSINPX, SCOSPX
      real             SSINHM, SCOSHM, SCSHMM, SGAM1
      real             SCOS1, SSIN1
      external SLNREL, SRLOG1, SRLOG, SREXP, SSINPX, SCOSPX
      external SSINHM, SCOSHM, SCSHMM, SGAM1
      external SCOS1, SSIN1
10    format (' Error is (relative error) / (round off level).'/
     1' Round off level is (smallest number r such that 1 + r .NE. 1).'/
     2' For the present machine, r =',1p,g13.6/
     3' Errors less than 0.5 r should be considered to be zero.')
c
c     SLNREL
      d = 1.0e0 / 2.0e0**12
      w = 1.0e0 + d
      w = log(w)
      u = slnrel(d)
      call SANAL ('SLNREL',d,u,w)
c
c     SRLOG1
      w = d - w
      u = srlog1(d)
      call SANAL ('SRLOG1',d,u,w)
c
c     SRLOG
      w = d - 1.0e0
      w = w - log(d)
      u = srlog(d)
      call SANAL ('SRLOG ',d,u,w)
c
c     SREXP
      w = exp(d)
      w = w - 1.0e0
      u = srexp(d)
      call SANAL ('SREXP ',d,u,w)
c
c     SSINPX
      d = 25.125e0
      w = sin(d*dpi)
      u = ssinpx(d)
      call SANAL ('SSINPX',d,u,w)
c
c     SCOSPX
      w = cos(d*dpi)
      u = scospx(d)
      call SANAL ('SCOSPX',d,u,w)
c
c     SSIN1
      d = 0.5e0**22
      w = sin(d)
      w = (d - w) / d**3
      u = ssin1(d)
      call SANAL ('SSIN1 ',d,u,w)
c
c     SCOS1
      w = cos(d)
      w = (d - w) / d**2
      u = scos1(d)
      call SANAL ('SCOS1 ',d,u,w)
c
c     SSINHM
      d = 0.25e0
      w = sinh(d)
      w = w - d
      u = ssinhm(d)
      call SANAL ('SSINHM',d,u,w)
c
c     SCOSHM
      w = cosh(d)
      w = w - 1.0e0
      u = scoshm(d)
      call SANAL ('SCOSHM',d,u,w)
c
c     SCSHMM
      w = cosh(d)
      w = w - 1.0e0
      w = w - 0.5*d*d
      u = scshmm(d)
      call SANAL ('SCSHMM',d,u,w)
c
c     SGAM1
      w = 1.0e0/sgamma(1.0e0+d)
      w = w - 1.0e0
      u = sgam1(d)
      call SANAL ('SGAM1 ',d,u,w)
c
      print 10, r1mach(4)
      stop
      end
 
      subroutine SANAL (ROUTIN, X, U, W)
      character*6 ROUTIN
      real             X, U, W, EW
      real             R1MACH
      external R1MACH
      real             ROUND
      save ROUND
      data ROUND /-1.0e0/
10    format (
     1' Function                    Result         ---- Not Using ----'/
     2' From This                   Using This     --- This Package --'/
     3' Package        Argument     Package        Result        Error')
20    format (1x,a6,5x,1p,e12.4,2x,e13.6,2x,e13.6,1x,e9.2)
      if (round .lt. 0.0e0) then
         round = r1mach(4)
         print 10
      end if
      ew = abs(w-u)/abs(u*round)
      print 20, routin,x,u,w,ew
      return
      end
