c      program DRDLNREL
c>> 1999-07-27 DRDLNREL Krogh analyz => DANAL for global analysis.
c>> 1996-07-09 DRDLNREL Krogh Moved formats up.
c>> 1994-11-02 DRDLNREL Krogh  Changes to use M77CON
c>> 1994-07-06 DRDLNREL WVS set up for chgtyp
c
c     Demonstration driver for more accurate routines from chapter 2-15.
c     To illustrate worst-case, expressions are computed little-by-
c     little.  If possible, the routine should be compiled with an
c     option that causes results not to be stored in registers from one
c     assignment to the next.  For Lahey Fortran on a PC, use /7.
c
c--D replaces "?": DR?LNREL, ?GAMMA, ?LNREL, ?RLOG1, ?RLOG, ?REXP
c--   &              ?SINPX, ?COSPX, ?SINHM, ?COSHM, ?CSHMM, ?GAM1
c--   &              ?COS1, ?SIN1, ?ANAL
      double precision DPI
      parameter (DPI = 3.141592653589793238462643383279502884197D0)
      double precision D, D1MACH, DGAMMA, U, W
      external D1MACH, DGAMMA
      double precision DLNREL, DRLOG1, DRLOG, DREXP, DSINPX, DCOSPX
      double precision DSINHM, DCOSHM, DCSHMM, DGAM1
      double precision DCOS1, DSIN1
      external DLNREL, DRLOG1, DRLOG, DREXP, DSINPX, DCOSPX
      external DSINHM, DCOSHM, DCSHMM, DGAM1
      external DCOS1, DSIN1
10    format (' Error is (relative error) / (round off level).'/
     1' Round off level is (smallest number r such that 1 + r .NE. 1).'/
     2' For the present machine, r =',1p,g13.6/
     3' Errors less than 0.5 r should be considered to be zero.')
c
c     DLNREL
      d = 1.0d0 / 2.0d0**12
      w = 1.0d0 + d
      w = log(w)
      u = dlnrel(d)
      call DANAL ('DLNREL',d,u,w)
c
c     DRLOG1
      w = d - w
      u = drlog1(d)
      call DANAL ('DRLOG1',d,u,w)
c
c     DRLOG
      w = d - 1.0d0
      w = w - log(d)
      u = drlog(d)
      call DANAL ('DRLOG ',d,u,w)
c
c     DREXP
      w = exp(d)
      w = w - 1.0d0
      u = drexp(d)
      call DANAL ('DREXP ',d,u,w)
c
c     DSINPX
      d = 25.125d0
      w = sin(d*dpi)
      u = dsinpx(d)
      call DANAL ('DSINPX',d,u,w)
c
c     DCOSPX
      w = cos(d*dpi)
      u = dcospx(d)
      call DANAL ('DCOSPX',d,u,w)
c
c     DSIN1
      d = 0.5d0**22
      w = sin(d)
      w = (d - w) / d**3
      u = dsin1(d)
      call DANAL ('DSIN1 ',d,u,w)
c
c     DCOS1
      w = cos(d)
      w = (d - w) / d**2
      u = dcos1(d)
      call DANAL ('DCOS1 ',d,u,w)
c
c     DSINHM
      d = 0.25d0
      w = sinh(d)
      w = w - d
      u = dsinhm(d)
      call DANAL ('DSINHM',d,u,w)
c
c     DCOSHM
      w = cosh(d)
      w = w - 1.0d0
      u = dcoshm(d)
      call DANAL ('DCOSHM',d,u,w)
c
c     DCSHMM
      w = cosh(d)
      w = w - 1.0d0
      w = w - 0.5*d*d
      u = dcshmm(d)
      call DANAL ('DCSHMM',d,u,w)
c
c     DGAM1
      w = 1.0d0/dgamma(1.0d0+d)
      w = w - 1.0d0
      u = dgam1(d)
      call DANAL ('DGAM1 ',d,u,w)
c
      print 10, d1mach(4)
      stop
      end

      subroutine DANAL (ROUTIN, X, U, W)
      character*6 ROUTIN
      double precision X, U, W, EW
      double precision D1MACH
      external D1MACH
      double precision ROUND
      save ROUND
      data ROUND /-1.0d0/
10    format (
     1' Function                    Result         ---- Not Using ----'/
     2' From This                   Using This     --- This Package --'/
     3' Package        Argument     Package        Result        Error')
20    format (1x,a6,5x,1p,e12.4,2x,e13.6,2x,e13.6,1x,e9.2)
      if (round .lt. 0.0d0) then
         round = d1mach(4)
         print 10
      end if
      ew = abs(w-u)/abs(u*round)
      print 20, routin,x,u,w,ew
      return
      end
