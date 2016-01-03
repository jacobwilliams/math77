c      program DRDXRK8
c>> 2008-02-24 DRDXRK8  Krogh -- Change dimenstions for new usage.
c>> 2001-05-25 DRDXRK8  Krogh -- Added comma to format.
c>> 1997-12-18 DRDXRK8  Krogh   Initial code
c--D replaces "?": DR?XRK8, ?XRK8, ?XRK8F, ?XRK8O
c Sample driver for DXRK8 --  Set up to solve simple two body problem
c with circular motion.
c
      integer NEQ, LIDAT, LDAT, LWORK
      parameter (NEQ=4, LIDAT=49+2, LDAT=32+2*NEQ+2+2, LWORK=9*4+6+8*4)
      integer IDAT(LIDAT)
      double precision DAT(LDAT), TS(3), TOL, Y(NEQ), WORK(LWORK)
      double precision OPT(6)
c++S Default SETTOL = 'TOL = 1.E-4'
c++  Default SETTOL = 'TOL = 1.D-10'
c++  Replace 'TOL = 1.D-10' = SETTOL
      parameter (TOL = 1.D-10)
      double precision XRKEND, XRKAE, XRKTDT
      parameter (XRKEND=0.D0, XRKAE=2.D0, XRKTDT=24.D0)
c            Abs. Err. tol.   Output at intervals of 2 Pi       End opt.
      data  OPT /XRKAE,TOL, XRKTDT,0.D0,6.283185307179586477D0, XRKEND/
      data   IDAT(4), IDAT(5), IDAT(6), IDAT(7) /
     1           NEQ,   LIDAT,    LDAT,   LWORK /
c      Starting:  T     H;  Final T.
      data  TS / 0.D0, 0.D0, 20.D0 /
c      Initial Y
      data Y / 1.D0, 0.D0, 0.D0, 1.D0 /
c
c Do the integration
      IDAT(1) = 0
  100 continue
         call DXRK8(TS, Y, OPT, IDAT, DAT, WORK)
         if (IDAT(1) .NE. 3) go to 100
      stop
      end

      subroutine DXRK8F(T, Y, F, IDAT)
c Sample derivative subroutine for use with DXRK8
c This evaluates derivatives for a simple two body problem.
c
      integer          IDAT(*)
      double precision T, Y(*), F(*), RQBI
c
c Evaluate the derivatives
      RQBI = 1.D0 / ((sqrt(Y(1)**2+Y(3)**2))**3)
      F(1) = Y(2)
      F(2) = -Y(1)*RQBI
      F(3) = Y(4)
      F(4) = -Y(3)*RQBI
      return
      end

      subroutine DXRK8O(TS, Y, IDAT, DAT)
c Sample output subroutine for use with DXRK8.
c This subroutine gives output for a simple two body problem.
c
      integer          IDAT(*)
      double precision TS(*), Y(*), DAT(*)
 1000 format (6X, 'RESULTS FOR A SIMPLE 2-BODY PROBLEM'//
     1   8X, 'T', 13X, 'U/V', 11X, 'UP/VP'/' ')
 1001 format (1P,3E15.6 / 15X, 2E15.6/' ')
c
c Do the output
      if (TS(1) .eq. 0.D0) then
        write (*, 1000)
      end if
      write (*, 1001) TS(1), Y(1), Y(2), Y(3), Y(4)
      return
      end
