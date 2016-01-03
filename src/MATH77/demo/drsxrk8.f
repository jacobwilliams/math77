c      program DRSXRK8
c>> 2008-02-24 DRSXRK8  Krogh -- Change dimenstions for new usage.
c>> 2001-05-25 DRSXRK8  Krogh -- Added comma to format.
c>> 1997-12-18 DRSXRK8  Krogh   Initial code
c--S replaces "?": DR?XRK8, ?XRK8, ?XRK8F, ?XRK8O
c Sample driver for SXRK8 --  Set up to solve simple two body problem
c with circular motion.
c
      integer NEQ, LIDAT, LDAT, LWORK
      parameter (NEQ=4, LIDAT=49+2, LDAT=32+2*NEQ+2+2, LWORK=9*4+6+8*4)
      integer IDAT(LIDAT)
      real             DAT(LDAT), TS(3), TOL, Y(NEQ), WORK(LWORK)
      real             OPT(6)
c++S Default SETTOL = 'TOL = 1.E-4'
c++  Default SETTOL = 'TOL = 1.D-10'
c++  Replace 'TOL = 1.E-4 ' = SETTOL
      parameter (TOL = 1.E-4 )
      real             XRKEND, XRKAE, XRKTDT
      parameter (XRKEND=0.E0, XRKAE=2.E0, XRKTDT=24.E0)
c            Abs. Err. tol.   Output at intervals of 2 Pi       End opt.
      data  OPT /XRKAE,TOL, XRKTDT,0.E0,6.283185307179586477E0, XRKEND/
      data   IDAT(4), IDAT(5), IDAT(6), IDAT(7) /
     1           NEQ,   LIDAT,    LDAT,   LWORK /
c      Starting:  T     H;  Final T.
      data  TS / 0.E0, 0.E0, 20.E0 /
c      Initial Y
      data Y / 1.E0, 0.E0, 0.E0, 1.E0 /
c
c Do the integration
      IDAT(1) = 0
  100 continue
         call SXRK8(TS, Y, OPT, IDAT, DAT, WORK)
         if (IDAT(1) .NE. 3) go to 100
      stop
      end
 
      subroutine SXRK8F(T, Y, F, IDAT)
c Sample derivative subroutine for use with SXRK8
c This evaluates derivatives for a simple two body problem.
c
      integer          IDAT(*)
      real             T, Y(*), F(*), RQBI
c
c Evaluate the derivatives
      RQBI = 1.E0 / ((sqrt(Y(1)**2+Y(3)**2))**3)
      F(1) = Y(2)
      F(2) = -Y(1)*RQBI
      F(3) = Y(4)
      F(4) = -Y(3)*RQBI
      return
      end
 
      subroutine SXRK8O(TS, Y, IDAT, DAT)
c Sample output subroutine for use with SXRK8.
c This subroutine gives output for a simple two body problem.
c
      integer          IDAT(*)
      real             TS(*), Y(*), DAT(*)
 1000 format (6X, 'RESULTS FOR A SIMPLE 2-BODY PROBLEM'//
     1   8X, 'T', 13X, 'U/V', 11X, 'UP/VP'/' ')
 1001 format (1P,3E15.6 / 15X, 2E15.6/' ')
c
c Do the output
      if (TS(1) .eq. 0.E0) then
        write (*, 1000)
      end if
      write (*, 1001) TS(1), Y(1), Y(2), Y(3), Y(4)
      return
      end
