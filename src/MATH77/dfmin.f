      subroutine DFMIN(X,XORF,MODE,TOL)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2004-11-11 DFMIN  Krogh  Removed if (..) C3 = statement doing nil.
c>> 2000-12-01 DFMIN  Krogh  Removed unused parameter ONE.
c>> 1996-03-30 DFMIN  Krogh  Added external statement.
C>> 1994-10-20 DFMIN  Krogh  Changes to use M77CON
C>> 1994-04-20 DFMIN  CLL Edited to make DP & SP files similar.
C>> 1987-12-09 DFMIN  Lawson  Initial code.
c
C     Finds a local minimum of a function f(x) in the closed interval
c     between A and B.
c     The function f(x) is defined by code in the calling program.
c     The user must initially provide A, B, and a tolerance, TOL.
c     The function will not be evaluated at A or B unless the
c     minimization search leads to one of these endpoints.
c
c     This subroutine uses reverse communication, i.e., it returns to
c     the calling program each time it needs to have f() evaluated at a
c     new value of x.
c     ------------------------------------------------------------------
c                   Subroutine Arguments
c
c  X, XORF, MODE  [all are inout]
c     On the initial call to this subroutine to solve a
c     new problem, the user must set MODE = 0, and must set
c        X = A
c        XORF = B
c        TOL = desired tolerance
c     A and B denote endpoints defining a closed
c     interval in which a local minimum is to be found.
c     Permit A < B or A > B or A = B.  If A = B the solution, x = A,
c     will be found immediately.
c     On any call the user can set MODE negative to start or stop
c     detail printing.  On such an entry we set NP = -1 - MODE, and
c     the normal algorithm will not be executed.  On the next NP calls
c     with MODE = 0 or 1, a detail line will be printed.
c
c  TOL [in]  is an absolute tolerance on the uncertainty in the final
c     estimate of the minimizing abcissa, X1.  Recommend TOL .ge. 0.
c     This subr will set TOLI = TOL if TOL > 0., and otherwise sets
c     TOLI = 0..  The operational tolerance at any trial abcissa, X,
c     will be
c         TOL2 = 2 * abs(X) * sqrt(D1MACH(4)) + (2/3) * C3
c     where C3 = TOLI/3.D0 + D1MACH(4)**2 where TOLI = max(TOL,0.D0).
c
c     On each return this subr will set MODE to a value in the range
c     [1:3] to indicate the action needed from the calling program or
c     the status on termination.
c
c     = 1 means the calling program must evaluate
c     f(X), store the value in XORF, and then call this
c     subr again.
c
c     = 2 means normal termination.  X contains the point of the
c     minimum and XORF contains f(X).
c
c     = 3 as for 2 but the requested accuracy could not be obtained.
c
c     = 4 means termination on an error condition:
c         MODE on entry not in [0:1].
c     ------------------------------------------------------------------
c  This algorithm is due to Richard. P. Brent.  It is presented as
c  an ALGOL procedure, LOCALMIN, in his 1973 book,
c  "Algorithms for minimization without derivatives", Prentice-Hall.
c  Published as subroutine FMIN in Forsythe, Malcolm, and Moler,
c  "Computer Methods for Mathematical Computations", Prentice-Hall,
c  1977.
c  The current subroutine adapted from F.,M.& M. by C. L. Lawson, and
c  F. T. Krogh, JPL, Oct 1987, for use in Fortran 77 in the JPL
c  MATH77 library.  The changes improve performance when a minimum is
c  at an endpoint, and change the user interface to reverse
c  communication.  Unlike the original, the changed algorithm may
c  evaluate the function at one of the end points.
c     ------------------------------------------------------------------
c  Subprograms referenced: D1MACH, IERM1
c     ------------------------------------------------------------------
c  THE METHOD USED IS A COMBINATION OF  GOLDEN  SECTION  SEARCH  AND
C  SUCCESSIVE PARABOLIC INTERPOLATION.  CONVERGENCE IS NEVER MUCH SLOWER
C  THAN  THAT  FOR  A  FIBONACCI SEARCH.  IF  F  HAS A CONTINUOUS SECOND
C  DERIVATIVE WHICH IS POSITIVE AT THE MINIMUM (WHICH IS NOT  AT  AX  OR
C  BX),  THEN  CONVERGENCE  IS  SUPERLINEAR, AND USUALLY OF THE ORDER OF
C  ABOUT  1.324....
c   THE FUNCTION  F  IS NEVER EVALUATED AT TWO POINTS CLOSER TOGETHER
C  THAN  EPS*ABS(FMIN) + (TOLI/3), WHERE EPS IS APPROXIMATELY THE SQUARE
C  ROOT  OF  THE  RELATIVE  MACHINE  PRECISION.  IF F IS A UNIMODAL
C  FUNCTION AND THE COMPUTED VALUES OF F ARE ALWAYS UNIMODAL WHEN
C  SEPARATED BY AT LEAST  EPS*ABS(X) + (TOLI/3), THEN  FMIN APPROXIMATES
C  THE ABCISSA OF THE GLOBAL MINIMUM OF F ON THE INTERVAL [AX,BX] WITH
C  AN ERROR LESS THAN  3*EPS*ABS(FMIN) + TOLI.  IF F IS NOT UNIMODAL,
C  THEN FMIN MAY APPROXIMATE A LOCAL, BUT PERHAPS NON-GLOBAL, MINIMUM TO
C  THE SAME ACCURACY. (Comments from F., M. & M.)
c     ------------------------------------------------------------------
c--D replaces "?": ?FMIN
c     Both versions use IERM1
c     ------------------------------------------------------------------
      external D1MACH
      double precision X,XORF,TOL, D1MACH
      double precision A,B,C,D,E,EPS,XM,P,Q,R,TOL1,TOL2,U,V,W
      double precision FU,FV,FW,FX,XI, TOLI, C3
      double precision ASAVE, BSAVE
      double precision HALF, TWO, THREE, FIVE, ZERO, PT95
      integer MODE, NEXT, IC, NP
      parameter(HALF = 0.5D0, TWO = 2.0D0, THREE = 3.0D0)
      parameter(FIVE = 5.0D0, ZERO = 0.0D0, PT95 = 0.95D0)
      save
      data EPS / ZERO /, NP / 0 /, IC / 0 /
c     ------------------------------------------------------------------
      IF (MODE .LT. 0) THEN
         NP = -1 - MODE
         RETURN
      END IF
      IF (NP .GT. 0) THEN
         NP = NP - 1
         print*, ' In DFMIN: X= ',X,', XORF= ',XORF,', IC= ',IC
      END IF
      if(MODE .eq. 1) then
         go to (301,302), NEXT
      endif
      if( MODE .ne. 0) then
c                                           Error:  MODE > 1 on entry.
         call IERM1('DFMIN',MODE,0,
     *   'Input value of MODE exceeds 1.','MODE',MODE,'.')
         MODE = 4
         return
      endif
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C               C is the squared inverse of the "Golden Ratio", 1.618...
C               C = 0.381966
c
      C = HALF*(THREE - sqrt(FIVE))
C
C           EPS IS THE SQUARE ROOT OF THE RELATIVE MACHINE  PRECISION.
C
      if (EPS .eq. ZERO) EPS = sqrt(D1MACH(4))
C
C  INITIALIZATION
C
      A = X
      B = XORF
      if( A .gt. B) then
         XI = A
         A = B
         B = XI
      endif
c                           Now we have A .le. B
      ASAVE = A
      BSAVE = B
      TOLI = TOL
      if(TOLI .le. ZERO) TOLI = ZERO
      C3 = TOLI/THREE + EPS ** 4
      V = A + C*(B - A)
      W = V
      XI = V
      IC = 0
      E = ZERO
c                   Return to calling prog for computation of FX = f(XI)
      X = XI
      MODE = 1
      NEXT = 1
      return
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  301 continue
      NEXT = 2
      FX = XORF
      FV = FX
      FW = FX
C                                         MAIN LOOP STARTS HERE
   20 XM = HALF*(A + B)
      TOL1 = EPS*abs(XI) + C3
      TOL2 = TWO*TOL1
C                            CHECK STOPPING CRITERION
C
      if (abs(XI - XM) .le. (TOL2 - HALF*(B - A))) go to 90
C
C                            IS GOLDEN-SECTION NECESSARY ?
C
      if (abs(E) .le. TOL1) go to 40
C
C                            FIT PARABOLA
C
      R = (XI - W)*(FX - FV)
      Q = (XI - V)*(FX - FW)
      P = (XI - V)*Q - (XI - W)*R
      Q = TWO*(Q - R)
      if (Q .gt. ZERO) P = -P
      Q =  abs(Q)
      R = E
      E = D
C
C  IS PARABOLA ACCEPTABLE
C
      if (abs(P) .ge. abs(HALF*Q*R)) go to 40
      if (P .le. Q*(A - XI)) go to 40
      if (P .ge. Q*(B - XI)) go to 40
C
C  A PARABOLIC INTERPOLATION STEP
C
      D = P/Q
      U = XI + D
C
C  F MUST NOT BE EVALUATED TOO CLOSE TO A OR B
C
      if ((U - A) .lt. TOL2 .or. (B - U) .lt. TOL2)
     *    D = sign(TOL1, XM - XI)
      go to 50
C
C  A GOLDEN-SECTION STEP
C
   40 IC = IC + 1
      if (IC .gt. 3) then
         if (A .eq. ASAVE) then
            if (IC .eq. 4) then
               E = A + (EPS * abs(A) + C3) - XI
            else
               if (B .ne. W) go to 45
               E = A - XI
            end if
         else if (B .eq. BSAVE) then
            if (IC .eq. 4) then
               E = B - (EPS * abs(B) + C3) - XI
            else
               if (A .ne. W) go to 45
               E = B - XI
            end if
         else
            IC = -99
            go to 45
         end if
         D = E
         go to 50
      end if
   45 if (XI .ge. XM) then
         E = A - XI
      else
         E = B - XI
      endif
      D = C*E
C
C  F MUST NOT BE EVALUATED TOO CLOSE TO XI
C
   50 if (abs(D) .ge. TOL1) then
         U = XI + D
      else
         if (B - A .gt. THREE * TOL1) TOL1 = TOL2
         U = min(B, max(A, XI + sign(PT95*TOL1, D)))
      endif
c
c                    Return to calling prog for computation of FU = f(U)
c                    Returning with MODE = 1 and NEXT = 2
      X = U
      return
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  302 continue
      FU = XORF
C
C  UPDATE  A, B, V, W, AND XI
C
      if (FU .le. FX) then
         if (FU .eq. FX) then
            if (B - min(XI, U) .gt. max(XI, U) - A) then
               B = max(XI, U)
            else
               A = min(XI, U)
            end if
         else
            if (U .ge. XI) then
               A = XI
            else
               B = XI
            endif
         end if
         V = W
         FV = FW
         W = XI
         FW = FX
         XI = U
         FX = FU
         go to 20
      endif
c
      if (U .lt. XI) then
         A = U
      else
         B = U
      endif
      if (FU .le. FW  .or.  W .eq. XI) then
         V = W
         FV = FW
         W = U
         FW = FU
      elseif (FU .le. FV  .or.  V .eq. XI  .or.  V .eq. W) then
         V = U
         FV = FU
      endif
      go to 20
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   90 continue
         X = XI
         XORF = FX
         MODE = 2
         if ((B - A .gt. THREE*TOLI) .and. (TOLI .ne. ZERO)) MODE = 3
         return
         end
