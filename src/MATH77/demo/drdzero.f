c     program DRDZERO
c>> 1995-05-28 DRDZERO  Krogh  Changes to use M77CON
c>> 1993-05-05 DRDZERO  Krogh  Adjusted to simplify conversion to C.
c>> 1992-03-24 DRDZERO  Krogh  Added "," to format statmement.
c>> 1991-11-25 DRDZERO  Krogh  Cleaned up Fortran version.
c>> 1987-12-09 DRDZERO  Krogh  Initial Code.
c--D replaces "?": DR?ZERO, ?ZERO
c     Demo driver for DZERO.  Univariate zero finder.
c     F. T. Krogh, Sept. 1987.
c     ------------------------------------------------------------------
      double precision X1, X2, F1, F2, A, B, TOL, TWO, EIGHT
      parameter(A = 0.0D0, B = 4.0D0, TOL = 0.0D0)
      parameter(TWO = 2.0D0, EIGHT = 8.0D0)
      integer MODE
   10 format(' Results from subroutine DZERO:'/'    MODE = ', I3/
     *'    Solution:        X1 = ',F11.8/
     *'                  f(X1) = ',1P,G11.3/
     *'    Accuracy:   X1 - X2 = ',G11.3/
     *'                  f(X2) = ',G11.3)
c     ------------------------------------------------------------------
      write(*,'(1x,a)')
     * 'DRDZERO.. Demo driver for DZERO, univariate zero finder.',
     * 'Problem: Find zero of 2**X - 8.   Exact result: X = 3.'
      X2 = B
      F2 = TWO**X2 - EIGHT
      MODE = 0
      X1 = A
   20 F1 = TWO**X1 - EIGHT
      call DZERO(X1, F1, X2, F2, MODE, TOL)
      if ( MODE .eq. 1) go to 20
      write(*,10) MODE, X1, F1, X1-X2, F2
      stop
      end
