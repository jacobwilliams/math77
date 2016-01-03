c     program DRSFMIN
c>> 1996-05-28 DRSFMIN Krogh  Moved format up.
c>> 1994-10-19 DRSFMIN Krogh  Changes to use M77CON
c>> 1994-07-15 DRSFMIN CLL
c>> 1993-03-01 CLL Changed "zero" to "minimum".
c>> 1992-03-24 CLL Added missing comma in format statement.
c>> 1987-12-09 DRSFMIN  Lawson  Initial Code.
c     Demo driver for SFMIN.  Finds minimum of a univariate function.
c     C. L. Lawson & S. Y. Chiu, JPL, Aug 1987, Oct 1987
c     F. T. Krogh, Oct. 1987.
c     ------------------------------------------------------------------
c--S replaces "?": DR?FMIN, ?FMIN
c     ------------------------------------------------------------------
      real             A, B, X, XORF, TOL, XTOL, TWO
      parameter(A = -1.0E0, B = 1.0E0, XTOL = 1.0E-7)
      parameter(TWO = 2.0E0)
      integer MODE
    1 format(' Results returned by SFMIN:'/'    MODE = ',i3/
     *'    Solution:     X = ',f16.08/
     *'               f(X) = ',f16.08)
c     ------------------------------------------------------------------
      write(*,'(1x,a)')
     * 'DRSFMIN.. Demo driver for SFMIN, univariate minimum finder.',
     * 'Problem:      Find minimum of 2**X + 2**(-2*X).',
     * 'Exact result: X = 1/3',
     * 'Min value: 1.5 * 2**(1/3) = 1.88988157...'
      X = A
      XORF = B
      TOL = XTOL
      MODE = 0
   10 call SFMIN(X, XORF, MODE, TOL)
      if (MODE .ne. 1) go to 20
         XORF = TWO**X + TWO**(-TWO*X)
      go to 10
   20 continue
      write(*,1) MODE, X, XORF
      stop
      end
