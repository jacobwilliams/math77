      program DRDSVA
c>> 1996-06-27 DRDSVA   Krogh  Special code for C conversion.
c>> 1996-05-28 DRDSVA   Krogh  Removed implicit statement.
c>> 1994-10-19 DRDSVA   Krogh  Changes to use M77CON
c>> 1989-03-07 DRDSVA   CLL
c     Demo driver for DSVA, Singular Value Analysis.
c
c     Mar 1987, C. L. Lawson & S. Y. Chiu, JPL.  This sample problem
c     taken from the book, Solving Least Squares Problems,
c     Prentice-Hall, 1974, by C. L. Lawson and R. J. Hanson.
c     ------------------------------------------------------------------
c--D replaces "?": DR?SVA, ?SVA
c     ------------------------------------------------------------------
      integer I, J, KPVEC(4), M, MDATA, MMAX, N, NMAX, NMAX2
      parameter(MMAX = 15, NMAX = 5, NMAX2 = NMAX*2)
      double precision A(MMAX,NMAX), B(MMAX), SING(NMAX)
      double precision D(NMAX), WORK(NMAX2)
      character*6 NAMES(NMAX)
c
      data ((A(I,J),J=1,5),I=1,15) /
     *-.13405547D0,-.20162827D0,-.16930778D0,-.18971990D0,-.17387234D0,
     *-.10379475D0,-.15766336D0,-.13346256D0,-.14848550D0,-.13597690D0,
     *-.08779597D0,-.12883867D0,-.10683007D0,-.12011796D0,-.10932972D0,
     * .02058554D0, .00335331D0,-.01641270D0, .00078606D0, .00271659D0,
     *-.03248093D0,-.01876799D0, .00410639D0,-.01405894D0,-.01384391D0,
     * .05967662D0, .06667714D0, .04352153D0, .05740438D0, .05024962D0,
     * .06712457D0, .07352437D0, .04489770D0, .06471862D0, .05876455D0,
     * .08687186D0, .09368296D0, .05672327D0, .08141043D0, .07302320D0,
     * .02149662D0, .06222662D0, .07213486D0, .06200069D0, .05570931D0,
     * .06687407D0, .10344506D0, .09153849D0, .09508223D0, .08393667D0,
     * .15879069D0, .18088339D0, .11540692D0, .16160727D0, .14796479D0,
     * .17642887D0, .20361830D0, .13057860D0, .18385729D0, .17005549D0,
     * .11414080D0, .17259611D0, .14816471D0, .16007466D0, .14374096D0,
     * .07846038D0, .14669563D0, .14365800D0, .14003842D0, .12571177D0,
     * .10803175D0, .16994623D0, .14971519D0, .15885312D0, .14301547D0 /
c
      data (B(I),I=1,15) /
     *-.4361D0, -.3437D0, -.2657D0, -.0392D0, .0193D0, .0747D0,
     * .0935D0,  .1079D0, .1930D0, .2058D0, .2606D0, .3142D0,
     * .3529D0, .3615D0, .3647D0 /
c
      data NAMES /'Earth ', 'Water ', 'Air   ', 'Fire  ', 'Time  '/
      data KPVEC / 1, 111111, -1, 76/
c     ------------------------------------------------------------------
c
      M = MMAX
      N = NMAX
      MDATA = M
      print '(''   DRDSVA..    Demonstrate Singular Value Analysis''/
     *   ''   Listing of input matrix, A, and vector, B, follows..'')'
c++ Code for ~.C. is active
      print '(/(5F12.7,F13.4))',
     *      ((A(I,J),J=1,N),B(I),I=1,M)
c++ Code for .C. is inactive
c%%  for (i = 1; i <= m; i++){
c%%     printf( "\n" );
c%%     for (j = 1; j <= n; j++)
c%%        printf( "%12.7f", a[j - 1][i - 1] );
c%%     printf( "%13.4f", B[i] );}
c%%  fprintf( stdout, " \n\n\n \n" );
c++ End
      print '(1X///1X)'
c
      call DSVA(A, MMAX, M, N, MDATA, B, SING, KPVEC, NAMES, 1, D, WORK)
c
      stop
      end
