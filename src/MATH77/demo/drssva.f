      program DRSSVA
c>> 1996-06-27 DRSSVA   Krogh  Special code for C conversion.
c>> 1996-05-28 DRSSVA   Krogh  Removed implicit statement.
c>> 1994-10-19 DRSSVA   Krogh  Changes to use M77CON
c>> 1989-03-07 DRSSVA   CLL
c     Demo driver for SSVA, Singular Value Analysis.
c
c     Mar 1987, C. L. Lawson & S. Y. Chiu, JPL.  This sample problem
c     taken from the book, Solving Least Squares Problems,
c     Prentice-Hall, 1974, by C. L. Lawson and R. J. Hanson.
c     ------------------------------------------------------------------
c--S replaces "?": DR?SVA, ?SVA
c     ------------------------------------------------------------------
      integer I, J, KPVEC(4), M, MDATA, MMAX, N, NMAX, NMAX2
      parameter(MMAX = 15, NMAX = 5, NMAX2 = NMAX*2)
      real             A(MMAX,NMAX), B(MMAX), SING(NMAX)
      real             D(NMAX), WORK(NMAX2)
      character*6 NAMES(NMAX)
c
      data ((A(I,J),J=1,5),I=1,15) /
     *-.13405547E0,-.20162827E0,-.16930778E0,-.18971990E0,-.17387234E0,
     *-.10379475E0,-.15766336E0,-.13346256E0,-.14848550E0,-.13597690E0,
     *-.08779597E0,-.12883867E0,-.10683007E0,-.12011796E0,-.10932972E0,
     * .02058554E0, .00335331E0,-.01641270E0, .00078606E0, .00271659E0,
     *-.03248093E0,-.01876799E0, .00410639E0,-.01405894E0,-.01384391E0,
     * .05967662E0, .06667714E0, .04352153E0, .05740438E0, .05024962E0,
     * .06712457E0, .07352437E0, .04489770E0, .06471862E0, .05876455E0,
     * .08687186E0, .09368296E0, .05672327E0, .08141043E0, .07302320E0,
     * .02149662E0, .06222662E0, .07213486E0, .06200069E0, .05570931E0,
     * .06687407E0, .10344506E0, .09153849E0, .09508223E0, .08393667E0,
     * .15879069E0, .18088339E0, .11540692E0, .16160727E0, .14796479E0,
     * .17642887E0, .20361830E0, .13057860E0, .18385729E0, .17005549E0,
     * .11414080E0, .17259611E0, .14816471E0, .16007466E0, .14374096E0,
     * .07846038E0, .14669563E0, .14365800E0, .14003842E0, .12571177E0,
     * .10803175E0, .16994623E0, .14971519E0, .15885312E0, .14301547E0 /
c
      data (B(I),I=1,15) /
     *-.4361E0, -.3437E0, -.2657E0, -.0392E0, .0193E0, .0747E0,
     * .0935E0,  .1079E0, .1930E0, .2058E0, .2606E0, .3142E0,
     * .3529E0, .3615E0, .3647E0 /
c
      data NAMES /'Earth ', 'Water ', 'Air   ', 'Fire  ', 'Time  '/
      data KPVEC / 1, 111111, -1, 76/
c     ------------------------------------------------------------------
c
      M = MMAX
      N = NMAX
      MDATA = M
      print '(''   DRSSVA..    Demonstrate Singular Value Analysis''/
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
      call SSVA(A, MMAX, M, N, MDATA, B, SING, KPVEC, NAMES, 1, D, WORK)
c
      stop
      end
