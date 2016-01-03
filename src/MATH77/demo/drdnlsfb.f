c     program DRDNLSFB
c>> 2001-05-24 DRDNLSFB Krogh Minor change for making .f90 version.
c>> 1994-11-02 DRDNLSFB Krogh  Changes to use M77CON
c>> 1994-09-14 DRDNLSFB CLL Set IV(OUTLEV) = 0 for comparing output.
c>> 1992-02-03 CLL @ JPL
c>> 1990-07-02 CLL @ JPL
c>> 1990-06-27 CLL @ JPL
c>> 1990-06-14 CLL @ JPL
c>> 1990-04-05 CLL @ JPL
c>> 1990-03-29 CLL @ JPL
c     Demo driver for DNLSFB. A variant of the nonlinear LS code NL2SOL.
c     DNLSFB solves the "separable" problem.
c     DNLSFB handles bounds on the nonlinear variables.
c     DNLSFB requires function values only.
c     Note:  The expressions below set LIV and LV larger than
c     necessary because the precise formulas cannot be written in a
c     Fortran parameter statement.
c     ------------------------------------------------------------------
c--D replaces "?": DR?NLSFB, ?NLSFB, ?CALCA, ?IVSET
c     ------------------------------------------------------------------
      external DCALCA
      integer  J,ITERM,IVAR,LIV,LV,MA,MB,MDATA,MDIR,NA,NB,NDATA
      parameter(MDATA = 30, MA = 2, MB = 5, MDIR = 4)
      parameter(LIV = 122 + 2*MDIR + 7*MA + 2*MB + MB+1 + 6*MA)
      parameter(LV = 105 + MDATA*(2*MB + 6 + MA) +
     *               (MB*(MB+3))/2 + MA*(2*MA + 22))
      integer IND(MB+1,MA), IV(LIV)
      integer F, OUTLEV, SOLPRT, STATPR, X0PRT
      parameter(F=10, OUTLEV=19, SOLPRT=22, STATPR=23, X0PRT=24)
      double precision ALF(MA), BET(MB), BND(2,MA)
      double precision DOF, V(LV), YDATA(MDATA)
      data (BND(1,J),J=1,2) /  2*5.0d0 /
      data (BND(2,J),J=1,2) / 2*10.0d0 /
      data ((IND(ITERM,IVAR),IVAR = 1,2),ITERM = 1,6)/
     *     0, 0,
     *     1, 0,
     *     1, 0,
     *     0, 1,
     *     0, 1,
     *     0, 0/
      data YDATA /
     *     1.700641d0, 1.793512d0, 1.838309d0, 1.838416d0, 1.792204d0,
     *     1.700501d0, 1.579804d0, 1.426268d0, 1.260724d0, 1.084901d0,
     *     0.917094d0, 0.761920d0, 0.627304d0, 0.522146d0, 0.446645d0,
     *     0.404920d0, 0.392033d0, 0.409622d0, 0.453045d0, 0.510765d0,
     *     0.584554d0, 0.663109d0, 0.747613d0, 0.829439d0, 0.908496d0,
     *     0.983178d0, 1.051046d0, 1.114072d0, 1.171746d0, 1.227823d0/
c     ------------------------------------------------------------------
      NDATA = MDATA
      NA = MA
      NB = MB
      ALF(1) =    5.0d0
      ALF(2) =   10.0d0
      IV(1) = 0

      print'(1x,a)',
     * 'Program DRDNLSFB.. Demo driver for DNLSFB.',
     * '   A variant of NL2SOL.',
     * '   DNLSFB handles the Separable problem.',
     * '   DNLSFB requires function values but not the Jacobian.',
     * '   DNLSFB handles bounds on the nonlinear variables.',
     * ' ',
     * 'Sample problem is a nonlinear curve fit to data.',
     * 'Model function is B1 + B2 * cos(A1*t) + B3 * sin(A1*t) +',
     * '                       B4 * cos(A2*t) + B5 * sin(A2*t) + Noise',
     * 'Data generated using',
     * '(A1, A2, B1, ..., B5) = (6, 9, 1, 0.5, 0.4, 0.2, 0.1)',
     * 'and Gaussian noise with mean 0 and',
     * 'sample standard deviation 0.001',
     * ' '

      call DIVSET(1, IV, LIV, LV, V)
      IV( X0PRT) = 1
      IV(OUTLEV) = 0
      IV(STATPR) = 1
      IV(SOLPRT) = 1

      call DNLSFB(NDATA, NA, NB, ALF, BND, BET, YDATA, DCALCA,
     *         IND, NB+1, IV, LIV, LV, V)

      DOF = max(NDATA - NA - NB, 1)
      print'(1x/1x,a,g12.4)',
     *    'SIGFAC: sqrt((2 * V(F))/DOF) =',
     *             sqrt(2.0d0 * V(F)/DOF)
      stop
      end
c     ==================================================================
      subroutine DCALCA(NDATA, NA, NB, ALF, NCOUNT, PHI)
c     Test case for separable nonlinear least squares computation.
c     Computes MDATA x NB matrix PHI as a function of the
c     nonlinear parameters ALF().
c     For J .le. NB the (I,J) term of PHI is the coefficient of the
c     linear coefficient B(J) in row I of the model.
c     In this example the model does not have a term that is not
c     multiplied by a linear coefficient.  If such a term is present
c     then PHI must have an (NB+1)st column to hold this term.
c     ------------------------------------------------------------------
      integer I, MDATA, NA, NB, NCOUNT, NDATA
      parameter(MDATA = 30)
      double precision ALF(NA)
      double precision DEL, PHI(NDATA,NB), T
c     ------------------------------------------------------------------
      T = 0.0D0
      DEL = 1.0D0 / 29.0D0
      do 10 I = 1,NDATA
         PHI(I,1) = 1.0D0
         PHI(I,2) = cos(ALF(1)*T)
         PHI(I,3) = sin(ALF(1)*T)
         PHI(I,4) = cos(ALF(2)*T)
         PHI(I,5) = sin(ALF(2)*T)
         T = T + DEL
   10 continue
      return
      end
