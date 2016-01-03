c     program DRSNLSFU
c>> 2001-05-24 DRSNLSFU Krogh Minor change for making .f90 version.
c>> 1994-11-02 DRSNLSFU Krogh  Changes to use M77CON
c>> 1994-09-14 DRSNLSFU CLL Set IV(OUTLEV) = 0 for comparing output.
c>> 1992-02-03 CLL @ JPL
c>> 1990-07-02 CLL @ JPL
c>> 1990-06-27 CLL @ JPL
c>> 1990-06-14 CLL @ JPL
c>> 1990-04-05 CLL @ JPL
c>> 1990-03-29 CLL @ JPL
c     Demo driver for SNLSFU. A variant of the nonlinear LS code NL2SOL.
c     SNLSFU solves the "separable" problem.
c     SNLSFU requires function values only.
c     Note:  The expressions below set LIV and LV larger than
c     necessary because the precise formulas cannot be written in a
c     Fortran parameter statement.
c     ------------------------------------------------------------------
c--S replaces "?": DR?NLSFU, ?NLSFU, ?CALCA, ?IVSET
c     ------------------------------------------------------------------
      external SCALCA
      integer  ITERM,IVAR,LIV,LV,MA,MB,MDATA,MDIR,MLEN,NA,NB,NDATA
      parameter(MDATA = 30, MA = 2, MB = 5, MDIR = 4)
      parameter(MLEN = (MB+MA)*(MDATA+MB+MA+1))
      parameter(LIV = 122 + 2*MDIR + 4*MA + 2*MB + MB+1 + 6*MA)
      parameter(LV = 105 + 2*MDATA*(MB+3) +
     *               MLEN + (MB*(MB+3))/2 + MA*(2*MA + 18))
      integer IND(MB+1,MA), IV(LIV)
      integer F, COVPRT, OUTLEV, SOLPRT, STATPR, X0PRT
      parameter(F=10)
      parameter(COVPRT=14, OUTLEV=19, SOLPRT=22, STATPR=23, X0PRT=24)
      real             ALF(MA), BET(MB), DOF, V(LV), YDATA(MDATA)
      data ((IND(ITERM,IVAR),IVAR = 1,2),ITERM = 1,6)/
     *     0, 0,
     *     1, 0,
     *     1, 0,
     *     0, 1,
     *     0, 1,
     *     0, 0/
      data YDATA /
     *     1.700641e0, 1.793512e0, 1.838309e0, 1.838416e0, 1.792204e0,
     *     1.700501e0, 1.579804e0, 1.426268e0, 1.260724e0, 1.084901e0,
     *     0.917094e0, 0.761920e0, 0.627304e0, 0.522146e0, 0.446645e0,
     *     0.404920e0, 0.392033e0, 0.409622e0, 0.453045e0, 0.510765e0,
     *     0.584554e0, 0.663109e0, 0.747613e0, 0.829439e0, 0.908496e0,
     *     0.983178e0, 1.051046e0, 1.114072e0, 1.171746e0, 1.227823e0/
c     ------------------------------------------------------------------
      NDATA = MDATA
      NA = MA
      NB = MB
      ALF(1) =    5.0e0
      ALF(2) =   10.0e0
      IV(1) = 0
 
      print'(1x,a)',
     * 'Program DRSNLSFU.. Demo driver for SNLSFU.',
     * '   A variant of NL2SOL.',
     * '   SNLSFU handles the Separable problem.',
     * '   SNLSFU requires function values but not the Jacobian.',
     * ' ',
     * 'Sample problem is a nonlinear curve fit to data.',
     * 'Model function is B1 + B2 * cos(A1*t) + B3 * sin(A1*t) +',
     * '                       B4 * cos(A2*t) + B5 * sin(A2*t) + Noise',
     * 'Data generated using',
     * '(A1, A2, B1, ..., B5) = (6, 9, 1, 0.5, 0.4, 0.2, 0.1)',
     * 'and Gaussian noise with mean 0 and',
     * 'sample standard deviation 0.001',
     * ' '
 
      call SIVSET(1, IV, LIV, LV, V)
      IV( X0PRT) = 1
      IV(OUTLEV) = 0
      IV(STATPR) = 1
      IV(SOLPRT) = 1
      IV(COVPRT) = 1
 
      call SNLSFU(NDATA, NA, NB, ALF, BET, YDATA, SCALCA,
     *         IND, NB+1, IV, LIV, LV, V)
 
      DOF = max(NDATA - NA - NB, 1)
      print'(1x/1x,a,g12.4)',
     *    'SIGFAC: sqrt((2 * V(F))/DOF) =',
     *             sqrt(2.0e0 * V(F)/DOF)
      stop
      end
c     ==================================================================
      subroutine SCALCA(NDATA, NA, NB, ALF, NCOUNT, PHI)
c     Test case for separable nonlinear least squares computation.
c     Computes NDATA x NB matrix PHI as a function of the
c     nonlinear parameters ALF().
c     For J .le. NB the (I,J) term of PHI is the coefficient of the
c     linear coefficient B(J) in row I of the model.
c     In this example the model does not have a term that is not
c     multiplied by a linear coefficient.  If such a term is present
c     then PHI must have an (NB+1)st column to hold this term.
c     ------------------------------------------------------------------
      integer I, NA, NB, NCOUNT, NDATA
      real             ALF(NA)
      real             DEL, PHI(NDATA,NB), T
c     ------------------------------------------------------------------
      T = 0.0E0
      DEL = 1.0E0 / 29.0E0
      do 10 I = 1,NDATA
         PHI(I,1) = 1.0E0
         PHI(I,2) = cos(ALF(1)*T)
         PHI(I,3) = sin(ALF(1)*T)
         PHI(I,4) = cos(ALF(2)*T)
         PHI(I,5) = sin(ALF(2)*T)
         T = T + DEL
   10 continue
      return
      end
