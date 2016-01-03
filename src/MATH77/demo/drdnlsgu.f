c     program DRDNLSGU
c>> 2001-05-24 DRDNLSGU Krogh Minor change for making .f90 version.
c>> 1997-06-18 DRDNLSGU Krogh  Changes to improve C portability.
c>> 1994-11-02 DRDNLSGU Krogh  Changes to use M77CON
c>> 1994-09-14 DRDNLSGU CLL Set IV(OUTLEV) = 0 for comparing output.
c>> 1992-04-13 CLL Rename and reorder common block [D/S]KEY.
c>> 1992-02-03 CLL @ JPL
c>> 1990-07-02 CLL @ JPL
c>> 1990-06-27 CLL @ JPL
c>> 1990-04-05 CLL @ JPL
c>> 1990-03-29 CLL @ JPL
c     Demo driver for DNLSGU. A variant of the nonlinear LS code NL2SOL.
c     DNLSGU solves the "separable" problem.
c     DNLSGU requires values of the function and the Jacobian matrix.
c     Note:  MDER is the number of ones in the array IND().
c     ------------------------------------------------------------------
c--D replaces "?": DR?NLSGU, ?NLSGU, ?CALCA, ?CALCB, ?IVSET, ?KEY
c     ------------------------------------------------------------------
      external DCALCA, DCALCB
      integer  ITERM,IVAR,LIV,LV,MA,MB,MDATA,MDER,MLEN,NA,NB,NDATA
      parameter(MDATA = 30, MA = 2, MB = 5)
      parameter(MDER = 4)
      parameter(LIV = 115 + MA + MB + 2*MDER)
      parameter(MLEN = (MB+MA)*(MDATA+MB+MA+1))
      parameter(LV = 105 + MDATA*(MB+MDER+3) +
     *               MLEN + (MB*(MB+3))/2 + MA*(2*MA+17))
      integer IND(MB+1,MA), IV(LIV)
      integer F, COVPRT, OUTLEV, SOLPRT, STATPR, X0PRT
      parameter(F=10)
      parameter(COVPRT=14, OUTLEV=19, SOLPRT=22, STATPR=23, X0PRT=24)
      double precision ALF(MA), BET(MB), DOF, V(LV), YDATA(MDATA)
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
     * 'Program DRDNLSGU.. Demo driver for DNLSGU.',
     * '   A variant of NL2SOL.',
     * '   DNLSGU handles the Separable problem.',
     * '   DNLSGU requires values of the function and the Jacobian.',
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
      IV(COVPRT) = 1

      call DNLSGU(NDATA, NA, NB, ALF, BET, YDATA, DCALCA, DCALCB,
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
c     This code Illustrates saving results in common between DCALCA and
c     DCALCB to avoid recalculation of common subexpressions.
c     ------------------------------------------------------------------
      common/DKEY/C1, C2, S1, S2, KEY
      save  /DKEY/
      integer I, KEY, MDATA, NA, NB, NCOUNT, NDATA
      parameter(MDATA = 30)
      double precision ALF(NA), C1(MDATA), C2(MDATA)
      double precision DEL, PHI(NDATA,NB)
      double precision S1(MDATA), S2(MDATA), T
c     ------------------------------------------------------------------
      T = 0.0D0
      DEL = 1.0D0 / 29.0D0
      KEY = NCOUNT
      do 10 I = 1,NDATA
         C1(I) = cos(ALF(1)*T)
         S1(I) = sin(ALF(1)*T)
         C2(I) = cos(ALF(2)*T)
         S2(I) = sin(ALF(2)*T)
         PHI(I,1) = 1.0D0
         PHI(I,2) = C1(I)
         PHI(I,3) = S1(I)
         PHI(I,4) = C2(I)
         PHI(I,5) = S2(I)
         T = T + DEL
   10 continue
      return
      end
c     ==================================================================
      subroutine DCALCB(NDATA, NA, NB, ALF, NCOUNT, DER)
c     Test case for separable nonlinear least squares computation.
c     Computes the NDATA x NDER matrix DER.  Here NDER is the number of
c     ones in the indicator array IND().  The columns of DER correspond
c     to nonzero entries of IND() traversed columnwise.
c     In this example NDER is 4 and the correspondence is as follows:
c         Col of DER:         1       2       3       4
c         Element of IND(): (2,1)   (3,1)   (4,2)   (5,2)
c     In this example Row I of DER will be set to contain the values
c     of the four partial derivatives:
c         Partial of PHI(I,2) with respect to ALP(1)
c         Partial of PHI(I,3) with respect to ALP(1)
c         Partial of PHI(I,4) with respect to ALP(2)
c         Partial of PHI(I,5) with respect to ALP(2)
c     ------------------------------------------------------------------
      common/DKEY/C1, C2, S1, S2, KEY
      save  /DKEY/
      integer I, KEY, MDATA, NCOUNT, NDATA, NA, NB, NDER
      parameter(MDATA = 30, NDER = 4)
      double precision ALF(NA), C1(MDATA), C2(MDATA)
      double precision DEL, DER(NDATA,NDER), S1(MDATA), S2(MDATA), T
c     ------------------------------------------------------------------
      T = 0.0D0
      DEL = 1.0D0 / 29.0D0
      if(NCOUNT .eq. KEY) then
         do 10 I = 1,NDATA
            DER(I,1) = -S1(I)*T
            DER(I,2) =  C1(I)*T
            DER(I,3) = -S2(I)*T
            DER(I,4) =  C2(I)*T
            T = T + DEL
   10    continue
      else
         do 20 I = 1,NDATA
            DER(I,1) = -sin(ALF(1)*T)*T
            DER(I,2) =  cos(ALF(1)*T)*T
            DER(I,3) = -sin(ALF(2)*T)*T
            DER(I,4) =  cos(ALF(2)*T)*T
            T = T + DEL
   20    continue
      endif
      return
      end
