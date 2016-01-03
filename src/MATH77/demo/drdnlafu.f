c     program DRDNLAFU
c>> 1994-11-02 DRDNLAFU Krogh  Changes to use M77CON
c>> 1994-09-14 DRDNLAFU CLL Set IV(OUTLEV) = 0 for comparing output.
c>> 1992-02-03 CLL @ JPL
c>> 1990-07-02 CLL @ JPL
c>> 1990-06-27 CLL @ JPL
c>> 1990-06-14 CLL @ JPL
c>> 1990-03-28 CLL @ JPL
c     Demo driver for DNLAFU. A variant of the nonlinear LS code NL2SOL.
c     DNLAFU requires function values only.
c     ------------------------------------------------------------------
c--D replaces "?": DR?NLAFU, ?NLAFU, ?CALCR, ?IVSET
c     ------------------------------------------------------------------
      external DCALCR
      integer  LIV, LV, MC, MDATA, NC, NDATA
      parameter(MDATA = 30, MC = 7)
      parameter(LIV = 82 + MC)
      parameter(LV = 105 + MC*(MDATA + 2*MC + 17) + 2*MDATA)
      integer IV(LIV)
      integer F, COVPRT, OUTLEV, SOLPRT, STATPR, X0PRT
      parameter(F=10)
      parameter(COVPRT=14, OUTLEV=19, SOLPRT=22, STATPR=23, X0PRT=24)
      double precision COEF(MC), DOF, V(LV)
c     ------------------------------------------------------------------
      NDATA = MDATA
      NC = MC
      COEF(1) =    5.0d0
      COEF(2) =   10.0d0
      COEF(3) =    0.5d0
      COEF(4) =    0.5d0
      COEF(5) =    0.5d0
      COEF(6) =    0.5d0
      COEF(7) =    0.5d0
      IV(1) = 0

      print'(1x,a)',
     * 'Program DRDNLAFU.. Demo driver for DNLAFU.',
     * '   A variant of NL2SOL.',
     * '   DNLAFU requires function values but not the Jacobian.',
     * ' ',
     * 'Sample problem is a nonlinear curve fit to data.',
     * 'Model function is C3 + C4 * cos(C1*t) + C5 * sin(C1*t) +',
     * '                       C6 * cos(C2*t) + C7 * sin(C2*t) + Noise',
     * 'Data generated using',
     * '(C1, ..., C7) = (6, 9, 1, 0.5, 0.4, 0.2, 0.1)',
     * 'and Gaussian noise with mean 0 and',
     * 'sample standard deviation 0.001',
     * ' '

      call DIVSET(1, IV, LIV, LV, V)
      IV( X0PRT) = 1
      IV(OUTLEV) = 0
      IV(STATPR) = 1
      IV(SOLPRT) = 1
      IV(COVPRT) = 1

      call DNLAFU(NDATA, NC, COEF, DCALCR, IV, LIV, LV, V)

      DOF = max(NDATA - NC, 1)
      print'(1x/1x,a,g12.4)',
     *    'SIGFAC: sqrt((2 * V(F))/DOF) =',
     *             sqrt(2.0d0 * V(F)/DOF)
      stop
      end
c     ==================================================================
      subroutine DCALCR(NDATA, NC, C, NCOUNT, RVEC)
c     Function evaluation to test nonlinear least squares computation.
c     ------------------------------------------------------------------
      integer I, MDATA, NCOUNT, NDATA, NC
      parameter(MDATA = 30)
      double precision C(NC), DEL, RVEC(NDATA), T, YDATA(MDATA)
      data YDATA /
     *     1.700641d0, 1.793512d0, 1.838309d0, 1.838416d0, 1.792204d0,
     *     1.700501d0, 1.579804d0, 1.426268d0, 1.260724d0, 1.084901d0,
     *     0.917094d0, 0.761920d0, 0.627304d0, 0.522146d0, 0.446645d0,
     *     0.404920d0, 0.392033d0, 0.409622d0, 0.453045d0, 0.510765d0,
     *     0.584554d0, 0.663109d0, 0.747613d0, 0.829439d0, 0.908496d0,
     *     0.983178d0, 1.051046d0, 1.114072d0, 1.171746d0, 1.227823d0/
c     ------------------------------------------------------------------
      T = 0.0D0
      DEL = 1.0D0 / 29.0D0
      do 10 I = 1,NDATA
         RVEC(I) = C(3) + C(4)*cos(C(1)*T) + C(5)*sin(C(1)*T) +
     *                    C(6)*cos(C(2)*T) + C(7)*sin(C(2)*T) -
     *                    YDATA(I)
         T = T + DEL
   10 continue
      return
      end
