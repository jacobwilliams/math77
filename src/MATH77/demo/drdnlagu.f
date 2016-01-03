c     program DRDNLAGU
c>> 1997-06-18 DRDNLAGU Krogh  Changes to improve C portability.
c>> 1994-11-02 DRDNLAGU Krogh  Changes to use M77CON
c>> 1994-09-14 DRDNLAGU CLL Set IV(OUTLEV) = 0 for comparing output.
c>> 1992-04-13 CLL Rename and reorder common block [D/S]KEY.
c>> 1992-02-03 CLL @ JPL
c>> 1990-07-02 CLL @ JPL
c>> 1990-06-27 CLL @ JPL
c>> 1990-06-14 CLL @ JPL
c>> 1990-03-28 CLL @ JPL
c     Demo driver for DNLAGU. A variant of the nonlinear LS code NL2SOL.
c     DNLAGU requires values of the function and the Jacobian matrix.
c     ------------------------------------------------------------------
c--D replaces "?": DR?NLAGU, ?NLAGU, ?CALCR, ?CALCJ, ?IVSET, ?KEY
c     ------------------------------------------------------------------
      external DCALCR, DCALCJ
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
     * 'Program DRDNLAGU.. Demo driver for DNLAGU.',
     * '   A variant of NL2SOL.',
     * '   DNLAGU requires values of the function and the Jacobian.',
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

      call DNLAGU(NDATA, NC, COEF, DCALCR, DCALCJ, IV, LIV, LV, V)

      DOF = max(NDATA - NC, 1)
      print'(1x/1x,a,g12.4)',
     *    'SIGFAC: sqrt((2 * V(F))/DOF) =',
     *             sqrt(2.0d0 * V(F)/DOF)
      stop
      end
c     ==================================================================
      subroutine DCALCR(NDATA, NC, C, NCOUNT, RVEC)
c     Function evaluation to test nonlinear least squares computation.
c     Illustrates saving results in common between DCALCR and DCALCJ
c     to avoid recalculation of common subexpressions.
c     ------------------------------------------------------------------
      common/DKEY/C1, C2, S1, S2, KEY
      save  /DKEY/
      integer I, KEY, MDATA, NCOUNT, NDATA, NC
      parameter(MDATA = 30)
      double precision C(NC), C1(MDATA), C2(MDATA), DEL, RVEC(NDATA)
      double precision S1(MDATA), S2(MDATA), T, YDATA(MDATA)
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
      KEY = NCOUNT
      do 10 I = 1,NDATA
         C1(I) = cos(C(1)*T)
         S1(I) = sin(C(1)*T)
         C2(I) = cos(C(2)*T)
         S2(I) = sin(C(2)*T)
         RVEC(I) = C(3) + C(4)*C1(I) + C(5)*S1(I) +
     *                    C(6)*C2(I) + C(7)*S2(I) -
     *                    YDATA(I)
         T = T + DEL
   10 continue
      return
      end
c     ==================================================================
      subroutine DCALCJ(NDATA, NC, C, NCOUNT, AJAC)
c     Jacobian evaluation to test nonlinear least squares computation.
c     ------------------------------------------------------------------
      common/DKEY/C1, C2, S1, S2, KEY
      save  /DKEY/
      integer I, KEY, MDATA, NCOUNT, NDATA, NC
      parameter(MDATA = 30)
      double precision AJAC(NDATA,NC), C(NC), C1(MDATA), C2(MDATA)
      double precision DEL, S1(MDATA), S2(MDATA), T
c     ------------------------------------------------------------------
      T = 0.0D0
      DEL = 1.0D0 / 29.0D0
      if(NCOUNT .eq. KEY) then
         do 10 I = 1,NDATA
            AJAC(I,1) = -C(4)*S1(I)*T + C(5)*C1(I)*T
            AJAC(I,2) = -C(6)*S2(I)*T + C(7)*C2(I)*T
            AJAC(I,3) = 1.0D0
            AJAC(I,4) = C1(I)
            AJAC(I,5) = S1(I)
            AJAC(I,6) = C2(I)
            AJAC(I,7) = S2(I)
            T = T + DEL
   10    continue
      else
         do 20 I = 1,NDATA
            AJAC(I,3) = 1.0D0
            AJAC(I,4) = cos(C(1)*T)
            AJAC(I,5) = sin(C(1)*T)
            AJAC(I,6) = cos(C(2)*T)
            AJAC(I,7) = sin(C(2)*T)
            AJAC(I,1) = -C(4)*AJAC(I,5)*T + C(5)*AJAC(I,4)*T
            AJAC(I,2) = -C(6)*AJAC(I,7)*T + C(7)*AJAC(I,6)*T
            T = T + DEL
   20    continue
      endif
      return
      end
