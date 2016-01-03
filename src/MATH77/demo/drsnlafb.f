c     program DRSNLAFB
c>> 2001-05-24 DRSNLAFB Krogh Minor change for making .f90 version.
c>> 1994-11-02 DRSNLAFB Krogh  Changes to use M77CON
c>> 1994-09-14 DRSNLAFB CLL Set IV(OUTLEV) = 0 for comparing output.
c>> 1992-02-03 CLL @ JPL
c>> 1990-07-02 CLL @ JPL
c>> 1990-06-27 CLL @ JPL
c>> 1990-06-14 CLL @ JPL
c>> 1990-04-05 CLL @ JPL
c>> 1990-03-28 CLL @ JPL
c     Demo driver for SNLAFB. A variant of the nonlinear LS code NL2SOL.
c     SNLAFB handles bounded variables and requires function values
c     only.
c     ------------------------------------------------------------------
c--S replaces "?": DR?NLAFB, ?NLAFB, ?CALCR, ?IVSET
c     ------------------------------------------------------------------
      external SCALCR
      integer  J, LIV, LV, MC, MDATA, NC, NDATA
      parameter(MDATA = 30, MC = 7)
      parameter(LIV = 82 + 4*MC)
      parameter(LV =  105 + MC*(MDATA + 2*MC + 21) + 2*MDATA)
      integer IV(LIV)
      integer F, OUTLEV, SOLPRT, STATPR, X0PRT
      parameter(F=10, OUTLEV=19, SOLPRT=22, STATPR=23, X0PRT=24)
      real             BND(2,MC), COEF(MC), DOF, V(LV)
      data (BND(1,J),J=1,7) /  2*5.0e0,  5*0.0E0 /
      data (BND(2,J),J=1,7) / 2*10.0e0,  5*1.0e0 /
c     ------------------------------------------------------------------
      NDATA = MDATA
      NC = MC
      COEF(1) =    5.0e0
      COEF(2) =   10.0e0
      COEF(3) =    0.5e0
      COEF(4) =    0.5e0
      COEF(5) =    0.5e0
      COEF(6) =    0.5e0
      COEF(7) =    0.5e0
 
      print'(1x,a)',
     * 'Program DRSNLAFB.. Demo driver for SNLAFB.',
     * '   A variant of NL2SOL.',
     * '   SNLAFB requires function values but not the Jacobian.',
     * '   SNLAFB handles bounds on variables.',
     * ' ',
     * 'Sample problem is a nonlinear curve fit to data.',
     * 'Model function is C3 + C4 * cos(C1*t) + C5 * sin(C1*t) +',
     * '                       C6 * cos(C2*t) + C7 * sin(C2*t) + Noise',
     * 'Data generated using',
     * '(C1, ..., C7) = (6, 9, 1, 0.5, 0.4, 0.2, 0.1)',
     * 'and Gaussian noise with mean 0 and',
     * 'sample standard deviation 0.001',
     * 'Setting lower bounds = ( 5, 5,0,0,0,0,0).',
     * 'Setting upper bounds = (10,10,1,1,1,1,1).',
     * ' '
 
      call SIVSET(1, IV, LIV, LV, V)
      IV( X0PRT) = 1
      IV(OUTLEV) = 0
      IV(STATPR) = 1
      IV(SOLPRT) = 1
 
      call SNLAFB(NDATA, NC, COEF, BND, SCALCR, IV, LIV, LV, V)
 
      DOF = max(NDATA - NC, 1)
      print'(1x/1x,a,g12.4)',
     *    'SIGFAC: sqrt((2 * V(F))/DOF) =',
     *             sqrt(2.0e0 * V(F)/DOF)
      stop
      end
c     ==================================================================
      subroutine SCALCR(NDATA, NC, C, NCOUNT, RVEC)
c     Function evaluation to test nonlinear least squares computation.
c     ------------------------------------------------------------------
      integer I, MDATA, NCOUNT, NDATA, NC
      parameter(MDATA = 30)
      real             C(NC), DEL, RVEC(NDATA), T, YDATA(MDATA)
      data YDATA /
     *     1.700641e0, 1.793512e0, 1.838309e0, 1.838416e0, 1.792204e0,
     *     1.700501e0, 1.579804e0, 1.426268e0, 1.260724e0, 1.084901e0,
     *     0.917094e0, 0.761920e0, 0.627304e0, 0.522146e0, 0.446645e0,
     *     0.404920e0, 0.392033e0, 0.409622e0, 0.453045e0, 0.510765e0,
     *     0.584554e0, 0.663109e0, 0.747613e0, 0.829439e0, 0.908496e0,
     *     0.983178e0, 1.051046e0, 1.114072e0, 1.171746e0, 1.227823e0/
c     ------------------------------------------------------------------
      T = 0.0E0
      DEL = 1.0E0 / 29.0E0
      do 10 I = 1,NDATA
         RVEC(I) = C(3) + C(4)*cos(C(1)*T) + C(5)*sin(C(1)*T) +
     *                    C(6)*cos(C(2)*T) + C(7)*sin(C(2)*T) -
     *                    YDATA(I)
         T = T + DEL
   10 continue
      return
      end
