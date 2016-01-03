      subroutine DBESYN(X, ALPHA, NUM, BY)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2009-10-28 DBESYN Krogh/Snyder Moved BGAMSQ = BGAM**2 inside else.
C>> 2000-01-05 DBESYN Krogh Changed c$ comments to c.  (Cray f90 prob.)
C>> 1995-11-13 DBESYN Krogh Converted from SFTRAN to Fortran
C>> 1994-10-19 DBESYN Krogh Changes to use M77CON
C>> 1994-04-19 DBESYN CLL  Edited to make DP & SP files similar.
C>> 1991-01-14 DBESYN CLL  Removed duplicate data statement.
C>> 1989-08-09 DBESYN CLL  More accurate constants for Cray
C>> 1986-03-18 DBESYN Lawson  Initial code.
c--D replaces "?": ?BESYN, ?BESPQ, ?ERV1, ?GAMMA, ?LGAMA
C
c     This subr computes the Y Bessel functions of X for
c     NUM orders from ALPHA through ALPHA + NUM - 1.
c     The results will be stored in BY(I), I = 1,...,NUM.
C
c     Require X .gt. 0.,    ALPHA .ge. 0.,    NUM .ge. 1.
C
c          The original subroutines SBYNU and BESJ/BESY were
c     designed and programmed by E. W. Ng and W. V. Snyder, JPL,
C     in 1973.  Modified by Ng and S. Singletary, JPL, 1974.
c
c     1984 April 16, JPL, C. Lawson and S. Chan.  Original subrs
c     combined into single subroutine.  Converted to SFTRAN3
c     and Fortran 77 for the JPL MATH77 library.
C
c     ------------------------------------------------------------------
c
c           As the machine precision, EPS, increases so does XPQ,
c     and thus so does the requirement for the storage dimension,
c     LDIMA.  Here are some values of -LOG10(EPS), XPQ, and LDIMA.
c
c     -LOG10(EPS)=  5     10     15     20     25     30
c            XPQ =  5.74  11.38  17.03  22.68  28.32  33.97
c          LDIMA = 17     33     49     63     79     95
c
c           Since LDIMA cannot be adjusted at run-time, we are
c     setting it to 95 to handle the case of CDC double precision
c     which is probably the largest precision system likely
c     to be encountered.  Note that the relative precision of results
c     from this subr cannot exceed about 16 or 17 decimal places
c     because of the limited accuracy of the polynomial coeffs used to
c     compute G0, and also the limited precision of the
c     subprograms referenced for gamma and loggamma.
c     ------------------------------------------------------------------
c     Subprograms used: TANH, LOG10, LOG, EXP, COS, SIN, SQRT
      external          D1MACH, DBESPQ, DERV1, ERMSG, ERMOR, IERV1
      external          DGAMMA, DLGAMA
c     ----------
      integer I, II, K, LDIMA, M, MU, MUSAVE, ND2, NMAX, NMIN, NUM
      parameter( LDIMA = 95, ND2 = 10)
      double precision D1MACH, DGAMMA, DLGAMA
      double precision AJ(LDIMA), ALPHA, ARG
      double precision BGAM, BGAMSQ, BIG, BIGLOG, BY(NUM)
      double precision C11293, C16, C1P5, C2BYPI, C59, CHI, CP6, CUTLOW
      double precision D1, D2, D2SER(0:ND2), D2VAL, D3, DR
      double precision EC, EM, EM1, EMU, EN1, EN2, EN3, EPS, ETA
      double precision FAC, FK, FKM1, FKP1, FOUR, FV, FVM1, FVP1
      double precision G, G0, G1, GMAX, GNU, HALF, HALFPI, HICUT
      double precision LOGPI, LOGTWO, ONE, P, PI, PIV2, PSI1, PSIZ
      double precision Q, Q2DXPV, SCALE, SMALL, SUM
      double precision TEMP, TEST, THREE, THSJ, THVDX, TWO, TWODX
      double precision V, V2, VPMU, X, XLOG, XPQ
      double precision YLOG, YV, YVM1, YVP1, Z, ZERO
      logical FLAG, J1SMAL
      save EPS, HICUT, SMALL, XPQ, BIG, BIGLOG
c
      parameter( ZERO = 0.D0, ONE = 1.D0, TWO = 2.D0)
      parameter( HALF = 0.5D0, THREE = 3.0D0)
      parameter( C16 = 16.D0, FOUR = 4.0D0)
      parameter(PI = 3.14159265358979323846264338327950288D0)
C
      parameter(HALFPI = 1.57079632679489661923132169163975144D0)
c          LOGPI = ln(pi)
      parameter(LOGPI = 1.14472988584940017414342735135305869D0)
c          C2BYPI = 2/pi
      parameter(C2BYPI = 0.63661977236758134307553505349005744D0)
c          EC = Euler's constant.
      parameter(EC = 0.57721566490153286060651209008240243D0)
c          LOGTWO = Ln(2)
      parameter(LOGTWO = 0.69314718055994530941723212145817657D0)
      parameter(   C1P5 = 1.5D0 )
      parameter( C11293 = 1.1293D0)
      parameter(    CP6 = 0.60206D0)
      parameter(    C59 = 0.59D0)
      parameter( CUTLOW = 0.012D0)
      parameter(   THSJ = 0.12D0)
      data EPS / ZERO /
C
      data D2SER / +.3674669051966159615185D+00,
     *   -.1782903980807269842231D+01,
     *   +.9411644685122855908427D+00,
     *   -.1958865250248747878077D+01,
     *   +.1557306621108283294475D+01,
     *   -.2521051413546812096437D+01,
     *   +.2184502685635110914145D+01,
     *   -.3140067153452674402872D+01,
     *   +.2817401038921461361158D+01,
     *   -.3772198775799670081858D+01,
     *   +.3452780604492584575060D+01/
C
c     ------------------------------------------------------------------
C
c     Set environmental parameters.
c
      if ( EPS .eq. ZERO) then
         EPS = D1MACH(3)
         HICUT = ONE / (EPS*C16)
         SMALL = C16 * D1MACH(1) / EPS
         XPQ = C11293 * (CP6 - LOG10( EPS )) - C59
         BIG = D1MACH(2) / TWO
         BIGLOG = LOG(BIG)
      end if
c     ------------------------------------------------------------------
c
c     Compute V, NMIN, and NMAX.
c
      NMIN = INT(ALPHA)
      V = ALPHA - DBLE(NMIN)
      NMAX = NMIN + NUM -1
c     ------------------------------------------------------------------
c
c     Test validity of given argument values.
C
      if ( X .lt. ZERO  .or.
     *   ALPHA .lt. ZERO .or. NUM .lt. 1) then
C                                                Error 1.
         call ERMSG('DBESYN',1,0,
     *             'Require X .gt. 0, ALPHA .ge. 0, NUM .ge. 1',',')
         go to 400
c
c     ------------------------------------------------------------------
C
c     Branch on size of X.
c
      else if (X .eq. ZERO ) then
c                                                Error 6.
         do 20 I = 1, NUM
            BY(I) = -BIG
   20    continue
         call ERMSG('DBESYN',6,0,
     *   'When X = 0., function value is -INFINITY.', ',')
         go to 400
      else if ( X .lt. EPS ) then
c
c ********************* Code for very small X case. ********************
c
c    Use a single term expression for Y, valid for X very close to zero.
c    Ref NBS AMS 55 Eqs 9.1.8 & 9.1.9.
c     For GNU = 0,   Y = (2/pi) * (EC + Ln(X/2)),  {EC = Euler's const.}
c     For GNU .gt. 0    Y =  -(1/pi) * Gamma(GNU) * (X/2)**(-GNU)
c
         XLOG = log( X )
         GNU = ALPHA
c
         do 140 I = 1, NUM
            if ( GNU .eq. ZERO ) then
               BY(I) = C2BYPI * (EC + XLOG - LOGTWO)
            else
               YLOG = DLGAMA(GNU) - GNU * (XLOG-LOGTWO) - LOGPI
               if (YLOG .lt. BIGLOG) then
                  BY(I) = -EXP(YLOG)
               else
c                                               Error 5.
                  do 120 II = I,NUM
                     BY(II) = -BIG
  120             continue
                  call ERMSG('DBESYN',5,0,
     *              'Results exceed overflow limit from BY(I) on.', ',')
                  call IERV1('I', I, ',')
                  go to 400
               end if
            end if
            GNU = GNU + ONE
  140    continue
         return
      else
         TWODX = TWO / X
         if ( X .le. XPQ ) then
c
c ********************* Code for the middle X case. ********************
c
C
C     J-TYPE BESSEL FUNCTIONS FOLLOW THE RECURRENCE RELATION
C     F(V-1,X)=(2*V/X)*F(V,X)-F(V+1,X).
C
            MU = INT(X) + 1
            DR = TWODX * (V+DBLE(MU))
            FKP1 = ONE
            FK =  ZERO
C
C     RECUR FORWARD UNTIL FKP1 IS GREATER THAN PRECISION OF ARITHMETIC.
C
  210       if (EPS * ABS(FKP1) .LE. ONE) then
               MU = MU + 1
               DR = DR + TWODX
               FKM1 = FK
               FK = FKP1
               FKP1 = DR * FK - FKM1
               go to 210
            end if
C
C     WE ARE NOW ASSURED THAT BACKWARD RECURRENCE FROM MU WILL YIELD
C     ACCURATE RESULTS.
C
C                                        GUARANTEE EVEN MU
            if (MOD(MU,2) .NE. 0)  MU = MU + 1
            MUSAVE = MU
c
c                                              Test for Error 3.
c     This error should never happen.  Large MU would be due to
c     large X.  But X is not larger than XPQ here.
c     See explanation at the beginning of this subroutine
c     of the relation of XPQ and LDIMA to the machine EPS.
c
            if ( MU + 1 .gt. LDIMA) then
               call ERMSG('DBESYN', 3, 0,
     *         'Need larger dimension, LDIMA, to process given X.', ',')
               call ERMOR('Require LDIMA .ge. MU + 1', ',')
               call IERV1('MU', MU, ',')
               call IERV1('LDIMA', LDIMA, ',')
               go to 400
            end if
c
            FVM1 = SMALL
            AJ(MU+1) = FVM1
            FV = ZERO
            ETA = ONE
            SUM = FVM1
            M = MU / 2
            EM = DBLE(M)
            EMU = DBLE(MU)
            FAC = (V + EMU) * TWODX
C
c     Set TEST = largest value that can be multiplied by
c     FAC without risking overflow.  The present value of
c     FAC is the largest that will occur during the recursion.
c     TEST will be used to protect against overflow during
c     the recursion.
c
            TEST = BIG / MAX(ONE, FAC)
C
C                            Loop while MU .gt. ZERO
C
  230       continue
               FVP1 = FV
               FV = FVM1
               if ( ABS(FV) .gt. TEST ) then
c                                        Rescale
                  FV = FV / SUM
                  FVP1 = FVP1 / SUM
                  do 240 II = MU+1, MUSAVE
                     AJ(II) = AJ(II) / SUM
  240             continue
                  SUM = ONE
               end if
               FVM1 = FAC * FV - FVP1
               MU = MU -1
               EMU = EMU - ONE
               FAC = (V + EMU) * TWODX
               AJ(MU+1) = FVM1
               if (MOD(MU,2) .eq. 0) then
                 if (V .eq. ZERO)  then
                   SUM = SUM + FVM1
                   if (MU .eq. 0) then
                     SCALE = ONE / SUM
                     go to 260
                   end if
                   SUM = SUM + FVM1
                 else
                   if (MU .NE. 0) then
                     VPMU = V + EMU
                     ETA = ETA * (EM/(V+(EM-ONE)))*(VPMU/(VPMU+TWO))
                     SUM = SUM + FVM1 * ETA
                     EM = EM - ONE
                   else
c
c           Here MU = 0 and EM = 0NE.  Thus the expression for
c           updating ETA reduces to the following simpler
c           expression.
c
                     ETA = ETA / (V + TWO)
                     SUM = SUM + FVM1 * ETA
                     BGAM = DGAMMA(V+ONE)
                     Q2DXPV = TWODX ** V
                     SCALE = ( BGAM / ETA ) * SUM * Q2DXPV
                     SCALE = ONE / SCALE
                     go to 260
                   end if
                 end if
               end if
               go to 230
  260       continue
C
C     NORMALIZE AJ() TO GET VALUES OF J-BESSEL FUNCTION.
C
            do 270 I = 1, MUSAVE+1
               AJ(I) = AJ(I) * SCALE
  270       continue
            MU = MUSAVE
c
c     Compute Y Bessel functions, making use of previously computed J
c     Bessel functions and other previously computed values, MU, BGAM,
c     Q2DXPV, TWODX, V.
c
c     Here V is in the range [0.,1.).  The quantities G0 and G1 depend
c     on X and V and are unbounded as V approaches 1.   Therefore we
c     make the the change of variables
c                V2 = V      if V .le. 0.5 and
c                V2 = 1 - V  if V .gt. 0.5
c     Then G0 and G1 are computed as functions of X and V2 with V2 in
c     the range (-0.5, 0.5].
C
c     Compute G0 and G1.
c
            V2 = V
            if ( V .eq. ZERO ) then
               Z = EC - log(TWODX)
               G0 =  Z / HALFPI
               G1 = TWO / HALFPI
               BGAMSQ = ONE
               Q2DXPV = ONE
            else
               BGAMSQ = BGAM**2
               if (V .gt. HALF) then
c
c           Use the transformed variable, V2 = V - 1.
c           Make corresponding transformation of Q2DXPV & BGAMSQ.
c
                  V2 = (V - HALF) - HALF
                  Q2DXPV = Q2DXPV / TWODX
                  BGAMSQ = BGAMSQ / V**2
               end if
               PIV2 = PI * V2
c
c           Here V2 is in [-.5, .5].  Now test against CUTLOW = 0.012
c
               if ( ABS(V2) .lt. CUTLOW ) then
C
c     Here we compute
c           G0 = (ONE / TAN(PIV2)) - Q2DXPV**2 * BGAMSQ / PIV2
c     by a formulation that retains accuracy for
c     V2 close to zero.
c     >     The no. of coeffs from D2SER() used to compute D2VAL
c     could be fewer on lower precision computers, however this
c     computation is only done about 2.4% of the time so the
c     potential time saving would probably not be noticeable.
c
c     This method was derived by C. Lawson and W. V. Snyder,
c     JPL, 1984 Apr 15.
c
c     First compute EM1 = (2/X)**(2*V2) - 1
c                       = exp(2 * V2 * log(2/X)) - 1
c
                  ARG = TWO * V2 * log( TWODX )
                  if ( ABS( ARG ) .lt. LOGTWO ) then
                     TEMP = TANH( HALF * ARG )
                     EM1 = TWO * TEMP / (ONE - TEMP)
                  else
                     EM1 = EXP( ARG ) - ONE
                  end if
c
c      Evaluate taylor series for
c      D2VAL = (PIV2 * cotan(PIV2) - BGAMSQ) / PIV2
c
                  D2VAL = D2SER(ND2)
                  do 280 I = ND2-1, 0, -1
                     D2VAL = D2SER(I) + V2 * D2VAL
  280             continue
c
                  G0 = D2VAL - BGAMSQ * (EM1 / PIV2)
                  G1 = (Q2DXPV**2/HALFPI) * BGAMSQ * (TWO+V2) / (ONE-V2)
               else
                  G0 = (ONE / TAN(PIV2)) - Q2DXPV**2 * BGAMSQ / PIV2
                  G1 = (Q2DXPV**2/HALFPI) * BGAMSQ * (TWO+V2) / (ONE-V2)
               end if
            end if
c ----------------------------------
c
C     COMPUTE YO FROM SUM(J'S) FORM
c
            EN3 = V2 + ONE
            EN2 = V2 + EN3
            EN1 = V2 + FOUR
            D1 = TWO
            D2 = D1 - V2
            D3 = D1 + V2
            FLAG = .FALSE.
c                                      THSJ = 0.12
            J1SMAL = ABS(AJ(1)) .lt. THSJ
            if ( J1SMAL .or. V2 .lt. ZERO) then
               FLAG = .TRUE.
C
C        Y(V2+1,X) MUST ALSO BE COMPUTED BY A SUM
C
               THVDX = THREE * V2 / X
               PSIZ = -BGAMSQ * Q2DXPV**2 / (HALFPI*X)
               PSI1 = G0 - HALF * G1
            end if
c
            if (V2 .ge. ZERO) then
               M = 3
               YV = G0 * AJ(1)
               if ( J1SMAL ) then
                  YVP1 = PSIZ * AJ(1) + PSI1 * AJ(2)
               end if
            else
               Z = TWODX * V * AJ(1)-AJ(2)
               YV = G0 * Z
               M = 2
               YVP1 = PSIZ * Z + PSI1 * AJ(1)
            end if
c
            do 290 I = M,MU,2
               YV = G1 * AJ(I) + YV
               G = G1
               G1 = -G1 * (EN1/D1) * (EN2/D2) * (EN3/D3)
               EN1 = EN1 + TWO
               EN2 = EN2 + ONE
               EN3 = EN3 + ONE
               D1 = D1 + ONE
               D2 = ONE + D2
               D3 = D3 + TWO
               if ( FLAG ) then
                  YVP1 = YVP1 + THVDX*G*AJ(I) + HALF*(G-G1)*AJ(I+1)
               end if
  290       continue
c
            if (V2 .lt. ZERO) then
               Z = YVP1
               YVP1 = V * Z * TWODX - YV
               YV = Z
            else if ( .NOT. J1SMAL ) then
C
C           NOW COMPUTE Y(V+1)
C           WRONSKIAN PROVIDED NOT NEAR A ZERO OF J
C
                  YVP1 = (YV*AJ(2)-ONE/(X*HALFPI)) / AJ(1)
            end if
            go to 350
         else if ( X .LE. HICUT ) then
c
c ********************* Code for the large X case. *********************
c
C
c     >     Here we have X .ge. XPQ, and V in [0.,1.).
c     The asymptotic series for
c     the auxiliary functions P and Q can be used.
c     From these we will compute Y(V,X) and Y(V+1,X) and
c     then recur forward.
c     Reference: NBS AMS 55 Eqs 9.2.5 & 9.2.6
c
            call DBESPQ (X,V,  P,Q)
            CHI = X - (V + HALF) * HALFPI
            YV = sqrt(ONE / (HALFPI*X)) * (P*SIN(CHI) + Q*COS(CHI))
C
            if ( NMAX .gt. 0 ) then
               call DBESPQ (X,V+ONE,   P,Q)
               CHI = X - (V + C1P5) * HALFPI
               YVP1 = sqrt(ONE / (HALFPI*X)) * (P*SIN(CHI) + Q*COS(CHI))
            end if
            go to 350
         else
c                                                     Error 2.
            call ERMSG('DBESYN', 2, 0,
     *         'Cannot obtain any accuracy when X exceeds HICUT.', ',')
            call DERV1('HICUT', HICUT, ',')
            go to 400
         end if
      end if
c
  350 continue
c                 Do forward recursion
c     Given YV = Y(V,X), YVP1 = Y(V+1,X), TWODX = 2/X, NMIN, NUM, NMAX =
c     NMIN + NUM -1, X, ALPHA, and BIG.  Recur forward and store
c     Y(NMIN+V) thru Y(NMAX+V) in BY(1) thru BY(NUM).
c
      if ( NMIN .eq. 0 ) then
         BY(1) = YV
         if ( NMAX .gt. 0 ) then
            BY(2) = YVP1
         end if
      else if ( NMIN .eq. 1 ) then
         BY(1) = YVP1
      end if
c
      if ( NMAX .gt. 1 ) then
         G = V * TWODX
         GMAX = G + TWODX * DBLE(NMAX-1)
         TEST = BIG / MAX(ONE, GMAX)
c
c        Note:  In the following statement, 3-NMIN can be nonpositive.
c
         do 370 K = 3-NMIN, NUM
            YVM1 = YV
            YV   = YVP1
            if (ABS(YV) .gt. TEST) then
c
c              The recursion has reached the overflow limit.
c              Set remaining elts of BY() to a large negative value
c              and issue error message.
c
               do 360 II = MAX(K, 1),NUM
                  BY(II) = -BIG
  360          continue
c                                                   Error 4.
               call ERMSG('DBESYN',4,0,
     *         'Results exceed overflow limit from BY(I) on.', ',')
               call IERV1('I', MAX(K,1), ',')
               go to 400
            end if
c
            G = G + TWODX
            YVP1 = G * YV - YVM1
            if ( K .ge. 1)  BY(K) = YVP1
  370    continue
      end if
      return
c                                     Error return
  400 continue
      call DERV1('X',X,',')
      call DERV1('ALPHA',ALPHA,',')
      call IERV1('NUM',NUM,'.')
      return
      end
