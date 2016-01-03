      subroutine SBESJN(X,ALPHA,NUM,BJ)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 SBESJN Krogh  Removed INT from type statement.
C>> 1995-11-13 SBESJN Krogh  Converted SFTRAN to Fortran
C>> 1995-10-24 SBESJN Krogh  Removed blanks in numbers for C conversion.
C>> 1994-10-19 SBESJN Krogh  Changes to use M77CON
C>> 1994-04-19 SBESJN CLL  Edited to make DP & SP files similar.
C>> 1992-03-13 SBESJN FTK  Removed implicit statements.
C>> 1989-08-09 SBESJN CLL  More accurate HALFPI for Cray
C>> 1986-03-18 SBESJN Lawson  Initial code.
c--S replaces "?": ?BESJN, ?GAMMA, ?BESPQ, ?ERV1
C
C     COMPUTE THE J-BESSEL FUNCTIONS OF X FOR ORDER ALPHA THROUGH
C     ALPHA + NUM - 1 in steps of one.
C     STORE THE RESULT INTO BJ(I),I=1,...,NUM.
C     Require X .ge. 0, ALPHA .ge. 0, NUM .ge. 1
C
c     Original code due to E. W. Ng and W. V. Snyder, JPL, 1973.
c     Modified by Ng and S. Singletary, 1974.
C     C.Lawson & S.Chan, JPL, 1984 Apr 05:
c        Changed calling sequence.
c        Adapted to SFTRAN3 and Fortran 77.
c        Added code to avoid overflow during the recurrsion.
c        Added code to use Taylor series for small x.
c        March 23. Improved accuracy of backward recursion.
c        Changed to use P and Q instead of R and THETA
c                in the region of large X.
C     ------------------------------------------------------------------
C
      save EPS, SMALL, XPQ, BIG, HICUT
c     ----------
      external           SGAMMA, SBESPQ, R1MACH, ERMSG, SERV1, IERV1
c     ----------
      integer I,I1,II,J,K,M,MU,NMAX,NMIN,NUM
      real             R1MACH, SGAMMA
      real             ALPHA,BESV,BESVM1,BESVP1,BIG,BJ(NUM),BJNU,BOT
      real             C11293,C16,C1P5,C59,CHI,CP6
      real             DR,EM,EMU,EPS,ETA,FAC,FAC2,FK,FKM1,FKP1,FV,FVM1
      real             FVP1,G,GNU,HALF,HALFPI,HALFX,HICUT
      real             ONE,P,Q,SCALE,SMALL,SUM,T1,T2,TENTH,TERM
      real             TERM1,TEST,TOP,TWO,TWODX,V,VPMU,X,XPQ,ZERO
      parameter(ZERO=0.E0, ONE=1.E0, TWO=2.E0, C16=16.E0)
      parameter( TENTH = 0.1E0, HALF = 0.5E0)
      parameter(HALFPI = 1.57079632679489661923132169163975144E0)
      parameter(C11293 = 1.1293E0, CP6 = 0.60206E0, C59 = 0.59E0)
      parameter(C1P5 = 1.5E0)
      data EPS / ZERO /
C     ------------------------------------------------------------------
C
c     Set environment parameters.  This should happen only
c     the first time this subroutine is called.
c
      if( EPS .EQ. ZERO ) then
         EPS = R1MACH(3)
         HICUT = ONE /(EPS*C16)
         XPQ = C11293 * (CP6 - log10(EPS)) - C59
         SMALL = C16 * R1MACH(1) / EPS
         BIG = R1MACH(2)/TWO
      end if
C
c     Test validity of input values.
c
      if (X .lt. ZERO .or. ALPHA .lt. ZERO .or. NUM .lt. 1) then
c
c                                                           Error 1.
        call ERMSG('SBESJN',1,0,
     *             'REQUIRE X.GE.0, ALPHA.GE.0, NUM.GE.1',',')
      else
c                            Begin computation.
         NMIN = int(ALPHA)
         V = ALPHA - real(NMIN)
         NMAX = NMIN + NUM - 1
C
         if( X .le. TENTH ) then
c
c ********************* Code for the small X case. *********************
c
            if( X .eq. ZERO ) then
c                                        Special for X .eq. 0.
               do 80 I = 1, NUM
                  BJ(I) =  ZERO
   80          continue
               if (ALPHA .eq. 0) BJ(1) = ONE
            else
c                    Here use Taylor series for small x.
C
               GNU = ALPHA
               HALFX = HALF*X
               FAC2 = -HALFX*HALFX
               TERM1 = (HALFX**GNU) / SGAMMA(GNU + ONE)
               do 150 I = 1, NUM
c           Sum the series for the Bessel fcn J sub GNU of X given
c           in Eq 9.1.10, page 360, of AMS 55.
c           1984 March 9, JPL, C. L. Lawson.
c
                  if(TERM1 .eq. ZERO) then
                     BJNU = ZERO
                  else
c
                     SUM = ZERO
                     TOP = FAC2
                     BOT = GNU + ONE
                     T1 = ONE
                     T2 = BOT
                     TERM = TOP/BOT
c
  100                if ( abs(TERM) .GT. EPS ) then
                        SUM = SUM + TERM
                        TOP = TOP * FAC2
                        T1 = T1 + ONE
                        T2 = T2 + ONE
                        BOT = BOT * T1 * T2
                        TERM = TOP/BOT
                        go to 100
                     end if
                     BJNU = TERM1 + TERM1 * SUM
                  end if
c
                  BJ(I) = BJNU
                  if( BJNU .eq. ZERO ) then
C
c     Here current result has underflowed to zero, so we will
c     set the rest of the results to zero also.
c
                     do 120 J = I+1, NUM
                        BJ(J) = ZERO
  120                continue
                     return
                  end if
                  GNU = GNU + ONE
                  TERM1 = TERM1 * (HALFX / GNU)
  150          continue
            end if
            return
         else if( X .lt. MAX(real(NMAX+1), XPQ) ) then
c
c ********************* Code for the middle X case. ********************
c
C
C     J-TYPE BESSEL FUNCTIONS FOLLOW THE RECURRENCE RELATION
C     F(V-1,X)=(2*V/X)*F(V,X)-F(V+1,X).
C
            TWODX = TWO / X
            MU = MAX( int(X)+1, NMAX)
            DR = TWODX * (V+real(MU))
            FKP1 = ONE
            FK =  ZERO
C
C     RECUR FORWARD UNTIL FKP1 IS GREATER THAN PRECISION OF ARITHMETIC.
C
  200       if ( EPS * abs(FKP1) .le. ONE ) then
               MU = MU + 1
               DR = DR + TWODX
               FKM1 = FK
               FK = FKP1
               FKP1 = DR * FK - FKM1
               go to 200
            end if
C
C     WE ARE NOW ASSURED THAT BACKWARD RECURRENCE FROM MU WILL YIELD
C     ACCURATE RESULTS.
C
C                                        GUARANTEE EVEN MU
            if (MOD(MU,2) .ne. 0)  MU = MU + 1
            FVM1 = SMALL
            FV = ZERO
            ETA = ONE
            SUM = FVM1
            M = MU / 2
            EM = real(M)
            EMU = real(MU)
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
C                            Loop while MU .GT. ZERO
  220       continue
               FVP1 = FV
               FV = FVM1
               if( abs(FV) .GT. TEST )then
                  FV = FV / SUM
                  FVP1 = FVP1 / SUM
                  I1 = MAX( 1, MU - NMIN + 1 )
                  do 230 II = I1, NUM
                     BJ(II) = BJ(II) / SUM
  230             continue
                  SUM = ONE
               end if
               FVM1 = FAC * FV - FVP1
               MU = MU -1
               EMU = EMU - ONE
               FAC = (V + EMU) * TWODX
               if (MU .ge. NMIN .AND. MU .le. NMAX) BJ(MU-NMIN+1) = FVM1
               if (MOD(MU,2) .eq. 0) then
                  if (V .eq. ZERO)  then
                     SUM = SUM + FVM1
                     if (MU .eq. 0) then
                        SCALE = ONE / SUM
                        go to 250
                     end if
                     SUM = SUM + FVM1
                  else
                     if (MU .ne. 0) then
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
                        SCALE = SGAMMA(V+ONE) / ETA * SUM * TWODX ** V
                        SCALE = ONE / SCALE
                        go to 250
                     end if
                  end if
               end if
            go to 220
  250       continue
C
C     NORMALIZE BJ() TO GET VALUES OF J-BESSEL FUNCTION.
C
            do 260 I = 1, NUM
               BJ(I) = BJ(I) * SCALE
  260       continue
            return
         else if( X .lt. HICUT ) then
c
c ********************* Code for the large X case. *********************
c
c  Here we have X .ge. XPQ, and V in [0.,1.).
c     The asymptotic series for  the auxiliary functions P and Q can be
c     used.  From these we will compute J(V,X) and J(V+1,X) and
c     then recur forward.  Reference: NBS AMS 55 Eqs 9.2.5 & 9.2.6
c
            call SBESPQ (X,V,  P,Q)
            CHI = X - (V + HALF) * HALFPI
            BESV = sqrt(ONE / (HALFPI*X)) * (P*COS(CHI) - Q*SIN(CHI))
C
            if( NMAX .GT. 0 ) then
               call SBESPQ (X,V+ONE,   P,Q)
               CHI = X - (V + C1P5) * HALFPI
               BESVP1 = sqrt(ONE / (HALFPI*X))*(P*COS(CHI)-Q*SIN(CHI))
            end if
c
            TWODX = TWO / X
c
c           Given BESV = J(V,X), BESVP1 = J(V+1,X), TWODX = 2/X,
c           NMIN, NUM, NMAX = NMIN + NUM -1, X, ALPHA, and BIG.
c           Recur forward and store J(NMIN+V) thru J(NMAX+V) in
c           BJ(1) thru BJ(NUM).
c           There should be no overflow posibility in this forward
c           recursion since NMAX .le. X - 1, and in this region the
c           magnitude of the J function is less than one.
c
            if( NMIN .eq. 0 ) then
               BJ(1) = BESV
               if( NMAX .GT. 0 ) then
                  BJ(2) = BESVP1
               end if
            else if( NMIN .eq. 1 ) then
               BJ(1) = BESVP1
            end if
c
            if( NMAX .GT. 1 ) then
               G = V * TWODX
c
c        Note:  In the following statement, 3-NMIN can be nonpositive.
c
               do 300 K = 3-NMIN, NUM
                  BESVM1 = BESV
                  BESV   = BESVP1
                  G = G + TWODX
                  BESVP1 = G * BESV - BESVM1
                  if( K .ge. 1)  BJ(K) = BESVP1
  300          continue
            end if
            return
         else
c                                                  Error 2.
            call ERMSG('SBESJN', 2, 0,
     *         'Cannot obtain any accuracy when X exceeds HICUT.', ',')
            call SERV1('HICUT', HICUT, ',')
         end if
      end if
      call SERV1('X',X,',')
      call SERV1('ALPHA',ALPHA,',')
      call IERV1('NUM',NUM,'.')
      return
      end
