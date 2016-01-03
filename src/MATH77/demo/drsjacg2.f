      PROGRAM DRSJACG2
c>> 2009-10-28 DRSJACG2 Krogh -- Remove implicit none.
c>> 2009-10-26 DRSJACG2 Krogh -- *'s used for dims for NAG compiler.
c>> 2008-11-01 DRSJACG2 Hanson -- Added row dim parameters to evaluators
c>> 2006-12-19 DRSJACG2 Hanson -- Added tot. energy constraint in 3rd ex
c>> 2006-04-11 DRDJACG1 Hanson -- Reduced lengths of sjacg work arrays.
c>> 2006-04-09 DRSJACG2  Krogh Fixed dimension of YSCALE.
c>> 2003-07-08 DRSJACG2  R. J. Hanson Fix accum. option with sjacg().
c>> 2002-02-01 DRSJACG2  R. J. Hanson Example 2 Code, with Download
 
C     Solve a planar pendulum problem in rectangular coordinates.
C     The equation is transformed from "Index 3" to "Index 1"
C     by differentiating the length constraint twice.  The system
C     is integrated (using SDASSFC) using these two constraints.
C     In the second integration the problem is transformed from
C     "Index 3" to "Index 0" by differentiating the length constraint
C     three times.  The routine SDASSFD uses these three constraints.
C     A total energy constraint is added in routine SDASSFE.
C     This example shows that the constraints remain satisfied,
C     and the integration can succeed with either approach.
C     It is more efficient to use the "Index 0" problem than
C     the "Index 1" problem.
 
C     (THIS VERSION USES NUMERICAL DIFFERENTIATION FOR THE PARTIALS
C     NEEDED IN ?DASLX, EXAMPLE 5, AVAILABLE WITH DOWNLOAD.)
c--S replaces "?": DR?JACG2,?DASLX,?DASSFC,?DASSFD,?DASSFE,?COPY,?JACG
 
      EXTERNAL SDASSFC, SDASSFD, SDASSFE
 
      integer NDIG, NEQ, MAXCON, LRW, LIW
C     Set number of equations:
      parameter (NEQ = 5)
C     Set number of constraints.
      parameter (MAXCON = 4)
C     Work space sizes:
      parameter (LIW = 30 + NEQ)
      parameter (LRW =  45 + (5 + 2*MAXCON + 4) * NEQ + NEQ**2)
      real             TOL
c++S Default NDIG = 4
c++  Default NDIG = 11
c++ Substitute for NDIG below
      parameter (NDIG = 4 )
      parameter (TOL = 10.E0 **(-NDIG))
 
      INTEGER I, INFO(16), IDID, IWORK(LIW)
      REAL             T, Y(5), YPRIME(5), TOUT,
     +  RTOL(5), ATOL(5), RWORK(LRW), LENGTH,
     +  DRL, DRV
      LOGICAL CONSTRAINT
      INTEGER KR, KF, KC
      common / COUNTS / KR, KF, KC
 
  100 format(20x,
     +'Example Results for a Constrained Pendulum Problem, Index 1')
  105 format(20x,'(Numerical Partials)')
  106 format(20x,'(Numerical Partials and Total Energy Constrained)')
  110 format(/8x,'T',12x,'y_1',11x,'y_2',11x,
     + 'y_3',11x,'y_4',11x,'y_5'/ 1P,6D14.4/)
  130 format(6x,'At the time value', F10.2, 2x,
     + 'the integration was stopped.')
  140 format(
     +'The pendulum length or variation has large weighted errors.'/
     +'These should remain less than 1 is magnitude:',
     + 10x,2F8.2/)
  150 format(6x,
     +'The pendulum length and its variation have small errors.'//)
  160 format(20x,
     +'Example Results for a Constrained Pendulum Problem, Index 0')
  170 format(5x,'No. of Residual Evaluations',6x,
     + 'Factorizations',6x,'No. of User Solves'/
     + 'Constraint Partials-',1x,I8,12x,I8,17x,I8/)
 
C     Tolerances:
      DO 10 I=1,NEQ
        ATOL(I)=TOL
        RTOL(I)=TOL
   10 CONTINUE
C     Setup options:
      DO 20 I=1,16
        INFO(I)=0
   20 CONTINUE
C     Use partial derivatives provided in evaluation routine:
      INFO(5)=2
C     Constrain solution, with 2 constraints:
      INFO(10)=2
C     Allow for more 10 * steps (than default):
      INFO(12)=50000
C     This is the pendulum length.
      LENGTH=1.1E0
      DO 30 I=1,1000,10
C     Integrate from T=I-1 to TOUT=T+1.  Final TOUT=10.
C     When the solution first drifts away from the constraints, stop.
        T=real(I-1)
        TOUT=T+10.E0
        CALL SDASLX (SDASSFC, NEQ, T, Y, YPRIME, TOUT, INFO, RTOL,
     +    ATOL, IDID, RWORK, LRW, IWORK, LIW)
C  Compute residuals on length and its variation.  They
C  should be smaller than the tolerances used.
        DRL=(Y(1)**2+Y(2)**2-LENGTH**2)/(RTOL(1)*LENGTH**2+ATOL(1))
        DRV=(Y(1)*Y(3)+Y(2)*Y(4))/(RTOL(1)*(abs(Y(1)*Y(3))+
     +   abs(Y(1)*Y(4)))+ATOL(1))
        CONSTRAINT=ABS(DRL) .le. 1.E0 .and. ABS(DRV) .le. 1.E0
        IF(.not. CONSTRAINT) GO TO 40
   30 CONTINUE
   40 CONTINUE
 
      write(*,100)
      write(*,105)
      write(*,110) TOUT, Y
      write(*,170) KR, KF, KC
      write(*,130) TOUT
      IF(.not. CONSTRAINT) THEN
        write(*,140) DRL, DRV
      ELSE
        write(*,150)
      END IF
 
C     Start the integration over for the index 0 problem.
      INFO(1)=0
C     Set the number of constraints to 3.
      INFO(10)=3
      DO 50 I=1,1000,10
C     Integrate from T=I-1 to TOUT=T+1.
        T=real(I-1)
        TOUT=T+10.E0
        CALL SDASLX (SDASSFD, NEQ, T, Y, YPRIME, TOUT, INFO, RTOL,
     +    ATOL, IDID, RWORK, LRW, IWORK, LIW)
 
C  Compute residuals on length and its variation.  They
C  should be smaller than the tolerances used.
        DRL=(Y(1)**2+Y(2)**2-LENGTH**2)/(RTOL(1)*LENGTH**2+ATOL(1))
        DRV=(Y(1)*Y(3)+Y(2)*Y(4))/(RTOL(1)*(abs(Y(1)*Y(3))+
     +   abs(Y(1)*Y(4)))+ATOL(1))
        CONSTRAINT=ABS(DRL) .le. 1.E0 .and. ABS(DRV) .le. 1.E0
        IF(.not. CONSTRAINT) GO TO 60
   50 CONTINUE
   60 CONTINUE
      write(*,160)
      write(*,105)
      write(*,110) TOUT, Y
      write(*,170) KR, KF, KC
      write(*,130) TOUT
      IF(.not. CONSTRAINT) THEN
        write(*,140) DRL, DRV
      ELSE
        write(*,150)
      END IF
C     Start the integration over for the index 0 problem.
      INFO(1)=0
C     Set the number of constraints to 4.
C     (This includes constant total energy).
      INFO(10)=4
      DO 70 I=1,1000,10
C     Integrate from T=I-1 to TOUT=T+1.
        T=real(I-1)
        TOUT=T+10.E0
        CALL SDASLX (SDASSFE, NEQ, T, Y, YPRIME, TOUT, INFO, RTOL,
     +    ATOL, IDID, RWORK, LRW, IWORK, LIW)
 
C  Compute residuals on length and its variation.  They
C  should be smaller than the tolerances used.
        DRL=(Y(1)**2+Y(2)**2-LENGTH**2)/(RTOL(1)*LENGTH**2+ATOL(1))
        DRV=(Y(1)*Y(3)+Y(2)*Y(4))/(RTOL(1)*(abs(Y(1)*Y(3))+
     +   abs(Y(1)*Y(4)))+ATOL(1))
        CONSTRAINT=ABS(DRL) .le. 1.E0 .and. ABS(DRV) .le. 1.E0
        IF(.not. CONSTRAINT) GO TO 80
   70 CONTINUE
   80 CONTINUE
      write(*,160)
      write(*,106)
      write(*,110) TOUT, Y
      write(*,170) KR, KF, KC
      write(*,130) TOUT
      IF(.not. CONSTRAINT) THEN
        write(*,140) DRL, DRV
      ELSE
        write(*,150)
      END IF
      END
 
      SUBROUTINE SDASSFC(T,Y,YPRIME,DELTA,D,LDD,CJ,IRES,RWORK,IWORK)
C     Routine for the swinging simple pendulum problem,
C     with constraints on the index 2 and 3 equations.
      REAL             T, Y(*), YPRIME(*), DELTA(*), D(LDD,*),
     +  CJ, RWORK(*), LSQ, MG
      INTEGER IRES, IWORK(*),LDD
 
      REAL             ONE, TWO, ZERO, MASS, LENGTH, GRAVITY
 
C     Use numerical derivatives but based on calls to SJACG.
      INTEGER MODE, IOPT(5), IWK(21)
      REAL             WK(3*5+18), F(5), FACC(5),
     .  FACJ(5), YSCALE(5)
 
      PARAMETER (ONE=1.E0, TWO=2.E0,
     +  ZERO=0.E0, MASS=1.E0, LENGTH=1.1E0, GRAVITY=9.806650E0)
      SAVE mg, LSQ, FACC, FACJ, IOPT, YSCALE
      INTEGER KR, KF, KC
      common / COUNTS / KR, KF, KC
C This is the setup call.  YPRIME need not be solved for as we
C are providing the correct initial values.
      IF(IRES .EQ. 0) THEN
        mg=mass*gravity
        LSQ=length**2
        Y(1)=LENGTH
        Y(2)=ZERO
        Y(3)=ZERO
        Y(4)=ZERO
        Y(5)=ZERO
 
        YPRIME(1)=ZERO
        YPRIME(2)=ZERO
        YPRIME(3)=ZERO
        YPRIME(4)=-GRAVITY
        YPRIME(5)=ZERO
 
        KR=0
        KF=0
        KC=0
C Initialize things needed for numerical differentiation.
        FACC(1)=ZERO
        FACJ(1)=ZERO
      END IF
 
C The sytem residual value.
 
      IF(IRES .EQ. 1) THEN
        DELTA(1)= Y(3)-YPRIME(1)
        DELTA(2)= Y(4)-YPRIME(2)
        DELTA(3)=-Y(1)*Y(5)-mass*YPRIME(3)
        DELTA(4)=-Y(2)*Y(5)-mass*YPRIME(4)-mg
        DELTA(5)= mass*(Y(3)**2+Y(4)**2)-mg*Y(2)-LSQ*Y(5)
C Count residual evaluations.
        KR=KR+1
      END IF
 
C The partial derivative matrix.
      IF(IRES .EQ. 2) THEN
        MODE=0
C Accumulate all but one partial derivative columns.
 
C Set to accumulate variables 1-4:
        IOPT(1)=-3
        IOPT(2)= 4
C Shift to using one-sided differences.
        IOPT(3)=-1
C Use on all remaining variables: number 5.
        IOPT(4)= 0
        YSCALE(1)=ZERO
C Pre-compute partials wrt y'.  These values are accumulated.
        D(1,1)=-CJ
        D(2,2)=-CJ
        D(3,3)=-mass*CJ
        D(4,4)=D(3,3)
   10 CONTINUE
C The value of f(y) is normally returned in WK(1:5).
C Note that all but one partial with respect to y' have been
C analytically pre-computed and so the results are accumulated.
        WK(1)=Y(3)
        WK(2)=Y(4)
        WK(3)=-Y(1)*Y(5)
        WK(4)=-Y(2)*Y(5)
        WK(5)= mass*(Y(3)**2+Y(4)**2)-mg*Y(2)-LSQ*Y(5)
C Compute f(y) and make a special copy step for the
C  evaluation point.
        IF(MODE .eq. 0) THEN
          F(1)=WK(1)
          F(2)=WK(2)
          F(3)=WK(3)
          F(4)=WK(4)
          F(5)=WK(5)
          FACJ(1)=ZERO
        END IF
C This is a Math a la Carte code for numerical derivatives.
        CALL SJACG (MODE, 5, 5, Y,
     .              F, D, LDD, YSCALE,
     .              FACJ, IOPT, WK, 33, IWK, 21)
        IF(MODE .gt. 0) GO TO 10
        IF(MODE .lt. 0) THEN
            PRINT
     1      '('' SDASSFC: Initial error in argument number '',I2)',-MODE
            STOP '1'
          END IF
C All system partials are computed.  Count an evaluation.
        KF=KF+1
      END IF
 
C The constraining equations after the corrector has converged.
C Both partials and residuals are required.
      IF(IRES .EQ. 5) THEN
        MODE=0
C In this use of numerical differentiation, no special treatment
C is required for the variables.  The rest of IOPT(:) stays intact.
        IOPT(1)=0
        YSCALE(1)=ZERO
   20 CONTINUE
C The value of f(y) is normally returned in WK(1:2).
        WK(1)=Y(1)**2+Y(2)**2-LSQ
        WK(2)=Y(1)*Y(3)+Y(2)*Y(4)
C Compute f(y) and make a special copy step for the point in question.
        IF(MODE .eq. 0) THEN
          DELTA(6)=WK(1)
          DELTA(7)=WK(2)
          FACC(1)=ZERO
        END IF
C This is a Math a la Carte code for numerical derivatives.
        CALL SJACG (MODE, 2, 5, Y,
     .              DELTA(6), D(6,1), LDD, YSCALE,
     .              FACC, IOPT, WK, 33, IWK, 21)
        IF(MODE .gt. 0) GO TO 20
 
        IF(MODE .lt. 0) THEN
            PRINT
     1      '('' SDASSFC: Initial error in argument number '',I2)',-MODE
            STOP '2'
          END IF
C All constraint partials are computed.  Count an evaluation.
        KC=KC+1
      END IF
      END
 
      SUBROUTINE SDASSFD(T,Y,YPRIME,DELTA,P,LDP,CJ,IRES,RWORK,IWORK)
C
C     Routine for the swinging simple pendulum problem,
C     with constraints on the index 1,2 amd 3 equations.
C     Use P(:,:) to distinguish from D(:,:) in routine SDASSFC.
      INTEGER IRES, IWORK(*), LDP
      REAL             T, Y(*), YPRIME(*), DELTA(*), P(LDP,*),
     +  CJ, RWORK(*), LSQ, MG
C     Use numerical derivatives but based on calls to SJACG.
      INTEGER MODE, IOPT(2), IWK(21)
      REAL             WK(3*5+18), F(5), FACC(5),
     .  FACJ(5), YSCALE(5)
 
      REAL             ONE, TWO, THREE, ZERO, MASS, LENGTH, GRAVITY
      PARAMETER (ONE=1.E0, TWO=2.E0, THREE=3.E0,
     +  ZERO=0.E0, MASS=1.E0, LENGTH=1.1E0, GRAVITY=9.806650E0)
      SAVE mg, LSQ, FACC, FACJ, YSCALE, IOPT
      INTEGER KR, KF, KC
      common / COUNTS / KR, KF, KC
C This is the setup call.
      IF(IRES .EQ. 0) THEN
        mg=mass*gravity
        LSQ=length**2
        Y(1)=LENGTH
        Y(2)=ZERO
        Y(3)=ZERO
        Y(4)=ZERO
        Y(5)=ZERO
        YPRIME(1)=ZERO
        CALL SCOPY(5,YPRIME,0,YPRIME,1)
        yprime(4)=-gravity
        KR=0
        KF=0
        KC=0
        YSCALE(1)=ZERO
      END IF
 
C The sytem residual value.
      IF(IRES .EQ. 1) THEN
        DELTA(1)=Y(3)-YPRIME(1)
        DELTA(2)=Y(4)-YPRIME(2)
        DELTA(3)=-Y(1)*Y(5)-mass*YPRIME(3)
        DELTA(4)=-Y(2)*Y(5)-mass*YPRIME(4)-mg
        DELTA(5)= -THREE*mg*Y(4) -LSQ*YPRIME(5)
        KR=KR+1
      END IF
 
C The partial derivative matrix.
      IF(IRES .EQ. 2) THEN
        MODE=0
        IOPT(1)=0
C This is the last column number to be accumulated when
C doing numerical differentiation.
        IOPT(2)= 5
 
   10 CONTINUE
C The value of f(y) is normally returned in WK(1:5).
C Note that the partials with respect to y' have been
C analytically pre-computed and so the results are accumulated.
        WK(1)=Y(3)
        WK(2)=Y(4)
        WK(3)=-Y(1)*Y(5)
        WK(4)=-Y(2)*Y(5)
        WK(5)= -THREE*mg*Y(4)
C Compute f(y) and make a special copy step for the point in question.
        IF(MODE .eq. 0) THEN
          F(1)=WK(1)
          F(2)=WK(2)
          F(3)=WK(3)
          F(4)=WK(4)
          F(5)=WK(5)
          FACJ(1)=ZERO
        END IF
C This is a Math a la Carte code for numerical derivatives.
        CALL SJACG (MODE, 5, 5, Y,
     .              F, P, LDP, YSCALE,
     .              FACJ, IOPT, WK, 33, IWK, 21)
        IF(MODE .gt. 0) GO TO 10
        IF(MODE .lt. 0) THEN
            PRINT
     1     '('' SDASSFD: Initial error in argument number '',I2)', -MODE
            STOP '1'
          END IF
C All system partials are computed.  Count an evaluation.
        KF=KF+1
C This part of the derivative matrix is a diagonal, hence
C easy to compute.
        P(1,1)=-CJ+P(1,1)
        P(2,2)=-CJ+P(2,2)
        P(3,3)=-mass*CJ+P(3,3)
        P(4,4)=P(3,3)
        P(5,5)=-LSQ*CJ+P(5,5)
 
      END IF
 
C The constraining equations after the corrector has converged.
C Both partials and residuals are required.
      IF(IRES .EQ. 5) THEN
        MODE=0
        IOPT(1)=0
        YSCALE(1)=ZERO
   20 CONTINUE
C The value of f(y) is normally returned in WK(1:3).
        WK(1)=Y(1)**2+Y(2)**2-LSQ
        WK(2)=Y(1)*Y(3)+Y(2)*Y(4)
        WK(3)=MASS*(Y(3)**2+Y(4)**2)-MG*Y(2)-LSQ*Y(5)
C Compute f(y) and make a special copy step for the
C evaluation point.
        IF(MODE .eq. 0) THEN
          DELTA(6)=WK(1)
          DELTA(7)=WK(2)
          DELTA(8)=WK(3)
          FACC(1)=ZERO
        END IF
C This is a Math a la Carte code for numerical derivatives.
        CALL SJACG (MODE, 3, 5, Y,
     .              DELTA(6), P(6,1), LDP, YSCALE,
     .              FACC, IOPT, WK, 33, IWK, 21)
        IF(MODE .gt. 0) GO TO 20
        IF(MODE .lt. 0) THEN
            PRINT
     1      '('' SDASSFD: Initial error in argument number '',I2)',-MODE
            STOP '2'
          END IF
C All constraint partials are computed.  Count an evaluation.
        KC=KC+1
      END IF
      END
 
      SUBROUTINE SDASSFE(T,Y,YPRIME,DELTA,P,LDP,CJ,IRES,RWORK,IWORK)
C
C     Routine for the swinging simple pendulum problem,
C     with constraints on the index 1,2 amd 3 equations.
C     Also the constant energy constraint is added.
C     Use P(:,:) to distinguish from D(:,:) in routine SDASSFC.
      INTEGER IRES, IWORK(*), LDP
      REAL             T, Y(*), YPRIME(*), DELTA(*), P(LDP,*),
     +  CJ, RWORK(*)
C     Use numerical derivatives but based on calls to SJACG.
      INTEGER MODE, IOPT(2), IWK(21)
      REAL             WK(43), F(5), FACC(5),
     .  FACJ(5), YSCALE(1)
 
      REAL             HALF, ONE, TWO, THREE, ZERO, MASS, LENGTH,
     +                 GRAVITY, LSQ, MG
      PARAMETER (ONE=1.E0, TWO=2.E0, THREE=3.E0, HALF=0.5E0,
     +  ZERO=0.E0, MASS=1.E0, LENGTH=1.1E0, GRAVITY=9.806650E0,
     +          mg=mass*gravity, LSQ=length**2)
c      SAVE FACC, FACJ, YSCALE, IOPT
      INTEGER KR, KF, KC
      common / COUNTS / KR, KF, KC
C This is the setup call.
      IF(IRES .EQ. 0) THEN
        Y(1)=LENGTH
        Y(2)=ZERO
        Y(3)=ZERO
        Y(4)=ZERO
        Y(5)=ZERO
        YPRIME(1)=ZERO
        YPRIME(2)=ZERO
        YPRIME(3)=ZERO
        YPRIME(5)=ZERO
        YPRIME(4)=-GRAVITY
        KR=0
        KF=0
        KC=0
        YSCALE(1)=ZERO
      END IF
 
C The sytem residual value.
      IF(IRES .EQ. 1) THEN
        DELTA(1)=Y(3)-YPRIME(1)
        DELTA(2)=Y(4)-YPRIME(2)
        DELTA(3)=-Y(1)*Y(5)-mass*YPRIME(3)
        DELTA(4)=-Y(2)*Y(5)-mass*YPRIME(4)-mg
        DELTA(5)= -THREE*mg*Y(4) -LSQ*YPRIME(5)
        KR=KR+1
      END IF
 
C The partial derivative matrix.
      IF(IRES .EQ. 2) THEN
        MODE=0
C Accumulate all partial derivative columns.
C Pre-compute partials wrt y' and accumulate results.
        IOPT(1)=-3
C This is the last column number to be accumulated when
C doing numerical differentiation.
        IOPT(2)= 5
 
C This part of the derivative matrix is a diagonal, hence
C easy to compute.
        P(1,1)=-CJ
        P(2,2)=-CJ
        P(3,3)=-mass*CJ
        P(4,4)=P(3,3)
        P(5,5)=-LSQ*CJ
   10 CONTINUE
C The value of f(y) is normally returned in WK(1:5).
C Note that the partials with respect to y' have been
C analytically pre-computed and the results are accumulated.
        WK(1)=Y(3)
        WK(2)=Y(4)
        WK(3)=-Y(1)*Y(5)
        WK(4)=-Y(2)*Y(5)
        WK(5)=-THREE*mg*Y(4)
C Compute f(y) and make a special copy step for the point in question.
        IF(MODE .eq. 0) THEN
          F(1)=WK(1)
          F(2)=WK(2)
          F(3)=WK(3)
          F(4)=WK(4)
          F(5)=WK(5)
          FACJ(1)=ZERO
          YSCALE(1)=ZERO
        END IF
C This is a Math a la Carte code for numerical derivatives.
        CALL SJACG (MODE, 5, 5, Y,
     .              F, P, LDP, YSCALE,
     .              FACJ, IOPT, WK, 43, IWK, 21)
        IF(MODE .gt. 0) GO TO 10
        IF(MODE .lt. 0) THEN
            PRINT
     1     '('' SDASSFD: Initial error in argument number '',I2)', -MODE
            STOP '1'
          END IF
C All system partials are computed.  Count an evaluation.
        KF=KF+1
      END IF
 
C The constraining equations after the corrector has converged.
C Both partials and residuals are required.
      IF(IRES .EQ. 5) THEN
        MODE=0
        IOPT(1)=0
        FACC(1)=ZERO
        YSCALE(1)=ZERO
   20 CONTINUE
C The value of f(y) is normally returned in WK(1:3).
        WK(1)=Y(1)**2+Y(2)**2-LSQ
        WK(2)=Y(1)*Y(3)+Y(2)*Y(4)
        WK(3)=MASS*(Y(3)**2+Y(4)**2)-MG*Y(2)-LSQ*Y(5)
C This constraint is constant total energy.
        WK(4)=HALF*MASS*(Y(3)**2+Y(4)**2)+MG*Y(2)
 
C Compute f(y) and make a special copy step for the
C evaluation point.
        IF(MODE .eq. 0) THEN
          DELTA(6)=WK(1)
          DELTA(7)=WK(2)
          DELTA(8)=WK(3)
          DELTA(9)=WK(4)
          F(1)=WK(1)
          F(2)=WK(2)
          F(3)=WK(3)
          F(4)=WK(4)
        END IF
C This is a Math a la Carte code for numerical derivatives.
        CALL SJACG (MODE, 4, 5, Y,
     .              F, P(6,1), LDP, YSCALE,
     .              FACC, IOPT, WK, 43, IWK, 21)
        IF(MODE .gt. 0) GO TO 20
        IF(MODE .lt. 0) THEN
            PRINT
     1      '('' SDASSFE: Initial error in argument number '',I2)',-MODE
            STOP '2'
          END IF
C All constraint partials are computed.  Count an evaluation.
         KC=KC+1
      END IF
      END
