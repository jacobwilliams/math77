      PROGRAM drsdasl4
c>> 2008-10-29 DRSDASL4 Krogh Fixed print output for C.
c>> 2008-10-26 DRSDASL4 Krogh Changed Fortran 90 type statement to F77.
c>> 2008-10-26 DRSDASL4 Krogh Moved Format statements up for C conv.
c>> 2008-10-24 DRSDASL4 Krogh Removed in INCLUDE statement & cDEC$...
c>> 2008-09-17 DRSDASL4 Hanson added starting values for y'
c>> 2008-08-26 DRSDASL4 Hanson added row dimension to evaluator
c>> 2001-10-11 DRSDASL4  R. J. Hanson Example 4 Code, with Download
 
c     Solve Enright and Pryce stiff test problem E5.
c     The equation is presented as y'= F(t,y), y_0 given.
c     This is solved by defining the residual function
c     f(t,y,y')=F(t,y)-y'.  Variational equations, with respect
c     to model parameters b0,b1,b2,b3, and b4, are integrated together
c     with state equations.  The block structure of the linear algebra
c     solve step is used.  The first NSTATE are state equations,
c     and the remaining blocks are for the variational equations.
 
c     Reverse communication is used for the evaluation and
c     linear solve steps.
 
c--S replaces "?": DR?DASL4, ?DASLX, ?DASSF4, ?COPY, ?GEFA, ?GESL, ?DOT
      INTEGER nstate,nvareq,neq,nepoch
      PARAMETER  (nstate=4,nvareq=5)
c     Set number of equations:
      PARAMETER  (neq=nstate+nvareq*nstate)
c     Set number of constraints.
      INTEGER maxcon
      PARAMETER  (maxcon=0)
c     Work space sizes:
      INTEGER liw
      PARAMETER  (liw=30+neq)
c.....The work space does not need the NEQ**2-size additional
c.....array piece because linear algebra is done in user work space.
c     This is flagged by INFO(5)=-6.
      INTEGER lrw
      PARAMETER  (lrw=45+(5+2*maxcon+4)*neq)
      PARAMETER  (nepoch=10)
      INTEGER i,j,k,l,info(16),idid,ires,ier,iwork(liw),ipvt(nstate),
     & ival(3)
 
      REAL             t,y(neq),yprime(neq),rtol(neq),atol(neq),
     & rwork(lrw),tout(nepoch),b0,b1,b2,b3,b4,sdot,cj
 
      INTEGER ndig
      REAL             tol
c++S Default NDIG = 3
c++  Default NDIG = 8
c++ Substitute for NDIG below
      PARAMETER  (ndig=3)
      PARAMETER  (tol=10.e0**(-ndig))
 
      EXTERNAL sdassf4
 
      REAL             delta(nstate),gy(nstate,nstate),
     & gp(nstate,nvareq),yp(nstate,nvareq),ypp(nstate,nvareq),
     & d(nstate,nstate)
 
  120 FORMAT (20x,'Example Results for a Stiff ODE Problem, E5.'/16x,
     & 'Five additional variational equations are integrated.'//10x,
     & 'T',12x,'y_1',11x,'y_2',11x,'y_3',11x,'y_4')
  130 FORMAT (13x,'No. of Residual Evaluations',6x,'Factorizations',6x,
     & 'No. of User Solves'/'Reverse Communication-',i9,15x,i9,13x,i9/)
 
c     Tolerances:
      DO 10 i=1,nstate
        atol(i)=tol
        rtol(i)=tol
   10 CONTINUE
c  Put more emphasis on relative error for the variational equations.
c  (How to set these tolerances depends on the problem.)
      DO 20 i=nstate+1,neq
        atol(i)=0
        rtol(i)=tol
   20 CONTINUE
 
c     Setup options:
      DO 30 i=1,16
        info(i)=0
   30 CONTINUE
 
c     Use partial derivatives and reverse communication
c     Do the linear algebra ourselves
      info(5)=-6
 
c     Set output points for integration:
      tout(1)=0.e0
      tout(2)=1.e-3
      DO 40 i=3,nepoch
        tout(i)=10*tout(i-1)
   40 CONTINUE
c     Have reverse communiation for f, and partials,
c     solver, and storage of partials.
      b0=1.76e-03
      b1=7.89e-10
      b2=1.1e+07
      b3=1.13e+09
      b4=1.13e+03
c     Set initial conditions for state and variational equations:
      y(1)=0.e0
      yprime(1)=0.e0
      CALL scopy (neq, y, 0, y, 1)
      CALL scopy (neq, yprime, 0, yprime, 1)
      y(1)=b0
      y(nstate+1)=1.e0
 
c     Set the initial value of YPRIME(*):
      yprime(1)=-b1*b0
      yprime(2)=b1*b0
      yprime(3)=b1*b0
 
      yprime(nstate+1)=-b1
      yprime(nstate+2)=b1
      yprime(nstate+3)=b1
 
      yprime(2*nstate+1)=-b0
      yprime(2*nstate+2)=b0
      yprime(2*nstate+3)=b0
c     Count residuals, factorizations and solves.
      ival(1)=0
      ival(2)=0
      ival(3)=0
c     Print heading.
      WRITE (*,120)
c     Allow more than nominal number of steps.
      info(12)=5000
      DO 110 L=2,nepoch
        t=tout(L-1)
c     Branch here to re-enter routine.
   50   CALL sdaslx (sdassf4, neq, t, y, yprime, tout(L), info, rtol,
     &   atol, idid, rwork, lrw, iwork, liw)
c     When IDID == 4 the code returned for reverse communication.
c     Otherwise the integration has finished up to this output point.
        IF (idid.ne.4) GO TO 100
c     Get flag for user action:
        ires=iwork(3)
 
c     Will need partials for IRES=1,2:
        IF (ires.le.2) THEN
          gy(1,1)=0.e0
          gp(1,1)=0.e0
          CALL scopy (nstate**2, gy, 0, gy, 1)
          CALL scopy (nstate*nvareq, gp, 0, gp, 1)
 
c     Evaluate DG/DY matrix:
 
          gy(4,1)=b2*y(3)
          gy(1,1)=-b1-gy(4,1)
          gy(2,1)=b1
          gy(3,1)=b1-gy(4,1)
 
          gy(1,2)=0.e0
          gy(3,2)=-b3*y(3)
          gy(2,2)=gy(3,2)
          gy(4,2)=0.e0
 
          gy(1,3)=-b2*y(1)
          gy(2,3)=-b3*y(2)
          gy(3,3)= gy(1,3)+gy(2,3)
          gy(4,3)=-gy(1,3)
 
          gy(1,4)=0.e0
          gy(2,4)=0.e0
          gy(3,4)= b4
          gy(4,4)=-b4
 
c     Evaluate DG/DP matrix:
          gp(1,2)=-y(1)
          gp(2,2)= y(1)
          gp(3,2)= y(1)
 
          gp(1,3)=-y(1)*y(3)
          gp(3,3)=-y(1)*y(3)
 
          gp(2,4)=-y(2)*y(3)
          gp(3,4)= y(2)*y(3)
 
          gp(3,5)= y(4)
          gp(4,5)=-y(4)
 
c     Extract DY/DP:
          CALL scopy (nstate*nvareq, y(nstate+1), 1, yp, 1)
c     Extract (DY/DP)':
          CALL scopy (nstate*nvareq, yprime(nstate+1), 1, ypp, 1)
        END IF
 
c     Evaluate state and variational equations.
        IF (ires.eq.1) THEN
c     Count residual evaluations.
          ival(1)=ival(1)+1
          delta(1)=-b1*y(1)-b2*y(1)*y(3)-yprime(1)
          delta(2)= b1*y(1)-b3*y(2)*y(3)-yprime(2)
          delta(3)= b1*y(1)-b2*y(1)*y(3)+b4*y(4)-b3*y(2)*y(3)-yprime(3)
          delta(4)= b2*y(1)*y(3)-b4*y(4)-yprime(4)
 
c     Compute matrix arithmetic, DG/DY * DY/DP + DG/DP:
c     Update DG/DP = DG/DP - D (Y')/DP:
          DO 70 j=1,nvareq
            DO 60 i=1,nstate
              gp(i,j)=gp(i,j)+sdot(nstate,gy(i,1),nstate,yp(1,j),1)-
     &         ypp(i,j)
   60       CONTINUE
   70     CONTINUE
c     Move the results where the integrator uses them:
          CALL scopy (nstate, delta, 1, rwork(iwork(4)), 1)
          CALL scopy (nstate*nvareq, gp, 1, rwork(iwork(4)+nstate), 1)
          GO TO 50
 
        END IF
 
        IF (ires.eq.2) THEN
c     Evaluate partial matrix, DG/DY-C*I_4.
c     Each of the NSTATE vectors has this
c     matrix as a diagonal block.  The C value
c     is located in RWORK(1).
c     There are NVAREQ+1 such vectors.
c     Taking advantage of this fact allows the
c     corrector equation to be factored and solved
c     using a system of size equal to the number of
c     state vector components.
          CALL scopy (nstate**2, gy, 1, d, 1)
          cj=rwork(1)
          DO 80 i=1,nstate
            d(i,i)=d(i,i)-cj
   80     CONTINUE
c     Factor the 4 by 4 matrix using the Linpack code.
c     Count factorizations.
          ival(2)=ival(2)+1
c     This is where savings occur, compared to solving a full system.
          CALL sgefa (d, nstate, nstate, ipvt, ier)
c     This passes the error code, after factorization,
c     back to the integrator.
          iwork(3)=ier
          GO TO 50
        END IF
c     Solve with NVAREQ+1 right-hand sides using the Linpack code.
c     This is where savings occur, compared to solving a full system.
        IF (ires.eq.4) THEN
c     Count solves.
          ival(3)=ival(3)+1
c     Index to place solutions of corrector equations.
          k=iwork(4)
          DO 90 j=1,nvareq+1
            CALL sgesl (d, nstate, nstate, ipvt, rwork(k), 0)
            k=k+nstate
   90     CONTINUE
 
          GO TO 50
        END IF
  100   CONTINUE
c     Print time, state and variational equation results.
c%% printf("%14.4e%14.4e%14.4e%14.4e%14.4e\n",
c%%   Tout[l], y[0], y[1], y[2], y[3]);
c%% for(_n=4L; _n < sizeof(y)/sizeof(float ); _n+=4)
c%%   printf("              %14.4e%14.4e%14.4e%14.4e\n",
c%%     y[_n], y[_n+1], y[_n+2], y[_n+3]);
        WRITE (*,'(1P,5D14.4/(14x,4D14.4))') tout(L),y
  110 CONTINUE
      WRITE (*,130) ival
      END
 
      SUBROUTINE sdassf4 (t, y, yprime, delta, d, ldd, cj, ires, rwork,
     & iwork)
 
c     Dummy routine for reverse communication:
c     This is a double precision version.
      REAL             t,y(*),yprime(*),delta(*),d(ldd,*),cj,rwork(*)
      INTEGER ires,iwork(*),ldd
 
c     This is the setup call.  It can be ignored since
c     all problem information is provided using reverse communication.
      IF (ires.eq.0) THEN
        RETURN
      END IF
c     Other values of IRES will not occur.
      END
 
