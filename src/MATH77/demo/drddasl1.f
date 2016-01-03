      PROGRAM drddasl1
c>> 2009-10-21 DRDDASL1 Hanson/Krogh Fixed initialization.
c>> 2008-10-26 DRDDASL1 Krogh Moved Format statements up for C conv.
c>> 2008-10-24 DRDDASL1 Krogh Removed in INCLUDE statement & cDEC$...
c>> 2008-09-02 DRDDASL1 Hanson added starting computation of y'
c>> 2008-08-26 DRDDASL1 Hanson added row dimensions to evaluators
c>> 2006-04-10 DRDDASL1 Krogh Removed declaration of unused E.
c>> 2002-05-29 DRDDASL1 Krogh Changes for C conversion problem.
c>> 2001-10-11 DRDDASL1 R. J. Hanson Document Example Code,
  
c  Solve a C. W. Gear index=2 problem.
c  Reduce it to an index=1, solve it.
c  Reduce it further to index=0, solve it.
c  Compare results, which are equivalent but
c  not exactly equal.
  
c--D replaces "?": DR?DASL1, ?DASLX, ?DASLS, ?DASSF1, ?DASSF0
      EXTERNAL ddassf0,ddassf1
      INTEGER ndig,ntimes
      DOUBLE PRECISION tol
c++S Default NDIG = 4
c++  Default NDIG = 8
c++ Substitute for NDIG below
      PARAMETER  (ndig=8)
      PARAMETER  (tol=10.d0**(-ndig))
      INTEGER liw,lrw,maxcon,neq,ldc,ltd
c     Set number of equations:
      PARAMETER  (neq=2)
c     Set maximum number of constraints.
      PARAMETER  (maxcon=2)
c     Work space sizes:
      PARAMETER  (liw=30+neq)
      PARAMETER  (lrw=45+(5+2*maxcon+4)*neq+neq**2)
      PARAMETER  (ntimes=10)
      PARAMETER  (ldc=2*neq,ltd=neq)
      INTEGER i,info(16),idid,iwork(liw)
  
      DOUBLE PRECISION t,y(neq),yprime(neq),tout,rtol(neq),atol(neq),
     & rwork(lrw),soln1(ntimes,neq),soln0(ntimes,neq),
     & solp1(ntimes,neq),solp0(ntimes,neq)
      DOUBLE PRECISION c(ldc,ltd),ftol,rnktol
  
      INTEGER kount0,kount1
      COMMON /counts/ kount0,kount1
  
   60 FORMAT (6x,'Example Results for a Transformed Index-2 DAE Problem'
     &//10x,'T',11x,'Y1/Y2',9x,'Y1P/Y2P')
   70 FORMAT (6x,'Differences, (Index-1) - (Index-0) Values'//10x,'T',
     &11x,'Y1/Y2',9x,'Y1P/Y2P')
   80 FORMAT (1p,3e15.6/15x,2e15.6/' ')
   90 FORMAT (6x,'Index-1 and Index-0 residual evalutions:',2i5)
  100 FORMAT (6x,'Initial conditions for y,y'' at t=0, index 1 system')
  110 FORMAT (6x,'Initial conditions for y,y'' at t=0, index 0 system')
  
c     Tolerances:
      DO 10 i=1,neq
        atol(i)=tol
        rtol(i)=tol
   10 CONTINUE
c     Setup options:
      DO 20 i=1,16
        info(i)=0
   20 CONTINUE
c     Use partial derivatives provided in evaluator routines:
      info(5)=2
c     Constrain solution with 1 constraint:
      info(10)=1
c     Compute the initial value of YPRIME(*):
      t=0
      ftol=tol
      rnktol=tol
c     Assign initial y, and guess for y', then get initial y'.
      y(1) = 1.D0
      y(2) = 0.D0
      yprime(1)=0.D0
      yprime(2)=0.D0     
      CALL ddasls (ddassf1, neq, t, y, yprime, info, ftol, rnktol, c,
     & ldc, ltd, idid, rwork, lrw, iwork, liw)
      WRITE (*,60)
      WRITE (*,100)
      WRITE (*,80) t,y(1),yprime(1),y(2),yprime(2)
  
c     Allow up to 5000 steps:
      info(12)=5000
  
      DO 30 i=1,ntimes
c     Integrate from T=I-1 to TOUT=T+1.  Final TOUT=10.
        t=i-1
        tout=t+1
        CALL ddaslx (ddassf1, neq, t, y, yprime, tout, info, rtol, atol,
     &    idid, rwork, lrw, iwork, liw)
        WRITE (*,80) tout,y(1),yprime(1),y(2),yprime(2)
c     Save solution and derivative for comparison to index 0 values.
        soln1(i,1)=y(1)
        soln1(i,2)=y(2)
        solp1(i,1)=yprime(1)
        solp1(i,2)=yprime(2)
   30 CONTINUE
  
c     Start integration again.
      info(1)=0
      DO 40 i=1,neq
        atol(i)=tol
        rtol(i)=tol
   40 CONTINUE
c     Switch from 1 to 2 constraints, and use the index 0 system.
      info(10)=2
      WRITE (*,70)
      t=0.D0
c     Assign initial y, and guess for y', then get initial y'.
      y(1) = 1.D0
      y(2) = 0.D0
      yprime(1)=0.D0
      yprime(2)=0.D0     
      CALL ddasls (ddassf0, neq, t, y, yprime, info, ftol, rnktol, c,
     & ldc, ltd, idid, rwork, lrw, iwork, liw)
      WRITE (*,110)
      WRITE (*,80) t,y(1),yprime(1),y(2),yprime(2)
  
      DO 50 i=1,ntimes
c     Integrate from T=I-1 to TOUT=T+1.  Final TOUT=10.
        t=i-1
        tout=t+1
        CALL ddaslx (ddassf0, neq, t, y, yprime, tout, info, rtol, atol,
     &    idid, rwork, lrw, iwork, liw)
c     Use solution and derivative differences for comparison
c     with index 1 values.
        soln0(i,1)=soln1(i,1)-y(1)
        soln0(i,2)=soln1(i,2)-y(2)
        solp0(i,1)=solp1(i,1)-yprime(1)
        solp0(i,2)=solp1(i,2)-yprime(2)
        WRITE (*,80) tout,soln0(i,1),solp0(i,1),soln0(i,2),solp0(i,2)
   50 CONTINUE
c     Print number of residual evaluations for index 1 and index 0
c     problems.
      WRITE (*,90) kount1,kount0
      END
  
      SUBROUTINE ddassf1 (t, y, yprime, delta, d1, ldd, cj, ires, rwork,
     & iwork)
c 
c     Routine for the Gear index=2 problem.
c     One equation is differentiated to reduce it to index 1,
c     with a constraint on the index 2 equation.
      DOUBLE PRECISION t,y(*),yprime(*),delta(*),d1(ldd,*),cj,rwork(*),
     & eta,one, two, zero 
      INTEGER ires,iwork(*),ldd
      INTEGER kount0,kount1
      COMMON /counts/ kount0,kount1
      one=1.d0
      two=2.d0
      zero=0.d0
      eta=10.d0
c This is the setup call.
      IF (ires.eq.0) THEN
        kount1=0
      END IF
  
c The system residual value.
      IF (ires.eq.1) THEN
        delta(1)=yprime(1)+eta*t*yprime(2)+(one+eta)*y(2)-sin(t)
c This second equation comes from differentiating the second equation in
c section C.1, and subtracting the result from the first equation.
        delta(2)=y(2)-two*sin(t)
c Count function evaluations.
        kount1=kount1+1
      END IF
  
c The partial of the iteration matrix with respect to y.  This is an
c index 1 system.  d1 is set to 0 prior to all calls here.  Partials
c are based on equations above.  Note that \partial y'_i / y_i is c_j.
      IF (ires.eq.2) THEN
        d1(1,1)=cj
        d1(1,2)=(one+eta)+cj*eta*t
        d1(2,2)=one
      END IF
  
c The constraining equation after the corrector has converged.
c Both partials and residuals are required.  This is for the second
c equation in C.1.
      IF (ires.eq.5) THEN
        d1(3,1)=one
        d1(3,2)=eta*t
        delta(3)=y(1)+eta*t*y(2)-cos(t)
      END IF
c The values of IRES=6,7 and 8 occur for the starting procedure
c that solves for y'.  First the partials with respect to  t of what
c is computedb when ires is 1.  Here and below we are computing
c partials of f not of the iteration matrix.
      IF (ires.eq.6) THEN
        delta(1)=eta*yprime(2)-cos(t)
        delta(2)= -two*cos(t)
      END IF
c Compute the partial w.r.t y' of the equations defined when IRES=1
      IF (ires.eq.7) THEN
        d1(1,1)=one
        d1(1,2)=eta*t
      END IF
c Compute the partial w.r.t y of the equations defined when IRES=1
      IF (ires.eq.8) THEN
        d1(1,2)=one+eta
        d1(2,2)=one
      END IF
      END
  
      SUBROUTINE ddassf0 (t, y, yprime, delta, d0, ldd, cj, ires, rwork,
     & iwork)
c 
c     Routine for the Gear index=2 problem.
c     One equation is differentiated twice to reduce it to index 0.
c     This gives constraints on the index 2 and index 1 equations.
      DOUBLE PRECISION t,y(*),yprime(*),delta(*),d0(ldd,*),cj,rwork(*),
     & eta, one, two, zero
      INTEGER i,ires,iwork(*),j,ldd
      INTEGER kount0,kount1
      COMMON /counts/ kount0,kount1
      one=1.d0
      two=2.d0
      zero=0.d0
      eta=10.d0
c This is the setup call.
      IF (ires.eq.0) THEN
        kount0=0
      END IF
  
c The system residual value.
      IF (ires.eq.1) THEN
        delta(1)=yprime(1)+eta*t*yprime(2)+(one+eta)*y(2)-sin(t)
        delta(2)=yprime(2)-two*cos(t)
c Count function evaluations.
        kount0=kount0+1
      END IF
  
c The mixed partial derivative matrix.
c This is an index 0 system.
      IF (ires.eq.2) THEN
        d0(1,1)=cj
        d0(1,2)=(one+eta)+cj*eta*t
        d0(2,2)=cj
      END IF
  
c The constraining equations after the corrector has converged.
c Both partials and residuals are required.
      IF (ires.eq.5) THEN
        d0(3,1)=one
        d0(3,2)=eta*t
        d0(4,1)=zero
        d0(4,2)=one
        delta(3)=y(1)+eta*t*y(2)-cos(t)
        delta(4)=y(2)-two*sin(t)
      END IF
c The partial w.r.t y' for the starting procedure
c Since this is an index 0 system the cases IRES=6,8 will not occur
      IF (ires.eq.7) THEN
        d0(1,1)=one
        d0(1,2)=eta*t
        d0(2,2)=one
      END IF
      END
  
