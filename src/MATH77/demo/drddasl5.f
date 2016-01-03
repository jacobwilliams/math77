      PROGRAM drddasl5
c>> 2008-10-26 DRDDASL5 Krogh Moved Format statements up for C conv.
c>> 2008-10-24 DRDDASL5 Krogh Removed in INCLUDE statement & cDEC$...
C>> 2008-08-27 DRDDASL5  solve for initial y' value, change user codes
C>> 2006-12-18 DRDDASL5  add total energy as a constraint.
C>> 2001-10-11 DRDDASL5  R. J. Hanson Example 5 Code, with Download
  
C     Solve a planar pendulum problem in rectangular coordinates.
C     The equation is transformed from "Index 3" to "Index 1"
C     by differentiating the length constraint twice.  The system
C     is integrated (using DDASSFA) using these three constraints,
C     including total energy.
C     In the second integration the problem is transformed from
C     "Index 3" to "Index 0" by differentiating the length constraint
C     three times.  The routine DDASSFB uses four constraints,
C     including total energy.
C     This example shows that the constraints remain satisfied,
C     and the integration can succeed with either approach.
C     It is more efficient to use the "Index 0" problem than
C     the "Index 1" problem.
C--D replaces "?": DR?DASL5, ?DASLS, ?DASLX, ?DASSFA, ?DASSFB
  
      EXTERNAL ddassfa,ddassfb,d1mach,ddasls
C     Set number of equations:
      INTEGER neq
      PARAMETER  (neq=5)
C     Set maximum number of constraints.
      INTEGER maxcon
      PARAMETER  (maxcon=4)
C     Work space sizes:
      INTEGER liw,lrw,ldc
      PARAMETER  (liw=30+neq)
      PARAMETER  (lrw=45+(5+2*maxcon+4)*neq+2*neq**2)
      PARAMETER  (ldc=2*neq)
      INTEGER ndig,kk
      DOUBLE PRECISION tol
C++S Default NDIG = 4
C++  Default NDIG = 11
C++ Substitute for NDIG below
      PARAMETER  (ndig=11)
      PARAMETER  (tol=10.d0**(-ndig))
  
      INTEGER i,info(16),idid,iwork(liw)
      DOUBLE PRECISION t,y(neq),yprime(neq),tout,rtol(neq),atol(neq),
     & rwork(lrw),length,drl,drv,c(ldc,neq),ftol,rnktol,
     & d1mach
  
      LOGICAL constraint
      INTEGER kr,kf,kc
      COMMON /counts/ kr,kf,kc

   70 FORMAT (14x,'Example Results for a Constrained Pendulum Problem, I
     &ndex 1')
   80 FORMAT (14x,'Initial Position and Derivative Values using DDASLS')
   90 FORMAT (6x,'At the time value',f10.2,2x,'the integration was stopp
     &ed.')
  100 FORMAT ('The pendulum length or variation has large weighted error
     &s.'/'These should remain less than 1 is magnitude:',10x,2f8.2/)
  110 FORMAT (14x,'Example Results for a Constrained Pendulum Problem, I
     &ndex 0')
  120 FORMAT (5x,'No. of Residual Evaluations',6x,'Factorizations',6x,'P
     &rojection Solves'/'Constraint Partials-',1x,i8,12x,i8,17x,i8/)
  130 FORMAT (/8x,'T',10x,'y_1',09x,'y_2',09x,'y_3',09x,'y_4',09x,'y_5'/
     &1p,6d12.4/)
  140 FORMAT (/8x,'T',09x,'y''_1',08x,'y''_2',08x,'y''_3',08x,'y''_4',
     & 08x,'y''_5'/1p,6d12.4/)     
  
      WRITE (*,70)
  
C     Tolerances:
      DO 10 i=1,neq
        atol(i)=tol
        rtol(i)=tol
   10 CONTINUE
C     Setup options:
      DO 20 i=1,16
        info(i)=0
   20 CONTINUE
C     Use partial derivatives provided in user code:
      info(5)=2
  
C     Constrain solution, forward with 3 constraints:
      info(10)=3
  
C     Compute the initial value of YPRIME(*).
C     Base computation of the initial y' on the
C     report by Krogh, Hanson, (2008), "Solving
C     Constrained Differential-Algebraic Systems
C     Using Projections," (8/2008).
      t=0
C     Tolerances for a small F and a rank tolerance:
      ftol=d1mach(4)**(2./3.)
      rnktol=ftol
C     Give starting values to y and y' before Newton method:
      do 25 i = 1, neq
        y(i) = 0.D0
        yprime(i) = 0.D0
 25   continue
c     (Initial value for y(1) reset when ires is 0)      
      CALL ddasls (ddassfa, neq, t, y, yprime, info, ftol, rnktol, c,
     & ldc, neq, idid, rwork, lrw, iwork, liw)
      WRITE (*,80)
C     Write the computed values for initial position and derivatives.      
      WRITE (*,130) t,y
      WRITE (*,140) t,yprime
  
C     Now have initial values of y' so no need to compute them.
C     Allow more than nominal number of steps.
      info(12)=50000
C     This is the pendulum length
      length=1.1d0
C     This is how long we will integrate.
      kk=1000
      DO 30 i=1,kk,10
C     Integrate from T=I-1 to TOUT=T+10.
C     If the solution drifts away from the constraints, stop.
        t=i-1
        tout=t+10
        CALL ddaslx (ddassfa, neq, t, y, yprime, tout, info, rtol, atol,
     &    idid, rwork, lrw, iwork, liw)
C  Compute residuals on length and its variation.  They
C  should be smaller than the tolerances used.
        drl=(y(1)**2+y(2)**2-length**2)/(rtol(1)*length**2+atol(1))
        drv=(y(1)*y(3)+y(2)*y(4))/(rtol(1)*(abs(y(1)*y(3))+
     &       abs(y(1)*y(4)))+atol(1))
        constraint=abs(drl).le.1.d0.and.abs(drv).le.1.d0
        IF (.not.constraint) GO TO 40
   30 CONTINUE
   40 CONTINUE
      WRITE (*,130) tout,y
      WRITE (*,120) kr,kf,kc
      WRITE (*,90) tout
      IF (.not.constraint) THEN
        WRITE (*,100) drl,drv
      END IF
  
C     Start the integration over for the index 0 problem.
      info(1)=0
C     Set the number of constraints to 4.
      info(10)=4
C     Starting values to y and y' before Newton method:
      do 45 i = 1, neq
        y(i) = 0.D0
        yprime(i) = 0.D0
 45   continue
c     (Initial value for y(1) and yprime(4) reset when ires is 0)
      DO 50 i=1,kk,10
C     Integrate from T=I-1 to TOUT=T+10.
        t=i-1
        tout=t+10
        CALL ddaslx (ddassfb, neq, t, y, yprime, tout, info, rtol, atol,
     &    idid, rwork, lrw, iwork, liw)
C  Compute residuals on length and its variation.  They
C  should be smaller than the tolerances used.
        drl=(y(1)**2+y(2)**2-length**2)/(rtol(1)*length**2+atol(1))
        drv=(y(1)*y(3)+y(2)*y(4))/(rtol(1)*(abs(y(1)*y(3))+
     &       abs(y(1)*y(4)))+atol(1))
        constraint=abs(drl).le.1.d0.and.abs(drv).le.1.d0
        IF (.not.constraint) GO TO 60
   50 CONTINUE
   60 CONTINUE
      WRITE (*,110)
      WRITE (*,130) tout,y
      WRITE (*,120) kr,kf,kc
      WRITE (*,90) tout
      IF (.not.constraint) THEN
        WRITE (*,100) drl,drv
      END IF
      END
  
      SUBROUTINE ddassfa (t, y, yprime, delta, d, ldd, cj, ires, rwork,
     & iwork)
C     Routine for the swinging simple pendulum problem,
C     with constraints on the index 2 and 3 equations.
      DOUBLE PRECISION t,y(*),yprime(*),delta(*),d(ldd,5),cj,rwork(*),
     & lsq,mg
      INTEGER ires,iwork(*),ldd
      DOUBLE PRECISION one,two,zero,mass,length,gravity
      PARAMETER  (one=1.d0,two=2.d0,zero=0.d0,mass=1.d0,length=1.1d0,
     & gravity=9.806650d0)
      SAVE  mg,lsq
      INTEGER kr,kf,kc
      COMMON /counts/ kr,kf,kc
C This is the setup call.
      IF (ires.eq.0) THEN
        mg=mass*gravity
        lsq=length**2
        y(1)=length

C This is the only nonzero value for y'_4(0) but the startup
C procedure will compute it.  It is written here for
C reference.
C        YPRIME(4)=-Gravity
        kr=0
        kf=0
        kc=0
      END IF
      mg=mass*gravity
      lsq=length**2
C The system residual value.
      IF (ires.eq.1) THEN
        delta(1)=y(3)-yprime(1)
        delta(2)=y(4)-yprime(2)
        delta(3)=-y(1)*y(5)-mass*yprime(3)
        delta(4)=-y(2)*y(5)-mass*yprime(4)-mg
        delta(5)=mass*(y(3)**2+y(4)**2)-mg*y(2)-lsq*y(5)
C Count residual evaluations.
        kr=kr+1
      END IF
  
C The mixed partial derivative matrix DF/Dy + c * DF/Dy'
      IF (ires.eq.2) THEN
        d(1,1)=-cj
        d(3,1)=-y(5)
  
        d(2,2)=-cj
        d(4,2)=-y(5)
        d(5,2)=-mg
  
  
        d(1,3)=one
        d(3,3)=-mass*cj
        d(5,3)=two*mass*y(3)
  
        d(2,4)=one
        d(4,4)=d(3,3)
        d(5,4)=two*mass*y(4)
  
        d(3,5)=-y(1)
        d(4,5)=-y(2)
        d(5,5)=-lsq
        kf=kf+1
      END IF
  
C The constraining equations after the corrector has converged.
C Both partials and residuals are required.
      IF (ires.eq.5) THEN
        d(6,1)=y(1)*two
        d(6,2)=y(2)*two
        d(6,3)=zero
        d(6,4)=zero
        d(6,5)=zero
  
        d(7,1)=y(3)
        d(7,2)=y(4)
        d(7,3)=y(1)
        d(7,4)=y(2)
        d(7,5)=zero
  
C Constraining total energy -
        d(8,1)=zero
        d(8,2)=mg
        d(8,3)=mass*y(3)
        d(8,4)=mass*y(4)
        d(8,5)=zero
        delta(6)=y(1)**2+y(2)**2-lsq
        delta(7)=y(1)*y(3)+y(2)*y(4)
        delta(8)=0.5d0*mass*(y(3)**2+y(4)**2)+mg*y(2)
        kc=kc+1
      END IF
  
C  What follows are Partials w.r.t. T, Y' and Y, needed for
C  computing the starting values of y' based on the
C  Krogh/Hanson starting algorithm.
C  These values (6,7,8) of IRES occur only for use in
C  the computation of initial values for y'.
      IF (ires.eq.6) THEN
C  This is the partial of F, the residual function, w.r.t. T.
C  There is no explicit time dependence so this is zero.
C  The contents of DELTA(:) are set zero by the calling program
C  so there is nothing to do.
C        DELTA(1:6)=ZERO
      END IF
  
C  This is the partial of F w.r.t. y'.
C  Values not assigned are set zero by the calling program.
      IF (ires.eq.7) THEN
        d(1,1)=-one
        d(2,2)=-one
        d(3,3)=-mass
        d(4,4)=-mass
        kf=kf+1
      END IF
  
C  This is the partial of F w.r.t. y.
C  Values not assigned are set zero by the calling program.
      IF (ires.eq.8) THEN
        d(3,1)=-y(5)
  
        d(4,2)=-y(5)
        d(5,2)=-mg
  
  
        d(1,3)=one
        d(5,3)=two*mass*y(3)
  
        d(2,4)=one
        d(5,4)=two*mass*y(4)
  
        d(3,5)=-y(1)
        d(4,5)=-y(2)
        d(5,5)=-lsq
        kf=kf+1
  
      END IF
      END
  
      SUBROUTINE ddassfb (t, y, yprime, delta, p, ldp, cj, ires, rwork,
     & iwork)
C 
C     Routine for the swinging simple pendulum problem,
C     with constraints on the index 1,2 amd 3 equations.
C     Use P(:,:) to distinguish from D(:,:) in routine DDASSFA.
  
      DOUBLE PRECISION t,y(*),yprime(*),delta(*),p(ldp,5),cj,rwork(*),
     & lsq,mg
      INTEGER ires,iwork(*),ldp
      DOUBLE PRECISION one,two,three,zero,mass,length,gravity
      PARAMETER  (one=1.d0,two=2.d0,three=3.d0,zero=0.d0,mass=1.d0,
     & length=1.1d0,gravity=9.806650d0)
      SAVE  mg,lsq
      INTEGER kr,kf,kc
      COMMON /counts/ kr,kf,kc
C This is the setup call.
      IF (ires.eq.0) THEN
        mg=mass*gravity
        lsq=length**2
        y(1)=length
        yprime(4)=-gravity
        kr=0
        kf=0
        kc=0
      END IF
  
C The sytem residual value.
      IF (ires.eq.1) THEN
        delta(1)=y(3)-yprime(1)
        delta(2)=y(4)-yprime(2)
        delta(3)=-y(1)*y(5)-mass*yprime(3)
        delta(4)=-y(2)*y(5)-mass*yprime(4)-mg
        delta(5)=-three*mg*y(4)-lsq*yprime(5)
        kr=kr+1
      END IF
  
C The partial derivative matrix.  Entries are all zero
C so only non-zero values are needed.
      IF (ires.eq.2) THEN
        p(1,1)=-cj
        p(3,1)=-y(5)
  
        p(2,2)=-cj
        p(4,2)=-y(5)
  
        p(1,3)=one
        p(3,3)=-mass*cj
  
        p(2,4)=one
        p(4,4)=p(3,3)
        p(5,4)=-three*mg
  
        p(3,5)=-y(1)
        p(4,5)=-y(2)
        p(5,5)=-lsq*cj
        kf=kf+1
      END IF
  
C The constraining equations after the corrector has converged.
C Both partials and residuals are required.
      IF (ires.eq.5) THEN
        p(6,1)=y(1)*two
        p(6,2)=y(2)*two
        p(6,3)=zero
        p(6,4)=zero
        p(6,5)=zero
  
        p(7,1)=y(3)
        p(7,2)=y(4)
        p(7,3)=y(1)
        p(7,4)=y(2)
        p(7,5)=zero
  
        p(8,1)=zero
        p(8,2)=-mg
        p(8,3)=two*mass*y(3)
        p(8,4)=two*mass*y(4)
        p(8,5)=-lsq
C Constraining total energy -
        p(9,1)=zero
        p(9,2)=mg
        p(9,3)=mass*y(3)
        p(9,4)=mass*y(4)
        p(9,5)=zero
        delta(6)=y(1)**2+y(2)**2-lsq
        delta(7)=y(1)*y(3)+y(2)*y(4)
        delta(8)=mass*(y(3)**2+y(4)**2)-mg*y(2)-lsq*y(5)
        delta(9)=0.5d0*mass*(y(3)**2+y(4)**2)+mg*y(2)
        kc=kc+1
      END IF
      END
