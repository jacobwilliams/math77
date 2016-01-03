      PROGRAM drddasl3
C>> 2009-10-28 DRDDASL3 Remove implicit none.
C>> 2009-10-21 DRDDASL3 Hanson/Krogh Fixed initialization.
C>> 2008-09-04 DRDDASL3 Hanson added starting computation of y'
C>> 2008-08-26 DRDDASL3 Hanson added row dimensions to evaluators
C>> 2001-10-11 DRDDASL3  R. J. Hanson Example 3 Code, with Download
  
C     Solve a planar pendulum problem in rectangular coordinates.
C     The equation is transformed from "Index 3" to "Index 1"
C     by differentiating the length constraint twice.  But
C     this results in a (non-physical) drift away from the constraint
C     for the length.  This example shows this drift at t=100.  The prob
C     is fixed in Example DRDDASL5 by providing information that
C     enables the integrator to move back onto the constraints.
  
C--D replaces "?": DR?DASL3, ?DASLS, ?DASLX, ?DASSF3, ?COPY, ?ROTG
C Version 6, Intel 10.1
      EXTERNAL ddassf3,d1mach,ddasls
      INTEGER ndig,nepoch,neq,maxcon,lrw,liw,ldc,ltd
C     Set number of equations:
      PARAMETER  (neq=5)
C     Set number of constraints.
      PARAMETER  (maxcon=0)
C     Work space sizes:
      PARAMETER  (liw=30+neq)
      PARAMETER  (lrw=45+(5+2*maxcon+4)*neq+neq**2)
      PARAMETER  (ldc=2*neq,ltd=neq)
      DOUBLE PRECISION tol,zero,d1mach
C++S Default NDIG = 5
C++  Default NDIG = 9
C++ Substitute for NDIG below
      PARAMETER  (ndig=9)
      PARAMETER  (tol=10.d0**(-ndig))
      PARAMETER  (zero=0.d0)
      INTEGER i,info(16),idid,iwork(liw)
      DOUBLE PRECISION t,y(neq),yprime(neq),tout,rtol(neq),atol(neq),
     & rwork(lrw),length,drl,drv,ftol,rnktol,c(ldc,ltd)
      LOGICAL constraint
      INTEGER kr,kf,ks
      COMMON /counts/ kr,kf,ks
   50 FORMAT (20x,'Example Results for a Simple Pendulum Problem')
   60 FORMAT (/8x,'T',12x,'y_1',11x,'y_2',11x,'y_3',11x,'y_4',11x,'y_5'/
     & 1p,6d14.4/)
   70 FORMAT (5x,'No. Residual Evaluations',6x,'Factorizations',6x,
     &'No. Projection Solves-'/18x,i6,15x,i6,17x,i6/)
   80 FORMAT('At the time value',f6.2,2x,'the integration was stopped.')
   90 FORMAT ('With no constraints, the length is expected to drift.',
     &'  The error',' in the'/'length and its derivative /',
     &' (error tolerance) are:',3x,2f8.2)
  100 FORMAT (6x,
     &'The pendulum length and its variation have small errors.')
  
C     Tolerances:
      DO 10 i=1,neq
        atol(i)=tol
        rtol(i)=tol
   10 CONTINUE
  
C     Setup options:
      DO 20 i=1,16
        info(i)=0
   20 CONTINUE

C     Compute the initial value of YPRIME(*):
C     Base computation of the initial y' on the
C     report by Krogh, Hanson, (2008), "Solving
C     Constrained Differential-Algebraic Systems
C     Using Projections," (8/2008).
      t=0
C     Tolerances for a small F and a rank tolerance:
      ftol=d1mach(4)**(2./3.)
      rnktol=ftol
C     Provide partials in dense format.      
      info(5)=2
c     Assign initial y, and guess for y', then get initial y'.
      do 25 i = 1, neq
        y(i) = 0.D0
        yprime(i) = 0.D0
 25   continue
c (Actual initial values for y(1) is set in ddassf3.)      
      CALL ddasls (ddassf3, neq, t, y, yprime, info, ftol, rnktol, c,
     & ldc, neq, idid, rwork, lrw, iwork, liw)

C     Allow more than nominal number of steps.
      info(12)=50000
      length=1.1d0
C     Use partial derivatives provided in DDASSF3, and do our own
C     linear algebra.
      info(5)=8  
      DO 30 i=1,10
C     Integrate from T=I-1 to TOUT=T+1.  Final TOUT=10.
        t=i-1
        tout=t+1
        CALL ddaslx (ddassf3, neq, t, y, yprime, tout, info, rtol, atol,
     &    idid, rwork, lrw, iwork, liw)
C  Compute residuals on length and its variation.  They
C  should be smaller than the tolerances used.  Relatively
C  soon the constraints are not satisfied.
        drl=(sqrt(y(1)**2+y(2)**2)-length)/(rtol(1)*length+atol(1))
        drv=(y(1)*y(3)+y(2)*y(4))/(rtol(1)*(abs(y(1)*y(3))+
     &  abs(y(1)*y(4)))+atol(1))
        constraint=abs(drl).le.1.d0.and.abs(drv).le.1.d0
        IF (.not.constraint) GO TO 40
   30 CONTINUE
   40 CONTINUE
      WRITE (*,50)
      WRITE (*,60) tout,y
      WRITE (*,70) kr,kf,ks
      WRITE (*,80) tout
      IF (.not.constraint) THEN
        WRITE (*,90) drl,drv
      ELSE
        WRITE (*,100)
      END IF
  
      END
  
      SUBROUTINE ddassf3 (t, y, yprime, delta, d, ldd, cj, ires, rwork,
     & iwork)
C 
C     Routine for the swinging simple pendulum problem.
C     Conservation of length is not enforced.
      DOUBLE PRECISION t,y(*),yprime(*),delta(*),d(ldd,5),cj,rwork(*),
     & dc(4),ds(4),u,v,lsq,mg,tj,yt(5)
      INTEGER ires,iwork(*),ldd
      LOGICAL factor
  
      DOUBLE PRECISION one,two,zero,mass,length,gravity
      PARAMETER  (one=1.d0,two=2.d0,zero=0.d0,mass=1.d0,length=1.1d0,
     & gravity=9.806650d0)
      SAVE  mg,lsq,yt,factor,tj,dc,ds
      INTEGER kr,kf,ks
      COMMON /counts/ kr,kf,ks
  
C This is the setup call.
      IF (ires.eq.0) THEN
        mg=mass*gravity
        lsq=length**2
        y(1)=length
        kr=0
        kf=0
        ks=0
      END IF
  
C The sytem residual value.
      IF (ires.eq.1) THEN
        delta(1)=y(3)-yprime(1)
        delta(2)=y(4)-yprime(2)
        delta(3)=-y(1)*y(5)-mass*yprime(3)
        delta(4)=-y(2)*y(5)-mass*yprime(4)-mg
        delta(5)=mass*(y(3)**2+y(4)**2)-mg*y(2)-lsq*y(5)
C Count residual evaluations.
        kr=kr+1
      END IF
C The partial derivative matrix.
      IF (ires.eq.2) THEN
C Save value of CJ and Y(*), and YPRIME(*), if needed.  These are
C local variables used in the solving step.
        tj=cj
        CALL dcopy (5, y, 1, yt, 1)
      END IF
C This matrix is factored with five plane rotations.
C Rotations 1 and 2 are identical but in planes
C (1,3) and (2,4).  This example illustrates using
C a special solver for the system.
      IF (ires.eq.3) THEN
        factor=.true.
C     This passes the error code for singularity back to the integrator.
C     It is an issue when the user solves their own linear systems.
        ires=0
      END IF
C Solve the corrector equation.
      IF (ires.eq.4) THEN
        IF (factor) THEN
C Count number of factorization steps.
          kf=kf+1
  
          d(1,1)=-tj
          d(3,1)=-yt(5)
          d(2,2)=-tj
          d(4,2)=-yt(5)
          d(5,2)=-mg
  
          d(1,3)=one
          d(3,3)=-mass*tj
          d(5,3)=two*mass*yt(3)
          d(2,4)=one
          d(4,4)=d(3,3)
  
          d(5,4)=two*mass*yt(4)
          d(3,5)=-yt(1)
          d(4,5)=-yt(2)
          d(5,5)=-lsq
  
          CALL drotg (d(1,1), d(3,1), dc(1), ds(1))
          d(1,3)=dc(1)+ds(1)*d(3,3)
          d(3,3)=-ds(1)+dc(1)*d(3,3)
          d(2,4)=d(1,3)
          d(4,4)=d(3,3)
          d(2,2)=d(1,1)
          d(1,5)=ds(1)*d(3,5)
          d(3,5)=dc(1)*d(3,5)
          d(2,5)=ds(1)*d(4,5)
          d(4,5)=dc(1)*d(4,5)
          d(1,1)=one/d(1,1)
  
C Eliminate now in planes 2 and 5, column 2.
          CALL drotg (d(2,2), d(5,2), dc(2), ds(2))
          u=dc(2)*d(2,3)+ds(2)*d(5,3)
          d(5,3)=-ds(2)*d(2,3)+dc(2)*d(5,3)
          d(2,3)=u
  
          v=dc(2)*d(2,4)+ds(2)*d(5,4)
          d(5,4)=-ds(2)*d(2,4)+dc(2)*d(5,4)
          d(2,4)=v
  
          u=dc(2)*d(2,5)+ds(2)*d(5,5)
          d(5,5)=-ds(2)*d(2,5)+dc(2)*d(5,5)
          d(2,5)=u
  
          d(2,2)=one/d(2,2)
  
C Eliminate in planes 3 and 5, column 3.
          CALL drotg (d(3,3), d(5,3), dc(3), ds(3))
          u=dc(3)*d(3,4)+ds(3)*d(5,4)
          d(5,4)=-ds(3)*d(3,4)+dc(3)*d(5,4)
          d(3,4)=u
  
          v=dc(3)*d(3,5)+ds(3)*d(5,5)
          d(5,5)=-ds(3)*d(3,5)+dc(3)*d(5,5)
          d(3,5)=v
  
          d(3,3)=one/d(3,3)
  
C Eliminate in planes 4 and 5, column 4.
          CALL drotg (d(4,4), d(5,4), dc(4), ds(4))
          u=dc(4)*d(4,5)+ds(4)*d(5,5)
          d(4,4)=one/d(4,4)
          d(5,5)=one/(-ds(4)*d(4,5)+dc(4)*d(5,5))
          d(4,5)=u
C It does not matter if FACTOR is TRUE or FALSE here.
C Things work either way, but with less work when FACTOR = FALSE.,
C since the factorization step is done only when the integrator
C requests it and not each solve step.
          factor=.false.
        END IF
C Count number of solve steps.
        ks=ks+1
C Apply rotations to right-hand side.
        u=dc(1)*delta(1)+ds(1)*delta(3)
        v=dc(1)*delta(2)+ds(1)*delta(4)
        delta(3)=-ds(1)*delta(1)+dc(1)*delta(3)
        delta(4)=-ds(1)*delta(2)+dc(1)*delta(4)
        delta(1)=u
        delta(2)=v
  
        u=dc(2)*delta(2)+ds(2)*delta(5)
        delta(5)=-ds(2)*delta(2)+dc(2)*delta(5)
        delta(2)=u
  
        u=dc(3)*delta(3)+ds(3)*delta(5)
        delta(5)=-ds(3)*delta(3)+dc(3)*delta(5)
        delta(3)=u
  
        u=dc(4)*delta(4)+ds(4)*delta(5)
        delta(5)=-ds(4)*delta(4)+dc(4)*delta(5)
        delta(4)=u
C Back substitute:
        delta(5)=delta(5)*d(5,5)
        delta(4)=delta(4)-delta(5)*d(4,5)
        delta(3)=delta(3)-delta(5)*d(3,5)
        delta(2)=delta(2)-delta(5)*d(2,5)
        delta(1)=delta(1)-delta(5)*d(1,5)
  
        delta(4)=delta(4)*d(4,4)
        delta(3)=delta(3)-delta(4)*d(3,4)
        delta(2)=delta(2)-delta(4)*d(2,4)
        delta(1)=delta(1)-delta(4)*d(1,4)
  
        delta(3)=delta(3)*d(3,3)
        delta(2)=delta(2)-delta(3)*d(2,3)
        delta(1)=delta(1)-delta(3)*d(1,3)
  
        delta(2)=delta(2)*d(2,2)
        delta(1)=delta(1)-delta(2)*d(1,2)
  
        delta(1)=delta(1)*d(1,1)
      END IF
  
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
  
      RETURN
      END
  
