      PROGRAM drsdasl2
c>> 2009-10-27 DRSDASL2 Krogh Declared DUM(4,*) for NAG compiler.
c>> 2009-10-19 DRSDASL2 Krogh Changed def. of d(4,4) so 0 doesn't abort.
c>> 2008-10-26 DRSDASL2 Krogh Moved Format statements up for C conv.
c>> 2008-10-24 DRSDASL2 Krogh Removed in INCLUDE statement & cDEC$...
C>> 2008-09-04 DRSDASL2 Hanson added starting computation of y'
C>> 2008-08-26 DRSDASL2 Hanson added row dimensions to evaluators
C>> 2001-10-11 DRSDASL2  R. J. Hanson Example 2 Code, with Download
 
C     Solve Enright and Pryce stiff test problem E5.
C     The equation is presented as y'= F(t,y), y_0 given.
C     This is solved by defining the residual function
C     f(t,y,y')=F(t,y)-y'.  Integration is done twice.
 
C     The first time analytic partials are provided.
 
C     The second time, difference quotients are used for
C     partials, with the integrator requesting only values
C     of f(t,y,y').  The two solutions are equivalent but
C     not exactly equal.
 
C--S replaces "?": DR?DASL2, ?DASLS, ?DASLX, ?DASSF2, ?DASSF3, ?COPY
C-- & ?ROTG
      EXTERNAL sdassf2,sdassf3
      INTEGER ndig,nepoch,neq,maxcon,ldc,ltd,lrw,liw
C     Set number of equations:
      PARAMETER  (neq=4)
C     Set number of constraints.
      PARAMETER  (maxcon=0)
C     Work space sizes:
      PARAMETER  (liw=30+neq)
      PARAMETER  (lrw=45+(5+2*maxcon+4)*neq+neq**2)
      PARAMETER  (ldc=neq,ltd=neq)
      REAL             tol
C++S Default NDIG = 3
C++  Default NDIG = 8
C++ Substitute for NDIG below
      PARAMETER  (ndig=3,nepoch=10)
      PARAMETER  (tol=10.e0**(-ndig))
      INTEGER i,j,info(16),idid,iwork(liw)
      REAL             t,y(neq),yprime(neq),rtol(neq),atol(neq),
     & rwork(lrw),tout(nepoch),y1(neq,nepoch),y2(neq,nepoch)
      REAL             c(ldc,ltd),ftol,rnktol
      LOGICAL match
      INTEGER kf2,ks2,kf3,ks3
      COMMON /counts/ kf2,ks2,kf3,ks3
   90 FORMAT (/20x,'Example Results for a Stiff ODE Problem, E5')
  100 FORMAT (6x,'Integration Values do not Match')
  110 FORMAT (/15x,'No. of Residual Evaluations',6x,'No. of User Solves'
     & /' Partials Provided-',8x,i4,24x,i4)
  120 FORMAT (' Divided Differences-',6x,i4,24x,i4)
  130 FORMAT (/10x,'T',7x,'RErr in y_1',3x,'RErr in y_2',3x,'RErr in y_3
     & ',3x,'RErr in y_4')
  140 FORMAT (1p,5d14.4)
 
C     Tolerances:
      DO 10 i=1,neq
        atol(i)=tol
        rtol(i)=tol
   10 CONTINUE
C     Setup options:
      DO 20 i=1,16
        info(i)=0
   20 CONTINUE
C Compute, factor and solve the partial derivative matrix in routine
C SDASSF2. This illustrates how to completely control the linear
C algebra. Using analytic partial derivatives.
      info(5)=5
C     Compute the initial value of YPRIME(*):
      t=0
      ftol=tol
      rnktol=tol
C     Give starting values to y and y' before Newton method:
      do 25 i = 1, neq
        y(i) = 0.E0
        yprime(i) = 0.E0
 25   continue
      y(1) = 1.76E-3
      CALL sdasls (sdassf2, neq, t, y, yprime, info, ftol, rnktol, c,
     & ldc, ltd, idid, rwork, lrw, iwork, liw)
C     Set output points for integration:
      tout(1)=0.e0
      tout(2)=1.e-3
      DO 30 i=3,nepoch
        tout(i)=10*tout(i-1)
   30 CONTINUE
      DO 40 i=2,nepoch
        t=tout(i-1)
        CALL sdaslx (sdassf2, neq, t, y, yprime, tout(i), info, rtol,
     &   atol, idid, rwork, lrw, iwork, liw)
        CALL scopy (neq, y, 1, y1(1,i), 1)
   40 CONTINUE
C     Compute the initial value of YPRIME(*):
      t=0
      ftol=tol
      rnktol=tol
C     Give starting values to y and y' before Newton method:
      do 45 i = 1, neq
        y(i) = 0.E0
        yprime(i) = 0.E0
 45   continue
      y(1) = 1.76E-3
      CALL sdasls (sdassf3, neq, t, y, yprime, info, ftol, rnktol, c,
     & ldc, ltd, idid, rwork, lrw, iwork, liw)
C     Restart the integration, but do not use analytic derivatives.
      info(1)=0
C     Use divided differences instead of user-provided partials.
C     And the default linear solver provided.
      info(5)=1
      DO 50 i=2,nepoch
        t=tout(i-1)
        CALL sdaslx (sdassf3, neq, t, y, yprime, tout(i), info, rtol,
     &   atol, idid, rwork, lrw, iwork, liw)
        CALL scopy (neq, y, 1, y2(1,i), 1)
   50 CONTINUE
 
C See if the solutions "match" the requested tolerances.
      match=.true.
      DO 70 j=2,nepoch
        DO 60 i=1,neq
          y2(i,j)=(y2(i,j)-y1(i,j))/(abs(y1(i,j))*rtol(i)+atol(i))
          match=match.and.(abs(y2(i,j)).le.100.e0)
   60   CONTINUE
   70 CONTINUE
C Output the relative errors and a summary.
      WRITE (*,90)
      WRITE (*,130)
      DO 80 j=2,nepoch
        WRITE (*,140) tout(j),(y2(i,j),i=1,neq)
   80 CONTINUE
 
      IF (match) THEN
        WRITE (*,110) kf2,ks2
        WRITE (*,120) kf3,ks3
      ELSE
        WRITE (*,100)
      END IF
      END
 
      SUBROUTINE sdassf2 (t, y, yprime, delta, dum, ldd, cj, ires,
     & rwork, iwork)
C
C     Routine for the Enright and Pryce problem E5.
C     DUM(*,*) is not used in this example.
      REAL             t,y(*),yprime(*),delta(*),dum(ldd,*),cj,rwork(*),
     & dc(3),ds(3),u,b0,b1,b2,b3,b4
      REAL             d(4,4)
      INTEGER i,ires,iwork(*),ldd
      SAVE  b0,b1,b2,b3,b4,d,dc,ds
      INTEGER kf2,ks2,kf3,ks3
      COMMON /counts/ kf2,ks2,kf3,ks3
 
C This is the setup call.
      IF (ires.eq.0) THEN
        b0=1.76e-03
        b1=7.89e-10
        b2=1.1e+07
        b3=1.13e+09
        b4=1.13e+03
C Set initial conditions.
        y(1)=0.e0
        CALL scopy (4, y, 0, y, 1)
        y(1)=b0
C Count evaluations in KF2 and KS2.
        kf2=0
        ks2=0
        RETURN
      END IF
 
C The sytem residual value:
      IF (ires.eq.1) THEN
        delta(1)=-b1*y(1)-b2*y(1)*y(3)-yprime(1)
        delta(2)=b1*y(1)-b3*y(2)*y(3)-yprime(2)
        delta(3)=b1*y(1)-b2*y(1)*y(3)+b4*y(4)-b3*y(2)*y(3)-yprime(3)
        delta(4)=b2*y(1)*y(3)-b4*y(4)-yprime(4)
        kf2=kf2+1
        RETURN
      END IF
C The partial derivative matrix:
      IF (ires.eq.2) THEN
        d(4,1)=b2*y(3)
        d(1,1)=-b1-d(4,1)-cj
        d(2,1)=b1
        d(3,1)=b1-d(4,1)
 
        d(1,2)=0.e0
        d(3,2)=-b3*y(3)
        d(2,2)=d(3,2)-cj
        d(4,2)=0.e0
 
        d(1,3)=-b2*y(1)
        d(2,3)=-b3*y(2)
        d(3,3)=d(1,3)+d(2,3)-cj
        d(4,3)=-d(1,3)
 
        d(1,4)=0.e0
        d(2,4)=0.e0
        d(3,4)=b4
        d(4,4)=-b4-cj
 
C This matrix is right factored to lower triangular for with three
C plane rotations.  Use planes 1 and 3:
 
 
C Tell integrator system is non-singular.
        ires=0
 
        CALL srotg (d(1,1), d(1,3), dc(1), ds(1))
 
        u=dc(1)*d(2,1)+ds(1)*d(2,3)
        d(2,3)=-ds(1)*d(2,1)+dc(1)*d(2,3)
        d(2,1)=u
 
        u=dc(1)*d(3,1)+ds(1)*d(3,3)
        d(3,3)=-ds(1)*d(3,1)+dc(1)*d(3,3)
        d(3,1)=u
 
        u=dc(1)*d(4,1)+ds(1)*d(4,3)
        d(4,3)=-ds(1)*d(4,1)+dc(1)*d(4,3)
        d(4,1)=u
        d(1,1)=1.e0/d(1,1)
C Eliminate in planes 2 and 3.
        CALL srotg (d(2,2), d(2,3), dc(2), ds(2))
 
        u=dc(2)*d(3,2)+ds(2)*d(3,3)
        d(3,3)=-ds(2)*d(3,2)+dc(2)*d(3,3)
        d(3,2)=u
 
        u=dc(2)*d(4,2)+ds(2)*d(4,3)
        d(4,3)=-ds(2)*d(4,2)+dc(2)*d(4,3)
        d(4,2)=u
        d(2,2)=1.e0/d(2,2)
 
C Eliminate in planes 3 and 4.
        CALL srotg (d(3,3), d(3,4), dc(3), ds(3))
        u=dc(3)*d(4,3)+ds(3)*d(4,4)
        d(4,4)=(-ds(3)*d(4,3)+dc(3)*d(4,4))
        if (d(4,4) .eq. 0.E0) then
          ires = 1
          return
        end if
        d(4,4) = 1.e0 / d(4,4)
        d(4,3)=u
        d(3,3)=1.e0/d(3,3)
 
        RETURN
      END IF
 
C Solve the corrector equation.
      IF (ires.eq.4) THEN
C Count number of solves.
        ks2=ks2+1
 
C Forward substitute:
        delta(1)=delta(1)*d(1,1)
        delta(4)=delta(4)-delta(1)*d(4,1)
        delta(3)=delta(3)-delta(1)*d(3,1)
        delta(2)=delta(2)-delta(1)*d(2,1)
 
        delta(2)=delta(2)*d(2,2)
        delta(4)=delta(4)-delta(2)*d(4,2)
        delta(3)=delta(3)-delta(2)*d(3,2)
 
        delta(3)=delta(3)*d(3,3)
        delta(4)=delta(4)-delta(3)*d(4,3)
        delta(4)=delta(4)*d(4,4)
 
C Apply three plane rotations,(3,4),(2,3),(1,3).
        u=dc(3)*delta(3)-ds(3)*delta(4)
        delta(4)=ds(3)*delta(3)+dc(3)*delta(4)
        delta(3)=u
 
        u=dc(2)*delta(2)-ds(2)*delta(3)
        delta(3)=ds(2)*delta(2)+dc(2)*delta(3)
        delta(2)=u
 
        u=dc(1)*delta(1)-ds(1)*delta(3)
        delta(3)=ds(1)*delta(1)+dc(1)*delta(3)
        delta(1)=u
        RETURN
      END IF
C This is df/dy' for the starting procedure.  This
C problem is a linear ODE.
      IF (ires.eq.7) THEN
        DO 10 i=1,4
          d(i,i)=-1.e0
   10   CONTINUE
        RETURN
      END IF
 
      END
 
 
      SUBROUTINE sdassf3 (t, y, yprime, delta, d, ldd, cj, ires, rwork,
     & iwork)
C
C     Routine for the Enright and Pryce problem E5.
 
      REAL             t,y(*),yprime(*),delta(*),d(ldd,4),cj,rwork(*),
     & dc(3),ds(3),b1,b2,b3,b4
      INTEGER ldd, ires,iwork(*)
      PARAMETER(b1=7.89e-10, b2=1.1e+07, b3=1.13e+09, b4=1.13e+03)
      INTEGER i,kf2,ks2,kf3,ks3
      COMMON /counts/ kf2,ks2,kf3,ks3
 
C This is the setup call.
      IF (ires.eq.0) THEN
C Count evaluations in KF3 and KS3.
        kf3=0
        ks3=0
      END IF
 
C The sytem residual value:
      IF (ires.eq.1) THEN
        delta(1)=-b1*y(1)-b2*y(1)*y(3)-yprime(1)
        delta(2)=b1*y(1)-b3*y(2)*y(3)-yprime(2)
        delta(3)=b1*y(1)-b2*y(1)*y(3)+b4*y(4)-b3*y(2)*y(3)-yprime(3)
        delta(4)=b2*y(1)*y(3)-b4*y(4)-yprime(4)
        kf3=kf3+1
      END IF
C The partial derivative matrix:
      IF (ires.eq.2) THEN
        d(4,1)=b2*y(3)
        d(1,1)=-b1-d(4,1)-cj
        d(2,1)=b1
        d(3,1)=b1-d(4,1)
 
        d(2,2)=-b3*y(3)-cj
        d(3,2)=d(2,2)
        d(1,3)=-b2*y(1)
        d(2,3)=-b3*y(2)
        d(3,3)=d(1,3)+d(2,3)-cj
        d(4,3)=-d(1,3)
        d(3,4)=b4
        d(4,4)=-b4-cj
      END IF
 
      IF (ires.eq.3) THEN
        WRITE (*,*) 'Should be no call to SDASSF3 with IRES = 3.'
      END IF
 
 
      IF (ires.eq.4) THEN
        WRITE (*,*) 'Should be no call to SDASSF3 with IRES = 4.'
      END IF
C This is df/dy' for the starting procedure.  This
C problem is a linear ODE of index 0.  So only IRES=1,6 occur.
      IF (ires.eq.7) THEN
        DO 10 i=1,4
          d(i,i)=-1.e0
   10   CONTINUE
        RETURN
      END IF
      END
