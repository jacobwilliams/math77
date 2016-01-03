      PROGRAM drsdasl6
c>> 2009-10-21 DRSDASL6 Krogh at do 120 upper limit km => km+1.
c>> 2008-10-26 DRSDASL6 Krogh Moved Format statements up for C conv.
c>> 2008-10-24 DRSDASL6 Krogh Removed an INCLUDE statement & cDEC$...
c>> 2008-08-27 DRSDASL6  assign initial y' value, change user codes
c>> 2001-10-11 DR SDASL6  R. J. Hanson Example 6 Code, with Download
 
c Driver program for the non-linear elliptic or Bratu problem.
c The unknowns are the array entries of the solution stacked up
c as one large vector.  In the evaluation routine, the two-dimensional
c aspect of the problem is used when evaluating residuals and
c also linear system solves.  The linear solves are done using
c the GMRES algorithm.
c--S replaces "?": ?DASLX, ?DASSF6, DR?DASL6
      EXTERNAL sdassf6
      INTEGER ndig
      REAL             tol
c++S Default NDIG = 2
c++  Default NDIG = 6
c++ Substitute for NDIG below
      PARAMETER  (ndig=2)
      PARAMETER  (tol=10.e0**(-ndig))
 
      INTEGER maxord,maxcon,n,neq,liw,lrw
 
c Note that the N here and in routine SDASSF6 must have the same value.
      PARAMETER  (n=40,neq=n**2,liw=30+neq)
c     The size of the work space LRW does not have the NEQ**2=N**4 term
c     because with INFO(5)=5, the Jacobian matrix data is stored in the
c     evaluation program ?DASF (= ?DASF6).
      PARAMETER  (maxord=5,maxcon=0,lrw=45+(maxord+maxcon+4)*neq)
      INTEGER i,j,imax,jmax,info(16),idid,iwork(liw)
      REAL             t,u(n,n),uprime(n,n),rtol(neq),atol(neq),
     & rwork(lrw),tout(11),maxu
      INTEGER ke,kf,ks,kp
      COMMON /counts/ ke,kf,ks,kp
   70 FORMAT (12x,'Example Results for a non-linear Elliptic PDE Problem
     1 '//12x,'Maximum value of solution with N by N system of ODEs')
   80 FORMAT (6x,'u(Imax,Jmax), Imax, Jmax',6x,'N, (N by N grid)',4x,
     1 'T final')
   90 FORMAT (9x,f6.2,4x,i4,2x,i4,7x,i4,16x,f6.2/)
  100 FORMAT (20x,'Number of requested evaluations'/6x,'Residuals',6x,
     1 'Fresh Solve Steps',6x,'Solutions',6x,'GMRES products')
  110 FORMAT (i12,6x,i12,7x,i12,7x,i12)
 
c     Absolute and relative tolerances:
      DO 10 i=1,neq
        atol(i)=tol
        rtol(i)=tol
   10 CONTINUE
 
c     Setup default options:
      DO 20 i=1,16
        info(i)=0
   20 CONTINUE
 
c Change some options from their defaults:
c Partials and solutions are done in SDASSF6 below.
      info(5)=5
c Restrict the BDF formulas to a lower order than 5.
c This is not needed in this example.
      info(9)=4
c Assign the starting values of U(*), UPRIME(*) in the
c initial call to SDASSF6.
 
c Set output points for return of solution.
      tout(1)=0.e0
      DO 30 i=2,11
        tout(i)=tout(i-1)+1.e0
   30 CONTINUE
 
      DO 40 i=2,11
        t=tout(i-1)
        CALL sdaslx (sdassf6, neq, t, u, uprime, tout(i), info, rtol,
     &   atol, idid, rwork, lrw, iwork, liw)
   40 CONTINUE
c Locate maximum value of solution.
c It should be near the centroid of the unit square.
      maxu=u(1,1)
      imax=1
      jmax=1
      DO 60 j=1,n
        DO 50 i=1,n
          IF (u(i,j).gt.maxu) THEN
            maxu=u(i,j)
            imax=i
            jmax=j
          END IF
   50   CONTINUE
   60 CONTINUE
 
c Summarize results and amount of work requested.
      WRITE (*,70)
      WRITE (*,80)
      WRITE (*,90) maxu,imax,jmax,n,tout(11)
      WRITE (*,100)
      WRITE (*,110) ke,kf,ks,kp
      END
 
      SUBROUTINE sdassf6 (t, u, upr, del, d, ldd, cj, ires, rwork,
     & iwork)
c
c     Routine for the non-linear elliptic Bratu problem.
c     This version computes initial data,
c     residuals, and solves the linear systems with GMRES.
c     A diagonal pre-conditioner is used, applied from both
c     sides implicitly. Other functions are left to the SDASLX.
 
c The problem size is N**2 equations.  Up to KM iterations are used
c in the GMRES algorithm.  The results after KM steps are
c returned to the integrator.  (Have KM .le. N**2; usually KM
c will be much smaller.)
      INTEGER n,km
c If N is changed in the calling program, have it the same value
c here.
      PARAMETER  (n=40,km=25)
      REAL             t,u(n,*),upr(n,*),del(n,*),d(ldd,*),cj,rwork(*),
     & u0(n,n),reltol,lambda,hsq3,hx,cc,ss,hsqlam,nrm,rss,rnrm,tol,tt
 
      REAL             rr(0:n+1,0:n+1),xd(n,n)
      REAL             hh(0:km,km+1),qq(n,n,km),ww(n,n)
      REAL             scl(n,n)
      REAL             cs(km),sn(km),y(km)
 
      INTEGER i,j,k,l,ires,iwork(*),ldd
      SAVE  hx,hsq3,lambda,hsqlam,xd,scl,u0,rr,reltol
      INTEGER ke,kf,ks,kp
      COMMON /counts/ ke,kf,ks,kp
C
 
c This is the setup call.
c It is used to compute quantities that need be set
c once during the integration.
      IF (ires.eq.0) THEN
        ke=0
        kf=0
        ks=0
        kp=0
        hx=1.e0/(1.e0+n)
        hsq3=3.e0*hx**2
        lambda=6.e0
        hsqlam=hsq3*lambda
 
c Set initial values for U(*,*) and UPR(*,*):
        DO 20 j=1,n
          DO 10 i=1,n
            u(i,j)=0.e0
            upr(i,j)=lambda
   10     CONTINUE
   20   CONTINUE
 
c Set zero boundary values in array RR(,).
c This makes it possible to use a natural double loop, and
c avoid special code for the edges.
        DO 30 j=0,n+1
          rr(0,j)=0.e0
          rr(n+1,j)=0.e0
          rr(j,n+1)=0.e0
          rr(j,0)=0.e0
   30   CONTINUE
 
c     Relative tolerance for residual norm reduction in GMRES solver.
c     (For a fixed N, "optimize" RELT, ATOL(), RTOL(), and KM,
c      inside SDASSF6.)
        reltol=9d-2
        RETURN
      END IF
 
c The system residual value:
      IF (ires.eq.1) THEN
c Count number of residual evaluations:
        ke=ke+1
c Compute required exponential functions.  Store the U(,) values
c in a bordered array XD(,) so that the special edge condtions
c are accounted for.  The zero edge conditions in RR(,) are used.
 
c (Automatic differentiation can start here.)
        DO 50 j=1,n
          DO 40 i=1,n
            del(i,j)=hsqlam*exp(u(i,j))-hsq3*upr(i,j)-8.e0*u(i,j)
            rr(i,j)=u(i,j)
   40     CONTINUE
   50   CONTINUE
 
c This is the internal nine point formula:
        DO 70 j=1,n
          DO 60 i=1,n
            del(i,j)=del(i,j)+rr(i-1,j-1)+rr(i,j-1)+rr(i+1,j-1)+
     &      rr(i-1,j)+rr(i+1,j)+rr(i-1,j+1)+rr(i,j+1)+rr(i+1,j+1)
   60     CONTINUE
   70   CONTINUE
        RETURN
c End of IF block for IRES .eq. 1.
      END IF
 
 
c The "Jacobian computation" is noted with this entry.
c All that is needed in this case is to save the scalar
c multiplying the partials with respect to y'.
      IF (ires.eq.2) THEN
c This passes the error code for singularity (IRES=0) to the integrator.
c It is an issue as the user code solves the linear systems.
c In this example we do not know about singularity until the solve step.
c We precompute quantities that are expensive.
        ires=0
        kf=kf+1
        DO 90 j=1,n
          DO 80 i=1,n
            u0(i,j)=hsqlam*exp(u(i,j))-hsq3*cj-8.e0
c The pre-conditioner used is obtained from the matrix diagonal.
c This yields a scaled problem,  A = D * B * D.  The system
c is solved with respect to B, applying scaling as needed during
c the GMRES algorithm.  The diagonal matrix D is stored in SCL(,).
            scl(i,j)=1.e0/sqrt(-u0(i,j))
c This array is used to hold the solution in GMRES.
            xd(i,j)=0.e0
   80     CONTINUE
   90   CONTINUE
        RETURN
c End of IF block for IRES .eq. 2.
      END IF
 
c Solve the corrector equation.
      IF (ires.eq.4) THEN
c Note number of corrector solve steps.
        ks=ks+1
c Use a rearranged version of the Saad and Schultz GMRES algorithm.
        rss=0.e0
        DO 110 j=1,n
c Compute the first residual, matrix * first approximation - RHS.
c The solution is started at zero.
c The difference in sign for this residual is accounted for in the
c final approximate solution formula.
          DO 100 i=1,n
c Apply scaling to the first residual.
            rr(i,j)=-del(i,j)*scl(i,j)
c Compute the sum of squares, for the eventual length
c of the first residual.
            rss=rss+rr(i,j)**2
  100     CONTINUE
  110   CONTINUE
        nrm=sqrt(rss)
c Set counter for number of GMRES basis vectors used.
        k=0
        DO 120 i=1,km+1
          hh(0,i)=0.e0
  120   CONTINUE
        hh(0,1)=nrm
c Set the relative tolerance for residual that terminates GMRES.
c This is passed from the main program unit.
        tol=reltol*nrm
 
c This is the primary iteration loop for the GMRES algorithm.
c Total number of matrix-vector products required by GMRES:
  130   kp=kp+1
c See if residual is now small enough.
        IF (abs(hh(0,k+1)).le.tol) GO TO 280
 
c Reciprocate factor for efficiency during scaling.
        rnrm=1.e0/nrm
        DO 150 j=1,n
          DO 140 i=1,n
            tt=rnrm*rr(i,j)
            qq(i,j,k+1)=tt
c     Account for pre-scaling before applying the product.
            rr(i,j)=tt*scl(i,j)
  140     CONTINUE
  150   CONTINUE
        k=k+1
c Compute the product, matrix * column k of basis matrix.
        DO 170 j=1,n
          DO 160 i=1,n
c Apply matrix product and post-scaling together.
            ww(i,j)=scl(i,j)*(u0(i,j)*rr(i,j)+rr(i-1,j-1)+rr(i-1,j)+
     &      rr(i-1,j+1)+rr(i,j+1)+rr(i,j-1)+rr(i+1,j-1)+rr(i+1,j)+
     &      rr(i+1,j+1))
  160     CONTINUE
  170   CONTINUE
 
 
c Keep a working copy of product for orthogonalization updates.
        DO 190 j=1,n
          DO 180 i=1,n
            rr(i,j)=ww(i,j)
  180     CONTINUE
  190   CONTINUE
 
c Compute updates and the new column of Hessenberg matrix.
        DO 240 l=1,k
          tt=0.e0
          DO 210 j=1,n
            DO 200 i=1,n
              tt=tt+ww(i,j)*qq(i,j,l)
  200       CONTINUE
  210     CONTINUE
          hh(k,l)=tt
          DO 230 j=1,n
            DO 220 i=1,n
              rr(i,j)=rr(i,j)-tt*qq(i,j,l)
  220       CONTINUE
  230     CONTINUE
  240   CONTINUE
c Apply previous rotations, that triangularized the Hessenberg matrix,
c to the new column.
        DO 250 i=1,k-1
          cc=cs(i)
          ss=sn(i)
          tt=hh(k,i)*cc+hh(k,i+1)*ss
          hh(k,i+1)=-hh(k,i)*ss+hh(k,i+1)*cc
          hh(k,i)=tt
  250   CONTINUE
c Compute the length of the updated residual vector.
        rss=0.e0
        DO 270 j=1,n
          DO 260 i=1,n
            rss=rss+rr(i,j)**2
  260     CONTINUE
  270   CONTINUE
 
c Update the new entry in the developing Hessenberg matrix.
        nrm=sqrt(rss)
        hh(k,k+1)=nrm
c Compute the plane rotation that returns the Hessenberg
c matrix to upper triangular form.
        tt=sqrt(rss+hh(k,k)**2)
        cc=hh(k,k)/tt
        ss=hh(k,k+1)/tt
        hh(k,k)=tt
c Apply rotation to the updated RHS.
        tt=hh(0,k)*cc+hh(0,k+1)*ss
c This is the new residual vector length.  It is checked for
c smallness at the next loop iteration.
        hh(0,k+1)=-hh(0,k)*ss+hh(0,k+1)*cc
        hh(0,k)=tt
c Save rotation parameters for later application to new
c Hessenberg matrix columns.
        cs(k)=cc
        sn(k)=ss
c This check stops the iterations if there is no longer enough
c array space.
c This ends the primary iteration loop for the GMRES algorithm.
        IF (k .lt. km) GO TO 130
 
  280   CONTINUE
 
c Solve the K by K upper triangular system for the multipliers
c of the orthogonal basis of the approximate solution.
        DO 300 j=k,1,-1
          tt=0.e0
          DO 290 i=j+1,k
            tt=tt+y(i)*hh(i,j)
  290     CONTINUE
          y(j)=(hh(0,j)-tt)/hh(j,j)
  300   CONTINUE
 
c  Compute approximate solution.  This is: initial approximation -
c  product of basis vectors multiplied by the y values.
c  This accounts for the change of sign on the first residual.
        DO 330 l=1,k
          DO 320 j=1,n
            DO 310 i=1,n
              xd(i,j)=xd(i,j)-qq(i,j,l)*y(l)
  310       CONTINUE
  320     CONTINUE
  330   CONTINUE
 
c This solution is therefore a starting approximation for the next one.
        DO 350 j=1,n
          DO 340 i=1,n
c     Account for pre-scaling in solution returned.
            del(i,j)=xd(i,j)*scl(i,j)
            xd(i,j)=0.e0
  340     CONTINUE
  350   CONTINUE
c End of IF block for IRES .eq. 4; end of GMRES algorithm application.
      END IF
      RETURN
      END
 
