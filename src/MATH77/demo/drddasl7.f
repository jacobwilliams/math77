      PROGRAM drddasl7
c>> 2008-10-26 DRDDASL7 Krogh Moved Format statements up for C conv.
c>> 2008-10-24 DRDDASL7 Krogh Removed in INCLUDE statement & cDEC$...
c>> 2008-08-27 DRDDASL7  compute initial y' value, change user codes
c>> 2006-04-26 DRDDASL7, Krogh Dimensioned ATOL and RTOL.
c>> 2006-04-24 DRDDASL7, Krogh Moved count initialization up.
c>> 2002-01-18 DRDDASL7, R. J. Hanson Example Code
  
c   DDASLX is used to solve an ODE problem, with a banded Jacobian.
c   Two runs with analytic partials are used.  Both forward and
c   reverse communication usage examples are illustrated.
  
c--D replaces "?": DR?DASL7, ?DASLX, ?DASSF7, ?EDIT2, ?GBFA, ?GBSL
      EXTERNAL ddassf7
      INTEGER ndig
      DOUBLE PRECISION tol
c++S Default NDIG = 4
c++  Default NDIG = 8
c++ Substitute for NDIG below
      PARAMETER  (NDIG=8)
      PARAMETER  (tol=10.d0**(-ndig))
      INTEGER neq,liw,lrw,ml,mu,maxord,maxcon
      PARAMETER  (neq=25)
      PARAMETER  (liw=neq+30+2)
c     The work space LRW has the banded matrix size
c     because with INFO(5)=4, the Jacobian matrix is stored in
c      banded form.  (Only MAXCON = 0 is allowed with the band solver.)
      PARAMETER  (ml=5,mu=0,maxord=5,maxcon=0,lrw=45+(maxord+maxcon+4)*
     & neq+(2*ml+mu+1)*neq+2*(neq/(ml+mu+1)+1))
      INTEGER i,idid,info(16),ires,iwork(liw),nerr,nqu,iout,lda,
     & ipvt(neq)
      DOUBLE PRECISION atol(1),delta(neq),er,ero,erm,hu,rtol(1),
     & rwork(lrw),t,tout,y(neq),yprime(neq)
  
  120 FORMAT (' Demo Program for DDASLX',///1x,' Problem 7: y'' = A * y 
     &, where',' A is a banded lower triangular matrix',/1x,' NEQ =',i3,
     &'   ML =',i2,'   MU =',i2,'( size, bandwidths)'/1x,' RTOL =',1p,
     & d10.1,'   ATOL =',d10.1,'( rel, abs tolerance)')
  130 FORMAT (1x,1p,d15.5,d14.3,i6,d14.3)
  140 FORMAT (/6x,'Example Results for an Index-0 Banded ODE Problem,','
     & Soln in Forward Comm',//9x,'T',11x,'Max Err',3x,
     & 'BDF Order Last Step=H')
  150 FORMAT (/6x,'Example Results for an Index-0 Banded ODE Problem,','
     & Soln in Reverse Comm',//9x,'T',11x,'Max Err',3x,
     & 'BDF Order Last Step=H')
c 
c A constant coefficient, banded matrix:
c 
      DO 10 i=1,16
        info(i)=0
   10 CONTINUE
  
c Banded matrix with user providing derivatives.
      info(5)=4
      iwork(1)=ml
      iwork(2)=mu
      lda=2*ml+mu+1
  
c     Tolerance:
      atol(1)=tol
      rtol(1)=0.d0
      WRITE (*,120) neq,ml,mu,rtol(1),atol(1)
      t=0.0d0
      DO 20 i=1,neq
        y(i)=0.0d0
        delta(i)=0.0d0
   20 CONTINUE
      y(1)=1.0d0
c     These are function and Jacobian evaluation counters.
      iwork(liw-1)=0
      iwork(liw)=0
  
c  The first call initializes internal data values, and the second
c  call gives a consistent value of YPRIME.  Note the reversed
c  positions of DELTA, YPRIME.  This usage computes a consistent value
c  of YPRIME in the case of Index 0 systems.
      DO 30 i=0,1
        ires=i
        CALL ddassf7 (t, y, delta, yprime, rwork, lda, rwork(1), ires,
     &   rwork, iwork)
  
   30 CONTINUE
  
      tout=0.01d0
      ero=0.0d0
      nerr=0
      WRITE (*,140) 
c     This shows passing data from evaluation routine ?DASF = ?DASF7
c      to the calling program.
      DO 40 iout=1,6
        CALL ddaslx (ddassf7, neq, t, y, yprime, tout, info, rtol, atol,
     &    idid, rwork, lrw, iwork, liw)
        CALL dedit2 (y, t, erm)
        hu=rwork(7)
        nqu=iwork(8)
        WRITE (*,130) t,erm,nqu,hu
c 
        IF (idid.lt.0) GO TO 50
        er=erm/atol(1)
        ero=max(ero,er)
        IF (er.gt.1.0d0) THEN
          nerr=nerr+1
        END IF
c Advance to the next output point.
        tout=tout*10.0d0
   40 CONTINUE
   50 CONTINUE
  
c Start over but solve banded linear system in reverse communication mod
      t=0.0d0
      DO 60 i=1,neq
        y(i)=0.0d0
        delta(i)=0.0d0
   60 CONTINUE
      y(1)=1.0d0
  
c  The first call initializes internal data values, and the second
c  call gives a consistent value of YPRIME. Note the reversed
c  positions of DELTA, YPRIME.  This computes a consistent value
c  of YPRIME in the case of Index 0 systems.
      DO 70 i=0,1
        ires=i
        CALL ddassf7 (t, y, delta, yprime, rwork, lda, rwork(1), ires,
     &   rwork, iwork)
   70 CONTINUE
  
      tout=0.01d0
      ero=0.0d0
      nerr=0
      info(1)=0
  
      WRITE (*,150) 
      iwork(liw-1)=0
      iwork(liw)=0
c All work done using reverse communication:
      info(5)=-14
      DO 100 iout=1,6
  
   80   CALL ddaslx (ddassf7, neq, t, y, yprime, tout, info, rtol, atol,
     &    idid, rwork, lrw, iwork, liw)
c     When IDID == 4 the code returned for reverse communication.
c     Otherwise the integration has finished up to this output point.
        IF (idid.ne.4) GO TO 90
        ires=iwork(3)
c Evaluate residuals or partials:
        IF (ires.le.2) THEN
          CALL ddassf7 (t, y, yprime, rwork(iwork(4)), rwork(iwork(5)),
     &     lda, rwork(1), ires, rwork, iwork)
  
        END IF
c Factor the banded matrix:
        IF (ires.eq.3) THEN
c The matrix is contained in the RWORK array.
c With this request, IWORK(5) points to the matrix, and IWORK(3) == IRES
c Note that IWORK(3) gets the error code (==0 means non-singular matrix)
          CALL dgbfa (rwork(iwork(5)), lda, neq, ml, mu, ipvt, iwork(3))
        END IF
  
c Solve with the banded matrix:
        IF (ires.eq.4) THEN
c The right-hand side is contained in the RWORK array.
c With this request, IWORK(4) points to the right-hand side.
          CALL dgbsl (rwork(iwork(5)), lda, neq, ml, mu, ipvt,
     &     rwork(iwork(4)), 0)
        END IF
        GO TO 80
   90   CONTINUE
        CALL dedit2 (y, t, erm)
        hu=rwork(7)
        nqu=iwork(8)
        WRITE (*,130) t,erm,nqu,hu
c 
        IF (idid.lt.0) GO TO 110
        er=erm/atol(1)
        ero=max(ero,er)
        IF (er.gt.1.0d0) THEN
          nerr=nerr+1
        END IF
c Advance to the next output point.
        tout=tout*10.0d0
  100 CONTINUE
  110 CONTINUE
      END
  
      SUBROUTINE ddassf7 (t, y, yprime, delta, pd, ldp, cj, ires, rwork,
     & iwork)
c 
c     Example from SLATEC distribution.
c     This exercises the banded matrix solver.
      DOUBLE PRECISION t,y(*),yprime(*),delta(*),pd(ldp,*),cj,rwork(*)
      INTEGER ires,iwork(*),i,j,k,mband,ng,ldp,ml,mu
      DOUBLE PRECISION alph1,alph2,d
      INTEGER neq,liw
      PARAMETER  (neq=25)
c     The last two value of IWORK() pass evaluation counters
c     back to the main program.
      PARAMETER  (liw=neq+30+2)
  
      SAVE  alph1,alph2,ng,ml,mu
c This is the setup call.
      IF (ires.eq.0) THEN
        alph1=1.0d0
        alph2=1.0d0
        ng=5
        ml=5
        mu=0
      END IF
  
c The system residual value.
      IF (ires.eq.1) THEN
        DO 20 j=1,ng
          DO 10 i=1,ng
            k=i+(j-1)*ng
            d=-2.0d0*y(k)
            IF (i.ne.1) d=d+y(k-1)*alph1
            IF (j.ne.1) d=d+y(k-ng)*alph2
            delta(k)=d-yprime(k)
   10     CONTINUE
   20   CONTINUE
c Count residual evaluations using end of integer work array.
        iwork(liw-1)=iwork(liw-1)+1
      END IF
  
c The partial derivative matrix.
      IF (ires.eq.2) THEN
        mband=ml+mu+1
        DO 30 j=1,neq
          pd(mband,j)=-2.0d0-cj
          pd(mband+1,j)=alph1
          pd(mband+2,j)=0.0d0
          pd(mband+3,j)=0.0d0
          pd(mband+4,j)=0.0d0
          pd(mband+5,j)=alph2
   30   CONTINUE
        DO 40 j=1,neq,ng
          pd(mband+1,j)=0.0d0
   40   CONTINUE
c Count partial evaluations using end of integer work array.
        iwork(liw)=iwork(liw)+1
  
      END IF
  
      END
  
      SUBROUTINE dedit2 (y, t, erm)
c***BEGIN PROLOGUE  DEDIT2
c***SUBSIDIARY
c***LIBRARY   SLATEC (DASSL)
c***TYPE      DOUBLE PRECISION (EDIT2-S, DEDIT2-D)
c***AUTHOR  PETZOLD, LINDA R., (LLNL)
c***ROUTINES CALLED  (NONE)
c***END PROLOGUE  DEDIT2
      DOUBLE PRECISION y(*),t,erm
      INTEGER i,j,k,ng
      DOUBLE PRECISION alph1,alph2,a1,a2,big,er,ex,ri,rj,yt
      DOUBLE PRECISION d1mach
      DATA alph1/1.0d0/,alph2/1.0d0/,ng/5/
      EXTERNAL d1mach
c***FIRST EXECUTABLE STATEMENT  DEDIT2
      erm=0.0d0
      IF (t.eq.0.0d0) RETURN
      ex=0.0d0
      big=.5d0*log(d1mach(2))
  
c Compute matrix exponential * initial data vector,
c for this particular lower triangular matrix.
  
      IF (t.lt.big) ex=exp(-2.0d0*t)
      a2=1.0d0
      DO 20 j=1,ng
        a1=1.0d0
        DO 10 i=1,ng
          k=i+(j-1)*ng
          yt=t**(i+j-2)*ex*a1*a2
          er=abs(y(k)-yt)
          erm=max(erm,er)
          ri=i
          a1=a1*alph1/ri
   10   CONTINUE
        rj=j
        a2=a2*alph2/rj
   20 CONTINUE
      RETURN
      END
