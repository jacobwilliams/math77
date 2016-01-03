      subroutine ddastp (x, y, yprime, neq, ldd, ddasf, info, h, wt,
     &                   idid, phi, delta, e, wm, iwork,
     &                   rwork, alpha, beta, gamma, psi, sigma, k)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2008-10-24 ddastp Krogh  Declared dnrm2
c>> 2008-08-26 ddastp Hanson add argument of leading dimension to ddasf
c>> 2006-05-18 ddastp Hanson Install test for inconsistent constraints
c>> 2006-04-14 ddastp Krogh  Zero high differences on order increase.
c>> 2003-03-05 ddastp Hanson Install Soderlind stepsize code.
c>> 2002-06-26 ddastp Krogh  Insured iwork(lk) has current value.
c>> 2001-12-12 ddastp Krogh  Changed code for reverse communication
c>> 2001-11-23 ddastp Krogh  Changed many names per library conventions.
c>> 2001-11-04 ddastp Krogh  Fixes for F77 and conversion to single
c>> 2001-11-01 ddastp Hanson Provide code to Math a la Carte.
c--D replaces "?": ?dastp, ?dasj, ?dasco, ?dasin, ?dasf,
c-- & ?dasnm, ?daslv, ?daslx, ?dasdb, ?dasgh, ?nrm2, ?copy

c***BEGIN PROLOGUE  DDASTP
c***SUBSIDIARY
c***PURPOSE  Perform one step of the DDASLX integration.
c***LIBRARY   SLATEC (DDASLX)
c***TYPE      DOUBLE PRECISION (SDASTP-S, DDASTP-D)
c***AUTHOR  Petzold, Linda R., (LLNL)
c***DESCRIPTION
c-----------------------------------------------------------------------
c     DDASTP SOLVES A SYSTEM OF DIFFERENTIAL/
c     ALGEBRAIC EQUATIONS OF THE FORM
c     G(X,Y,YPRIME) = 0,  FOR ONE STEP (NORMALLY
c     FROM X TO X+H).
c
c     THE METHODS USED ARE MODIFIED DIVIDED
c     DIFFERENCE,FIXED LEADING COEFFICIENT
c     FORMS OF BACKWARD DIFFERENTIATION
c     FORMULAS. THE CODE ADJUSTS THE STEPSIZE
c     AND ORDER TO CONTROL THE LOCAL ERROR PER
c     STEP.
c
c
c     THE PARAMETERS REPRESENT
c     X  --        INDEPENDENT VARIABLE
c     Y  --        SOLUTION VECTOR AT X
c     YPRIME --    DERIVATIVE OF SOLUTION VECTOR AFTER SUCCESSFUL STEP
c     NEQ --       NUMBER OF EQUATIONS TO BE INTEGRATED
c     DDASF --     EXTERNAL USER-SUPPLIED SUBROUTINE
c                  TO EVALUATE THE RESIDUAL.  THE CALL IS
c                  CALL DDASF(X,Y,YPRIME,DELTA,D,LDD,C,IRES,RWORK,IWORK)
c                  X,Y,YPRIME ARE INPUT.  DELTA IS OUTPUT.
c                  ON INPUT, IRES=0.  DDASF SHOULD ALTER IRES ONLY
c                  IF IT ENCOUNTERS AN ILLEGAL VALUE OF Y OR A
c                  STOP CONDITION.  SET IRES=-1 IF AN INPUT VALUE
c                  OF Y IS ILLEGAL, AND DDASTP WILL TRY TO SOLVE
c                  THE PROBLEM WITHOUT GETTING IRES = -1.  IF
c                  IRES=-2, DDASTP RETURNS CONTROL TO THE CALLING
c                  PROGRAM WITH IDID = -1.  IRES CAN ALSO BE SET TO
C                  LARGE NEGATIVE VALUES TO SET DEBUGGING PRINT.
c     H --         APPROPRIATE STEP SIZE FOR NEXT STEP.
c                  NORMALLY DETERMINED BY THE CODE
c     WT --        VECTOR OF WEIGHTS FOR ERROR CRITERION.
c     IDID --      COMPLETION CODE WITH THE FOLLOWING MEANINGS:
c                  IDID= 1 -- THE STEP WAS COMPLETED SUCCESSFULLY
c                  IDID=-1 -- IRES EQUAL TO -2 WAS ENCOUNTERED,
c                             AND CONTROL IS BEING RETURNED TO
c                             THE CALLING PROGRAM
c                  IDID=-4 -- THE CORRECTOR COULD NOT CONVERGE
c                             BECAUSE IRES WAS EQUAL TO MINUS ONE
c                  IDID=-6 -- THE ERROR TEST FAILED REPEATEDLY
c                  IDID=-7 -- THE CORRECTOR COULD NOT CONVERGE
c                  IDID=-8 -- THE ITERATION MATRIX IS SINGULAR
c                  IDID=-9 -- THE CORRECTOR COULD NOT CONVERGE.
c                             THERE WERE REPEATED ERROR TEST
c                             FAILURES ON THIS STEP.
c     PHI --       ARRAY OF DIVIDED DIFFERENCES USED BY
c                  DDASTP. THE LENGTH IS NEQ*(K+1),WHERE
c                  K IS THE MAXIMUM ORDER
c     DELTA,E --   WORK VECTORS FOR DDASTP OF LENGTH NEQ
c     WM,IWORK --  REAL AND INTEGER ARRAYS STORING
c                  MATRIX INFORMATION SUCH AS THE MATRIX
c                  OF PARTIAL DERIVATIVES,PERMUTATION
c                  VECTOR, AND VARIOUS OTHER INFORMATION.
c     RWORK --     THE USUAL WORK ARRAY.
c     ALPHA, BETA, GAMMA, PSI, SIGMA --  USED FOR INTEGRATION
c                  COEFFICIENTS.
c     K --         THE CURRENT INTEGRATION ORDER.
c
c     THE OTHER PARAMETERS ARE INFORMATION
c     WHICH IS NEEDED INTERNALLY BY DDASTP TO
c     CONTINUE FROM STEP TO STEP.
c
c-----------------------------------------------------------------------
c***ROUTINES CALLED  DDASJ, DDASNM, DDASLV, DDASIN
c***REVISION HISTORY  (YYMMDD)
c   830315  DATE WRITTEN
c   901009  Finished conversion to SLATEC 4.0 format (F.N.Fritsch)
c   901019  Merged changes made by C. Ulrich with SLATEC 4.0 format.
c   901026  Added explicit declarations for all variables and minor
c           cosmetic changes to prologue.  (FNF)
c   981119  Replaced RES, JAC by DDASF, RJH.
c***END PROLOGUE  DDASTP
c
      integer    neq, ldd, idid, iwork(*), k, info(16)
      double precision x, y(*), yprime(*), h, wt(*), phi(neq,*),
     &           delta(*), e(*), wm(*), rwork(*), alpha(*), beta(*),
     &           gamma(*), psi(*), sigma(*)
      external   ddasf
c
      external   ddasj, ddasnm, ddaslv, ddasin, dnrm2
      double precision ddasnm
c
      integer    i,ires, j, j1, kdiff, km1, knew, kp1, kp2,
     &           m, maxit, ncf, nef, nsf, nsp1
      double precision alpha0, alphas, cjlast, ck, delnrm, enorm, erk,
     &           erkm1, erkm2, erkp1, err, est, hnew, oldnrm, pnorm,
     &           r, rate, temp1, temp2, terk, terkm1, terkm2, terkp1,
     &           xold, xrate, sc
C   DDASGH --  computes the new stepsize. There are three controllers
C            one can choose: H211b(b=4), PI.4.2 and standard
C            control.
C  CTRNM -- the name of controller:
C            CTRNM = H211B4:   H221B(B=4) controller <= only one used
C                 = PI42:     PI.4.2 controller
C                 = STAND:    standard controller
C                 default controller is H211b4 <= only one used
      double precision ESTOLD, RATIO
      logical    convgd
c
      data maxit/4/
      data xrate/0.25d0/

      integer    locate
c
c     POINTERS INTO IWORK
      integer  lml, lmu, lires, ldelt, lwm, lmxord, lk, lkold, lmat,
     &     lcnstr, lns, lnstl, lnst, lnre, lnje, letf, lctf, lnpd,
     &     ljcalc, lphase, revloc, mxstep, le, lwt, lphi, ntemp, lipvt
      parameter (lml=1, lmu=2, lires=3, ldelt=4, lwm=5, lmxord=6, lk=7,
     &     lkold=8, lmat=9, lcnstr=10, lns=11, lnstl=12, lnst=13,
     &     lnre=14, lnje=15, letf=16, lctf=17, lnpd=18, ljcalc=19,
     &     lphase=20, revloc=21, mxstep=22, le=23, lwt=24, lphi=25,
     &     ntemp=26, lipvt=31)
c
c     POINTERS INTO RWORK
      integer  lcj, ltstop, lhmax, lh, ltn, lcjold, lhold, lnjac,
     &     lround, lhmin, lalpha, lbeta, lgamma, lpsi, lsigma, ldelta
      parameter  (lcj=1, ltstop=2, lhmax=3, lh=4, ltn=5, lcjold=6,
     &     lhold=7, lnjac=8, lround=9, lhmin=10, lalpha=11, lbeta=17,
     &     lgamma=23, lpsi=29, lsigma=35, ldelta=46)
c
c     POINTERS INTO INFO
      integer  itol, iout, istop, imat, idb, imaxh, ih0, iord, icnstr,
     &     inityp, ixstep
      parameter  (itol=2, iout=3,  istop=4, imat=5, idb=6, imaxh=7,
     &     ih0=8, iord=9, icnstr=10, inityp=11, ixstep= 12)
      integer ISMOOT, IFIRST
      parameter (ISMOOT=13, IFIRST=16)
      double precision conrate, dnrm2
      save
c
c-----------------------------------------------------------------------
c     BLOCK 1.
c     INITIALIZE. ON THE FIRST CALL,SET
c     THE ORDER TO 1 AND INITIALIZE
c     OTHER VARIABLES.
c-----------------------------------------------------------------------
c
c     INITIALIZATIONS FOR ALL CALLS
c***FIRST EXECUTABLE STATEMENT  DDASTP
      locate = mod(iwork(revloc), 8)
     
      if (locate .gt. 0) then
         ires = iwork(lires)
         iwork(revloc) = iwork(revloc) / 8
         go to (130, 140, 145, 190, 230, 260, 286), locate
      end if

c     No reverse communication active
      if (locate .lt. 0) then
c     FIRST STEP INITIALIZATIONS
         iwork(letf)   = 0
         iwork(lctf)   = 0
         rwork(lhold)  = 0.0d0
         psi(1)        = h
         rwork(lcjold)         = 1.0d0/h
         rwork(lcj)            = rwork(lcjold)
         rwork(lnjac)  = 100.d0
         iwork(ljcalc) = -1
         delnrm        = 1.0d0
         iwork(lphase) = 0
         iwork(lns)    = 0
         iwork(revloc) = 0
         RATIO =1.D0
      end if
c        Not doing reverse communication
      idid   = 1
      xold   = x
      ncf    = 0
      nsf    = 0
      nef    = 0
c-----------------------------------------------------------------------
c     BLOCK 2
c     COMPUTE COEFFICIENTS OF FORMULAS FOR
c     THIS STEP.
c-----------------------------------------------------------------------
   20 continue
      k = iwork(lk)
      kp1  = k + 1
      kp2  = k + 2
      km1  = k - 1
      xold = x
      if ((h.ne.rwork(lhold)) .or. (k.ne.iwork(lkold))) iwork(lns) = 0
      iwork(lns)   = min(iwork(lns) + 1, iwork(lkold) + 2)
      nsp1 = iwork(lns) + 1
      if (kp1 .lt. iwork(lns)) go to 40
c
      beta(1)  = 1.0d0
      alpha(1) = 1.0d0
      temp1    = h
      gamma(1) = 0.0d0
      sigma(1) = 1.0d0
      do 30  i=2, kp1
         temp2    = psi(i-1)
         psi(i-1) = temp1
         beta(i)  = beta(i-1)*psi(i-1)/temp2
         temp1    = temp2 + h
         alpha(i) = h/temp1
         sigma(i) = (i-1)*sigma(i-1)*alpha(i)
         gamma(i) = gamma(i-1) + alpha(i-1)/h
   30 continue
      psi(kp1) = temp1
   40 continue
c
c     COMPUTE ALPHAS, ALPHA0
      alphas = 0.0d0
      alpha0 = 0.0d0
      do 50  i=1, k
         alphas = alphas - 1.0d0/i
         alpha0 = alpha0 - alpha(i)
   50 continue
c
c     COMPUTE LEADING COEFFICIENT RWORK(LCJ)
      cjlast = rwork(lcj)
      rwork(lcj)     = -alphas/h
c
c     COMPUTE VARIABLE STEPSIZE ERROR COEFFICIENT CK
      ck = abs(alpha(kp1)+alphas-alpha0)
      ck = max(ck,alpha(kp1))
c
c     DECIDE WHETHER NEW JACOBIAN IS NEEDED
      temp1 = (1.0d0-xrate)/(1.0d0+xrate)
      temp2 = 1.0d0/temp1

      if ((rwork(lcj)/rwork(lcjold).lt.temp1) .or.
     &    (rwork(lcj)/rwork(lcjold).gt.temp2)) iwork(ljcalc) = -1
      if (rwork(lcj) .ne. cjlast) rwork(lnjac) = 100.d0
c
c     CHANGE PHI TO PHI STAR
      if (kp1 .lt. nsp1) go to 80
      do 70  j=nsp1, kp1
         do 60  i=1, neq
   60    phi(i,j) = beta(j)*phi(i,j)
   70 continue
   80 continue
c
c     UPDATE TIME
      x = x + h
c
c-----------------------------------------------------------------------
c     BLOCK 3
c     PREDICT THE SOLUTION AND DERIVATIVE,
c     AND SOLVE THE CORRECTOR EQUATION
c-----------------------------------------------------------------------
c
c         stepping past TOUT.  Y(:)} is obtained by interpolation.
c         YPRIME(:) is obtained by interpolation.
c      4  The integration has paused for reverse communication.  Respond
c         based on the values of IRES
c                  Task Interupted
c     -1  IRES set to -2 by the user.
c     -2  Accuracy requested exceeds machine precision.  RTOL and ATOL
c         have been increased.
c     -3  There have been too many steps between output points.
c                  Quit or Restart Integration
c     
c     FIRST,PREDICT THE SOLUTION AND DERIVATIVE
   90 continue
      do 100  i=1, neq
         y(i) = phi(i,1)
  100 yprime(i) = 0.0d0
      do 120  j=2, kp1
         do 110  i=1, neq
            y(i) = y(i) + phi(i,j)
  110    yprime(i) = yprime(i) + gamma(j)*phi(i,j)
  120 continue
      pnorm = ddasnm(neq,y,wt,rwork,iwork)
c
c     SOLVE THE CORRECTOR EQUATION USING A
c     MODIFIED NEWTON SCHEME.
      convgd    = .true.
      m         = 0
      iwork(lnre) = iwork(lnre) + 1
      ires = 1
      if (info(idb) .ne. 0) call ddasdb(2, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (iwork(lmat) .ge. 0) then
         call ddasf (x, y, yprime, delta, wm, LDD, rwork(lcj),
     &   ires, rwork, iwork)

      else
         iwork(lires) = ires
c     Need to put locations [from E(*)] to DELTA(*).
         iwork(revloc) = 8 * iwork(revloc) + 1
         return
      end if
c REVERSE ENTRY 1:
  130 continue
      if (info(idb) .ne. 0) call ddasdb(3, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (ires .lt. 0) then
         if (ires .ge. -2) go to 290
         info(idb) = -ires
         ires = 0
      end if
c
c     IF INDICATED,REEVALUATE THE
c     ITERATION MATRIX PD = DG/DY + RWORK(LCJ)*DG/DYPRIME
c     (WHERE G(X,Y,YPRIME)=0). SET
c     IWORK(LJCALC) TO 0 AS AN INDICATOR THAT
c     THIS HAS BEEN DONE.
      if (iwork(ljcalc) .ne. -1) go to 150
      iwork(lnje) = iwork(lnje) + 1
      iwork(ljcalc)     = 0
c REVERSE ENTRY 2:
  140 continue
      call ddasj (neq, ldd, x, y, yprime, delta,h, wt, e,
     &     wm, iwork, rwork, ddasf, info, ires)
      if (iwork(revloc) .ne. 0) then
         if (iwork(revloc) .lt. 0) then 
            iwork(revloc) = 3
         else
            iwork(revloc) = 8 * iwork(revloc) + 2
         end if
         return
      end if
c REVERSE ENTRY 3:
  145 continue
      rwork(lcjold) = rwork(lcj)
      rwork(lnjac)     = 100.d0
      if (ires .lt. 0) then
         if (ires .ge. -2) go to 290
         info(idb) = -ires
         ires = 0
      end if
      if (ires .ne. 0) go to 290

      nsf = 0
c
c
c     INITIALIZE THE ERROR ACCUMULATION VECTOR E.
  150 continue
      do 160  i=1, neq
  160 e(i) = 0.0d0
c
c
c     CORRECTOR LOOP.
  170 continue
c
c     MULTIPLY RESIDUAL BY TEMP1 TO ACCELERATE CONVERGENCE
      temp1 = 2.0d0/(1.0d0+rwork(lcj)/rwork(lcjold))
  
      do 180  i=1, neq
  180 delta(i) = delta(i)*temp1
c
c     COMPUTE A NEW ITERATE (BACK-SUBSTITUTION).
c     STORE THE CORRECTION IN DELTA.
      call ddaslv (neq, ldd, x, y, yprime, delta, ddasf, info,
     &             iwork, rwork)
      if (iwork(revloc) .lt. 0) then
         iwork(revloc) = 4
         return
      end if
c REVERSE ENTRY 4:
  190 continue
      if (info(idb) .ne. 0) call ddasdb(4, neq, x, y, yprime,
     &    info, rwork, iwork, ires, wt, rwork)
c
c     UPDATE Y, E, AND YPRIME
      do 200  i=1, neq
         y(i) = y(i) - delta(i)
         e(i) = e(i) - delta(i)
  200 yprime(i) = yprime(i) - rwork(lcj)*delta(i)
c
c     TEST FOR CONVERGENCE OF THE ITERATION
      delnrm = ddasnm(neq,delta,wt,rwork,iwork)
C
C     Set convergence rate allowed as Soderlind proposes:
      CONRATE=0.125D0
      IF(INFO(ISMOOT) .ne. 0) CONRATE=.33D0
      if (delnrm .le. 100.d0 * rwork(lround) * pnorm) go to 250
      if (m .gt. 0) go to 210
      oldnrm = delnrm
      go to 220
  210 rate = (delnrm/oldnrm)**(1.0d0/m)
      if (rate .gt. 0.90d0) go to 240
      rwork(lnjac) = rate / (1.0d0 - rate)
  220 CONTINUE
      if ((rwork(lnjac) * delnrm) .le. CONRATE) go to 250
c
c     THE CORRECTOR HAS NOT YET CONVERGED.
c     UPDATE M AND TEST WHETHER THE
c     MAXIMUM NUMBER OF ITERATIONS HAVE
c     BEEN TRIED.
      m = m + 1
      if (m .ge. maxit) go to 240
c
c     EVALUATE THE RESIDUAL
c     AND GO BACK TO DO ANOTHER ITERATION
      iwork(lnre) = iwork(lnre) + 1
      ires = 1
      if (info(idb) .ne. 0) call ddasdb(2, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (iwork(lmat) .ge. 0) then
         call ddasf (x, y, yprime, delta, wm, LDD, rwork(lcj),
     &     ires, rwork, iwork)
      else
         iwork(lires) = ires
         iwork(revloc) = 5
         return
      end if
c REVERSE ENTRY 5:
  230 continue
      if (info(idb) .ne. 0) call ddasdb(3, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (ires .lt. 0) then
         if (ires .ge. -2) go to 290
         info(idb) = -ires
         ires = 0
      end if
      go to 170
c     THE CORRECTOR FAILED TO CONVERGE IN MAXIT
c     ITERATIONS. IF THE ITERATION MATRIX
c     IS NOT CURRENT,RE-DO THE STEP WITH
c     A NEW ITERATION MATRIX.
  240 continue
      if (iwork(ljcalc) .eq. 0) go to 290
      iwork(ljcalc) = -1
      go to 90
c     THE ITERATION HAS CONVERGED.  IF CONSTRAINTS on SOLUTION are
c     REQUIRED, SET THE SOLUTION, IF THE PERTURBATION
c     TO DO IT IS SMALL ENOUGH.  IF THE CHANGE IS TOO LARGE, THEN
c     CONSIDER THE CORRECTOR ITERATION TO HAVE FAILED.
c     Also have an option right here to project back onto constraints.
c     This is primarily for problems that started with index > 1.
  250 if (iwork(lcnstr) .eq. 0) go to 300
C     Define weights to use for projecting back to constraints.
      DO 255 I=1,NEQ
C        DELTA(I)=ONE/WT(I)
         DELTA(I)=WT(I)
c          DELTA(I)=sqrt(WT(I))
  255 CONTINUE
      ires = 5
      if (info(idb) .ne. 0) call ddasdb(2, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (iwork(lmat) .gt. 0) then
         call ddasf (x, y, yprime, delta, wm, LDD, rwork(lcj),
     &   ires, rwork,  iwork)
      else
         iwork(lires) = ires
c     Need to put constraint change in DELTA(*).
         iwork(revloc) = 6
         return
      end if
c REVERSE ENTRY 6:
  260 continue
      if (info(idb) .ne. 0) call ddasdb(3, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (ires .lt. 0) then
         if (ires .ge. -2) go to 290
         info(idb) = -ires
         ires = 0
      end if
c     The user has computed info for constraints.  Compute a single weighted
c     least-distance Newton step back to the constraints. Put move into
c     delta(:).
      if (abs(iwork(lmat)) .le. 4) 
     &   call ddasco (wm(neq+1), ldd, neq, wt, delta)
c If ldd .eq. neq there are no constraints.  Nothing needs
c to be done.
      if(ldd .eq. neq) go to 300     
      delnrm = ddasnm(neq,delta,wt,rwork,iwork)
      sc     = ddasnm(neq,e,wt,rwork,iwork)
      if (delnrm .le. sc ) then
         do 270  i=1, neq
  270    e(i) = e(i) - delta(i)
      else
         do 280  i=1, neq
c Scale change so it is no larger that the current size of e(:).
  280    e(i) = e(i) - (sc/delnrm)*delta(i)
      end if
C Check if the residual norm for the linear system was positive. 
C If so then compare this value with what a perturbation in y implies
C for a perturbation in the constraints.
          
      if(delta(neq+1) .eq. 0) go to 300
C Save a copy of the solution      
        call dcopy(neq, y, 1, delta, 1)
C Perturb the solution values in units of the largest amount of
C error allowed on each component.  A random number could be used
C here instead of 1, -1, 1, ....
        sc=1
        do 285 i = 1,neq
          y(i) = y(i) + sc/wt(i)
          sc = -sc
  285   continue
      ires = 5
      sc = delta(neq+1)
      if (iwork(lmat) .gt. 0) then
         call ddasf (x, y, yprime, delta, wm, LDD, rwork(lcj),
     &   ires, rwork,  iwork)
      else
         iwork(lires) = ires
         iwork(revloc) = 7
         return
      end if
c REVERSE ENTRY 7:
  286 continue 
c Compute the residual norm on the constraints with the
c perturbation.
        delnrm = dnrm2(ldd-neq,delta(neq+1),1)
c If the solution residual norm is almost as large
c as the perturbation norm then there is a serious problem.        
        if(sc .ge. 0.5*delnrm)then
c This is the error flag assigned to this condition.        
          IDID = -30
c These values are needed to respond to the error condition.         
          delta(1)=sc
          delta(2)=delnrm
          return
        end if            
C Restore the solution if the system appears consistent.     
        call dcopy(neq, delta, 1, y, 1)
      go to 300
c
c
c     EXITS FROM BLOCK 3
c     NO CONVERGENCE WITH CURRENT ITERATION
c     MATRIX,OR SINGULAR ITERATION MATRIX
  290 convgd = .false.
      if (ires .eq. -2) then
         idid = -1
         return
      end if
c
  300 iwork(ljcalc) = 1
      if (.not.convgd) go to 490
c-----------------------------------------------------------------------
c     BLOCK 4
c     ESTIMATE THE ERRORS AT ORDERS K,K-1,K-2
c     AS IF CONSTANT STEPSIZE WAS USED. ESTIMATE
c     THE LOCAL ERROR AT ORDER K AND TEST
c         stepping past TOUT.  Y(:)} is obtained by interpolation.
c         YPRIME(:) is obtained by interpolation.
c      4  The integration has paused for reverse communication.  Respond
c         based on the values of IRES.
c                  Task Interupted
c     -1  IRES set to -2 by the user.
c     -2  Accuracy requested exceeds machine precision.  RTOL and ATOL
c         have been increased.
c     -3  There have been too many steps between output points.
c                  Quit or Restart Integration
c     
c     WHETHER THE CURRENT STEP IS SUCCESSFUL.
c-----------------------------------------------------------------------
c
c     ESTIMATE ERRORS AT ORDERS K,K-1,K-2

      IF(INFO(IFIRST) .NE. 0) ESTOLD = EST
      enorm = ddasnm(neq,e,wt,rwork,iwork)
      erk   = sigma(k+1)*enorm
      terk  = (k+1)*erk
      est   = erk
      knew  = k
      if (k .eq. 1) go to 350
c         To check if order should be decreased
      do 310  i=1, neq
        delta(i) = phi(i,kp1) + e(i)
  310 continue
      erkm1  = sigma(k)*ddasnm(neq,delta,wt,rwork,iwork)
      terkm1 = k*erkm1
      if (k .gt. 2) go to 320
      if (terkm1 .le. 0.5d0*terk) go to 340
      go to 350
  320 continue
      do 330  i=1, neq
  330 delta(i) = phi(i,k) + delta(i)
      erkm2  = sigma(k-1)*ddasnm(neq,delta,wt,rwork,iwork)
      terkm2 = (k-1)*erkm2
      if (max(terkm1,terkm2) .gt. terk) go to 350
c     LOWER THE ORDER
  340 continue
      knew = k - 1
      est  = erkm1
c
c
c     CALCULATE THE LOCAL ERROR FOR THE CURRENT STEP
c     TO SEE IF THE STEP WAS SUCCESSFUL
  350 continue
      err = ck*enorm
      if (err .gt. 1.0d0) go to 490
c-----------------------------------------------------------------------
c     BLOCK 5
c     THE STEP IS SUCCESSFUL. DETERMINE
c     THE BEST ORDER AND STEPSIZE FOR
c     THE NEXT STEP. UPDATE THE DIFFERENCES
c     FOR THE NEXT STEP.
c-----------------------------------------------------------------------
      idid         = 1
      iwork(lnst)  = iwork(lnst) + 1
      kdiff        = k - iwork(lkold)
      iwork(lkold) = k
      rwork(lhold) = h
c
c
c     ESTIMATE THE ERROR AT ORDER K+1 UNLESS:
c        ALREADY DECIDED TO LOWER ORDER, OR
c        ALREADY USING MAXIMUM ORDER, OR
c        STEPSIZE NOT CONSTANT, OR
c        ORDER RAISED IN PREVIOUS STEP
      if ((knew .eq. km1) .or. (k .eq. iwork(lmxord))) iwork(lphase) = 1
      if (iwork(lphase) .eq. 0) go to 400
      if (knew .eq. km1) go to 390
      if (k .eq. iwork(lmxord)) go to 410
C Free the order and let the error sequence decide if the order
C gets increased or otherwise changed.
      if (INFO(ISMOOT) .ne. 0 .and. ((kp1 .ge. iwork(lns))
     *    .or. (kdiff .eq. 1))) go to 410
      do 360  i = 1, neq
c         stepping past TOUT.  Y(:)} is obtained by interpolation.
c         YPRIME(:) is obtained by interpolation.
c      4  The integration has paused for reverse communication.  Respond
c         based on the values of IRES.
c                  Task Interupted
c     -1  IRES set to -2 by the user.
c     -2  Accuracy requested exceeds machine precision.  RTOL and ATOL
c         have been increased.
c     -3  There have been too many steps between output points.
c                  Quit or Restart Integration
c     
  360 delta(i) = e(i) - phi(i,kp2)
      erkp1  = (1.0d0/(k+2))*ddasnm(neq,delta,wt,rwork,iwork)
      terkp1 = (k+2)*erkp1
      if (k .gt. 1) go to 370
      if (terkp1 .ge. 0.5d0*terk) go to 410
      go to 380
  370 if (terkm1 .le. min(terk,terkp1)) go to 390
      if (terkp1.ge.terk .or. k.eq.iwork(lmxord)) go to 410
c
c     RAISE ORDER
  380 k = kp1
      est = erkp1
      if (k .lt. iwork(lmxord)) then
        do 385 i = 1, neq
          phi(i,k+2) = 0.d0
 385    continue
      end if
      go to 410
c
c     LOWER ORDER
  390 k = km1
      est = erkm1
      go to 410
c
c     IF IWORK(LPHASE) = 0, INCREASE ORDER BY 1 AND MULTIPLY STEPSIZE BY
c     FACTOR 2
  400 k = kp1

      IF(INFO(IFIRST) .EQ. 0) ESTOLD = EST
C Selectively use Soderlind's smoothing:
      IF(info(ISMOOT).ne. 0) THEN   
        hnew = h*2.0d0
        h    = hnew
        go to 440
      END IF

c
c
c     DETERMINE THE APPROPRIATE STEPSIZE FOR
c     THE NEXT STEP.
  410 CONTINUE
      IF(info(ISMOOT).ne.0) THEN
        hnew = h
        temp2 = k + 1
        r     = (2.0d0*est+0.0001d0)**(-1.0d0/temp2)
        if (r .lt. 2.0d0) go to 420
        hnew = 2.0d0*h
        go to 430

  420   if (r .gt. 1.0d0) go to 430
        r    = max(0.5d0,min(0.9d0,r))
        hnew = h*r
  430   h = hnew
        go to 440
      ELSE

C New Soderlind logic goes here:
        temp2 = k + 1
        call ddasgh(EST,ESTOLD,RATIO,H,HNEW,TEMP2)
        h=hnew
      END IF
c
c     UPDATE DIFFERENCES FOR NEXT STEP
  440 continue
      if (iwork(lkold) .eq. iwork(lmxord)) go to 460
      do 450  i=1, neq
  450 phi(i,kp2) = e(i)
  460 continue
      do 470  i=1, neq
  470 phi(i,kp1) = phi(i,kp1) + e(i)
      do 480  j1=2, kp1
         j = kp1 - j1 + 1
         do 475  i=1, neq
            phi(i,j) = phi(i,j) + phi(i,j+1)
  475    continue 
  480 continue
      iwork(lk) = k
      INFO(IFIRST) = 1
      return
c-----------------------------------------------------------------------
c     BLOCK 6
c     THE STEP IS UNSUCCESSFUL. RESTORE X,PSI,PHI
c     DETERMINE APPROPRIATE STEPSIZE FOR
c     CONTINUING THE INTEGRATION, OR EXIT WITH
c     AN ERROR FLAG IF THERE HAVE BEEN MANY
c     FAILURES.
c-----------------------------------------------------------------------
  490 iwork(lphase) = 1
c
c     RESTORE X,PHI,PSI
      x = xold
      if (kp1 .lt. nsp1) go to 520
      do 510  j=nsp1, kp1
         temp1 = 1.0d0/beta(j)
         do 500  i=1, neq
  500    phi(i,j) = temp1*phi(i,j)
  510 continue
  520 continue
      do 530  i=2, kp1
  530 psi(i-1) = psi(i) - h
c
c
c     TEST WHETHER FAILURE IS DUE TO CORRECTOR ITERATION
c     OR ERROR TEST
      if (convgd) go to 560
      iwork(lctf) = iwork(lctf) + 1
c
c
c     THE NEWTON ITERATION FAILED TO CONVERGE WITH
c     A CURRENT ITERATION MATRIX.  DETERMINE THE CAUSE
c     OF THE FAILURE AND TAKE APPROPRIATE ACTION.
      if (ires .eq. 0) go to 540
c
c     THE ITERATION MATRIX IS SINGULAR. REDUCE
c     THE STEPSIZE BY A FACTOR OF 4. IF
c     THIS HAPPENS THREE TIMES IN A ROW ON
c     THE SAME STEP, RETURN WITH AN ERROR FLAG
      nsf = nsf + 1
      r   = 0.25d0
      h   = h*r
      if ((nsf.lt.3) .and. (abs(h) .ge. rwork(lhmin))) go to 600
      idid = -8
      go to 590
c
c
c     THE NEWTON ITERATION FAILED TO CONVERGE FOR A REASON
c     OTHER THAN A SINGULAR ITERATION MATRIX.  IF IRES = -2, THEN
c     RETURN.  OTHERWISE, REDUCE THE STEPSIZE AND TRY AGAIN, UNLESS
c     TOO MANY FAILURES HAVE OCCURRED.
  540 continue
      if (ires .eq. -2) then
         idid = -1
      else
         ncf = ncf + 1
         r = 0.25d0
         h = h*r
         if ((ncf.lt.10) .and. (abs(h) .ge. rwork(lhmin))) go to 600
         idid = -7
         if (ires .eq. -1) idid = -4
         if (nef .ge. 3) idid = -9
      end if
      go to 590
c
c
c     THE NEWTON SCHEME CONVERGED, AND THE CAUSE
c     OF THE FAILURE WAS THE ERROR ESTIMATE
c     EXCEEDING THE TOLERANCE.
  560 nef = nef + 1
      iwork(letf) = iwork(letf) + 1
      if (nef .gt. 1) go to 570
c
c     ON FIRST ERROR TEST FAILURE, KEEP CURRENT ORDER OR LOWER
c     ORDER BY ONE.  COMPUTE NEW STEPSIZE BASED ON DIFFERENCES
c     OF THE SOLUTION.
      k     = knew
      temp2 = k + 1
      IF(INFO(ISMOOT) .ne. 0) THEN
        r     = 0.90d0*(2.0d0*est+0.0001d0)**(-1.0d0/temp2)
        r     = max(0.25d0,min(0.9d0,r))
        h     = h*r
      ELSE
        CALL ddasgh(EST,ESTOLD,RATIO,H,HNEW,TEMP2)
        H = HNEW
      END IF
      if (abs(h) .ge. rwork(lhmin)) go to 600
      idid = -6
      go to 590

c
c     ON SECOND ERROR TEST FAILURE, USE THE CURRENT ORDER OR
c     DECREASE ORDER BY ONE.  REDUCE THE STEPSIZE BY A FACTOR OF
c     FOUR.
  570 if (nef .gt. 2) go to 580
      iwork(lk) = knew
      h = 0.25d0*h
      if (abs(h) .ge. rwork(lhmin)) go to 600
      idid = -6
      go to 590
c
c     ON THIRD AND SUBSEQUENT ERROR TEST FAILURES, SET THE ORDER TO
c     ONE AND REDUCE THE STEPSIZE BY A FACTOR OF FOUR.
  580 iwork(lk) = 1
      h = 0.25d0*h
      if (abs(h) .ge. rwork(lhmin)) go to 600
      idid = -6
      go to 590
c
c     FOR ALL CRASHES, RESTORE Y TO ITS LAST VALUE,
c     INTERPOLATE TO FIND YPRIME AT LAST X, AND RETURN
  590 continue
      call ddasin (x, x, y, yprime, neq, k, phi, psi)
      k=k
      return
c
c
c     GO BACK AND TRY THIS STEP AGAIN
  600 CONTINUE
      
      go to 20
c
c------END OF SUBROUTINE DDASTP------
      end
