      subroutine ddas1 (x, y, yprime, neq, ldd, ddasf, info, h, wt,
     &                   idid, phi, delta, e, wm, iwork, rwork)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2009-09-30 ddas1 Krogh  Removed unused variable sc.
c>> 2008-08-26 ddas1 Hanson add argument of leading dimension to ddasf
c>> 2006-05-18 ddas1 Hanson added checks for inconsistent constraints
c>> 2006-04-16 ddas1 Krogh  declared j.
c>> 2004-04-13 ddas1 Hanson cleared phi array at outset.
c>> 2003-03-06 ddas1 Hanson changed weights to use reciprocals
c>> 2001-12-12 ddas1 Krogh  Changed code for reverse communication
c>> 2001-11-23 ddas1 Krogh  Changed many names per library conventions.
c>> 2001-11-01 ddas1 Hanson Provide code to Math a la Carte.
c--D replaces "?": ?das1, ?dasf, ?dasj, ?daslv, ?dasco,
c--&  ?daslx, ?dasnm, ?dasdb
c***BEGIN PROLOGUE  DDAS1
c***SUBSIDIARY
c***PURPOSE  Initialization routine for DDASLX.
c***LIBRARY   SLATEC (DDASLX)
c***TYPE      DOUBLE PRECISION (SDAS1-S, DDAS1-D)
c***AUTHOR  Petzold, Linda R., (LLNL)
c***DESCRIPTION
c-----------------------------------------------------------------
c     DDAS1 TAKES ONE STEP OF SIZE H OR SMALLER WITH THE BACKWARD EULER
c     METHOD, TO FIND YPRIME.  X AND Y ARE UPDATED TO BE CONSISTENT WITH
c     THE NEW STEP.  A MODIFIED DAMPED NEWTON ITERATION IS USED TO SOLVE
c     THE CORRECTOR ITERATION.
c
c     THE INITIAL GUESS FOR YPRIME IS USED IN THE PREDICTION, AND IN
c     FORMING THE ITERATION MATRIX, BUT IS NOT INVOLVED IN THE ERROR
c     TEST. THIS MAY HAVE TROUBLE CONVERGING IF THE INITIAL GUESS IS NO
c     GOOD, OR IF G(X,Y,YPRIME) DEPENDS NONLINEARLY ON YPRIME.
c
c     THE PARAMETERS REPRESENT:
c     X --         INDEPENDENT VARIABLE
c     Y --         SOLUTION VECTOR AT X
c     YPRIME --    DERIVATIVE OF SOLUTION VECTOR
c     NEQ --       NUMBER OF EQUATIONS
c     LDD --       FIRST (ROW) DIMENSION FOR MATIX.
c     DDASF --     THE USER DEFINED ROUTINE.
c     INFO --      THE USER SUPPLIED ARRAY DEFINING OPTIONS.
c     H --         STEPSIZE. IMDER MAY USE A STEPSIZE SMALLER THAN H.
c     WT --        VECTOR OF WEIGHTS FOR ERROR CRITERION
c     IDID --      COMPLETION CODE WITH THE FOLLOWING MEANINGS
c       IDID= 1 -- YPRIME WAS FOUND SUCCESSFULLY
c       IDID=-10 -- DDAS1 FAILED TO FIND YPRIME
c       IDID=-28 -- IRES NOT RESET WHEN IRES=5 WITH USER DEFINED MATRIX.
c     PHI --       WORK SPACE FOR DDAS1
c     DELTA,E --   WORK SPACE FOR DDAS1
c     WM,IWORK --  REAL AND INTEGER ARRAYS STORING
c                  MATRIX INFORMATION
c     RWORK --     THE USUAL WORK ARRAY.
c
c-----------------------------------------------------------------
c***ROUTINES CALLED  DDASF, DDASNM, DDASLV
c***REVISION HISTORY  (YYMMDD)
c   830315  DATE WRITTEN
c   901009  Finished conversion to SLATEC 4.0 format (F.N.Fritsch)
c   901019  Merged changes made by C. Ulrich with SLATEC 4.0 format.
c   901026  Added explicit declarations for all variables and minor
c           cosmetic changes to prologue.  (FNF)
c   901030  Minor corrections to declarations.  (FNF)
c   981118  Use one external user routine, RJH.
c***END PROLOGUE  DDAS1
c
      integer    neq, ldd, idid, iwork(*), info(16)
      integer    locate
      double precision x, y(*), yprime(*), h, wt(*), phi(neq,*),
     &     delta(*), e(*), wm(*), rwork(*)
      external   ddasf
c
      external   ddasj, ddasnm, ddaslv
      double precision ddasnm
c
      integer    i, ires, j, jcalc, m, maxit, mjac, ncf,
     &           nef, nsf
      double precision damp, delnrm, err, oldnrm, r, rate, s,
     &           xold, ynorm
      logical    convgd
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

      data maxit/10/, mjac/5/
      data damp/0.75d0/
c
      save      
c
c---------------------------------------------------
c     BLOCK 1.
c     INITIALIZATIONS.
c---------------------------------------------------
c
c***FIRST EXECUTABLE STATEMENT  DDAS1
      if (iwork(revloc) .ne. 0) then
         ires = iwork(lires)
         locate = mod(iwork(revloc), 8)
         iwork(revloc) = iwork(revloc) / 8
c                1   2    3    4   5    6
         go to (50, 70, 100, 320, 75, 360), locate
      end if
c     Control drops through here on first call:
      idid   = 1
      nef    = 0
      ncf    = 0
      nsf    = 0
      xold   = x
      ynorm  = ddasnm(neq,y,wt,rwork,iwork)
c
c     SAVE Y AND YPRIME IN PHI
      do 10  i=1, neq
         phi(i,1) = y(i)
         phi(i,2) = yprime(i)
   10 continue
c
c  Clear rest of phi(*,*) array to define differences.
      do 15 j=3, iwork(lmxord)+1
        do 15 i=1,neq
           phi(i,j) = 0.d0
   15 continue
              
c
c
c----------------------------------------------------
c     BLOCK 2.
c     DO ONE BACKWARD EULER STEP.
c----------------------------------------------------
c
c     SET UP FOR START OF CORRECTOR ITERATION
   20 rwork(lcj) = 1.0d0/h
      x = x + h
c
c     PREDICT SOLUTION AND DERIVATIVE
      do 30  i=1, neq
         y(i) = y(i) + h*yprime(i)
   30 continue
c
      jcalc  = -1
      m      = 0
      convgd = .true.
c
c
c     CORRECTOR LOOP.
   40 iwork(lnre) = iwork(lnre) + 1
      ires = 1
      if (info(idb) .ne. 0) call ddasdb(2, neq, x, y, yprime, info,
     &     rwork, iwork, ires,rwork, rwork) 
      if (iwork(lmat) .ge. 0) then
         call ddasf (x, y, yprime, delta, wm, LDD, rwork(lcj), ires,
     &   rwork, iwork) 
      else
         iwork(revloc) = 8 * iwork(revloc) + 1
         iwork(lires) = ires
         return
      end if
c REVERSE ENTRY 1:
   50 continue
c     Test signal for invalid request.
      if (info(idb) .ne. 0) call ddasdb(3, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (ires .lt. 0) then
         if (ires .eq. -1) go to 190
         if (ires .eq. -2) go to 240
         info(idb) = -ires
         ires = 0
      end if
c
c
c     EVALUATE THE ITERATION MATRIX
      if (jcalc .ne. -1) go to 80
      iwork(lnje) = iwork(lnje) + 1
      jcalc     = 0
c REVERSE ENTRY 2:
   70 continue
      call ddasj (neq, ldd, x, y, yprime, delta, h, wt, e, wm, iwork,
     &             rwork, ddasf, info, ires)

c     See if reverse communication needed:
      if (iwork(revloc) .ne. 0) then
         if (iwork(revloc) .lt. 0) then
            iwork(revloc) = 5
         else
            iwork(revloc) = 8 * iwork(revloc) + 2
         end if
         return
      end if
c 
c REVERSE ENTRY 5:  After user computes and factors the matrix.
   75 continue
      s = 1000000.d0
      if (ires .lt. 0) then
         if (ires .eq. -1) go to 190
         if (ires .eq. -2) go to 240
         info(idb) = -ires
         ires = 0
      end if
      if (ires .ne. 0) go to 190
      nsf = 0
c
c
c
c     MULTIPLY RESIDUAL BY DAMPING FACTOR
   80 continue
      do 90  i=1, neq
         delta(i) = delta(i)*damp
   90 continue
c
c     COMPUTE A NEW ITERATE (BACK SUBSTITUTION)
c     STORE THE CORRECTION IN DELTA
c
      call ddaslv (neq, ldd, x, y, yprime, delta, ddasf, info,
     &             iwork, rwork)
      if (iwork(revloc) .ne. 0) then
         iwork(revloc) = 3
         return
      end if
c REVERSE ENTRY 3:
  100 continue
c
c     UPDATE Y AND YPRIME
      do 110  i=1, neq
         y(i) = y(i) - delta(i)
         yprime(i) = yprime(i) - rwork(lcj)*delta(i)
  110 continue
      
c
c     TEST FOR CONVERGENCE OF THE ITERATION.
c
      delnrm = ddasnm(neq,delta,wt,rwork,iwork)
      if (delnrm .le. 100.d0 * rwork(lround) * ynorm) go to 200
c
      if (m .gt. 0) go to 120
      oldnrm = delnrm
      go to 130
c
  120 rate = (delnrm/oldnrm)**(1.0d0/m)
      if (rate .gt. 0.90d0) go to 190
      s = rate/(1.0d0-rate)
c
  130 if (s*delnrm .le. 0.33d0) go to 200
c
c
c     THE CORRECTOR HAS NOT YET CONVERGED. UPDATE
c     M AND AND TEST WHETHER THE MAXIMUM
c     NUMBER OF ITERATIONS HAVE BEEN TRIED.
c     EVERY MJAC ITERATIONS, GET A NEW
c     ITERATION MATRIX.
c
      m = m + 1
      if (m .ge. maxit) go to 190
c
      if ((m/mjac)*mjac .eq. m) jcalc = -1

      go to 40
c
c
c     THE ITERATION HAS CONVERGED.

c     EXITS FROM CORRECTOR LOOP.
  190 convgd = .false.
      go to 220
  200 continue
c
c
c
c-----------------------------------------------------
c     BLOCK 3.
c     THE CORRECTOR ITERATION CONVERGED.
c     DO ERROR TEST.
c-----------------------------------------------------
c
      do 210  i=1, neq
         e(i) = y(i) - phi(i,1)
  210 continue
      err = ddasnm(neq,e,wt,rwork,iwork)
c
      if (err .le. 1.0d0) go to 280
c
c
c
c--------------------------------------------------------
c     BLOCK 4.
c     THE BACKWARD EULER STEP FAILED. RESTORE X, Y
c     AND YPRIME TO THEIR ORIGINAL VALUES.
c     REDUCE STEPSIZE AND TRY AGAIN, IF
c     POSSIBLE.
c---------------------------------------------------------
c
  220 continue
      x = xold
      do 230  i=1, neq
         y(i) = phi(i,1)
         yprime(i) = phi(i,2)
  230 continue
c
      if (convgd) go to 260
      if (ires .ne. 0) then
         nsf = nsf + 1
         h   = h*0.25d0
         if ((nsf.lt.3) .and. (abs(h) .ge. rwork(lhmin))) go to 270
         idid = -10
         go to 280
      end if
      if (ires .ne. -2) go to 250
  240 idid = -10
      go to 280
  250 ncf = ncf + 1
      h = h*0.25d0
      if ((ncf.lt.10) .and. (abs(h) .ge. rwork(lhmin))) go to 270
      idid = -10
      go to 280
c
  260 nef = nef + 1
      r = 0.90d0/(2.0d0*err+0.0001d0)
      r = max(0.1d0,min(0.5d0,r))
      h = h*r
      if ((abs(h).ge.rwork(lhmin)) .and. (nef.lt.10)) go to 270
      idid = -10
      go to 280
  270 go to 20
c
  280 continue
c     CHECK for CONSTRAINTS
      if (iwork(lcnstr) .eq. 0 ) go to 400
      ires = 5

      if (iwork(lcnstr) .ge. 0) then
         if (info(idb) .ne. 0) call ddasdb(2, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
         call ddasf (x, y, yprime, delta, wm, LDD, rwork(lcj),
     &   ires, rwork, iwork)
      else
         iwork(revloc) = 4
         iwork(lires) = ires
         return
      end if
c REVERSE ENTRY 4:
  320 continue
      if (info(idb) .ne. 0) call ddasdb(3, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork) 
      if (ires .ne. 5) then
         if (ires .lt. 0) then
            if (ires .eq. -1) go to 190
            if (ires .eq. -2) go to 240
            info(idb) = -ires
            ires = 0
         end if
      else 
         i = abs(iwork(lmat))
         if ((i .eq. 5) .or. (i .eq. 6)) then
            idid = -28
            go to 400
         end if
c     If IRES=5, compute a weighted least-distance Newton step
c     back to the constraints. Put move into delta(:).
         call ddasco (wm(neq+1), ldd, neq, wt, delta)

      end if
C Check that the moves back to the constraints are smaller than
c sizes of the error tolerances.  If they are then apply the
c moves.  Otherwise evaluate the constraints and return an error flag.
      do 330 i=1, neq
C Recall that tol_i = 1/ wt_i.        
        if(abs(delta(i)*wt(i)) .gt. 1) go to 350        
  330 continue       
C If moving onto the constraints was reasonable, make the move.               
      do 340  i=1, neq
        y(i) = y(i) - delta(i)
        yprime(i) = yprime(i) - rwork(lcj)*delta(i)
 340  continue
      go to 400
 350  continue
C Here the projection back to the constraints was 
C .gt. the requested tolerance in some component.
c  Evaluate the constraints at the current values. 
      if (iwork(lmat) .ge. 0) then
         call ddasf (x, y, yprime, delta, wm, LDD, rwork(lcj), ires,
     &   rwork, iwork) 
      else
         iwork(revloc) = 6
         iwork(lires) = ires
         return
      end if
c REVERSE ENTRY 6:
 360  continue
C This error flag will result in printing a message 
c and residuals on the constraints. 
      IDID = -29      
c-------------END OF SUBROUTINE DDAS1----------------------
 400  return
      end
