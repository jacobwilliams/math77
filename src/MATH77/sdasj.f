      subroutine sdasj (neq, ldd, x, y, yprime, delta, h, wt, e,
     &                  wm, iwork, rwork, sdasf, info, ires)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2010-08-26 sdasj Krogh Changed declaration of info to info(*).
c>> 2008-08-26 sdasj Hanson add argument of leading dimension to sdasf
c>> 2001-12-12 sdasj Krogh  Changed code for reverse communication
c>> 2001-11-23 sdasj Krogh  Changed many names per library conventions.
c>> 2001-11-04 sdasj Krogh  Fixes for F77 and conversion to single & C
c>> 2001-11-01 sdasj Hanson Provide code to Math a la Carte.
c--S replaces "?": ?dasj, ?dasf, ?daslx, ?dasdb, ?swap, ?gbfa, ?gefa
c***BEGIN PROLOGUE  SDASJ
c***SUBSIDIARY
c***PURPOSE  Compute the iteration matrix for SDASLX and form the
c            LU-decomposition.
c***LIBRARY   SLATEC (SDASLX)
c***AUTHOR  Petzold, Linda R., (LLNL)
c***DESCRIPTION
c-----------------------------------------------------------------------
 
c   THIS ROUTINE COMPUTES THE ITERATION MATRIX PD=DG/DY+CJ*DG/DYPRIME
c   (WHERE G(X,Y,YPRIME)=0, AND CJ IS CONTAINED IN RWORK(LCJ)).
c   HERE PD IS COMPUTED BY THE USER-SUPPLIED ROUTINE SDASF IF |INFO(5)|
c   IS 2, 4, 9, OR 13 AND IS COMPUTED BY FINITE DIFFERENCES IF |INFO(5)|
c   IS 1, 3, 8, OR 12.  IF INFO(5) < 0, THEN COMPUTATIONS OF J ARE DONE
c   BY USING REVERSE COMMUNICTION IF THESE COMPUTATIONS ARE DONE BY THE
c   THE USER.  WHEN |INFO(5) = 5 OR 6, THE USER IS DOING ALL
c   COMPUTATIONS ASSOCIATED WITH J AND THE ASSOCIATED LINEAR ALGEBRA.
c   THE PARAMETERS HAVE THE FOLLOWING MEANINGS.
c   NEQ      = NUMBER OF EQUATIONS
c   LDD      = FIRST (ROW) DIMENSION OF THE MATRIX.
c   X        = CURRENT VALUE OF THE INDEPENDENT VARIABLE.
c   Y        = ARRAY CONTAINING PREDICTED VALUES
c   YPRIME   = ARRAY CONTAINING PREDICTED DERIVATIVES
c   DELTA    = RESIDUAL EVALUATED AT (X,Y,YPRIME)
c             (USED ONLY FOR BAND MATRICES AND FOR REVERSE
c              COMMUNICATION.)
c   H        = CURRENT STEPSIZE IN INTEGRATION
c   WT       = VECTOR OF WEIGHTS FOR COMPUTING NORMS
c   E        = WORK SPACE (TEMPORARY) OF LENGTH NEQ
c   WM       = REAL WORK SPACE FOR MATRICES. ON
c              OUTPUT IT CONTAINS THE LU DECOMPOSITION
c              OF THE ITERATION MATRIX.
c   IWORK    = INTEGER WORK SPACE CONTAINING MATRIX INFORMATION
c   SDASF    = NAME OF THE EXTERNAL USER-SUPPLIED ROUTINE
c              TO EVALUATE THE RESIDUAL FUNCTION G(X,Y,YPRIME)
c   IRES     = FLAG WHICH IS EQUAL TO ZERO IF NO ILLEGAL VALUES
c              IN SDASF, AND LESS THAN ZERO OTHERWISE.  (IF IRES
c              IS LESS THAN ZERO, THE MATRIX WAS NOT COMPLETED)
c-----------------------------------------------------------------------
c***ROUTINES CALLED  SGBFA, SGEFA
c***REVISION HISTORY  (YYMMDD)
c   830315  DATE WRITTEN
c   901009  Finished conversion to SLATEC 4.0 format (F.N.Fritsch)
c   901010  Modified three MAX calls to be all on one line.  (FNF)
c   901019  Merged changes made by C. Ulrich with SLATEC 4.0 format.
c   901026  Added explicit declarations for all variables and minor
c           cosmetic changes to prologue.  (FNF)
c   901101  Corrected PURPOSE.  (FNF)
c***END PROLOGUE  SDASJ
c
      integer    ldd, neq, iwork(*), ires, info(*)
      real             x, y(*), yprime(*), delta(*), h, wt(*),
     &           e(*), wm(*), rwork(*)
      external   sdasf
c
      external   sgbfa, sgefa
c
      integer    i, i1, i2, ii, lmata, ipsave, isave, j, k, l,
     &           mba, mband, meb1, meband, msave,
     &           n, nrow
      integer    ier, locate
      real             del, delinv, squr, ypsave, ysave
c
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
 
      save
c***FIRST EXECUTABLE STATEMENT  SDASJ
      if (iwork(revloc) .ne. 0) then
         ires = iwork(lires)
         locate = mod(iwork(revloc), 8)
         iwork(revloc) = iwork(revloc) / 8
         go to (90, 50, 210, 170), locate
      end if
 
c     The first entry drops through here.
      lmata = abs(iwork(lmat))
c             1  2   3   4   5   6  7  8   9  10 11 12  13  14
      go to (30,10,140,120,250,280,30,10,140,120,30,10,140,120), lmata
      go to 30
c
c
c     Dense full user-supplied matrix
   10 CONTINUE
      do 20  i=1, iwork(lnpd)
         wm(i) = 0.0e0
   20 continue
      ires = 2
      if (info(idb) .ne. 0) call sdasdb(2, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (iwork(lmat) .ge. 0) then
         call sdasf(x, y, yprime, e, wm, LDD, rwork(lcj), ires,
     $   rwork, iwork)
         go to 90
      end if
c     Reverse communication to compute the partials.
      iwork(revloc) = 1
      iwork(lires) = ires
c     This location will be the same as WM when the user responds.
      go to 240
c
c
c     DENSE FINITE-DIFFERENCE-GENERATED MATRIX
   30 continue
      ires = 1
      nrow = 0
      squr = sqrt(rwork(lround))
      i = 0
c         Loop over the columns to generate the approximate Jacobian.
   40 i = i + 1
      if (i .gt. neq) go to 80
      del       = squr*max(abs(y(i)),max(abs(h*yprime(i)),abs(wt(i))))
      del       = sign(del,h*yprime(i))
      del       = (y(i)+del) - y(i)
      ysave     = y(i)
      ypsave    = yprime(i)
      y(i)      = y(i) + del
      yprime(i) = yprime(i) + rwork(lcj)*del
      if (info(idb) .ne. 0) call sdasdb(2, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (iwork(lmat) .ge. 0) then
         call sdasf(x, y, yprime, e, wm, LDD, rwork(lcj), ires,
     &   rwork, iwork)
         go to 60
      else
         iwork(revloc)  = 2
         iwork(lires) = ires
c     This is placed in the start of DELTA(*), in units of RWORK(*).
         call sswap (neq, e, 1, delta, 1)
         go to 240
      end if
c REVERSE ENTRY 2:
   50 continue
      call sswap (neq, e, 1, delta, 1)
   60 continue
      if (info(idb) .ne. 0) call sdasdb(3, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (ires .lt. 0) then
         if (ires .ge. -2) go to 240
         info(idb) = -ires
         ires = 1
      end if
      delinv = 1.0e0/del
      do 70  l=1, neq
   70 wm(nrow+l) = (e(l)-delta(l))*delinv
      nrow      = nrow + neq
      y(i)      = ysave
      yprime(i) = ypsave
      go to 40
   80 continue
c
c
c     DO DENSE-MATRIX LU DECOMPOSITION ON PD
c REVERSE ENTRY 1:
   90 continue
      if (info(idb) .ne. 0) call sdasdb(3, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (ires .lt. 0) then
         if (ires .ge. -2) go to 240
         info(idb) = -ires
         ires = 0
      end if
      if (lmata .gt. 4) then
         ires = 3
         if (lmata .le. 10) go to 260
         go to 290
      else
         call sgefa (wm, ldd, neq, iwork(lipvt), ier)
         if (ier .eq. 0) ires = 0
      end if
      go to 240
c
c     BANDED USER-SUPPLIED MATRIX
  120 do 130  i=1, iwork(lnpd)
         wm(i) = 0.0e0
  130 continue
      ires = 2
      if (info(idb) .ne. 0) call sdasdb(2, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (iwork(lmat) .lt. 0) then
         iwork(revloc)  = 3
         iwork(lires) = ires
         go to 240
      end if
      call sdasf (x, y, yprime, e, wm, LDD, rwork(lcj), ires,
     & rwork, iwork)
      meband = 2*iwork(lml) + iwork(lmu) + 1
      go to 210
 
c
c
c     BANDED FINITE-DIFFERENCE-GENERATED MATRIX
  140 mband = iwork(lml) + iwork(lmu) + 1
      mba    = min(mband,neq)
      meband = mband + iwork(lml)
      meb1   = meband - 1
      msave  = (neq/mband) + 1
      isave  = iwork(ntemp) - 1
      ipsave = isave + msave
      ires = 1
      squr = sqrt(rwork(lround))
      j = 0
  150 continue
      j = j + 1
      if (j .gt. mba) go to 220
      do 160  n=j, neq, mband
         k           = (n-j)/mband + 1
         wm(isave+k) = y(n)
         wm(ipsave+k) = yprime(n)
         del  = squr*max(abs(y(n)),max(abs(h*yprime(n)),abs(wt(n))))
         del  = sign(del,h*yprime(n))
         del  = (y(n)+del) - y(n)
         y(n) = y(n) + del
  160    yprime(n) = yprime(n) + rwork(lcj)*del
      if (info(idb) .ne. 0) call sdasdb(2, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (iwork(lmat) .ge. 0) then
         call sdasf(x, y, yprime, e, wm, LDD, rwork(lcj), ires,
     &   rwork, iwork)
         if (ires .lt. 0) then
            if (ires .ge. -2) go to 240
            info(idb) = -ires
            ires = 0
         end if
         go to 180
      else
         iwork(revloc)  = 4
         ires    = 1
         iwork(lires) = ires
c     This is placed in the start of DELTA(*), in units of RWORK(*).
         call sswap (neq, e, 1, delta, 1)
         go to 240
      end if
c REVERSE ENTRY 4:
  170 continue
      call sswap (neq, e, 1, delta, 1)
  180 continue
      if (info(idb) .ne. 0) call sdasdb(3, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      do 200  n=j, neq, mband
         k        = (n-j)/mband + 1
         y(n)     = wm(isave+k)
         yprime(n) = wm(ipsave+k)
         del      = squr*max(abs(y(n)),max(abs(h*yprime(n)),abs(wt(n))))
         del      = sign(del,h*yprime(n))
         del      = (y(n)+del) - y(n)
         delinv   = 1.0e0/del
         i1       = max(1,(n-iwork(lmu)))
         i2       = min(neq,(n+iwork(lml)))
         ii       = n*meb1 - iwork(lml)
         do 190  i=i1, i2
  190    wm(ii+i) = (e(i)-delta(i))*delinv
  200 continue
      go to 150
c
c     DO LU DECOMPOSITION OF BANDED PD
c REVERSE ENTRY 3
  210 if (info(idb) .ne. 0) call sdasdb(3, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
  220 continue
      if (ires .lt. 0) then
         if (ires .ge. -2) go to 240
         info(idb) = -ires
         ires = 0
      end if
      if (lmata .gt. 4) then
         ires = 3
         if (lmata .le. 10) go to 260
         go to 290
      else
         call sgbfa (wm, meband, neq, iwork(lml), iwork(lmu),
     &        iwork(lipvt), ier)
         if (ier .eq. 0) ires = 0
      end if
  240 continue
      return
c
c    User defined matrix, Get Jacobian and factor in one step
  250 continue
      ires = 2
c    Enters here if already have matrix and want to factor
  260 continue
      if (info(idb) .ne. 0) call sdasdb(2, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      call sdasf (x, y, yprime, e, wm, LDD, rwork(lcj), ires,
     & rwork, iwork)
 
      if (info(idb) .ne. 0) call sdasdb(3, neq, x, y, yprime,
     &    info, rwork, iwork, ires, rwork, rwork)
      if (ires .lt. -2) then
         info(idb) = -ires
         ires = 0
      end if
      go to 240
c    User defined but with reverse communication
  280 ires = 2
c    Enter here if matrix is computed.
  290 continue
      iwork(lires) = ires
      iwork(revloc) = -1
      go to 240
c------ END OF SUBROUTINE SDASJ ------
      end
