      subroutine sdaslv (neq, ldd, x, y, yprime, delta, sdasf, info,
     &                   iwork, rwork)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2008-08-26 sdaslv Hanson add argument of leading dimension to sdasf
c>> 2001-12-12 sdaslv Krogh  Changed code for reverse communication
c>> 2001-11-23 sdaslv Krogh  Changed many names per library conventions.
c>> 2001-11-04 sdaslv Krogh  Fixes for F77 and conversion to single
c>> 2001-11-01 sdaslv Hanson Provide code to Math a la Carte.
c--S replaces "?": ?daslv, ?daslx, ?gbsl, ?gesl, ?dasf, ?dasdb
c***BEGIN PROLOGUE  SDASLV
c***SUBSIDIARY
c***PURPOSE  Linear system solver for SDASLX.
c***LIBRARY   SLATEC (SDASLX)
c***TYPE      DOUBLE PRECISION (SDASLV-S, SDASLV-D)
c***AUTHOR  Petzold, Linda R., (LLNL)
c***DESCRIPTION
c-----------------------------------------------------------------------
c     THIS ROUTINE MANAGES THE SOLUTION OF THE LINEAR
c     SYSTEM ARISING IN THE NEWTON ITERATION.
c     MATRICES AND REAL TEMPORARY STORAGE AND
c     REAL INFORMATION ARE STORED IN THE ARRAY rwork(iwork(lwm)).
c     INTEGER MATRIX INFORMATION IS STORED IN
c     THE ARRAY IWORK.
c     FOR A DENSE MATRIX, THE LINPACK ROUTINE
c     SGESL IS CALLED.
c     FOR A BANDED MATRIX,THE LINPACK ROUTINE
c     SGBSL IS CALLED.
 
C     A user routine interior to SDASF or reverse communication
C     may also be called.  That is why the current values of Y(*)
C     and YPRIME(*) are passed to this routine.  For example an
C     iterative solver will typically need this information.
c-----------------------------------------------------------------------
c***ROUTINES CALLED  SGBSL, SGESL, SDASF
c***REVISION HISTORY  (YYMMDD)
c   830315  DATE WRITTEN
c   901009  Finished conversion to SLATEC 4.0 format (F.N.Fritsch)
c   901019  Merged changes made by C. Ulrich with SLATEC 4.0 format.
c   901026  Added explicit declarations for all variables and minor
c           cosmetic changes to prologue.  (FNF)
c   981130  Put in reverse communication and forward communication
c           solution steps, RJH.
c***END PROLOGUE  SDASLV
c
      integer    ldd, neq, iwork(*), info(16), ires
      real             delta(*), rwork(*)
      real             x, y(*), yprime(*)
c
      external   sdasf, sgbsl, sgesl
c
      integer    lmata, meband
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
c
c***FIRST EXECUTABLE STATEMENT  SDASLV
      lmata = abs(iwork(lmat))
      if (lmata .le. 2) then
c                          Dense matrix
         call sgesl(rwork(iwork(lwm)), ldd, neq, iwork(lipvt), delta, 0)
      else if (lmata .le. 4) then
c                          Banded matrix
         meband = 2*iwork(lml) + iwork(lmu) + 1
         call sgbsl(rwork(iwork(lwm)), meband, neq, iwork(lml),
     &        iwork(lmu),iwork(lipvt), delta, 0)
      else if ((lmata .eq. 6) .or. (lmata .ge. 11)) then
c                          User solves with reverse communication
         iwork(lires) = 4
         iwork(revloc) = -1
      else
c                          User solves in sdasf
         ires = 4
         if (info(idb) .ne. 0) call sdasdb(2, neq, x, y, yprime,
     &        info, rwork, iwork, ires, rwork, rwork)
         call sdasf (x, y, yprime, delta, rwork(iwork(lwm)), LDD,
     &     rwork(lcj), ires, rwork, iwork)
 
         if (info(idb) .ne. 0) call sdasdb(3, neq, x, y, yprime,
     &        info, rwork, iwork, ires, rwork, rwork)
      end if
      return
c------END OF SUBROUTINE SDASLV------
      end
