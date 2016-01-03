      subroutine sdasdb (kase, neq, t, y, yprime, info, rwork, iwork,
     &     idid, atol, rtol)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2010-08-26 sdasdb Krogh Changed declaration of info to info(*).
c>> 2006-04-09 sdasdb Krogh Declared MDFDAT
c>> 2002-05-29 sdasdb Krogh Fixed prob. giving diagnostic on a compiler
c>> 2001-12-13 sdasdb Krogh Initial code
c--S Replaces "?": ?dasdb, ?daslx, ?dasf, ?dastp, ?mess
c
c Gives pretty print out of variables used by the DAE solver sdaslx.
c
 
c kase tells us from where we were called.
c   0  Entry to sdaslx
c   1  Exit from sdaslx
c   2  Just prior to call to sdasf
c   3  Just after call to sdasf
c  >3  Inside sdastp
c  <0  Presumably a call from user code. In this case -kase defines a
c      two digit number d_1d_0.  d_0 is treated as if it gives kase,
c      and d_1 is treated as if it were the k-th digit for this kase.
 
c     The printing rules are given as a 7 digit number
c     d_6d_5d_4d_3d_2d_1d_0  in info(idb), which is interpreted as
c     follows.
c       d_0 Print on entry to SDASLX
c        = 0  No print.
c        = 1  IDID/IRES, INFO(1), NEQ, T, TOUT
c        = 2  The above + y, y'
c        = 3  The above + Tolerances
c        = 4  The above + INFO (all of it)
c       d_1 Print on exit from SDASLX. Print is as for d_0
c       d_2 Before a call to SDASF (or return for reverse communication
c           that would ordinarily call SDASF).
c        = 0  No print.
c        = 1  Print T, IDID (which is IRES in this case)
c        = 2  The above + anything vectors used in the computations,
c             except for y, and y'.
c        = 3  Print y and y'
c        = 4  Print matrix if used in computation.
c       d_3 As for the case above, except print is for what is in
c           locations just stored to.
c       d_4 Internal print inside subroutine SDASTP.
c        = 0  No print.
c        = 1  Print corrections
c        = 2  Print y and y'
c        = 3  Error weights
c        = 4  difference tables
c        = 5  integration coefficients
c       d_5  Determines how much of WORK and IWORK are printed, when
c            there is other print.
c        = 0  No print.
c        = 1  Always print IWORK(1:16)
c        = 2  Always print WORK(1:9)
c        = 3  Always print both of the above.
c       d_6 For turning off, or limiting the amount of print.
c        = 0  No effect.
c        = 1  No effect, but gives a way to specify a value of 0, 1 or 2
c             when passing a negative value of IRES after starting.
c        > 1  Print data for just this many of the first variables, and
c             just this many of the first rows in matrices when
c             variables or matrices are printed.
 
c     Declarations for the calling sequence
      integer kase, neq, info(*), iwork(*), idid
      real             t, y(*), yprime(*), rwork(*), atol(*), rtol(*)
 
      integer idat(3)
      real             fdat(3)
c     Local variables
      integer i, j, k, ki, km, kx, mattyp, nvec
 
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
c                      Parameters for calls to mess and smess
      integer MEDDIG, NEDDIG, MEFDAT, METDIG, MENTXT, METABS, MERET,
     1   METEXT, METABL, MEIVEC, MEFVEC, MEFMAT
      parameter (MEDDIG =12)
      parameter (NEDDIG =-MEDDIG)
      parameter (METDIG =22)
      parameter (MENTXT = 23)
      parameter (MEFDAT = 25)
      parameter (METABS =32)
      parameter (MERET  =51)
      parameter (METEXT =53)
      parameter (METABL =55)
      parameter (MEIVEC =57)
      parameter (MEFVEC =61)
      parameter (MEFMAT =62)
 
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA INFO() in index order: $Tinfo1=$I$Ttol=$I$Tout=$I$T stop=$I$T$C
c   mat=$I$Tdb=$I$Tmaxh=$I$Th0=$I$Tord=$I$Tcnstr=$I$T$C
c   inityp=$I$Tmxstep=$I$T$E
c   $
cAB $NIWORK() in index order: $Tlml=$I$Tlmu=$I$Tmxord=$I$T$C
c   k=$I$Tkold=$I$Tns=$I$Tnstl=$I$Tnst=$I$Tnre=$I$T$C
c   nje=$I$Tetf=$I$Tctf=$I$Tnpd=$I$Tjcalc=$I$Tphase=$I$T$C
c   revloc=$I$Tmxstep=$I$Te=$I$Twt=$I$Tphi=$I$Twm=$I$T$C
c   ntemp=$I$E
c   $
cAC $NRWORK() in index order: tstop=$F  hmax=$F  h=$F  tn=$F $C
c    cj=$F  cjold=$F  hold=$F  njac=$F  round=$F  hmin=$F$E
      integer LTXTAA,LTXTAB,LTXTAC
      parameter (LTXTAA=  1,LTXTAB=  1,LTXTAC=  1)
      character MTXTAA(1) * (134)
      character MTXTAB(1) * (213)
      character MTXTAC(1) * (112)
      data MTXTAA/'INFO() in index order: $Tinfo1=$I$Ttol=$I$Tout=$I$T s
     *top=$I$Tmat=$I$Tdb=$I$Tmaxh=$I$Th0=$I$Tord=$I$Tcnstr=$I$Tinityp=$I
     *$Tmxstep=$I$T$E'/
      data MTXTAB/'$NIWORK() in index order: $Tlml=$I$Tlmu=$I$Tmxord=$I$
     *Tk=$I$Tkold=$I$Tns=$I$Tnstl=$I$Tnst=$I$Tnre=$I$Tnje=$I$Tetf=$I$Tct
     *f=$I$Tnpd=$I$Tjcalc=$I$Tphase=$I$Trevloc=$I$Tmxstep=$I$Te=$I$Twt=$
     *I$Tphi=$I$Twm=$I$Tntemp=$I$E'/
      data MTXTAC/'$NRWORK() in index order: tstop=$F  hmax=$F  h=$F  tn
     *=$F  cj=$F  cjold=$F  hold=$F  njac=$F  round=$F  hmin=$F$E'/
c End of automaticaaly generated data
c
      integer ltxt1
      parameter (ltxt1 = 23)
      character*(ltxt1) text1(7)
      data text1 /
     1 '$N$N***************  $B', 'From user code$E',
     2 'On entry to SDASLX$E','On exit from SDASLX$E',
     3 'Before call to DDASLF$E','After call to DDASLF$E',
     4 'Inside SDASTP$E' /
      integer mact1(5),mact2(2),mact3(4),mact4(4),mact5(8),mact6(24)
c                       1      2  3       4      5
      data mact1 / METEXT,MENTXT, 2, METEXT, MERET /
      data mact2 / METEXT, MERET /
      data mact3 / METEXT, MEFVEC, 0, MERET /
      data mact4 / METABS, 10, METEXT, MERET /
      data mact5 / METEXT, MEFMAT, 0, 0, 0, 0, 0, MERET /
      data mact6 / METEXT, MEFVEC, 6, METEXT, MEFDAT, 7, MEFVEC, 6,
     &   METEXT, MEFDAT, 13, MEFVEC, 6, METEXT, MEFDAT, 19, MEFVEC, 6,
     &   METEXT, MEFDAT, 25, MEFVEC, 6, MERET /
 
 
      nvec = neq
      ki = kase
      if (ki .lt. 0) then
c                   User code call
         k = -ki
         j = 10
         ki = 0
   10    if ((mod(k, j) .eq. 0) .and. (ki .lt. 3)) then
            j = 10 * j
            ki = ki + 1
            go to 10
         end if
         mact1(3) = 2 * ltxt1 + 1
         call mess(mact1, text1, idat)
         km = 2
      else
         k = info(idb)
         km = 1
      end if
      if (k .ge. 1000000) then
         if (k .lt. 2000000) then
            k = k - 1000000
            if (kase .ge. 0) info(idb) = k
            if (k .eq. 0) return
         end if
         if (k .ge. 2000000) nvec = k / 1000000
      end if
      mattyp = abs(iwork(lmat))
      if (mattyp .gt. 4) then
         mattyp = iwork(lmat) - 6
         if (mattyp .gt. 4) mattyp = mattyp - 4
      end if
 
c          kx defines printing rules for iwork and rwork
      kx = mod(k / 100000, 10)
 
c Get the appropriate digit for the kase we have
      k = mod(k / 10 ** ki, 10)
      if (k .eq. 0) return
c               Always print out a heading
      mact1(3) = (ki + 2) * ltxt1 + 1
      call mess(mact1(km), text1, idat)
 
      mact3(3) = nvec
 
      if (ki .le. 1) then
c              Entry or exit from sdaslx
c First print T, IDID, INFO(1), NEQ, TOUT
         idat(1) = idid
         idat(2) = info(1)
         idat(3) = neq
         fdat(1) = t
         call smess(mact2,
     &     'T = $F,  IDID = $I,  info(1) = $I,  NEQ = $I$E',
     &     idat, fdat)
 
         if (k .eq. 1) go to 100
c Print y and y'
         call smess(mact3, '$NY():$B', idat, y)
         call smess(mact3, 'Y''():$B', idat, yprime)
 
         if (k .eq. 2) go to 100
c Print Tolerances
 
         if (info(itol) .eq. 0) then
            fdat(1) = atol(1)
            fdat(2) = rtol(1)
            call smess(mact2, 'ATOL = $F,  RTOL = $F$E', idat, fdat)
         else
            call smess(mact3, 'ATOL():$B', idat, atol)
            call smess(mact3, 'RTOL():$B', idat, rtol)
         end if
         if (k .eq. 3) go to 100
c Print all of INFO
         call mess(mact4, MTXTAA, info)
 
      else if (ki .le. 3) then
c              Returning from or calling sdasf
c Print T, IRES
         fdat(1) = t
         idat(1) = idid
         call smess(mact2, 'T = $F,  IRES = $I$E', idat, fdat)
 
         if (k .eq. 1) go to 100
c Print vectors wanted
         if ((idat(1) .eq. 1) .or. (idat(1) .eq. 4)) then
            if ((ki .ne. 2) .or. (idat(1) .eq. 4))
     &           call smess(mact3, '$NDELTA: $B', idat, rwork(ldelta))
         else if ((idat(1) .eq. 5) .and. (ki .ne. 2)) then
            if ((kase .lt. 0) .and. (info(1) .ne. 1)) then
c                        Don't have access to info in this case
               mact3(3) = 1
            else
               if ((nvec.eq.neq) .or. (nvec.gt. iwork(lcnstr)))
     &              mact3(3) = iwork(lcnstr)
            end if
            call smess(mact3, '$NDELTA: $B', idat, rwork(ldelta+nvec))
         end if
         if (k .ge. 3) then
            call smess(mact3, '$NY(): $B', idat, y)
            call smess(mact3, 'YPRIME(): $B', idat, yprime)
         end if
         if (k .le. 3) go to 100
         if (idat(1) .le. 1) go to 100
         if ((kase .lt. 0) .and. (info(1) .ne. 1)) go to 100
         if (mattyp .lt. 0) go to 100
c Print the matrix if computed
         i = idat(1)
         if (i .eq. 5) i = 2
         if (i + ki .gt. 4) then
            mact5(3) = neq + iwork(lcnstr)
            mact5(4) = nvec
            mact5(5) = nvec
            if (mattyp .gt. 4) mact5(5) = iwork(1) + iwork(2)
            if (idat(1) .eq. 5) then
               if ((nvec .eq. neq) .or. (nvec .gt. iwork(lcnstr)))
     &              mact5(3) = iwork(lcnstr)
               call smess(mact5, '$NConstraint Rows$E', idat,
     &               rwork(lwm+neq))
            else
               call smess(mact5, '$NMatrix$B', idat, rwork(lwm))
            end if
         end if
      else
c              From inside sdastp
c Print corrections
         call smess(mact3, '$NDELTA: $B', idat, rwork(ldelta+nvec))
         if (k .eq. 1) go to 100
c Print y, y', and corrections.
         call smess(mact3, '$NY(): $B', idat, y)
         call smess(mact3, 'YPRIME(): $B', idat, yprime)
         if (k .eq. 2) go to 100
c Print error weights
         call smess(mact3, 'Weights(): $B', idat, atol)
         if (k .eq. 3) go to 100
c Print difference tables
         mact5(3) = neq
         mact5(4) = nvec
         mact5(5) = iwork(lk)+1
         mact5(6) = 22
         call smess(mact5, '$NDifference Tables$EEq. $#', idat,
     &        rwork(iwork(lphi)))
         mact5(6) = 0
         if (k .eq. 4) go to 100
c Print integration coefficients
         do 80 I = 3, 23, 5
            mact6(I) = iwork(lk)
   80    continue
         call smess(mact6,
     &  'Integration Coefficients$Nalpha: $Bbeta:  $Bgamma: $B'//
     &  'psi:   $Bsigma: $B', idat, rwork(lalpha))
 
      end if
  100 continue
      if (kx .eq. 0) return
      if (mod(kx, 2) .eq. 1) then
c Print IWORK
         if (mattyp .lt. 3) then
            iwork(1) = 0
            iwork(2) = 0
         end if
         call mess(mact4, MTXTAB, iwork)
      end if
      if (kx .lt. 2) return
c Print RWORK
      call smess(mact2, MTXTAC, idat, rwork)
      return
      end
