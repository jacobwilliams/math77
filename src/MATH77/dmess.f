      subroutine DMESS (MACT, TEXT, IDAT, FDAT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2009-09-27 DMESS Krogh  Same as below, in another place.
c>> 2009-07-23 DMESS Krogh  Changed ,1x to :1x in write to FMTF.
c>> 2008-06-13 DMESS Krogh  Changed -0's to 0.
c>> 2007-09-08 DMESS Krogh  Fixed definitions of MEVLAS.
c>> 2006-10-08 DMESS Krogh  Another try, see 2005-05-26
c>> 2006-10-08 DMESS Krogh  Fixed minor problem in matrix/vector output.
c>> 2006-10-01 DMESS Krogh  Print NaN's and infity (at least for g77).
c>> 2006-07-01 DMESS Krogh  messxc => dmessxc (and not static) (for C)
c>> 2006-04-07 DMESS Krogh  Major rewrite of code for F.Pt. format.
c>> 2006-04-04 DMESS Krogh  Fixes in C code for vectors & matrices.
c>> 2006-04-02 DMESS Krogh  Added code for output of sparse vector.
c>> 2005-07-10 DMESS Krogh  Small adjustment for last correction.
c>> 2005-05-26 DMESS Krogh  Fixed "*****" output in boundary case.
c>> 2002-05-16 DMESS Krogh  Added way for user to get error count.
c>> 2002-03-27 DMESS Krogh  Fixed crash when number is -INF.
c>> 2001-06-08 DMESS Krogh  Eliminated Hollerith in formats.
c>> 2001-05-25 DMESS Krogh  Added a couple of commas in formats.
c>> 1997-06-17 DMESS Krogh  In C code made messxc, static.
c>> 1996-07-12 DMESS Krogh  Changes to use .C. and C%%.
c>> 1996-03-30 DMESS Krogh  Added external statement.
c>> 1994-10-20 DMESS Krogh  Changes to use M77CON
c>> 1994-09-21 DMESS Krogh  Added CHGTYP code.
c>> 1994-09-08 DMESS Krogh  Added new matrix/vector capabilities.
c>> 1994-08-17 DMESS Krogh  Removed duplicate save statement.
c>> 1994-04-19 DMESS Krogh  Removed blank line from DMESS.
c>> 1993-05-14 DMESS Krogh  Changed TEXT to array of character strings.
c>> 1993-04-14 DMESS Krogh  Fixes for conversion to C. (C%% comments.)
c>> 1992-07-12 DMESS Krogh  Fixed so negative KDFDEF works.
c>> 1992-05-27 DMESS Krogh  Initialized LDFDEF in a data statement.
c>> 1992-05-14 DMESS Krogh  Put common blocks in save statement.
c>> 1992-04-28 DMESS Krogh  Corrected minor error in floating pt. format
c>> 1992-02-28 DMESS Krogh  Initial Code.
c
c--D replaces "?": ?MESS,?MESSXC
c
c Processes Messages -- Actions are controlled by MACT().  See
c comment is subroutine MESS.  This program is for the extra
c argument of type real.
c
c BUF    In common CMESSC, see MESS.
c DOLS   In common for intitialization, not used here.  See MESS.
c EUNIT  In common for intitialization, not used here.  See MESS.
c FDAT   Formal argument -- gives floating point data to print.
c FBIG   Largest magnitude of floating point number to output.
c FSMA   Smalles magnitude floating point number to be printed.
c FMTF   In common CMESSC, format for printing floating point number.
c FMTG   In common CMESSC, user format to use in place of FMTF.
c FMTSP  Format for printing sparse vectors.
c FOUT   Floating point number to be output.
c FSMA   Smallest postitive floating point number.
c ICOL   In common CMESSI, see MESS.
c ID     Number of decimal digits for floating point format statement.
c IDAT   Integer data -- passed to MESS.
c IVAR   In common CMESSI, see MESS.
c IWF    In common CMESSI, see MESS.
c IWG    In common CMESSI, see MESS.
c J      Temporary index.
c K      Temporary index.
c KSMA   Number of leading 0's in "F" format.  If < 0, -KSMA gives the
c    number of extra digits to the left of the decimal point.
c    KSMA depends on abs(smallest number printed).
c KBIG   Number of extra digits before the decimal point required by the
c    largest number to be printed if "F" format is used.
c KDF    In common CMESSI, see MESS.
c KDFDEF In common CMESSI, see MESS.
c KDIAG  In common CMESSI, not used here, see MESS.
c KEXE   Extra space required for E format.
c KF     In common CMESSI, see MESS.
c KLINE  In common CMESSI, see MESS.
c KSCRN  In common CMESSI, see MESS.
c KRES1  In common CMESSI, see MESS.
c KSPEC  In common CMESSI, see MESS.
c LASTER In common CMESSI, not used here, see MESS.
c LASTI  In common CMESSI, see MESS.
c LBUF   In common CMESSI, see MESS.
c LDFDEF Value of KDFDEF for this routine.  (Saved)
c LENBUF In common CMESSI, see MESS.
c LENLIN In common CMESSI, not used here, see MESS.
c LENTRY In common CMESSI, see MESS.
c LHEAD  In common CMESSI, not used here, see MESS.
c LINERR In common CMESSI, not used here, see MESS.
c LINMSG In common CMESSI, not used here, see MESS.
c LOCBEG In common CMESSI, see MESS.
c LPRINT In common CMESSI, not used here, see MESS.
c LSTOP  In common CMESSI, not used here, see MESS.
c LSTRT  In common CMESSI, see MESS.
c MACT   Formal argument, see MESS.
c MDAT   In common CMESSI, not used here, see MESS.
c MEMDA5 In common CMESSI, see MESS.
c MESS   Program called for most of the message processing.
c MPT    In common CMESSI, see MESS.
c MUNIT  In common CMESSI, not used here, see MESS.
c NCOL   In common CMESSI, see MESS.
c NDIM   In common CMESSI, see MESS.
c NEG    1 if any number is < 0, else it is 0.
c NFDAT  In common CMESSI, see MESS.
c NIDAT  In common CMESSI, not used here, see MESS.
c NMDAT  In common CMESSI, not used here, see MESS.
c NTEXT  In common CMESSI, not used here, see MESS.
c OUNIT  In common CMESSI, not used here, see MESS.
c D1MACH External func. giving floating pt. info. about the environment.
c SUNIT  In common CMESSI, not used here, see MESS.
c TEXT   Formal argument, passed to MESS, see there.
c XARGOK In common CMESSI, see MESS.
c
C%% void dmessxc(long int);
c
      external         D1MACH
      integer          MACT(*), IDAT(*)
      double precision FDAT(*)
      character        TEXT(*)*(*)
      character        FMTSP*29
      integer          ICOL, ID, J, K, KSMA, KBIG, KEXE, LDFDEF, NEG
      double precision FBIG, FOUT, FSMA, D1MACH
      save LDFDEF, FMTSP
      save /CMESSI/, /CMESSC/
c++ CODE for .C. is inactive
c      integer  kciwid, kccwid, kcrwid, lbeg, lend, lfprec, lgprec
c      common /MESSCC/ kciwid,kccwid,kcrwid,lbeg,lend,lfprec,lgprec
c++ END
c
c ************************** Data from common block ********************
c
c For comments on these variables, see the listing for MESS.
c
      integer   LENBUF, MEVBAS, MEVLAS
      parameter (LENBUF=250)
      parameter (MEVBAS=10)
      parameter (MEVLAS=33)
      logical          XARG, GOTFMT, XARGOK
      integer          ERRCNT, EUNIT, ICHAR0, IRC, IVAR(MEVBAS:MEVLAS),
     1   IMAG, INC, ITEXT, IWF, IWG, KDF, KDFDEF, KDI, KDIAG, KDJ,
     2   KLINE, KSCRN, KSHIFT, KSPEC, KT, MAXERR, LASTI, LBUF, LENLIN,
     3   LENOUT, LENTRY, LENTXT, LHEAD, LINERR, LINMSG, LOCBEG, LPRINT,
     4   LSTOP, LSTRT, LTEXT, MAXWID(2), MDAT(5), MPT, MUNIT, NCOL,
     5   NDIM, NFDAT, NIDAT, NMDAT, NROW, NTEXT, OUNIT, SUNIT, TABSPA
c
      character BUF*(LENBUF), DOLS*72, FMTC*7, FMTF*20, FMTG*15,
     1  FMTI*7, FMTIM(2)*7, FMTJ*7, FMTR*7, FMTT*15
      common /CMESSI/ SUNIT, LHEAD, KDFDEF, LINMSG, LINERR, MUNIT,
     1   EUNIT, KSCRN, KDIAG, MAXERR, LSTOP, LPRINT, KDF, NTEXT, NIDAT,
     2   NFDAT, NMDAT, MDAT, TABSPA, ERRCNT, ICHAR0, IMAG, INC, IRC,
     3   ITEXT, IWF, IWG, KDI, KDJ, KLINE, KSHIFT, KSPEC, KT, LASTI,
     4   LBUF, LENLIN, LENOUT, LENTRY, LENTXT, LOCBEG, LSTRT, LTEXT,
     5   MAXWID, MPT, NROW, NCOL, NDIM, OUNIT, GOTFMT, XARG, XARGOK
      common /CMESSC / BUF, DOLS, FMTF, FMTG, FMTI, FMTJ, FMTT, FMTIM
      equivalence (IVAR(MEVBAS), SUNIT)
      equivalence (FMTIM(1), FMTR), (FMTIM(2), FMTC)
c
      data LDFDEF / 0 /
c

c ************************* Start of Executable Code *******************
c
      XARGOK = .true.
      if (LDFDEF .eq. 0) then
         LDFDEF = 1 - int(log10(d1mach(3)))
      end if
      KDFDEF = LDFDEF
      KDF = KDFDEF
   10 call MESS (MACT, TEXT, IDAT)
c             4    5    6    7    8    9   10   11
      go to (20, 100, 100, 200, 300, 400, 100, 500), LENTRY-3
      XARGOK = .false.
      LDFDEF = KDFDEF
      return
c                                      Print from FDAT
   20 J = LBUF + 1
      FOUT = FDAT(NFDAT)
      NFDAT = NFDAT + 1
      if (KSPEC .ge. 8) then
         LBUF = LBUF + IWG
C%% messcc.lend = cmessi.lbuf;
C%% cmessc.buf[messcc.lend] = ' ';
C%% if ((j > 1) && (cmessc.buf[j-2] >= '0') &&
C%%    (cmessc.buf[j-2] <= '9')) cmessc.buf[j++ - 1] = ' ';
C%% sprintf(&cmessc.buf[j-1], cmessc.fmtg, cmessi.iwg,
C%%    messcc.lgprec, fout);
C%% if (cmessc.buf[messcc.lend] != 0) {messcc.lbeg=j; dmessxc(kexe);}
         write (BUF(J:LBUF), FMTG) FOUT
         go to 10
      end if
      if (FOUT .le. 0.D0) then
        if (FOUT .eq. 0.D0) then
          FDAT(NFDAT-1) = 0.D0
          FOUT = 0.D0
        else
          NEG = 1
        end if
      else if (FOUT .gt. 0.D0) then
        NEG = 0
      else
c               Must be a Nan
        NEG = 0
        FBIG = 1.0
        FSMA = 1.0
        IWF = 2
        go to 40
      end if

      FBIG = abs(FOUT)
      FSMA = FBIG
      IWF = 2
c                                      Get the format.
   40 continue
      if (KDF .eq. 0) KDF = KDFDEF
      KEXE = 0
      if (FBIG .ne. 0.D0) then
        if (FSMA .eq. 0.D0) FSMA = 1.D0
        FBIG = FBIG * (1.D0 + .5D0 * .1D0**abs(KDF))
        IWF = IWF + NEG
        if (KDF .lt. 0) then
          KSMA = 0
        else
          KSMA = -log10(FSMA)
          if (FSMA .lt. 1.D0) KSMA = KSMA + 1
        end if
        KBIG = log10(FBIG)
        if (FBIG .lt. 1.D0) then
          KBIG = KBIG - 1
          if (FBIG .gt. 1.D0 - .1D0**abs(KDF-1)) KBIG = 0
        end if
c         This is to get ininities out (at least with g77)
        if ((KBIG .lt. -1000) .or. (KBIG .gt. 1000)) KBIG = 8
        if ((KSMA .lt. -1000) .or. (KSMA .gt. 1000)) KSMA = 8
        if (max(KBIG, 0) + max(KSMA,0) .ge. 4) then
c               Want to use an "E" format
          KEXE = 3 + max(0, int(log10(dble(max(KBIG,abs(KSMA))+1.D-5))))
          if (KDF .lt. 0) then
            ID = -KDF
          else
            ID = KDF - 1
          end if
          IWF = IWF + ID + KEXE
c++ CODE for ~.C. is active
          if (LENTRY .eq. 10) IWF = IWF - 1
          write (FMTF, '(''(1P,99(E'',I2,''.'',I2,''E'',I1,'':1X))'')')
     1      IWF, ID, KEXE - 2
c++ CODE for .C. is inactive
cc WATCOM C and others (?) man need an extra 1 here??
c%%    strcpy(cmessc.fmtf, "%*.*E ");
c      lfprec = id
c++ END
          go to 60
        end if
      else
        KSMA = 1
        KBIG = -1
      end if
c               Want to use an "F" format
      if (KDF .lt. 0) then
        ID = -KDF
      else
        ID = KDF + KSMA - 1
      end if
c++ CODE for ~.C. is active
      IWF = IWF + ID + max(KBIG, -1)
      write (FMTF, '(''(0P,99(F'',I2,''.'',I2,'':1X))'')') IWF,ID
c++ CODE for .C. is inactive
c      IWF = IWF + ID + max(KBIG, 0)
c%%    strcpy(cmessc.fmtf, "%*.*f ");
c      lfprec = id
c++ END
   60 if (LENTRY .ne. 4) then
        IWF = IWF + 1
        if (LENTRY .ne. 10) go to 10
c               Take care of setup for sparse vector
        IMAG = 0
        do 70 J = LOCBEG, LASTI
          IMAG = max(abs(IMAG), IDAT(J))
 70     continue
        call MESSFI
c  Format forms:     12345678901234567890   123456789012345678  1234567
c                    (1P,99(Edd.ddEd:1X))   (0P,99(Fxx.xx:1X))  (99Idd)
c++ CODE for ~.C. is active
        if (FMTF(8:8) .eq. 'F') then
          FMTSP=
     1      '(99(' // FMTI(4:6) // ''') '',0P,' // FMTF(8:18)
        else
          FMTSP=
     1     '(99(' // FMTI(4:6) // ''')'' ,1P,' // FMTF(8:20)
        end if
c++ CODE for .C. is inactive
Cc Using cmessc.fmtf in place of fmtsp
C%%      if (cmessc.fmtf[4] == 'f') {
C%%      strcpy(cmessc.fmtf, "%*ld) %*.*f ");
C%%      kexe = 0;
C%%      }
c%%      else strcpy(cmessc.fmtf, "%*ld) %*.*E ");
c%%      cmessi.iwf++;
c++ END
        IWF = IWF + KDI + 1
        go to 10
      end if
c
      LBUF = LBUF + IWF
C%% messcc.lend = cmessi.lbuf;
C%% cmessc.buf[messcc.lend] = ' ';
C%% if ((j > 1) && (cmessc.buf[j-2] >= '0') &&
C%%    (cmessc.buf[j-2] <= '9')) cmessc.buf[j++ - 1] = ' ';
C%% sprintf(&cmessc.buf[j-1], cmessc.fmtf, cmessi.iwf,
C%%    messcc.lfprec, fout);
C%% if (cmessc.buf[messcc.lend] != 0) {messcc.lbeg=j; dmessxc(kexe);}
      write (BUF(J:LBUF),FMTF) FOUT
      go to 10
c                                     Get format for a vector or matrix
  100 ICOL = 1
      if (FDAT(LOCBEG) .lt. 0.D0) then
        NEG = 1
      else if (FDAT(LOCBEG) .ge. 0.D0) then
        NEG = 0
      else
c               Must be a Nan
        NEG = 0
        FBIG = 1.0
        FSMA = 1.0
        go to 110
      end if

      FBIG = abs(FDAT(LOCBEG))
      FSMA = FBIG
  110 do 120 J = LOCBEG, LASTI, INC
        if (FDAT(J) .le. 0.D0) then
          if (FDAT(J) .eq. 0.D0) then
            FDAT(J) = 0.D0
          else
            NEG = 1
          end if
        end if
        FBIG = max(FBIG, abs(FDAT(J)))
        if (FSMA .eq. 0.D0) then
          FSMA = abs(FDAT(J))
        else if (FDAT(J) .ne. 0.D0) then
          FSMA = min(FSMA, abs(FDAT(J)))
        end if
  120 continue
      if (NCOL .ne. 0) then
         ICOL = ICOL + 1
         LOCBEG = LOCBEG + NDIM
         LASTI = LASTI + NDIM
         if (ICOL .le. NCOL) go to 110
      end if
      IWF = 2
      go to 40
c                                    Floating point vector output
  200 continue
C%% messcc.lend = cmessi.lstrt-1;
C%% neg = 0;
C%% for (j=cmessi.mpt; j<cmessi.mpt+cmessi.kline; j++){
C%%   messcc.lbeg = messcc.lend;
C%%   messcc.lend = messcc.lend + cmessi.iwf;
C%%   if (kexe) {
C%%     if (kexe == 5)
C%%        neg = ((fabs(fdat[cmessi.inc*j-1]) < 1.e100)) ? -1: 0;
C%%     else if (kexe ==3) neg = 1;
C%%   }
C%%   sprintf(&cmessc.buf[messcc.lbeg], cmessc.fmtf,
C%%    cmessi.iwf+neg, messcc.lfprec, fdat[cmessi.inc*j-1]);
C%%   if ((kexe == 3) || ((kexe == 5) && neg)) dmessxc(kexe); }
      write(BUF(LSTRT:LBUF),FMTF)(FDAT(K),K=MPT,MPT+INC*(KLINE-1),INC)


c      print '(/A/)', BUF(1:LBUF)

      MPT = MPT + KLINE * INC
      go to 10
c                                    Floating point matrix output
  300 continue
C%% messcc.lend = cmessi.lstrt-1;
C%% neg = 0;
C%% for (j=cmessi.mpt; j<=cmessi.lasti; j+=cmessi.ndim){
C%%    messcc.lbeg = messcc.lend;
C%%    messcc.lend = messcc.lend + cmessi.iwf;
C%%   if (kexe) {
C%%     if (kexe == 5) neg = ((fabs(fdat[j-1]) < 1.e100)) ? -1: 0;
C%%     else if (kexe ==3) neg = 1;
C%%   }
CC%%    if ((messcc.lbeg > 1) && (cmessc.buf[messcc.lbeg-1] >= '0') &&
CC%%       (cmessc.buf[messcc.lbeg-1] <= '9'))
CC%%       cmessc.buf[messcc.lbeg++] = ' ';
C%%    sprintf(&cmessc.buf[messcc.lbeg],
C%%       cmessc.fmtf, cmessi.iwf+neg, messcc.lfprec, fdat[j-1]);
C%%    if ((kexe == 3) || ((kexe == 5) && neg)) dmessxc(kexe); }
      write (BUF(LSTRT:LBUF), FMTF) (FDAT(K), K = MPT, LASTI, NDIM)
      go to 10
c                                    Table output
  400 continue
C%% messcc.lend = cmessi.lstrt-1;
C%% neg=0;
C%% for (j=cmessi.mpt; j<cmessi.mpt+cmessi.kline; j++){
C%%    messcc.lbeg = messcc.lend;
C%%    messcc.lend = messcc.lend + cmessi.iwf;
CC%%    if ((messcc.lbeg > 1) && (cmessc.buf[messcc.lbeg-1] >= '0') &&
CC%%       (cmessc.buf[messcc.lbeg-1] <= '9'))
CC%%       cmessc.buf[messcc.lbeg++] = ' ';
C%%    sprintf(&cmessc.buf[messcc.lbeg], cmessc.fmtf,
C%%       cmessi.iwf+neg, messcc.lfprec, fdat[j-1]); }
      write (BUF(LSTRT:LBUF), FMTF) (FDAT(K), K = MPT, MPT+KLINE-1)
      go to 10


c                                   Sparse vector output
 500  continue
C%%  messcc.lend = -1;
C%%  neg = 0;
C%%  for (j=cmessi.mpt; j<cmessi.mpt+cmessi.kline; j++) {
C%%    messcc.lbeg = messcc.lend + 1;
C%%     messcc.lend = messcc.lend + cmessi.iwf;
C%%   if (kexe) {
C%%     if (kexe == 5) neg = ((fabs(fdat[j-1]) < 1.e100)) ? -1: 0;
C%%     else if (kexe == 3) neg = 1;
C%%   }
C%%   sprintf(&cmessc.buf[messcc.lbeg], cmessc.fmtf, cmessi.kdi,
C%%     idat[j-1],cmessi.iwf-cmessi.kdi-2+neg,messcc.lfprec,fdat[j-1]);
C%%     if ((kexe == 3) || ((kexe == 5) && neg)) dmessxc(kexe); }
      write (BUF(1:LBUF), FMTSP) (IDAT(K),FDAT(K),K=MPT,MPT+KLINE-1)
      MPT = MPT + KLINE
      go to 10

      end

c%%  void dmessxc(long int kexe)
c%%{
c%%  /* Adjust for lack of control on digits in exponent */
c%%  char c;
c%% if (cmessc.fmtf[4] == 'f') return;
c%% if (kexe == 4) return;
c%% if (kexe == 3) { // Should only be one digit in the exponent
c%%   cmessc.buf[messcc.lend-1] = cmessc.buf[messcc.lend];
c%%   cmessc.buf[messcc.lend] = ' ';
c%% }
c%% else { // Should be at least 3 digits in the exponent.
c%%   c =cmessc.buf[messcc.lend-4];
c%%   if ((c < '0') || (c > '9')) {
c%%     cmessc.buf[messcc.lend-1] = cmessc.buf[messcc.lend-2];
c%%     cmessc.buf[messcc.lend-2] = cmessc.buf[messcc.lend-3];
c%%     cmessc.buf[messcc.lend-3] = '0';
c%%     cmessc.buf[messcc.lend] = ' ';
c%%   }
c%% }
c%% return;
c%%} /* end of function */
