c  File: DRSTGFI.[F|FOR] CONTAINS DRSTGFI AND STGFCN.
      program DRSTGFI
c>> 2001-07-16 DRDTGF1 Krogh Added exponent 0 to some constants.
c>> 2001-05-22 DRDTGF1 Krogh Minor change for making .f90 version.
c>> 1997-07-01 DRDTGF1 Krogh Reversed subscripts in B (CLL suggestion)
c>> 1997-06-19 DRDTGF1 Krogh Minor changes to get proper C conversion.
c>> 1997-06-18 DRSTGFI CLL
c>> 1996-03-04 DRSTGFI CLL
c>> 1996-02-02 DRSTGFI CLL
c>> 1995-10-31 DRSTGFI CLL
c   Demo driver for STGFI, STGGRD, STGPD, etc.
c     ------------------------------------------------------------------
c--S replaces "?": DR?TGFI, ?TGFI, ?TGGRD, ?TGPD, ?TGFCN
c     ------------------------------------------------------------------
      integer mb, mp, meval, mt
      parameter(mb= 16, mp= 28, mt= 336)
      parameter(meval = 5)
      integer Bdry(4,mb), i, ieval, INFO(3), IP(mp), kf
      integer mode, ncont, np, nt, TRIANG(mt)
      real             DZ(2,mp), dzout(2)
      real             dztemp(2), DZTRUE(2)
      real             q(2)
      real             savwrk(28)
      real             W(mp), X(mp)
      real             Y(mp)
      real             Z(mp), zout, ztrue
      logical  WANTDZ
      character title*75
      data X /
     *-0.76059E0,-0.02286E0,-0.44790E0, 0.15068E0,-0.87287E0,-0.23390E0,
     * 0.06093E0,-0.84142E0,-0.69173E0,-0.56613E0,-0.42243E0,-0.17249E0,
     *-0.06484E0, 0.48286E0,-0.88784E0, 0.10277E0, 0.69087E0,-0.84292E0,
     * 0.08784E0,-0.95068E0, 0.02496E0, 0.94973E0, 0.04588E0,-0.51667E0,
     *-0.77561E0, 0.90000E0,-0.70830E0,-0.40500E0 /
      data Y /
     *-0.31421E0, 0.75657E0, 0.14321E0, 0.42353E0,-0.62983E0, 0.12326E0,
     * 0.34054E0,-0.55144E0, 0.34158E0,-0.67143E0, 0.43087E0, 0.96081E0,
     *-0.68130E0, 0.12095E0,-0.10663E0, 0.29219E0, 0.31028E0, 0.12934E0,
     * 0.10709E0,-0.42307E0, 0.49895E0, 0.68597E0,-0.78215E0,-0.12362E0,
     *-0.88827E0,-0.60000E0,-0.88620E0, 0.08600E0 /
      data np / mp /
      data wantdz / .true. /
c     ------------------------------------------------------------------
      write(*,'(a/a)') ' Program DRSTGFI.  Demo driver for STGFI,',
     *                 '                    STGGRD, STGPD, etc.'
      call STGGRD(X,Y,NP,IP,W,TRIANG,MT,Bdry,MB,NT, INFO)
      if (INFO(1) .NE. 0) then
         write(*,'(a,i5)')
     *      ' Error return from STGGRD. INFO(1) =', INFO(1)
         stop
      endif
c
      do 100 ncont = 0,1
         do 80 KF= 0,4
            write(*,'(//a,i1,a)') ' New Case.  Interpolation with C',
     *         ncont,' continuity.'
            do 20 I=1,NP
               call STGFCN(KF,X(I),Y(I),title,Z(I),DZtemp(1),DZtemp(2))
   20       continue
            write(*,'(/a)') title
            call STGPD(X,Y,Z,DZ,NP, TRIANG, NT, IP)
            savwrk(1) = 0.0e0
            write(*,'(/3x,a//a,a/16x,a/16x,a/)')
     *          ' VALUES AND PARTIAL DERIVS INTERPOLATED ALONG A PATH.',
     *          '    I    X     Y',
     *          '    Z_INTERP     Z_TRUE     Z_ERR',
     *          '  DZ1_INTERP   DZ1_TRUE   DZ1_ERR',
     *          '  DZ2_INTERP   DZ2_TRUE   DZ2_ERR'
            do 60 ieval = 0, meval-1
               q(1) = -1.0e0 + ieval*2.0e0/(meval-1)
               q(2) =  0.8e0 * q(1)
               call STGFI( X, Y, Z, DZ, TRIANG, NT, Bdry, Mb,
     *                    ncont, Q, zout, WANTDZ, DZout, MODE, SAVWRK)
               if(mode .ge. 0) then
                  call STGFCN(KF,  q(1), q(2),
     *                title, ztrue, dztrue(1), dztrue(2))
                  write(*, '(1X,I4,2F6.2,2F11.6,E10.2)') ieval,q,
     *                 zout, ztrue, zout - ztrue
                  write(*,'(17x,2F11.6,E10.2)') dzout(1), dztrue(1),
     *                 dzout(1) - dztrue(1)
                  write(*,'(17x,2F11.6,E10.2)') dzout(2), dztrue(2),
     *                 dzout(2) - dztrue(2)
               else
                  write(*,'(1x,i4,2f6.2,a)')
     *               ieval, q, '  Error.'
               endif
   60       continue
   80    continue
  100 continue
      end
c     ==================================================================
      subroutine STGFCN  (KF,X,Y, TITLE, Z,ZX,ZY)
c>> 1995-09-26 CLL Editing for inclusion into MATH77.
c     C.L.Lawson, JPL, 1976 Dec 10.  Edited comments 1979 Mar 3.
c     This subr evaluates a function and its first partial derivs as
c     selected by KF.  KF can be from 0 to 4.
c     Input is KF, X, and Y.  Output is TITLE, Z, ZX, and ZY.
c     ------------------------------------------------------------------
      integer KF
      real             X,Y,Z,ZX,ZY
      character title*75
      integer I
      character title1(0:4)*56, title2(0:4)*19
      data (title1(i),title2(i),i=0,4) /
     * '  CONSTANT FUNCTION     Z = 2', ' ',
     * '  LINEAR FUNCTION       Z = ( 1 + 2*X + 3*Y ) / 6', ' ',
     * '  QUADRATIC FUNCTION    Z = ( -1 + 2*X - 3*Y + 4*X**2 - ',
     *                             'X*Y + 9*Y**2 ) / 10',
     * '  CUBIC FUNCTION    Z = ( 9*X**3 - 2*(X**2)*Y + 3*X*Y**2',
     *                             ' - 4 * Y**3 ) / 10',
     * '  EXPONENTIAL FUNCTION   Z = EXP( -2 * (X**2 + Y**2) )', ' '/
c     ------------------------------------------------------------------
      title = title1(kf) // title2(kf)
       if(kf .eq. 0) then
c                                  KF=0  CONSTANT FCN.
         Z=2.0e0
         ZX=0.0e0
         ZY=0.0e0
       elseif(kf .eq. 1) then
c                                  KF=1  LINEAR FCN.
         Z=(1.0e0 + 2.0e0*X + 3.0e0*Y) / 6.0e0
         ZX=2.0e0/6.0e0
         ZY=3.0e0/6.0e0
       elseif(kf .eq. 2) then
c                                  KF=2  QUADRATIC FCN.
         Z=(-1.0e0 + 2.0e0*X - 3.0e0*Y +4.0e0*X**2 -X*Y + 9.0e0*Y**2)*
     *       0.1e0
         ZX =(2.0e0 + 8.0e0*X -Y)* 0.1e0
         ZY =(-3.0e0 -X + 18.0e0*Y)* 0.1e0
       elseif(kf .eq. 3) then
c                                  KF=3  CUBIC FCN.
         Z=(9.0e0*X**3 - 2.0e0*(X**2)*Y + 3.0e0*X*Y**2 - 4.0e0*Y**3)*
     *       0.1e0
         ZX=(27.0e0*X**2 - 4.0e0*X*Y + 3.0e0*Y**2)* 0.1e0
         ZY=(-2.0e0*X**2 + 6.0e0*X*Y - 12.0e0*Y**2)* 0.1e0
       elseif(kf .eq. 4) then
c                                  KF=4  EXPONENTIAL FCN.
         Z=  EXP(-(X**2 + Y**2) * 2.0e0)
c
c              NOTE THAT THE INFLECTION POINT OF THIS FCN IN ANY
c              RADIAL DIRECTION FROM THE ORIGIN IS AT R = .5
c
         ZX= -4.0e0*X*Z
         ZY= -4.0e0*Y*Z
       endif
      return
      end
