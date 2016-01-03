c  File: DRDTGFI1.[F|FOR] CONTAINS DRDTGFI1 AND DTGFCN.
      program DRDTGFI1
c>> 2001-07-16 DRDTGFI1 Krogh Added exponent 0 to some constants.
c>> 2001-05-22 DRDTGFI1 Krogh Minor change for making .f90 version.
c>> 1997-07-01 DRDTGFI1 Krogh Reversed subscripts in B (CLL suggestion)
c>> 1997-06-19 DRDTGFI1 Krogh Minor changes to get proper C conversion.
c>> 1997-06-18 DRDTGFI1 CLL
c>> 1996-03-04 DRDTGFI1 CLL
c>> 1996-02-02 DRDTGFI1 CLL
c>> 1995-10-31 DRDTGFI1 CLL
c   Demo driver for DTGFI, DTGGRD, DTGPD, etc.
c     ------------------------------------------------------------------
c--D replaces "?": DR?TGFI1, ?TGFI, ?TGGRD, ?TGPD, ?TGFCN
c     ------------------------------------------------------------------
      integer mb, mp, meval, mt
      parameter(mb= 16, mp= 28, mt= 336)
      parameter(meval = 5)
      integer Bdry(4,mb), i, ieval, INFO(3), IP(mp), kf
      integer mode, ncont, np, nt, TRIANG(mt)
      double precision DZ(2,mp), dzout(2)
      double precision dztemp(2), DZTRUE(2)
      double precision q(2)
      double precision savwrk(28)
      double precision W(mp), X(mp)
      double precision Y(mp)
      double precision Z(mp), zout, ztrue
      logical  WANTDZ
      character title*75
      data X /
     *-0.76059D0,-0.02286D0,-0.44790D0, 0.15068D0,-0.87287D0,-0.23390D0,
     * 0.06093D0,-0.84142D0,-0.69173D0,-0.56613D0,-0.42243D0,-0.17249D0,
     *-0.06484D0, 0.48286D0,-0.88784D0, 0.10277D0, 0.69087D0,-0.84292D0,
     * 0.08784D0,-0.95068D0, 0.02496D0, 0.94973D0, 0.04588D0,-0.51667D0,
     *-0.77561D0, 0.90000D0,-0.70830D0,-0.40500D0 /
      data Y /
     *-0.31421D0, 0.75657D0, 0.14321D0, 0.42353D0,-0.62983D0, 0.12326D0,
     * 0.34054D0,-0.55144D0, 0.34158D0,-0.67143D0, 0.43087D0, 0.96081D0,
     *-0.68130D0, 0.12095D0,-0.10663D0, 0.29219D0, 0.31028D0, 0.12934D0,
     * 0.10709D0,-0.42307D0, 0.49895D0, 0.68597D0,-0.78215D0,-0.12362D0,
     *-0.88827D0,-0.60000D0,-0.88620D0, 0.08600D0 /
      data np / mp /
      data wantdz / .true. /
c     ------------------------------------------------------------------
      write(*,'(a/a)') ' Program DRDTGFI1.  Demo driver for DTGFI,',
     *                 '                    DTGGRD, DTGPD, etc.'
      call DTGGRD(X,Y,NP,IP,W,TRIANG,MT,Bdry,MB,NT, INFO)
      if (INFO(1) .NE. 0) then
         write(*,'(a,i5)')
     *      ' ERROR return FROM DTGGRD. INFO(1) =', INFO(1)
         stop
      endif
c
c     do 200 ncont = 0,1
      do 200 ncont = 1,1
c     do 100 KF= 0,4
      do 100 KF= 2,4,2
         write(*,'(//a,i1,a)') ' New Case.  Interpolation with C',
     *      ncont,' continuity.'
         do 20 I=1,NP
               call DTGFCN(KF,X(I),Y(I),title,Z(I),DZtemp(1),DZtemp(2))
   20    continue
         write(*,'(/a)') title
         call DTGPD(X,Y,Z,DZ,NP, TRIANG, NT, IP)
         savwrk(1) = 0.0d0
         write(*,'(/3x,a//a,a/16x,a/16x,a/)')
     *       ' VALUES AND PARTIAL DERIVS INTERPOLATED ALONG A PATH.',
     *       '    I    X     Y',
     *       '    Z_INTERP     Z_TRUE     Z_ERR',
     *       '  DZ1_INTERP   DZ1_TRUE   DZ1_ERR',
     *       '  DZ2_INTERP   DZ2_TRUE   DZ2_ERR'
         do 30 ieval = 0, meval-1
            q(1) = -1.0d0 + ieval*2.0d0/(meval-1)
            q(2) =  0.8d0 * q(1)
            call DTGFI( X, Y, Z, DZ, TRIANG, NT, Bdry, Mb,
     *                 ncont, Q, zout, WANTDZ, DZout, MODE, SAVWRK)
            if(mode .ge. 0) then
               call DTGFCN(KF,  q(1), q(2),
     *             title, ztrue, dztrue(1), dztrue(2))
               write(*, '(1X,I4,2F6.2,2F11.6,E10.2)') ieval,q,
     *              zout, ztrue, zout - ztrue
               write(*,'(17x,2F11.6,E10.2)') dzout(1), dztrue(1),
     *              dzout(1) - dztrue(1)
               write(*,'(17x,2F11.6,E10.2)') dzout(2), dztrue(2),
     *              dzout(2) - dztrue(2)
            else
               write(*,'(1x,i4,2f6.2,a)')
     *            ieval, q, '   Error.'
            endif
   30    continue
  100 continue
  200 continue
      end
c     ==================================================================
      subroutine DTGFCN  (KF,X,Y, TITLE, Z,ZX,ZY)
c>> 1995-09-26 DRDTGFI1 CLL Editing for inclusion into MATH77.
c     C.L.Lawson, JPL, 1976 Dec 10.  Edited comments 1979 Mar 3.
c     This subr evaluates a function and its first partial derivs as
c     selected by KF.  KF can be from 0 to 4.
c     Input is KF, X, and Y.  Output is TITLE, Z, ZX, and ZY.
c     ------------------------------------------------------------------
      integer KF
      double precision X,Y,Z,ZX,ZY
      character title*75
      character title1(0:4)*56, title2(0:4)*19
      integer I
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
         Z=2.0d0
         ZX=0.0d0
         ZY=0.0d0
       elseif(kf .eq. 1) then
c                                  KF=1  LINEAR FCN.
         Z=(1.0d0 + 2.0d0*X + 3.0d0*Y) / 6.0d0
         ZX=2.0d0/6.0d0
         ZY=3.0d0/6.0d0
       elseif(kf .eq. 2) then
c                                  KF=2  QUADRATIC FCN.
         Z=(-1.0d0 + 2.0d0*X - 3.0d0*Y +4.0d0*X**2 -X*Y + 9.0d0*Y**2)*
     *       0.1d0
         ZX =(2.0d0 + 8.0d0*X -Y)* 0.1d0
         ZY =(-3.0d0 -X + 18.0d0*Y)* 0.1d0
       elseif(kf .eq. 3) then
c                                  KF=3  CUBIC FCN.
         Z=(9.0d0*X**3 - 2.0d0*(X**2)*Y + 3.0d0*X*Y**2 - 4.0d0*Y**3)*
     *       0.1d0
         ZX=(27.0d0*X**2 - 4.0d0*X*Y + 3.0d0*Y**2)* 0.1d0
         ZY=(-2.0d0*X**2 + 6.0d0*X*Y - 12.0d0*Y**2)* 0.1d0
       elseif(kf .eq. 4) then
c                                  KF=4  EXPONENTIAL FCN.
         Z=  EXP(-(X**2 + Y**2) * 2.0d0)
c
c              NOTE THAT THE INFLECTION POINT OF THIS FCN IN ANY
c              RADIAL DIRECTION FROM THE ORIGIN IS AT R = .5
c
         ZX= -4.0d0*X*Z
         ZY= -4.0d0*Y*Z
       endif
      return
      end
