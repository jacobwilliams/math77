c  File: drSTGrec.[f|for] contains drSTGrec, STGfcn.
      program DRSTGREC
c>> 2001-07-16 DRSTGREC Krogh Added exponent 0 to some constants.
c>> 2001-05-24 DRSTGREC Krogh Minor change for making .f90 version.
c>> 2001-05-22 DRSTGREC Krogh Minor change for making .f90 version.
c>> 1997-07-01 DRSTGREC Krogh Reversed subscripts in B (CLL suggestion)
c>> 1997-06-19 DRSTGREC Krogh Minor changes to get proper C conversion.
c>> 1997-06-18 DRSTGREC CLL Added Bdry and MB to arg list of ?TGREC.
c              Also ZFILL has extended interpretation.
c>> 1996-03-04 DRSTGREC CLL
c>> 1996-02-02 DRSTGREC CLL
c>> 1995-09-26 DRSTGREC CLL Editing for inclusion into MATH77.
c>> 1991-11-20 DRSTGREC CLL  Minor editing. Adding use of ?SORTP.
c     C. L. LAWSON, JPL, 1977 MAR 2, CHANGED APR 7
c     C.L.L., 1979 MAR 3. MINOR CHANGES.
c
c     PORTABLE DEMONSTRATION DRIVER FOR SUBROUTINES THAT
c     CONSTRUCT A TRIANGULAR GRID AND THEN DO LOOK-UP AND INTERPOLATION
c     USING THE GRID.  THE INTERPOLATED SURFACE HAS C1 CONTINUITY.
c     ------------------------------------------------------------------
c  The amount of output by this program is controlled by a set of
c  logical variables.
c
c  PRTMAX  Print summary of max and RMS errors over the rectangular
c          grid.
c  PRTT  Call STGPRG to print the initial X() and Y() data, the
c        pointers in TRIANG() defining the triangular grid, and the
c        pointers in Bdry() identifying the boundary of the triangular
c        grid.
c  PRTR  Print true and computed values and errors at all nodes of the
c        rectangular grid.
c  PRTTPD  Print the true and computed values of partial derivatives,
c          and the errors at all of the initial (x,y) points.
c     ------------------------------------------------------------------
c     THE CALL RELATIONSHIPS ARE AS FOLLOWS..
c
c     DRSTGREC           DEMONSTRATION DRIVER.
c        STGGRD          CONSTRUCT TRIANGULAR GRID
c           ?SORTP       Sorting.   From the JPL Math77 library.
c           STGANG       COMPUTE PSEUDOANGLES
c           ?TRSET       **     THESE FOUR SUBRS ARE USED TO STORE AND
c           STGGET       **     FETCH POINTERS IN THE ARRAY TRIANG()
c           ?TRPUT       **     THAT DEFINES THE TRIANGULAR GRID.
c           ?TRSIZ       **
c           ?TRADJ       TEST AND MODIFY TRIANGULAR GRID
c             ?TRSET
c             STGGET
c             ?TRPUT
c        STGPRG          print DESCRIPTION OF TRIANGULAR GRID
c           STGGET
c        STGFCN          PROVIDE VALUES OF TEST functionS.
c        STGPD           ESTIMATE PARTIAL DERIVATIVES AT GRID POINTS
c           STGGET
c           ?TRMOR       FIND MORE PTS FOR PARTIAL DERIV ESTIMATION.
c              STGGET
c           ?TRLS        WEIGHT,SCALE,STABILIZE,SOLVE FOR LOCAL L.S. FIT
c              ?ROTG     SETUP FOR GIVENS ROTATION. FROM THE BLAS.
c              ?ROT      APPLY GIVENS ROTATION. FROM THE BLAS.
c        STGREC          BUILD RECTANGULAR GRID OF INTERPOLATED VALUES.
c           ?TRFI        Lookup and interpolation in triangular grid.
c              ?TRFND       LOOK-UP IN TRIANGULAR GRID
c                 STGGET
c              ?TRC0        C0 INTERPOLATION IN A TRIANGLE
c              ?TRC1        C1 INTERPOLATION IN A TRIANGLE
c     ------------------------------------------------------------------
c
c     REMARKS ON DIMENSIONS..
c
c          IN TERMS OF THE PARAMETERS MP, MT, MB, MX, AND MY,
c     THE REQUIRED DIMENSIONS ARE..
c     real             ZGRD(MX,MY), DZGRD(MX,MY,2)
c     real             X(MP),Y(MP),Z(MP),DZ(2,MP),W(MP),XYLIM(4)
c     real             DZZ(2),ERRDZ(2)
c     real             DZTRUE(2,MP)
c     integer  IP(MP), TRIANG(MT), Bdry(4,MB)
c
c     (NOTE THAT SOME OF THESE DECLARED VARIABLES ARE ONLY
c      USED IN THIS DEMO DRIVER ITSELF AND ARE NOT ESSENTIAL
c      FOR THE GENERAL USE OF THE SUBROUTINES BEING DEMONSTRATED.)
c
c         MP DENOTES THE MAX NO. OF DATA POINTS TO BE HANDLED.
c          THE MAX NO. OF TRIANGLES CANNOT EXCEED 2*MP.  SINCE WE
c     STORE 6 POINTERS PER TRIANGLE IT FOLLOWS THAT MT CAN BE
c     SET AS         MT = (12*MP)
c          MB MUST BE LARGER BY 1 THAN THE MAX NO. OF BOUNDARY PTS
c     IN ANY SUBSET OF THE GIVEN SET OF (X,Y) DATA.   THE EXPECTED
c     NO. OF BNDRY PTS IN A RANDOM SAMPLE OF PTS FROM A UNIFORM
c     DISTRIBUTION ON DISC IS ABOUT  3.3 * (CUBE ROOT OF MP).
c     TWICE THIS EXPECTED VALUE PROVIDES AN ADEQUATE BOUND FOR
c     MOST CASES.  SETTING MB = MP+1 WOULD BE ABSOLUTELY SAFE
c     BUT LARGER THAN NECESSARY IN MOST CASES.
c          SETTING MB OR MT TOO SMALL WILL BE DETECTED AS ERROR
c     CONDITIONS  BY THE SUBROUTINES.
c          MX AND MY GIVE THE DIMENSIONS OF THE RECTANGULAR GRID ONTO
c     WHICH VALUES WILL BE INTERPOLATED UNDER CONTROL OF STGREC.
c     ------------------------------------------------------------------
c--S replaces "?": DR?TGREC, ?TGGRD, ?TGPRG, ?TGFCN, ?TGPD, ?TGREC
c--&               ?TGGET,   ?TGANG
c     ------------------------------------------------------------------
      integer mb, mp, mt, mx, my
      parameter(mb= 16, mp= 28, mt= 336, mx= 5, my= 5)
      integer Bdry(4,mb), i, INFO(3), IP(mp), j, k, kf, kount
      integer ncont, np, nt, nx, ny, TRIANG(mt), filmod
      real             delx, dely, DZ(2,mp), DZGRD(mx,my,2)
      real             DZTRUE(2,mp), DZZ(2)
      real             err, ERRDZ(2), errm, ERRMDZ(2)
      real             rms, RMSDZ(2), sum1, SUM2(2)
      real             W(mp), X(mp), xx, XYLIM(4),Y(mp), yy
      real             Z(mp), zero, zfill, ZGRD(mx,my), zz
        parameter(zero = 0.0e0)
      logical   PRTMAX, PRTT, PRTR, PRTTPD, skip, WANTPD
      character title*75
      character       line1*56,       line2*19
      data PRTMAX, PRTT,PRTR,PRTTPD/.true., .true., .false., .false./
      data line1, line2/
     * '  ------------------------------------------------------',
     *                             '-------------------'/
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
      data XYLIM / -1.0e0, +1.0e0, -1.0e0, +1.0e0 /
      data np, nx, ny / mp, mx, my /
      data wantpd / .true. /
c     ------------------------------------------------------------------
      write(*,'(a/a/)') ' Program DRSTGREC.  Demo driver for STGREC,',
     * '         STGGRD, STGPD, etc.'
      call STGGRD(X,Y,NP,IP,W,TRIANG,MT,Bdry,MB,NT, INFO)
      if (INFO(1) .NE. 0) then
         write(*,'(a,i5)')
     *      ' Error return from STGGRD. INFO(1) =', INFO(1)
         stop
      endif
      if(PRTT) call STGPRG(X,Y,NP,TRIANG,Bdry,INFO(2),NT)
c
      do 200 KF= 0, 4
         do 20 I=1,NP
               call STGFCN(KF,X(I),Y(I),
     *            title, Z(I),DZTRUE(1,I),DZTRUE(2,I))
   20    continue
         write(*,'(/a,a/a/a,a)')
     *   line1,line2,   title,   line1,line2
         call STGPD(X,Y,Z,DZ,NP, TRIANG, NT, IP)
         if (PRTTPD) then
            write(*,'(/a,a//a,a/41x,a/)')
     *      ' PARTIAL DERIVS ESTIMATED BY STGPD AT THE GIVEN',
     *      ' DATA POINTS',
     *      '    I       X       Y       Z            ',
     *      'DZ1        DZTRUE1    ERR1',
     *      'DZ2        DZTRUE2    ERR2'
            do 50 I=1,NP
              do 40 J=1,2
                 ERRDZ(J) = DZ(J,I) - DZTRUE(J,I)
   40         continue
              write(*,'(1X,I4,3F8.4,2F15.6,E10.2/29x,2F15.6,E10.2)')
     *           I,X(I),Y(I),Z(I),(DZ(J,I), DZTRUE(J,I),
     *           ERRDZ(J), J=1,2)
   50       continue
         end if
c
         do 110 NCONT = 0, 1
         do 100 filmod = 0,1
              if(filmod .eq. 0) then
                 zfill = zero
              else
                 zfill = 999.0e0
              endif
         call STGREC(X,Y,Z,DZ,NP,TRIANG,NT, Bdry, MB, XYLIM,NX,NY,
     *                  ZFILL,ZGRD,MX,MY, NCONT, WANTPD,DZGRD)
c
         if (PRTR .or. PRTMAX) then
            write(*,'(//5x,a,i1,a)')
     *       ' Using a C',ncont,' interpolation method.'
            if(zfill .eq. zero) then
                 write(*,'(5x,a/5x,a)')
     *       ' Assigning EXTRAPOLATED VALUES to points outside',
     *       ' the convex hull of the given data.'
              else
                 write(*,'(5x,a,f7.1,a/5x,a)')
     *       ' Assigning the value ',ZFILL,' to points outside',
     *       ' the convex hull of the given data.'
            endif
 
            if(PRTR) then
            write(*,'(/5x,a/7x,a//a,a/20x,a/20x,a/)')
     *       ' VALUES AND PARTIAL DERIVATIVES INTERPOLATED AT',
     *       ' LATTICE POINTS OF A RECTANGULAR GRID.',
     *       '   I   J     X     Y',
     *       '    Z_INTERP     Z_TRUE     Z_ERR',
     *       '  DZ1_INTERP   DZ1_TRUE   DZ1_ERR',
     *       '  DZ2_INTERP   DZ2_TRUE   DZ2_ERR'
            endif
 
            DELX =  (XYLIM(2)-XYLIM(1))/(NX-1)
            DELY =  (XYLIM(4)-XYLIM(3))/(NY-1)
            XX = XYLIM(1)
            SUM1 = 0.0e0
            SUM2(1) = 0.0e0
            SUM2(2) = 0.0e0
            KOUNT = 0
            ERRM = 0.0e0
            ERRMDZ(1) = 0.0e0
            ERRMDZ(2) = 0.0e0
            SKIP = .true.
            do 80 I=1,NX
               YY = XYLIM(3)
               do 70 J=1,NY
                  call STGFCN(KF,  XX,YY, title, ZZ, DZZ(1), DZZ(2) )
                  if (ZGRD(I,J) .NE. ZFILL) then
                     ERR = ZGRD(I,J) - ZZ
                     if (ERRM .LT. ABS(ERR)) ERRM = ABS(ERR)
                     SUM1 = SUM1 + ERR**2
                     KOUNT = KOUNT + 1
                     do 60 K=1,2
                        ERRDZ(K)= DZGRD(I,J,K) - DZZ(K)
                        if (ERRMDZ(K) .LT. ABS(ERRDZ(K)))
     *                        ERRMDZ(K) = ABS(ERRDZ(K))
                        SUM2(K) = SUM2(K) + ERRDZ(K)**2
   60                continue
                     if(PRTR) then
                     write(*,
     *  '(1X,2I4,2F6.2,2F11.6,E10.2/21x,2F11.6,E10.2/21x,2F11.6,E10.2)')
     *                  I,J,XX,YY,
     *                  ZGRD(I,J), zz, err,
     *                  DZGRD(I,J,1), DZZ(1), errdz(1),
     *                  DZGRD(I,J,2), DZZ(2), errdz(2)
                     endif
                     skip = .true.
                  else
                     if(PRTR) then
                        if(skip) then
                           write(*,*) ' '
                           skip = .false.
                        endif
                     endif
                  end if
                     YY = YY+DELY
   70          continue
               XX = XX + DELX
   80       continue
            RMS = SQRT(SUM1 / KOUNT)
            do 90 K=1,2
               RMSDZ(K) = SQRT(SUM2(K) / KOUNT)
   90       continue
            if(PRTMAX) then
            write(*, '(/9x,a43/1x,3g17.6//9x,a43/1x,3g17.6/)')
     *        'MAX ERR Z       MAX ERR ZX       MAX ERR ZY',
     *         errm, errmdz(1), errmdz(2),
     *        'RMS ERR Z       RMS ERR ZX       RMS ERR ZY',
     *         RMS, rmsdz(1), rmsdz(2)
            endif
         end if
  100    continue
  110    continue
  200 continue
      end
c     ==================================================================
      subroutine STGFCN  (KF,X,Y, TITLE, Z,ZX,ZY)
c>> 1995-09-26 CLL Editing for inclusion into MATH77.
c     C.L.Lawson, JPL, 1976 Dec 10.  Edited comments 1979 Mar 3.
c     This subr evaluates a function and its first partial derivs as
c     selected by KF.  KF can be from 0 to 4.
c     Input is KF, X, and Y.  Output is TITLE, Z, ZX, and ZY.
c     ------------------------------------------------------------------
c--S replaces "?": ?TRANG
c     ------------------------------------------------------------------
      integer KF
      real             X,Y,Z,ZX,ZY
      character title*75
      character title1(0:4)*56, title2(0:4)*19
      integer i
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
