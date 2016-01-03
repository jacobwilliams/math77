      SUBROUTINE STGREC(X,Y,Z,DZ,NP, TRIANG,NT, Bdry, MB, XYLIM,NX,NY,
     * ZFILL, ZVALS, MX, MY, NCONT, WANTPD, DZVALS )
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1997-07-01 STGREC Krogh Reversed subscripts in B (CLL suggestion)
c>> 1997-06-18 STGREC CLL Added arguments Bdry and MB.
C>> 1996-02-02 STGREC CLL
C>> 1996-01-11 STGREC CLL
C>> 1995-11-03 STGREC CLL Added argument, NCONT.
C>> 1995-09-26 STGREC CLL Editing for inclusion into MATH77.
c  This subr builds a rectangular array of Z values by interpo-
c  lation in a triangular grid.  The subr will use either C0 or C1
c  interpolation methods, depending on the setting of NCONT = 0 or 1.
c  Optionally this subr also builds a rectangular array of values of
c  first partial derivatives.
c     CLL, 1979 JUNE 12, MINOR CHANGES.
c     C.L.LAWSON, JPL, 1976 DEC 8.  ADDED PARTIAL DERIVS 1977 MAR 3.
c     ------------------------------------------------------------------
C                      Subroutine arguments
c
c     All args are of intent IN except ZVALS() and DZVALS() which are
c     of intent OUT.  The contents of DZ() are only used when NCONT = 1.
c
c  X(),Y(),Z() [floating,in] NP sets of (X,Y,Z) data.
C
c  (DZ(I,J),I=1,2),J=1,NP) [floating,in] Partial derivs of Z w.r.t.
c     X and Y.  The contents of this array are only used when NCONT = 1,
c     not when NCONT = 0.
C
c  NP [integer,in]  No. of data points.
c
c  TRIANG(1:6*NT),NT [integer,in]  TRIANG() contains pointers (integers)
c     describing the triangular grid.  NT is the number of triangles.
c     Each triangle requires 6 pointers.
c
c  Bdry(1:4, MB) [integer, in]  Array containing pointers defining the
c      boundary of the (convex) triangular grid.
c
c  MB [integer, in]  Dimension parameter for Bdry() array.
C
c  XYLIM(1:4) [floating,in]  Contains XMIN, XMAX, YMIN, and YMAX
c     specifying boundaries of the rectangular grid.
c
c  NX,NY [integer, in] NO. OF X AND Y GRID LINES IN RECTANGULAR GRID.
C
c  ZFILL [floating, in]  Selects action to be taken when a node of the
c     rectangular grid is outside the (convex) triangular grid.
c     If ZFILL = 0.0, extrapolated values will be provided by this
c     package.
c     If ZFILL .ne. 0.0, this subroutine will store ZFILL into ZVALS()
c     and DZVALS() for locations outside the triangular grid.
c
c  ZVALS(,) [floating,out]  MX BY MY ARRAY IN WHICH THE NX BY NY SET OF
c     INTERPOLATED VALUES WILL BE STORED.
c
c  MX,MY [integer, in] DIMENSIONS OF THE ARRAYS ZVALS( , ) AND
c     DZVALS( , ,2)
C
c  NCONT [integer,in]  Order of continuity of interpolation method to be
c     used.
c     0 selects a C0 method.  This method uses linear interpolation over
c     each triangle.  The function value is continuous across the whole
c     grid but generally the 1st partial derivatives are discontinuous
c     across boundaries between triangles.
c     1 selects a C1 method.  This method partitions each triangle into
c     3 smaller triangles and constructs a bicubic patch over each of
c     the smaller triangles.  The function value and 1st partial deriv-
c     atives are continuous across the whole grid but generally the 2nd
c     and higher order partial derivatives are discontinuous across
c     boundaries between the original triangles and between the small
c     triangles.
c
c  WANTPD [logical, in] User sets = .true. if partial derivs are
c     desired, and .false. otherwise.
C
c  DZVALS(,,) [floating,out]  MX by NY by 2 array in which the
c     NX by NY by 2 set of partial derivatives will be stored if
c     WANTPD = .TRUE.
c     In the case of NCONT = 0, the interpolation function does not
c     generally have a partial derivative at points on the boundary
c     between two triangles.  For such points this subr will return the
c     1st partial derivatives associated with one of the adjoining
c     triangles.
c     ------------------------------------------------------------------
c--S replaces "?": ?TGREC, ?TGFI
c     ------------------------------------------------------------------
      integer mxwrk
      parameter(mxwrk = 28)
      integer MB, mx, my, np
        integer Bdry(4, MB)
      integer i, ix, iy
      integer mode, NCONT, NT, nx, ny, TRIANG(1:*)
      real             DELX, DELY, DZ(2,NP), DZOUT(2), DZVALS(MX,MY,2)
      real             SVWRK0(mxwrk), SVWRK1(mxwrk)
      real             X(NP), XY(2), XYLIM(4), Y(NP)
      real             Z(NP), zero, ZFILL, ZVALS(MX,MY)
        parameter(zero=0.0e0)
      logical FIRST, WANTPD
c     ------------------------------------------------------------------
      DELX = (XYLIM(2)-XYLIM(1)) / (NX-1)
      DELY = (XYLIM(4)-XYLIM(3)) / (NY-1)
      do 10 i = 1,mxwrk
         svwrk0(i) = zero
   10 continue
C
c        XY(1) AND XY(2) WILL BE SET SUCCESSIVELY TO THE (X,Y) COORDS
c        OF EACH POINT OF THE RECTANGULAR GRID.
c        When NCONT .eq. 0, the values of the 1st derivatives are
c        constant within each triangle.  Thus in the case of
c        NCONT .eq. 0 and WANTDZ being true, we only need to compute the
c        partial derivatives when NEWTRI is true.
C
      XY(2) = XYLIM(3)
      DO 60 IY=1,NY
         do 20 i = 1,mxwrk
            svwrk1(i) = svwrk0(i)
   20    continue
         FIRST = .true.
         XY(1) = XYLIM(1)
         DO 50 IX=1,NX
            call STGFI( X, Y, Z, DZ, TRIANG, NT, Bdry, MB,
     *            NCONT, XY, ZVALS(IX,IY), WANTPD, DZOUT, MODE, SVWRK1)
            IF(FIRST) then
               do 30 i = 1,mxwrk
                  svwrk0(i) = svwrk1(i)
   30          continue
               FIRST = .false.
            endif

            IF (MODE .EQ. 0) THEN
c
c                  0 means XY was found within the triangular grid.
c
                IF (WANTPD) THEN
                  DZVALS(IX,IY,1)=DZOUT(1)
                  DZVALS(IX,IY,2)=DZOUT(2)
                END IF
            ELSEif(mode .eq. 1) then
c
c                  1 MEANS XY IS OUTSIDE THE TRIANGULAR GRID.
c                  Will use extrapolated value if ZFILL .eq. zero.
c                  Otherwise set results to the ZFILL
c
               if(ZFILL .eq. zero) then
                  IF (WANTPD) THEN
                     DZVALS(IX,IY,1)=DZOUT(1)
                     DZVALS(IX,IY,2)=DZOUT(2)
                  ENDIF
                 else
                  ZVALS(IX,IY) = ZFILL
                  IF (WANTPD) THEN
                     DZVALS(IX,IY,1)=ZFILL
                     DZVALS(IX,IY,2)=ZFILL
                  ENDIF
                 endif
              else
c
c                 Mode = 2 MEANS CYCLING HAS HAPPENED IN the lookup
c                 process.  This should not happen.
c                 ERROR MESSAGE WILL BE PRINTED FROM _TGFI.
C
               ZVALS(IX,IY) = ZFILL
               IF (WANTPD) THEN
                  DZVALS(IX,IY,1)=ZFILL
                  DZVALS(IX,IY,2)=ZFILL
               ENDIF
            ENDIF
C
            XY(1) = XY(1) + DELX
   50    continue
         XY(2) = XY(2) + DELY
   60 continue
      RETURN
      END
