      subroutine DTGFI( X, Y, Z, DZ, TRIANG, NT, Bdry, MB,
     *            NCONT, Q, ZOUT, WANTDZ, DZOUT, MODEFI, SAVWRK)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1997-07-01 DTGFI Krogh Reversed subscripts in B (CLL suggestion)
C>> 1997-06-24 DTGFI CLL Added names MODEFI and MODEFND.
C>> 1997-06-23 DTGFI Krogh Removed "implicit none"
C>> 1997-06-22 DTGFI CLL
C>> 1997-06-04 DTGFI CLL
C>> 1996-02-02 DTGFI CLL
C>> 1996-01-11 DTGFI CLL
C>> 1995-10-30 DTGFI CLL
C>> 1995-09-26 DTGFI CLL Editing for inclusion into MATH77.
c  This routine uses DTGFND to do lookup, and either DTGC0 or DTGC1
c  to do interpolation in a triangular grid, and uses DTGEXT for
c  extrapolation.
C     ------------------------------------------------------------------
c                         Subroutine Arguments
c
c   X(1:NP), Y(1:NP) [in]  (x,y) coordinates of vertices of the
c                          triangular grid.
c
c   Z(1:NP) [in]  Z(i) is the value at (X(i),Y(i)) of the data to
c                 be interpolated.
c
c   DZ(1:2, 1:NP) [in]  DZ(1:2, i) are the values of the partial
c         derivatives of the interpolation function at (X(i),Y(i)) with
c         respect to x and y, respectively.
c
c         {NP is the number of vertices in the triangular grid, but is
c          not explicitly used in this subroutine.}
c
c   TRIANG(1:6*NT) [in]  Array of integer pointers defining the
c            connectivity of the triangular grid.
c
c   NT [in]  No. of triangles in the triangular grid.
c
c   Bdry(1:4, MB) [integer, in]  Array containing pointers defining the
c      boundary of the (convex) triangular grid.
C         Bdry(1, K) = FWD POINTER. Points to next vertex in
c                     counterclockwise order.
C         Bdry(2, K) = BACKWARD POINTER. Points to next vertex in
c                     clockwise order.
C         Bdry(3, K) = A BOUNDARY POINT
C         Bdry(4, K) = A BOUNDARY TRIANGLE
c      The triangle Bdry(4, K) has a boundary edge that
c      connects the points Bdry(3, K) and Bdry(3, K+1).
c      In general not all elements of the array Bdry() are members of
c      the linked list defining the boundary.
c      The entry with K = 1 is a member of the boundary list.  To scan
c      the boundary, start at K = 1 and follow the forward or backward
c      pointers.
c
c   MB [in]
c
c   NCONT [in]  = 0 or 1 to request either C0 or C1 continuity.
c
c   Q(1:2) [in]  The (x,y) coordinates of the point for which this
c             subr will attempt to find an enclosing triangle and then
c             do interpolation.
c
c   ZOUT [out]  Interpolated value computed by this subroutine.
c
c   WANTDZ [in]  =.TRUE. means compute DZOUT() as well as ZOUT.
c              =.FALSE. means compute only ZOUT and not DZOUT().
c
c   DZOUT(1:2) [out]  First partial derivs w.r.t. x and y of the
c              interpolated surface at the interpolation point.
c
c   MODEFI [out]
c          =0  Means OK.  Q is interior or almost so.
c          =1  Q is exterior by more than the built-in tolerance.
c              An extrapolated value will be returned.
c          =2  Bad.  Subroutine is cycling in the search.  This should
c              never happen.
c
c   SAVWRK(1:28) [inout]  This array is used as work space and to save
c      quantities that may be reusable on a subsequent call.
c      Let us say the settings of X(), Y(), Z(), DZ(), TRIANG(),
c      and NT define a "grid and data".  On the first call to this
c      subroutine for use of a particular "grid and data" the user must
c      set SAVWRK(1) = 0.  This subroutine will generally reset
c      SAVWRK(1) to a nonzero value on return, indicating there are
c      saved quantities in SAVWRK() relating to this "grid and data".
c      If the user will be switching back and forth between different
c      "grid and data" specifications, some saving of computation time
c      can be achieved by using distinct SAVWRK() arrays associated with
c      distinct "grid and data" specifications.  It is the user's
c      responsibility to set SAVWRK(1) = 0. on any call for which the
c      current SAVWRK() array is not the one that was used on the most
c      recent previous call involving the current "grid and data".
c
C     ------------------------------------------------------------------
c              How the array SAVWRK() is used internally.
c
c     The 1st location of SAVWRK() is regarded as containing a value
c     of INDSGN, and the remaining 27 locations contain S(1:3, 1:9).
c
c     We start by setting       INDSGN = SAVWRK(1)
c     and                       INDTRI = abs(INDSGN)
c     We start the current lookup at INDTRI.  If INDTRI is positive this
c     causes the current lookup to start in the triangle at which the
c     previous lookup for this "grid and data" ended, which in some
c     cases significantly reduces the search time.
c
c     If INDSGN > 0, and NCONT = 2, and the value of INDTRI does not
c     change during the lookup, we assume there is useful saved info in
c     S(1:3,7:9).  Otherwise we compute this info as needed.
c
c                    Contents of S(,)
c
c   (s(1:3, 1:9) [inout]  Columns 1-6 contain data specifying the
c              interpolation problem.  Col 1 contains unnormalized
c              barycentric coordinates of the interpolation point.
c              The other cols contain data depending only on the
c              triangle and its vertex data and not on the
c              interpolation point.  Cols 7-9 are used to save computed
c              quantities depending only on the triangle and the
c              partial deriv values from one call to the next.
c         s( ,1) = Unnormalized barycentric coords of interp point.
c         s( ,2) = u = x coord of edge vector
c         s( ,3) = v = y coord of edge vector
c         s( ,4) = z = Fcn value at vertex
c         s( ,5) = zx = Partial deriv of z w.r.t. x
c         s( ,6) = zy = Partial deriv of z w.r.t. y
c         s( ,7) = htilda = Scaled p.d. tangential to edge at left end.
c         s( ,8) = ktilda = Scaled p.d. tangential to edge at right end.
c         s( ,9) = lfac   = Factor involving lengths of edges.
c
c  Let the indices of the vertices of this triangle, in counter-
c  clockwise order be denoted by P(1), P(2), and P(3).
c  For descriptive convenience we regard the subscript of P() and the
c  1st subsubscript of S(,) as always being reduced modulo 3 to 1, 2,
c  or 3.  Also for convenience we shall write P(i) to mean the vertex
c  indexed by P(i).
c
c  The unnormalized barycentric coordinate that is zero along the edge
c  from P(i) to P(i+1) and has a positive value at P(i+2) is stored in
c  S(i,1).  The (x,y) coordinates of the vector from P(i) to P(i+1) are
c  stored in (S(i,2), S(i,3)).
c
c  The function value Z at P(i+2) is stored in S(i,4) for i = 1, 2, & 3.
c  ZX, the partial deriv of Z w.r.t. X at P(i+2) is stored in S(i,5)
c     for i = 1, 2, & 3.
c  ZY, the partial deriv of Z w.r.t. Y at P(i+2) is stored in S(i,6)
c     for i = 1, 2, & 3.
C     ------------------------------------------------------------------
c                      Internal Variables
c
c  NEWTRI [in]  = .false. means that saved quantities are
c              present in  s(1:3, 7:9).9).
c              If newtri=.true. the subr will compute s(1:3, 7:9).
c
c  INDTRI  On entry to DTGFND, indtri designates the triangle at which
c      the search will begin.  On return from DTGFND, indtri is the
c      index of the last triangle tested.
c      This is the triangle containing q if MODEFND=0.
c      If MODEFND = 1, 2, or 3, this is a boundary triangle and q is
c      outside a boundary edge of this triangle.
c
c  TRI(1:8)  Integer array of pointers defining one triangle.  It is
c     never assumed to contain any saved values on entry to
c     DTGFND.  On return from DTGFND, TRI(1:7) defines the triangle
c     whose index is the returned value of INDTRI.
c
c     The indices of the vertices of this triangle, in counter-
c     clockwise order, are P(1), P(2), and P(3), which may be obtained
c     as P(i) = TRI(3+i) for i = 1, 2, and 3.  TRI(7) contains the same
c     value as TRI(4).  TRI(8) will be set in this subr to equal TRI(5).
c     The triangle adjacent to this triangle across the edge from P(i)
c     to P(i+1) is indexed by TRI(i) for i = 1, 2, and 3.  If there is
c     no adjacent triangle across this edge then TRI(i) = 0.
c
c
c     ------------------------------------------------------------------
c--D replaces "?": ?TGFI, ?TGFND, ?TGC0, ?TGC1, ?TGEXT
c     ------------------------------------------------------------------
      integer MB
      integer Bdry(1:4, MB)
      integer indsgn, indtri, MODEFI, MODEFND, ncont, nt
      integer tri(8), triang(1:*)
      double precision dz(2,*), dzout(2), q(2), savwrk(28)
      double precision x(*), y(*), z(*), zero, zout
      logical newtri, wantdz
      parameter(zero = 0.0d0)
c     ------------------------------------------------------------------
      indsgn = savwrk(1)
      indtri = abs(indsgn)
      call DTGFND(x,y,triang,nt,q,indtri,tri,savwrk(2),MODEFND)
      if (MODEFND .eq. 0) then
c
c           The point q(1:2) was found ok.
c           tri(1:7) and s(1:3, 1:3) contain info set by DTRFND.
c           Next move info into s(1:3, 4:6) and call DTRC0 or DTRC1 for
c           interpolation.
c           Note that savwrk(-2+i + 3*j) contains s(i,j).
c
c           S(1,4) = Z(TRI(6))
c           S(2,4) = Z(TRI(4))
c           S(3,4) = Z(TRI(5))
c
         savwrk(1+10) = Z(TRI(6))
         savwrk(1+11) = Z(TRI(4))
         savwrk(1+12) = Z(TRI(5))
         if(ncont .eq. 0) then
            call DTGC0(savwrk(2), zout, wantdz ,dzout)
            indsgn = -indtri
         else
c
c           S(1,5) = DZ(1,TRI(6))
c           S(2,5) = DZ(1,TRI(4))
c           S(3,5) = DZ(1,TRI(5))
c
c           S(1,6) = DZ(2,TRI(6))
c           S(2,6) = DZ(2,TRI(4))
c           S(3,6) = DZ(2,TRI(5))

            savwrk(1+13) = DZ(1,TRI(6))
            savwrk(1+14) = DZ(1,TRI(4))
            savwrk(1+15) = DZ(1,TRI(5))

            savwrk(1+16) = DZ(2,TRI(6))
            savwrk(1+17) = DZ(2,TRI(4))
            savwrk(1+18) = DZ(2,TRI(5))

            newtri = indtri .ne. indsgn
            call DTGC1(newtri, savwrk(2), zout, wantdz ,dzout)
            indsgn = indtri
         endif
         MODEFI = 0
      elseif(MODEFND .gt. 0) then
c
c           Here MODEFND = 1, 2, or 3.
c           Point q is outside the convex hull of the data.
c           Use extrapolation.
c
           call DTGEXT(x, y, z, dz, triang, bdry, mb, ncont,
     *                  q, indtri, MODEFND, zout, wantdz, dzout)
         indsgn = -indtri
           MODEFI = 1
      else
c           Here MODEFND = -1.
c           This means cycling has happened in DTGFND. this
c           should not happen.  Error message will have been printed
c           from DTGFND.
c
         zout = zero
         if (wantdz) then
            dzout(1) = zero
            dzout(2) = zero
         end if
         indsgn = -indtri
           MODEFI = 2
      end if
      savwrk(1) = indsgn
      return
      end
