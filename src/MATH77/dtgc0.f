      SUBROUTINE DTGC0  (S, ZOUT, WANTDZ, DZOUT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1996-02-02 DTGC0 CLL
C>> 1996-01-11 DTGC0 CLL
C>> 1995-11-01 DTGC0 CLL
C>> 1995-09-26 DTGC0 CLL Editing for inclusion into MATH77.
c     C.L.LAWSON, JPL, 1976 DEC 7
c  This subr interpolates over a triangle using linear interpolation.
c  This method gives C0 continuity with neighboring triangles, i.e.,
c  continuity of the value but generally not continuity of first and
c  higher order partial derivatives.
c  Optionally the subr also computes first partial derivatives.
c
c   (S(1:3, 1:4) [inout]  Columns 1-4 contain data set by the user to
c          specify the interpolation problem.  Col 1 contains
c          unnormalized barycentric coordinates of the interpolation
c          point.  The other cols contain data depending only on the
c          triangle and its vertex data and not on the interpolation
c          point.
c
c         S( ,1) = UNNORMALIZED BARYCENTRIC COORDS OF INTERP POINT.
c         S( ,2) = U = X COORD OF EDGE VECTOR
c         S( ,3) = V = Y COORD OF EDGE VECTOR
c         S( ,4) = Z = FCN VALUE AT VERTEX
c
c   ZOUT [out]  INTERPOLATED VALUE COMPUTED BY SUBR.
C
c   WANTDZ [in]  =.TRUE. MEANS COMPUTE DZOUT() AS WELL AS ZOUT.
c              =.FALSE. MEANS COMPUTE ONLY ZOUT AND NOT DZOUT().
C
c   DZOUT(1:2) [out]  First partial derivs w.r.t. X and Y of the
c      interpolated surface at the interpolation point.
c      Note that since this subroutine computes only a linear
c      interpolant over a triangle, the partial derivs w.r.t. X and Y
c      will be the same at every point of the triangle.  In general
c      these derivatives will jump to different values when moving to
c      an adjacent triangle.
c     ------------------------------------------------------------------
c         Details of the contents of TRI(1:7) and S(1:3, 1:4).
c
c  We assume the point Q is in the triangle indexed by INDTRI.
c  The indices of the vertices of this triangle, in counter-
c  clockwise, order are P(1), P(2), and P(3) which may be obtained as
c  P(i) = TRI(3+i) for i = 1, 2, and 3.  TRI(7) contains the same
c  value as TRI(4).  The triangle adjacent to this triangle across
c  the edge from P(i) to P(i+1) is indexed by TRI(i) for i = 1, 2,
c  and 3.  If there is no adjacent triangle across this edge then
c  TRI(i) = 0.
c
c  For descriptive convenience we regard the subscript of P() and the
c  1st subsubscript of S(,) as always being reduced modulo 3 to 1, 2,
c  or 3.  Also for convenience we shall write P(i) to mean the vertex
c  indexed by P(i).
c
c  The unnormalized barycentric coordinate that is zero along the edge
c  from P(i) to P(i+1) and has a positive value at P(i+2) is stored in
c  S(i,1).  The (x,y) coordinates of the vector from P(i) to P(i+1) are
c  stored in (S(i,2), S(i,3)).
c  The function value Z at P(i+2) is stored in S(i,4) for i = 1, 2, & 3.
c     ------------------------------------------------------------------
c--D replaces "?": ?TGC0
C     ------------------------------------------------------------------
      DOUBLE PRECISION delz(2), denom, dzout(2), S(3,4), zout
      LOGICAL  WANTDZ
C     ------------------------------------------------------------------
c        To compute zout:
c        Combine the vertex function values using the unnormalized
c        barycentric coordinates as weights, and normalize by the
c        sum of the unnormalized barycentric coordinates.
c
      zout = (s(1,1)*s(1,4) +s(2,1)*s(2,4) +s(3,1)*s(3,4)) /
     *            (S(1,1)+S(2,1)+S(3,1))
      if(wantdz) then
         delz(1) = s(3,4) - s(2,4)
         delz(2) = s(1,4) - s(3,4)
         denom = s(1,2) * s(2,3) - s(2,2) * s(1,3)
         if(denom .ne. 0.0d0) then
            dzout(1) = (delz(1) * s(2,3) - delz(2) * s(1,3)) / denom
            dzout(2) = (s(1,2) * delz(2) - s(2,2) * delz(1)) / denom
         else
            dzout(1) = 0.0d0
            dzout(2) = 0.0d0
         endif
      endif
      return
      end
