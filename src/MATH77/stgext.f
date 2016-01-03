      subroutine STGEXT(x, y, z, dz, triang, bdry, mb, ncont,
     *                  q, indtri, MODE, zout, wantdz, dzout)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2000-12-01 STGEXT Krogh Removed unused parameters three & four.
c>> 1997-07-01 STGEXT Krogh Reversed subscripts in B (CLL suggestion)
c>> 1997-06-23 STGEXT Krogh Removed "_" in names, set up for M77CON.
c>> 1997-06-22 cll
c>> 1997-06-18 STGEXT CLL
c>> 1996-10-15 STGEXT CLL
c
c  This subroutine handles extrapolation outside the convex hull
c  of given data points.
c
c  We partition the (infinite) space exterior to the convex hull by
c  constructing two rays into the exterior space rooted at each boundary
c  vertex, one ray perpendicular to each of the two boundary edges that
c  meet at the vertex.  The boundary has NB vertices.  This partitions
c  the exterior into 2*NB (infinite) cells.  We call these cells wedges
c  and blocks.  A wedge is a cell bounded by two boundary rays rooted at
c  a single vertex point.  A block is a cell bounded by a boundary edge
c  and the two rays perpendicular to each end of the edge.
c
c  We first determine which of these cells contains the specified point,
c  q, and then use an extrapolation formula in that cell that is
c  designed to maintain C0 continuity if Ncont = 0, or C1 continuity if
c  Ncont = 1.
c
c  The C0 extrapolation is exact for a linear function but not for
c  polynomials of degree > 1.
c  The C1 extrapolation is exact for a linear function but not for
c  polynomials of degree > 1.
c     ------------------------------------------------------------------
c                      Subroutine arguments
c
c  wantdz, dzout(1:2)
c   X(), Y() [in]  (x,y) coordinates of vertices of the
c                          triangular grid.
c
c   Z() [in]  Z(i) is the value at (X(i),Y(i)) of the data to
c                 be interpolated.
c
c   DZ(1:2, *) [in]  DZ(1:2, i) are the values of the partial
c         derivatives of the interpolation function at (X(i),Y(i)) with
c         respect to x and y, respectively.
c
c   Triang() [integer, in]  Array of integer pointers defining the
c            connectivity of the triangular grid.

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
c   Q(1:2) [floating, in]  The (x,y) coordinates of the point for which
c          this subr will compute an extrapolated value.
c
c   indtri [integer, inout]  On entry, this is the index of a boundary
c          triangle, relative to which Q is outside a boundary edge.
c          On return, this will be the index of a boundary triangle,
c          containing a boundary vertex that is a closest vertex on the
c          boundary to Q.  This is being returned for use as a starting
c          point for the next search, if desired.
c
c   MODE [integer, in]
c
c   ZOUT [out]  Extrapolated function value computed by this subroutine.
c
c   WANTDZ [in]  =.TRUE. means compute DZOUT() as well as ZOUT.
c              =.FALSE. means compute only ZOUT and not DZOUT().
c
c   DZOUT(1:2) [out]  First partial derivs w.r.t. x and y of the
c              extrapolated surface at the point, Q().
c     ------------------------------------------------------------------
c               Internal Variables
c
c  TRI(1:8)  Integer array of pointers defining one triangle.
c     TRI(1:3) are pointers to neighboring triangles in counterclockwise
c     order.
c     TRI(4:6) are pointers to vertex points in counterclockwise order.
c     Subroutine _TGGET sets TRI(1:6).  When needed in this subroutine
c     we set TRI(7) = TRI(4) and TRI(8) = TRI(5).
c     For i = 1, 2, or 3, the points TRI(i+3) and TRI(i+4) are endpoints
c     of the edge that is shared between this triangle and triangle
c     TRI(i).
c     If there is no adjacent triangle across this edge then TRI(i) = 0.
c
c  p0, p1, p2, ib [integers]  P0, p1 and p2 are indices of points.
c     Ib is the index into Bdry() such that p1 = Bdry(3, ib).
c
c     If Q is found to be in a "block", the block is rooted on the
c     boundary segment connecting vertices p0 and p1 in the clockwise
c     direction.  p0 = Bdry(3, Bdry(1, ib)), p1 = Bdry(3, ib).  The
c     index of the boundary triangle containing points p0 and p1 is
c     Bdry(4, ib).  P2 is not used in this case.
c
c     If Q is found to be in a "wedge", the wedge is rooted at p1.
c     P0, p1, and ib have the same relations as described above for Q
c     in a block.  In addition p2 is the next boundary vertex clockwise
c     from p1.  Thus p2 = Bdry(3, Bdry(2, ib)).  The index
c     of the boundary triangle containing points p1 and p2 is
c     Bdry(4, Bdry(2, ib)).
c     ------------------------------------------------------------------
c--S replaces "?": ?TGEXT, ?TGGET, ?TGQS, ?TGC0
c     ------------------------------------------------------------------
      integer mb
      integer bdry(4, mb), i, ib, indtri
      integer mode, ncont
      integer p0, p1, p2, tri(8), triang(*)
      real             alpha, b, beta, bsq, c
      real             derk0s, derk1s, dz(2,*), dzout(2)
      real             fu0, fuu0, fuuv, fvu0, fvuv
      real             g0, g1, h0s, h1s, k0s, k1s
      real             mean, one
      real             p0test, p1test
      real             q(2), q0(2), q2(2), recipb, s, ss, st
      real             sarray(3,4), six
      real             t, tt, two
      real             ugrad0, ugrad1
      real             vgrad0, vgrad1, vq, w0(2), w0norm, w2(2), w2norm
      real             x(*), xloc(3)
      real             y(*), yloc(3)
      real             z(*), z0, z2, zero, zout
        parameter(zero=0.0e0, one=1.0e0, two=2.0e0)
        parameter(six=6.0e0)
      logical wantdz

c     ------------------------------------------------------------------
      call STGGET(indtri, tri, triang)
      tri(7) = tri(4)
      p1 = tri(mode+3)
      p0 = tri(mode+4)
c
c        Set ib to be the index of p1 in the boundary list.
c
      ib = 1
        do 5 i = 1, MB
         if(Bdry(3, ib) .eq. p1) go to 10
           ib = Bdry(1, ib)
    5   continue
c     call IERM1(SUBNAM,INDIC,LEVEL,MSG,LABEL,VALUE,FLAG)
      call ierm1('STGEXT',1,2,'Index p0 not found in boundary list.',
     *     'P0',p0,'.')
      zout = zero
      return

   10 continue
c
c        Define a (u,v) coordinate system, with p0 as the origin and the
c        positive u axis in the direction of p1.  The point q is above
c        this u axis.
c
c        Is q to the right of p0 in the (u,v) system?
c
      p0test = (q(1) - x(p0)) * (x(p1)-x(p0)) +
     *         (q(2) - y(p0)) * (y(p1)-y(p0))
        if(p0test .ge. zero) then
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c            Searching to right (clockwise around boundary)
c
c        Is q to the left of p1 in the (u,v) system?
c
   20    continue
           p1test = (q(1) - x(p1)) * (x(p1)-x(p0)) +
     *            (q(2) - y(p1)) * (y(p1)-y(p0))
           if(p1test .le. zero) then
c
c              Q is in the block rooted on the segment from p0 to p1.
c
            bsq = (x(p1)-x(p0))**2 + (y(p1)-y(p0))**2
              s = one + p1test/bsq
              go to 200
         endif
c
c           Move one segment to right around boundary.
c           New (u,v) system is based on new p0 and p1.
c
         p0 = p1
         ib = Bdry(2, ib)
           p1 = Bdry(3, ib)
c
c           Is q to the left of p0 in the (u,v) system?
c
         p0test = (q(1) - x(p0)) * (x(p1)-x(p0)) +
     *            (q(2) - y(p0)) * (y(p1)-y(p0))
           if(p0test .le. zero) then
c
c              Q is in the wedge rooted at p0.
c              Change names so wedge is rooted at p1.
c
            p2 = p1
              p1 = p0
            ib = Bdry(1, ib)
                p0 = Bdry(3, Bdry(1, ib))
            go to 100
           endif
           go to 20
        else
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c            Searching to left (counterclockwise around boundary)
c
   30    continue
c
c           Move one segment to left around boundary.
c           New (u,v) system is based on new p0 and p1.
c
         p1 = p0
           ib = Bdry(1, ib)
           p0 = Bdry(3, Bdry(1, ib))
c
c           Is q to the right of p1 in the (u,v) system?
c
           p1test = (q(1) - x(p1)) * (x(p1)-x(p0)) +
     *            (q(2) - y(p1)) * (y(p1)-y(p0))
           if(p1test .ge. zero) then
c
c              Q is in the wedge rooted at p1.
c
            p2 = Bdry(3, Bdry(2, ib))
              go to 100
         endif
c
c           Is q to the right of p0 in the (u,v) system?
c
         p0test = (q(1) - x(p0)) * (x(p1)-x(p0)) +
     *            (q(2) - y(p0)) * (y(p1)-y(p0))
           if(p0test .ge. zero) then
c
c           Q is in the block rooted on the segment from p0 to p1.
c
            bsq = (x(p1)-x(p0))**2 + (y(p1)-y(p0))**2
              s = p0test/bsq
              go to 200
c            Compute value ...
           endif
           go to 30
        endif
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  100 continue
c
c        Here the point q is in a wedge rooted at p1 and
c        p1 = Bdry(3, ib).  We also have p0 and p2 as the next boundary
c        vertices in the counterclockwise and clockwise directions,
c        respectively.
c
c                Compute Zout, and optionally Dzout(1:2).
c
      if(ncont .eq. 0) then
c
c        Let d0 be the vector from p1 to p0. Let w0 be the vector
c        resulting from rotating d0 90 degrees clockwise.
c        Let d2 be the vector from p1 to p2. Let w2 be the vector
c        resulting from rotating d2 90 degrees counterclockwise.
c
c        Rescale w0 and w2 so they have the same length, say the
c        geometric mean of their original lengths.
c        Set q0 = p1+w0 and q2 = p1+w2.
c
c        Let z0 be the z value obtained by using STGC0 to do C0
c        extrapolation at q0 using the data of the boundary
c        triangle containing vertices p0 and p1.
c
c        Let z2 be the z value obtained by using STGC0 to do C0
c        extrapolation at q2 using the data of the boundary
c        triangle containing vertices p1 and p2.
c
c        Construct a triangle with vertices p1, q2, and q0,
c        and give it vertex z values z(p1), z2 and z0, respectively.
c
c        Use STGC0 with this triangle to interpolate/extrapolate
c        for a z value and/or for partial derivatives at Q.
c
           w0(1) =   y(p0) - y(p1)
           w0(2) = -(x(p0) - x(p1))
           w0norm = sqrt(w0(1)**2 + w0(2)**2)
           w2(1) = -(y(p2) - y(p1))
           w2(2) =   x(p2) - x(p1)
           w2norm = sqrt(w2(1)**2 + w2(2)**2)
           mean = sqrt(w0norm*w2norm)
         q0(1) = x(p1) + w0(1) * mean / w0norm
           q0(2) = y(p1) + w0(2) * mean / w0norm
           q2(1) = x(p1) + w2(1) * mean / w2norm
           q2(2) = y(p1) + w2(2) * mean / w2norm
         call STGGET(Bdry(4, ib), tri, triang)
         tri(7) = tri(4)
           tri(8) = tri(5)
           call STGQS(q0, tri, x, y, sarray)
           do 110 i = 1,3
              sarray(i,4) = z(tri(5+i))
  110    continue
         call STGC0  (Sarray, z0, .false., DZOUT)

         call STGGET(Bdry(4, Bdry(2, ib)), tri, triang)
         tri(7) = tri(4)
           tri(8) = tri(5)
           call STGQS(q2, tri, x, y, sarray)
           do 120 i = 1,3
              sarray(i,4) = z(tri(5+i))
  120    continue
         call STGC0  (Sarray, z2, .false., DZOUT)
           xloc(1) = x(p1)
           yloc(1) = y(p1)
           xloc(2) = q2(1)
           yloc(2) = q2(2)
           xloc(3) = q0(1)
           yloc(3) = q0(2)
           tri(4) = 1
           tri(5) = 2
           tri(6) = 3
           tri(7) = 1
           call STGQS(q, tri, xloc, yloc, sarray)
           sarray(1,4) = z0
           sarray(2,4) = z(p1)
           sarray(3,4) = z2
         call STGC0  (Sarray, ZOUT, WANTDZ, DZOUT)
      else
c                   Here we assume Ncont = 1
c
         zout = z(p1) + dz(1,p1)*(q(1)-x(p1)) + dz(2,p1)*(q(2)-y(p1))
           if(wantdz) then
            dzout(1) = dz(1,p1)
            dzout(2) = dz(2,p1)
           endif
      endif
      go to 800
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  200 continue
c
c        Here the point q is in a block rooted at the boundary edge
c        connecting p0 and p1.
c        Also, p0 = Bdry(3, Bdry(1, ib)), p1 = Bdry(3, ib)
c        Our goal is to compute Zout, and optionally Dzout(1:2).
c
c        P1 is in the clockwise direction around the boundary from p0.
c        The (x,y) coordinates of p0 and p1 are (x(p0), y(p0)) and
c        (x(p1), y(p1)) respectively.
c        We use an (x',y') system which is the translate of the
c        (x,y) system to have p0 as its origin.  Thus x' = x - x(p0) and
c        y' = y - y(p0).
c        We use a (u,v) coordinate system, which is a rotation of the
c        (x',y') system having its origin at p0 and the positive
c        u axis in the direction of p1.
c
c        Let b denote the (positive) distance from p0 to p1. The (u,v)
c        coordinates of p0 and p1 are (0,0) and (0,b) respectively.
c        We already have bsq (= b**2) computed so we can compute
c        b = sqrt(bsq).
c
c        Let uq and vq denote the u and v coordinates of q.  Define
c        s = uq/b.  We already have s computed, so we can compute
c        uq = b*s.
c
c        In cases where needed we will compute alpha and/or beta.
c        These allow construction of the (orthogonal) matrix for
c        transformations between the (x',y') and (u,v) coordinate
c        systems.  Alpha and beta are respectively the x' and y'
c        coordinates of p1.
c
c        Let J denote the 2x2 orthogonal matrix:
c                   J = |alpha   -beta|
c                       | beta   alpha|
c        Then
c                   J * |u| = |x'|
c                         |v|   |y'|
c
c                   J_transposed * |x'| = |u|
c                                  |y'|   |v|
c
c                   J * |df/du| = |df/dx|
c                       |df/dv| = |df/dy|
c
c                   J_transposed * |df/dx| = |df/du|
c                                    |df/dy|   |df/dv|
c
      if(ncont .eq. 0) then
         call STGGET(Bdry(4, ib), tri, triang)
         tri(7) = tri(4)
           tri(8) = tri(5)
           call STGQS(q, tri, x, y, sarray)
           do 210 i = 1,3
              sarray(i,4) = z(tri(5+i))
  210    continue
         call STGC0  (Sarray, ZOUT, WANTDZ, DZOUT)
*          zout = z(p0)*t + z(p1)*s
*          if(wantdz) then
*             ugrads = recipb * (z(p1) - z(p0))
*             alpha = (x(p1) - x(p0)) * recipb
*             beta = (y(p1) - y(p0)) * recipb
*             dzout(1) = alpha*ugrads
*             dzout(2) =  beta*ugrads
*        endif
        else
c                   Here we assume Ncont = 1
c
         b = sqrt(bsq)
           recipb = one/b
           t = one-s
           alpha = (x(p1) - x(p0)) * recipb
           beta  = (y(p1) - y(p0)) * recipb
           vq = -beta * (q(1)-x(p0)) + alpha * (q(2)-y(p0))
c
c           Test for and treat special cases of q at p0 or p1.
c           When q is at p0 we have vq and s both zero.
c           When q is at p1 we have vq and t both zero.
c
           if(vq .eq. zero) then
              if(s .eq. zero) then
                 zout = z(p0)
                 if(wantdz) then
                    dzout(1) = dz(1,p0)
                    dzout(2) = dz(2,p0)
                 endif
                 go to 800
              endif
              if(t .eq. zero) then
                 zout = z(p1)
                 if(wantdz) then
                    dzout(1) = dz(1,p1)
                    dzout(2) = dz(2,p1)
                 endif
                 go to 800
              endif
           endif
c
c           Partial derivs w.r.t. u and v at p0.
c
           ugrad0 = alpha*dz(1,p0) +  beta*dz(2,p0)
           vgrad0 = -beta*dz(1,p0) + alpha*dz(2,p0)
c
c           Partial derivs w.r.t. u and v at p1.
c
         ugrad1 = alpha*dz(1,p1) +  beta*dz(2,p1)
           vgrad1 = -beta*dz(1,p1) + alpha*dz(2,p1)

           ss = s**2
           tt = t**2
           h0s = (one + two*s) * tt
           h1s = (one + two*t) * ss
           k0s = s*tt
           k1s = -t*ss
           fu0 = z(p0)*h0s + z(p1)*h1s + b*(ugrad0*k0s + ugrad1*k1s)
           fvu0 = t*vgrad0 + s*vgrad1
           c = (vgrad1 - vgrad0) * recipb
           g0 = c*vq/(b*s + vq)
           g1 = c*vq/(b*t + vq)
           zout = fu0 + vq * fvu0 - b*vq*(k0s*g0 + k1s*g1)

           if(wantdz) then
              st = s*t
              derk0s = tt - two * st
              derk1s = ss - two * st
              fuu0 = recipb * six * st * (z(p1) - z(p0)) +
     *               ugrad0 * derk0s + ugrad1 * derk1s
              fuuv = fuu0 + c*vq -
     *               b*vq*(recipb * derk0s * g0
     *                     - k0s*g0/(b*s+vq)
     *                     + recipb * derk1s *g1
     *                     + k1s*g1/(b*t+vq))
              fvuv = fvu0 - b*(k0s*g0 + k1s*g1)
     *                      -b*c*vq*(k0s*b*s/(b*s+vq)**2
     *                               +k1s*b*t/(b*t+vq)**2)

              dzout(1) = alpha * fuuv -  beta * fvuv
              dzout(2) =  beta * fuuv + alpha * fvuv
         endif
      endif
  800 continue
        end
c     ==================================================================
      subroutine stgqs(q, tri, x, y, s)
c>> 1997-06-21 cll
c  Given point q() and the triangle described by tri(), this
c  subroutine puts values into s(1:3,1:3) to prepare for use of
c  subroutine STGC0 to evaluate the C0 interpolation/extrapolation
c  formulas.
c  The point q() is not required to be inside the triangle tri().
c  This subroutine uses tri(4:7)
c  See comments in subroutines STGC0 or DTGFND for specifications of the
c  elements of tri() and s().
c     ------------------------------------------------------------------
      integer j, j1, j2, tri(7)
      real             q(2), s(3,4), x(*), y(*)
c     ------------------------------------------------------------------
      DO 10 J=1,3
         J1= TRI(J+3)
         J2 = TRI(J+4)
         S(J,2) = X(J2)-X(J1)
         S(J,3) = Y(J2)-Y(J1)
         S(J,1) = S(J,2) * (Q(2)-Y(J1)) - S(J,3) * (Q(1)-X(J1))
   10 continue
      end
