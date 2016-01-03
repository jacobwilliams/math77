      SUBROUTINE STGC1  (NEWTRI, S, ZOUT, WANTDZ, DZOUT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1996-02-02 STGC1 CLL
C>> 1995-09-26 STGC1 CLL Editing for inclusion into MATH77.
c     C.L.Lawson, JPL, 1976 Dec 7
c     This subr interpolates over a triangle using the piecewise
c     bicubic formulas of  Clough and Tocher (1965) as formulated for
c     computation by C.L.Lawson in JPL Tech. Memorandum 33-770, May,1976
c     This method gives C1 continuity with neighboring triangles, i.e.,
c     continuity of value and first partial derivatives.
c     Generally there will be jumps in second and higher order
c     derivatives across edges between triangles, and along implicit
c     edges that partition each triangle into three smaller triangles.
c     Optionally the subr also computes first partial derivatives.
c
c   NEWTRI [in]  This argument allows certain computations to be
c          skipped, saving 15 multiplies, 3 divides, and 9 adds,
c          when the user has made no changes to the contents of
c          columns 2, 3, 5, 6, 7, 8, & 9 of the S() array since a
c          previous call.  (Thus it is allowable to have changed the
c          contents of columns 1 and 4.)
c          Setting NEWTRI = .TRUE. causes these computations to be done,
c          while .FALSE. cause them to be skipped.
C
c   (S(1:3, 1:9) [inout]  Columns 1-6 contain data set by the user to
c          specify the interpolation problem.  Col 1 contains
c          unnormalized barycentric coordinates of the interpolation
c          point.  The other cols contain data depending only on the
c          triangle and its vertex data and not on the interpolation
c          point.  Cols 7-9 are computed by this subroutine, and in
c          appropriate cases can be saved from one call to the next.
c          See NEWTRI above.
c
c         S( ,1) = UNNORMALIZED BARYCENTRIC COORDS OF INTERP POINT.
c         S( ,2) = U = X COORD OF EDGE VECTOR
c         S( ,3) = V = Y COORD OF EDGE VECTOR
c         S( ,4) = Z = FCN VALUE AT VERTEX
c         S( ,5) = ZX = PARTIAL DERIV OF Z W.R.T. X
c         S( ,6) = ZY = PARTIAL DERIV OF Z W.R.T. Y
c         S( ,7) = HTILDA = SCALED P.D. TANGENTIAL TO EDGE AT LEFT END.
c         S( ,8) = KTILDA = SCALED P.D. TANGENTIAL TO EDGE AT RIGHT END.
c         S( ,9) = LFAC   = FACTOR INVOLVING LENGTHS OF EDGES.
c
c   ZOUT [out]  INTERPOLATED VALUE COMPUTED BY SUBR.
C
c   WANTDZ [in]  =.TRUE. MEANS COMPUTE DZOUT() AS WELL AS ZOUT.
c              =.FALSE. MEANS COMPUTE ONLY ZOUT AND NOT DZOUT().
C
c   DZOUT(1:2) [out]  FIRST PARTIAL DERIVS W.R.T. X AND Y OF THE
c              INTERPOLATED SURFACE AT THE INTERPOLATION POINT.
c     ------------------------------------------------------------------
c         Details of the contents of TRI(1:7) and S(1:3, 1:6).
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
c  ZX, the partial deriv of Z w.r.t. X at P(i+2) is stored in S(i,5)
c     for i = 1, 2, & 3.
c  ZY, the partial deriv of Z w.r.t. Y at P(i+2) is stored in S(i,6)
c     for i = 1, 2, & 3.
c     ------------------------------------------------------------------
c--S replaces "?": ?TGC1
C     ------------------------------------------------------------------
      INTEGER          ADD1(3), i, im1, ip1, j, m, mm1, mp1, SUB1(3)
      REAL             A, B, C
      real             DA, DB, DC, DGTILD(3), DPHI(3)
      real             DR(3), DRHO(3), DZOUT(2)
      real             fac, GTILDA(3), LSQ(3)
      REAL             r(3),  RHO(3), PHI(3), S(3,9), sum
      REAL             zout
      LOGICAL  WANTDZ,NEWTRI
      DATA ADD1(1),ADD1(2),ADD1(3)/ 2, 3, 1 /
      DATA SUB1(1),SUB1(2),SUB1(3)/ 3, 1, 2 /
C     ------------------------------------------------------------------
c              COMPUTE R() = NORMALIZED BARYCENTRIC COORDINATES.
c              AND LSQ()   = SQUARED EDGE LENGTHS
C
      FAC = 1.E00/(S(1,1)+S(2,1)+S(3,1))
      DO 10 I=1,3
         R(I) = FAC*S(I,1)
         LSQ(I)= S(I,2)**2 + S(I,3)**2
   10 continue
c                             COMPUTE PHI(I) = R(I+1) * R(I-1)
      PHI(1) = R(2)*R(3)
      PHI(2) = R(3)*R(1)
      PHI(3) = R(1)*R(2)
C
c              COMPUTE RHO() = CLOUGH-TOCHER PIECEWISE BICUBIC
c                              CORRECTION FUNCTION
C
      IF ( R(1) .LE. R(2)) THEN
      IF ( R(1) .LE. R(3)) THEN
            M = 1
      ELSE
            M = 3
      END IF
      ELSE
      IF ( R(2) .LE. R(3)) THEN
            M = 2
      ELSE
            M = 3
      END IF
      END IF
C
      A = 0.5E0 * R(M)**2
      B = (1.0E0/3.0E0)*R(M)
      C = PHI(M) + (5.0E0/3.0E0)*A
      RHO(M) = R(M) * C - A
      MP1 = ADD1(M)
      MM1 = SUB1(M)
      RHO(MP1) = A*(R(MM1) - B)
      RHO(MM1) = A*(R(MP1) - B)
C
      SUM = 0.0e0
        DO 20 I=1,3
         IP1 = ADD1(I)
         IM1 = SUB1(I)
C
         IF (NEWTRI ) THEN
c                                                               H TILDA
            S(I,7) = S(I,2)*S(IP1,5) + S(I,3)*S(IP1,6)
c                                                               K TILDA
            S(I,8) = S(I,2)*S(IM1,5) + S(I,3)*S(IM1,6)
c                                                               LFAC
            S(I,9) = 3.0E0*(LSQ(IP1)-LSQ(IM1)) / LSQ(I)
         END IF
c                                                               G TILDA
         GTILDA(I)= (R(IP1)-R(IM1))*PHI(I) + S(I,9)*RHO(I)
     *              -RHO(IP1)+RHO(IM1)
         SUM = SUM + S(I,7)*(GTILDA(I)+PHI(I))
     *             + S(I,8)*(GTILDA(I)-PHI(I))
   20 continue
      ZOUT = 0.5E0 * SUM
C
      DO 30 I=1,3
         IP1 = ADD1(I)
         IM1 = SUB1(I)
         ZOUT=ZOUT + S(I,4)*(R(I)+GTILDA(IM1)-GTILDA(IP1))
   30 continue
c                                  FINISHED COMPUTING ZOUT.
C
c                                  NOW COMPUTE DZOUT() IF REQUESTED.
      IF ( WANTDZ) THEN
         DO 100 J=1,2
            if(j .eq. 1) then
               DO 40 I=1,3
                  DR(I) = -FAC*S(I,3)
   40          continue
            else
               DO 50 I=1,3
                  DR(I) = +FAC*S(I,2)
   50          continue
            endif
C
            DPHI(1) = R(2)*DR(3) + DR(2)*R(3)
            DPHI(2) = R(3)*DR(1) + DR(3)*R(1)
            DPHI(3) = R(1)*DR(2) + DR(1)*R(2)
C
            DA = R(M)*DR(M)
            DB = (1.E0/3.E0)*DR(M)
            DC = DPHI(M) + (5.E0/3.E0)*DA
            DRHO(M) = R(M)*DC + DR(M)*C - DA
            DRHO(MP1) = A*(DR(MM1) - DB) + DA*(R(MM1) - B)
            DRHO(MM1) = A*(DR(MP1) - DB) + DA*(R(MP1) - B)
C
            SUM = 0.0e0
            DO 60 I=1,3
               IP1=ADD1(I)
               IM1=SUB1(I)
               DGTILD(I)  =(R(IP1)-R(IM1))*DPHI(I)
     *                  +(DR(IP1) - DR(IM1))*PHI(I)
     *                  +S(I,9)*DRHO(I) - DRHO(IP1) + DRHO(IM1)
               SUM = SUM +S(I,7)*(DGTILD(I)+DPHI(I))
     *                +S(I,8)*(DGTILD(I)-DPHI(I))
   60       continue
            SUM = 0.5E0 * SUM
            DO 70 I=1,3
               IP1=ADD1(I)
               IM1=SUB1(I)
               SUM = SUM + S(I,4)*(DR(I) + DGTILD(IM1) - DGTILD(IP1))
   70       continue
            DZOUT(J) = SUM
  100    continue
      END IF
      RETURN
      END
