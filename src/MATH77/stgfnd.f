      SUBROUTINE STGFND(X,Y, TRIANG,NT,Q,INDTRI,TRI,S,MODE)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1997-06-15 STGFND CLL In args: Removed NP. Changed meaning of MODE.
c>> 1996-03-30 STGFND  Krogh  Removed Fortran 90 comments.
C>> 1996-02-02 STGFND CLL
C>> 1995-09-26 STGFND CLL Editing for inclusion into MATH77.
C
c     C.L.LAWSON, JPL, 1976 DEC 3
c         LOOK UP POINT Q IN TRIANGULAR GRID.
c
c   X(1:NP), Y(1:NP) [in]  (x,y) coordinates of vertices of the
c                          triangular grid.
c
c         {NP is the number of vertices in the triangular grid, but is
c          not explicitly used in this subroutine.}
c
c   TRIANG(1:6*NT) [in]  Array of integer pointers defining the
c            connectivity of the triangular grid.
c
c   NT [in]  No. of triangles in the triangular grid.
c
c   Q(1:2) [in]  The (x,y) coordinates of the point for which this
c             subr will attempt to find an enclosing triangle.
c
c   INDTRI [in]  DESIGNATES TRIANGLE AT WHICH SEARCH WILL BEGIN.
c              ON RETURN INDTRI IS THE INDEX OF THE LAST TRIANGLE
c              TESTED.  THIS IS THE TRIANGLE CONTAINING Q IF MODE=0.
c              IF MODE = 1, 2, or 3, THIS IS A BOUNDARY TRIANGLE AND Q
c              IS OUTSIDE A BOUNDARY EDGE OF THIS TRIANGLE.
C
c   TRI(1:7) [out]  INTEGER ARRAY OF POINTERS ASSOCIATED
c              WITH TRIANGLE INDTRI.
c
c   S(1:3, 1:3) [out]  If MODE=0 on return, S(,) CONTAINS
c              S(:,1) = UNNORMALIZED BARYCENTRIC COORDINATES OF Q.
c              S(:,2) = X COORDINATES OF EDGE VECTORS.
c              S(:,3) = Y COORDINATES OF EDGE VECTORS.
C
c   MODE [out]
c      = 0  MEANS OK.  Q IS INTERIOR OR ALMOST SO.
c      = 1, 2, or 3.   Means Q is exterior by more than the built-in
c           tolerance. The value of MODE identifies the edge of the
c           triangle relative to which Q is outside.  TRI(MODE) will be
c           zero, indicating a boundary edge.  Q is outside this edge.
c           The vertices at the ends of this edge, in counterclockwise
c           order are TRI(3+MODE) and TRI(4+MODE).
c
c      = -1  BAD.  SUBR IS CYCLING IN THE SEARCH.  THIS SHOULD NEVER
c              HAPPEN.
C
c     ------------------------------------------------------------------
c         Details of the contents of TRI(1:7) and S(1:3, 1:3).
c
c  For descriptive convenience we regard the subscript of P() and the
c  1st subsubscript of S(,) as always being reduced modulo 3 to 1, 2,
c  or 3.  Also for convenience we shall write P(i) to mean the vertex
c  indexed by P(i).
c
c  TRI() contains integer pointers defining the triangle indexed by
c  INDTRI.  The indices of the vertices of this triangle, in counter-
c  clockwise order are P(1), P(2), and P(3) which may be obtained as
c  P(i) = TRI(3+i) for i = 1, 2, and 3.  TRI(7) contains the same
c  value as TRI(4).  The triangle adjacent to this triangle across
c  the edge from P(3+i) to P(4+i) is indexed by TRI(i) for i = 1, 2,
c  and 3.  If there is no adjacent triangle across this edge then
c  TRI(i) = 0.
c
c  The unnormalized barycentric coordinate that is zero along the edge
c  from P(i) to P(i+1) and has a positive value at P(i+2) is stored in
c  S(i,1).  The (x,y) coordinates of the vector from P(i) to P(i+1) are
c  stored in (S(i,2), S(i,3)).
c     ------------------------------------------------------------------
c--S replaces "?": ?TGFND, ?TGGET
c     ------------------------------------------------------------------
      integer icount, indtri, iskip, j, j1, j2, j2save, MODE, NT
      integer TRI(7), TRIANG(1:*)
      real             p1, p2, Q(2), S(3,3), X(*), Y(*)
c     ------------------------------------------------------------------
      IF( INDTRI .LE. 0 .OR. INDTRI .GT. NT) INDTRI=1
      MODE = -1
      ISKIP  = 0
      J2SAVE = 0
      DO 50 ICOUNT = 1, NT
         CALL STGGET(INDTRI,TRI,TRIANG)
         TRI(7)=TRI(4)

            DO 10 J=1,3
               J1= TRI(J+3)
               IF ( J1 .EQ. J2SAVE) THEN
                  ISKIP=J
               ELSE
                  J2 = TRI(J+4)
                  S(J,2) = X(J2)-X(J1)
                  P2 = S(J,2) * (Q(2)-Y(J1))
                  S(J,3) = Y(J2)-Y(J1)
                  P1 = S(J,3) * (Q(1)-X(J1))
                  S(J,1) = -P1+P2
C
                  IF (S(J,1) .LT. -1.0E-4*(ABS(P1)+ABS(P2))) THEN
C
c                        Q IS OUTSIDE TRIANGLE INDTRI BY MORE THAN
c                        TOLERANCE.  MOVE TO NEIGHBORING TRIANGLE IF
c                        THERE IS ONE.
C
                     IF (TRI(J) .NE. 0) THEN
                        J2SAVE = J2
                        INDTRI = TRI(J)
                        go to 40
                     ELSE
                        MODE = J
                        go to 80
                     END IF
                  END IF
               END IF
   10       continue
c                                  Q IS INSIDE OR ALMOST INSIDE
c                                  THIS TRIANGLE
            MODE = 0
            go to 90
   40    continue
   50 continue
   80 continue
   90 continue
c                             COMPUTE SKIPPED INFO FOR USE IN
c                             INTERPOLATION SUBR.
      IF (ISKIP .NE. 0) THEN
         J1=TRI(ISKIP+3)
         J2=TRI(ISKIP+4)
         S(ISKIP,2)= X(J2)-X(J1)
         S(ISKIP,3)= Y(J2)-Y(J1)
         S(ISKIP,1)= -S(ISKIP,3)*(Q(1)-X(J1))
     *               +S(ISKIP,2)*(Q(2)-Y(J1))
      END IF
      RETURN
      END
