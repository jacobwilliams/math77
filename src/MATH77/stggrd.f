      SUBROUTINE STGGRD(X,Y,NP,IP,W,TRIANG,MT,B,MB,NT,INFO)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c  File: stggrd.[f|for]
c  contains stggrd, stgang, stgadj, stgupd
c
c>> 1997-07-01 STGGRD Krogh Reversed subscripts in B (CLL suggestion)
c>> 1997-06-14 STGGRD Krogh Removed implicit none, added external stmt.
c>> 1997-06-14 STGGRD CLL Argument INFO() replaces NBUSED, MODE, & OK1.
C>> 1997-06-09 STGGRD  Krogh  Converted SFTRAN code to Fortran.
C>> 1995-09-26 STGGRD CLL Editing for inclusion into MATH77.
c>> 1991-11-20 STGGRD CLL Using Math77 subroutine _SORTP for sorting.
C
C         THIS SUBR CONSTRUCTS A LIST OF POINTERS IN THE ARRAY
C    TRIANG() DEFINING A TRIANGULAR GRID HAVING THE GIVEN (X,Y)
C    DATA AS VERTICES.
C         THE GRID WILL BE OPTIMAL IN THE SENSE OF THE MAX-MIN ANGLE
C    CRITERION.  IT HAS BEEN SHOWN, INDEPENDENTLY BY LAWSON AND BY
C    R. SIBSON, THAT THIS IS EQUIVALENT TO SOME OTHER OPTIMALLITY
C    CRITERIA.  THE GRID PRODUCED IS A DELAUNAY TRIANGULATION AND
C    IS THE GRAPH THEORETIC DUAL OF A TESSELLATION MADE UP OF
C    PROXIMITY REGIONS ASSOCIATED WITH THE NAMES DIRICHLET,
C    VORONOI, AND THIESSEN.
C
C    C. L. LAWSON, JPL, 1976 NOV 28
C     C.L.L., 1979 MAR 3. CONVERTED TO SFTRAN3.
C     C.L.L., 1979 JUL 22. ADDED CALL TO STGSIZ.
C
C  X(1:NP),Y(1:NP) [in]    (X,Y) DATA POINTS.
C  NP [in]                    NO. OF DATA POINTS.  REQUIRE NP .GE. 3.
C
C  IP() [scratch]   WORK SPACE OF LENGTH at least NP.
C  W()  [scratch]   WORK SPACE OF LENGTH at least NP.
C  TRIANG() [out]  ARRAY IN WHICH THIS SUBR WILL BUILD A LIST DEFINING
C             A triangular grid.  access to this list is via the
C             four subrs STGGET, STGPUT, STGSIZ, and STGSET.
C  MT [in]   DIMENSION OF ARRAY TRIANG().  NO. OF TRIANGLES IT
C            CAN HOLD, NTLIM, IS DETERMINED BY USE OF SUBROUTINE
C            STGSIZ.    NTLIM WILL BE MT/6 IF POINTERS ARE NOT
C            PACKED.  NTLIM = (3*MT)/6 IF POINTERS ARE PACKED
C            THREE PER WORD.
c  Bdry(1:4, MB) [integer, out]  Array containing pointers defining the
c      boundary of the (convex) triangular grid.
C         Bdry(1, K) = FWD POINTER. Points to next vertex in
c                     counterclockwise order.
C         Bdry(2, K) = BACKWARD POINTER. Points to next vertex in
c                     clockwise order.
C         Bdry(3, K) = A BOUNDARY POINT
C         Bdry(4, K) = A BOUNDARY TRIANGLE
c      The triangle Bdry(4, K) has a boundary edge that
c      connects the points Bdry(3, K) and Bdry(3, K+1).
c
c      On return from this subroutine, some elements of the array Bdry()
c      may not be members of the linked list defining the boundary. The
c      entry with K = 1 will always be a member of the boundary list.
c      Thus for another subroutine to scan the Bdry() list made by this
c      subroutine it should start with K = 1 and follow the forward or
c      backward pointers.
c
C  MB [in]   FIRST DIMENSION OF B(,).  SETTING MB = NP+1 WILL
C            ALWAYS BE ADEQUATE, HOWEVER A MUCH SMALLER VALUE
C            WILL OFTEN SUFFICE.  SUGGEST TRYING MB = 6 * (cube root
C            of NP) WHEN NP .GE. 64.
C  NT [out]  OUTPUT..  NO. OF TRIANGLES
c
c  INFO(1:3) [out, integer]  Termination status information.
c
c     INFO(1) INDICATES STATUS ON TERMINATION.
C              0  NORMAL TERMINATION.  The triangular grid is complete
c                 and successfully optimized.
C              1  ERROR. The triangular grid is complete but not
c                 successfully optimized.  Apparent looping in STGADJ.
C                 GRID MAY NOT BE OPTIMAL.
C              2  ERROR.  ALL GIVEN POINTS ARE COLINEAR.  NO TRIANGLES
C                 CONSTRUCTED.  (OR ELSE THERE ARE SOME DUPLICATE PTS)
C              3  ERROR.  DUPLICATE POINTS.  TRIANGULAR GRID NOT
C                 COMPLETED.
C              4  ERROR.  NOT ENOUGH SPACE IN B(,).  USER MUST
C                 INCREASE MB.
C              5  NOT ENOUGH SPACE IN TRIANG().  USER MUST MAKE
C                 MT BIGGER.
C     INFO(2)  NO. OF BOUNDARY POINTS.
C     INFO(3)  SMALLEST SETTING OF MB THAT WOULD SUFFICE FOR THIS DATA.
c
c        [Relations to internal variables or previously used variable
c         names:  INFO(1) = MODE, INFO(2) = NB, INFO(3) = MBUSED,
c         The value of INFO(1) = MODE will be  set to 0 when
c         OK1 = .true. and 1 when OK1 = .false.]
C
C                  USE SUBR STGSIZ TO COMPUTE NTLIM, THE MAX NO. OF
C                 TRIANGLES, GIVEN THE DIMENSION PARAMETER MT.
C
c     ------------------------------------------------------------------
c--S replaces "?": ?TGGRD, ?TGANG, ?TGSET, ?TGGET, ?TGPUT, ?TGSIZ,
c--&               ?TGADJ, ?SORTP, ?TGUPD
c     ------------------------------------------------------------------
      INTEGER NP, mb, mt
      INTEGER AVAIL, B(4, MB), ext, TRI(6)
      integer i, i1, i2, i3, i3prev, index, INFO(3), ip(np), isave
      integer jhi, jlo, jnew
      integer k, kb1, kbhi, kblo, kbnew, kbz
      integer mode
      integer nb, nt, ntlim, mbused,  TRIANG(MT)
      real             aref, cross
      external STGANG
      real             STGANG
      real             dx12, dy12, eight, pangle
      REAL             W(np), X(NP), xmean, Y(NP), ymean, zero
        parameter(zero = 0.0e0, eight = 8.0e0)
      LOGICAL  OK, OK1
c     ------------------------------------------------------------------
      call STGSIZ( MT, NTLIM )
      MODE = 0
      OK1 = .TRUE.
C
C        FIND AN EXTREME POINT (X(EXT),Y(EXT))
C
      EXT=1
      do 10 I = 1, NP
         if ( X(I) .lt. X(EXT)
     *     .or. (X(I) .eq. X(EXT)  .and.  Y(I) .lt. Y(EXT) )) EXT=I
   10 continue
C
C             COMPUTE SQUARED DISTANCES FROM (X(EXT),Y(EXT)).
C             ALSO INITIALIZE IP().
      do 20 I = 1, NP
         W(I) = (X(I)-X(EXT))**2 + (Y(I)-Y(EXT))**2
   20 continue
c
c        Use _SORTP from the JPL MATH77 library to
c        determine a sorted order for the contents of W() and set
c        indices indicating the sorted order in IP().
c        IP() will be set so that W(IP(1)) is the smallest number in
c        W(), W(IP(2)) is the next larger number, etc.  The contents
c        of IP() will be used as indices into the X() and Y() arrays.
c        The contents of W() will not be reordered by _SORTP, however
c        this does not matter here since the next time we reference W()
c        will be to store other things into it.
c
      call SSORTP(W, 1, NP, IP)
C
C             BUILD THE FIRST TRIANGLE USING POINTS IP(1), IP(2), AND
C             THE NEXT POINT THAT IS NOT COLINEAR WITH THESE TWO.
C
      I1=IP(1)
      I2=IP(2)
      DX12= X(I2)-X(I1)
      DY12= Y(I2)-Y(I1)
      I3PREV = 0
      do 40 I = 3, NP
         I3 = IP(I)
         IP(I) = I3PREV
         CROSS =-(X(I3)-X(I2))*DY12 + (Y(I3)-Y(I2))*DX12
         if (CROSS .ne. 0.) then
            if (CROSS .lt. 0.) then
C                                 SWAP I2 AND I3
               ISAVE=I2
               I2   =I3
               I3   =ISAVE
            end if
            go to 50
         end if
         I3PREV=I3
   40 continue
C
C             ERROR IF DROP THRU TO HERE.  ALL NP POINTS ARE COLINEAR
C             OR ELSE POINTS I1 AND I2 ARE IDENTICAL.
C
      MODE = 2
      go to 999
C
C
   50 continue
C                  HERE THE VERTICES OF THE FIRST TRIANGLE ARE I1,I2,I3
C
C                  COMPUTE MEAN OF VERTICES OF FIRST TRIANGLE.
C
      XMEAN = (X(I1)+X(I2)+X(I3))/3.
      YMEAN = (Y(I1)+Y(I2)+Y(I3))/3.
C
C             COMPUTE THE PSEUDOANGLE OF VECTOR FROM (XMEAN,YMEAN)
C             TO I1.
C
      AREF = STGANG(X(I1)-XMEAN, Y(I1)-YMEAN, zero)
C
C        INITIALIZE BOUNDARY POINTER STRUCTURE B(,).
C        B(1, K) = FWD POINTER
C        B(2, K) = BACKWARD POINTER
C        B(3, K) = A BOUNDARY POINT
C        B(4, K) = A BOUNDARY TRIANGLE
C        W(K)   = PSEUDOANGLE OF POINT B(3, K)
C
      do 70 K = 1, MB
         B(1, K) = K+1
   70 continue
      B(1, MB) = 0
      B(1, 4)  = 0
      AVAIL   = 5
      do 80 K = 1, 4
         B(2, K) = K-1
         B(4, K) = 1
   80 continue
      B(4, 4) = 0
C
      B(3, 1) = I1
      B(3, 2) = I2
      B(3, 3) = I3
      B(3, 4) = I1
C
      W(1) = zero
      W(2) = STGANG(X(I2)-XMEAN,Y(I2)-YMEAN,AREF)
      W(3) = STGANG(X(I3)-XMEAN,Y(I3)-YMEAN,AREF)
      W(4) = eight
      NB = 3
      MBUSED = 3
C
C                  BUILD FIRST TRIANGLE IN TRIANG().
C
      if (NTLIM .le. 0) then
         MODE = 5
         go to 999
      end if
      call STGSET(1,0,0,0,I1,I2,I3,TRIANG,MT)
      NT=1
C    ******************************************************************
C
C                  BEGIN MAIN LOOP.  ADD ONE NEW POINT AT A TIME INTO
C                  THE TRIANGULAR GRID STRUCTURE.
C
C    ******************************************************************
      KBNEW=2
      do 200 INDEX = 4, NP
         JNEW = IP(INDEX)
         PANGLE = STGANG(X(JNEW)-XMEAN, Y(JNEW)-YMEAN, AREF)
C
C                  USE  PANGLE  AS SEARCH KEY IN THE TABLE OF
C                  PSEUDO ANGLES OF BOUNDARY POINTS.
C                  START SEARCH AT PREVIOUS NEW BOUNDARY POINT.
C
         KB1= KBNEW
         if ( PANGLE .gt. W(KB1)) then
            KBHI = KB1
  100       KBLO = KBHI
            KBHI = B(1, KBLO)
            if ( PANGLE .gt. W(KBHI) ) go to 100
         else
C                            HERE WE HAVE  PANGLE .le. W(KB1)
            KBLO = KB1
  110       KBHI = KBLO
            KBLO = B(2, KBHI)
            if ( PANGLE .lt. W(KBLO) ) go to 110
         end if
C
         JLO = B(3, KBLO)
         JHI = B(3, KBHI)
C
C                            TEST FOR DUPLICATE POINTS
C
         if ( PANGLE .eq. W(KBLO) ) then
            if ( X(JNEW) .eq. X(JLO) .and. Y(JNEW).eq.Y(JLO)) then
               MODE = 3
               go to 999
            end if
         else if (PANGLE .eq. W(KBHI) ) then
            if (X(JNEW).eq.X(JHI) .and. Y(JNEW).eq.Y(JHI)) then
               MODE = 3
               go to 999
            end if
         end if
C
C                            Attach point JNEW to JHI and JLOW.
         if (AVAIL .eq. 0) then
C                                 ERROR..  INSUFFICIENT STORAGE IN B()
C                                 TO REPRESENT THE BOUNDARY.
            MODE = 4
              go to 999
           endif
         if (NT   .ge. NTLIM) then
C                                 ERROR..  INSUFFICIENT STORAGE IN
C                                 TRIANG()
            MODE=5
            go to 999
         endif
C                                 UPDATE TRIANG().
            NT=NT+1
            call STGUPD(B(3, KBLO), B(4, KBLO), TRI, TRIANG, MT, NT)
            call STGSET(NT,B(4, KBLO),0,0,JHI,JLO,JNEW,TRIANG,MT)
C
C                                 UPDATE B() AND W().
            NB = NB+1
            if (NB .gt. MBUSED) MBUSED = NB
            KBNEW = AVAIL
            AVAIL = B(1, AVAIL)
            B(1, KBNEW) = KBHI
            B(2, KBNEW) = KBLO
            B(3, KBNEW) = JNEW
            B(4, KBNEW) = NT
            W(KBNEW)   = PANGLE
            B(1, KBLO)  = KBNEW
            B(2, KBHI)  = KBNEW
            B(4, KBLO)  = NT
C
            call STGADJ(NT,1,X,Y,NP,TRIANG,MT,B,MB, KBNEW, OK)
            OK1 = OK1 .and. OK

C  Done attaching point JNEW to JHI and JLOW.
C  We have just assigned JNEW the index KBNEW in the boundary list.
C
C             LOOP BACKWARD ALONG BOUNDARY FROM KBNEW TO FIND OTHER
C             POINTS TO WHICH POINT JNEW CAN BE CONNECTED.
C
         KBHI= KBLO
         KBLO = B(2, KBHI)
  130    if (KBLO .ne. 0) then
            JHI = B(3, KBHI)
            JLO = B(3, KBLO)
            CROSS = -(Y(JHI)-Y(JNEW)) * (X(JLO)-X(JHI))
     *              +(X(JHI)-X(JNEW)) * (Y(JLO)-Y(JHI))
            if (CROSS .gt. 0.) then
C                  CONNECT POINT JNEW TO JLO
               if (NT .ge. NTLIM) then
C                    ERROR..  INSUFFICIENT SPACE IN TRIANG().
C
                  MODE=5
                    go to 999
               endif
C                              UPDATE TRIANG()
                  NT=NT+1
                  call STGUPD(B(3,KBLO), B(4,KBLO), TRI, TRIANG, MT, NT)
                  call STGUPD(B(3,KBHI), B(4,KBHI), TRI, TRIANG, MT, NT)
                  call STGSET(NT, B(4, KBLO), 0, B(4, KBHI), JHI, JLO,
     1               JNEW, TRIANG, MT)
C
C                              UPDATE B()
                  B(1, KBLO) = KBNEW
                  B(2, KBNEW)= KBLO
                  B(4, KBLO) = NT
                  NB=NB-1
                  B(1, KBHI) = AVAIL
                  AVAIL     = KBHI
C
                  call STGADJ(NT,1,X,Y,NP,TRIANG,MT,B,MB, KBNEW, OK)
                  OK1 = OK1 .and. OK
c                     Done connecting point JNEW to JLO
c
                  KBHI = KBLO
                  KBLO = B(2, KBHI)
                  go to 130

            end if
         end if
C
C          LOOP FORWARD ALONG BOUNDARY FROM KBNEW TO FIND OTHER
C          POINTS TO WHICH POINT JNEW CAN BE CONNECTED.
C
         KBLO = B(1, KBNEW)
         KBHI = B(1, KBLO)
  150    if (KBHI.ne.0) then
            JHI= B(3, KBHI)
            JLO= B(3, KBLO)
            CROSS = -(Y(JLO)-Y(JHI)) * (X(JNEW)-X(JLO))
     *         +(X(JLO)-X(JHI)) * (Y(JNEW)-Y(JLO))
            if (CROSS .gt. 0) then
               if (NT .ge. NTLIM) then
C                       ERROR..  INSUFFICIENT SPACE IN TRIANG().
                  MODE = 5
                    go to 999
               endif
C          Connect point JNEW to JHI -- Update TRIANG()
                  NT=NT+1
                  call STGUPD(B(3, KBLO),B(4, KBLO),TRI,TRIANG,MT,NT)
                  call STGUPD(B(3, KBNEW),B(4, KBNEW),TRI,TRIANG,MT,NT)
                  call STGSET(NT, B(4, KBLO), B(4, KBNEW), 0, JHI, JLO,
     1               JNEW, TRIANG, MT)
C
C                                 UPDATE B()
                  B(1, KBNEW) = KBHI
                  B(2, KBHI)  = KBNEW
                  B(4, KBNEW) = NT
                  NB=NB-1
                  B(1, KBLO)  = AVAIL
                  AVAIL      = KBLO
C
                  call STGADJ(NT,1,X,Y,NP,TRIANG,MT,B,MB, KBNEW, OK)
                  OK1 = OK1 .and. OK
C                                 Done connecting point JNEW to JHI
c
                  KBLO = KBHI
                  KBHI = B(1, KBLO)
                  go to 150

            end if
         end if
  200 continue
      MBUSED = MBUSED + 1
C
  999 continue
c
c        As built, the linked boundary list runs (counterclockwise) from
c        b(*, 1) to b(*, 4), with b(*, 1) and b(*, 4) representing the
c        same point.  The back pointer in b(*, 1) and the forward
c        pointer in b(*, 4) are both zero, indicating ends of the list.
c        Change this so the linked list is fully circular, still
c        including b(*, 1), but detaching b(*, 4).  Let kbz be the value
c        of the back pointer in b(*, 4).  Set the back pointer in b(*,
c        1) to be kbz, and the forward pointer in b(*, kbz) to be 1.
c
      kbz = b(2, 4)
      b(2, 1) = kbz
      b(1, kbz) = 1

      if(MODE .eq. 0 .and. .not. OK1) MODE = 1
      INFO(1) = MODE
      INFO(2) = NB
      INFO(3) = MBUSED
      end
c     ==================================================================

      subroutine STGUPD( JFIX, KB4, TRI, TRIANG, MT, NT)
c               Updates TRIANG, using JFIX and KB4
      integer JFIX, KB4, MT, NT, TRI(6), TRIANG(MT)
      integer I
c
      call STGGET(KB4,TRI,TRIANG)
      do 10 I = 1, 3
         if (TRI(I+3) .eq. JFIX) then
            TRI(I) = NT
            go to 20
         end if
   10 continue
   20 call STGPUT(KB4, TRI, TRIANG, MT)
      return
      end
c     ==================================================================
      REAL             FUNCTION STGANG  (XX,YY,AREF)
C>> 1995-09-26 CLL Editing for inclusion into MATH77.
C
c     Compute the pseudoangle  STGANG  between the reference direction
c     whose pseudoangle is  AREF  and the vector  (XX,YY).
c     Pseudoangle is measured counterclockwise with period 8.
c     Require  0. .le. AREF .lt. 8. on input.
c     On return STGANG will be in the interval 0. .le. STGANG .lt. 8.
C
c     C.L.LAWSON, JPL, 1976 NOV 8
C
c     ------------------------------------------------------------------
      real             xx,yy,aref,a
      real             eight, four, two, zero
      parameter(eight = 8.0e0, four = 4.0e0, two = 2.0e0, zero = 0.0e0)
c     ------------------------------------------------------------------
      if (abs(xx) .gt. abs(yy)) then
         a = yy/xx
         if (xx .lt. zero) then
            a=a+four
         else
            if (yy .lt. zero) then
               a=a+ eight
            end if
         end if
      else
         if (yy .eq. zero) then
            a = zero
         else
            a = two - xx/yy
            if (yy .lt. zero) a = a+four
         end if
      end if
c     .                            A IS NOW THE PSEUDOANGLE OF (XX,YY)
c     .                            RELATIVE TO (1.,0.) AND LYING IN
c     .                            THE INTERVAL   0. .le. A .lt. 8.
      stgang = a - aref
      if (stgang .lt. zero) stgang = stgang + eight
      return
      end
c     ==================================================================
      SUBROUTINE STGADJ(TIN,NIN,X,Y,NP,TRIANG,MT,B,MB,KBNEW, OK)
C>> 1995-09-26 CLL Editing for inclusion into MATH77.
C
C     C.L.LAWSON, JPL, 1976 NOV 30, Changed 1977 APR 7
C     Changed 1977 June 16 to get rid of internal arrays
c     used to stack triangles to be tested.
c
c     Given triangle TIN and the neighboring triangle that is in
c     position NIN relative to TIN,  Test for possible exchange of
c     their common edge.
c     Use the criterion of maximizing the smallest angle, which is the
c     same as the criterion of the empty circumcircle.
c     If the swap is made then certain other edges must be tested.
c     This subr does the tests, swaps edges as needed, and does
c     tests and swaps on other edges as needed.
c     The counter COUNT is used to guard against infinite looping
c     which should never happen anyway.  If count exceeds 100 the
c     subr sets OK = .false. indicating that the triangulation
c     produced may not be optimal.
c     ------------------------------------------------------------------
C                       Subroutine Arguments
c
c  TIN [integer, in]
c  NIN [integer, in]
c  X(),Y() [floating, in]
c  TRIANG() [integer, inout]
c  MT [integer, in]
c  B(1:4, ) [integer, inout]
c  MB [integer, in]
c  KBNEW [integer, in]  Index into the boundary array, B(,).
c  OK [logical, out]
c     ------------------------------------------------------------------
      integer mb
      INTEGER  ADD1(3)
      INTEGER  B(4, MB), COUNT
      integer i, k, kbkwd,KBNEW, kfwd
      integer more, mt
      integer nabor1, nabor2, nin, np
      INTEGER  QV1,QV2,QV3,QV4,QV1S(3),QV2S(3),QV3S(3),SUB1(3)
      INTEGER  t1, T11,T12,t2, T21,T22, tin
      INTEGER  TNEW, TOLD, TRI1(6),TRI2(6), TRIANG(MT)
      real             a123, a234, a341, a412
      real             diag13, diag24
      real             dx12, dx23, dx34, dx41, dy12, dy23, dy34, dy41
      real             s12, s23, s34, s41
      real             tau13, tau24
      REAL             X(NP),Y(NP)
      LOGICAL  OK, SWAP
      DATA QV1S(1),QV1S(2),QV1S(3)/ 5, 6, 4/
      DATA QV2S(1),QV2S(2),QV2S(3)/ 6, 4, 5/
      DATA QV3S(1),QV3S(2),QV3S(3)/ 4, 5, 6/
      DATA ADD1(1),ADD1(2),ADD1(3)/ 2, 3, 1/
      DATA SUB1(1),SUB1(2),SUB1(3)/ 3, 1, 2/
c     ------------------------------------------------------------------
      T1=TIN
      NABOR1 = NIN
      COUNT = 1
      MORE = 1
      OK = .TRUE.
   20 if ((MORE .gt. 0) .and. OK) then

         call STGGET(T1,TRI1,TRIANG)
         T2 = TRI1(NABOR1)
         if ( T2 .ne. 0) then
C T2=0 MEANS SIDE NABOR OF TRIANGLE T1 IS A BOUNDARY EDGE SO THERE IS NO
C TRIANGLE T2.  HERE WE HAVE  T2 .ne. 0
            call STGGET(T2,TRI2,TRIANG)
C  SET QV1,...,QV4 AS INDICES OF THE 4 VERTICES OF THE QUADRILATERAL
C  FORMED BY THE 2 TRIANGLES T1 AND T2
            I=QV1S(NABOR1)
            QV1=TRI1(I)
            I=QV2S(NABOR1)
            QV2=TRI1(I)
            I=QV3S(NABOR1)
            QV3=TRI1(I)
            do 40 NABOR2 = 1, 3
               if (TRI2(NABOR2) .eq. T1) then
                  I=QV2S(NABOR2)
                  QV4=TRI2(I)
                  go to 50
               end if
   40       continue
   50       continue

C The max-min angle test below is made using the max-min angle criterion
c as described in JPL internal TECH.  MEMO.  914-299, FEB., 1972 by
c C. L.  LAWSON.
C
            DX23 = X(QV3)-X(QV2)
            DY23 = Y(QV3)-Y(QV2)
            DX34 = X(QV4)-X(QV3)
            DY34 = Y(QV4)-Y(QV3)
            A234 = DX23*DY34 - DY23*DX34
            if (A234 .le. 0.) then
               SWAP = .FALSE.
            else
               DX12 = X(QV2)-X(QV1)
               DY12 = Y(QV2)-Y(QV1)
               DX41 = X(QV1)-X(QV4)
               DY41 = Y(QV1)-Y(QV4)
               A412 = DX41*DY12 - DY41*DX12
               if (A412 .le. 0.) then
                  SWAP = .FALSE.
               else
                  A341 = DX34*DY41 - DY34*DX41
                  A123 = A234 + A412 - A341
                  S12 = DX12**2 + DY12**2
                  S23 = DX23**2 + DY23**2
                  S34 = DX34**2 + DY34**2
                  S41 = DX41**2 + DY41**2
                  DIAG13 = (X(QV3)-X(QV1))**2 + (Y(QV3)-Y(QV1))**2
                  DIAG24 = (X(QV4)-X(QV2))**2 + (Y(QV4)-Y(QV2))**2
                  TAU13 = min( A123**2 / max(S12,S23),
     *               A341**2 / max(S34,S41)) / DIAG13
                  TAU24 = min( A234**2 / max(S23,S34),
     *               A412**2 / max(S41,S12)) / DIAG24
                  SWAP = TAU24 .gt. TAU13
               end if
            end if
c End of max-min angle test
         else
            SWAP = .FALSE.
         end if
         if ( SWAP) then
c  Remove the edge connecting QV1 and QV3.  Insert new edge connecting
C  QV2 and QV4.  Redefine triangle T1 to have vertices QV1, QV2, and
C  QV4.  Redefine triangle T2 to have vertices QV2, QV3, AND QV4.
            I = ADD1(NABOR1)
            T11 = TRI1(I)
            I = ADD1(I)
            T12 = TRI1(I)
C
            I   = ADD1(NABOR2)
            T21 = TRI2(I)
            I   = ADD1(I)
            T22 = TRI2(I)
C
            call STGSET(T1,T11,T2,T22,QV1,QV2,QV4,TRIANG,MT)
            call STGSET(T2,T12,T21,T1,QV2,QV3,QV4,TRIANG,MT)
C
            TNEW=T2
            if ( T12 .ne. 0) then
               call STGGET( T12,TRI2, TRIANG)
               do 70 I = 1, 3
                  if (TRI2(I) .eq. T1) then
                     TRI2(I) = TNEW
                     go to 80
                  end if
   70          continue
   80          call STGPUT( T12,TRI2, TRIANG, MT)
            else
C The following test of (QV2 .eq.  B(3, KBNEW)) will always be true if
C the outer algorithm calling this subroutine is the one originally
C planned, always adding new points from the exterior, not from the
C interior.  The false branch is provided to handle the more general
c case.
               if (QV2 .eq. B(3, KBNEW) ) then
                  B(4, KBNEW) = TNEW
               else
C                 WRITE(6,1001) QV2,B(3, KBNEW)
c Search alternately fwd and bkwd from KBNEW in the boundary list B()
c for the point QV2.  When found set the associated triangle to TNEW.
                  KFWD = KBNEW
                  KBKWD= B(2, KFWD )
   90             continue
                     if (KFWD .ne. 0) then
                        if (B(3, KFWD) .eq. QV2) then
                           B(4, KFWD) = TNEW
                           go to 100
                        end if
                        KFWD = B(1, KFWD)
                     end if
C
                     if (KBKWD .ne. 0) then
                        if (B(3, KBKWD) .eq. QV2) then
                           B(4, KBKWD) = TNEW
                           go to 100
                        end if
                        KBKWD = B(2, KBKWD)
                     end if
                  go to 90
               end if
            end if
C
  100       TNEW=T1
            if ( T22 .ne. 0) then
               call STGGET( T22,TRI2, TRIANG)
               DO 110 I = 1, 3
                  if (TRI2(I) .eq. T2) then
                     TRI2(I) = TNEW
                     go to 120
                  end if
  110          continue
  120          call STGPUT( T22,TRI2, TRIANG, MT)
            else
C              WRITE(6,1002) QV4
c Search alternately fwd and bkwd from KBNEW in the boundary list B()
c for the point QV2.  When found set the associated triangle to TNEW.
               KFWD = KBNEW
               KBKWD= B(2, KFWD )
  140          continue
                  if (KFWD .ne. 0) then
                     if (B(3, KFWD) .eq. QV4) then
                        B(4, KFWD) = TNEW
                        go to 150
                     end if
                     KFWD = B(1, KFWD)
                  end if
C
                  if (KBKWD .ne. 0) then
                     if (B(3, KBKWD) .eq. QV4) then
                        B(4, KBKWD) = TNEW
                        go to 150
                     end if
                     KBKWD = B(2, KBKWD)
                  end if
               go to 140
            end if
  150       NABOR1 = 3
            MORE = MORE+1
            COUNT = COUNT+1
            OK = COUNT .le. 100
         else
            MORE = MORE-1
            if (MORE .gt. 0) then
               TOLD = T1
               I = SUB1(NABOR1)
               T1=TRI1(I)
               call STGGET(T1,TRI1,TRIANG)
               do 160 K = 1, 3
                  if (TRI1(K) .eq. TOLD) then
                     NABOR1 = SUB1(K)
                     go to 20
                  end if
  160          continue
            end if
         end if
         go to 20
      end if
      return
C
C1001 FORMAT(33H0ADJUST..  USING FIX-B WITH QV2 =,I5,5X,
C    * 12HB(3, KBNEW) =,I5)
C1002 FORMAT(33H0ADJUST..  USING FIX-B WITH QV4 =,I5)
      end
