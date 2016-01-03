      SUBROUTINE STGPD (X, Y, Z, DZ, NP, TRIANG, NT, IWORK)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c  File: STGPD.[f|for] contains STGPD, sTGMOR, sTGLS.
C>> 2007-02-28 STGPD Krogh Massive changed to remove assigned go to's.
c>> 2005-12-07 STGPD Krogh  Removed unused label.
c>> 1996-05-10 STGPD Krogh  MW removed from dim. declarator in C version
c>> 1996-03-30 STGPD Krogh  MIN0 => MIN
C>> 1996-02-02 STGPD CLL
C>> 1996-01-15 STGPD CLL
C>> 1996-01-11 STGPD CLL
C>> 1995-09-26 STGPD CLL Editing for inclusion into MATH77.
C
c  THIS SUBR ESTIMATES FIRST PARTIAL DERIVS AT THE GIVEN DATA POINTS.
c  C.L.LAWSON, JPL, 1976 DEC 21.  EDITED COMMENTS 1979 MAR 5.
C
c  ---------------------------------------------------------------------
C                     Method
c
c      THE PARTIAL DERIVS AT POINT  JP  WILL BE ESTIMATED BY FITTING
c  A QUADRATIC IN X AND Y TO A SET OF  MFIT+1  POINTS CONSISTING OF
c  POINT  JP  AND   MFIT NEARBY POINTS IN THE TRIANGULAR GRID.
c         THE VALUE OF MFIT WILL IN GENERAL BE DIFFERENT AT DIFFERENT
c  POINTS.  MFIT WILL BE IN THE RANGE FROM MFMIN TO MFMAX.  LET NNEB
c  DENOTE THE NO. OF IMMEDIATE NEIGHBORING POINTS TO POINT JB.  IF
c  MFMIN .LE. NNEB .LE. MFMAX  THEN MFIT=NNEB.
c      IF NNEB .LT. MFMIN THEN MFIT = MFMIN.
c      IF NNEB .GT. MFMAX THEN MFIT = MFMAX.
c  ---------------------------------------------------------------------
c                 Subroutine Arguments
c
c  X(), Y(), Z()     [in]
c  DZ(2,)            [out]
c  NP                [in]
c  TRIANG(), NT  [in]
c  (IWORK(I),I=1,NP) [scratch]  INITIALLY SET TO ZERO BY STGPD.
c       STGPD WILL SET IWORK(JP)= 1 WHEN IT ESTIMATES THE PARTIAL
c       DERIVATIVES AT POINT JP.
C
c  ---------------------------------------------------------------------
C                     COMMON BLOCK
c
c  /sTGCM1/ provides communication between STGPD and sTGMOR.
c  ---------------------------------------------------------------------
c                 Internal variables
C
c  AVAIL  POINTS TO AVAILABLE SPACE IN KE().
c  MFMAX  Dimensioning parameter used in the following declarations:
c           INTEGER   KE(MFMAX  ,4), JUSED(MFMAX)
c           REAL             W(MFMAX+5,6), DSQ(MFMAX)
c  MFMIN  LOWER BOUND FOR MFIT.  SET TO A CONSTANT VALUE BY STGPD.
c  MFIT   NO. OF NEIGHBORING POINTS USED WITH EACH POINT JP
C         TO ESTIMATE PARTIALS AT POINT JP.  GENERALLY DIFFERS
C         FOR EACH POINT JP.
c  FIRSTL,LASTL     POINTERS TO FIRST AND LAST ELEMENTS OF THE
c                   LIST CONTAINED IN KE().
c  KE(,) IS A DOUBLY  LINKED LIST DESCRIBING A CYCLE OF EDGES
c       SURROUNDING POINT JP.  AN EDGE IS DEFINED BY ITS TWO ADJACENT
c       TRIANGLES.
c           KE(L,1)   FWD POINTER  (COUNTERCLOCKWISE).
c           KE(L,2)   BKWD POINTER (CLOCKWISE).
c           KE(L,3)   TRIANGLE INSIDE EDGE L.
c           KE(L,4)   TRIANGLE OUTSIDE EDGE L,  OR ELSE ZERO IF EDGE
c                     ON THE BOUNDARY.
c  JUSED()   AN UNORDERED SET OF INDICES OF ROINTS USED IN THE LOCAL
c            FIT THAT ARE NOT IMMEDIATE NEIGHBORS OF POINT JP.
c  X0,Y0,Z0  COORDINATES OF POINT JP.
c  DSQ(L)    SQUARED DISTANCE FROM POINT JP TO THE MIDPOINT OF SIDE L
c  NEWPT     LOGICAL FLAG.  FOR A NEW POINT JP STGPD SETS
c            NEWPT TRUE.  THIS CAUSES sTGLS TO TRIANGULARIZE THE
c            LEAST SQUARES SYSTEM FOR JP. sTGLS THEN TESTS THE
c            CONDITION OF THE SYSTEM. IF IT IS BAD sTGLS SETS NEWPT
c            FALSE AND RETURNS. ESTPT THEN USES sTGMOR TO GET ONE
c            MORE POINT AND CALLS sTGLS AGAIN, LEAVING NEWPT
c            FALSE.  sTGLS THEN ACCUMULATES THE ADDITIONAL POINT
c            AND AGAIN TESTS THE CONDITION.  EVENTUALLY EITHER
c            ENOUGH POINTS ARE ADDED TO OBTAIN ADEQUATE CONDITION
c            OR ELSE sTGLS APPLIES AN ARBITRARY STABILIZATION AND
c            SO IN EITHER CASE sTGLS FINALLY RETURNS A SOLUTION.
C
c     ------------------------------------------------------------------
c--S replaces "?": ?TGPD, ?TGGET, ?TGMOR, ?TGLS, ?TGCM1
c--&               ?TGMOR, ?TGCM1, ?TGLS,  ?ROT,  ?ROTG
c     ------------------------------------------------------------------
      common  /sTGCM1/ X0, Y0, Z0, DSQ,
     *  AVAIL, MFMIN, MFIT, FIRSTL, LASTL, KE, JUSED, NEWPT
c Note -- if nfmax is changed, mw changes and C code must change
c everywhere mw is used in a dimension declarator.
      integer mfmax
      parameter(mfmax = 16)
      real             X0, Y0, Z0, DSQ(mfmax)
      integer AVAIL,MFMIN,MFIT, FIRSTL, LASTL, KE(mfmax,4), JUSED(mfmax)
      logical NEWPT
      integer mw
      parameter(mw = mfmax+5)
      integer NP
      integer ADD1(3), i, IWORK(np), ip1, ipcent, it, iv1, iv2, ivert
      integer j1, j2, jj1, jj2, jp
      integer kp, l, LIMIT
      integer lolim
      integer NT
      integer SUB1(3)
      integer TNOW, TRI(6), TRIANG(1:*), TRSAVE(6)
c++ Code for .C. is INACTIVE
C      real             dsfcn, DZ(2,NP), W(21,6)
c++ Code for ~.C. is ACTIVE
      real             dsfcn, DZ(2,NP), W(mw,6)
c++ END
      real             X(NP), x1, x2, Y(NP), y1, y2, Z(NP)
      logical HITBDY
C
      data ADD1(1),ADD1(2),ADD1(3),SUB1(1),SUB1(2),SUB1(3)/2,3,1, 3,1,2/
c     ------------------------------------------------------------------
      DSFCN(X1,Y1,X2,Y2) = ((X1+X2)*.5 - X0)**2 + ((Y1+Y2)*.5 - Y0)**2
c     ------------------------------------------------------------------
      MFMIN = 6
      LIMIT = min( MFMAX, NP - 1 )
      LOLIM = min( MFMIN, NP - 1 )
      NEWPT = .true.
C                        SET IWORK() = 0
      do 20 I = 1 , NP
        IWORK(I) = 0
 20   continue
C                                  MAIN LOOP THRU TRIANGLES
C
      IT = 1
      go to 40
 30   IT = IT + 1
 40   if (NT .lt. IT) return
      call STGGET(IT , TRSAVE, TRIANG)
C                                       LOOP THRU VERTICES OF A TRIANGLE
      IVERT = 1
      go to 60
 50   IVERT = IVERT + 1
 60   if (IVERT .gt. 3) go to 30
      JP= TRSAVE(IVERT + 3)
      if (IWORK(JP) .ne. 0) go to 50
      IWORK(JP) = 1
C1001 FORMAT(39H0ESTPD..  ESTIMATING PARTIALS AT POINT ,I5/1X)
C     THE FORTRAN END FOLLOWS..
C              WRITE(*,1001) JP
C           PARTIALS FOR POINT JP
      X0 = X(JP)
      Y0 = Y(JP)
      Z0 = Z(JP)
C                                  SET POINTERS FOR AVAILABLE SPACE.
      do 80 I = 2, MFMAX
        KE(I - 1,1) = I
 80   continue
      AVAIL = 1
      KE(MFMAX,1) = 0
C                                            BUILD FITTING EQUATIONS.
      go to 120
 90   if (MFIT .lt. LOLIM) then
        call sTGMOR ( X,Y,Z,NP, TRIANG,W)
      end if
C                                            SOLVE FITTING EQUATIONS.
C
      call sTGLS(W,MW,MFIT,NEWPT, LIMIT, DZ(1,JP),DZ(2,JP))
C                     GO FOR MORE POINTS IF NEEDED TO IMPROVE CONDITION.
 100  if (.not. NEWPT) then
        call sTGMOR (X,Y,Z,NP, TRIANG,W)
        call sTGLS(W,MW,MFIT,NEWPT, LIMIT, DZ(1,JP),DZ(2,JP))
        go to 100
      end if
C
      go to 50
C          PROCESS RING OF NEIGHBORS
 120  do 130 I = 1,6
        TRI(I) = TRSAVE(I)
 130  continue
C            POP AVAIL TO L
      L = AVAIL
      AVAIL = KE(AVAIL,1)
C                             BUILD FIRST EDGE IN RING OF NEIGHBORS.
      FIRSTL = L
      LASTL =L
      KE(L,1) = 0
      KE(L,2) = 0
      KE(L,3) = IT
      IV1=   ADD1(IVERT)
      KE(L,4)= TRI(IV1)
      J1 = TRI(IV1 + 3)
      IV2=   SUB1(IVERT)
      J2 = TRI(IV2 + 3)
      DSQ(L) = DSFCN(X(J1),Y(J1),X(J2),Y(J2))
C
C                            BUILD EQUAS FOR FIRST TWO NEIGHBORING PTS.
      MFIT = 1
      KP = J1
C                   BUILD ROW
C1000 FORMAT(25H BUILD ROW..  CENTER PT =,I5,12H,  EQUA NO =,I3,
C    *13H  NEARBY PT =,I5)
C        WRITE(*,1000) JP,MFIT,KP
      JUSED(MFIT) = KP
      W(MFIT,4) = X(KP) - X0
      W(MFIT,5) = Y(KP) - Y0
      W(MFIT,6) = Z(KP) - Z0
      W(MFIT,1) = W(MFIT,4)**2
      W(MFIT,2) = W(MFIT,4) * W(MFIT,5)
      W(MFIT,3) = W(MFIT,5)**2
      MFIT = 2
      KP = J2
C                   BUILD ROW
C1000 FORMAT(25H BUILD ROW..  CENTER PT =,I5,12H,  EQUA NO =,I3,
C    *13H  NEARBY PT =,I5)
C        WRITE(*,1000) JP,MFIT,KP
      JUSED(MFIT) = KP
      W(MFIT,4) = X(KP) - X0
      W(MFIT,5) = Y(KP) - Y0
      W(MFIT,6) = Z(KP) - Z0
      W(MFIT,1) = W(MFIT,4)**2
      W(MFIT,2) = W(MFIT,4) * W(MFIT,5)
      W(MFIT,3) = W(MFIT,5)**2
      go to  180
C
C                             MOVE LEFT AND THEN RIGHT AROUND POINT JP
C                             TO BUILD EQUAS FOR IMMEDIATELY
C                             NEIGHBORING POINTS.
C
 160  if (.not.  HITBDY) go to 90
      do 170 I = 1,6
        TRI(I) = TRSAVE(I)
 170  continue
      go to 360
C                    MOVE LEFT
C                                     LEFT MEANS COUNTERCLOCKWISE.
 180  JJ2 =  J2
C                                             *
C      ****************************************
      IPCENT = IVERT
      HITBDY = .false.
      if (MFIT .ge. MFMAX) go to 160
      I     = SUB1(IPCENT)
      TNOW = TRI(I)
      if (TNOW .ne. 0) go to 200
C                                            HAVE HIT BOUNDARY
      HITBDY  = .true.
      go to 160
 200  call STGGET(TNOW,TRI, TRIANG)
      do 210 IPCENT = 1,3
        if (TRI(IPCENT + 3).eq.JP) go to 220
 210  continue
C
C                                       IDENTIFY NEXT POINT
 220  I = SUB1(IPCENT)
      KP= TRI(I + 3)
C
C                   RECORD INFORMATION ABOUT  THE NEW EDGE IN
C                   THE LIST STRUCTURE.
C
C            POP AVAIL TO L
      L = AVAIL
      AVAIL = KE(AVAIL,1)
      KE(L,2)= LASTL
      KE(L,3)= TNOW
      I      = ADD1(IPCENT)
      KE(L,4)= TRI(I)
      KE(LASTL,1) = L
      LASTL  = L
      JJ1 = JJ2
      JJ2 = KP
      DSQ(L) = DSFCN(X(JJ1),Y(JJ1),X(JJ2),Y(JJ2) )
      if (KP .eq. J1) go to 240
C                                       KP IS A NEW NEIGHBORING POINT.
      MFIT = MFIT +1
C                   BUILD ROW
C1000 FORMAT(25H BUILD ROW..  CENTER PT =,I5,12H,  EQUA NO =,I3,
C    *13H  NEARBY PT =,I5)
C        WRITE(*,1000) JP,MFIT,KP
      JUSED(MFIT) = KP
      W(MFIT,4) = X(KP) - X0
      W(MFIT,5) = Y(KP) - Y0
      W(MFIT,6) = Z(KP) - Z0
      W(MFIT,1) = W(MFIT,4)**2
      W(MFIT,2) = W(MFIT,4) * W(MFIT,5)
      W(MFIT,3) = W(MFIT,5)**2
      KE(L,1) = 0
      go to 160
C
C                   KP IS NOT A NEW POINT.  WE HAVE CYCLED ALL THE WAY
C                   AROUND TO THE INITIAL POINT J1.  THIS MEANS THE
C                   ENTIRE RING OF NEIGHBORS HAS BEEN FOUND.
C
 240  KE(L,1) = FIRSTL
      KE(FIRSTL,2) = LASTL
C
C         ABOVE LOOP IS EXITED FOR ONE OF THREE REASONS..
C              (1) MFIT .EQ. MFMAX,
C              (2) HITBDY = .TRUE.,
C           OR (3) COMPLETE RING OF NEIGHBORS HAS BEEN BUILT
C
      go to 160
C
C     **************************
C                              *
C     PROCEDURE ( MOVE RIGHT )
C                              *
C      *************************
C                                      RIGHT MEANS CLOCKWISE.
 360  JJ1 = J1
      IPCENT = IVERT
 370  if (MFIT .ge. MFMAX) go to 90
      TNOW = TRI(IPCENT)
C
C              THE FOLLOWING TEST CAUSES AN EXIT FROM THE LOOP IF THE
C              BOUNDARY ON THE RIGHT HAS BEEN ENCOUNTERED.
C
      if (TNOW .eq. 0) go to 90
      call STGGET(TNOW,TRI, TRIANG)
      do 380 IPCENT = 1,3
        if (TRI(IPCENT + 3) .eq. JP) go to 390
 380  continue
C                                       IDENTIFY NEXT POINT
 390  IP1 = ADD1(IPCENT)
      KP = TRI(IP1 + 3)
C
C                   RECORD INFO ABOUT THE NEW EDGE IN THE LIST STRUCTURE
C
C            POP AVAIL TO L
      L = AVAIL
      AVAIL = KE(AVAIL,1)
      KE(L,1) = FIRSTL
      KE(L,2) = 0
      KE(L,3) = TNOW
      KE(L,4) = TRI(IP1)
      KE(FIRSTL,2) = L
      FIRSTL  = L
      JJ2 = JJ1
      JJ1 = KP
      DSQ(L) = DSFCN(X(JJ1),Y(JJ1),X(JJ2),Y(JJ2) )
C
C                                       KP IS A NEW NEIGHBORING POINT.
      MFIT = MFIT + 1
C                   BUILD ROW
C1000 FORMAT(25H BUILD ROW..  CENTER PT =,I5,12H,  EQUA NO =,I3,
C    *13H  NEARBY PT =,I5)
C        WRITE(*,1000) JP,MFIT,KP
      JUSED(MFIT) = KP
      W(MFIT,4) = X(KP) - X0
      W(MFIT,5) = Y(KP) - Y0
      W(MFIT,6) = Z(KP) - Z0
      W(MFIT,1) = W(MFIT,4)**2
      W(MFIT,2) = W(MFIT,4) * W(MFIT,5)
      W(MFIT,3) = W(MFIT,5)**2
      go to 370
C
C                   ABOVE LOOP IS EXITED WHEN
C                      (1) MFIT .EQ. MFMAX
C                   OR (2) HIT BOUNDARY ON RIGHT
C
      END
c     ==================================================================
      SUBROUTINE sTGMOR(X,Y,Z,NP,TRIANG,W)
C>> 1995-09-26 CLL Editing for inclusion into MATH77.
C
C     C.L.LAWSON, JPL, 1977 JAN 4
C         CHANGED NOV 30, 1977    TT
C         THIS SUBR IS CALLED BY STGPD AS FOLLOWS:
C      (1) IF NEWPT = .TRUE.,
C            THE SUBR EXTENDS THE RING OF NEARBY POINTS AROUND
C          POINT (X0,Y0) UP TO A TOTAL OF MIN(MFMIN,NP-1) NEARBY POINTS
C          TO BE USED IN COMPUTING A LOCAL FIT TO THE SURFACE NEAR POINT
C          (X0,Y0).
C      (2) IF NEWPT = .FALSE.,
C            THE SUBR GETS ONLY ONE MORE NEARBY POINT AND
C          RETURNS.
c     ------------------------------------------------------------------
      common  /sTGCM1/ X0, Y0, Z0, DSQ,
     *  AVAIL, MFMIN, MFIT, FIRSTL, LASTL, KE, JUSED, NEWPT
      integer mfmax
      parameter(mfmax = 16)
      real             X0, Y0, Z0, DSQ(mfmax)
      integer AVAIL,MFMIN,MFIT, FIRSTL, LASTL, KE(mfmax,4), JUSED(mfmax)
      logical NEWPT
C
      logical TSTLP1,TSTLP2,TSTLM1,TSTLM2,AVOK, TEST, USED
      integer ADD1(3), i, im1, ip1, k, kp
      integer l, limit2, lm1, lm2, lmin, lnew
      integer lp1, lp2, lp3
      integer NP
      integer SUB1(3),TNOW, TRI(6),TRIANG(1:*),V1,V2
c++ Code for .C. is INACTIVE
C      real             dmin, dsfcn, W(21,6)
c++ Code for ~.C. is ACTIVE
      integer mw
      parameter(mw = mfmax+5)
      real             dmin, dsfcn, W(mw,6)
c++ END
      real             X(NP), x1, x2, Y(NP), y1, y2, Z(NP)
C
      data ADD1(1),ADD1(2),ADD1(3),SUB1(1),SUB1(2),SUB1(3)/2,3,1, 3,1,2/
C
C     NAMELIST/DEBUG/AVAIL,MFMIN,MFIT,FIRSTL,LASTL, JUSED,
C    *X0,Y0,Z0,DSQ
c     ------------------------------------------------------------------
      DSFCN(X1,Y1,X2,Y2) = ((X1+X2)*.5 - X0)**2 + ((Y1+Y2)*.5 - Y0)**2
c     ------------------------------------------------------------------
C
      if (NEWPT) then
        LIMIT2 = MIN( MFMIN, NP - 1 )
      else
        LIMIT2 = MFIT + 1
C                        WHEN sTGMOR IS CALLED WITH
C                        NEWPT = .FALSE. MFIT MUST BE LESS THAN MFMIN
C                        AND NP-1 SO THAT LIMIT2 CAN SAFELY BE SET
C                        TO MFIT+1 AS ABOVE.
      end if
      AVOK = .true.
      go to 200

C
C     *******************************************
C                                               *
C     PROCEDURE ( DELETE EDGES L,LP1, AND LP2 )
C                                               *
C      ******************************************
C
 10   LM1 = KE(L,2)
      LP3 = KE(LP2,1)
      if (LM1 .ne. 0) KE(LM1,1) = LP3
      if (LP3 .ne. 0) KE(LP3,2) = LM1
      if ( L .eq. FIRSTL) then
        FIRSTL = LP3
      else if ((LP1 .eq. FIRSTL)  .or.  (LP2 .eq. FIRSTL)) then
        FIRSTL = LP3
        LASTL  = LM1
      else if (LP2 .eq. LASTL) then
        LASTL = LM1
      end if
C                   TRANSFER L, LP1, AND LP2 TO AVAILABLE SPACE LIST
C
      KE(LP2,1) = AVAIL
      AVAIL = L
 20   FORMAT(60X,'MORPTS..  DELETING THREE EDGES.',
     *  '  TNOW=', I5)
      WRITE(*,20) TNOW
      go to 200
C                                                                 *
C     DELETE EDGE L.  ADD TWO NEW EDGES. /KP,AVOK/
 50   continue
C             POP AVAIL TO LNEW. SET AVOK
C                                              *
C      *****************************************
      if ( AVAIL .eq. 0) then
        AVOK = .false.
C                                            ERROR CONDITION
C
 60     FORMAT(/' SUBR sTGMOR..  WARNING..  DIMENSION OF KE(,) NOT',
     *    ' LARGE ENOUGH TO FIND MFMIN NEARBY POINTS FOR ESTIMATING',
     *    ' PARTIAL DERIVS.'/
     *    ' PROGRAM WILL CONTINUE USING FEWER NEARBY POINTS.')
        WRITE(*,60)
      else
        LNEW = AVAIL
        AVAIL = KE(AVAIL,1)
      end if

      if (AVOK) then
        KE(L,1) = LNEW
        KE(LNEW,1) = LP1
        KE(LNEW,2) = L
        KE(LNEW,3) = TNOW
        if (LP1 .ne. 0) KE(LP1,2) = LNEW
        if (L .eq. LASTL) LASTL = LNEW
        call STGGET( TNOW,TRI, TRIANG)
        do 80 I = 1,3
          if (TRI(I) .eq. KE(L,3)) go to 90
 80     continue
 90     IP1 = ADD1(I)
        KE(L,4) = TRI(IP1)
        KE(L,3) = TNOW
        IM1 = SUB1(I)
        KE(LNEW,4) = TRI(IM1)
        KP         = TRI(IM1 + 3)
        V1 = TRI(IP1 + 3)
        DSQ(L)    = DSFCN( X(V1),Y(V1), X(KP),Y(KP) )
        V1 = TRI(I + 3)
        DSQ(LNEW) = DSFCN( X(V1),Y(V1), X(KP),Y(KP) )
      end if
C            PROCESS NEW VERTEX
C
C         HERE THE TRIANGLE TNOW HAS ONLY EDGE L IN COMMON WITH THE
C         CURRENT RING  AROUND POINT JP.  THE VERTEX OPPOSITE SIDE L
C         IN TRIANGLE TNOW IS KP.
C            TEST TO SEE IF POINT KP HAS ALREADY BEEN USED TO
C         BUILD AN EQUATION.
C
      USED = .false.
      do 160 K = 1,MFIT
        if (JUSED(K) .eq. KP) then
          USED = .true.
          go to 170
        end if
 160  continue
C
 170  if (USED) go to 200
      MFIT = MFIT + 1
C               BUILD ROW
C1000    FORMAT(' BUILD ROW IN sTGMOR..   ',5X,''   EQUA NO =',I3,
C    *   '  NEARBY PT =',I5)
C        WRITE(*,1000)    MFIT,KP
      JUSED(MFIT) = KP
      W(MFIT,4) = X(KP) - X0
      W(MFIT,5) = Y(KP) - Y0
      W(MFIT,6) = Z(KP) - Z0
      W(MFIT,1) = W(MFIT,4)**2
      W(MFIT,2) = W(MFIT,4) * W(MFIT,5)
      W(MFIT,3) = W(MFIT,5)**2


c
 200  if ((MFIT .ge. LIMIT2) .or. (.not. AVOK)) return
C        WRITE(*,DEBUG)
C1004    FORMAT(/' KE(,)='/(7X,5I5))
C        WRITE(*,1004) (I,(KE(I,J), J=1,4), I=1,16)
C
C                   FIND EDGE WHOSE MIDPOINT IS CLOSEST TO POINT JP.
C                   SKIP BOUNDARY EDGES.
      L = FIRSTL
      LMIN = 0
 220  if (KE(L,4) .ne. 0) then
        if (LMIN .eq. 0) then
          TEST = .true.
        else
          TEST = DSQ(L) .lt. DMIN
        end if
        if (TEST) then
          DMIN = DSQ(L)
          LMIN = L
        end if
      end if
      if (L .eq. LASTL) go to 230
      L = KE(L,1)
      go to 220
 230  L = LMIN
C
C         EDGE L IS THE NEAREST TO POINT JP.
C         INVESTIGATE TRIANGLE ON OPPOSIDE SIDE OF EDGE L.
C
      TNOW = KE(L,4)
      LP1 = KE(L,1)
      TSTLP1 = LP1 .ne. 0
      if (TSTLP1) TSTLP1 = KE(LP1,4) .eq. TNOW
      LM1 = KE(L,2)
      TSTLM1 = LM1 .ne. 0
      if (TSTLM1) TSTLM1 = KE(LM1,4) .eq. TNOW
      if (.not. TSTLP1) go to 270
      LP2 = KE(LP1,1)
      TSTLP2 = LP2 .ne. 0
      if (TSTLP2) TSTLP2 = KE(LP2,4) .eq. TNOW
      if (.not. TSTLP2) go to 250
      go to 10
 250  if (.not. TSTLM1) go to 300
      LP2 = LP1
      LP1 = L
      L  =LM1
      go to 10
 270  if (.not. TSTLM1) go to 50
      LM2 = KE(LM1,2)
      TSTLM2 = LM2 .ne. 0
      if (TSTLM2) TSTLM2 = KE(LM2,4) .eq. TNOW
      if (.not. TSTLM2) go to 280
      LP2 = L
      LP1 = LM1
      L   = LM2
      go to 10
 280  LP1 = L
      L  =LM1
C                        END OF TOP LEVEL CODE.  PROCEDURES FOLLOW..
C
C     ****************************************************
C                                                        *
C     PROCEDURE ( DELETE EDGES L AND LP1.  ADD NEW EDGE)
C                                                        *
C      ***************************************************
C
 300  LP2 = KE(LP1,1)
      KE(L,1) = LP2
      if (LP2 .ne. 0) KE(LP2,2) = L
C
      if (LP1 .eq.  FIRSTL) then
        FIRSTL = LP2
      else if (LP1 .eq. LASTL) then
        LASTL = L
      end if
C
      KE(LP1,1) = AVAIL
      AVAIL = LP1
      call STGGET( TNOW,TRI, TRIANG)
C
      do 310 I = 1,3
        if (TRI(I) .eq. KE(L,3)) go to 320
 310  continue
 320  KE(L,3) = TNOW
      IP1 = ADD1(I)
      KE(L,4) = TRI(IP1)
      if (KE(L,4) .ne. 0) then
        IM1 = SUB1(I)
        V1  = TRI(IP1 + 3)
        V2  = TRI(IM1 + 3)
        DSQ(L) = DSFCN(X(V1),Y(V1),  X(V2),Y(V2) )
      end if
C1003    FORMAT(60X,'MORPTS..  DELETING TWO EDGES, ADDING ONE.',
C    *   ' TNOW=', I5)
C        WRITE(*,1003) TNOW
      go to 200
C
      END
c     ==================================================================
      SUBROUTINE sTGLS (W,MW,MFIT,NEWPT, LIMIT, DZ1,DZ2)
C>> 1995-09-26 CLL Editing for inclusion into MATH77.
C
C     WEIGHT, SCALE, STABILIZE, AND SOLVE A LEAST SQRS PROBLEM.
C
C     C.L.LAWSON, JPL, 1976 FEB 27
C         CHANGED NOV 28, 1977    TT
C     EDITED CODE. NO CHANGE IN ALGORITHM. CLL. 1979 MAR 5.
C
C     ON INPUT, WITH NEWPT TRUE, COLUMNS OF W(,) CONTAIN TRANSLATED
C     VALUES OF X**2, X*Y, Y**2, X, Y, Z.
C
C          THE NUMBER, STAB, IS USED AS A MARQUARDT STABILIZATION
C     VALUE TO DAMP DOWN THE VALUES OF THE THREE SECOND PARTIAL
C     DERIVATIVES WHEN THE AVAILABLE DATA DOES NOT PRODUCE A
C     REASONABLY WELL-CONDITIONED LEAST SQUARES PROBLEM.
C     A LARGER VALUE OF STAB DAMPS THE SECOND PARTIALS MORE.
C
C     THE OUTPUT IS DZ1 AND DZ2.
c     ------------------------------------------------------------------
      real             AVE,WT,WT1,WT2
      save             AVE,WT,WT1,WT2
      integer limit
C
      integer i, IROW, J, JJ, KP, LEN, MFIT, MW
      real             AVESQ, C1, C2, COSINE, DMIN, DZ1, DZ2
c++ Code for .C. is INACTIVE
C      real             SINE, STAB, SUM, W(21,6)
c++ Code for ~.C. is ACTIVE
      real             SINE, STAB, SUM, W(MW,6)
c++ END
      logical   NEWPT
c     ------------------------------------------------------------------
      STAB = 1.E0
      if (.not. NEWPT) go to 200
      SUM = 0.0e0
      KP = MFIT
      do 100 I = 1,KP
        SUM = SUM + W(I,1) + W(I,3)
 100  continue
c                          KP  will be converted to floating point.
      AVESQ = SUM/KP
      AVE   = SQRT(AVESQ)
C                          WE HAVE EXPERIMENTED WITH A VARIETY OF
C                          WEIGHTING FORMULAS AND FOUND NONE SIG-
C                          NIFICANTLY BETTER THAN THE UNIFORM
C                          WEIGHTING..  WT = 1.
      WT = 1.0e0
      WT1 = WT / AVE
      WT2 = WT / AVESQ
      IROW = 1
      go to 130
C                  SCALE ROW IROW
 120  W(IROW,1) = W(IROW,1) * WT2
      W(IROW,2) = W(IROW,2) * WT2
      W(IROW,3) = W(IROW,3) * WT2
      W(IROW,4) = W(IROW,4) * WT1
      W(IROW,5) = W(IROW,5) * WT1
      W(IROW,6) = W(IROW,6) * WT
      IROW = IROW + 1
 130  if (KP .ge. IROW) go to 120
C           GIVENS TRIANGULARIZATION
      do 160 J = 1,6
        if (J + 1 .LE. KP) then
          LEN = 6 - J
          do 150 I = J + 1,KP
            if (W(I,J) .ne. 0.0e0) then
              call SROTG (W(J,J),W(I,J),COSINE,SINE)
              W(I,J) = 0.0e0
              if (LEN .gt. 0)
     *          call SROT(LEN,W(J,J+1),MW,W(I,J+1),MW,COSINE,SINE)
            end if
 150      continue
        end if
 160  continue
      go to 230
C
 200  IROW = MFIT
C                  SCALE NEW ROW
      W(IROW,1) = W(IROW,1) * WT2
      W(IROW,2) = W(IROW,2) * WT2
      W(IROW,3) = W(IROW,3) * WT2
      W(IROW,4) = W(IROW,4) * WT1
      W(IROW,5) = W(IROW,5) * WT1
      W(IROW,6) = W(IROW,6) * WT
C
C             THE NEW ROW WILL BE ACCUMULATED INTO THE TRIANGLE
C             USING GIVENS ROTATIONS.
      do 220 J = 1,6
        LEN = 6 - J
        if (W(IROW,J) .ne. 0.0e0) then
          call SROTG (W(J,J), W(IROW,J), COSINE,SINE)
          W(IROW,J) = 0.0e0
          if (LEN .gt. 0)
     *      call SROT (LEN,W(J,J+1),MW,W(IROW,J+1),MW,COSINE,SINE)
        end if
 220  continue
c
 230  if (KP .ge. 5) then
        DMIN = min( min( min(abs(W(1,1)), abs(W(2,2))),
     *    min(abs(W(3,3)), abs(W(4,4)))),
     *    abs(W(5,5)) )
      else
        DMIN = 0.0e0
      end if
C               TEST CONDITION OF SYSTEM.
      if (DMIN .ge. 0.01e0) go to 300
C
C               SYSTEM IS ILL-CONDITIONED.
C
      if (MFIT .lt. LIMIT) then
C
C               RETURN TO ADD ONE MORE POINT TO THE FITTING PROBLEM
C               IN HOPES THAT THIS WILL IMPROVE THE CONDITION.
C
        NEWPT = .false.
        return
      end if
C
C               CANNOT ADD ANY MORE POINTS.  STABILIZE BY DAMPING
C               THE SECOND PARTIAL DERIVATIVES.
C
      JJ = 1
      go to 280
c
 250  continue
C             THE NEW ROW WILL BE ACCUMULATED INTO THE TRIANGLE
C             USING GIVENS ROTATIONS.
      do 260 J = 1,6
        LEN = 6 - J
        if (W(IROW,J) .ne. 0.0e0) then
          call SROTG (W(J,J), W(IROW,J), COSINE,SINE)
          W(IROW,J) = 0.0e0
          if (LEN .gt. 0)
     *      call SROT (LEN,W(J,J+1),MW,W(IROW,J+1),MW,COSINE,SINE)
        end if
 260  continue

      JJ = JJ + 1
 280  if (JJ .gt. 3) go to 300
      IROW = MFIT + JJ
      do 290 J = 1,6
        W(IROW,J) = 0.0e0
 290  continue
      W(IROW,JJ) = STAB
      go to 250
C
C                 SOLVE FOR DZ1 AND DZ2
C
 300  C2 = W(5,6) / W(5,5)
      C1 = (W(4,6) - C2 * W(4,5)) / W(4,4)
      DZ1 = C1 / AVE
      DZ2 = C2 / AVE
      NEWPT = .true.
C
      return
      end
