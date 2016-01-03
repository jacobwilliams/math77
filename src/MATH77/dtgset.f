      SUBROUTINE DTGSET( J,T1,T2,T3,P1,P2,P3, TRIANG,MT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c  File: DTGSET.[F|FOR] CONTAINS DTGSET, DTGGET, DTGPUT, DTGSIZ.
C
C     THE FOUR SUBROUTINES DTGGET, DTGPUT, DTGSET, AND DTGSIZ MUST
C     BE MUTUALLY COMPATIBLE WITH REGARD TO THE METHOD OF PACKING
C     POINTERS IN TRIANG().
C
C     IN THIS VERSION ONLY ONE POINTER IS STORED PER WORD FOR MAXIMUM
C     PORTABILITY.
c
C>> 1996-02-02 DTGSET CLL
C>> 1995-01-11 DTGSET CLL Editing for inclusion into MATH77.
c     INPUT..  J AND THE INTEGERS T1,T2,T3,P1,P2,P3 DEFINING THE
c              JTH TRIANGLE.
c     THIS SUBR PUTS THIS INFO INTO THE ARRAY TRIANG().
C     C.L.LAWSON, JPL, 1979 JULY 22
c     ------------------------------------------------------------------
c--D replaces "?": ?TGSET, ?TGGET, ?TGPUT, ?TGSIZ
c     ------------------------------------------------------------------
      INTEGER  J,T1,T2,T3,P1,P2,P3, MT,TRIANG(MT)
c     ------------------------------------------------------------------
      TRIANG(6*J-5) = T1
      TRIANG(6*J-4) = T2
      TRIANG(6*J-3) = T3
      TRIANG(6*J-2) = P1
      TRIANG(6*J-1) = P2
      TRIANG(6*J) = P3
      RETURN
      END
c     ==================================================================
      SUBROUTINE DTGGET( J, TJ, TRIANG )
c     INPUT: J, TRIANG().
c     THIS SUBR FETCHES THE JTH TRIANGLE FROM TRIANG() AND STORES IT
c     IN TJ(1:6).
C     C.L.LAWSON, JPL, 1979 JULY 22
c     ------------------------------------------------------------------
      INTEGER  J, TJ(6), TRIANG(1:*)
c     ------------------------------------------------------------------
      TJ(1) = TRIANG(6*J-5)
      TJ(2) = TRIANG(6*J-4)
      TJ(3) = TRIANG(6*J-3)
      TJ(4) = TRIANG(6*J-2)
      TJ(5) = TRIANG(6*J-1)
      TJ(6) = TRIANG(6*J)
      RETURN
      END
c     ==================================================================
      SUBROUTINE DTGPUT( J, TJ, TRIANG, MT )
c     INPUT MT=DIMENSION OF TRIANG()
c     INPUT J AND TJ(1:6).    THIS SUBR PUTS THE CONTENTS OF
c     TJ(1:6) INTO THE ARRAY TRIANG() TO DEFINE THE JTH TRIANGLE.
C     C.L.LAWSON, JPL, 1979 JULY 22
c     ------------------------------------------------------------------
      INTEGER  J, TJ(1:6), MT, TRIANG(1:MT)
c     ------------------------------------------------------------------
      TRIANG(6*J-5) = TJ(1)
      TRIANG(6*J-4) = TJ(2)
      TRIANG(6*J-3) = TJ(3)
      TRIANG(6*J-2) = TJ(4)
      TRIANG(6*J-1) = TJ(5)
      TRIANG(6*J) = TJ(6)
      RETURN
      END
c     ==================================================================
      SUBROUTINE DTGSIZ( MT, NTLIM )
C     INPUT..     MT = DIMENSION OF ARRAY TRIANG()
C     OUTPUT..    NTLIM = MAX NUMBER OF TRIANGLES WHOSE DESCRIPTION
C                 POINTERS CAN BE STORED IN TRIANG().  THERE ARE SIX
C                 POINTERS PER TRIANGLE.
C     C.L.LAWSON, JPL, 1979 JULY 22
c     ------------------------------------------------------------------
      INTEGER  MT, NTLIM
c     ------------------------------------------------------------------
      NTLIM = MT/6
      RETURN
      END
