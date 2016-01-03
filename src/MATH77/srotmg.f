      subroutine SROTMG (D1,D2,X1,Y1,PARAM)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2006-06-07 SROTMG Krogh  Removed arithmetic ifs
C>> 1999-12-30 SROTMG Krogh  Big reorg., no more assigned goto's.
C>> 1999-12-22 SROTMG Krogh  Declared IGO.
C>> 1994-10-20 SROTMG Krogh  Changes to use M77CON
C>> 1994-04-19 SROTMG Krogh   Converted to use generic intrinsics.
C>> 1985-08-02 SROTMG Lawson  Initial code.
c--S replaces "?": ?ROTMG
C
C     CONSTRUCT THE MODIFIED GIVENS TRANSFORMATION MATRIX H WHICH ZEROS
C     THE SECOND COMPONENT OF THE 2-VECTOR  (SQRT(D1)*X1,SQRT(D2)*
C     Y1)**T.
C     WITH PARAM(1)=PFLAG, H HAS ONE OF THE FOLLOWING FORMS..
C
C     PFLAG=-1.E0     PFLAG=0.E0        PFLAG=1.E0     PFLAG=-2.E0
C
C       (H11  H12)    (1.E0  H12)    (H11  1.E0)    (1.E0  0.E0)
C     H=(        )    (         )    (         )    (          )
C       (H21  H22),   (H21  1.E0),   (-1.E0 H22),   (0.E0  1.E0).
C     LOCATIONS 2-5 OF PARAM CONTAIN H11, H21, H12, AND H22
C     RESPECTIVELY. (VALUES OF 1.E0, -1.E0, OR 0.E0 IMPLIED BY THE
C     VALUE OF PARAM(1) ARE NOT STORED IN PARAM.)
C
C     THE VALUES OF GAMSQ AND RGAMSQ SET IN THE DATA STATEMENT MAY BE
C     INEXACT.  THIS IS OK AS THEY ARE ONLY USED FOR TESTING THE SIZE
C     OF D1 AND D2.  ALL ACTUAL SCALING OF DATA IS DONE USING GAM.
C
      real             GAM, RGAMSQ, D2, H11, H21, PARAM(5), P2,
     1     Q2, U, Y1, GAMSQ, D1, PFLAG, H12, H22, P1, Q1,
     2     TEMP, X1
C
      data GAM, GAMSQ, RGAMSQ / 4096.E0, 16777216.E0, 5.9604645E-8 /

      if ( D1 .lt. 0.E0) go to 210
      P2 = D2 * Y1
      if (P2 .eq. 0.E0) then
         PFLAG = -2.E0
         go to 260
      end if
C     Regular case
      P1 = D1 * X1
      Q2 = P2 * Y1
      Q1 = P1 * X1
      if (ABS(Q1) .gt. ABS(Q2)) then
         H21 = -Y1 / X1
         H12 = P2 / P1
         U = 1.E0 - H12 * H21
         if (U .le. 0.E0) go to 210
         PFLAG = 0.E0
         D1 = D1 / U
         D2 = D2 / U
         X1 = X1 * U
      else
         if (Q2 .lt. 0.E0) go to 210
         PFLAG = 1.E0
         H11 = P1 / P2
         H22 = X1 / Y1
         U = 1.E0 + H11 * H22
         TEMP = D2 / U
         D2 = D1 / U
         D1 = TEMP
         X1 = Y1 * U
      end if

C     Scale Check
 100  if (D1 .le. D2) then
         if (D1 .le. RGAMSQ) then
            if (D1 .ne. 0.E0) then
               if (PFLAG .ge. 0.E0) go to 150
               D1 = D1 * GAM**2
               X1 = X1 / GAM
               H11 = H11 / GAM
               H12 = H12 / GAM
               go to 100
            end if
         end if
         if (ABS(D2) .lt. GAMSQ) go to 220
         if (PFLAG .ge. 0.E0) go to 150
         D2 = D2 / GAM**2
         H21 = H21 * GAM
         H22 = H22 * GAM
         go to 100
      else
         if (ABS(D2) .le. RGAMSQ) then
            if (PFLAG .ge. 0.E0) go to 150
            D2 = D2 * GAM**2
            H21 = H21 / GAM
            H22 = H22 / GAM
            go to 100
         end if
         if (D1 .lt. GAMSQ) go to 220
         if (PFLAG .ge. 0.E0) go to 150
         D1 = D1 / GAM**2
         X1 = X1 * GAM
         H11 = H11 * GAM
         H12 = H12 * GAM
         go to 100
      end if

C     Fix H
 150  if (PFLAG .eq. 0.E0) then
         H11 = 1.E0
         H22 = 1.E0
      else
         H21 = -1.E0
         H12 = 1.E0
      end if
      PFLAG = -1.E0
      go to 100

C     Zero H, D, and X1
 210  PFLAG = -1.E0
      H11 = 0.E0
      H12 = 0.E0
      H21 = 0.E0
      H22 = 0.E0
C
      D1 = 0.E0
      D2 = 0.E0
      X1 = 0.E0

C     Return
 220  continue
      if (PFLAG .eq. 0.E0) then
        PARAM(3) = H21
        PARAM(4) = H12
      else if (PFLAG .gt. 0.E0) then

        PARAM(2) = H11
        PARAM(5) = H22
      else
        PARAM(2) = H11
        PARAM(3) = H21
        PARAM(4) = H12
        PARAM(5) = H22
      end if
 260  PARAM(1) = PFLAG
      return
      end
