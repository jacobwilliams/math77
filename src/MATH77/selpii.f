      SUBROUTINE SELPII (PHI, K2, ALPHA2, P, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>>1996-05-02 SELPII Krogh  Fixed "max" for C conversion.
C>>1994-10-20 SELPII Krogh  Changes to use M77CON
C>>1991-10-10 SELPII WV Snyder JPL Original code.
C ----------------------------------------------------------------------
C        COMPUTES REAL ELLIPTIC INTEGRAL OF THE THIRD KIND
C ----------------------------------------------------------------------
      REAL             PHI, K2, ALPHA2, P
      INTEGER IERR
C        IERR = 0..3, AS FOR SRJVAL AND SRFVAL
C        IERR = 4 IF ABS(PHI) > PI/2.
C
C--S replaces "?": ?ELPII, ?ERM1, ?RFVAL, ?RJVAL
C --------------------
      REAL             A, B, C, PIHALF, R, RF, S, S2
      PARAMETER (PIHALF = 1.5707 96326 79489 66192 31322E0)
C ----------------------------------------------------------------------
      IF (ABS(PHI) .GT. PIHALF) THEN
         IERR = 4
         CALL SERM1 ('SELPII',4,0,'ABS(PHI) > PI/2','PHI',PHI,'.')
         RETURN
      end if
C
      S = SIN(PHI)
      C = COS(PHI)
      A = C*C
      S2 = S*S
      B = 1.0 - K2*S2
      S2 = ALPHA2*S2
      R = 1.0 - S2
      C = MAX(A,MAX(B,R))
      A = C*A
      B = C*B
      R = C*R
C
      CALL SRJVAL (A, B, C, R, P, IERR)
      IF (IERR .EQ. 0) then
         CALL SRFVAL (A, B, C, RF, IERR)
         P = SQRT(C) * S * ((S2 * C * P) / 3.0 + RF)
      end if
      RETURN
C
      END
