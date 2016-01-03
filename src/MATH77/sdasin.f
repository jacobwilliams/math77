      SUBROUTINE SDASIN (X, XOUT, YOUT, YPOUT, NEQ, KOLD, PHI, PSI)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2001-11-23 sdasin Krogh  Changed many names per library conventions.
c>> 2001-11-04 sdasin Krogh  Fixes for F77 and conversion to single
c>> 2001-11-01 sdasin Hanson Provide code to Math a la Carte.
c--S replaces "?": ?dasin, ?daslx, ?dastp
c      IMPLICIT NONE
C***BEGIN PROLOGUE  SDASIN
C***SUBSIDIARY
C***PURPOSE  Interpolation routine for SDASLX.
C***LIBRARY   SLATEC (SDASLX)
C***TYPE      DOUBLE PRECISION (SDASIN-S, SDASIN-D)
C***AUTHOR  Petzold, Linda R., (LLNL)
C***DESCRIPTION
c ----------------------------------------------------------------------
C     THE METHODS IN SUBROUTINE SDASTP USE POLYNOMIALS
C     TO APPROXIMATE THE SOLUTION. SDASIN APPROXIMATES THE
C     SOLUTION AND ITS DERIVATIVE AT TIME XOUT BY EVALUATING
C     ONE OF THESE POLYNOMIALS, AND ITS DERIVATIVE,THERE.
C     INFORMATION DEFINING THIS POLYNOMIAL IS PASSED FROM
C     SDASTP, SO SDASIN CANNOT BE USED ALONE.
C
C     THE PARAMETERS ARE:
C     X     THE CURRENT TIME IN THE INTEGRATION.
C     XOUT  THE TIME AT WHICH THE SOLUTION IS DESIRED
C     YOUT  THE INTERPOLATED APPROXIMATION TO Y AT XOUT
C           (THIS IS OUTPUT)
C     YPOUT THE INTERPOLATED APPROXIMATION TO YPRIME AT XOUT
C           (THIS IS OUTPUT)
C     NEQ   NUMBER OF EQUATIONS
C     KOLD  ORDER USED ON LAST SUCCESSFUL STEP
C     PHI   ARRAY OF SCALED DIVIDED DIFFERENCES OF Y
C     PSI   ARRAY OF PAST STEPSIZE HISTORY
c ----------------------------------------------------------------------
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   830315  DATE WRITTEN
C   901009  Finished conversion to SLATEC 4.0 format (F.N.Fritsch)
C   901019  Merged changes made by C. Ulrich with SLATEC 4.0 format.
C   901026  Added explicit declarations for all variables and minor
C           cosmetic changes to prologue.  (FNF)
C***END PROLOGUE  SDASIN
C
      INTEGER  NEQ, KOLD
      REAL              X, XOUT, YOUT(*), YPOUT(*), PHI(NEQ,*), PSI(*)
C
      INTEGER  I, J, KOLDP1
      REAL              C, D, GAMMA, TEMP1
C
C***FIRST EXECUTABLE STATEMENT  SDASIN
      KOLDP1=KOLD+1
      TEMP1=XOUT-X
      DO 10 I=1,NEQ
         YOUT(I)=PHI(I,1)
10       YPOUT(I)=0.0E0
      C=1.0E0
      D=0.0E0
      GAMMA=TEMP1/PSI(1)
      DO 30 J=2,KOLDP1
         D=D*GAMMA+C/PSI(J-1)
         C=C*GAMMA
         GAMMA=(TEMP1+PSI(J-1))/PSI(J)
         DO 20 I=1,NEQ
            YOUT(I)=YOUT(I)+C*PHI(I,J)
20          YPOUT(I)=YPOUT(I)+D*PHI(I,J)
30       CONTINUE
      RETURN
C
c -----END OF SUBROUTINE SDASIN------
      END
