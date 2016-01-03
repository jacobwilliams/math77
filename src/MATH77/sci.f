      REAL             FUNCTION SCI (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 SCI Krogh  Moved external statement up for mangle.
c>> 1996-03-30 SCI Krogh  Added external statements.
C>> 1995-11-22 SCI Krogh  Removed multiple entry for C conversion.
C>> 1995-11-03 SCI Krogh  Removed blanks in numbers for C conversion.
c>> 1994-10-20 SCI Krogh  Changes to use M77CON
c>> 1990-01-23 SCI CLL Using name SCIN for result in entry SCIN.
c>> 1989-03-14 Original W. V. Snyder at JPL
C
C     COMPUTE THE COSINE INTEGRAL OF X =
C     INTEGRAL FROM X TO INFINITY OF -(COS(T)/T) DT =
C     GAMMA + LOG(ABS(X)) + INTEGRAL FROM 0 TO X OF ((COS(T)-1)/T DT),
C     WHERE GAMMA IS EULER'S CONSTANT.
C
C     FOR ABS(X)<16, USE A CHEBYSHEV SERIES WITH ARGUMENT 2*Z*Z-1 WHERE
C     Z=X/16 TO EVALUATE (CI(X)-LOG(ABS(X))-GAMMA)/(Z*Z), THEN MULTIPLY
C     THE RESULT BY Z*Z AND ADD LOG(ABS(X)) AND GAMMA.  LOSS OF ABOUT
C     TWO DIGITS OCCURS NEAR ABS(X)=16 DUE TO CANCELLATION.
C
C     FOR ABS(X).GE.16, USE CHEBYSHEV SERIES WITH ARGUMENT 2*Z*Z-1 WHERE
C     WHERE Z=16/X ARE USED TO COMPUTE F(X)/Z AND G(X)/(Z*Z).  THEN
C     CI(X)=F(X)*SIN(X)-G(X)*COS(X).
C
C     THIS ALGORITHM YIELDS AT MOST 15 DIGITS OF PRECISION.
C
C--S replaces "?": ?CI, ?CII, ?CIN, ?CPVAL, ?ERM1
C
      EXTERNAL SCII
      REAL             X, SCII
      SCI = SCII(.true., X)
      end
C
      REAL             FUNCTION SCIN (X)
      EXTERNAL SCII
      REAL             X, SCII
      SCIN = SCII(.false., X)
      end
c
      REAL             FUNCTION SCII (LDCI, X)
      LOGICAL LDCI
      INTEGER ERRLEV
      PARAMETER (ERRLEV=0)
      EXTERNAL SCPVAL
      REAL             C(23), SCPVAL, F(13), G(13), GAMMA,X, Z,ZW
      parameter(GAMMA = 0.577215664901533E0)
      DATA C/
     1 + 0.5E0,                  + 0.5E0,
     2 +14.992589367813409E0, -19.386124096607770E0,
     3 +12.741870869758071E0, - 8.107903970562531E0,
     4 + 4.862022348500627E0, - 2.497505088539025E0,
     5 + 1.008660787358110E0, - 0.312080924825428E0,
     6 + 0.074678255294576E0, - 0.014110865253535E0,
     7 + 0.002152046752074E0, - 0.000270212331184E0,
     8 + 0.000028416945498E0, - 0.000002540125611E0,
     9 + 0.000000195437144E0, - 0.000000013084020E0,
     A + 0.000000000769379E0, - 0.000000000040066E0,
     B + 0.000000000001861E0, - 0.000000000000078E0,
     C + 0.000000000000003E0/
      DATA F/
     1 + 0.5E0,                  + 0.5E0,
     2 + 0.062263729028927E0, - 0.000233756041393E0,
     3 + 0.000002453755677E0, - 0.000000058670317E0,
     4 + 0.000000002356196E0, - 0.000000000136096E0,
     5 + 0.000000000010308E0, - 0.000000000000964E0,
     6 + 0.000000000000107E0, - 0.000000000000014E0,
     7 + 0.000000000000002E0/
      DATA G/
     1 + 0.5E0,                  + 0.5E0,
     2 + 0.003862856096703E0, - 0.000042644182622E0,
     3 + 0.000000724995950E0, - 0.000000023468225E0,
     4 + 0.000000001169202E0, - 0.000000000079604E0,
     5 + 0.000000000006875E0, - 0.000000000000717E0,
     6 + 0.000000000000087E0, - 0.000000000000012E0,
     7 + 0.000000000000002E0/
C
      IF (LDCI) then
         IF (X.LE.0.0) THEN
            CALL SERM1('SCI',1,ERRLEV,'Argument not positive','X',X,'.')
C        Provide a value in case the error message processor returns.
            SCII = 0.0
         ELSE IF (X.LT.16.0) THEN
            Z = X/16.0
            ZW = Z*Z
            SCII = LOG(X) - ZW*SCPVAL(C,20,ZW) + GAMMA
         ELSE
            Z = 16.0/X
            ZW = Z*Z
            SCII = Z*(SCPVAL(F,10,ZW)*SIN(X) - Z*SCPVAL(G,10,ZW)*COS(X))
         END IF
      ELSE
C
C     Evaluate the entire function
C     Cin(x) = Integral from 0 to x of ((cos(t) - 1) / t) dt.
C
         IF (ABS(X).LT.16.0) THEN
            Z = X/16.0
            ZW = Z*Z
            SCII = ZW*SCPVAL(C,20,ZW)
         ELSE
            Z = 16.0/X
            ZW = Z*Z
            SCII = LOG(ABS(X)) + GAMMA -
     1         Z*(SCPVAL(F,10,ZW)*SIN(X) - Z*SCPVAL(G,10,ZW)*COS(X))
         END IF
      END IF
      RETURN
      END
