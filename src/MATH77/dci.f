      DOUBLE PRECISION FUNCTION DCI (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 DCI Krogh  Moved external statement up for mangle.
c>> 1996-03-30 DCI Krogh  Added external statements.
C>> 1995-11-22 DCI Krogh  Removed multiple entry for C conversion.
C>> 1995-11-03 DCI Krogh  Removed blanks in numbers for C conversion.
c>> 1994-10-20 DCI Krogh  Changes to use M77CON
c>> 1990-01-23 DCI CLL Using name DCIN for result in entry DCIN.
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
C--D replaces "?": ?CI, ?CII, ?CIN, ?CPVAL, ?ERM1
C
      EXTERNAL DCII
      DOUBLE PRECISION X, DCII
      DCI = DCII(.true., X)
      end
C
      DOUBLE PRECISION FUNCTION DCIN (X)
      EXTERNAL DCII
      DOUBLE PRECISION X, DCII
      DCIN = DCII(.false., X)
      end
c
      DOUBLE PRECISION FUNCTION DCII (LDCI, X)
      LOGICAL LDCI
      INTEGER ERRLEV
      PARAMETER (ERRLEV=0)
      EXTERNAL DCPVAL
      DOUBLE PRECISION C(23), DCPVAL, F(13), G(13), GAMMA,X, Z,ZW
      parameter(GAMMA = 0.577215664901533D0)
      DATA C/
     1 + 0.5D0,                  + 0.5D0,
     2 +14.992589367813409D0, -19.386124096607770D0,
     3 +12.741870869758071D0, - 8.107903970562531D0,
     4 + 4.862022348500627D0, - 2.497505088539025D0,
     5 + 1.008660787358110D0, - 0.312080924825428D0,
     6 + 0.074678255294576D0, - 0.014110865253535D0,
     7 + 0.002152046752074D0, - 0.000270212331184D0,
     8 + 0.000028416945498D0, - 0.000002540125611D0,
     9 + 0.000000195437144D0, - 0.000000013084020D0,
     A + 0.000000000769379D0, - 0.000000000040066D0,
     B + 0.000000000001861D0, - 0.000000000000078D0,
     C + 0.000000000000003D0/
      DATA F/
     1 + 0.5D0,                  + 0.5D0,
     2 + 0.062263729028927D0, - 0.000233756041393D0,
     3 + 0.000002453755677D0, - 0.000000058670317D0,
     4 + 0.000000002356196D0, - 0.000000000136096D0,
     5 + 0.000000000010308D0, - 0.000000000000964D0,
     6 + 0.000000000000107D0, - 0.000000000000014D0,
     7 + 0.000000000000002D0/
      DATA G/
     1 + 0.5D0,                  + 0.5D0,
     2 + 0.003862856096703D0, - 0.000042644182622D0,
     3 + 0.000000724995950D0, - 0.000000023468225D0,
     4 + 0.000000001169202D0, - 0.000000000079604D0,
     5 + 0.000000000006875D0, - 0.000000000000717D0,
     6 + 0.000000000000087D0, - 0.000000000000012D0,
     7 + 0.000000000000002D0/
C
      IF (LDCI) then
         IF (X.LE.0.0) THEN
            CALL DERM1('DCI',1,ERRLEV,'Argument not positive','X',X,'.')
C        Provide a value in case the error message processor returns.
            DCII = 0.0
         ELSE IF (X.LT.16.0) THEN
            Z = X/16.0
            ZW = Z*Z
            DCII = LOG(X) - ZW*DCPVAL(C,20,ZW) + GAMMA
         ELSE
            Z = 16.0/X
            ZW = Z*Z
            DCII = Z*(DCPVAL(F,10,ZW)*SIN(X) - Z*DCPVAL(G,10,ZW)*COS(X))
         END IF
      ELSE
C
C     Evaluate the entire function
C     Cin(x) = Integral from 0 to x of ((cos(t) - 1) / t) dt.
C
         IF (ABS(X).LT.16.0) THEN
            Z = X/16.0
            ZW = Z*Z
            DCII = ZW*DCPVAL(C,20,ZW)
         ELSE
            Z = 16.0/X
            ZW = Z*Z
            DCII = LOG(ABS(X)) + GAMMA -
     1         Z*(DCPVAL(F,10,ZW)*SIN(X) - Z*DCPVAL(G,10,ZW)*COS(X))
         END IF
      END IF
      RETURN
      END
