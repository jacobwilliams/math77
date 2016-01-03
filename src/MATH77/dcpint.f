      SUBROUTINE DCPINT(A,NA,B,NB)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 DCPINT Krogh   Declared all vars.
C>> 1994-10-20 DCPINT Krogh  Changes to use M77CON
C>> 1987-12-09 DCPINT Lawson  Initial code.
c--D replaces "?": ?CPINT
      INTEGER NA, NB, J, NBP1, K
      DOUBLE PRECISION A(*),B(*),D1,D2,FAC,ZERO,HALF,TWO
      DATA ZERO,HALF,TWO/ 0.D0,.5D0, 2.D0 /
C
C     INTEGRATION OF FINITE CHEBYSHEV SERIES
C     C.L.LAWSON, JPL, 1973 JULY 18
C
C     (A(I),I=1,2)       SCALE FACTORS .
C     (A(I+2),I=1,NA+1)  COEFS OF CHEBY SERIES .
C     NA                 DEGREE OF CHEBY SERIES .
C     (B(I),I=1,2)       OUTPUT..  SCALE FACTORS .
C     (B(I+2),I=1,NB+1)  OUTPUT..  COEFS DEFINING INTEGRATED
C                                  SERIES .
C     NB                 OUTPUT..  DEGREE OF SERIES
C
      IF (NA .LT. 0) THEN
        CALL IERM1('DCPINT',1,0,'REQUIRE NA .GE. 0','NA',NA,'.')
      ELSE
        B(1)=A(1)
        B(2)=A(2)
        NB=NA+1
        D2=ZERO
        IF (NB .NE. 1) THEN
          D1=ZERO
          FAC=2*NB
          DO 50 J=1,NA
            B(NA+5-J) = (A(NA+4-J)-D2)/FAC
            D2=D1
            D1=A(NA+4-J)
   50       FAC=FAC-TWO
        END IF
        B(4)=A(3)-HALF*D2
        B(3)=ZERO
        NBP1=NB+1
        DO 70 K=1,NBP1
   70     B(K+2)=B(K+2)*B(2)
      END IF
      RETURN
      END
