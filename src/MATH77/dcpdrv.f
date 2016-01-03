      SUBROUTINE DCPDRV (C,NC,D,ND)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 DCPDRV Krogh   Declared all vars.
C>> 1994-10-20 DCPDRV Krogh  Changes to use M77CON
C>> 1987-12-09 DCPDRV Lawson  Initial code.
c--D replaces "?": ?CPDRV
      INTEGER NC, ND, J, K
      DOUBLE PRECISION C(*),D(*),D1,D2,FAC,ZERO,HALF,TWO
      DATA ZERO,HALF,TWO/ 0.D0, .5D0, 2.D0 /
C
C     INTEGRATION OF FINITE CHEBYSHEV SERIES
C     C.L.LAWSON, JPL, 1973 JULY 19
C
C     (C(I),I=1,2)       SCALE FACTORS .
C     C(I+2),I=1,NC+1    COEFS OF CHEBY SERIES .
C     NC                 DEGREE OF CHEBY SERIES .
C     (D(I),I=1,2)       OUTPUT..  SCALE FACTORS .
C     (D(I+2),I=1,ND+1)  OUTPUT..  COEFS DEFINING DIFFERENTIATED
C                                  SERIES .
C     ND                 OUTPUT..  DEGREE OF SERIES
C
      IF (NC .LT. 0) THEN
        CALL IERM1('DCPDRV',1,0,'REQUIRE NC .GE. 0','NC',NC,'.')
      ELSE
        D(1)=C(1)
        D(2)=C(2)
        IF (NC .EQ. 0) THEN
          ND=0
          D(3)=ZERO
          RETURN
        END IF
        D2=ZERO
        ND=NC-1
        IF(ND .NE. 0) THEN
          D1=ZERO
          FAC=2*NC
          DO 20 J=1,ND
            D(ND+4-J)= FAC*C(ND+5-J) + D2
            D2=D1
            D1=D(ND+4-J)
   20       FAC=FAC-TWO
        END IF
        D(3)=C(4) + D2*HALF
        DO 40 K=1,NC
   40     D(K+2)=D(K+2)/D(2)
      END IF
      RETURN
      END
