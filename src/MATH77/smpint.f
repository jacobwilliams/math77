      SUBROUTINE SMPINT (A,NA,B,NB)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-15 CLL Edited to use M77CON.
C>> 1994-11-11 SMPINT  Krogh   Declared all vars.
C>> 1987-12-09 SMPINT Lawson  Initial code.
C
C     INDEFINITE INTEGRATION OF A POLYNOMIAL REPRESENTED BY COEFFS
C     RELATIVE TO THE MONOMIAL BASIS INCLUDING A LINEAR TRANSFORMATION
C     OF THE ARGUMENT.
C     C.L.LAWSON, JP, 1973 DEC 6
C
C     (A(I),I=1,2)       SCALE FACTORS
C     (A(I+2),I=1,NA+1)  COEFS OF POLY
C     NA                 DEGREE OF POLY
C     (B(I),I=1,2)       OUTPUT..  SCALE FACTORS
C     (B(I+2),I=1,NB+1   OUTPUT..  COEFS OF INTEGRATED
C                                  POLY
C     NB                 OUTPUT..  DEGREE OF POLY
C
c     ------------------------------------------------------------------
c--S replaces "?": ?MPINT
c     ------------------------------------------------------------------
      INTEGER I, NA, NAP3, NB
      REAL             A(*), B(*), DIV
c     ------------------------------------------------------------------
      IF (NA .LT. 0) THEN
        CALL IERM1('SMPINT',1,0,'REQUIRE NA .GE. 0','NA',NA,'.')
      ELSE
        B(1)=A(1)
        B(2)=A(2)
        NAP3=NA+3
        NB=  NAP3-2
        B(3)=0.E0
        DIV=0.E0
        DO 30 I=3,NAP3
          DIV=DIV+1.E0
   30     B(I+1)=B(2)*A(I)/DIV
      END IF
      RETURN
      END
