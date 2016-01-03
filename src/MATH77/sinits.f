      subroutine SINITS (DOS, NOS, ETA, NTERMS)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1995-11-22 SINITS Krogh  Simplified the DO Loop.
C>> 1994-10-20 SINITS Krogh  Changes to use M77CON
C>> 1990-11-28 SINITS CLL Changed name and usage from fcn to subr.
C>> 1990-01-23 CLL  Corrected calls to IERM1 and SERM1.
C>> 1985-08-02 INITDS Lawson  Initial code.
C
C INITIALIZE THE DOUBLE PRECISION ORTHOGONAL SERIES DOS SO THAT NTERMS
C IS THE NUMBER OF TERMS NEEDED TO INSURE THE ERROR IS NO LARGER THAN
C ETA.  ORDINARILY ETA WILL BE CHOSEN TO BE ONE-TENTH MACHINE PRECISION.
C
C             --  ARGUMENTS --
C DOS()  [float,in]  ARRAY OF NOS COEFFICIENTS IN AN ORTHOGONAL SERIES.
C NOS    [integer,in]  NUMBER OF COEFFICIENTS IN DOS().
C ETA    [float,in]  REQUESTED ACCURACY OF SERIES.
C NTERMS [integer,out]  No. of terms needed to assure error .le. ETA.
C     ------------------------------------------------------------------
c--S replaces "?": ?INITS, ?ERM1
c     Also calls IERM1
C     ------------------------------------------------------------------
      integer I, NOS, NTERMS
      real             ERR, ETA, DOS(NOS)
C     ------------------------------------------------------------------
      IF (NOS .LT. 1) CALL IERM1('SINITS',1,0,
     *'Number of coefficients < 1','NOS',NOS,'.')
C
      ERR = 0.E0
      DO 10 I = NOS, 2, -1
        ERR = ERR + abs(DOS(I))
        IF (ERR .gt. ETA) GO TO 20
 10   CONTINUE
C
 20   IF (I .EQ. NOS) CALL SERM1('SINITS',2,0,
     *'Requested accuracy ETA is too small.','ETA',ETA,'.')
      NTERMS = I
C
      RETURN
      END
