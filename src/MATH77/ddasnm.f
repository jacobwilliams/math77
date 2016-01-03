      DOUBLE PRECISION FUNCTION DDASNM (NEQ, V, WT, RWORK, IWORK)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2003-03-06 ddasnm Hanson changed norm computation to use reciprocals.
c>> 2001-11-23 ddasnm Krogh  Changed many names per library conventions.
c>> 2001-11-04 ddasnm Krogh  Fixes for F77 and conversion to single
c>> 2001-11-01 ddasnm Hanson Provide code to Math a la Carte.
c--D replaces "?": ?DASNM, ?DASLX
c      IMPLICIT NONE
C***BEGIN PROLOGUE  DDASNM
C***SUBSIDIARY
C***PURPOSE  Compute vector norm for DDASLX.
C***LIBRARY   SLATEC (DDASLX)
C***TYPE      DOUBLE PRECISION (SDASNM-S, DDASNM-D)
C***AUTHOR  Petzold, Linda R., (LLNL)
C***DESCRIPTION
c ----------------------------------------------------------------------
C     THIS FUNCTION ROUTINE COMPUTES THE WEIGHTED
C     ROOT-MEAN-SQUARE NORM OF THE VECTOR OF LENGTH
C     NEQ CONTAINED IN THE ARRAY V,WITH WEIGHTS
C     CONTAINED IN THE ARRAY WT OF LENGTH NEQ.
C        DDASNM=SQRT((1/NEQ)*SUM(V(I)/WT(I))**2)
c ----------------------------------------------------------------------
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   830315  DATE WRITTEN
C   901009  Finished conversion to SLATEC 4.0 format (F.N.Fritsch)
C   901019  Merged changes made by C. Ulrich with SLATEC 4.0 format.
C   901026  Added explicit declarations for all variables and minor
C           cosmetic changes to prologue.  (FNF)
C***END PROLOGUE  DDASNM
C
      INTEGER  NEQ, IWORK(*)
      DOUBLE PRECISION  V(NEQ), WT(NEQ), RWORK(*)
C
      INTEGER  I
      DOUBLE PRECISION  SUM, VMAX

      EXTERNAL D1MACH
      DOUBLE PRECISION D1MACH, G, H, T
      INTEGER L

C***FIRST EXECUTABLE STATEMENT  DDASNM

      H=sqrt(sqrt(d1mach(2)))
      G=d1mach(1)/d1mach(4)
      sum=0.d0
      vmax=0.d0
      DO 100 I=1,NEQ
        t=abs(v(i)*wt(i))
C If a component will have a square .gt. sqrt(huge) then
C shift to a scaled version of the norm.
        if(t .gt. H) GO TO 110
        sum=sum+t**2
        vmax=max(vmax, t)

  100 CONTINUE
C May have a damaging underflow here.  If vmax = 0 then 
C vector was flat zero.  If sum of squares is .le. tiny/epsilon
C then underflows (set to zero) may hurt accuracy.  So 
C shift to a scaled version of the norm.
      I=NEQ+1
      if(sum .le. G .and. vmax .gt. 0.D0) GO TO 110
      ddasnm=sqrt(sum/neq)
      return

  110 CONTINUE
      DDASNM = 0.0D0

C Can start loop at I since the first I-1 components have
C been scanned for the max abs already.
      DO 10 L = I,NEQ
        IF(ABS(V(L)*WT(L)) .GT. VMAX) VMAX = ABS(V(L)*WT(L))
10      CONTINUE
      IF(VMAX .LE. 0.0D0) GO TO 30
      SUM = 0.0D0
      DO 20 I = 1,NEQ
20      SUM = SUM + ((V(I)*WT(I))/VMAX)**2
      DDASNM = VMAX*SQRT(SUM/NEQ)
30    CONTINUE
      RETURN
c -----END OF FUNCTION DDASNM------
      END
