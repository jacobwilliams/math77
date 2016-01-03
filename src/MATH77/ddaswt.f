      SUBROUTINE DDASWT (NEQ, INFO, RTOL, ATOL, Y, WT, RWORK, IWORK)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2003-03-11 ddaswt Hanson moved Soderlind's changes to ATOL, RTOL here.
c>> 2003-03-06 ddaswt Hanson started using reciprocal weights
c>> 2001-11-23 ddaswt Krogh  Changed many names per library conventions.
c>> 2001-11-04 ddaswt Krogh  Fixes for F77 and conversion to single
c>> 2001-11-01 ddaswt Hanson Provide code to Math a la Carte.
c--D replaces "?": ?daswt, ?daslx
c      IMPLICIT NONE
C***BEGIN PROLOGUE  DDASWT
C***SUBSIDIARY
C***PURPOSE  Set error weight vector for DDASLX.
C***LIBRARY   SLATEC (DDASLX)
C***TYPE      DOUBLE PRECISION (SDASWT-S, DDASWT-D)
C***AUTHOR  Petzold, Linda R., (LLNL)
C***DESCRIPTION
c ----------------------------------------------------------------------
C     THIS SUBROUTINE SETS THE ERROR WEIGHT VECTOR
C     WT ACCORDING TO WT(I)=RTOL(I)*ABS(Y(I))+ATOL(I),
C     I=1,-,N.
C     RTOL AND ATOL ARE SCALARS IF IWT = 0,
C     AND VECTORS IF IWT = 1.
c ----------------------------------------------------------------------
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   830315  DATE WRITTEN
C   901009  Finished conversion to SLATEC 4.0 format (F.N.Fritsch)
C   901019  Merged changes made by C. Ulrich with SLATEC 4.0 format.
C   901026  Added explicit declarations for all variables and minor
C           cosmetic changes to prologue.  (FNF)
C***END PROLOGUE  DDASWT
C
      integer  NEQ, INFO(16), IWORK(*)
      double precision  RTOL(*), ATOL(*), Y(*), WT(*), RWORK(*)

C
      integer  I
      double precision  ATOLI, RTOLI
CSM   
      integer itol, ismoot
      parameter(itol=2, ismoot=13)
      double precision TOL0, EXMP, EXMPM1
CSM
C
C***FIRST EXECUTABLE STATEMENT  DDASWT

      DO 10 I=1,NEQ
         IF (info(itol) .ne. 0) THEN
           RTOLI=RTOL(I)
           ATOLI=ATOL(I)
         ELSE
           RTOLI=RTOL(1)
           ATOLI=ATOL(1)
         END IF
         IF(info(ISMOOT) .eq. 0) THEN
           TOL0 = 1.0D-5

           EXMP = 7.0D0/9.0D0
           EXMPM1 = EXMP-1.0D0
           ATOLI= TOL0**(EXMPM1/EXMP)*ATOLI**(1.0D0/EXMP)
           RTOLI= TOL0**(EXMPM1/EXMP)*RTOLI**(1.0D0/EXMP)
c          ATOLI=min(5.D-3,ATOLI)
c          RTOLI=min(5.D-3,RTOLI)
        END IF
        WT(I)=1.d0/(RTOLI*ABS(Y(I))+ATOLI)
CSM
 10   CONTINUE
      RETURN
C -----------END OF SUBROUTINE DDASWT ----------------------------------
      END
