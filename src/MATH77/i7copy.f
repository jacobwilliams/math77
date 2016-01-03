      SUBROUTINE I7COPY(P, Y, X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c File: I7COPY.for       Three integer subrs used by the
c       David Gay & Linda Kaufman nonlinear LS package.
c       Needed for versions that allow Bounded variables.
c       I7COPY, I7PNVR, and I7SHFT
c
c>> 1992-04-27 CLL Removed unreferenced stmt labels.
c>> 1990-03-20 CLL @ JPL
*** from netlib, Wed Feb  7 19:41:53 EST 1990 ***
C
C  ***  SET Y = X, WHERE X AND Y ARE INTEGER P-VECTORS  ***
C
c     ------------------------------------------------------------------
      INTEGER P
      INTEGER X(P), Y(P)
C
      INTEGER I
C
      DO 10 I = 1, P
 10      Y(I) = X(I)
      END
c     ==================================================================
      SUBROUTINE I7PNVR(N, X, Y)
C
C  ***  SET PERMUTATION VECTOR X TO INVERSE OF Y  ***
C
      INTEGER N
      INTEGER X(N), Y(N)
C
      INTEGER I, J
      DO 10 I = 1, N
         J = Y(I)
         X(J) = I
 10      CONTINUE
C
      END
c     ==================================================================
      SUBROUTINE I7SHFT(N, K, X)
C
C  ***  SHIFT X(K),...,X(N) LEFT CIRCULARLY ONE POSITION  ***
C
c     ------------------------------------------------------------------
      INTEGER N, K
      INTEGER X(N)
C
      INTEGER I, NM1, T
C
      IF (K .GE. N) GO TO 999
      NM1 = N - 1
      T = X(K)
      DO 10 I = K, NM1
 10      X(I) = X(I+1)
      X(N) = T
 999  RETURN
      END
