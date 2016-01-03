      SUBROUTINE CCOEF(N, CROOT, CCOF)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1995-11-29 CCOEF  Krogh   Converted from SFTRAN to Fortran 77
C>> 1987-02-25 CCOEF  Lawson  Initial code.
c Conversion should only be done from "Z" to "C" for processing to C.
c--C replaces "?": ?COEF
C
c     Given N complex numbers, this subr computes the (complex)
c     coefficients of the Nth degree monic polynomial having these
c     numbers as its roots.
c     C. L. Lawson, JPL, 1987 Feb 13.
c
c     N     [In, Integer]  Number of given roots and degree of poly.
c     CROOT() [In, Complex]  The given ith complex root is CROOT(i).
c     CCOF()  [Out, Complex]  The (complex) coefficient of z**j will be
c           stored in CCOF(N+1-j) for j = 0, ...,N+1.  The high
c           order coeff will be one, i.e. CCOF(1) = (1.0, 0.0).
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INTEGER N, I, J
      COMPLEX CROOT(N), CCOF(N+1), CROOTI
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CCOF(1) = 1.E0
      CCOF(2) = -CROOT(1)
      DO 20 I = 2,N
        CROOTI = CROOT(I)
        CCOF(I+1) = -CCOF(I) * CROOTI
        DO 10 J = I,2,-1
           CCOF(J) = CCOF(J) - CCOF(J-1) * CROOTI
   10   continue
   20 continue
      RETURN
      END
