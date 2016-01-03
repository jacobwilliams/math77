      SUBROUTINE SQRBD (IPASS, Q, E, N, V, MDV, NRV, C, MDC, NCC)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 SQRBD  Krogh  Added external statement.
C>> 1994-10-20 SQRBD  Krogh  Changes to use M77CON
C>> 1992-03-13 SQRBD  FTK  Removed implicit statements.
C>> 1987-11-24 SQRBD  Lawson  Initial code.
c--S replaces "?": ?QRBD, ?ROT, ?ROTG, ?SWAP
c
C          Computes the singular value decomposition of an N order
c     upper bidiagonal matrix, B.  This decomposition is of the form
c
c     (1)            B = U * S * (V**t)
c
c     where U and V are each N x N orthogonal and S is N x N
c     diagonal, with nonnegative diagonal terms in nonincreasing
c     order.  Note that these matrices also satisfy
c
c                    S = (U**t) * B * V
c
c          The user may optionally provide an NRV x N matrix V1 in
c     the array V(,) and an N x NCC matrix C in the array C(,).
c     This subr will replace these matrices respectively by the
c     NRV x N matrix
c                                 V2 = V1 * V
c     and the N x NCC matrix
c                                 C2 = (U**t) * C1
c
c          On entry the bidiagonal matrix, B, is to be given in the
c    arrays Q() and E() as indicated by the diagram:
c
C                       (Q1,E2,0...    )
C                       (   Q2,E3,0... )
C                 B  =  (       .      )
C                       (         .   0)
C                       (   0       .EN)
C                       (            QN)
C
C          On return, the singular values, i.e. the diagonal terms of
c     S, will be stored in Q(1) through Q(N).
c
c     THIS CODE IS BASED ON THE PAPER AND 'ALGOL' CODE..
C REF..
C  1. REINSCH,C.H. AND GOLUB,G.H. 'SINGULAR VALUE DECOMPOSITION
C     AND LEAST SQUARES SOLUTIONS' (NUMER. MATH.), VOL. 14,(1970).
C
C     ------------------------------------------------------------------
C                             Subroutine Arguments
c
c     IPASS     (Out)  On return, IPASS = 1 means computation was
c               successful or N .le. 0.
c               IPASS = 2 means computation not successful after 10*N
c               iterations.  The algorithm usually succeeds in about
c               2*N iterations.
c
c     Q()       (In/Out)  On entry must contain the diagonal terms of
c               the bidiagonal matrix B.  On return contains the N
c               singular values of B.  These will be nonnegative and
c               in nonincreasing order.
c
c     E()       (In/Work)  On entry must contain the superdiagonal terms
c               of B.  E(1) is not used.  For i = 2, ..., N, E(i) must
c               contain B(i-1,i).  The contents of this array will be
c               modified by this subr.
c
c     N        (In)  The order of the bidiagonal matrix B and the
c               number of singular values to be produced.
c
c     V(,)      (In/Out)  If NRV .gt. 0, on entry this array contains an
c               NRV x N matrix, V1.  On return V(,) will contain the
c               NRV x N matrix V2 = V1 * V where V is defined by Eq.(1)
c               above.  If NRV .eq. 0 the array V(,) will not be
c               referenced.
c
c     MDV       (In)  First dimensioning parameter for the array V(,).
c               Require MDV .ge. NRV.
c
c     NRV       (In)  No. of rows in matrix V1 in array V(,).
c               Require NRV .ge. 0.
c
c     C(,)      (In/Out)  If NCC .gt. 0, on entry this array contains an
c               N x NCC matrix, C1.  On return C(,) will contain the
c               N x NCC matrix C2 = (U**t) * C1 where U is defined by
c               Eq.(1) above.  If NCC .eq. 0 the array C(,) will not be
c               referenced.
c
c     MDC       (In)  First dimensioning parameter for the array C(,).
c               Require MDC .ge. N.
c
c     NCC       (In)  No. of columns in matrix C1 in array C(,).
c               Require NCC .ge. 0.
c     ------------------------------------------------------------------
c     Subprograms referenced: ERMSG, R1MACH, SROT, SROTG, SSWAP.
c     ------------------------------------------------------------------
c          This code was originally developed by Charles L. Lawson and
c     Richard J. Hanson at Jet Propulsion Laboratory in 1973.  The
c     original code was described and listed in the book,
c
c                  Solving Least Squares Problems
c                  C. L. Lawson and R. J. Hanson
c                  Prentice-Hall, 1974
c
c     Feb, 1985, C. L. Lawson & S. Y. Chiu, JPL.  Adapted code from the
c     Lawson & Hanson book to Fortran 77 for use in the JPL MATH77
c     library.
c     Prefixing subprogram names with S or D for s.p. or d.p. versions.
c     Using generic names for intrinsic functions.
c     Adding calls to BLAS and MATH77 error processing subrs in some
c     program units.
c     ------------------------------------------------------------------
      EXTERNAL R1MACH
      INTEGER I,II,IPASS,J,K,L,LP1,MDC,MDV,N,N10,NCC,NQRS,NRV
      REAL             BIG,C(MDC,*),CS,R1MACH,DNORM,E(N),EPS,F,G,H
      REAL             ONE,Q(N),SMALL,SN,T,TEN,TWO,V(MDV,N),X,Y,Z,ZERO
      PARAMETER(ZERO = 0.0E0, ONE = 1.0E0, TWO = 2.0E0, TEN = 10.0E0)
      LOGICAL WNTV ,HAVERS,FAIL
c     ------------------------------------------------------------------
      EPS = R1MACH(4)
      BIG = TEN / SQRT(EPS)
      IPASS = 1
      IF (N .LE. 0) RETURN
      N10 = 10 * N
      WNTV = NRV.GT.0
      HAVERS = NCC.GT.0
      FAIL = .FALSE.
      NQRS = 0
      E(1) = ZERO
      DNORM = ZERO
           DO 10 J = 1,N
   10      DNORM = MAX(ABS(Q(J))+ABS(E(J)),DNORM)
           SMALL = DNORM * EPS
C     ------------------------------------------------------------------
C                                     Begin main loop.
           DO 200 K = N, 1, -1
C
C     TEST FOR SPLITTING OR RANK DEFICIENCIES..
C         FIRST MAKE TEST FOR LAST DIAGONAL TERM, Q(K), BEING SMALL.
   20       CONTINUE
            IF(K .EQ. 1) GO TO 50
            IF (ABS(Q(K)) .GT. SMALL) GO TO 50
C
C             SINCE Q(K) IS SMALL WE WILL MAKE A SPECIAL PASS TO
C             TRANSFORM E(K) TO ZERO.
C
           CS = ZERO
           SN = -ONE
           DO 40 II = 2,K
             I = K + 1 - II
             F = -SN * E(I + 1)
             E(I+1) = CS * E(I+1)
             CALL SROTG(Q(I),F,CS,SN)
C            TRANSFORMATION CONSTRUCTED TO ZERO POSITION (I,K).
C
C            ACCUMULATE RT. TRANSFORMATIONS IN V.
             IF (WNTV) CALL SROT(NRV,V(1,I),1,V(1,K),1,CS,SN)
   40      CONTINUE
C
C               THE MATRIX IS NOW BIDIAGONAL, AND OF LOWER ORDER
C               SINCE E(K)  .EQ.  ZERO..
C
   50           DO 60 L = K, 1, -1
                  IF (ABS(E(L)) .LE. SMALL) GO TO 100
                  IF (ABS(Q(L-1)) .LE. SMALL) GO TO 70
   60           CONTINUE
C
C                THE ABOVE LOOP CAN'T COMPLETE SINCE E(1)  =  ZERO.
C
           GO TO 100
C
C                          CANCELLATION OF E(L), L.GT.1.
C
   70      CS = ZERO
           SN = -ONE
           DO 90 I = L,K
             F = -SN * E(I)
             E(I) = CS * E(I)
             IF (ABS(F) .LE. SMALL) GO TO 100
             CALL SROTG(Q(I),F,CS,SN)
             IF (HAVERS) CALL SROT(NCC,C(I,1),MDC,C(L-1,1),MDC,CS,SN)
   90      CONTINUE
C
C                                 TEST FOR CONVERGENCE..
C
  100      Z = Q(K)
           IF (L .EQ. K) GO TO 170
C
C                      SHIFT FROM BOTTOM 2 BY 2 MINOR OF B**(T)*B.
C
           X = Q(L)
           Y = Q(K-1)
           G = E(K-1)
           H = E(K)
           F = ((Y-Z)*(Y+Z) + (G-H)*(G+H)) / (TWO*H*Y)
C
           IF (ABS(F) .LT. BIG) THEN
             G = SQRT(ONE + F**2)
           ELSE
             G = ABS(F)
           END IF
C
           IF (F .GE. ZERO) THEN
              T = F + G
           ELSE
              T = F - G
           END IF
C
           F = ((X-Z)*(X+Z) + H*(Y/T-H)) / X
C
C                                                 NEXT QR SWEEP..
           CS = ONE
           SN = ONE
           LP1 = L+1
           DO 160 I = LP1,K
             G = E(I)
             Y = Q(I)
             H = SN * G
             G = CS * G
             CALL SROTG(F,H,CS,SN)
             E(I-1) = F
             F = X * CS + G * SN
             G = -X*SN + G*CS
             H = Y * SN
             Y = Y * CS
C
C                      ACCUMULATE ROTATIONS (FROM THE RIGHT) IN 'V'
C
             IF (WNTV) CALL SROT(NRV,V(1,I-1),1,V(1,I),1,CS,SN)
             CALL SROTG(F,H,CS,SN)
             Q(I-1) = F
             F = CS*G + SN*Y
             X = -SN*G + CS*Y
C
C              APPLY ROTATIONS FROM THE LEFT TO RIGHT HAND SIDES IN 'C'
C
             IF (HAVERS) CALL SROT(NCC,C(I-1,1),MDC,C(I,1),MDC,CS,SN)
  160 CONTINUE
C
           E(L) = ZERO
           E(K) = F
           Q(K) = X
           NQRS = NQRS + 1
           IF (NQRS .LE. N10) THEN
c
c                            Return for further iteration on Q(K).
              GO TO 20
           ELSE
C                            Iteration count limit exceeded for Q(K).
              FAIL = .TRUE.
           END IF
C     ------------------------------------------------------------------
C          Accepting Q(K).  Either convergence has been reached or
c          the iteration count limit has been exceeded.
C          Adjust sign of Q(K) to be nonnegative.
c
  170      IF (Z .LT. ZERO) THEN
              Q(K) = -Z
              IF (WNTV) THEN
                 DO 180 J = 1,NRV
  180            V(J,K) = -V(J,K)
              END IF
           END IF
C
  200      CONTINUE
c                                    End of main loop.
c     ------------------------------------------------------------------
      IF (N .EQ. 1) RETURN
c
c         Test sing vals for being in order.  If not in order then
c         sort them.
c
           DO 210 I = 2,N
           IF (Q(I) .GT. Q(I-1)) GO TO 220
  210      CONTINUE
C                                     Sing vals are sorted.
           GO TO 290
c                                     Sort the sing vals.
  220      DO 270 I = 2,N
              T = Q(I-1)
              K = I-1
                DO 230 J = I,N
                   IF (T .LT. Q(J)) THEN
                      T = Q(J)
                      K = J
                   END IF
  230           CONTINUE
              IF (K .NE. I-1) THEN
                 Q(K) = Q(I-1)
                 Q(I-1) = T
                 IF (HAVERS) CALL SSWAP(NCC, C(I-1,1),MDC, C(K,1),MDC)
                 IF (WNTV)   CALL SSWAP(NRV, V(1,I-1),1, V(1,K),1)
              END IF
  270      CONTINUE
C
c                                    Sing vals are sorted.
C
C     ------------------------------------------------------------------
  290 CONTINUE
      IF (FAIL) THEN
C                                     Error message.
         IPASS = 2
         CALL ERMSG('SQRBD',1,0,
     *   'Convergence failure in computing singular values.','.')
      END IF
      RETURN
      END
