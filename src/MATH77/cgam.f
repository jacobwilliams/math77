      SUBROUTINE  CGAM(CARG,CANS,ERREST,MODE)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 CGAM  Krogh   Added external statement.
C>> 1995-11-20 CGAM Krogh Set up so M77CON converts between "Z" and "C".
C>> 1994-08-17 CLL Add tests on BIGINT to allow easier conversion to C.
C>> 1994-05-25 CGAM WVS generate COEF using PARAMETER
C>> 1994-04-20 CGAM CLL Make DP and SP versions similar.
C>> 1993-04-13 CGAM CLL Edit for conversion to C.
C>> 1992-04-20 CGAM CLL Edited comments.
C>> 1991-11-11 CGAM CLL Made [Z/C]GAM from CDLGAM
C>> 1991-01-16 CDLGAM Lawson  Removing use of subr D2MACH.
C>> 1985-08-02 CDLGAM Lawson  Initial code.
C
C *** COMPLEX GAMMA AND LOGGAMMA FUNCTIONS WITH ERROR ESTIMATE
C
C     -----------------------------------------------------------------
C     SUBROUTINE ARGUMENTS
C     --------------------
C     CARG()   A complex argument, given as an array of 2 floating-point
c              elements consisting of the real component
c              followed by the imaginary component.
c
c     CANS()   The complex answer, stored as an array of 2
c              floating-point numbers, representing the real and
c              imaginary parts.
c
c     ERREST   On output ERREST gives an estimate of the absolute
c              (for LOGGAMMA) or the relative (for GAMMA) error
c              of the answer.
c
c     MODE     Selects function to be computed.  set it to 0 for
c              LOGGAMMA, and 1 for GAMMA.
C     -----------------------------------------------------------------
C     MACHINE DEPENDANT PARAMETERS
C     If the fraction part of a floating point number
C     contains T digits using base B then
C         EPS3  = B ** (-T)
C         EPS4  = B ** (-T+1)
C         OMEGA = overflow limit
C         DESET = 5.0 on a binary machine
C               = 2.0 on a base 16 machine
C     -----------------------------------------------------------------
C     REFERENCE: H.KUKI, Comm.ACM, Vol.15, (1972),
C     pp.262-267, 271-272.  Subroutine name was CDLGAM.
C     Code developed for UNIVAC 1108 by E.W.NG, JPL, 1969.
C     Modified for FORTRAN 77 portability by C.L.LAWSON &
C     S.CHAN, JPL, 1983.
c     -----------------------------------------------------------------
c--C replaces "?": ?GAM
c--S (type)replaces "?": ?ERM1, ?ERV1
c     Also uses I1MACH, and R1MACH
c     -----------------------------------------------------------------
      external         I1MACH, R1MACH
      real             R1MACH
      real             A,AL1,AL2,AL2P,B,BIGINT
      real             CARG(2),CANS(2),COEF(7), CUT1
      real             DE0,DE1,DELTA,DESET,DN
      real             ELIMIT, EPS3, EPS4,ERREST,F0,F1,G0,G1
      real             H,H1,H2,HL2P,OMEGA, ONE
      real             PI, REPS3, T1,T2,TWOPI
      real             U,U1,U2,UU1,UU2,UUU1,UUU2,V1,V2,VV1,VV2
      real             W1,W2,W3,Y1,Z1,Z2, ZERO, ZZ1
      integer I1MACH, ITEMP, J, K, LF1, LF2, LF3, MODE, N
      logical FIRST
C
      SAVE FIRST,BIGINT,COEF,OMEGA,EPS4,EPS3,REPS3,CUT1,DESET,ELIMIT
C
      parameter(ONE = 1.0E0, ZERO = 0.0E0)
      parameter(F0 = 840.07385296052619E0, F1 = 20.001230821894200E0)
      parameter(G0 = 1680.1477059210524E0, G1 = 180.01477047052042E0)
      parameter(PI = 3.14159265358979323846E0)
      parameter(TWOPI = 6.283185307179586476925E0)
      parameter(HL2P = 0.918938533204672742E0)
      parameter(AL2P = 1.83787706640934548E0)

c              COEF(8-i) = bernoulli(2i)/(2i*(2i-1)).
      real             C1, C2, C3, C4, C5, C6, C7
      parameter (C1 = +1.0e0/156.0e0)
      parameter (C2 = -691.0e0/360360.0e0)
      parameter (C3 = +1.0e0/1188.0e0)
      parameter (C4 = -1.0e0/1680.0e0)
      parameter (C5 = +1.0e0/1260.0e0)
      parameter (C6 = -1.0e0/360.0e0)
      parameter (C7 = +1.0e0/12.0e0)
      DATA COEF / C1, C2, C3, C4, C5, C6, C7 /
c     DATA COEF   /+0.641025641025641026E-2,
c    *             -0.191752691752691753E-2,
c    *             +0.841750841750841751E-3,
c    *             -0.595238095238095238E-3,
c    *             +0.793650793650793651E-3,
c    *             -0.277777777777777778E-2,
c    *             +0.833333333333333333E-1/
      DATA FIRST/.TRUE./
c     ------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        OMEGA = R1MACH(2)
        EPS3  = R1MACH(3)
        EPS4  = R1MACH(4)
        REPS3 = ONE / EPS3
        ELIMIT = log(OMEGA)
        CUT1  = log(EPS3)
        BIGINT = I1MACH(9) - 2
          IF (I1MACH(10) .EQ. 2) THEN
            DESET = 5.E0
          ELSE
            DESET = 2.E0
          ENDIF
      ENDIF
      DE0 = DESET
      DE1 = ZERO
      Z1 = CARG(1)
      Z2 = CARG(2)
C
C *** Setting DELTA = estimate of uncertainty level of
C                     argument data.
C
      DELTA = EPS4 * (abs(Z1) + abs(Z2))
      IF (DELTA .EQ. ZERO) DELTA = EPS4
C
C *** FORCE SIGN OF IMAGINARY PART OF ARG TO NON-NEGATIVE
C
      LF1 = 0
      IF (Z2 .lt. ZERO) then
         LF1 = 1
         Z2 = -Z2
      endif
      LF2 = 0
      IF (Z1 .GE. ZERO) GO TO 100
C
C *** CASE WHEN REAL PART OF ARG IS NEGATIVE
C
      LF2 = 1
      LF1 = LF1-1
      T1 = AL2P - PI*Z2
      T2 = PI*(0.5E0 - Z1)
      U  = -TWOPI*Z2
      IF (U .lt. -0.1054E0) then
         A  = ZERO
C
C ***    IF EXP(U) .LE. EPS3, IGNORE IT TO SAVE TIME AND TO AVOID
C        IRRELEVANT UNDERFLOW
C
         IF (U .gt. CUT1) then
            A  = exp(U)
         endif
         H1 = ONE - A
      else
         U2 = U * U
         A  = -U*(F1*U2 + F0)
         H1 = (A + A)/((U2 + G1)*U2 + G0 + A)
         A  = ONE - H1
      endif
c
c              Here Z1 is negative.
c
      if(Z1 .lt. -BIGINT) then
         CALL SERM1('CGAM',3,0,'Require CARG(1) .ge. -BIGINT',
     *              'CARG(1)', Z1, ',')
         CALL SERV1('-BIGINT',-BIGINT,'.')
         go to 700
      endif
c
c           Truncate to integer: ITEMP
c
      ITEMP = Z1 - 0.5e0
      B  = Z1 - ITEMP
      H2 = A*sin(TWOPI*B)
      B  = sin(PI*B)
      H1 = H1 + (B+B)*B*A
      H  = abs(H2) + H1 - TWOPI*A*DELTA
      IF (H .LE. ZERO) GO TO 500
      DE0 = DE0 + abs(T1) + T2
      DE1 = PI + TWOPI*A/H
      Z1 = ONE - Z1
C
C *** CASE WHEN NEITHER REAL PART NOR IMAGINARY PART OF ARG IS
C     NEGATIVE.  DEFINE THRESHOLD CURVE TO BE THE BROKEN LINES
C     CONNECTING POINTS 10F0*I, 10F4.142*I, 0.1F14.042*I,AND
C     0.1FOMEGA*I
C
  100 LF3 = 0
      Y1 = Z1 - 0.5E0
      W1 = ZERO
      W2 = ZERO
      K  = 0
      B  = max(0.1E0, min(10.0E0, 14.142E0-Z2)) - Z1
      IF (B .LE. ZERO) GO TO 200
C
C *** CASE WHEN REAL PART OF ARG IS BETWEEN 0 AND THRESHOLD
C
      LF3 = 1
      ZZ1 = Z1
      N  = B + ONE
      DN = N
      Z1 = Z1 + DN
      A  = Z1*Z1 + Z2*Z2
      V1 = Z1/A
      V2 = -Z2/A
C
C *** INITIALIZE U1+U2*I AS THE RIGHTMOST FACTOR 1-1/(Z+N)
C
      U1 = ONE - V1
      U2 = -V2
      K  = 6.0E0 - Z2*0.6E0 - ZZ1
      IF (K .gt. 0) then
C
C *** FORWARD ASSEMBLY OF FACTORS (Z+J-1)/(Z+N)
C
         N  = N - K
         UU1 = (ZZ1*Z1 + Z2*Z2) / A
         UU2 = DN*Z2/A
         VV1 = ZERO
         VV2 = ZERO
         DO 110  J = 1,K
            B  = U1*(UU1+VV1) - U2*(UU2+VV2)
            U2 = U1*(UU2+VV2) + U2*(UU1+VV1)
            U1 = B
            VV1 = VV1 + V1
            VV2 = VV2 + V2
  110    continue
      endif
      IF (N .GE. 2) then
C
C *** BACKWARD ASSEMBLY OF FACTORS 1-J/(Z+N)
C
         VV1 = V1
         VV2 = V2
         DO 130  J = 2,N
            VV1 = VV1 + V1
            VV2 = VV2 + V2
            B  = U1*(ONE - VV1) + U2*VV2
            U2 = -U1*VV2 + U2*(ONE - VV1)
            U1 = B
  130   continue
      endif
      U  = U1*U1 + U2*U2
      IF (U .EQ. ZERO) GO TO 500
      IF (MODE .NE. 0) then
         IF (K .LE. 0) GO TO 200
      endif
      AL1 = log(U)*0.5E0
      IF (MODE .EQ. 0) then
         W1 = AL1
         W2 = atan2(U2,U1)
         IF (W2 .LT. ZERO)  W2 = W2 + TWOPI
         IF (K .LE. 0) GO TO 200
      endif
      A  = ZZ1 + Z2 - DELTA
      IF (A .LE. ZERO) GO TO 500
      DE0 = DE0 - AL1
      DE1 = DE1 + 2.0E0 + ONE/A
C
C *** CASE WHEN REAL PART OF ARG IS GREATER THAN THRESHOLD
C
  200 A  = Z1*Z1 + Z2*Z2
      AL1 = log(A)*0.5E0
      AL2 = atan2(Z2,Z1)
      V1 = Y1*AL1 - Z2*AL2
      V2 = Y1*AL2 + Z2*AL1
C
C *** Evaluate asymptotic terms. Ignore this term,
C     if ABS(ARG)**2 .GT. REPS3, to save time and
C     to avoid irrelevant underflow.
C
      VV1 = ZERO
      VV2 = ZERO
      IF (A .GT. REPS3) GO TO 220
      UU1 = Z1/A
      UU2 = -Z2/A
      UUU1 = UU1*UU1 - UU2*UU2
      UUU2 = UU1*UU2*2.0E0
      VV1 = COEF(1)
      DO 210  J = 2,7
        B  = VV1*UUU1 - VV2*UUU2
        VV2 = VV1*UUU2 + VV2*UUU1
  210   VV1 = B + COEF(J)
      B  = VV1*UU1 - VV2*UU2
      VV2 = VV1*UU2 + VV2*UU1
      VV1 = B
  220 W1 = (((VV1 + HL2P) - W1) - Z1) + V1
      W2 = ((VV2 - W2) - Z2) + V2
      DE0 = DE0 + abs(V1) + abs(V2)
      IF (K .LE. 0)  DE1 = DE1 + AL1
C FINAL ASSEMBLY
      IF (LF2 .EQ. 0) then
         IF (MODE .NE. 0) then
            IF (W1 .GT. ELIMIT) GO TO 550
            A  = exp(W1)
            W1 = A*cos(W2)
            W2 = A*sin(W2)
            IF (LF3 .NE. 0) then
               B  = (W1*U1 + W2*U2) / U
               W2 = (W2*U1 - W1*U2) / U
               W1 = B
            endif
         endif
         GO TO 400
      endif
      H  = H1*H1 + H2*H2
      IF (H .EQ. ZERO) GO TO 500
      IF (MODE .EQ. 0 .or. H .le. 1.0E-2) then
         A  = log(H)*0.5E0
         IF (H .LE. 1.0E-2)  DE0 = DE0 - A
         IF (MODE .EQ. 0) then
            W1 = (T1 - A) - W1
            W2 = (T2 - atan2(H2,H1)) - W2
            GO TO 400
         endif
      endif
c                       Here we have MODE .ne. 0 and LF2 .ne. 0.
      T1 = T1 - W1
      T2 = T2 - W2
      IF (T1 .GT. ELIMIT) GO TO 550
      A  = exp(T1)
      T1 = A*cos(T2)
      T2 = A*sin(T2)
      W1 = (T1*H1 + T2*H2)/H
      W2 = (T2*H1 - T1*H2)/H
      IF (LF3 .NE. 0) then
         B  = W1*U1 - W2*U2
         W2 = W1*U2 + W2*U1
         W1 = B
      endif
  400 continue
      IF (LF1 .NE. 0)  W2 = -W2
C
C *** TRUNCATION ERREST OF STIRLINGS FORMULA IS UP TO EPS3.
C
      DE1 = DE0*EPS4 + EPS3 + DE1*DELTA

c                                      Normal termination.
c
c     The imaginary part of the log of a complex number is nonunique
c     to within multiples of 2*Pi.  We prefer a result for loggamma
c     having its imaginary part .gt. -Pi and .le. +Pi.  The result at
c     this point is usually in this range.  If not we will move it
c     into this range.  -- CLL 11/11/91
c
      if(MODE .eq. 0) then
         if(W2 .le. -PI .or. W2 .gt. PI) then
            W3 = abs(W2)
            T1 = W3/PI + ONE
            if(abs(T1) .gt. BIGINT) then
               CALL SERM1('CGAM',4,0,'Argument out of range.',
     *                    'CARG(1)',CARG(1),',')
               CALL SERV1('CARG(2)',CARG(2),'.')
               go to 700
            endif
            T2 = int(T1) / 2
            W3 = W3 - T2 * TWOPI
            if(W2 .ge. 0.0e0) then
               W2 = W3
            else
               W2 = -W3
            endif
            if(W2 .le. -PI) then
               W2 = W2 + TWOPI
            elseif(W2 .gt. PI) then
               W2 = W2 - TWOPI
            endif
         endif
      endif
      CANS(1) = W1
      CANS(2) = W2
      ERREST = DE1
      return
c                                           Error termination.
C
C *** CASE WHEN ARGUMENT IS TOO CLOSE TO A SINGULARITY
C
  500 CONTINUE
      CALL SERM1('CGAM',1,0,'Z TOO CLOSE TO A SINGULARITY',
     *           'Z(1)',CARG(1),',')
      CALL SERV1('Z(2)',CARG(2),'.')
      GO TO 700
C
  550 CONTINUE
      CALL SERM1('CGAM',2,0,'ARG TOO LARGE. EXP FUNCTION OVERFLOW',
     *           'Z(1)',CARG(1),',')
      CALL SERV1('Z(2)',CARG(2),'.')
  700 continue
      CANS(1) = OMEGA
      CANS(2) = OMEGA
      ERREST = OMEGA
      return
      end
