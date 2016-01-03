      SUBROUTINE DGAMI (A, X, PANS, QANS, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c
C>> 1998-10-29 DGAMI Krogh  Moved external statement up for mangle.
c>> 1996-03-30 DGAMI Krogh ALOG10 => CLOG10
c>> 1994-11-23 DGAMI CLL Added ?GR17 & ?GR29 to list of "c--" names.
c>> 1994-11-18 DGAMI WV Snyder del block data.  Change error reporting.
C>> 1994-11-03 DGAMI Krogh  Changed F90 do's to F77 do's.
C>> 1994-10-20 DGAMI Krogh  Changes to use M77CON
C>> 1994-05-09 DGAMI WVS JPL Make SP and DP from same source
C>> 1994-03-14 DGAMI WVS JPL Repair incomplete error message
C>> 1993-07-21 DGAMI WVS JPL Conversion from NSWC to Math 77
c--D replaces "?": ?GAMI, ?GAMIC, ?GAMIE, ?GAMIK, ?ERF, ?ERFC, ?ERFCE,
c--&               ?GAM1, ?RCOMP, ?RLOG, ?RLOG1, ?REXP, ?GAMIB,
c--&               ?GR17, ?GR29,  ?ERM1, ?ERV1
C ----------------------------------------------------------------------
C        Evaluation of the incomplete gamma function ratios
C        P(A,X) and Q(A,X), where P(A,X) = gamma(A,X)/GAMMA(A),
C        and P(A,X) + Q(A,X) = 1.
C
C                        ----------
C
C     IT IS ASSUMED THAT A AND X ARE NONNEGATIVE, WHERE A AND X
C     ARE NOT BOTH 0.
C
C     PANS AND QANS ARE VARIABLES. DGAMI ASSIGNS PANS THE VALUE
C     P(A,X) AND QANS THE VALUE Q(A,X).
C
C     NORMAL RETURN ...
C
C        UPON NORMAL RETURN, BOTH OF PANS AND QANS WILL BE IN THE RANGE
C     [0.0 .. 1.0], PANS + QANS WILL BE WITHIN ROUND-OFF OF 1.0, AND
C     IERR = 0.
C
C     ERROR RETURN ...
C
C        PANS IS ASSIGNED THE VALUE 3 WHEN A AND X ARE BOTH ZERO AND THE
C     VALUE 4 WHEN ONE OR BOTH OF A OR X IS NEGATIVE.  IERR = INT(PANS)
C     WHEN PANS > 1.  IERR is assigned the value 2 if the desired error
c     was not achieved.
c
c     To set tolerances, CALL DGAMIK, q.v.  To discover the error
c     committed on the last call to DGAMI, CALL DGAMIE, q.v.
C
C ----------------------------------------------------------------------
C     WRITTEN BY ALFRED H. MORRIS, JR.
C        NAVAL SURFACE WARFARE CENTER, DAHLGREN, VIRGINIA
C     REVISED ... DEC 1991
c     See "Computation of the Incomplete Gamma Function", ACM TOMS 12, 4
c     (Dec 1986) 377-393.
C     Adapted to Math 77 by W. Van Snyder, JPL, 1993 May 5.
C ------------------------
      EXTERNAL DERF, DERFC, DERFCE, DGAM1
      EXTERNAL DRCOMP, DRLOG, drlog1, DREXP
      double precision DERF, DERFC, DERFCE, DGAM1
      double precision DRCOMP, DRLOG, drlog1, DREXP
C ------------------------
      double precision A, X, PANS, QANS
      INTEGER IERR
      double precision ACC, AMN, APN, A2N, A2NM1, B2N,
     *                 B2NM1, C, G, H, J, L, R, RTA, RTX, S,
     *                 SUM, T, TOL, TWOA, U,     W, Y, Z, WK(20)
      double precision ACC0(5), BIG(5), E0(5), X0(5), X1(5)
      double precision A0(4), A1(4), A2(2), A3(2), A4(2), A5(2),
     *                 A6(2), A7(2), A8(2)
      double precision B0(6), B1(4), B2(5), B3(5), B4(4), B5(3),
     *                 B6(2), B7(2)
      double precision C0, C1, C2, C3, C4, C5, C6, C7, C8
      double precision D0(6), D1(4), D20, D2(2), D30, D3(2), D40, D4(1)
      double precision D50, D5(1), D60, D6(1), D70, D80
      integer I, IOP, M, N
      double precision E8, R30
      SAVE E8, R30
C ------------------------
      double precision CLOG10, RT2PIN, RTPI, RTPIN
C     CLOG10 = LN(10)
C     RT2PIN = 1/SQRT(2*PI)
C     RTPI   = SQRT(PI)
C     RTPIN   = 1/SQRT(PI)
      PARAMETER (CLOG10=2.30258509299404568401799145468D0)
      PARAMETER (RT2PIN=.398942280401432677939946059935D0)
      PARAMETER (RTPI  =1.77245385090551602729816748334D0)
      PARAMETER (RTPIN =1.0D0 / RTPI)
C ------------------------
C     SOME OF THE TEMME COEFFICIENTS:
      double precision D00, D01, D02, D03, D04, D10, D11, D12
      PARAMETER (D00 = - 1.0D0 / 3.0D0, D01 = 1.0D0 / 12.0D0)
      PARAMETER (D02 = -2.0D0 / 135.0D0, D03 = 1.0D0 / 864.0D0)
      PARAMETER (D04 = 1.0D0 / 2835.0D0)
      PARAMETER (D10 = -1.0D0 / 540.0D0, D11 = -1.0D0 / 288.0D0)
      PARAMETER (D12 = 1.0D0 / 378.0D0)
C --- COMMON /DGAMIC/ ---- (see DGAMIK for discussion)
C PQERR is approximately X * ABS(derivative of P(a,x) with respect to
c     X).  PQERR is used to estimate the error in P and Q due to
c     uncertainty in X:  Error in P or Q = PQERR * (relative error in
c     X).  When A and X are equal, PQERR = sqrt(A / (2*Pi) ).  Since P
c     and Q are bounded by 1, there can be no precision in the result if
c     A or X has relative error eps, and A > 2*Pi/eps**2.  If A and X
c     are not nearly equal, PQERR is small.
      double precision PTOL, QTOL, XERR, PQERR, ROUND
      integer MSGOFF
      logical FIRST
      save FIRST
      common /DGAMIC/ PTOL, QTOL, XERR, PQERR, ROUND, MSGOFF
      save /DGAMIC/
      data FIRST /.TRUE./
C ------------------------
      data ACC0 / 5.0d-30, 5.0d-17, 5.0d-15, 5.0d-7, 5.0d-4 /
      data BIG / 50.0d0, 30.0d0, 25.0d0, 14.0d0, 10.0d0/
      data E0 / 0.0d0, 0.0d0, 0.25d-3, 0.25d-1, 0.14d0 /
      data X0 /68.0d0, 45.0d0, 31.0d0, 17.0d0, 9.7d0 /
      data X1 / 2.0d0, 2.0d0, 1.1d0, 1.1d0, 1.1d0 /
C ------------------------
C
C             COEFFICIENTS FOR MINIMAX APPROXIMATIONS
C                          FOR C0,...,C8
C
C ------------------------
      DATA A0(1) /-.231272501940775D-02/, A0(2)/-.335378520024220D-01/,
     *     A0(3) /-.159840143443990D+00/, A0(4)/-.333333333333333D+00/
      DATA B0(1)  /.633763414209504D-06/, B0(2)/-.939001940478355D-05/,
     *     B0(3)  /.239521354917408D-02/, B0(4)/ .376245718289389D-01/,
     *     B0(5)  /.238549219145773D+00/, B0(6)/ .729520430331981D+00/
C ------------------------
      DATA A1(1) /-.398783924370770D-05/, A1(2)/-.587926036018402D-03/,
     *     A1(3) /-.491687131726920D-02/, A1(4)/-.185185185184291D-02/
      DATA B1(1)  /.386325038602125D-02/, B1(2) /.506042559238939D-01/,
     *     B1(3)  /.283344278023803D+00/, B1(4) /.780110511677243D+00/
C ------------------------
      DATA A2(1)  /.669564126155663D-03/, A2(2) /.413359788442192D-02/
      DATA B2(1) /-.421924263980656D-03/, B2(2) /.650837693041777D-02/,
     *     B2(3) / .682034997401259D-01/, B2(4) /.339173452092224D+00/,
     *     B2(5) / .810647620703045D+00/
C ------------------------
      DATA A3(1)  /.810586158563431D-03/, A3(2) /.649434157619770D-03/
      DATA B3(1) /-.632276587352120D-03/, B3(2) /.905375887385478D-02/,
     *     B3(3) / .906610359762969D-01/, B3(4) /.406288930253881D+00/,
     *     B3(5) / .894800593794972D+00/
C ------------------------
      DATA A4(1) /-.105014537920131D-03/, A4(2)/-.861888301199388D-03/
      DATA B4(1)  /.322609381345173D-01/, B4(2) /.178295773562970D+00/,
     *     B4(3)  /.591353097931237D+00/, B4(4) /.103151890792185D+01/
C ------------------------
      DATA A5(1) /-.435211415445014D-03/, A5(2)/-.336806989710598D-03/
      DATA B5(1)  /.178716720452422D+00/, B5(2) /.600380376956324D+00/,
     *     B5(3)  /.108515217314415D+01/
C ------------------------
      DATA A6(1) /-.182503596367782D-03/, A6(2) /.531279816209452D-03/
      DATA B6(1)  /.345608222411837D+00/, B6(2) /.770341682526774D+00/
C ------------------------
      DATA A7(1)  /.443219646726422D-03/, A7(2) /.344430064306926D-03/
      DATA B7(1)  /.821824741357866D+00/, B7(2) /.115029088777769D+01/
C ------------------------
      DATA A8(1)  /.878371203603888D-03/, A8(2)/-.686013280418038D-03/
C ------------------------
C
C              COEFFICIENTS FOR THE TEMME EXPANSION
C
C ------------------------
      DATA D0(1) / D01 /, D0(2) / D02 /, D0(3) / D03 /, D0(4) / D04 /,
     *     D0(5) /-.178755144032922D-03/, D0(6) / .391926317852244D-04/
C ------------------------
      DATA D1(1) / D11 /, D1(2) / D12 /,
     *     D1(3) /-.990226337448560D-03/, D1(4) / .205761316872428D-03/
C ------------------------
      DATA D20   / .413359788359788D-02/, D2(1) /-.268132716049383D-02/,
     *     D2(2) / .771604938271605D-03/
C ------------------------
      DATA D30   / .649434156378601D-03/, D3(1) / .229472093621399D-03/,
     *     D3(2) /-.469189494395256D-03/
C ------------------------
      DATA D40   /-.861888290916712D-03/, D4(1) / .784039221720067D-03/
C ------------------------
      DATA D50   /-.336798553366358D-03/, D5(1) /-.697281375836586D-04/
C ------------------------
      DATA D60   / .531307936463992D-03/, D6(1) /-.592166437353694D-03/
C ------------------------
      DATA D70   / .344367606892378D-03/
C ------------------------
      DATA D80   /-.652623918595309D-03/
C ------------------------
C
      if (first) then
         first = .false.
         call DGAMIB
         call DGAMIK (0.0d0, 0.0d0, -1.0d0, 0)
         e8 = 8.0d0 * round
         r30 = max(round,1.0d-30)
      end if
c
      IF (A .EQ. 0.5D0) THEN
         IF (X .LT. 0.0D0) GO TO 510
c        Equation 8 in ref.
         RTX = SQRT(X)
         pqerr = rtx * rtpin * exp(-x)
         IF (X .LT. 0.25D0) THEN
            PANS = DERF(RTX)
            GO TO 395
         END IF
         QANS = DERFC(RTX)
         go to 390
      END IF
C
      acc = max(r30,min(abs(ptol),abs(qtol)))
      iop = 1
      do 150 n = 2, 5
         if (acc .ge. acc0(n)) iop = n
  150 continue
      acc = min(acc0(5),acc)
C
C              SELECT THE APPROPRIATE ALGORITHM
C
      IF (A .GE. 1.0D0) THEN
         IF (X .LT. 0.0D0) GO TO 510
         IF (X .EQ. 0.0D0) GO TO 300
         IF (A .LT. BIG(IOP)) THEN
            IF (A .LE. X .AND. X .LT. X0(IOP)) THEN
               TWOA = A + A
               M = TWOA
               IF (TWOA .EQ. M) THEN
C
C                 FINITE SUMS FOR Q WHEN A .GE. 1 AND 2*A IS AN INTEGER
c                 Equation 14 in ref.
C
                  I = M/2
                  T = EXP(-X)
                  C = A - I
                  IF (C .EQ. 0.0D0) THEN
                     SUM = T
                  ELSE
                     RTX = SQRT(X)
                     T = 2.0D0*T*RTX/RTPI
                     SUM = DERFC(RTX) + T
                  END IF
                  DO 160 N = 2, I
                     C = C + 1.0D0
                     T = (X*T)/C
                     SUM = SUM + T
  160             CONTINUE
                  pqerr = X * T
                  QANS = SUM
                  go to 390
               END IF
            END IF
         ELSE
C
            L = X/A
            IF (L .EQ. 0.0D0) GO TO 300
            if (l .le. 0.5d0) then
               s = (l - 0.5d0) - 0.5d0
               z = drlog(l)
            else
               s = x - a
               if (s + a .ne. x) then
c                 Exponents of S and A are different
                  if (x .gt. a) then
                     s = (0.5d0*x - a) + 0.5d0*x
                  else
                     s = (x - 0.5d0*a) - 0.5d0*a
                  end if
               end if
               s = s / a
               z = drlog1(s)
            end if
            rta = sqrt(a)
            if (z .ge. 700.0d0/a) GO TO 330
            Y = A*Z
            IF (ABS(S) .LE. 0.4D0) THEN
C
C                 MINIMAX APPROXIMATIONS
C
               U = 1.0D0/A
               Z = SQRT(Z + Z)
               IF (L .LT. 1.0D0) Z = -Z
               C = EXP(-Y)
               W = 0.5D0*DERFCE(SQRT(Y))
               IF (RTA*ABS(S) .GT. E0(IOP)) THEN
c                 Equations 17 in ref.
                  go to (210, 220, 230, 240, 250), iop
  210             continue
                     call dgr29 (z, u, t)
                     go to 290
c
  220             continue
                     call dgr17 (z, u, t)
                     go to 290
C
  230             IF (ABS(S) .LE. 1.0D-3) GO TO 260
C
C              USING THE MINIMAX APPROXIMATIONS
C
         C0 = (((A0(1)*Z + A0(2))*Z + A0(3))*Z + A0(4)) / ((((((B0(1)*Z
     *     + B0(2))*Z + B0(3))*Z + B0(4))*Z + B0(5))*Z + B0(6))*Z + 1.0)
         C1 = (((A1(1)*Z + A1(2))*Z + A1(3))*Z + A1(4)) /
     *     ((((B1(1)*Z + B1(2))*Z + B1(3))*Z + B1(4))*Z + 1.0)
         C2 = (A2(1)*Z + A2(2))/(((((B2(1)*Z + B2(2))*Z + B2(3))*Z +
     *     B2(4))*Z + B2(5))*Z + 1.0)
         C3 = (A3(1)*Z + A3(2))/(((((B3(1)*Z + B3(2))*Z + B3(3))*Z +
     *     B3(4))*Z + B3(5))*Z + 1.0)
         C4 = (A4(1)*Z + A4(2))/((((B4(1)*Z + B4(2))*Z + B4(3))*Z +
     *     B4(4))*Z + 1.0)
         C5 = (A5(1)*Z + A5(2))/(((B5(1)*Z + B5(2))*Z + B5(3))*Z + 1.0)
         C6 = (A6(1)*Z + A6(2))/((B6(1)*Z + B6(2))*Z + 1.0)
         C7 = (A7(1)*Z + A7(2))/((B7(1)*Z + B7(2))*Z + 1.0)
         C8 = A8(1)*Z + A8(2)
         T = (((((((C8*U + C7)*U + C6)*U + C5)*U + C4)*U + C3)*U +
     *                  C2)*U + C1)*U + C0
                  GO TO 290
C
C                 TEMME EXPANSION
C
  240             continue
                     C0 = (((((D0(6) * Z + D0(5)) * Z + D0(4)) * Z
     *                     + D0(3)) * Z + D0(2)) * Z + D0(1)) * Z + D00
                     C1 = (((D1(4) * Z + D1(3)) * Z + D1(2)) * Z
     *                     + D1(1)) * Z + D10
                     C2 = D2(1) * Z + D20
                     T  = (C2*U + C1)*U + C0
                     GO TO 290
C
  250             continue
                     T  = ((D0(3) * Z + D0(2)) * Z + D0(1)) * Z + D00
                     GO TO 290
               END IF
C
C              TEMME EXPANSION FOR L = 1 (Equations 17-18 in ref).
c              (Can't get here if IOP = 1 or 2 because E0(1..2) = 0.0).
C
               IF (IOP - 4) 260,270,280
C
  260          C0 = ((D0(3) * Z + D0(2)) * Z + D0(1)) * Z + D00
               C1 = ((D1(3) * Z + D1(2)) * Z + D1(1)) * Z + D10
               C2 = (D2(2) * Z + D2(1)) * Z + D20
               C3 = (D3(2) * Z + D3(1)) * Z + D30
               C4 = D4(1) * Z + D40
               C5 = D5(1) * Z + D50
               C6 = D6(1) * Z + D60
               T  = (((((((D80*U + D70)*U + C6)*U + C5)*U + C4)*U
     *                   + C3)*U + C2)*U + C1)*U + C0
               GO TO 290
C
  270          C0 = (D0(2) * Z + D0(1)) * Z + D00
               C1 = D1(1) * Z + D10
               T  = (D20*U + C1)*U + C0
               GO TO 290
C
  280          T  = D0(1) * Z + D00
C
  290          pqerr = C * RTA * RT2PIN
               IF (L .GE. 1.0D0) THEN
                  QANS = C*(W + RT2PIN*T/RTA)
                  GO TO 390
               END IF
               PANS = C*(W - RT2PIN*T/RTA)
               go to 395
            END IF
         END IF
C
         R = DRCOMP(A,X)
         IF (R .EQ. 0.0D0) GO TO 331
         pqerr = R * SQRT(A) * RT2PIN
         IF (X .LE. MAX(A,CLOG10)) THEN
C
C              TAYLOR SERIES FOR P/R (Equation 15 in ref.)
C
            APN = A + 1.0D0
            T = X/APN
            DO 51 N = 1, 20
               WK(N) = T
               APN = APN + 1.0D0
               T = T*(X/APN)
               IF (T .LE. 1.0D-3) GO TO 60
   51       CONTINUE
            N = 20
C
   60       SUM = T
            TOL = 0.5D0*ACC
   61       CONTINUE
               APN = APN + 1.0D0
               T = T*(X/APN)
               SUM = SUM + T
            IF (T .GT. TOL) GO TO 61
C
            DO 70 M = N, 1, -1
               SUM = SUM + WK(M)
   70       CONTINUE
            PANS = (R/A)*(1.0D0 + SUM)
            go to 395
         END IF
         IF (X .LT. X0(IOP)) GO TO 170
C
C              ASYMPTOTIC EXPANSION (Equation 15 in ref).
C
         AMN = A - 1.0D0
         T = AMN/X
         DO 81 N = 1, 20
            WK(N) = T
            AMN = AMN - 1.0D0
            T = T*(AMN/X)
            IF (ABS(T) .LE. 1.0D-3) GO TO 90
   81    CONTINUE
         N = 20
C
   90    SUM = T
   91    IF (ABS(T) .GE. ACC) THEN
            AMN = AMN - 1.0D0
            T = T*(AMN/X)
            SUM = SUM + T
            GO TO 91
         END IF
C
         DO 101 M = N, 1, -1
            SUM = SUM + WK(M)
  101    CONTINUE
         QANS = (R/X)*(1.0D0 + SUM)
         go to 390
      END IF
C
C     A .LT. 1.0 HERE
C
      IF (X .LT. X1(IOP)) THEN
         IF (A .LT. 0.0D0 .OR. X .LT. 0.0D0) GO TO 510
         IF (a*x .eq. 0.0D0) GO TO 320
C
C              TAYLOR SERIES FOR P(A,X)/X**A (Equations 9-10 in ref.)
C
         L = 3.0D0
         C = X
         SUM = X/(A + 3.0D0)
         TOL = 3.0D0*ACC/(A + 1.0D0)
  120    CONTINUE
            L = L + 1.0D0
            C = -C*(X/L)
            T = C/(A + L)
            SUM = SUM + T
         IF (ABS(T) .GT. TOL) GO TO 120
         J = A*X*((SUM/6.0D0 - 0.5D0/(A + 2.0D0))*X + 1.0D0/(A + 1.0D0))
C
         Z = A*LOG(X)
         H = DGAM1(A)
         G = 1.0D0 + H
         pqerr = A*SQRT(A)*EXP(Z-X)*RT2PIN
         U = EXP(Z)
c        Equation 9 in ref.
         PANS = U*G*(0.5D0 + (0.5D0 - J))
         IF (PANS .LE. 0.9D0) GO TO 395
         L = DREXP(Z)
c        Equation 10 in ref.
         QANS = MAX((U*J - L)*G - H, 0.0D0)
         go to 390
      END IF
C
C     A .LT. 1.0 AND X .GE. X1(IOP) here (BUT A .NE. 0.5)
C
      IF (A .LT. 0.0D0) GO TO 510
      IF (A .EQ. 0.0D0) GO TO 310
      R = DRCOMP(A,X)
      pqerr = R*SQRT(A)*RT2PIN
      IF (R .EQ. 0.0D0) GO TO 310
C
C              CONTINUED FRACTION EXPANSION (Equation 11 in ref.)
C              a .le. 1 and x .ge. 1.1, or 1 .le. a .le. big and
c              a .le. x .lt. x0, or a .gt. big and 7*a/5 .le. x .lt. x0.
C
  170 TOL = MAX(E8,4.0D0*ACC)
      B2N = X + (1.0D0 - A)
      B2NM1 = X / B2N
      A2NM1 = 1.0D0 / B2N
      A2N = A2NM1
      C = 1.0D0
  180 CONTINUE
         A2NM1 = X*A2N + C*A2NM1
         B2NM1 = X     + C*B2NM1
         C = C + 1.0D0
         T = C - A
         A2N = A2NM1 + T*A2N
         B2N = B2NM1 + T
C
         A2NM1 = A2NM1/B2N
         B2NM1 = B2NM1/B2N
         A2N = A2N/B2N
         IF (ABS(A2N - A2NM1/B2NM1) .GE. TOL*A2N) GO TO 180
C
      QANS = R*A2N
      go to 390
C
C              SPECIAL CASES
C
  330 IF (A .EQ. X) THEN
         QANS = 0.5D0
         pqerr = RTA*RT2PIN
         go to 390
      END IF
C
  331 IF (X .GT. A) GO TO 310
C
C The error magnification factor is identically zero along both axes.
  300 PANS = 0.0D0
      QANS = 1.0D0
      pqerr = 0.0
      go to 400
C
  320 IF (A .NE. 0.0D0) GO TO 300
      IF (X .eq. 0.0D0) GO TO 500
C
  310 QANS = 0.0D0
      pqerr = 0.0
c
c              Check the error committed
c
  390 PANS = 0.5D0 + (0.5D0 - QANS)
      GO TO 400
  395 QANS = 0.5D0 + (0.5D0 - PANS)
  400 pqerr = pqerr * xerr
      if (pqerr .gt. 4.0d0 * round) then
         if (ptol .gt. 0.0) then
            if (pqerr .gt. ptol * pans) go to 520
         else
            if (pqerr .gt. -ptol) go to 520
         end if
         if (qtol .gt. 0.0) then
            if (pqerr .gt. qtol * qans) go to 520
         else
            if (pqerr .gt. -qtol) go to 520
         end if
      end if
      ierr = 0
      return
C
C              ERROR RETURNS
C
  500 CALL ERMSG ('DGAMI',3,2+msgoff,'A and X are both zero','.')
      PANS = 3.0
      IERR = 3
      RETURN
  510 CALL DERM1 ('DGAMI',4,2+msgoff,'One of A or X is negative','A',A,
     1            ',')
      CALL DERV1 ('X',X,'.')
      PANS = 4.0
      IERR = 4
      RETURN
  520 call derm1 ('DGAMI',2,2+msgoff,'Error tolerances not achieved',
     1            'Absolute error',pqerr,',')
      call derv1 ('PANS',pans,',')
      call derv1 ('PTOL',ptol,',')
      call derv1 ('QANS',qans,',')
      call derv1 ('QTOL',qtol,',')
      call derv1 ('A',a,',')
      call derv1 ('X',x,'.')
      ierr = 2
      return
      end

      subroutine DGAMIK (PTOLA, QTOLA, XERRA, MSGA)
c
c     Control DGAMI tolerances and error action.
c
c PTOLA and QTOLA are tolerances for PANS and QANS.  When negative they
c     are absolute tolerances; when positive they are relative
c     tolerances; when zero they mean use the default tolerance.  The
c     default is 4 times the round-off level.
c XERRA is the relative error in X.  Negative means use the default,
c     which is one round-off.
c MSGA is the message action offset, added onto the message level for
c     each message produced by DGAMI.
c
      double precision PTOLA, QTOLA, XERRA
      integer MSGA
c
c ROUND is the round-off level.
c
      external D1MACH
      external DGAMIB
      double precision D1MACH
c
c /DGAMIC/ is used to remember and communicate the tolerance, error and
c     message action settings.
c PTOL, QTOL are tolerances for PANS and QANS, set from PTOLA and QTOLA.
c XERR is the relative error in X, set from XERRA.
c PQERR is the absolute error in the last computation of P or Q, if any,
c     else it is -1.0.
c ROUND is the round-off level.
c MSGOFF is the message action offset.
c FIRST is .TRUE. iff DGAMIK has never been called.
c
      double precision PTOL, QTOL, XERR, PQERR, ROUND
      integer MSGOFF
      logical FIRST
      save FIRST
      common /DGAMIC/ PTOL, QTOL, XERR, PQERR, ROUND, MSGOFF
      save /DGAMIC/
      data FIRST /.TRUE./
c
      if (first) then
         call dgamib
         round = d1mach(4)
         first = .false.
      end if
      if (ptola .eq. 0.0d0) then
         ptol = 4.0d0 * round
      else
         ptol = ptola
      end if
      if (qtola .eq. 0.0d0) then
         qtol = 4.0d0 * round
      else
         qtol = qtola
      end if
      if (xerra .lt. 0.0d0) then
         xerr = round
      else
         xerr = xerra
      end if
      msgoff = msga
      return
      end

      subroutine DGAMIE (PQERRV)
c
c     Return the error committed on the last call to DGAMI, if any, else
c     return -1.0
c
      double precision PQERRV
      double precision PTOL, QTOL, XERR, PQERR, ROUND
      integer MSGOFF
      logical FIRST
      save FIRST
      common /DGAMIC/ PTOL, QTOL, XERR, PQERR, ROUND, MSGOFF
      save /DGAMIC/
      data FIRST /.TRUE./
      if (first) then
        call dgamib
        first = .false.
      end if
      pqerrv = pqerr
      return
      end

      subroutine DGAMIB
      double precision PTOL, QTOL, XERR, PQERR, ROUND
      integer MSGOFF
      logical FIRST
      save FIRST
      common /DGAMIC/ PTOL, QTOL, XERR, PQERR, ROUND, MSGOFF
      save /DGAMIC/
      data FIRST /.TRUE./
      if (first) then
        PQERR = -1.0d0
        first = .false.
      end if
      end
