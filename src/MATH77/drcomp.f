      double precision FUNCTION DRCOMP (A, X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2001-06-18 DRCOMP Krogh  Removed blanks inside floating pt. nums.
C>> 1998-10-29 DRCOMP Krogh  Moved external statement up for mangle.
C>> 1994-11-08 CLL Edited data stmt for conversion to C.
C>> 1994-10-20 DRCOMP Krogh  Changes to use M77CON
C>> 1992-05-06 DRCOMP WVS JPL Conversion from NSWC to Math 77
c--D replaces "?": ?CSEVL, ?GAMMA, ?GAM1, ?INITS, ?RCOMP
c--   &              ?RLOG, ?XPARG
C ----------------------------------------------------------------------
C                EVALUATION OF EXP(-X)*X**A/GAMMA(A)
c ----------------------------------------------------------------------
C     RT2PIN = 1/SQRT(2*PI)
c -------------------------
      double precision RT2PIN
      parameter (RT2PIN=.398942280401432677939946059934D0)
c -------------------------
      double precision A, X, T, U
      external DCSEVL, DGAMMA, DGAM1, DRLOG, DXPARG, D1MACH
      double precision DCSEVL, DGAMMA, DGAM1, DRLOG, DXPARG, D1MACH
      double precision C(30), EMIN
      integer NTERMS
      save NTERMS, EMIN
      data NTERMS /0/
c     Coefficients for a 30 term truncated Chebyshev series for the
c     tail of the Stirling approximation for log gamma (a), i.e.
c     10/a*sum(bernoulli(2*m)/((2*m)*(2*m-1))*(10/a)^(2*m),m=1..30):
      data C /
     1  +1.6666674603806317936070839523784458533810D-1,
     2  -0.27778224326356936403488508628449409187D-4,
     3  +0.39686751449471417966822710527999178D-7,
     4  -0.148869543950843614683173607368649D-9,
     5  +0.1053394436947088748911134694152D-11,
     6  -0.12017062666093330302702077565D-13,
     7  +0.201455314614512105435465085D-15,
     8  -0.4667111367329679717841157D-17,
     9  +0.143038901715103888195513D-18, -0.5615023733620632617039D-20,
     B  +0.275468595656664717191D-21,    -0.16574673341842014242D-22,
     D  +0.1205878144005562980D-23,      -0.105029344505420423D-24,
     F  +0.10888441731031588D-25,        -0.1345007495569099D-26,
     2  +0.198172466731588D-27,          -0.35717869246269D-28,
     4  +0.7475856209303D-29,            -0.1991899701570D-29,
     6  +0.510170672127D-30,             -0.177502712854D-30,
     8  +0.43613516909D-31,              -0.17176901844D-31,
     A  +0.3387002917D-32,               -0.1399833946D-32,
     C  +0.188634194D-33,                -0.79096000D-34,
     E  +0.5378442D-35,                  -0.2252006D-35 /
c
      DRCOMP = 0.0D0
      IF (X .EQ. 0.0D0) RETURN
      IF (A .LT. 20.0D0) THEN
C
         T = A*LOG(X) - X
         IF (T .LT. DXPARG(1)) RETURN
         IF (A .LT. 1.0D0) THEN
            DRCOMP = (A*EXP(T))*(1.0D0 + DGAM1(A))
            RETURN
         END IF
         DRCOMP = EXP(T)/DGAMMA(A)
      ELSE
         U = X/A
         IF (U .EQ. 0.0D0) RETURN
         if (nterms .eq. 0) then
            call dinits (c,30,0.1d0*d1mach(4),nterms)
            emin = dxparg(1)
         end if
         T = -(dcsevl ((10.0d0/a)**2, c, nterms)/a + a*drlog(u))
         IF (T .GE. emin) DRCOMP = RT2PIN * SQRT(A) * EXP(T)
      END IF
      RETURN
      END
