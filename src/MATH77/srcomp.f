      real             FUNCTION SRCOMP (A, X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2001-06-18 SRCOMP Krogh  Removed blanks inside floating pt. nums.
C>> 1998-10-29 SRCOMP Krogh  Moved external statement up for mangle.
C>> 1994-11-08 CLL Edited data stmt for conversion to C.
C>> 1994-10-20 SRCOMP Krogh  Changes to use M77CON
C>> 1992-05-06 SRCOMP WVS JPL Conversion from NSWC to Math 77
c--S replaces "?": ?CSEVL, ?GAMMA, ?GAM1, ?INITS, ?RCOMP
c--   &              ?RLOG, ?XPARG
C ----------------------------------------------------------------------
C                EVALUATION OF EXP(-X)*X**A/GAMMA(A)
c ----------------------------------------------------------------------
C     RT2PIN = 1/SQRT(2*PI)
c -------------------------
      real             RT2PIN
      parameter (RT2PIN=.398942280401432677939946059934E0)
c -------------------------
      real             A, X, T, U
      external SCSEVL, SGAMMA, SGAM1, SRLOG, SXPARG, R1MACH
      real             SCSEVL, SGAMMA, SGAM1, SRLOG, SXPARG, R1MACH
      real             C(30), EMIN
      integer NTERMS
      save NTERMS, EMIN
      data NTERMS /0/
c     Coefficients for a 30 term truncated Chebyshev series for the
c     tail of the Stirling approximation for log gamma (a), i.e.
c     10/a*sum(bernoulli(2*m)/((2*m)*(2*m-1))*(10/a)^(2*m),m=1..30):
      data C /
     1  +1.6666674603806317936070839523784458533810E-1,
     2  -0.27778224326356936403488508628449409187E-4,
     3  +0.39686751449471417966822710527999178E-7,
     4  -0.148869543950843614683173607368649E-9,
     5  +0.1053394436947088748911134694152E-11,
     6  -0.12017062666093330302702077565E-13,
     7  +0.201455314614512105435465085E-15,
     8  -0.4667111367329679717841157E-17,
     9  +0.143038901715103888195513E-18, -0.5615023733620632617039E-20,
     B  +0.275468595656664717191E-21,    -0.16574673341842014242E-22,
     D  +0.1205878144005562980E-23,      -0.105029344505420423E-24,
     F  +0.10888441731031588E-25,        -0.1345007495569099E-26,
     2  +0.198172466731588E-27,          -0.35717869246269E-28,
     4  +0.7475856209303E-29,            -0.1991899701570E-29,
     6  +0.510170672127E-30,             -0.177502712854E-30,
     8  +0.43613516909E-31,              -0.17176901844E-31,
     A  +0.3387002917E-32,               -0.1399833946E-32,
     C  +0.188634194E-33,                -0.79096000E-34,
     E  +0.5378442E-35,                  -0.2252006E-35 /
c
      SRCOMP = 0.0E0
      IF (X .EQ. 0.0E0) RETURN
      IF (A .LT. 20.0E0) THEN
C
         T = A*LOG(X) - X
         IF (T .LT. SXPARG(1)) RETURN
         IF (A .LT. 1.0E0) THEN
            SRCOMP = (A*EXP(T))*(1.0E0 + SGAM1(A))
            RETURN
         END IF
         SRCOMP = EXP(T)/SGAMMA(A)
      ELSE
         U = X/A
         IF (U .EQ. 0.0E0) RETURN
         if (nterms .eq. 0) then
            call sinits (c,30,0.1e0*r1mach(4),nterms)
            emin = sxparg(1)
         end if
         T = -(scsevl ((10.0e0/a)**2, c, nterms)/a + a*srlog(u))
         IF (T .GE. emin) SRCOMP = RT2PIN * SQRT(A) * EXP(T)
      END IF
      RETURN
      END
