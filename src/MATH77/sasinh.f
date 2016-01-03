      REAL             FUNCTION SASINH(X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2001-05-25 SASINH  Krogh Minor change for making .f90 version.
C>> 1996-04-01 SASINH Krogh  Converted to Fortran 77 from SFTRAN.
C>> 1995-10-24 SASINH Krogh  Removed blanks in numbers for C conversion.
C>> 1994-10-19 SASINH Krogh  Changes to use M77CON
C>> 1994-04-19 SASINH CLL Edited type stmts to make DP & SP similar.
C>> 1992-03-13 SASINH FTK  Removed implicit statements.
C>> 1986-03-18 SASINH Lawson  Initial code.
C--S replaces "?": ?ACOSH,?ACSCH,?ACTNH,?ASECH,?ASINH,?ATANH
C--&               ?HYPC, ?HYPS, ?HYPH, ?HYPER, ?ERM1
C
C     This program unit has six entry points for computing
C     the six inverse hyperbolic functions.
C     Using the SFTRAN3 preprocessor.
C     Uses R1MACH(3) to obtain machine precision.
C     All critical constants are given correctly rounded
C     to 25 decimal places. Accuracy of the subprogram
C     adapts to the machine precision up to 25 decimal
C     places precision.
C
C     -----------------------------------------------------------
C
C--   Begin mask code changes
C     The single precision code was tested in May 1983, on a
C     Univac 1100 using the JPL program ST4 that compares a
C     single precision function with a double precision function.
C     Each function was tested on at least 6000 points. Max
C     relative errors are expressed as a multiple of R1MACH(3),
C     which for the Univac 1100 is 2**(-27) = 0.745E-8 .
C
C     FUNCTION            RANGE                MAX REL ERROR
C     --------            -----                -------------
C      SASINH       ALL X                           2.4
C
C      SACOSH       1.0 .LE. X .LE. 1.03            LARGE
C                   1.03 .LE. X .LE. 1.21           3.4
C                   1.21 .LE. X                     2.3
C
C      SATANH       ABS(X) .LE. 0.44                3.2
C                   0.44 .LE. ABS(X) .LE. 0.92      5.1
C                   0.92 .LE. ABS(X) .LE. 1.0       LARGE
C
C      SACSCH       ABS(X) .NE. 0                   3.4
C
C      SASECH       0.0 .LT. X .LE. 0.24            2.0
C                   0.24 .LE. X .LE. 0.68           3.0
C                   0.68 .LE. X .LE. 0.88           10.0
C                   0.88 .LE. X .LE. 1.0            LARGE
C
C      SACTNH       1.0 .LE. ABS(X) .LE. 1.16       LARGE
C                   1.16 .LE. ABS(X) .LE. 2.20      7.1
C                   2.20 LE. ABS(X)                 3.7
C
C     -----------------------------------------------------------
C
C     The D.P. and S.P. codes have each been tested using
C     identities such as X - SINH( ASINH(X) ) = 0. The
C     D.P. codes have about the same accuracy relative to
C     D1MACH(3) as do the S.P. codes relative to R1MACH(3).
C
C--   End mask code changes
C     Error Messages
C     1) Arg .LT. 1                        ( IN SACOSH )
C     2) Arg .LT. or .EQ. to 1             ( IN SATANH )
C     3) ABS(Arg) .LT. or .EQ. to 1        ( IN SACTNH )
C     4) Arg .EQ. 0                        ( IN SACSCH )
C     5) Arg .LT. or .EQ. 0 or Arg .GT. 1  ( IN SASECH )
C
C     -----------------------------------------------------------
C
C     C.L.Lawson and Stella Chan,JPL,Feb 23,1983.
C
      REAL             X
      EXTERNAL SHYPS
      REAL             SHYPS
C
C     Begin computation for SASINH(x)
C     Defined for all x.The value u ranges from negative infinity to
C     positive infinity as x ranges from negative infinity to positive i

C
      IF (X .EQ. 0.E0) THEN
        SASINH=0.E0
      ELSE
        SASINH = SIGN(SHYPS(ABS(X)), X)
      END IF
      RETURN
      END
C
C
      REAL             FUNCTION SACOSH(X)
C
C     Defined (float -valued) for all X greater than or equal to 1.
C     We compute the nonnegative value U ranging from zero to infinity
C     as X ranges from 1 to infinity.The second value would be -U.
      REAL             X
      EXTERNAL SHYPC
      REAL             SHYPC
C
      IF (X .LT. 1.E0) THEN
        SACOSH=0.E0
        CALL SERM1('SACOSH',1,0,'ARG.LT.1','ARG',X,'.')
      ELSE
        SACOSH = SHYPC(X)
      END IF
      RETURN
      END
C
C
      REAL             FUNCTION SATANH(X)
C
C     Defined for -1 < x < +1. The value U ranges from negative
C     infinity to positive infinity.
      REAL             X
      EXTERNAL SHYPH
      REAL             SHYPH
C
      IF (ABS(X) .GE. 1.E0) THEN
        SATANH=0.E0
        CALL SERM1('SATANH',2,0,'ABS(ARG).GE.1','ARG',X,'.')
      ELSE IF (X .EQ. 0.E0) THEN
        SATANH=0.E0
      ELSE
        SATANH = SIGN(SHYPH(ABS(X)), X)
      END IF
      RETURN
      END
C
C
      REAL             FUNCTION SACTNH(X)
C
C     Defined for ABS(X) > 1. The value U ranges from zero to negative
C     infinity as X ranges from negative infinity to -1,and from
C     positive infinity to zero as X ranges from 1 to positive
C     infinity.
      REAL             X
      EXTERNAL SHYPH
      REAL             SHYPH
C
      IF (ABS(X) .LE. 1.E0) THEN
        SACTNH=0.E0
        CALL SERM1('SACTNH',3,0,'ABS(ARG).LE.1','ARG',X,'.')
      ELSE
        SACTNH = SIGN(SHYPH(1.E0 / ABS(X)), X)
      END IF
      RETURN
      END
C
C
      REAL             FUNCTION SACSCH(X)
C
C     Defined for all X not equal to 0. The value varies from
C     zero to negative infinity as X ranges from negative
C     infinity to zero,jumps from negative infinity to positive
C     infinity at zero, and varies from positive infinity to
C     zero as X ranges from zero to positive infinity.
      REAL             X
      EXTERNAL SHYPS
      REAL             SHYPS
C
      IF (X .EQ. 0.E0) THEN
        SACSCH=0.E0
        CALL SERM1('SACSCH',4,0,'ARG.EQ.0','ARG',X,'.')
      ELSE
        SACSCH = SIGN(SHYPS(1.E0 / ABS(X)), X)
      END IF
      RETURN
      END
C
C
      REAL             FUNCTION SASECH(X)
C
C     Defined  (float  valued) for X greater than zero and X less than
C     or equal to 1.E0. We compute the nonnegative value that varies
C     from infinity to zero as X varies from zero to 1.E0. The other
C     value would be the negative of this.
      REAL             X
      EXTERNAL SHYPC
      REAL             SHYPC
C
      IF (X .LE. 0.E0 .OR. X .GT. 1.E0) THEN
        SASECH=0.E0
        CALL SERM1('SASECH',5,0,'ARG.GT.1 OR ARG.LE.0','ARG',X,'.')
      ELSE
        SASECH = SHYPC(1.E0 / X)
      END IF
      RETURN
      END
C
C
      REAL             FUNCTION SHYPS(P)
C
C     Here P > 0.
C     We break the p range into three intervals separated by CUT2
C     and HICUT. In the middle range between CUT2 and HICUT we use
C           SHYPS=LOG(P+SQRT(1+P**2))
C     CUT2 is set to 1.176 to keep the argument of LOG() greater
C     than e = 2.718 to avoid amplification of relative error by LOG().
C       We set HICUT = 10.**16 assuming that all machines on which
C     this code is to run have overflow limit greater than 10.**32
C     and precision not greater than 32 decimal places. Thus we
C     assume that HICUT**2 would not overflow and HICUT**2 plus or
C     minus 1.E0 would evaluate to HICUT**2.
C     The idea of using SHYPS = LOG(P) + LOG(2) for P .GT. HICUT is
C     copied from an ASINH subroutine due to WAYNE FULLERTON.
C
      REAL             P
      EXTERNAL SHYPER
      REAL             CU, CUT2, SHYPER, HICUT, LOGE2
      DATA CUT2, HICUT / 1.176E0, 1.E16 /
      DATA LOGE2 / 0.6931471805599453094172321E0 /
C
      IF (P .GT. HICUT) THEN
        SHYPS = LOG(P) + LOGE2
      ELSE
        CU=SQRT(1.E0+P**2)
        IF (P .LT. CUT2) THEN
          SHYPS = SHYPER(P, CU)
        ELSE
          SHYPS = LOG(P + CU)
        END IF
      END IF
      RETURN
      END
C
C
      REAL             FUNCTION SHYPC(P)
C
C     Here P .GE. 1.
C     See comments in function SHYPS
C     The mid-range formula for SACOSH is:
C         U=LOG(P+SQRT(P**2-1))
C
      REAL             P
      EXTERNAL SHYPER
      REAL             CUT2, SHYPER, HICUT, LOGE2, SU
      DATA CUT2, HICUT / 1.176E0, 1.E16 /
      DATA LOGE2 / 0.6931471805599453094172321E0 /
C
      IF (P .GT. HICUT) THEN
        SHYPC = LOG(P) + LOGE2
      ELSE
        SU = SQRT( (P-1.E0)*(P+1.E0) )
        IF (SU .LT. CUT2) THEN
          SHYPC = SHYPER(SU, P)
        ELSE
          SHYPC = LOG(SU + P)
        END IF
      END IF
      RETURN
      END
C
C
      REAL             FUNCTION SHYPH(Q)
C
C     Here Q satisfies 0 < Q < 1.
C     When Q exceeds CUT1 = 0.463 then (1+Q)/(1-Q) > e = 2.718,
C     and this is large enough to be used as an argument
C     to LOG() without amplification of relative error.
C     When Q is less than CUT1 we transform Q to COSH(U)
C     and SINH(U) and use procedure (LOW RANGE).
C
C     The simple formulas for CU and SU would be
C           CU = 1.E0 / SQRT(1.E0 - Q**2)
C     and   SU = Q / SQRT(1.E0 - Q**2)
C     The more complicated formulas used here have less
C     accumulation of round-of error since
C     0.0 .LT. TERM .LT. 0.1283
C
      REAL             Q
      EXTERNAL SHYPER
      REAL             CUT1, SHYPER, DIF, QSQ, TERM
      DATA CUT1 / 0.463E0 /
C
      IF (Q .LT. CUT1) THEN
        QSQ = Q * Q
        DIF = 1.E0 - QSQ
        TERM = QSQ / (DIF + SQRT(DIF))
        SHYPH = SHYPER(Q + Q*TERM, 1.E0 + TERM)
      ELSE
        SHYPH = 0.5E0 * LOG((1.E0+Q)/(1.E0-Q))
      END IF
      RETURN
      END
C
C  do (LOW RANGE) ==> DYPER(SU, CU)
C
      REAL             FUNCTION SHYPER(SU, CU)
C
C     This procedure controls computation of U, given
C     SU=SINH(U) and CU=COSH(U).These will satisfy
C          0 .LE. SU .LE. 1.176
C     AND  0 .LE. CU .LE. 1.5437
C     so the result, U , will satisfy
C          0 .LE. U .LE. 1.00052
C
C
C     Here the argument R satisfies 0 .LE. R .LE. SINH(0.125) = 0.125326
C
C     ------------------------------------------------------------------
C
C     On the first call to this procedure the value NMAX
C     will be set and saved.
C     FOR MACHINE PRECISION BETWEEN  4.43062 AND  6.45985 USE NMAX =  2
C     FOR MACHINE PRECISION BETWEEN  6.45985 AND  8.43090 USE NMAX =  3
C     FOR MACHINE PRECISION BETWEEN  8.43090 AND 10.36773 USE NMAX =  4
C     FOR MACHINE PRECISION BETWEEN 10.36773 AND 12.28199 USE NMAX =  5
C     FOR MACHINE PRECISION BETWEEN 12.28199 AND 14.18024 USE NMAX =  6
C     FOR MACHINE PRECISION BETWEEN 14.18024 AND 16.06654 USE NMAX =  7
C     FOR MACHINE PRECISION BETWEEN 16.06654 AND 17.94359 USE NMAX =  8
C     FOR MACHINE PRECISION BETWEEN 17.94359 AND 19.81325 USE NMAX =  9
C     FOR MACHINE PRECISION BETWEEN 19.81325 AND 21.67688 USE NMAX = 10
C     FOR MACHINE PRECISION BETWEEN 21.67688 AND 23.53550 USE NMAX = 11
C     FOR MACHINE PRECISION BETWEEN 23.53550 AND 25.38987 USE NMAX = 12
C
      REAL             SU, CU
      EXTERNAL R1MACH
      REAL             B1,B2,C(0:13),CV(8),R1MACH,R,RSQ,SV(8),U,V(8)
      INTEGER I, II, NMAX
      LOGICAL FIRST
      SAVE FIRST, NMAX
C
C     Coeffs for SASINH series. C(0) is half of its true value.
C     This series will be used only for arguments, R , in the
C     range 0 .LE. R .LE. SINH(0.125) = 0.125326 .
C
      DATA   C( 0)/0.5E0/
      DATA   C( 1) /-.1666666666666666666666667E+00/
      DATA   C( 2) /+.7500000000000000000000000E-01/
      DATA   C( 3) /-.4464285714285714285714286E-01/
      DATA   C( 4) /+.3038194444444444444444444E-01/
      DATA   C( 5) /-.2237215909090909090909091E-01/
      DATA   C( 6) /+.1735276442307692307692308E-01/
      DATA   C( 7) /-.1396484375000000000000000E-01/
      DATA   C( 8) /+.1155180089613970588235294E-01/
      DATA   C( 9) /-.9761609529194078947368421E-02/
      DATA   C(10) /+.8390335809616815476190476E-02/
      DATA   C(11) /-.7312525873598845108695652E-02/
      DATA   C(12) /+.6447210311889648437500000E-02/
      DATA   C(13) /-.5740037670841923466435185E-02/
C
C     SV(I) = SINH(V(I))
C     CV(I) = COSH(V(I))
C
      DATA   V( 1) /+.1250000000000000000000000E+00/
      DATA  SV( 1) /+.1253257752411154569820575E+00/
      DATA  CV( 1) /+.1007822677825710859846950E+01/
      DATA   V( 2) /+.2500000000000000000000000E+00/
      DATA  SV( 2) /+.2526123168081683079141252E+00/
      DATA  CV( 2) /+.1031413099879573176159295E+01/
      DATA   V( 3) /+.3750000000000000000000000E+00/
      DATA  SV( 3) /+.3838510679136145687542957E+00/
      DATA  CV( 3) /+.1071140346704586767299498E+01/
      DATA   V( 4) /+.5000000000000000000000000E+00/
      DATA  SV( 4) /+.5210953054937473616224256E+00/
      DATA  CV( 4) /+.1127625965206380785226225E+01/
      DATA   V( 5) /+.6250000000000000000000000E+00/
      DATA  SV( 5) /+.6664922644566160822726066E+00/
      DATA  CV( 5) /+.1201753692975606324229229E+01/
      DATA   V( 6) /+.7500000000000000000000000E+00/
      DATA  SV( 6) /+.8223167319358299807036616E+00/
      DATA  CV( 6) /+.1294683284676844687841708E+01/
      DATA   V( 7) /+.8750000000000000000000000E+00/
      DATA  SV( 7) /+.9910066371442947560531743E+00/
      DATA  CV( 7) /+.1407868656822803158638471E+01/
      DATA   V( 8) /+.1000000000000000000000000E+01/
      DATA  SV( 8) /+.1175201193643801456882382E+01/
      DATA  CV( 8) /+.1543080634815243778477906E+01/
      DATA B1,B2 / 0.52344E0, 4.4306E0 /
c
      data FIRST / .true. /
c
      IF (FIRST) THEN
        FIRST = .FALSE.
C     The following linear expression approximate the NMAX
C     selection criteria described above. The expression
C     never sets NMAX too small but will set NMAX too
C     large, by one, in boundary cases.
        NMAX = 2 + INT(B1 * (-log10(R1MACH(3)) - B2))
        NMAX = MAX(2,MIN(NMAX,12))
      END IF
c
      IF (SU .LE. SV(1)) THEN
        R = SU
      ELSE
        II = MIN ( 7,INT(SU/SV(1)) )
        IF (SU .LT. SV(II)) II = II - 1
C
C     Here we transform to an argument, R , in the range
C     0 .LE. R .LE. SINH(0.125). The simple formula would
C     be :     R = SU * CV(II) - SV(II) * CU
C     However we transform this to extract the factor
C     (SU - SV(II)) for better control of round-off error.
C
        R = (SU-SV(II))*(CV(II)-(SV(II)+SU)*SV(II)/(CV(II)+CU))
      END IF
C
      RSQ = R*R
      U = C(NMAX)
      DO 110 I = NMAX-1, 0, -1
        U = RSQ * U  + C(I)
  110 CONTINUE
      SHYPER = R * (U + 0.5E0)
      IF (SU .GT. SV(1)) SHYPER = V(II) + SHYPER
      return
      end
