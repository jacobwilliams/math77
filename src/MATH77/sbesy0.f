      REAL             FUNCTION SBESY0 (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 SBESY0 Krogh  Added external statement.
C>> 1995-11-13 SBESY0 Krogh  Changes to simplify conversion to C.
C>> 1994-11-11 SBESY0 Krogh   Declared all vars.
C>> 1994-10-20 SBESY0 Krogh  Changes to use M77CON
C>> 1990-11-29 SBESY0 CLL
C>> 1985-08-02 SBESY0 Lawson  Initial code.
C JULY 1977 EDITION.  W. FULLERTON, C3, LOS ALAMOS SCIENTIFIC LAB.
C C.L.LAWSON & S.CHAN, JPL, 1984 FEB ADAPTED TO JPL MATH77 LIBRARY.
c     ------------------------------------------------------------------
c--S replaces "?": ?BESY0, ?BESJ0, ?BMP0, ?INITS, ?CSEVL, ?ERM1
c     ------------------------------------------------------------------
      EXTERNAL R1MACH, SBESJ0, SCSEVL
      INTEGER NTY0
      REAL             X, BY0CS(19), AMPL, THETA, TWODPI, XSML,
     1  Y, R1MACH, SCSEVL, SBESJ0
C
C SERIES FOR BY0        ON THE INTERVAL  0.          TO  1.60000E+01
C                                        WITH WEIGHTED ERROR   8.14E-32
C                                         LOG WEIGHTED ERROR  31.09
C                               SIGNIFICANT FIGURES REQUIRED  30.31
C                                    DECIMAL PLACES REQUIRED  31.73
C
      SAVE NTY0, XSML
C
      DATA BY0CS / -.1127783939286557321793980546028E-1,
     *  -.1283452375604203460480884531838E+0,
     *  -.1043788479979424936581762276618E+0,
     *  +.2366274918396969540924159264613E-1,
     *  -.2090391647700486239196223950342E-2,
     *  +.1039754539390572520999246576381E-3,
     *  -.3369747162423972096718775345037E-5,
     *  +.7729384267670667158521367216371E-7,
     *  -.1324976772664259591443476068964E-8,
     *  +.1764823261540452792100389363158E-10,
     *  -.1881055071580196200602823012069E-12,
     *  +.1641865485366149502792237185749E-14,
     *  -.1195659438604606085745991006720E-16,
     *  +.7377296297440185842494112426666E-19,
     *  -.3906843476710437330740906666666E-21,
     *  +.1795503664436157949829120000000E-23,
     *  -.7229627125448010478933333333333E-26,
     *  +.2571727931635168597333333333333E-28,
     *  -.8141268814163694933333333333333E-31     /
C
      DATA TWODPI / 0.636619772367581343075535053490057E0 /
      DATA NTY0, XSML / 0, 0.E0 /
C     ------------------------------------------------------------------
      IF (NTY0 .eq. 0) then
      call SINITS (BY0CS, 19, 0.1E0*R1MACH(3), NTY0)
      XSML = SQRT (4.0E0*R1MACH(3))
      endif
C
      IF (X .LE. 0.E0) THEN
        SBESY0 = 0.E0
        CALL SERM1 ('SBESY0',1,0,'X IS ZERO OR NEGATIVE','X',X,'.')
      ELSE IF (X .LE. 4.E0) THEN
        IF (X .LE. XSML) THEN
          Y = 0.E0
        ELSE
          Y = X * X
        END IF
        SBESY0 = TWODPI*LOG(0.5E0*X)*SBESJ0(X) + .375E0 +
     *            SCSEVL (.125E0*Y-1.E0, BY0CS, NTY0)
      ELSE
        CALL SBMP0 (X, AMPL, THETA)
        SBESY0 = AMPL * SIN(THETA)
      END IF
C
      RETURN
C
      END