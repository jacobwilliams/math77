      REAL             FUNCTION SBESJ1 (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 SBESJ1 Krogh  Added external statement.
C>> 1995-11-13 SBESJ1 Krogh  Changed data statment for C converstion.
C>> 1995-11-03 SBESJ1 Krogh  Removed blanks in numbers for C conversion.
C>> 1994-11-11 SBESJ1 Krogh   Declared all vars.
C>> 1994-10-20 SBESJ1 Krogh  Changes to use M77CON
C>> 1992-02-07 SBESJ1 WV Snyder Correct sign for X < 0.
C>> 1990-11-29 SBESJ1 CLL
C>> 1985-08-02 SBESJ1 Lawson  Initial code.
C JUNE 1978 EDITION.  W. FULLERTON, C3, LOS ALAMOS SCIENTIFIC LAB.
C C.L.LAWSON & S.CHAN, JPL, 1984 FEB ADAPTED TO JPL MATH77 LIBRARY.
c     ------------------------------------------------------------------
c--S replaces "?": ?BESJ1, ?BMP1, ?INITS, ?CSEVL
c     ------------------------------------------------------------------
      EXTERNAL R1MACH, SCSEVL
      INTEGER NTJ1
      REAL             X, BJ1CS(19), AMPL, THETA, XSML, XMIN, Y,
     1  R1MACH, SCSEVL
C
C SERIES FOR BJ1        ON THE INTERVAL  0.          TO  1.60000E+01
C                                        WITH WEIGHTED ERROR   1.16E-33
C                                         LOG WEIGHTED ERROR  32.93
C                               SIGNIFICANT FIGURES REQUIRED  32.36
C                                    DECIMAL PLACES REQUIRED  33.57
C
      SAVE NTJ1, XSML, XMIN
C
      DATA BJ1CS / -.117261415133327865606240574524003E+0,
     *  -.253615218307906395623030884554698E+0,
     *  +.501270809844695685053656363203743E-1,
     *  -.463151480962508191842619728789772E-2,
     *  +.247996229415914024539124064592364E-3,
     *  -.867894868627882584521246435176416E-5,
     *  +.214293917143793691502766250991292E-6,
     *  -.393609307918317979229322764073061E-8,
     *  +.559118231794688004018248059864032E-10,
     *  -.632761640466139302477695274014880E-12,
     *  +.584099161085724700326945563268266E-14,
     *  -.448253381870125819039135059199999E-16,
     *  +.290538449262502466306018688000000E-18,
     *  -.161173219784144165412118186666666E-20,
     *  +.773947881939274637298346666666666E-23,
     *  -.324869378211199841143466666666666E-25,
     *  +.120223767722741022720000000000000E-27,
     *  -.395201221265134933333333333333333E-30,
     *  +.116167808226645333333333333333333E-32 /
C
      DATA NTJ1, XSML, XMIN / 0, 2*0.E0 /
C     ------------------------------------------------------------------
      IF (NTJ1 .eq. 0) then
         call SINITS (BJ1CS, 19, 0.1E0*R1MACH(3), NTJ1)
         XSML = SQRT (4.0E0*R1MACH(3))
         XMIN = 2.0E0*R1MACH(1)
      endif
C
      Y = ABS(X)
C
      IF (Y .LE. XMIN) THEN
        SBESJ1 = 0.E0
      ELSE IF (Y .LE. XSML) THEN
        SBESJ1 = .5E0 * X
      ELSE IF (Y .LE. 4.E0) THEN
        SBESJ1 = X * ( .25E0 + SCSEVL (.125E0*Y*Y-1.E0,BJ1CS, NTJ1) )
      ELSE
        CALL SBMP1 (Y, AMPL, THETA)
        SBESJ1 = AMPL * COS(THETA)
        IF (X .LT. 0.0e0) SBESJ1 = -SBESJ1
      END IF
C
      RETURN
      END
