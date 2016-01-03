      real             function SPSI (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 SPSI Krogh  Moved external statement up for mangle.
c>> 1996-04-27 SPSI Krogh  Changes to use .C. and C%%.
c>> 1994-11-28 SPSI CLL Edit long data stmt for conversion to C.
c>> 1994-11-18 SPSI WV Snyder get rid of block data
C>> 1994-11-02 SPSI Krogh  Changes to use M77CON
C>> 1994-09-01 SPSI CLL Declare all variables.
C>> 1994-05-19 SPSI WV Snyder JPL Make SP like DP using CHGTYP
C>> 1993-07-23 SPSI WV Snyder JPL Adaptation from CMLIB
c--S replaces "?": ?PSI, ?PSIC, ?CSEVL, ?ERM1, ?INITS, ?PSIK, ?ERV1
c--&               ?PSIB, ?PSIE
C JUNE 1977 EDITION.   W. FULLERTON, C3, LOS ALAMOS SCIENTIFIC LAB.
      real             BIG, X, PSICS(42), APSICS(16), AUX, PI, XBIG, Y
      external SCSEVL, R1MACH
      real             SCSEVL, R1MACH
c
      real             TOL, XERR, PSIERR, ROUND
      integer I, IERR, MSGOFF, N, NTAPSI, NTPSI
      logical FIRST
      save PSICS, APSICS, PI, NTPSI, NTAPSI, XBIG, FIRST
      common /SPSIC/ TOL, XERR, PSIERR, ROUND, IERR, MSGOFF
      save /SPSIC/
c
      data FIRST /.TRUE./
C
C The array APSCS(1:42) contains coefficients for the
C series for PSI        ON THE INTERVAL  0.          TO  1.00000E+00
C                                        WITH WEIGHTED ERROR   5.79E-32
C                                         LOG WEIGHTED ERROR  31.24
C                               SIGNIFICANT FIGURES REQUIRED  30.93
C                                    DECIMAL PLACES REQUIRED  32.05
c++ Save data by elements if ~.C.
      data PSICS(1) /   -.38057080835217921520437677667039E-1 /
      data PSICS(2) /   +.49141539302938712748204699654277E+0 /
      data PSICS(3) /   -.56815747821244730242892064734081E-1 /
      data PSICS(4) /   +.83578212259143131362775650747862E-2 /
      data PSICS(5) /   -.13332328579943425998079274172393E-2 /
      data PSICS(6) /   +.22031328706930824892872397979521E-3 /
      data PSICS(7) /   -.37040238178456883592889086949229E-4 /
      data PSICS(8) /   +.62837936548549898933651418717690E-5 /
      data PSICS(9) /   -.10712639085061849855283541747074E-5 /
      data PSICS(10) /  +.18312839465484165805731589810378E-6 /
      data PSICS(11) /  -.31353509361808509869005779796885E-7 /
      data PSICS(12) /  +.53728087762007766260471919143615E-8 /
      data PSICS(13) /  -.92116814159784275717880632624730E-9 /
      data PSICS(14) /  +.15798126521481822782252884032823E-9 /
      data PSICS(15) /  -.27098646132380443065440589409707E-10 /
      data PSICS(16) /  +.46487228599096834872947319529549E-11 /
      data PSICS(17) /  -.79752725638303689726504797772737E-12 /
      data PSICS(18) /  +.13682723857476992249251053892838E-12 /
      data PSICS(19) /  -.23475156060658972717320677980719E-13 /
      data PSICS(20) /  +.40276307155603541107907925006281E-14 /
      data PSICS(21) /  -.69102518531179037846547422974771E-15 /
      data PSICS(22) /  +.11856047138863349552929139525768E-15 /
      data PSICS(23) /  -.20341689616261559308154210484223E-16 /
      data PSICS(24) /  +.34900749686463043850374232932351E-17 /
      data PSICS(25) /  -.59880146934976711003011081393493E-18 /
      data PSICS(26) /  +.10273801628080588258398005712213E-18 /
      data PSICS(27) /  -.17627049424561071368359260105386E-19 /
      data PSICS(28) /  +.30243228018156920457454035490133E-20 /
      data PSICS(29) /  -.51889168302092313774286088874666E-21 /
      data PSICS(30) /  +.89027730345845713905005887487999E-22 /
      data PSICS(31) /  -.15274742899426728392894971904000E-22 /
      data PSICS(32) /  +.26207314798962083136358318079999E-23 /
      data PSICS(33) /  -.44964642738220696772598388053333E-24 /
      data PSICS(34) /  +.77147129596345107028919364266666E-25 /
      data PSICS(35) /  -.13236354761887702968102638933333E-25 /
      data PSICS(36) /  +.22709994362408300091277311999999E-26 /
      data PSICS(37) /  -.38964190215374115954491391999999E-27 /
      data PSICS(38) /  +.66851981388855302310679893333333E-28 /
      data PSICS(39) /  -.11469986654920864872529919999999E-28 /
      data PSICS(40) /  +.19679385886541405920515413333333E-29 /
      data PSICS(41) /  -.33764488189750979801907200000000E-30 /
      data PSICS(42) /  +.57930703193214159246677333333333E-31 /
c
C The array APSICS(1:16) contains coefficients for the
C series for APSI       ON THE INTERVAL  0.          TO  1.00000E-02
C                                        WITH WEIGHTED ERROR   7.75E-33
C                                         LOG WEIGHTED ERROR  32.11
C                               SIGNIFICANT FIGURES REQUIRED  28.88
C                                    DECIMAL PLACES REQUIRED  32.71
c
      data APSICS / -.832710791069290760174456932269E-3 ,
     *              -.416251842192739352821627121990E-3 ,
     *              +.103431560978741291174463193961E-6 ,
     *              -.121468184135904152987299556365E-9 ,
     *              +.311369431998356155521240278178E-12,
     *              -.136461337193177041776516100945E-14,
     *              +.902051751315416565130837974000E-17,
     *              -.831542997421591464829933635466E-19,
     *              +.101224257073907254188479482666E-20,
     *              -.156270249435622507620478933333E-22,
     *              +.296542716808903896133226666666E-24,
     *              -.674686886765702163741866666666E-26,
     *              +.180345311697189904213333333333E-27,
     *              -.556901618245983607466666666666E-29,
     *              +.195867922607736251733333333333E-30,
     *              -.775195892523335680000000000000E-32 /
C

      data PI / 3.14159265358979323846264338327950E0 /
      data NTPSI, NTAPSI, XBIG / 0, 0, 0.0E0 /
C
      if (first) then
         call spsik (0.0e0, -1.0e0, 0)
         first = .false.
      end if
      IF (NTPSI.EQ.0) THEN
         BIG = R1MACH(3)
         CALL SINITS (PSICS, 42, 0.1E0*BIG,NTPSI)
         CALL SINITS (APSICS, 16, 0.1E0*BIG,NTAPSI)
         XBIG = 1.0E0/SQRT(BIG)
      END IF
C
      ierr = 0
      Y = ABS(X)
C
      IF (Y.LE.10.0E0) THEN
C
C SPSI(X) FOR ABS(X) .LE. 10
C
         IF (X .EQ. 0.0E0) THEN
            ierr = 1
            CALL ERMSG ('SPSI',ierr,2+MSGOFF,'X is 0.0','.')
            SPSI = 0.0E0
            psierr = -1.0e0
            RETURN
         END IF
         N = X
         IF (X .LT. 0.0E0) THEN
            IF (X .EQ. N) go to 100
            N = N - 1
         END IF
         Y = X - N
         N = N - 1
         SPSI = SCSEVL (Y + Y - 1.0E0, PSICS, NTPSI)
         IF (N.EQ.0) THEN
            PSIERR = ROUND
            RETURN
         END IF
C
         IF (N.LE.0) THEN
C
            N = -N
C
            DO 10 I = 0, N - 1
               SPSI = SPSI - 1.0E0/(X+I)
   10       continue
         ELSE
C
C SPSI(X) FOR X .GE. 2.0 AND X .LE. 10.0
C
            DO 20 I = 1, N
              SPSI = SPSI + 1.0E0/(Y+I)
   20       continue
         END IF
         if (x .lt. -0.5E0) then
            n = x + x
            if (x .ne. n) then
c              X is not a half-integer (X can't be integer here).
               y = pi / tan(pi * x)
               go to 50
            end if
         end if
         psierr = round
         return
      END IF
C
C SPSI(X) FOR ABS(X) .GT. 10.0
C
      if (x .lt. 0.0E0) then
         n = x
         if (x .eq. n) go to 100
      end if
      IF (Y.LT.XBIG) THEN
         AUX = SCSEVL (2.0E0*(10.0E0/Y)**2-1.0E0, APSICS, NTAPSI)
      ELSE
         AUX = 0.0E0
      END IF
      if (x .gt. 0.0E0) then
         SPSI = LOG(X) - 0.5e0/X + AUX
         psierr = round
         return
      end if
      n = x + x
      if (x .ne. n) then
c        X is not a half-integer (X can't be integer here).
         y = pi / tan(pi * x)
      else
         y = 0.0e0
      end if
      SPSI = LOG(ABS(X)) - 0.5e0/X + AUX - Y
c
c Error estimate when Psi might be going to infinity or zero
50    psierr = max(round, abs(x * xerr * y * y / spsi))
      if (psierr .gt. tol) go to 200
      return
C
100   ierr = 2
      CALL SERM1 ('SPSI',ierr,2+MSGOFF,'X is a negative integer','X',X,
     1   '.')
      SPSI = 0.0e0
      psierr = -1.0e0
      RETURN
c
200   ierr = -3
      CALL ERMSG ('SPSI',ierr,1+MSGOFF,'Desired precision not achieved b
     1ecause X is too near a negative integer',',')
      CALL ERMOR ('or a zero of PSI(x)',',')
      call serv1 ('TOL',tol,',')
      call serv1 ('ERR',psierr,',')
      call serv1 ('X',X,'.')
      return
C
      END

      subroutine SPSIK (TOLI, XERRI, MSGOFI)
      real             TOLI, XERRI
      integer MSGOFI
      logical FIRST
      save FIRST
      external R1MACH, SPSIB
      real             R1MACH
      real             TOL, XERR, PSIERR, ROUND
      integer IERR, MSGOFF
      common /SPSIC/ TOL, XERR, PSIERR, ROUND, IERR, MSGOFF
      save /SPSIC/
      data FIRST /.TRUE./
      if (first) then
         call spsib
         round = r1mach(4)
         tol = sqrt(round)
         first = .false.
      end if
      if (toli .le. 0.0e0) then
         tol = sqrt(round)
      else
         tol = max(toli, round)
      end if
      if (xerri .lt. 0.0e0) then
         xerr = round
      else
         xerr = xerri
      end if
      msgoff = msgofi
      return
      end

      subroutine SPSIE (ERR, IERFLG)
      real             ERR
      integer IERFLG
      logical FIRST
      save FIRST
      real             TOL, XERR, PSIERR, ROUND
      integer IERR, MSGOFF
      common /SPSIC/ TOL, XERR, PSIERR, ROUND, IERR, MSGOFF
      save /SPSIC/
      data FIRST /.TRUE./
      if (first) then
         call spsib
         first = .false.
      end if
      err = psierr
      ierflg = ierr
      return
      end

      subroutine SPSIB
      logical FIRST
      save FIRST
      real             TOL, XERR, PSIERR, ROUND
      integer IERR, MSGOFF
      common /SPSIC/ TOL, XERR, PSIERR, ROUND, IERR, MSGOFF
      save /SPSIC/
      data FIRST /.TRUE./
      if (first) then
         first = .false.
         ierr = 0
         psierr = 0.0
         tol = -1.0e0
      end if
      end
