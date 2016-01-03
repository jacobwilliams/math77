      double precision function DPSI (X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 DPSI Krogh  Moved external statement up for mangle.
c>> 1996-04-27 DPSI Krogh  Changes to use .C. and C%%.
c>> 1994-11-28 DPSI CLL Edit long data stmt for conversion to C.
c>> 1994-11-18 DPSI WV Snyder get rid of block data
C>> 1994-11-02 DPSI Krogh  Changes to use M77CON
C>> 1994-09-01 DPSI CLL Declare all variables.
C>> 1994-05-19 DPSI WV Snyder JPL Make SP like DP using CHGTYP
C>> 1993-07-23 DPSI WV Snyder JPL Adaptation from CMLIB
c--D replaces "?": ?PSI, ?PSIC, ?CSEVL, ?ERM1, ?INITS, ?PSIK, ?ERV1
c--&               ?PSIB, ?PSIE
C JUNE 1977 EDITION.   W. FULLERTON, C3, LOS ALAMOS SCIENTIFIC LAB.
      double precision BIG, X, PSICS(42), APSICS(16), AUX, PI, XBIG, Y
      external DCSEVL, D1MACH
      double precision DCSEVL, D1MACH
c
      double precision TOL, XERR, PSIERR, ROUND
      integer I, IERR, MSGOFF, N, NTAPSI, NTPSI
      logical FIRST
      save PSICS, APSICS, PI, NTPSI, NTAPSI, XBIG, FIRST
      common /DPSIC/ TOL, XERR, PSIERR, ROUND, IERR, MSGOFF
      save /DPSIC/
c
      data FIRST /.TRUE./
C
C The array APSCS(1:42) contains coefficients for the
C series for PSI        ON THE INTERVAL  0.          TO  1.00000D+00
C                                        WITH WEIGHTED ERROR   5.79D-32
C                                         LOG WEIGHTED ERROR  31.24
C                               SIGNIFICANT FIGURES REQUIRED  30.93
C                                    DECIMAL PLACES REQUIRED  32.05
c++ Save data by elements if ~.C.
      data PSICS(1) /   -.38057080835217921520437677667039D-1 /
      data PSICS(2) /   +.49141539302938712748204699654277D+0 /
      data PSICS(3) /   -.56815747821244730242892064734081D-1 /
      data PSICS(4) /   +.83578212259143131362775650747862D-2 /
      data PSICS(5) /   -.13332328579943425998079274172393D-2 /
      data PSICS(6) /   +.22031328706930824892872397979521D-3 /
      data PSICS(7) /   -.37040238178456883592889086949229D-4 /
      data PSICS(8) /   +.62837936548549898933651418717690D-5 /
      data PSICS(9) /   -.10712639085061849855283541747074D-5 /
      data PSICS(10) /  +.18312839465484165805731589810378D-6 /
      data PSICS(11) /  -.31353509361808509869005779796885D-7 /
      data PSICS(12) /  +.53728087762007766260471919143615D-8 /
      data PSICS(13) /  -.92116814159784275717880632624730D-9 /
      data PSICS(14) /  +.15798126521481822782252884032823D-9 /
      data PSICS(15) /  -.27098646132380443065440589409707D-10 /
      data PSICS(16) /  +.46487228599096834872947319529549D-11 /
      data PSICS(17) /  -.79752725638303689726504797772737D-12 /
      data PSICS(18) /  +.13682723857476992249251053892838D-12 /
      data PSICS(19) /  -.23475156060658972717320677980719D-13 /
      data PSICS(20) /  +.40276307155603541107907925006281D-14 /
      data PSICS(21) /  -.69102518531179037846547422974771D-15 /
      data PSICS(22) /  +.11856047138863349552929139525768D-15 /
      data PSICS(23) /  -.20341689616261559308154210484223D-16 /
      data PSICS(24) /  +.34900749686463043850374232932351D-17 /
      data PSICS(25) /  -.59880146934976711003011081393493D-18 /
      data PSICS(26) /  +.10273801628080588258398005712213D-18 /
      data PSICS(27) /  -.17627049424561071368359260105386D-19 /
      data PSICS(28) /  +.30243228018156920457454035490133D-20 /
      data PSICS(29) /  -.51889168302092313774286088874666D-21 /
      data PSICS(30) /  +.89027730345845713905005887487999D-22 /
      data PSICS(31) /  -.15274742899426728392894971904000D-22 /
      data PSICS(32) /  +.26207314798962083136358318079999D-23 /
      data PSICS(33) /  -.44964642738220696772598388053333D-24 /
      data PSICS(34) /  +.77147129596345107028919364266666D-25 /
      data PSICS(35) /  -.13236354761887702968102638933333D-25 /
      data PSICS(36) /  +.22709994362408300091277311999999D-26 /
      data PSICS(37) /  -.38964190215374115954491391999999D-27 /
      data PSICS(38) /  +.66851981388855302310679893333333D-28 /
      data PSICS(39) /  -.11469986654920864872529919999999D-28 /
      data PSICS(40) /  +.19679385886541405920515413333333D-29 /
      data PSICS(41) /  -.33764488189750979801907200000000D-30 /
      data PSICS(42) /  +.57930703193214159246677333333333D-31 /
c
C The array APSICS(1:16) contains coefficients for the
C series for APSI       ON THE INTERVAL  0.          TO  1.00000D-02
C                                        WITH WEIGHTED ERROR   7.75D-33
C                                         LOG WEIGHTED ERROR  32.11
C                               SIGNIFICANT FIGURES REQUIRED  28.88
C                                    DECIMAL PLACES REQUIRED  32.71
c
      data APSICS / -.832710791069290760174456932269D-3 ,
     *              -.416251842192739352821627121990D-3 ,
     *              +.103431560978741291174463193961D-6 ,
     *              -.121468184135904152987299556365D-9 ,
     *              +.311369431998356155521240278178D-12,
     *              -.136461337193177041776516100945D-14,
     *              +.902051751315416565130837974000D-17,
     *              -.831542997421591464829933635466D-19,
     *              +.101224257073907254188479482666D-20,
     *              -.156270249435622507620478933333D-22,
     *              +.296542716808903896133226666666D-24,
     *              -.674686886765702163741866666666D-26,
     *              +.180345311697189904213333333333D-27,
     *              -.556901618245983607466666666666D-29,
     *              +.195867922607736251733333333333D-30,
     *              -.775195892523335680000000000000D-32 /
C

      data PI / 3.14159265358979323846264338327950D0 /
      data NTPSI, NTAPSI, XBIG / 0, 0, 0.0D0 /
C
      if (first) then
         call dpsik (0.0d0, -1.0d0, 0)
         first = .false.
      end if
      IF (NTPSI.EQ.0) THEN
         BIG = D1MACH(3)
         CALL DINITS (PSICS, 42, 0.1D0*BIG,NTPSI)
         CALL DINITS (APSICS, 16, 0.1D0*BIG,NTAPSI)
         XBIG = 1.0D0/SQRT(BIG)
      END IF
C
      ierr = 0
      Y = ABS(X)
C
      IF (Y.LE.10.0D0) THEN
C
C DPSI(X) FOR ABS(X) .LE. 10
C
         IF (X .EQ. 0.0D0) THEN
            ierr = 1
            CALL ERMSG ('DPSI',ierr,2+MSGOFF,'X is 0.0','.')
            DPSI = 0.0D0
            psierr = -1.0d0
            RETURN
         END IF
         N = X
         IF (X .LT. 0.0D0) THEN
            IF (X .EQ. N) go to 100
            N = N - 1
         END IF
         Y = X - N
         N = N - 1
         DPSI = DCSEVL (Y + Y - 1.0D0, PSICS, NTPSI)
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
               DPSI = DPSI - 1.0D0/(X+I)
   10       continue
         ELSE
C
C DPSI(X) FOR X .GE. 2.0 AND X .LE. 10.0
C
            DO 20 I = 1, N
              DPSI = DPSI + 1.0D0/(Y+I)
   20       continue
         END IF
         if (x .lt. -0.5D0) then
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
C DPSI(X) FOR ABS(X) .GT. 10.0
C
      if (x .lt. 0.0D0) then
         n = x
         if (x .eq. n) go to 100
      end if
      IF (Y.LT.XBIG) THEN
         AUX = DCSEVL (2.0D0*(10.0D0/Y)**2-1.0D0, APSICS, NTAPSI)
      ELSE
         AUX = 0.0D0
      END IF
      if (x .gt. 0.0D0) then
         DPSI = LOG(X) - 0.5d0/X + AUX
         psierr = round
         return
      end if
      n = x + x
      if (x .ne. n) then
c        X is not a half-integer (X can't be integer here).
         y = pi / tan(pi * x)
      else
         y = 0.0d0
      end if
      DPSI = LOG(ABS(X)) - 0.5d0/X + AUX - Y
c
c Error estimate when Psi might be going to infinity or zero
50    psierr = max(round, abs(x * xerr * y * y / dpsi))
      if (psierr .gt. tol) go to 200
      return
C
100   ierr = 2
      CALL DERM1 ('DPSI',ierr,2+MSGOFF,'X is a negative integer','X',X,
     1   '.')
      DPSI = 0.0d0
      psierr = -1.0d0
      RETURN
c
200   ierr = -3
      CALL ERMSG ('DPSI',ierr,1+MSGOFF,'Desired precision not achieved b
     1ecause X is too near a negative integer',',')
      CALL ERMOR ('or a zero of PSI(x)',',')
      call derv1 ('TOL',tol,',')
      call derv1 ('ERR',psierr,',')
      call derv1 ('X',X,'.')
      return
C
      END

      subroutine DPSIK (TOLI, XERRI, MSGOFI)
      double precision TOLI, XERRI
      integer MSGOFI
      logical FIRST
      save FIRST
      external D1MACH, DPSIB
      double precision D1MACH
      double precision TOL, XERR, PSIERR, ROUND
      integer IERR, MSGOFF
      common /DPSIC/ TOL, XERR, PSIERR, ROUND, IERR, MSGOFF
      save /DPSIC/
      data FIRST /.TRUE./
      if (first) then
         call dpsib
         round = d1mach(4)
         tol = sqrt(round)
         first = .false.
      end if
      if (toli .le. 0.0d0) then
         tol = sqrt(round)
      else
         tol = max(toli, round)
      end if
      if (xerri .lt. 0.0d0) then
         xerr = round
      else
         xerr = xerri
      end if
      msgoff = msgofi
      return
      end

      subroutine DPSIE (ERR, IERFLG)
      double precision ERR
      integer IERFLG
      logical FIRST
      save FIRST
      double precision TOL, XERR, PSIERR, ROUND
      integer IERR, MSGOFF
      common /DPSIC/ TOL, XERR, PSIERR, ROUND, IERR, MSGOFF
      save /DPSIC/
      data FIRST /.TRUE./
      if (first) then
         call dpsib
         first = .false.
      end if
      err = psierr
      ierflg = ierr
      return
      end

      subroutine DPSIB
      logical FIRST
      save FIRST
      double precision TOL, XERR, PSIERR, ROUND
      integer IERR, MSGOFF
      common /DPSIC/ TOL, XERR, PSIERR, ROUND, IERR, MSGOFF
      save /DPSIC/
      data FIRST /.TRUE./
      if (first) then
         first = .false.
         ierr = 0
         psierr = 0.0
         tol = -1.0d0
      end if
      end
