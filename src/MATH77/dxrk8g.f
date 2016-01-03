      subroutine DXRK8G (TS, Y, WORK, IDAT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2008-02-25 DXRK8G Krogh Lots of changes in usage, bug fixes.
c>> 1997-02-18 DXRK8G  Krogh  Initial code.
c--D replaces "?": ?XRK8G, ?XRK8I, ?XRK8, ?MESS, ?ZERO

c Checks and finds G-Stops for the integrator DXRK8.
c   From dxrk8f:  call dxrk8g(TS, Y, F, IDAT)
c   Reverse Comm: call dxrk8g(TS, Y, WORK, IDAT)
c
c User code from dxrk8f looks something like:
c    if (IDAT(1) .eq. 1) then
c      if (There are extrapolating G-Stops) then
c        TS(3+i) = value of the i-th extrapolating G_i, i = 1, ...
c        call DXRKG(TS, Y, F, IDAT)
c        if (IDAT(3) .ne. 0) return
c       end if
c       Compute F and return
c    end if
c    do
c      Compute TS(3+i) for all G_i, i = 1, ...
c      if (IDAT(3) .ne. -1) return
c    end do
c
c
c ************************ Calling Sequence Arguments ******************
c
c T      The current value of the independent variable.
c Y      The current value of the dependent variable.
c WORK   Contains information about the stepsize, previous T, last T for
c   which G's were evaluated, and when interpolating, contains other
c   data required for the interpolation.
c IDAT   Used to hold various integer values used in the integration.
c
c ************************* Variable Definitions ***********************
c
c    Most names are defined in DXRK8.  Just those local to DXRK8G here.
c
c LI    IDAT(LOCINT) = Location in WORK where information connected with
c    interpolation is saved.  See IDAT(LOCINT) in DXRK8.
c LG    Difference between indexes for g and the old g.
c NG    Index for current G.
c TL    Value of T where G's were last computed on call to DZERO.
c
c ******************** Variable Declarations ***************************
c
      double precision TS(*), Y(*), WORK(*)
      integer IDAT(*)
c
c Parameters used to reference IDAT
      integer INTSAV, KDZERO, KEOS, KFAIL, KHMIN, KSTEP,
     1  KSTEPM, KSTEPX, KSTIFF, LBF, LINT, LOCEOS, LOCEPR,
     2  LOCERR, LOCINT, LOCOUT, LOCXP, LREVF, LREVO, LSTLAS,
     3  LTFIN, LWHYF, NERRSV, NGLO, NGHI, NSTIFF, NTGS, NUMINT,
     4  NXGS
      parameter (INTSAV=41, KSTIFF=20, KDZERO=21, KEOS=19, KFAIL=28,
     1  KHMIN=30, KSTEP=27, KSTEPM=34, KSTEPX=36, LBF=13, LINT=10,
     2  LOCEOS=18, LOCEPR=11, LOCERR=44, LOCINT=38, LOCOUT=14,
     3  LOCXP=12, LREVF=23, LREVO=24, LSTLAS=15,
     4  LTFIN=42, LWHYF=40, NERRSV=26, NGLO=31, NGHI=32,
     5  NSTIFF=29, NTGS=17, NUMINT=39, NXGS=16)
c
c Local variables
      integer I, LI, NG, LG
      double precision TL
c Parameters for error messages
      integer MERET, MEEMES
      parameter (MERET=51, MEEMES=52)
c Other Stuff for error processing
      integer MACT(5)
      double precision DATERR(1)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DXRK8G$B
cAB Calling DXRK8G when IDAT(1) is not 1 or 2, and no G-Stop was $C
c   found for IDAT(1) = $I, T = $F.$E
cAC TS($I) has changed sign in an interval where previous checks $C
c   showed no sign change.  The location of the stop returned, $F, $C
c   is at best questionable, and is certainly of low accuracy.$E
      integer LTXTAA,LTXTAB,LTXTAC
      parameter (LTXTAA=  1,LTXTAB=  9,LTXTAC=103)
      character MTXTAA(2) * (143)
      data MTXTAA/'DXRK8G$BCalling DXRK8G when IDAT(1) is not 1 or 2, an
     *d no G-Stop was found for IDAT(1) = $I, T = $F.$ETS($I) has change
     *d sign in an interval wh','ere previous checks showed no sign chan
     *ge.  The location of the stop returned, $F, is at best questionabl
     *e, and is certainly of low accuracy.$E'/
c **** End of automatically generated text
c
      data MACT / MEEMES, 36, 0, 0, MERET /
c
c ********************** Start of Executable Code **********************
c
      LG = abs(IDAT(NTGS)) - 2
      if (IDAT(1) .ne. 2) then
        if (IDAT(1) .eq. 1) then
          IDAT(3) = 0
          if (IDAT(NTGS) .lt. 0) then
c Just started an integraion, just Copy G to GOLD
            do 20 NG = 4, IDAT(NXGS)
              TS(NG+LG) = TS(NG)
 20         continue
c Remember t at the saved values for G/
            TS(LG+3) = TS(1)
            return
          end if
c Checking for sign changes.
          do 30 NG = 4, IDAT(NXGS)
c Check extrapolating G's when computing F.
            if (TS(NG) * TS(NG+LG) .le. 0) then
              if (TS(NG+LG) .ne. 0.D0) then
                if (IDAT(LWHYF) .ge. 13) then
c  Sign change occurred while getting derivatives for interpolation.
                  MACT(3) = 25
                  MACT(4) = LTXTAC
                  IDAT(1) = 2
                  IDAT(2) = NG
                  IDAT(3) = NG
                  I = 2
                  go to 510
                end if
c Take a step that doesn't quite reach this sign change.
                TS(2) = .9D0 * (TS(1)-TS(LG+3)) *
     1            abs(TS(LG+NG) / (TS(LG+NG) - TS(NG)))
                TS(1) = TS(LG+3) + 1.8 * TS(2)
                IDAT(LWHYF)=19
                IDAT(3) = NG
                IDAT(NGLO) = -NG
                IDAT(NGHI) = -NG
                return
              end if
            end if
 30       continue
c         Found no stops
          return
        end if
c                   Set IDAT(LWHYF) so we know G's were computed.
        IDAT(LWHYF) = 21
        if ((IDAT(3) .ge. 4) .and. (IDAT(3) .lt. IDAT(2)) .and.
     1    (IDAT(2) .lt. LG+3)) then
c Apparently there were multiple stops athe same value of t, get next.
          do 40 NG = IDAT(2) - 1, IDAT(3), -1
            if ((TS(NG)*TS(NG+LG).le.0.D0).and.(TS(NG+LG).ne.0.D0)) then
              IDAT(2) = NG
              IDAT(NGHI) = NG
              TS(NG+LG) = 0.D0
              return
            end if
 40       continue
        end if
        MACT(3) = 7
        MACT(4) = LTXTAB
c          No good way to get error flag back to users main program
c          without setting IDAT(1) which could snowball the problem.
        go to 500
      end if
c End of step actions -- Set IDAT(LWHYF) so we know G's were computed.
      IDAT(LWHYF) = 21
      if (IDAT(NTGS) .lt. 0) then
c         Initial setup for TOLD and GOLD
        IDAT(NTGS) = -IDAT(NTGS)
        do 50 NG = 4, LG+2
          TS(NG+LG) = TS(NG)
 50     continue
        TS(NG) = TS(1)
        return
      end if
      LI = IDAT(LOCINT)
c If IDAT(2) is -1, then we are in the middle of an iteration
      if (IDAT(2) .eq. -1) go to 320
c User just computed a full set of G's.  Set flag assuming no stop.
      if (IDAT(3) .gt. 0) then
        NG = IDAT(3) + 1
      else
        NG = 4
      end if
      if (abs(TS(1) - TS(LG+3)) .le. .25D0 * abs(TS(2))) then
c  We want to leave GOLD_i as 0 if it is 0 when close to last 0.
c  (This is prevent noise from resulting in multiple G-Stops.)
        do 60 NG = NG, LG + 2
          if (TS(NG+LG) .ne. 0) then
            if (TS(NG) * TS(NG+LG) .le. 0.D0) then
              if (TS(NG) .ne. 0.D0) go to 300
            end if
            TS(NG+LG) = TS(NG)
          end if
 60     continue
        go to 100
      end if
c            Continue here after finding a stop.
 70   continue
c  t is not close to location of previously computed or just found a
c  sign change
      do 80 NG = NG, LG + 2
        if (TS(NG) * TS(NG+LG) .le. 0.D0) then
          if (TS(NG+LG) .ne. 0.D0) go to 300
        end if
        TS(NG+LG) = TS(NG)
 80   continue
 100  if (IDAT(3) .eq. 0.D0) then
c               No sign change was seen.
        TS(NG) = TS(1)
        IDAT(2) = 0
        return
      end if
      NG = IDAT(3)
      IDAT(NGHI) = NG
      IDAT(NGLO) = NG
      IDAT(2) = NG
c  Set to 0 so rounding errors don't give us an extra stop.
      TS(NG+LG) = 0.D0
c  Check if there is a G-Stop preceding the one found.
c  (This can only happen if there are multiple stops at a given time.)
      do 120 NG = IDAT(2) - 1, 4, -1
        if (TS(NG) * TS(NG+LG) .le. 0.D0) then
          if (TS(NG+LG) .ne. 0.D0) then
c   Found a stop remember the first.
            IDAT(NGLO) = NG
          else
            TS(NG+LG) = TS(NG)
          end if
        end if
 120  continue
c  Set last TS(1) where G-Stops were computed
      TS(LG+3) = TS(1)
      return
c           Start search for a G -Stop
 300  IDAT(2) = -1
      IDAT(KDZERO) = 0
      IDAT(3) = -NG
      WORK(LI-6) = TS(NG+LG)
      WORK(LI-5) = TS(LG+3)
      go to 350
c                       In the middle of an iteration
 320  NG = IDAT(3)
      if (NG .gt. 0) go to 420
      NG = -NG
c## Note DZERO has save variables as does MESS and DMESS.
c At some point we need to set aside space in WORK for all saves that
c might be required.  In DZERO this is a good size set: DIV, FL, FLMFB,
c FO, KNKP, KS, KTYP, LCHG, LMODE, LNLP, MACT, NP, RND, SMALL, XL,
c XLMXB, XO, XX, XXMXOL.  (Even more in MESS and DMESS!) A simpler code
c would need less.  Or we could allow save variable here since the
c "save" is only over the duration of the iteration for the 0???
c
 350  TL = TS(1)
      call DZERO(TS(1),TS(NG),WORK(LI-5),WORK(LI-6),IDAT(KDZERO),0.D0)
      if (IDAT(NUMINT) .le. 0) then
c                          Not ready to interpolate
        IDAT(2) = -2
        IDAT(3) = -NG
        IDAT(NGHI) = NG
        IDAT(NGLO) = -NG
        return
      end if
      if (IDAT(KDZERO) .ne. 1) go to 400
 380  call DXRK8I(TS(1), Y, IDAT, WORK)
      return
c                  Some kind of convergence
 400  if (TS(2) * (WORK(LI-5) - TS(1)) .gt. 0.D0) TS(1) = WORK(LI-5)
      IDAT(3) = NG
      if (TS(2) * (TS(1) - TL) .gt. 0) go to 380
      TS(1) = TL
c  Get here after a final g evaluation or on multiple stops.
 420  IDAT(2) = NG
      NG = NG + 1
      go to 70

c Process an error.
 500  I = 1
 510  DATERR(1) = TS(1)
      call DMESS(MACT, MTXTAA, IDAT(I), DATERR)
      return
      end
