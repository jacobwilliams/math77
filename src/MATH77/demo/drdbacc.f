c     program DRDBACC
c>> 1996-05-28 DRDBACC  Krogh Moved formats up.
c>> 1994-10-19 DRDBACC  Krogh  Changes to use M77CON
c>> 1987-12-09 DRDBACC  Lawson  Initial Code.
c--D replaces "?": DR?BACC, ?BACC, ?BSOL
c     Demonstration driver for DBACC & DBSOL
c     C. L. Lawson & S. Y. Chiu, JPL, July 1987, Sept 1987.
c     ------------------------------------------------------------------
      integer NX, LDG, NB, ISCALE
      integer I, IERR2, IERR3, IG, IR, J, JT, JTPREV, MT, MTOTAL
      parameter(NX = 10, LDG = 24, NB = 2, ISCALE = 6)
      double precision XTAB(NX), G(LDG,3), YFIT(NX), C(NX,NX)
      double precision DELX, DOF, DTOR, RDUMMY, RNORM, SIGFAC, VFAC
      double precision X, YF, YTRUE
      double precision ZERO, XINC, XLIMIT
      double precision ONE, C45, HALF
      parameter(ZERO = 0.0D0, XINC = 1.0D0, XLIMIT = 89.5D0)
      parameter(ONE = 1.0D0, C45 = 45.0D0, HALF = 0.5D0)
      data XTAB / 00.0D0, 10.0D0, 20.0D0, 30.0D0, 40.0D0,
     *            50.0D0, 60.0D0, 70.0D0, 80.0D0, 90.0D0 /
c
 1000 format(' Calling DBACC with JT =',i3,', MT =',i3)
 1003 format(1x/'     X       Y        YFIT   R=Y-YFIT'/1X)
 1004 format(1X, F6.1, 3F10.5)
 1006 format(1X,10(F7.2,1X))
c     ------------------------------------------------------------------
      write(*,'(1x,a//1x,a/1x,a/1x)')
     *'        Demonstration driver for DBACC and DBSOL.',
     *'Compute least-squares fit of a continuous piecewise linear',
     *'     function to the sine function on 0 to 90 degrees.'
      DTOR = atan(ONE)/C45
      MTOTAL = 0
      IR = 1
      MT = 0
      JT = 1
      IG = 0
      X = -XINC
      DELX = XTAB(JT+1) - XTAB(JT)
c
   20 if( X .lt. XLIMIT)then
         X = X + XINC
         MTOTAL = MTOTAL + 1
      if(X .gt. XTAB(JT+1))then
            write(*,1000) JT, MT
            call DBACC(G,LDG,NB,IR,MT,JT,JTPREV, IERR2)
            IG = IR-1
            MT = 0
            JT = min(JT+1, NX-1)
            DELX = XTAB(JT+1) - XTAB(JT)
      end if
         IG = IG + 1
         MT = MT + 1
         G(IG,1) = (XTAB(JT+1) - X) / DELX
         G(IG,2) = (X - XTAB(JT)) / DELX
         G(IG,3) = sin(x * DTOR)
      go to 20
      end if

      if(MT .gt. 0)then
         write(*,1000) JT, MT
         call DBACC(G,LDG,NB,IR,MT,JT,JTPREV, IERR2)
      end if

      call DBSOL(1,G,LDG,NB,IR,JTPREV,YFIT,NX,RNORM, IERR3)
c                       The following statement does a type conversion.
      DOF = MTOTAL - NX
      SIGFAC = RNORM / SQRT(DOF)
      write(*,'(1x/1x, a, i4, a, f10.5, a, f10.5)')
     *  'MTOTAL =', MTOTAL, ',  RNORM =', RNORM, ',  SIGFAC =', SIGFAC
      write(*,1003)
      do 30 I=1,NX
         YTRUE = sin(XTAB(I)*DTOR)
         write(*,1004) XTAB(I),YTRUE,YFIT(I),YFIT(I) - YTRUE
      if( I .ne. NX)then
            X = HALF*(XTAB(I+1) + XTAB(I))
            YF = HALF*(YFIT(I+1) + YFIT(I))
            YTRUE = sin(X*DTOR)
            write(*,1004) X,YTRUE,YF,YF - YTRUE
      end if
   30 continue
c
c     Compute unscaled covariance matrix in C(,).
c
      do 50 J = 1,NX
        do 40 I = 1,NX
            C(I,J) = ZERO
   40   continue
        C(J,J) = ONE
        call DBSOL(2,G,LDG,NB,IR,JTPREV,C(1,J),NX,RDUMMY,IERR3)
        call DBSOL(3,G,LDG,NB,IR,JTPREV,C(1,J),NX,RDUMMY,IERR3)
   50 continue
c
      write(*,'(1x/10x,a,i1/1x)')
     *  'Covariance matrix scaled up by 10**',ISCALE
c
      VFAC = (10**ISCALE) * SIGFAC**2
      do 60 I = 1,NX
        print 1006,(VFAC*C(I,J),J=1,NX)
   60 continue
c
      stop
      end
