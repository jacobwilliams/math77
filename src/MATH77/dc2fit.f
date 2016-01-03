      SUBROUTINE DC2FIT(XI,YI,SDI,NXY,B,NB,W,NW,YKNOT,YPKNOT,
     *         SIGFAC, IERR1)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2000-12-01 DC2FIT Krogh  Dim. SDI(*) instead NXY.
c>> 1995-11-21 DC2FIT Krogh  Converted from SFTRAN to Fortran 77.
c>> 1994-10-19 DC2FIT Krogh  Changes to use M77CON
c>> 1994-01-31 DC2FIT CLL Added test for SDI(i) .le. 0 when SDI(1) > 0.
c>> 1990-01-23 CLL Deleted ref to unused variable NX in call to IERM1
C>> 1989-10-20 CLL
C>> 1987-10-22 DC2FIT Lawson  Initial code.
c           Least squares fit to discrete data by a C-2 cubic spline.
c     ------------------------------------------------------------------
C     Algorithm and program designed by C.L.Lawson and R.J.Hanson.
c     The general approach but not the complete code is given in
C     'SOLVING LEAST SQUARES PROBLEMS', by Lawson and Hanson,
C     publ by Prentice-Hall, 1974.
C     Programming and later changes and corrections by Lawson,Hanson,
C     T.Lang, and D.Campbell, Sept 1968, Nov 1969, and Aug 1970.
C     Modified 1968 Sept 17 to provide C-2 continuity.
C     1974 5/21, C.L.Lawson, Fixed bug that caused ISEG to get too big.
C     Also changed to exit immidiately if B() array is not
C     strictly increasing.
c     1984 July 10. Modified for improved portability and to conform
c          to Fortran 77.  C. L. Lawson, JPL.
c          Added calls to the error message subrs.
c     7/23/87 CLL.  Added the IERR1 argument.
C     1989-10-20 CLL  Changed code so there are no RETURN statements in
c     Sftran procedures.  Previous code had such a RETURN that led to
c     a warning diagnostic from a Cray compiler due to unreachable
c     CONTINUE statements.  Also introduced "c--" lines for CHGTYP.
c     ------------------------------------------------------------------
c                     SUBROUTINE ARGUMENTS
c
c  (XI(i),i=1,NXY)  [in]  Abcissas of data to be fitted.  Require
c        this data be ordered so X(i) .le. X(i+1).
c
c  (YI(i),i=1,NXY)  [in]  Ordinates of data to be fitted.
c
c  (SDI(i),i=1,NXY) [in]  User may use this array to assign an
c                     a priori standard deviation of error to each
c       YI(i) value.  The weighted fitting algorithm will take
c       account of these.  Optionally the user may set SDI(1) to
c       a negative value.  Then this subr will use ABS(SDI(1)) as
c       the standard deviation for each YI(i) value.  In this case
c       the SDI() array can be dimensioned SDI(1).
c       If SDI(1) = 0., the subr issues an error message and returns.
c
c  NXY  [in]  No. of data pairs, (XI(i), YI(i)), and no. of elts
c          in SDI() if SDI(1) is positive.  Require NXY .ge. 4.
c
c  (B(j),j=1,NB)  [in]  Breakpoints for the spline function,
c        including endpoints.  These breakpoints must be
c        strictly increasing:  B(j) .lt. B(j+1).
c        It is required that all abcissas, XI(i), lie in the
c        closed interval, [B(1), B(NB)].
c
c  NB  [in]  No. of breakpoints, including endpoints.
c         The no. of parameters in the least squares problem
c         will be NB + 2.
c         To have a nonsingular problem one must have
c         NXY .ge. NB + 2, and the distribution of the breakpoints
c         must not be too skewed relative to the data abcissas.
c       If singularity is detected, an error message will be
c       issued by the subr that solves the band matrix.
c
c  W()  [scratch]  Work space dimensioned W(NW,5).
c
c  NW  [in]  First dimension of W().   Must satisfy NW .ge. NB + 4
c          Let KMAX denote the max no. of data abcissas, XI(i),
c          in any one breakpoint interval, i.e. between B(j) and
c          B(j+1) for some j.   The subr will be more efficient
c          if NW is at least NB + 3 + KMAX.
c
c  (YKNOT(k) and YPKNOT(k),k=1,NB)  [out]  The subr will return
c       values defining the fitted C2 spline curve in these arrays.
c       These values and first derivatives of the fitted curve at the
c       knot abcissae.  YKNOT(j) = f(B(j))   and
c       YPKNOT(j) = fprime(B(j))  for  j = 1,...,NB.
c       The user can then evaluate the fitted curve at any point by
c       Hermite interpolation.  See subrs DHINT or SHINT.
c
c  SIGFAC  [out]  The subr sets SIGFAC to RNORM / sqrt(DOF) where
c      RNORM = sqrt( sum over i of [( (yfit(i) - YI(i))/SDI(i))**2])
c       and DOF = max(1, NXY - (NB+2))
C
c  IERR1  [out]  Error status indicator.  Note that IERR2 comes from
c         DBACC and IERR3 comes from DBSOL.
c
c        =    0 means no errors detected.
c        =  100 means  NB .lt. 2   .or.   NXY .lt. NB+2
c        =  200 means  B(I) .ge. B(I+1)
c        =  300 means  NW .lt. NB+4
c        =  400 means  XI(I-1) .gt. XI(I)
c        =  500 means  B(1) .gt. XI(1) .or. B(NB) .lt. XI(NXY)
c        =  600 means  Need larger dimension NW.
c        =  700 + IERR2 means IERR2 .ne. 0
c        =  800 + IERR2 means IERR2 .ne. 0
c        =  900 + IERR2 means IERR2 .ne. 0
c        = 1000 + IERR3 means IERR3 .ne. 0 due to singularity
c                       detected in DBSOL.
c        = 1100 means SDI(1) = zero.
c        = 1200 means SDI(1) > zero and SDI(i) .le. zero for some i.
c     ------------------------------------------------------------------
c             Important internal variables.
c
c     ISEG     Index of current spline segment, starting with 1 for
c              the first segment.
c              Also tells the band matrix subroutine the column index
c              of the least squares matrix with which the first col
c              of the new block of data in G() is to be associated.
C     KSIZE    Size of current block.
C     JPOINT   Current data pointer.
c     ------------------------------------------------------------------
c--D replaces "?": ?C2FIT, ?BACC, ?C2BAS, ?ERM1, ?ERV1, ?BSOL, ?TRC2C
c     Both versions use ERMSG, IERM1, IERV1
c     Lower level subrs needed: (D/S)HTCC, (D/S)NRM2, ERFIN
c     ------------------------------------------------------------------
      integer I, IERR1, IERR2, IERR3, IRNOW, ISEG, J, JPOINT, JTPREV
      integer K, KSIZE, N1, NB, NPARAM, NW, NXY
      integer NBAND, NBAND1
      parameter(NBAND = 4, NBAND1 = NBAND+1)
      double precision XI(NXY), YI(NXY), SDI(*), B(NB), W(NW, 5)
      double precision YKNOT(NB), YPKNOT(NB), P(4), ONE, ZERO
      double precision DOF, RNORM, sdijp, SIGFAC, WT, WT1
      logical  NEWSEG, USEWT1
      parameter( ONE = 1.0D0, ZERO = 0.0D0)
c     ------------------------------------------------------------------
      IERR1 = 0
C
C          EXIT IMMEDIATELY IF NB .lt. 2  OR  NXY .LT NB+2  OR  IF THE
C          BREAKPOINTS ARE NOT STRICTLY INCREASING.
c
      NPARAM = NB+2
      IF ( NB .lt. 2   .or.   NXY .lt. NPARAM) THEN
         IERR1 = 100
         call IERM1('DC2FIT',IERR1,0,
     *   'Require NB .ge. 2 and NXY .ge. NB+2', 'NB',NB,',')
         call IERV1('NXY',NXY,'.')
         go to 300
      END IF
c
      N1 = NB-1
      DO 20 I = 1,N1
        IF (B(I) .ge. B(I+1)) THEN
          IERR1 = 200
          call IERM1('DC2FIT',IERR1,0,
     *    'Require knots, B(I), to be strictly increasing.',
     *    'I',I,',')
          call DERV1('B(I)',B(I),',')
          call DERV1('B(I+1)',B(I+1),'.')
          go to 300
        END IF
   20 CONTINUE
C
C     Require NW .ge. NB+4
C
      IF (NW .lt. NB+4) THEN
        IERR1 = 300
        call IERM1('DC2FIT',IERR1,0,'Require NW .ge. NB+4','NW',NW,',')
        call IERV1('NB',NB,'.')
        go to 300
      END IF
C
c     ------------------------------------------------------------------
C                                       TEST SDI(1)
      IF (SDI(1) .lt. ZERO) THEN
         WT1 = -ONE/SDI(1)
         USEWT1 = .true.
      ELSE IF ( SDI(1) .gt. ZERO) THEN
         USEWT1 = .false.
      ELSE
         IERR1 = 1100
         call ERMSG('DC2FIT',IERR1,0,'Require SD(1) .ne. Zero','.')
         return
      END IF
C
c                             Test ordering of XI() array.
c
      DO 40 I=2,NXY
        IF (XI(I-1) .gt. XI(I)) THEN
          IERR1 = 400
          call IERM1('DC2FIT',IERR1,0,
     *    'Require abcissas, X(I), to be nondecreasing.',
     *    'I',I,',')
          call DERV1('X(I-1)',XI(I-1),',')
          call DERV1('X(I)',XI(I),'.')
          go to 300
        END IF
   40  CONTINUE
c
C                             TEST THE FIRST AND LAST BREAKPOINT
C                             FOR BRACKETING THE DATA ABCISSAS.
c
      IF (B(1) .gt. XI(1) .or. B(NB) .lt. XI(NXY)) THEN
        IERR1 = 500
        call DERM1('DC2FIT',IERR1,0,
     *     'Require B(1) .LE. X(1) and B(NB) .ge. XI(NXY)',
     *     'B(1)',B(1),',')
        call DERV1('X(1)',XI(1),',')
        call DERV1('B(NB)',B(NB),',')
        call DERV1('X(NXY)',XI(NXY),'.')
        go to 300
      END IF
C
C     BEGIN LOOP TO FORM EQUATIONS FOR C2 LEAST SQUARES FIT.
C
      IRNOW = 1
      K = 1
      KSIZE = 0
      NEWSEG = .TRUE.
      ISEG = 1
      DO 120 JPOINT = 1, NXY
         IF( K .gt. NW ) THEN
            call DBACC(W, NW, NBAND, IRNOW, KSIZE, ISEG, JTPREV, IERR2)
            if(IERR2 .ne. 0) then
               IERR1 = 700 + IERR2
               go to 200
            end if

            IF(IRNOW .gt. NW) THEN
               IERR1 = 600
               call IERM1('DC2FIT',IERR1,0,
     *            'Need larger dimension NW.','NW',NW,'.')
               go to 300
            END IF
            K = IRNOW
            KSIZE = 0
         END IF
c
C        DO WHILE( XI(JPOINT) .gt. B(ISEG+1) )
   80    IF ( XI(JPOINT) .gt. B(ISEG+1) ) THEN
            call DBACC(W, NW, NBAND, IRNOW, KSIZE, ISEG, JTPREV, IERR2)
            if(IERR2 .ne. 0) then
               IERR1 = 800 + IERR2
               go to 200
            end if
            KSIZE = 0
            K = IRNOW
            ISEG = ISEG + 1
            NEWSEG = .TRUE.
            go to 80
         END IF
c        END WHILE
c                          Build one equation
         call DC2BAS(XI(JPOINT), 0, ISEG,NEWSEG,B,NB,P)
         IF( USEWT1) THEN
            WT = WT1
         ELSE
            sdijp = sdi(jpoint)
            if(sdijp .gt. ZERO) then
               WT=ONE/sdijp
            else
               IERR1 = 1200
               call ERMSG('DC2FIT',IERR1,0,
     *            'With SD(1) > 0  require all SD(I) > 0.', ',')
               call DERV1('SD(1)',SDI(1),'.')
               call IERV1('I',jpoint,',')
               call DERV1('SD(I)',sdijp,'.')
               return
            end if
         END IF
         DO 100 J = 1,4
            W(K,J)=P(J)*WT
  100    CONTINUE
         W(K,5)=YI(JPOINT)*WT
c                          End of build one equation
         K = K+1
         KSIZE = KSIZE + 1
c
  120 CONTINUE
      call DBACC(W, NW, NBAND, IRNOW, KSIZE, ISEG, JTPREV, IERR2)
            if(IERR2 .ne. 0) then
               IERR1 = 900 + IERR2
               go to 200
            end if
C
C     ALL DATA POINTS HAVE BEEN PROCESSED.  CALL FOR SOLUTION.
C
      call DBSOL(1,W, NW, NBAND, IRNOW, JTPREV, W(1,NBAND1),
     *           NPARAM, RNORM, IERR3)
            if(IERR3 .ne. 0) then
               IERR1 = 1000 + IERR2
               call ERMSG('DC2FIT',IERR1,0,
     *         'Singularity noted in DBSOL.','.')
               go to 300
            end if

      DOF = MAX(1, NXY - NPARAM)
      SIGFAC = RNORM / sqrt(DOF)
C
C                  TRANSFORM PARAMETERS TO Y,YPRIME BASIS
C
      call DTRC2C (B,NB,W(1,5),YKNOT,YPKNOT)
      RETURN
c
c             Error in _BACC
  200 call ERMSG('DC2FIT',IERR1,0,
     *   'Error detected in subroutine DBACC','.')

c             Set YKNOT() & YPKNOT() to zero
  300 DO 320 I=1,NB
            YKNOT(I)=ZERO
            YPKNOT(I)=ZERO
  320 CONTINUE
      return
      end
