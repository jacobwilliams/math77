      SUBROUTINE SBACC (G,LDG,NB,IR,MT,JT,JTPREV, IERR2)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-10-20 SBACC Krogh  Changes to use M77CON
C>> 1992-06-16 SBACC CLL
C>> 1989-10-20 CLL
C>> 1987-11-24 SBACC  Lawson  Initial code.
c     Sequential accumulation of data for a banded least-squares
c     problem.  For solution use _BSOL.
c     ------------------------------------------------------------------
c                            Subroutine arguments
c
c     G(,)  [inout]  Array in which user provides data and this subr
c              builds a packed representation of the orthogonally
c              transformed equivalent problem.
c     LDG   [in]  Leading dimensioning parameter for G(,).
c     NB    [in]  Bandwidth of data matrix.  Set on first call for a
c              new problem and not changed thereafter.
c     IR    [inout]  User must set IR = 1 on 1st call for a new problem.
c              User must not thereafter alter IR.  This subr will update
c              IR as IR := JT + min(NB+1, MT+max(IR-JT,0)).
c              IR identifies the row of G(,) in which user's new block
c              of data begins.
c     MT    [in]  Number of new rows of data being provided on the
c              current entry.
c     JT    [in]  Column index in the user's problem of the data being
c              provided in column 1 of G(,).
c     JTPREV    [inout]  Should not be set or altered by the user.
c              This subr will update JTPREV as JTPREV := JT.
c              JTPREV identifies a row in G(,) at which the mode of
c              packed storage changes.  Data in row JTPREV and below
c              are subject to possible change as future data are
c              introduced, whereas data above row JTPREV are not.
c     IERR2  [inout]  Error status indicator.  Must be set to zero on
c              initial call to this subr for a new problem.  Values on
c              return have meaning as follows:
c           0 means no errors detected.
c           1 means MT exceeds LDG - IR + 1
c           2 means JT .lt. JTPREV
c           3 means JT .gt. min(JTPREV + NB, IR) which causes the
c             problem to be structurally singular.  The transformations
c             by this subr can be completed in this case but the
c             resulting transformed problem cannot be solved simply
c             by use of _BSOL.
c           4 means the condition of Error 3 above applys but computa-
c             tion must be abandonded due to storage limitations
c             indicated by MT > LDG - JT + 1.
c           5 This subr will STOP because the input value of IERR2 was
c             nonzero.
c     ------------------------------------------------------------------
C     C.L.LAWSON AND R.J.HANSON, JET PROPULSION LABORATORY, 1973 JUN 12
C     Reference:   'SOLVING LEAST SQUARES PROBLEMS', PRENTICE-HALL, 1974
c     Comments in this code refer to Algorithm Step numbers on
c     pp 213-217 of the book.
c     ------------------------------------------------------------------
c     1984 July 11, C. L. Lawson, JPL.  Adapted code from book
c     to Fortran 77 for JPL MATH 77 library.
c     Prefixing subprogram names with S or D for s.p. or d.p. versions.
c     Using generic names for any intrinsic functions.
c     July 1987.  Using _HTCC in place of _H12.
C     1989-10-20 CLL Moved integer declaration earlier to avoid warning
c     msg from Cray compiler.
C     1992-06-16 CLL  Using "min" fcn in arg list of call to _HTCC
c     to avoid "index out of range" when using a bounds checker, even
c     though no reference would be made to the "out of range" location.
c     ------------------------------------------------------------------
c--S replaces "?": ?BACC, ?HTCC
c     Both versions use IERM1, IERV1
c     ------------------------------------------------------------------
      integer I, IE, IEROLD, IERR2, IG, IG1, IG2, IR, ISTEP, J, JG, JT
      integer JTPREV, K, KH, KH1, L, LDG, LP1, MH, MT, NB, NBP1
      real             G(LDG,NB+1), ZERO, UPARAM
      parameter(ZERO = 0.0E0)
c     ------------------------------------------------------------------
C
C              ALG. STEPS 1-4 ARE PERFORMED EXTERNAL TO THIS SUBROUTINE.
C
      if (MT .le. 0) return
      if (IR .eq. 1) then
c                                     Initialize for new problem.
         JTPREV = 1
         IERR2 = 0
      elseif(IERR2 .ne. 0) then
         IEROLD = IERR2
         IERR2 = 5
         call IERM1('SBACC',IERR2,0,
     *   'Value of IERR2 nonzero on entry','IERR2 on entry',IEROLD,'.')
         stop
      endif
c
      NBP1=NB+1
      if( MT .gt. LDG - IR + 1) then
         IERR2 = 1
         call IERM1('SBACC',IERR2,0,
     *   'MT must not exceed LDG - IR + 1','MT',MT,',')
         call IERV1('LDG',LDG,',')
         call IERV1(' IR', IR,'.')
         return
      endif
C                                             ALG. STEP 5
      IF (JT.EQ.JTPREV) GO TO 70
C                                             ALG. STEP 6
      if(JT .lt. JTPREV) then
         IERR2 = 2
         call IERM1('SBACC',IERR2,0,
     *   'Require JT .ge. JTPREV', 'JT',JT,',')
         call IERV1('JTPREV', JTPREV,'.')
         return
      endif
C                                             ALG. STEP 7
      if (JT .gt. min(JTPREV+NB,IR) ) then
         IERR2 = 3
         call IERM1('SBACC',IERR2,0,
     *   'With JT .gt. min(JTPREV+NB,IR) problem will be singular.',
     *   'JT',JT,',')
         call IERV1('JTPREV', JTPREV,',')
         call IERV1('NB', NB,',')
         call IERV1('IR', IR,'.')
c
c        Note:  At this point we know the problem will be structurally
c        singular and thus will not be solvable simply by use of _BSOL.
c        This does not preclude, however, completion of the transforma-
c        tion to the band triangular form and so we continue.  The
c        calling program will be informed of this condition by the set-
c        ting of of IERR2 = 3, and thus can quit or continue as desired.
c        If this is not due to a usage mistake
c        then the user should probably introduce more data, modify his
c        or her mathematical model, or use a stabilizing method such as
c        is discussed on pp. 218-219 of the L & H book.
c
         if(MT .gt. LDG - JT + 1) then
            IERR2 = 4
            call IERM1('SBACC',IERR2,0,
     *      'MT must not exceed LDG - JT + 1','MT',MT,',')
            call IERV1('LDG', LDG,',')
            call IERV1('JT', JT,'.')
            return
         endif
C                                             ALG. STEPS 8-9
         do 10 I=1,MT
            IG1=JT+MT-I
            IG2=IR+MT-I
            do 10 J=1,NBP1
   10          G(IG1,J)=G(IG2,J)
C                                             ALG. STEP 10
         IE=JT-IR
         do 20 I=1,IE
            IG=IR+I-1
            do 20 J=1,NBP1
   20          G(IG,J)=ZERO
C                                             ALG. STEP 11
         IR=JT
      endif
c
c     Here we must shift some rows left by variable amounts.
c     min(NB-1,IR-JTPREV-1) is the number of rows to be shifted.
c     K = min(L,JT-JTPREV) is the extent of the left shift
c     for row JTPREV+L.
c
C                                             ALG. STEP 12-13
         do 50 L=1, min(NB-1,IR-JTPREV-1)
C                                             ALG. STEP 14
            K=min(L,JT-JTPREV)
C                                             ALG. STEP 15
            LP1=L+1
            IG=JTPREV+L
            do 40 I=LP1,NB
   40          G(IG,I-K)=G(IG,I)
C                                             ALG. STEP 16
            do 50 JG = NBP1-K, NB
   50          G(IG,JG)=ZERO
C                                             ALG. STEP 17
      JTPREV=JT
C                                             ALG. STEPS 18-19
   70 continue
c
c     MH = No. of rows in the block to which the orthogonal
c          triangularization will be applied.
c     KH1 = Number of pivot columns to be used in the orghogonal
c           transformations.
c
      MH=IR+MT-JTPREV
      KH1 = min(NBP1,MH-1)
      ISTEP = IR-JTPREV+1
C                                             ALG. STEP 20
      do 80 I=1,KH1
   80   CALL SHTCC (1,I,max(I+1,ISTEP),MH,G(JTPREV,I),UPARAM,
     *            G(JTPREV,min(I+1,NBP1)),LDG,NBP1-I)
C                                             ALG. STEP 21
      KH=min(NBP1,MH)
      IR=JTPREV+KH
C                                             ALG. STEP 22
      if (KH .GE. NBP1) then
C                                             ALG. STEP 23
         do 90 I=1,NB
   90       G(IR-1,I)=ZERO
C                                             ALG. STEP 24
      endif
C                                             ALG. STEP 25
      return
      end
