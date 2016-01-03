      subroutine SBLSE (A, LDA, MTRUE, N, M1TRUE, B, BND, UNBND,
     *   KPRINT, TOL, IERR, X, RNORM, NSETS, W, SIZE, TNORM,
     *   Z, CC, SS, INDEX, JSTAT, XT, RT, DIFF)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2001-05-25 SBLSE Krogh Minor change for making .f90 version.
c>> 2001-01-16 SBLSE Krogh  Fixed problem with conversion to C.
c>> 2000-12-01 SBLSE Krogh  Removed unused parameter Factor.
c>> 1999-12-22 SBLSE Krogh  Declared some 'NPR' variables.
c>> 1996-03-30 SBLSE Krogh  Added external statement.
c>> 1994-10-19 SBLSE Krogh  Changes to use M77CON
c>> 1992-10-09 SBLSE CLL  Solve for correction rather than new X.
c>> 1992-09-02 CLL  Quit doing look-ahead updating of the B() array.
c>> 1992-09-02 CLL  Add random choice to combat cycling.
c>> 1992-09-01 CLL  Change elimination method in constraint matrix.
c>> 1992-09-01 CLL  Add more updating of SIZE().
c>> 1992-06-17 CLL  Minor editing of comments.
c>> 1990-07-04 CLL  Minor editing of comments.
c>> 1988-04-26 CLL
c>> 1988-01-04 CLL
c
c     *** Bounded variables Least Squares with Equality constraints ***
c
c     In the following descriptions M denotes the input value MTRUE, and
c     M1 denotes the input value M1TRUE.
c     Given an M by N matrix, A, and an M-vector, b,  compute an
c     N-vector, x, that satisfies the first M1 equations of the linear
c     system Ax = b as equality constraints, and satisfies the remaining
c     rows of the system in the least squares sense, subject to bounds
c     on the components of x.
c
c     The bounds are specified in the form:
c
c               BND(1,J) .le. X(J) .le. BND(2,J)
c
c     where the special value UNBND is used with the meaning that
c     if BND(1,J) = UNBND, X(J) is unbounded below, and
c     if BND(2,J) = UNBND, X(J) is unbounded above.
c
c     A problem with general linear inequality constraints can be put in
c     the form acceptable to this subroutine by the introduction of
c     slack variables.  For example suppose a problem has inequality
c     constraints represented as Cx .ge. d where C is an M3 x N matrix.
c     Add M3 components to the end of the x vector, impose nonnegativity
c     bounds on these new components, and write the equality con-
c     straints, [C : -I] x = d, where I denotes the identity matrix of
c     order M3.
c
c     In anticipation of this usage, this subroutine searches
c     for columns in the constraint equations that have only a single
c     nonzero element (i.e., singletons) and uses these, if the signs
c     permit, for efficient construction of the initial
c     triangularization of the constraint equations.
c     ------------------------------------------------------------------
c          This code is a substantial generalization of the subroutine,
c     NNLS, that solves the  least squares problem, Ax = b, subject to
c     all x(j) .ge. 0.    The subroutine NNLS appeared in SOLVING
c     LEAST SQUARES PROBLEMS, by Lawson and Hanson, Prentice-Hall, 1974.
c
c          This algorithm has two major phases, the initial solution of
c     the constraint equations, subject to the bounds on the variables,
c     and then the solution of the least-squares problem, subject to the
c     equality constraints and the bounds on the variables.
c
c          The following Sftran procedures are used to get started and
c          to finish:
c
c     procedure( INITIALIZE )
c     procedure( TERMINATION )
c
c          The following Sftran procedures are used only in Phase 1:
c
c     procedure( TRIANGULARIZE USING SINGLETONS )
c     procedure( ALGORITHM BVEQ )
c     procedure( BVEQ: SELECT ANOTHER COEF TO SOLVE FOR )
c     procedure( BVEQ: MOVE JPNEW FROM SET F TO SET S )
c     procedure( MSG FOR IERR = -1 )
c     procedure( REARRANGE ROWS )
c
c          The following Sftran procedures are used only in Phase 2:
c
c     procedure( ALGORITHM BVLS )
c     procedure( BVLS: SELECT COEF TO SOLVE FOR )
c     procedure( BVLS: MOVE JPNEW FROM SET F TO SET S )
c     procedure( GIVENS ROTATIONS, USING IGIV1 AND IGIV2 )
c     procedure( HOUSEHOLDER TRIANGULARIZATION )
c     procedure( ELIMINATE IN LOWER ROWS )
c
c          The following Sftran procs are used in both Phases 1 & 2:
c
c     procedure( CONTROL CONSTRAINT TESTING FOR SET S )
c     procedure( SEE IF ALL CONSTRAINED COEFFS ARE FEASIBLE )
c     procedure( PERTURB ANY OUTLIERS IN SET S TO THE BOUNDARY )
c     procedure( MOVE COEF JZNEW FROM SET S TO SET F )
c     procedure( GAUSSIAN ELIMINATION, USING IGAUS1 AND IGAUS2 )
c     procedure( SOLVE TRIANGULAR SYSTEM, INPUT AND OUTPUT IN Z() )
c     procedure( DEBUG )
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c          BVLS was developed by C. L. Lawson and R. J. Hanson at
c     JPL, 1973 June 12.  This version was identified as BVLS2.
c          Changes made by Lawson, JPL, 1982 Sept.
c     Added NSETS into the argument list.
c     1982 Sept 16.  Converted code to SFTRAN3.  New version was called
c          BVLS5.
c     1986 June 25. C. L. Lawson. Adapting code to MATH77 style.
c     1987 Oct. CLL  Changing name to SBLS.  Using new MATH77 versions
c          of second-level subroutines.  Developing SBLSE from SBLS to
c          solve the equality-constrained bounded variable problem.
c     1988 April CLL & RJH.  Added code to delete constraint rows that
c          are inconsistent or dependent.
c     1992 Sept/Oct CLL  Major changes.  Solving for change to X rather
c          than directly for X.  This changes formulas for updating the
c          right-side vector in B().  No longer need array SIZE().
c     ------------------------------------------------------------------
c                            SUBROUTINE ARGUMENTS
C
c     A()     [In/Out]  On entry A() contains the MTRUE by N matrix, A.
c             On return A() contains the product matrix, G*A,
c             where G is an MTRUE by MTRUE matrix generated
c             implicitly by this subroutine.
c
c     LDA     [In]  LDA is the first dimensioning parameter for the
c             array, A().  Require LDA .ge. MTRUE.
c
c     MTRUE  [in]  Total number of rows in the matrix, A.  Require
c            MTRUE > 0.
c
c     N  [in]  Number of columns in the matrix, A.  Require N > 0.
c
c     M1TRUE  [in]  Number of leading rows of [A,b] to be treated as
c               equality constraints.  User must set M1TRUE = 0 if
c               there are no equality constraint rows.
c               Require 0 .le. M1TRUE .le. min(MTRUE, N).
c
c     B()     [In/Out]  On entry B() contains the MTRUE-vector, B.
c             On return, B() contains G*(B - A[Z] * X[Z] ) where A[Z]
c             and X[Z] denote the parts indexed in Set F.
c
c     BND(,)  [In] If BND(1,J) .eq. UNBND then X(J) is unbounded below.
c             Otherwise BND(1,J) is the lower bound for X(J).
c             If BND(2,J) .eq. UNBND then X(J) is unbounded above.
c             Otherwise BND(2,J) is the upper bound for X(J).
c             If BND(1,J) .ne. UNBND and BND(2,J) .ne. UNBND, require
c             BND(1,J) .le. BND(2,J).
c
c     UNBND   [In]  Special value to be used in BND(,) to indicate
c             unboundedness.
c
c     KPRINT  [in]  Larger values give more printing.
c           0:       No printing.
c           .ge. 1:  Print initial values of each component of X() and
c                    a line for each move of an index between Sets
c                    Z and P.
c           .ge. 2:  Print details showing how index is selected to
c                    move between Sets Z and P.
c           .ge. 3:  Use extra input items XT(), RT(), DIFF() to
c                    compute residuals at various stages to verify
c                    transformed problems stay consistent with the
c                    original problem.
c           .ge. 4:  Print full data array [A : b] at various stages.
c
c     TOL     [in]   Tolerance used in tests of variables against
c           their bounds, and in testing the right side of constraint
c           equations for zero.  Used in a relative sense in testing
c           against nonzero bounds, and in an absolute sense testing
c           against zero.  Suggest a value such as EPS**(0.75), however
c           a larger value has been found necessary to force
c           triangularization of the constraint equations in some cases.
c           Here EPS denotes the machine precision, i.e. the smallest
c           number EPS such that 1. + EPS is not 1.
c
c     IERR  [Out]  Status indicator on return.
c              = -1  Failed to fully triangularize first M1TRUE rows.
c                    Subroutine attempts to complete the computation,
c                    omitting the constraint rows not triangularized.
c                    User should check residuals of constraint rows.
c              = 0  No errors detected.
c              = 1  MTRUE .le. 0  or  N .le. 0
c              = 2  Inconsistent setting of bounds.
c              = 3  Too many iterations.  Nominal ITMAX = 5*N.
c
c     X()     [Out]  On entry X() need not be initialized.  On return,
c             X() will contain the solution N-vector.
c
c     RNORM   [Out]  The euclidean norm of the residual vector.
c
c     NSETS   [Out]  Indicates the No. of components of the solution
c             vector, X(), that are in Set S.  These values are obtained
c             by solving a system of equations, and thus will generally
c             not be at a bound.
c
c     W()     [Out]  An N-array of working space.  On return, W() will
c             contain the dual solution vector.  This will satisfy
c             W(i) = 0 for all i in Set S,
c             W(i) .le. 0 for all i in Set F, such that X(i) is at its
c             lower bound, and
c             W(i) .ge. 0 for all i in Set F, such that X(i) is at its
c             upper bound.
c
c     SIZE()  [Scratch] !!!  No longer needed  1992-10-12  !!!
c             Working space dimensioned at least M1TRUE.
c             SIZE(i) holds scaled roundoff error estimates for B(i).
c             The roundoff error estimate is TOL times SIZE(i).
c
c     TNORM()  [Scratch]  An N-array of working space.
c
c     Z()     [Scratch]  Work array of size min(MTRUE, N).  Used during
c             solution of triangular system to hold right-side vector
c             initially and the solution vector at conclusion.  Also
c             used during interpolation to restore feasibility.
c
c     CC(), SS()  [Scratch]  Work arrays of size min(MTRUE,N) each.
c              Used to hold cosines and sines of the Givens
c              transformations in order to support a column-oriented
c              implementation.
c
c     INDEX()   [Out]  An INTEGER working array of length at least N.
c                 On exit the contents of this array define the Sets
c                 S, F, and H.
c
c     JSTAT()  [Scratch]  Of length N.
c
c     XT(1:N), RT(1:MTRUE), DIFF(1:MTRUE)  For use in testing this subr.
c         Could be deleted in a production version.  These arrays are
c         accessed only if KPRINT .ge. 3.
c         On entry XT() and RT() should contain the known
c         solution and residual vectors respectively. Diff() is scratch.
c     ------------------------------------------------------------------
c                 Description of Sets S, F, and H.
c
c     The N components of X() are classified into three disjoint sets.
c
c                 INDEX(1)   thru INDEX(NSETS) = Set S.
c                 INDEX(IF1) thru INDEX(IF2)   = Set F.
c                 INDEX(IF2+1) thru INDEX(N)   = Set H.
c
c                 NF  = IF2 - IF1 + 1
c                 IF1 = NSETS + 1 = NSP1
c                 NSETS + NF .le. N
c
c     The sizes of sets S and F are given by NSETS and NF.
c     Set H consists of the components that are constrained to a
c     unique value by the given constraints.
c     Sets S and F contain components that a priori are allowed a
c     possible range of values of nonzero length.
c     During the computation, set F contains components that are
c     temporarily being held constant.
c     Set S contains components whose values are determined by solving
c     the currently triangularized subsystem, or by adjusting such a
c     solution set to shift its values from infeasible to feasible.
c     Members of set F are available for consideration for moving to
c     set S.
c     ------------------------------------------------------------------
c--S replaces "?": ?BLSE, ?ERV1, ?HTCC, ?ROTG, ?ROT, ?DOT
c--&                 ?NRM2, ?AXPY, ?COPY, ?SWAP, ?RANU
c     Other subprograms referenced directly: ERMOR, ERMSG, IERM1, IERV1
c     ------------------------------------------------------------------
c                         Interesting internal variables
c
c     MSG1  [logical]   Used to cause the error message associated with
c           IERR = -1 to be issued only the first time the error is
c           detected.  False means the message has not been issued.
c           True means it has.
c     ------------------------------------------------------------------
      external SDOT, SNRM2, SRANU
      real             SDOT, SNRM2, SRANU
      integer N
      integer BMODE, I, I1, IBOUND, ID, IERR
      integer IG1, IGAUS1, IGAUS2, IGIV1, IGIV2
      integer II, IIS, IMAX, INDEX(N), IS, ISNEW, IPUT, IPUT1, IPUT2
      integer IR, IRLAST, ITEMP, ITER, ITMAX, IF0, IF1, IF2, IFNEW
      integer J, JCOL, JD, JJ, JJS, JS, JPIV, JSNEW, JPUT
      integer JSTAT(N), JF, JFNEW
      integer KPRINT, KSTEP, L, LBOUND, LDA, LEN
      integer M, M1, M1P1, M1TRUE, MDB, MINMN, MTRUE
      integer NSP1, NSETS, NF

      real             A(LDA,N), B(MTRUE), X(N), W(N), Z(*), BND(2,N)
      real             UNBND, SIZE(*), TNORM(N), CC(*), SS(*)
      real             A1, ABSA1, ALPHA, AMAX
      real             B1, BNDL, BSAVE, BSIZE, BTEMP
      real             DIFF(MTRUE), FAC
      real             HALF, ONE, RANGE, RNORM, RT(MTRUE)
      real             T, TEMP, TMAX, TOL, TWO, UP, WMAX
      real             XT(N), XTRY
      real             ZERO
      logical CMODE, FIND, FREE1, FREE2, FREE, FREEJS(2)
      logical HITBND, MOVEJF, MSG1
      parameter(ZERO = 0.0E0, TWO = 2.0E0)
      parameter(ONE = 1.0E0, HALF = 0.5E0)
c NPR Variables not declared by SFTRAN
      integer NPR004, NPR005, NPR009, NPR012, NPR014, NPR021, NPR022
c     ------------------------------------------------------------------
      GO TO 30001
20002 if( IERR .ne. 0 ) return
c
      IF (.NOT.( M1 .gt. 0)) GO TO 20005
      GO TO 30002
c
c           Here all singletons with the right signs have been brought
c           into Set S.  If NSETS .lt. M1 we must build the triangle
c           further.
c
20006 IF (.NOT.(NSETS .lt. M1)) GO TO 20008
               CMODE = .true.
      GO TO 30003
20009 CONTINUE
20008 ASSIGN 20010 TO NPR004
      GO TO 30004
20010 CONTINUE

20005 IF (IERR .gt. 0 .or. NSETS .eq. MINMN .or. NF .eq. 0) GO TO 20003

      DO 20011 JF = 1, N
      IF(X(JF) .ne. ZERO)THEN
               call SAXPY(M-M1, -X(JF), A(M1P1,JF), 1, B(M1P1), 1)
      END IF
20011 CONTINUE
         CMODE = .false.
      IF (.NOT.(M1 .gt. 0)) GO TO 20017
            IG1 = 1
      ASSIGN 20018 TO NPR005
      GO TO 30005
20018 CONTINUE

20017 ASSIGN 20019 TO NPR004
      GO TO 30004
20019 CONTINUE

      GO TO 30006
20020 ASSIGN 20021 TO NPR004
      GO TO 30004
20021 CONTINUE
20003 GO TO 30007
20022 return
c
c     ------------------------------------------------------------------

C     procedure( INITIALIZE )
c
30001 M = MTRUE
      M1 = M1TRUE
      IF(M .le. 0 .or. N .le. 0)THEN
         IERR = 1
         call IERM1('SBLSE',IERR,0,
     *   'Require M > 0 and N > 0.',  'M',M,',')
         call IERV1('N',N,'.')
         return
      END IF
      IERR = 0
      ITER=0
      ITMAX = 6*N
      MSG1 = .false.
      M1P1 = M1+1
      MINMN = min(M,N)
      IF2=N
      IF1=1
      NSETS=0
      NSP1=1
c     .        Initialize INDEX() and set W() = zero.  Setting W() =
c     .        zero is not needed for the functioning of this
c     .        subroutine, it is just done so the W() array will not
c     .        have garbage in it if debugging printing is used.
c
      DO 20025 J=1,N
         INDEX(J)=J
         W(J) = ZERO
20025 CONTINUE
c
c     .                         BEGIN..  LOOP ON IF0 TO INITIALIZE  X()
      IF0=IF1
20028 IF( IF0  .le.  IF2 )THEN
      J=INDEX(IF0)
      IF( BND(1,J)  .eq.  UNBND)THEN
      IF(BND(2,J)  .eq.  UNBND)THEN
            X(J) = ZERO
      ELSE
            X(J) = min(ZERO,BND(2,J))
      END IF
      ELSEIF( BND(2,J)  .eq.  UNBND)THEN
         X(J) = max(ZERO,BND(1,J))
      ELSE
         RANGE = BND(2,J) - BND(1,J)
      IF( RANGE  .eq.  ZERO )THEN
c
c           .  Here X(J) is constrained to a single value.
c
            INDEX(IF0)=INDEX(IF2)
            INDEX(IF2)=J
            IF0=IF0-1
            IF2=IF2-1
            W(J)=ZERO
            X(J)=BND(1,J)
      ELSEIF( RANGE  .gt.  ZERO)THEN
c
c           .   The following statement sets X(J) to 0 if the
c           .   constraint interval includes 0, and otherwise sets
c           .   X(J) to the endpoint of the constraint interval
c           .   that is closest to 0.
c
            X(J) = max(BND(1,J), min(BND(2,J),ZERO))
      ELSE
c                                 ****** Error:  IERR = 2 ******
            IERR = 2
            call IERM1('SBLSE',IERR,0,
     *      'Inconsistent setting of bound for X(J).', 'J',J,',')
            call SERV1('UNBND',UNBND,',')
            call SERV1('BND(1,J)',BND(1,J),',')
            call SERV1('BND(2,J)',BND(2,J),',')
      GO TO 31001
      END IF
      END IF
      if(KPRINT .ge. 1)
     * print '(1x,a,i4,a,g14.6)','Initializing:  J=',J,',  X(J)=',X(J)
c
         IF0=IF0+1
      GO TO 20028
      END IF
c        .                         END.. LOOP ON IF0 TO INITIALIZE  X()
      NF = IF2 - IF1 + 1
31001 GO TO 20002
c     ------------------------------------------------------------------

C     procedure( TRIANGULARIZE USING SINGLETONS )
c
c     Search for singletons.  Use them to start the triangle if
c     their signs are right.
c
30002 ITEMP = IF1
      DO 20038 IF0 = ITEMP, IF2
      IF (NSETS .eq. M1) GO TO 20039
         JF = INDEX(IF0)
c
c        .  The following loop sets II.
c        .  If A(1..M, JF) contains exactly one nonzero, II will
c        .  be set to the row index of that element.
c        .  Otherwise II will be set to zero.
c
         II = 0
      DO 20041 I = 1, M
      IF(A(I,JF) .ne. ZERO)THEN
      IF(II .ne. 0)THEN
                  II = 0
      GO TO 20042
      END IF
               II = I
      END IF
20041 CONTINUE
20042 CONTINUE
c
      IF(II .gt. NSETS .and. II .le. M1)THEN
            BTEMP = B(II) - SDOT(N, A(II,1), LDA, X, 1)
            XTRY = X(JF) + BTEMP / A(II,JF)
      IF((BND(1,JF) .eq. UNBND .or. XTRY .ge. BND(1,JF)) .and.
     *   (BND(2,JF) .eq. UNBND .or. XTRY .le. BND(2,JF)))THEN
               MOVEJF = .true.
      ELSE
               BSIZE = abs(B(II))
      DO 20052 J = 1,N
                  BSIZE = BSIZE + abs(A(II,J)*X(J))
20052 CONTINUE
      IF(abs(BTEMP) .lt. TOL * BSIZE)THEN
                  MOVEJF = .true.
                  XTRY = X(JF)
      ELSE
                  MOVEJF = .false.
      END IF
      END IF

      IF(MOVEJF)THEN

c
c              .  Move JF from Set F to Set S.
c
               X(JF) = XTRY
      IF(II .ne. NSP1)THEN
                  call SSWAP(N, A(II,1),LDA, A(NSP1,1),LDA)
                  B(II) = B(NSP1)
      END IF
               B(NSP1) = ZERO
      IF(IF0 .ne. IF1)THEN
                  INDEX(IF0) = INDEX(IF1)
                  INDEX(IF1) = JF
      END IF
               NSETS = NSP1
               NSP1 = NSETS + 1
               IF1 = NSP1
               NF = NF -1
      END IF
      END IF
20038 CONTINUE
20039 CONTINUE
      if(KPRINT .ge. 1) print '(1x,a,i4)',
     *   'Started using singletons.  NSETS=',NSETS
      GO TO 20006
c     ------------------------------------------------------------------

C     procedure( ALGORITHM BVEQ )
c
c     BVEQ == Bounded Variables solution of linear EQuations.
c     Solve this problem using rows 1 through M1 and the
c     variables indexed in Set S and Set F.
c     This proc will decrease the values of M1 and M if rows of the
c     equality constraint system are linearly dependent on earlier rows.
c
30003 if(KPRINT .ge. 1)
     * print '(1x/1x,a,a,i5,a,i5)',
     * 'Entering Proc BVEQ..  ', 'NSETS=',NSETS,',  NF=',NF

      KSTEP = NSP1
20063 IF (.NOT.(KSTEP .le. M1)) GO TO 20064
      GO TO 30008
20065 CONTINUE


20066 CONTINUE
c
c     .  Quit on error flag, or
c     .  if Set F is empty.
c
      IF (.NOT.(NF .eq. 0)) GO TO 20069
      IF (.NOT.(NSETS .lt. M1)) GO TO 20070
      ASSIGN 20070 TO NPR009
      GO TO 30009
20070 GO TO 20064
20069 IF (.NOT.(IERR .gt. 0  )) GO TO 20071
      GO TO 20064
20071 CONTINUE
c
      GO TO 30010
20072 IF (.NOT.(FIND)) GO TO 20074
      GO TO 30011
c
c        .  Delta_x is now in Z().
c
c        .  The following proc may move one variable from Set S
c        .  to Set F.  All variables remaining in Set S when it
c        .  completes will be feasible.  May set IERR positive.
c
20075 ASSIGN 20076 TO NPR012
      GO TO 30012
20076 IF(.not. HITBND)THEN
            KSTEP = KSTEP + 1
      GO TO 20067
      END IF
      GO TO 20073
c
c        .  The following proc deletes a constraint row that appears
c        .  to be dependant on preceeding constraint rows or
c        .  inconsistent with preceeding constraint rows.  It also
c        .  decrements M1 and M.  Will set IERR negative.
c
20074 ASSIGN 20079 TO NPR009
      GO TO 30009
20079 GO TO 30013
20080 GO TO 20067
c
20073 GO TO 20066
20067 CONTINUE
      GO TO 20063
c
20064 GO TO 20009
c     ------------------------------------------------------------------

C     procedure( BVEQ: SELECT ANOTHER COEF TO SOLVE FOR )
c
c     .    Search thru Set F for a new coef to solve for.
c     Pick column having elt of largest magnitude in row NSP1
c     among those having acceptable sign.
c     On entry BSAVE contains the original input value of B(NSP1), and
c     B(NSP1) contains the residual for row NSP1 using the current X().
c     Index JF = INDEX(IF0) is acceptable
c     if X(JF) is free to increase and B(NSP1)/A(NSP1,JF) .ge. zero, or
c     if X(JF) is free to decrease and B(NSP1)/A(NSP1,JF) .le. zero.
c
c     .  If no new coeff is found, then check abs(B(NSP1)) for being
c     small enough that it might be zero apart from roundoff error.
c     If not, set FIND = false and exit.
c     If so, set B(NSP1) = zero and try the search again.
c
c     .    We wish to force the maximum number of
c     components into the solution, and thus we accept components that
c     will enter with a zero change as well as those that will enter
c     with a change in the acceptable direction.
c     .    If a coef is selected, set FIND = true.
c     The index of the selected coef is JSNEW = INDEX(ISNEW).
c     .    If no coef is selected, set FIND = false.
c
30010 FIND = .false.
      AMAX = TOL
      ISNEW = 0
      B1 = B(NSP1)
      IF(KPRINT .ge. 4)THEN
            print '(1x/1x,a/1x)','Before selecting next JSNEW.  [A:b] ='
      DO 20083 I = 1,NSETS+1
               print '(1x,i3,2x,5g13.6/(6x,5g13.6))',
     *          I,(A(I,J),J=1,N), B(I)
20083 CONTINUE
      END IF
c
      DO 20086 BMODE = 1,2
      if(KPRINT .ge. 2) print '(4x,a,g14.6,a,i2/4x,a,i4/4x,a)',
     * 'Scan Set F with B(NSP1) = ', B1,',   BMODE = ',BMODE,
     * '                  NSP1 = ',NSP1,
     *' -- JF,A(NSP1,JF),FREE1,FREE2,FREE,         JFbest --'
c
c     .                                 Begin loop through Set F.
      DO 20089 IF0=IF1,IF2
         JF=INDEX(IF0)
         A1 = A(NSP1,JF)
         ABSA1 = abs(A1)
      IF(ABSA1 .le. AMAX)THEN
            if(KPRINT .ge. 2) print '(4x,i4,g14.6)',JF, A1
      ELSE
c
c        .  Set FREE1 = true if X(JF) is not at the left end-point of
c        .  its constraint region.
c        .  Set FREE2 = true if X(JF) is not at the right end-point of
c        .  its constraint region.
c        .  Set FREE = true if X(JF) is not at either end-point of its
c        .  constraint region.
c
         FREE1 = BND(1,JF) .eq. UNBND  .or.  X(JF) .ne. BND(1,JF)
         FREE2 = BND(2,JF) .eq. UNBND  .or.  X(JF) .ne. BND(2,JF)
         FREE = FREE1 .and. FREE2
c
      IF( FREE )THEN
            AMAX = ABSA1
            ISNEW = IF0
      ELSEIF( FREE1 )THEN
      IF(B1/A1 .le. ZERO)THEN
               AMAX = ABSA1
               ISNEW = IF0
      END IF
      ELSEIF( FREE2 )THEN
      IF(B1/A1 .ge. ZERO)THEN
               AMAX = ABSA1
               ISNEW = IF0
      END IF
      END IF
      IF(KPRINT .ge. 2)THEN
      IF(ISNEW .eq. 0)THEN
               print '(4x,i4,g14.6,3L2)',
     *         JF,A1,FREE1,FREE2,FREE
      ELSE
               print '(4x,i4,g14.6,3L2,16x,i4)',
     *         JF,A1,FREE1,FREE2,FREE,       INDEX(ISNEW)
      END IF
      END IF
      END IF
20089 CONTINUE
c
      IF (AMAX .ne. TOL) GO TO 20087
      IF(BMODE .eq. 1 .and. B1 .ne. ZERO)THEN
         BSIZE = abs(BSAVE)
      DO 20108 J = 1,N
            BSIZE = BSIZE + abs(A(NSP1,J)*X(J))
20108 CONTINUE

      IF(abs(B1) .le. TOL * BSIZE)THEN
      IF(KPRINT .ge. 2)THEN
               print'(1x,a,i4/1x,a,g14.6,a,g14.6)',
     *         'Setting B(NSP1) = 0.,   NSP1=',NSP1,
     *         '    Old B(NSP1) =',B(NSP1),',   TOL*BSIZE =', TOL*BSIZE
      END IF
            B(NSP1) = ZERO
            B1 = ZERO
      GO TO 20086
      END IF
      END IF
      FIND = .false.
      GO TO 31010
20086 CONTINUE
20087 CONTINUE
c     .                          Selected index is JSNEW = INDEX(ISNEW)
      FIND = .true.
      JSNEW = INDEX(ISNEW)
      FREEJS(1) = BND(1,JSNEW)  .eq.  UNBND   .or.
     *            X(JSNEW)  .ne.  BND(1,JSNEW)
      FREEJS(2) = BND(2,JSNEW)  .eq.  UNBND   .or.
     *            X(JSNEW)  .ne.  BND(2,JSNEW)
31010 GO TO 20072
c     ------------------------------------------------------------------

C     procedure( BVEQ: MOVE JSNEW FROM SET F TO SET S )
c
c     The index  JSNEW=INDEX(IFNEW)  has been selected to be moved from
c     Set F to Set S.
c     B(1..M) has been adjusted as though X(JSNEW) = 0.
c     Update indices moving JSNEW from Set F to Set S.
c     Do one stage of Gaussian elimination in the upper matrix.
c     Copy B() to Z() and solve for Delta_x, leaving it in Z().
c     Set W(JSNEW) = 0.
c
30011 if(KPRINT .ge. 1) print '(1x,a,i4)',
     * '==>  Move index from F to S.  JSNEW  =',JSNEW
c
c
      INDEX(ISNEW)=INDEX(IF1)
      INDEX(IF1)=JSNEW
      IF1=IF1+1
      NSETS=NSP1
      NSP1=NSP1+1
      NF = NF - 1
      LEN = M1 - NSETS
c
      W(JSNEW)=ZERO
      call SCOPY(NSETS, B, 1,  Z, 1)
      ASSIGN 20115 TO NPR014
      GO TO 30014
20115 GO TO 20075
c     ------------------------------------------------------------------
C     procedure( MSG FOR IERR = -1 )

30009 IERR = -1
      IF(.not. MSG1)THEN
         MSG1 = .true.
         call ERMSG('SBLSE', IERR, 0,
     *   'Failed to fully triangularize the equality constraint rows.',
     *   ',')
         call ERMOR(
     *   'Could be due to dependent or inconsistent constraints.',',')
         call ERMOR(
     *   'The computation will be completed, ignoring the',',')
         call ERMOR(
     *    'un-triangularized constraint rows.  User should check',',')
         call ERMOR('residuals of constraint equations.','.')
      END IF
      GO TO NPR009,(20070,20079)
c     ------------------------------------------------------------------
C     procedure( REARRANGE ROWS )

C     Here when Row NSP1 of A() appears to be either dependent or
c     inconsistent relative to preceeding constraint rows.
c     Delete Row NSP1.
c     Have NSP1 .le. M1.
c     If NSP1 < M1 copy row M1 to row NSP1.
c     Copy row M to row M1.
c     Decrease M1 and M each by 1.
c
c     .              If NSP1 < M1 copy row M1 to row NSP1.
c
30013 IF(NSP1 .lt. M1)THEN
         call SCOPY(N, A(M1,1), LDA, A(NSP1,1), LDA)
         B(NSP1) = B(M1)
         RT(NSP1) = RT(M1)
      END IF
c     .                              Copy row M to row M1.
c
         call SCOPY(N, A(M,1), LDA, A(M1,1), LDA)
         B(M1) = B(M)
         RT(M1) = RT(M)
c     .                              Decrement M1 and M.
      M = M-1
      M1 = M1-1
      M1P1 = M1+1
      MINMN = min(M,N)
      GO TO 20080
c     ------------------------------------------------------------------

C     procedure( ALGORITHM BVLS )
c
c     .   BVLS == Bounded Variables Least Squares
c     Solve this problem using rows 1 through M and the
c     variables indexed in Set S and Set F.
c
30006 if(KPRINT .ge. 1)
     * print '(1x/1x,a,a,L2, a,i5,'',''/23x,2(a,i5))',
     * 'Entering Proc BVLS..  ',
     * 'CMODE=',CMODE,', M=',M,
     * 'NSETS=',NSETS,', NF=',NF

20120 CONTINUE
c
c     .  Quit if Set F is empty, or
c     .  if M cols have been triangularized, or
c     .  on error flag.

      IF ( NF .eq. 0  .or.  NSETS .eq. M ) GO TO 20121
c
      GO TO 30015
c
c     .  If no index was found to be moved from set Z to set P,
c     .  then go to termination.
c
20122 IF ( .not. FIND ) GO TO 20121
c
      GO TO 30016
c
20123 CONTINUE
20124 CONTINUE
c
c        When this loop is entered, NSETS will be > M1.
c        NSETS decreases by one on each repetition of this loop, but
c        will still be > M1  at the beginning of every
c        repetition of this loop.
c
            call SCOPY(NSETS, B, 1, Z, 1)
      ASSIGN 20126 TO NPR014
      GO TO 30014
c
20126 ASSIGN 20127 TO NPR012
      GO TO 30012
c
c           .  The above proc will set IERR nonzero if
c           .  the iteration count is exceeded.
c           .  Otherwise the proc will either set HITBND := true and
c           .  decrement NSETS, or set HITBND := false and leave
c           .  NSETS unchanged.  NSETS will be .ge. M1.
c
20127 IF (IERR .gt. 0) GO TO 20121
      IF (.not. HITBND .or. NSETS .eq. M1) GO TO 20125
      GO TO 20124
20125 CONTINUE
c
c     .  All coeffs in Set S are feasible.  Loop back.
      GO TO 20120
20121 CONTINUE
c
      GO TO 20020
c     ------------------------------------------------------------------

C     procedure( BVLS: SELECT COEF TO SOLVE FOR )
c
c     .    Search thru Set F for a new coef to solve for.
c     First compute dual coeff, W(J) = negative gradient.
c     Next classify the members of Set F by setting ISTAT().
c     .   ISTAT()   Meaning
c     .       -1    X(J) is rejected for moving to Set S.  Either
c     .             W(J) = 0, or X(J) is at a bound and the sign of W(J)
c     .             indicates the objective function will increase if
c     .             X(J) is moved away from the bound.
c     .        1    X(J) is accepted for further consideration
c     .             as a candidate for moving to Set S.  W(J) is
c     .             nonzero and if X(J) is at a bound, the sign of W(J)
c     .             indicates moving away from the bound will decrease
c     .             the objective function.
c
c     Among those with ISTAT(J) > 0, choose one having column norm
c     exceeding a locally determined tolerance, and among these the
c     largest abs(W(J)).
c     .    Note that when the sign of W(J) is uncertain we reject
c     moving X(J) to Set S.  This biases the method toward keeping
c     variables in Set F when in doubt.
c     .    If a coef is selected, set FIND = true.
c     The index of the selected coef is JSNEW = INDEX(ISNEW).
c     .    If no coef is selected, set FIND = false.
c
30015 FIND = .FALSE.
      TMAX = ZERO
      LEN = M - NSETS
      IF(KPRINT .ge. 4)THEN
            print '(1x/1x,a/1x)', 'Before computing W(): [A:b] ='
      DO 20130 I = 1,M
               print '(1x,i3,2x,5g13.6/(6x,5g13.6))',
     *          I,(A(I,J),J=1,N), B(I)
20130 CONTINUE
      END IF
c
         if(KPRINT .ge. 2) print '(4x,a)','Scan Set F:',
     *' -- J,FREE1,FREE2,FREE, W(J),       JSTAT(J), TNORM(J), W(+) --'
c
c     .                                 Begin loop through Set F.
      DO 20133 IF0=IF1,IF2
         J=INDEX(IF0)
c
c        .  Set FREE1 = true if X(J) is not at the left end-point of
c        .  its constraint region.
c        .  Set FREE2 = true if X(J) is not at the right end-point of
c        .  its constraint region.
c        .  Set FREE = true if X(J) is not at either end-point of its
c        .  constraint region.
c
         FREE1 = BND(1,J)  .eq.  UNBND   .or.   X(J)  .ne.  BND(1,J)
         FREE2 = BND(2,J)  .eq.  UNBND   .or.   X(J)  .ne.  BND(2,J)
         FREE = FREE1 .and. FREE2
c
c        .             Compute dual coefficient W(J).
c
         W(J) = SDOT(LEN, A(NSP1,J), 1, B(NSP1),1)
c
c        .  The following code sets JSTAT(J) and computes TNORM(J)
c        .  and TMAX.
c        .  The columns whose dual signs are ok to be moved into Set S
c        .  will be identified by JSTAT(J) = 1. Those not ok
c        .  have JSTAT(J) = -1.
c        .     For the ok columns the norm is computed and stored in
c        .  TNORM(J) and the max of these is accumulated in TMAX.
c
         JSTAT(J) = -1
      IF(W(J) .ne. ZERO)THEN
      IF( FREE )THEN
            JSTAT(J) = 1
            TNORM(J) = SNRM2(LEN, A(NSP1,J), 1)
            TMAX = max(TMAX, TNORM(J))
      ELSEIF( FREE1 )THEN
      IF(W(J) .lt. ZERO)THEN
               TNORM(J) = SNRM2(LEN, A(NSP1,J), 1)
               TMAX = max(TMAX, TNORM(J))
               JSTAT(J) = 1
      END IF
      ELSEIF( FREE2 )THEN
      IF(W(J) .gt. ZERO)THEN
               TNORM(J) = SNRM2(LEN, A(NSP1,J), 1)
               TMAX = max(TMAX, TNORM(J))
               JSTAT(J) = 1
      END IF
      END IF
      END IF

      IF(KPRINT .ge. 2)THEN
      IF(JSTAT(J) .eq. 1)THEN
               print '(4x,i4,3L2, g14.6,i4,g14.6)',
     *         J,FREE1,FREE2,FREE,W(J),     JSTAT(J),TNORM(J)
      ELSE
               print '(4x,i4,3L2, g14.6,i4)',
     *         J,FREE1,FREE2,FREE,W(J),     JSTAT(J)
      END IF
      END IF
20133 CONTINUE
c
      IF(TMAX .eq. ZERO)THEN
         FIND = .false.
      GO TO 31015
      END IF
      FIND = .true.
c
c        .     Among the ok columns having norm .ge. half*TMAX
c        .     find one having largest abs(W(J)).
c
      WMAX = ZERO
      DO 20152 IF0 = IF1, IF2
         J = INDEX(IF0)
      IF(JSTAT(J) .gt. 0)THEN
      IF(TNORM(J) .ge. HALF*TMAX)THEN
      IF(abs(W(J)) .ge. WMAX)THEN
                  ISNEW = IF0
                  WMAX = abs(W(J))
      END IF
      END IF
      END IF
20152 CONTINUE
c     .                          Selected index is JSNEW = INDEX(ISNEW)
      JSNEW = INDEX(ISNEW)
      FREEJS(1) = BND(1,JSNEW)  .eq.  UNBND   .or.
     *            X(JSNEW)  .ne.  BND(1,JSNEW)
      FREEJS(2) = BND(2,JSNEW)  .eq.  UNBND   .or.
     *            X(JSNEW)  .ne.  BND(2,JSNEW)
31015 GO TO 20122
c     ------------------------------------------------------------------

C     procedure( BVLS: MOVE JSNEW FROM SET F TO SET S )
c
c     The index  JSNEW=INDEX(IF0)  has been selected to be moved from
c     Set F to Set S.
c     Compute new Householder transformation.  Apply it to A and b.
c     Update indices defining Sets P and Z.
c     Zero subdiagonal elts in col JSNEW,  Set W(JSNEW)=0.
c
30016 if(KPRINT .ge. 1) print '(1x,a,i4)',
     * '==>  Move index from F to S.  JSNEW  =',JSNEW
c
c     .  Compute new Householder transformation and apply it to B().
c
      call SHTCC(1,NSP1,NSP1+1,M,A(1,JSNEW),  UP,B,M,1)
c
c     .                                  Update pointers.
      INDEX(ISNEW)=INDEX(IF1)
      INDEX(IF1)=JSNEW
      IF1=IF1+1
      NSETS=NSP1
      NSP1=NSP1+1
      NF = NF - 1
c
      DO 20161 JF=IF1,IF2
             JJ=INDEX(JF)
             call SHTCC (2,NSETS,NSP1,M,A(1,JSNEW),
     *          UP,A(1,JJ),LDA,1)
20161 CONTINUE
         if(KPRINT .ge. 3)
     *       call SHTCC (2,NSETS,NSP1,M,A(1,JSNEW),  UP,RT(1),M,1)
c
      DO 20164 L=NSP1,M
             A(L,JSNEW)=ZERO
20164 CONTINUE
      W(JSNEW)=ZERO
      GO TO 20123
c     ------------------------------------------------------------------

C     procedure( CONTROL CONSTRAINT TESTING FOR SET S )
c
c     On entry, the Delta_x obtained by solving the current Set S
c     is in the array Z().
c     If X + Delta_x satisfies the bounds, this proc returns with
c        X := X + Delta_x,   HITBND := false,
c        B(min(M1,NSETS):NSETS) := 0.
c     Otherwise this proc returns with
c        X := X + ALPHA * Delta_x,  HITBND := true,
c        B(min(M1,NSETS):NSETS) := (1 - ALPHA)*B(min(M1,NSETS):NSETS),
c        assigns values to IFNEW and IBOUND, and decrements NSETS.
c
30012 IF(KPRINT .ge. 2)THEN
         print '(1x,a)','   Solved Set S obtaining Delta_x:'
      DO 20169 I = 1,NSETS
            print '(7x,a,i4,g14.6,a,g14.6)',
     *         'J,X(J)=',INDEX(I),X(INDEX(I))+Z(I),', D_X=', Z(I)
20169 CONTINUE
      END IF

      ITER=ITER+1
      IF(ITER  .gt.  ITMAX)THEN
c        .                             ****** Error: IERR = 3 ******
         IERR = 3
         call IERM1('SBLSE',IERR,0,
     *   'Iteration count ITER exceeds ITMAX.','ITER',ITER,',')
         call IERV1('ITMAX',ITMAX,'.')
      GO TO 31012
      END IF
c
      GO TO 30017
c
c     The above proc call sets HITBND.  If HITBND = true then it
c     also sets ALPHA, IFNEW, and IBOUND.
c
20174 IF (.NOT.( HITBND )) GO TO 20176
c
         JFNEW=INDEX(IFNEW)
         if(KPRINT .ge. 2) print '(1x,a,i4,a,i4,a/15x,a,g14.6)',
     *   '   Hit a bound.  IFNEW=',IFNEW,
     *   ',  JFNEW=INDEX(IFNEW)=',JFNEW,',','ALPHA=',ALPHA
c
c        Here ALPHA will be between 0 and 1 for use in computing
c             X()  :=  X() + ALPHA * Delta_x.
c
      DO 20177 IS=1,NSETS
            L=INDEX(IS)
            X(L) = X(L) + ALPHA * Z(IS)
            if(KPRINT .ge. 2) print '(1x,a,i4,g14.6)',
     *         '   Interpolate to feasibility:  L,X(L)=',L,X(L)
20177 CONTINUE
      DO 20180 I = min(M1P1, NSETS), NSETS
            B(I) = (ONE - ALPHA) * B(I)
20180 CONTINUE
c
      GO TO 30018
20183 GO TO 20175
20176 DO 20184 IS=1,NSETS
            L=INDEX(IS)
            X(L) = X(L) + Z(IS)
20184 CONTINUE
      DO 20187 I = min(M1P1, NSETS), NSETS
            B(I) = ZERO
20187 CONTINUE
c
20175 IF (.NOT.(NSETS .gt. 0)) GO TO 20191
      GO TO 30019
c
20192 CONTINUE
20191 CONTINUE
31012 GO TO NPR012,(20076,20127)
c     ------------------------------------------------------------------

C     procedure( SEE IF ALL CONSTRAINED COEFFS ARE FEASIBLE )
c
c     The Delta_x vector is currently in the array Z().
c     See if each x + Delta_x in Set S is in the closure of its
c     constraint region.
c     If so, set HITBND = false.
c     If not, set HITBND = true, and also set ALPHA, IFNEW, and IBOUND.
c     Then ALPHA will satisfy  0. .le. ALPHA .lt. 1.
c
30017 ALPHA=TWO
      DO 20193 IS=1,NSETS
          L=INDEX(IS)
          XTRY = X(L) + Z(IS)
      IF(BND(1,L) .ne. UNBND .and. XTRY .lt. BND(1,L))THEN
c            .                               XTRY crosses lower bound.
             LBOUND=1
      ELSEIF(BND(2,L) .ne. UNBND .and. XTRY .gt. BND(2,L))THEN
c            .                               XTRY crosses upper bound.
             LBOUND=2
      ELSE
         LBOUND = 0
      END IF
c
      IF( LBOUND  .ne.  0 )THEN
         BNDL = BND(LBOUND,L)
         T = (BNDL - X(L)) / (XTRY - X(L))
      IF(KPRINT .ge. 2)THEN
            print'(1x,a,i4,a,i4,a,g15.7/1x,a,g15.7,a,g15.7,a,g15.7)',
     *   'L=',L,',   LBOUND=',LBOUND,',   BND(LBOUND,L)=',BNDL,
     *   'X(L)=',X(L),',   XTRY=',XTRY,',   T=',T
      END IF
      IF(T .le. ALPHA )THEN
      IF( T .gt. ONE - TOL  .or.
     *  (L .eq. JSNEW .and. .not. FREEJS(LBOUND)) .or.
     *  (BNDL .eq. ZERO .and. abs(XTRY) .le. TOL) .or.
     *  (BNDL .ne. ZERO .and. abs(XTRY - BNDL) .le. TOL*abs(BNDL))
     *  )THEN
c
c              .  If the test of T is true
c              .  we assume XTRY differs from its bound only due
c              .  to roundoff.  To avoid having this cause a move
c              .  from Set S to Set F we set XTRY to its bound and
c              .  ignore this value of T.
c              .  Similarly, if the tests of L and FREEJS() are
c              .  passed, it means the
c              .  coeff JSNEW, which is the coeff most recently
c              .  moved into P, has crossed the bound it was on just
c              .  before it was moved into P.  We assume this is due
c              .  to roundoff error and adjust the value back to this
c              .  bound.
c              .  Similarly if the other tests are passed we assume
c              .  XTRY differs from its bound only due to roundoff.
c
               if(KPRINT .ge. 1) print '(4x,a,a/4x,a,i4,a,i4,a,g16.8)',
     *            'While testing against bounds,',
     *            ' adjust XTRY for roundoff error.',
     *            'IS=',IS,',  L=INDEX(IS)=',L,',  Old XTRY=',XTRY
               Z(IS) = BND(LBOUND,L) - X(L)
               if(KPRINT .ge. 1) print '(30x,a,g16.8)',
     *            ',  New XTRY=', X(L) + Z(IS)
      ELSE
      IF(ALPHA .eq. ZERO)THEN
c
c                 .  Here T must be zero, and we have already saved a
c                 .  previous value of IS (into IFNEW) associated with
c                 .  a zero value of T.
c                 .  To reduce further the unlikely chance of
c                 .  cycling, we make a random selection between the
c                 .  current IS and the earlier saved one.  As there is
c                 .  no need to continue the IS loop, we exit here.
c
      IF(SRANU() .lt. HALF)THEN
                     IFNEW = IS
                     IBOUND = LBOUND
      END IF
      GO TO 20194
      ELSE
                  ALPHA=T
                  IFNEW=IS
                  IBOUND=LBOUND
      END IF
      END IF
      END IF
      END IF
20193 CONTINUE
20194 CONTINUE
      HITBND = ALPHA  .ne.  TWO
      GO TO 20174
c     ------------------------------------------------------------------

C     procedure( PERTURB ANY OUTLIERS IN SET S TO THE BOUNDARY )
c
c     .  See if the remaining coeffs in set P are feasible.
c     They should be because of the way alpha was determined.
c     If any are infeasible it is due to round-off error.
c     Any that are infeasible or on a boundary will be set to
c     the boundary value and kept in set P.
c
30019 DO 20211 IS=1,NSETS
               IBOUND = 0
               JS=INDEX(IS)
      IF(BND(1,JS)  .ne.  UNBND .and.
     *   X(JS)  .lt.  BND(1,JS))THEN
                  IBOUND=1
      ELSEIF(BND(2,JS) .ne. UNBND .and.
     *  X(JS) .gt. BND(2,JS))THEN
                  IBOUND=2
      END IF
      IF(IBOUND .ne. 0)THEN
c
c              .  The current coeff that appears to have crossed a
c              .  bound should, at worst be on its bound.
c              .  We assume this is due to roundoff error and adjust
c              .  the value back to the bound.

      IF(KPRINT .ge. 1)THEN
                  print '(4x,a,a/4x,a,i4,a,g16.8)',
     *            'While testing against bounds,',
     *            ' adjust X(JS) for roundoff error.',
     *            'JS=',JS,',  Old X(JS)=',X(JS)
                  print '(10x,a,g16.8)',',   New X(JS)=',BND(IBOUND,JS)
      END IF
               X(JS) = BND(IBOUND,JS)
      END IF
20211 CONTINUE
      GO TO 20192
c     ------------------------------------------------------------------

C     procedure( MOVE COEF JFNEW FROM SET S TO SET F )
c
c     Here we have JFNEW = INDEX(IFNEW) and this coeff has just hit its
c     bound indexed by IBOUND.
c     This proc is ref'd by Proc CONTROL CONSTRAINT TESTING, which is
c     ref'd by Proc BVEQ and Proc BVLS.
c     When reached via BVEQ we have CMODE = true
c     When reached via BVLS we have CMODE = false
c
30018 X(JFNEW)=BND(IBOUND,JFNEW)
      IF(KPRINT .ge. 1)THEN
         print '(1x,a,i5)','==>  Move index from S to F.  JFNEW =',JFNEW
         if(KPRINT .ge. 2) print '(27x,a,g14.6)','X(JFNEW) =',X(JFNEW)
      END IF
      NSP1=NSETS
      NSETS=NSETS-1
      NF = NF + 1
      IF1=IF1-1
c     .             If IFNEW .eq. NSETS+1 there is nothing more to do.
c
      IF (.NOT.(IFNEW .ne. NSP1)) GO TO 20224
c     .                                Shift indices in Set S.
      DO 20225 IS = IFNEW, NSETS
         INDEX(IS) = INDEX(IS+1)
20225 CONTINUE
      INDEX(IF1)=JFNEW
c
c     .  Here the columns INDEX(IFNEW .. NSETS) are of lengths
c     .  IFNEW+1 .. NSETS+1, respectively.  We must eliminate the
c     .  last element of each of these columns.
c
      IF (.NOT.(IFNEW .lt. M1)) GO TO 20229
         IGAUS1 = IFNEW
         IGAUS2 = min(NSETS-1, M1-1)
      GO TO 30020
20230 IF (.NOT.(NSETS .le. M1)) GO TO 20232
c
c           .  Use diag elt (NSETS,NSETS) to eliminate elt below it.
c
            JS = INDEX(NSETS)
      IF(A(NSP1, JS) .ne. ZERO)THEN
               FAC = -A(NSP1,JS)/A(NSETS,JS)
               call SAXPY(N, FAC, A(NSETS,1), LDA, A(NSP1,1), LDA)
               A(NSP1,JS) = ZERO
               B(NSP1) = B(NSP1) + FAC * B(NSETS)
               RT(NSP1) = RT(NSP1) + FAC * RT(NSETS)
      END IF
      GO TO 20231
20232 ASSIGN 20235 TO NPR021
      GO TO 30021
20235 CONTINUE
20231 GO TO 20228
20229 IF (.NOT.(IFNEW .eq. M1)) GO TO 20236
      ASSIGN 20237 TO NPR021
      GO TO 30021
20237 GO TO 20228
20236    IGIV1 = IFNEW
         IGIV2 = NSETS
      ASSIGN 20238 TO NPR022
      GO TO 30022
20238 CONTINUE
20228 CONTINUE
20224 GO TO 20183
c     ------------------------------------------------------------------
C     procedure( COMPLETE RETRIANGULARIZATION FROM ROW M1 ON DOWN )
c
c     .  Here the columns INDEX(M1 .. NSETS) are of lengths
c     .  M1+1 .. NSETS+1, respectively.  We must eliminate the
c     .  last element of each of these columns.
c
c        .  Find element of largest magnitude in row M1 among column
c        .  indices INDEX(M1 .. NSETS).
c
30021    AMAX = ZERO
      DO 20239 IS = M1, NSETS
            JS = INDEX(IS)
      IF(abs(A(M1,JS)) .gt. AMAX)THEN
               IMAX = IS
               AMAX = abs(A(M1,JS))
      END IF
20239 CONTINUE
c        .                Interchange columns indexed by M1 and IMAX.
c
      IF(IMAX .ne. M1)THEN
            JPIV = INDEX(IMAX)
            INDEX(IMAX) = INDEX(M1)
            INDEX(M1) = JPIV
      END IF
c
c        .  Do one stage of Gaussian elimination.
c
         IG1 = M1
      ASSIGN 20246 TO NPR005
      GO TO 30005
c
c        .  Use Householder transformations to
c        .  retriangularization in columns M1+1 through IMAX-1
c
20246 IF (.NOT.(M1P1 .le. IMAX-1 )) GO TO 20248
            IPUT1 = M1P1
            IPUT2 = IMAX-1
            IRLAST = IMAX+1
      GO TO 30023
c
c        .  Use Givens transformations to
c        .  retriangularization in columns max(M1+1,IMAX) through NSETS
c
20249 CONTINUE
20248 IF (.NOT.(max(M1P1, IMAX) .le. NSETS)) GO TO 20251
            IGIV1 = max(M1P1, IMAX)
            IGIV2 = NSETS
      ASSIGN 20252 TO NPR022
      GO TO 30022
20252 CONTINUE
20251 GO TO NPR021,(20235,20237)
c     ------------------------------------------------------------------
*     procedure( GIVENS ROTATIONS, USING IGIV1 AND IGIV2 )
c
c     This proc applies Givens rotations to pairs of rows, IS and IS+1,
c     for IS = IGIV1, ..., IGIV2.
c     This version applies these to all columns from 1 to N, even though
c     in general this causes arithmetic to be done on elements that
c     are known to be zero.
c
*        do for IS = IGIV1, IGIV2
*           JS=INDEX(IS)
*              call SROTG (A(IS,JS),A(IS+1,JS),CC,SS)
*              A(IS+1,JS)=ZERO
*              LEN = JS-1
*              if(LEN .gt. 0)
*    *            call SROT(LEN,A(IS,1),LDA,A(IS+1,1),LDA,CC,SS)
*              LEN = N - JS
*              if(LEN .gt. 0)
*    *            call SROT(LEN,A(IS,JS+1),LDA,A(IS+1,JS+1),LDA,CC,SS)
*              call SROT (1, B(IS),1, B(IS+1),1, CC,SS)
*              if(KPRINT .ge. 3)
*    *         call SROT (1, RT(IS),1, RT(IS+1),1, CC,SS)
*        end for ! IS
*     end proc !( GIVENS ROTATIONS, USING IGIV1 AND IGIV2 )
c     ------------------------------------------------------------------

C     procedure( GIVENS ROTATIONS, USING IGIV1 AND IGIV2 )
c
c     This proc applies Givens rotations to pairs of rows, IS and IS+1,
c     for IS = IGIV1, ..., IGIV2.
c     This version applies these only to the columns that might contain
c     nonzeros in the affected rows.
c
30022 DO 20253 L = IGIV1, IF2
         JCOL = INDEX(L)
c
c        .          Apply previously determined Givens transformations.
c
      DO 20256 IR = IGIV1, min(L-1,IGIV2)
            TEMP =          CC(IR)*A(IR,JCOL) + SS(IR)*A(IR+1,JCOL)
            A(IR+1,JCOL) = -SS(IR)*A(IR,JCOL) + CC(IR)*A(IR+1,JCOL)
            A(IR  ,JCOL) = TEMP
20256 CONTINUE
c        .                     Compute a new Givens transformation.
c
      IF(L .le. IGIV2)THEN
            call SROTG(A(L,JCOL), A(L+1,JCOL), CC(L), SS(L))
            A(L+1,JCOL) = ZERO
      END IF
20253 CONTINUE
c
c        .          Apply previous transformations to B()
c
      DO 20261 IR = IGIV1, IGIV2
            TEMP =     CC(IR)*B(IR) + SS(IR)*B(IR+1)
            B(IR+1) = -SS(IR)*B(IR) + CC(IR)*B(IR+1)
            B(IR  ) = TEMP
20261 CONTINUE
c
c        . Apply previous transformations to RT() for code testing.
c
      IF(KPRINT  .ge. 3)THEN
      DO 20266 IR = IGIV1, IGIV2
            TEMP =     CC(IR)*RT(IR) + SS(IR)*RT(IR+1)
            RT(IR+1) = -SS(IR)*RT(IR) + CC(IR)*RT(IR+1)
            RT(IR  ) = TEMP
20266 CONTINUE
      END IF
      GO TO NPR022,(20238,20252)
c     -----------------------------------------------------------------

C     procedure( GAUSSIAN ELIMINATION, USING IGAUS1 AND IGAUS2 )
c
c     This proc applies Gaussian elimination to pairs of rows,
c     IS and IS+1, for IS = IGAUS1 .. IGAUS2.
c
30020 continue
c++ CODE for ~.C. is active
      if(KPRINT .ge. 2) print '(1x,a,2i4)',
     *   'Gauss elim in pairs of rows.  IGAUS1, IGAUS2 =',IGAUS1,IGAUS2
c++ CODE for .C. is inactive
C%%   if (kprint >= 2) printf(
C%%     " Gauss elim in pairs of rows.  IGAUS1, IGAUS2 =%4ld %4ld\n",
C%%     igaus1, igaus2);
c++ END



      DO 20269 IS = IGAUS1, IGAUS2
            JS=INDEX(IS)
      IF(abs(A(IS+1,JS)) .gt. abs(A(IS,JS)))THEN
                  call SSWAP(N, A(IS,1),LDA, A(IS+1,1),LDA)
                  TEMP = B(IS)
                  B(IS) = B(IS+1)
                  B(IS+1) = TEMP
                  TEMP = RT(IS)
                  RT(IS) = RT(IS+1)
                  RT(IS+1) = TEMP
      END IF
               FAC = -A(IS+1,JS)/A(IS,JS)
               call SAXPY(N, FAC, A(IS,1),LDA, A(IS+1,1),LDA)
               A(IS+1,JS) = ZERO
               B(IS+1) = FAC * B(IS) + B(IS+1)
               RT(IS+1) = FAC * RT(IS) + RT(IS+1)
20269 CONTINUE
      GO TO 20230
c     ------------------------------------------------------------------

C     procedure( HOUSEHOLDER TRIANGULARIZATION )
c
c     Build new columns of triangular matrix indexed by IPUT1 thru IPUT2
c     Last row to be affected is IRLAST.
c
30023 DO 20274 IPUT = IPUT1, IPUT2
         JPUT = INDEX(IPUT)
         call SHTCC(1,IPUT,IPUT+1,IRLAST,A(1,JPUT), UP,B,M,1)
         if(KPRINT .ge. 4) print'(a,2i4/(5x,5g14.6))',
     *      ' Hous Tri.. IPUT,IRLAST,B(IPUT:IRLAST)=',
     *      IPUT,IRLAST,(B(I),I=IPUT,IRLAST)
      DO 20277 I = IPUT+1, IF2
            call SHTCC(2,IPUT,IPUT+1,IRLAST,A(1,JPUT), UP,
     *                A(1,INDEX(I)),LDA,1)
20277 CONTINUE
         if(KPRINT .ge. 3)
     *       call SHTCC (2,IPUT,IPUT+1,IRLAST,A(1,JPUT),  UP,RT(1),M,1)
      DO 20280 I = IPUT+1, IRLAST
            A(I,JPUT) = ZERO
20280 CONTINUE
20274 CONTINUE
      IF (.NOT.(KPRINT .ge. 3)) GO TO 20283
      ASSIGN 20283 TO NPR004
      GO TO 30004
20283 GO TO 20249
c     ------------------------------------------------------------------

C     procedure( PREPARE ROW NSP1 TO BE OBJECTIVE FUNCTION )
c
c     Save current B(NSP1) into BSAVE.
c     Set B(NSP1) to be the residual in row NSP1 for the current
c     X vector.
c     Do Gaussian elimination in row NSP1 using preceeding rows as
c     pivot rows.  Don't need to apply the Gaussian elim in the B()
c     vector because all of B(1:NSETS) is zero.
c
30008 BSAVE = B(NSP1)
      B(NSP1) = B(NSP1) - SDOT(N, A(NSP1,1), LDA, X, 1)
      DO 20284 IS = 1,NSETS
         JS = INDEX(IS)
      IF(IS .gt. 1)THEN
            A(NSP1,JS) = A(NSP1,JS) - SDOT(IS-1, Z(1),1, A(1,JS),1)
      END IF
         Z(IS) = A(NSP1,JS)/A(IS,JS)
         A(NSP1,JS) = ZERO
20284 CONTINUE
      DO 20289 IF0 = IF1, IF2
         JF = INDEX(IF0)
         A(NSP1,JF) = A(NSP1,JF) - SDOT(NSETS, Z(1),1, A(1,JF),1)
20289 CONTINUE
      GO TO 20065
c     ------------------------------------------------------------------

C     procedure( ELIMINATE IN LOWER ROWS )
c
c     The equality constraint rows 1 thru M1 have been triangularized.
c     Elimination in columns 1 thru IG1-1 in the lower matrix has been
c     previously done.  Now eliminate in columns IG1 thru M1 of the
c     lower matrix, i.e. in rows M1+1 through M.
c
30005 continue
      if(KPRINT .ge. 3)
     *   print '(1x/1x,''Gaussian elimination.  IG1,M1='',2i4)', IG1, M1
      DO 20292 IS = IG1, M1
         JS = INDEX(IS)
         LEN = 0
      DO 20295 I = M, M1P1, -1
      IF(A(I,JS) .ne. ZERO)THEN
               LEN = I - M1
      GO TO 20296
      END IF
20295 CONTINUE
20296 CONTINUE
      IF(LEN .ne. 0)THEN
            FAC = -ONE/A(IS,JS)
      DO 20302 I = IS+1, IF2
               J = INDEX(I)
               TEMP = FAC*A(IS,J)
               call SAXPY(LEN, TEMP, A(M1P1,JS),1, A(M1P1,J),1)
20302 CONTINUE
            TEMP = FAC*B(IS)
            call SAXPY(LEN, TEMP, A(M1P1,JS),1, B(M1P1),1)
            if(KPRINT .ge. 3)
     *      call SAXPY(LEN, FAC * RT(IS), A(M1P1,JS),1, RT(M1P1),1)
      DO 20305 I = M1P1, M1 + LEN
               A(I,JS) = ZERO
20305 CONTINUE
      END IF
20292 CONTINUE
      IF (.NOT.(KPRINT .ge. 3)) GO TO 20308
      ASSIGN 20308 TO NPR004
      GO TO 30004
20308 GO TO NPR005,(20018,20246)
c     ------------------------------------------------------------------
C     procedure( SOLVE TRIANGULAR SYSTEM, INPUT AND OUTPUT IN Z() )
c
c     .  Solve the triangular system defined by the column vectors
c     .  indexed in Set S.  The right-side vector is given in
c     .  Z(), and the solution vector will be returned in Z().
c     .  It is assured that all diagonal terms are nonzero.
c
30014 DO 20309 IIS = NSETS, 1, -1
          if(IIS .ne. NSETS) call SAXPY(IIS, -Z(IIS+1), A(1,JJS),1, Z,1)
          JJS=INDEX(IIS)
          Z(IIS)=Z(IIS)/A(IIS,JJS)
20309 CONTINUE
      GO TO NPR014,(20115,20126)
c     ------------------------------------------------------------------

C     procedure( TERMINATION )
c
c                     Compute the norm of the final residual vector.
c                     Clean up W().
30007 RNORM = ZERO
      IF(IERR .le. 0 .or. IERR .ge. 3)THEN
         I1 = max(M1P1, NSP1)
         LEN = M - I1 + 1
      IF(LEN .gt. ZERO)THEN
            RNORM = SNRM2(LEN, B(I1), 1)
      ELSE
      DO 20316 J=1,N
               W(J)=ZERO
20316 CONTINUE
      END IF
      END IF
      GO TO 20022
c     ------------------------------------------------------------------

C     procedure( DEBUG )
c
30004 IF(KPRINT .ge. 3)THEN
      IF(CMODE)THEN
            MDB = M1
      ELSE
            MDB = M
      END IF
         call SCOPY(MDB, B,1, DIFF,1)
      DO 20323 JD=1,N
            call SAXPY(MDB, X(JD)-XT(JD), A(1,JD),1, DIFF,1)
20323 CONTINUE
         call SAXPY(MDB, -ONE, RT,1, DIFF,1)
         print '(1x/1x,a)', 'Consistency test. DIFF()='
         print '(1x,5g14.6)',(DIFF(ID),ID=1,MDB)
      END IF
      GO TO NPR004,(20010,20019,20021,20283,20308)
c     ------------------------------------------------------------------
      end
