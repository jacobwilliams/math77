      SUBROUTINE SMLC01 (SMLCFG, N,M,MEQ,A,LDA,B,XL,XU,X,ACC,
     *                   IPRINT, NFMAX, IW, LIW, W, LW)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c     File: SMLC.for    Minimization with linear constraints.
C>> 2001-10-05 SMLC Krogh  Minor format changes -- 1PE => 1P,E
C>> 2001-06-18 SMLC Krogh  Changed ". LT." to " .LT."
C>> 1996-07-08 SMLC Krogh  Multiple changes for C conversion.
c>> 1996-03-30 SMLC Krogh  Removed MAX from type stmt., added external.
c>> 1995-11-15 SMLC Krogh  Moved formats up for C conversion.
C>> 1994-11-11 SMLC Krogh  Declared all vars.
c>> 1994-11-02 SMLC Krogh  Changes to use M77CON
c>> 1992-04-27 SMLC CLL
c>> 1992-04-17 CLL
c>> 1992-03-27 CLL
c>> 1992-02-05 CLL Declared all floating point variables.
c>> 1992-01-16 CLL
c>> 1991-06-10 CLL & FTK Editing comments.
c>> 1991-04-30 CLL & FTK
c>> 1991-04-19 CLL
c>> 1991-04-15 FTK & CLL  Made FIRST an argument of SMLC20
c>> 1990-09-12 C. L. Lawson and F. T. Krogh, JPL
c>> 1990-07-25 C. L. Lawson, JPL
c>> 1990-07-12 C. L. Lawson, JPL
c>> 1990-04-03 C. L. Lawson, JPL
c--S replaces "?": ?MLC,?MLC01,?MLC02,?MLCFG,?MLC20,?MLC21,?MLC04,?MLC05
c--&      ?MLC13,?MLC15,?MLC03,?MLC12,?MLC06,?MLC11,?MLC09,?MLC16,?MLC19
c--&      ?MLC07,?MLC18,?MLC08,?MLC14,?MLC10,?MLC17,?ERV1
c     Also calls     ERMOR, ERMSG, IERM1, IERV1
C     ------------------------------------------------------------------
c     This algorithm and the original code is due to
c     M. J. D. Powell, Cambridge Univ., 1989.
c     1990-04-03 This version adapted for usage at JPL by
c     C. L. Lawson.  Instrumented with "c--" lines to automate type
c     conversions.
c     1990-07-11 CLL Added new argument NFMAX to SMLC01.  Added new
c     subroutine SMLC20 to compute gradient by finite differences.
c     Added argument HAVEG in the user-provided subr SMLCFG.
c     Added args XL and XU to subr _MLC09 in order to be able to pass
c     them on to SMLC20.
c     Added reference to [D/R]1MACH in _MLC15.
c     1990-07-12 CLL  Transposed indices in the array A(,).
c     The constraints are now Ax .le. b rather than (A**t)x .le. b.
c     1990-07-19 CLL  Reorganized the arg list of SMLC01.
c        Prev args INFO, NACT, IACT() are embedded in new arg IW().
c        New item, FVAL, and previous args W() and PAR() are combined
c        into new arg W().  The new item FVAL is the final value of the
c        objective function.
c     1990-07-24 CLL  Setting INFO = 0 initially.  It keeps this value
c        till it is changed for some reason.  Previously it was set to
c        4 initially.
c        Changed specification of IPRINT.  It now contains 5 print
c        parameters.  This gives more flexibility, such as requesting
c        printing only at termination.
c        Moved the main block of intermediate printing into a new subr,
c        SMLC21.  This allows cleaner logic for testing when to print.
c        SMLC21 is called both from SMLC02 and from _MLC05.  In revising
c        the print control we removed variables NFVALK and ITERP, and
c        added variables NFVPR and TOLPRT.
c        Printing of error messages is now done using JPL MATH77 error
c        printing subroutines.  Requires linking of
c        SERV1, ERFIN, ERMOR, ERMSG, IERM1, and IERV1.  The calls are in
c        SMLC01, SMLC02, and _MLC15.  ERFIN is not called directly.
c        Setting INFO = 9 in SMLC02 when terminate due to solution being
c        determined just by the constraints.  Also compute and return
c        the func value in this case.
c     1990-09-12 CLL and FTK Added arg. W20 for _MLC20.
c     1991-04-19 CLL Change to have NFVALS count every call to _MLCFG.
c        Move test of IPFREQ to be sure last iteration gets printed if
c        it is a multiple of IPFREQ.  Changed usage of NFVPR.
c 1992 March/April CLL Additions to the code in MINFUN (C05) and
c    LSRCH (C09).  The code will now return INFO = 2 in some cases
c    in which it previously returned INFO = 3.  It will also finish in
c    fewer iterations in some cases.  Added more to error msg on quit-
c    ting with INFO = 3.  Added HAVEG into arg list of _MLC05.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c--   Begin Mask code changes.
c     Names of subroutines have been changed as follows:
c     Original names          New DP names         New SP names
c     GETMIN                  DMLC01               SMLC01
c     MINFLC                  DMLC02               SMLC02
c     EQCONS                  DMLC03               SMLC03
c     GETFES                  DMLC04               SMLC04
c     MINFUN                  DMLC05               SMLC05
c     CONRES                  DMLC06               SMLC06
c     GETD                    DMLC07               SMLC07
c     SDEGEN                  DMLC08               SMLC08
c     LSRCH                   DMLC09               SMLC09
c     NEWCON                  DMLC10               SMLC10
c     SATACT                  DMLC11               SMLC11
c     ADDCON                  DMLC12               SMLC12
c     ADJTOL                  DMLC13               SMLC13
c     DELCON                  DMLC14               SMLC14
c     INITZU                  DMLC15               SMLC15
c     KTVEC                   DMLC16               SMLC16
c     SDIRN                   DMLC17               SMLC17
c     STEPBD                  DMLC18               SMLC18
c     ZBFGS                   DMLC19               SMLC19
c     FGCALC                  DMLCFG               SMLCFG
c     New code by FTK -->     DMLC20               SMLC20
c     New code by CLL -->     DMLC21               SMLC21
c--   End Mask code changes.

C
C  This is main entry point to a package of subroutines that calculate
C  the least value of a differentiable function of several variables
C  subject to linear constraints on the values of the variables.
c     ------------------------------------------------------------------
c                  Subroutine Arguments
c
C  SMLCFG [in]  Name of subroutine having an interface of the form
C                SUBROUTINE SMLCFG (N, X, F, G, HAVEG)
c                INTEGER N
c                [DOUBLE PRECISION or REAL] X(N), F, G(N)
C                LOGICAL HAVEG
c     provided by the user to compute values of the objective function,
c     F, and optionally its gradient vector, G(), at the argument value
c     given by X().  The user may choose either to write code to compute
c     G() or not.  If SMLCFG computes G() it must also set HAVEG = .true
c     If SMLCFG does not compute G() it must set HAVEG = .false.
c     In this latter case the package will make additional calls to
c     SMLCFG to compute a finite difference approximation to the
c     gradient vector.
c     The vectors X() and G() will be of length N.  The subroutine must
c     not change the values of N or X().
c     SMLCFG may use COMMON block(s) if neccessary to communicate
c     data between the user's main program and SMLCFG.
C  N is the number of variables and must be set by the user.
C  M is the number of linear constraints (excluding simple bounds) and
C     must be set by the user.
C  MEQ is the number of constraints that are equalities and must be set
C     by the user.
C  A(.,.) is a 2-dimensional array whose rows are the gradients of
C     the M constraint functions.  Its entries must be set by the user
C     and its dimensions must be at least M by N.
C  LDA is the actual first dimension of the array A that is supplied by
C     the user, so its value may not be less than M.
C  B(.) is a vector of constraint right hand sides that must also be set
C     by the user.  Specifically the constraints on the variables X(I)
C     I=1(1)N are
C          A(K,1)*X(1)+...+A(K,N)*X(N) .EQ. B(K)  K=1,...,MEQ
C          A(K,1)*X(1)+...+A(K,N)*X(N) .LE. B(K)  K=MEQ+1,...,M  .
C     Note that the data that define the equality constraints come
C     before the data of the inequalities.
C  XL(.) and XU(.) are vectors whose components must be set to lower and
C     upper bounds on the variables.  Choose very large negative and
C     positive entries if a component should be unconstrained, or set
C     XL(I)=XU(I) to freeze the I-th variable.  Specifically these
C     simple bounds are
C          XL(I) .LE. X(I) and X(I) .LE. XU(I)  I=1,...,N  .
C  X(.) is the vector of variables of the optimization calculation.  Its
C     initial elements must be set by the user to an estimate of the
C     required solution.  The subroutines can usually cope with poor
C     estimates, and there is no need for X(.) to be feasible initially.
C     These variables are adjusted automatically and the values that
C     give the least feasible calculated value of the objective function
C     are available in X(.) on the return from SMLC01.
C  ACC is a tolerance on the first order conditions at the calculated
C     solution of the optimization problem.  These first order
C     conditions state that, if X(.) is a solution, then there is a set
C     of active constraints with indices IACT(K) K=1(1)NACT, say, such
C     that X(.) is on the boundaries of these constraints, and the
C     gradient of the objective function can be expressed in the form
C          GRAD(F)=PAR(1)*GRAD(C(IACT(1)))+...
C                        ...+PAR(NACT)*GRAD(C(IACT(NACT)))  .
C     Here PAR(K) K=1(1)NACT are Lagrange multipliers that are
C     nonpositive
C     for inequality constraints, and GRAD(C(IACT(K))) is the gradient
C     of the IACT(K)-th constraint function, so it is A(IACT(K),.) if
C     IACT(K) .LE. M, and it is minus or plus the J-th coordinate vector
c     if the constraint is the lower or upper bound on X(J)
c     respectively.  The normal return from the calculation occurs when
c     X(.) is feasible and the sum of squares of components of the
c     vector RESKT(.) is at most ACC**2, where RESKT(.) is the
c     N-component vector of residuals of the first order condition that
c     is displayed above.  Sometimes the package cannot satisfy this
c     condition, because noise in the function values can prevent a
c     change to the variables, no line search being allowed to increase
c     the objective function.
C  IPRINT  [in, integer]  Contains print control parameters, packed as
c     follows:
c     IPRINT = IPFREQ*100 + IPTOL*8 + IPFRST*4 + IPMORE*2 + IPLAST
c     Printing only begins after a first feasible point is found.
c     The items to be printed are selected by IPMORE.  The other
c     parameters determine when to print.
c     IPFREQ is zero or positive.  If positive, printing will be done on
c        iterations 0, IPFREQ, 2*IPFREQ, etc.
c     IPTOL = 0 or 1.  1 means to print the new TOL value
c        and the standard items each time TOL is changed.
c     IPFRST = 0 or 1.  1 means to print on the first iteration, i.e. on
c        iteration No. 0.
c     IPMORE = 0 or 1.  0 means the items to be printed are ITERC,
c        NFVALS, F, X(1:N), and G(1:N).  1 means to also print
c        IACT(1:NAXT), PAR(1:NACT) and RESKT(1:N).
c     IPLAST = 0 or 1.  1 means to print the final results, and the
c        reason for termination.
c  NFMAX  [in, integer]  If positive this sets an upper limit on the
c     number of function evaluations.  When gradients are estimated by
c     differences, the count includes the function evaluations for this
c     purpose also.  If zero there is no upper limit.
c  IW(.)  [work, out, integer]  Integer work space.  Also used to return
c     status information.  Its length must be at least LIW.
c     On return:
c        IW(1) contains INFO.  Indicates reason for termination.
c              See below for explanation of values.
c        IW(2) contains ITERC.  No. of iterations.
c        IW(3) contains NFVALS.  No. of function evaluations, not
c              counting extra func evals done to estimate the gradient
c              when the gradient is not computed explicitly.  In this
c              latter case the actual No. of func evals will be
c              (K+1)*NFVALS, where K is the number of solution
c              components whose lower and upper bounds are not equal.
c        IW(4) contains NACT.  The number of active constraints at the
c              solution point.  Will be in the interval [0, N].
c        IW(5:4+NACT) contains IACT(1:NACT).  IACT() is used as work
c              space of length M + 2*N.  On return the first NACT
c              locations contain the indices of the constraints that
c              are active at the final point.  Indices [1:M] refer to
c              rows of the system Ax .le. b.  Indices [M+1:M+N]
c              refer to component lower bounds.  Indices [M+N+1:M+2*N]
c              refer to component upper bounds.
C
c  LIW  [in, integer]  Dimension of the IW() array.  Require
c       LIW .ge. 4 + M + 2*N.
c
C  W(.)  [work, out, float]  Working space array of float variables.
c     Also used to return results.
c     Its length must be at least LW.
c     On return:
c     W(1) contains FVAL, the final value of the objective function.
c     W(2:1+N) contains the final gradient vector, GRAD(1:N).
c     W(2+N:1+2*N) contains the Kuhn-Tucker residual vector, RESKT(1:N).
c     W(2+2*N:1+2*N+NACT) contains the Lagrange multipliers, PAR(1:NACT)
c        where NACT has a value in [0,N].  GRAD(), RESKT(), and PAR()
c        are mentioned above in the description of ACC.
c  LW  [in, integer]  Dimension of the W() array.  Require
c      LW .ge. 3 + M + 16*N + N**2.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                      Return values of INFO
c
c     INFO indicates the reason for termination.
c
C          INFO=1   X(.) is feasible and the condition that depends on
C          ACC is satisfied.
C          INFO=2   X(.) is feasible and rounding errors are preventing
C          further progress.
C          INFO=3   X(.) is feasible but the objective function fails to
C     decrease although a decrease is predicted by the current gradient
C     vector.  If this return occurs and RESKT(.) has large components
C     then the user's calculation of the gradient of the objective
C     function may be incorrect.  One should also question the coding of
C     the gradient when the final rate of convergence is slow.
C          INFO=4   In this case the calculation cannot begin because IA
C     is less than N or because the lower bound on a variable is greater
C     than the upper bound.
C          INFO=5   This value indicates that the equality constraints
C     are inconsistent.   These constraints include any components of
C     X(.) that are frozen by setting XL(I)=XU(I).
C          INFO=6   In this case there is an error return because the
C     equality constraints and the bounds on the variables are found to
C     be inconsistent.
C          INFO=7   This value indicates that there is no vector of
C     variables that satisfies all of the constraints.  Specifically,
C     when this return or an INFO=6 return occurs, the current active
C     constraints (whose indices are IACT(K) K=1(1)NACT) prevent any
C     change in X(.) that reduces the sum of constraint violations,
C     where only bounds are included in this sum if INFO=6.
C          INFO=8   In this case the limit on the number of calls of
C     subroutine SMLCFG (see below) has been reached, and there would
C     have been further calculation otherwise.
c          INFO = 9  The solution is determined just by the constraints.
c     In this case PAR() and RESKT() are not defined on return.
c     FVAL will be defined on return.  G() will only be defined on
c     return if SMLCFG returns HAVEG = .true.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  Note 1.   The variables N, M, MEQ, LDA, ACC, IPRINT, NFMAX and the
C     elements of the arrays A(.,.), B(.), XL(.) and XU(.) are not
c     altered by the optimization procedure.  Their values, and the
c     initial components of X(.) must be set on entry to SMLC01.
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  Note 2.   A paper on the method of calculation and a report on the
C     main features of the computer code are available from the author
C     M.J.D.Powell (D.A.M.T.P., University of Cambridge, Silver Street,
C     Cambridge CB3 9EW, England).
c     ------------------------------------------------------------------
      external SMLCFG
      integer LDA, LIW, LW
      integer I, IBRES, ID
      integer IFVAL, IG, IGM, IGS, IIACT, IINFO, IITERC, INACT, INFO
      integer INFVAL, IPAR, IPRINT, IRESKT, IU, IW(LIW), IW20, IXBIG
      integer IXS, IZ, IZTG, KTNORM, M, MEQ, N, NFMAX
      real             A(LDA,*),ACC, B(*),TEMP, XL(*),XU(*),X(*),W(LW)
      parameter(IFVAL = 1, KTNORM = 2, IG = 3)
      parameter(IINFO = 1, IITERC = 2, INFVAL = 3, INACT = 4, IIACT = 5)
C     ------------------------------------------------------------------
c                                   Check sizes of LIW and LW
      INFO = 0
      if(LIW .lt. 4 + M + 2*N) then
         INFO = 4
         call IERM1('SMLC01', INFO, 0,'Dimension LIW is too small.',
     *        'LIW',LIW, ',')
         call IERV1('Need',4 + M + 2*N,'.')
      endif
c
      if(LW .lt. 3 + M + N*(16 + N)) then
         INFO = 4
         call IERM1('SMLC01', INFO, 0,'Dimension LW is too small.',
     *        'LW',LW, ',')
         call IERV1('Need',3 + M + N*(16 + N),'.')
      endif
      if(INFO .ne. 0) then
         IW(1) = INFO
         return
      endif
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C                         Partition the array W()
c                         parameter(IFVAL = 1, KTNORM = 2, IG = 3)
      IRESKT=IG+N
      IPAR = IRESKT + N
      IZ=IPAR + N
      IU=IZ+N*N
      IXBIG=IU+N
      IBRES=IXBIG+N
      ID=IBRES+M+N+N
      IZTG=ID+N
      IGM=IZTG+N
      IXS=IGM+N
      IGS=IXS+N
C                  Need 4*N+1 locations beginning at IW20.
      IW20=IGS+N
C                         Partition the array IW()
c     parameter(IINFO = 1, IITERC = 2, INFVAL = 3, INACT = 4, IIACT = 5)
C
c     Zero the RESKT vector so it will be safe to compute its norm on
c     return from SMLC02 even in cases when SMLC02 does not compute
c     RESKT.
c
      do 10 I = IRESKT, IRESKT + N -1
         W(I) = 0.0E0
   10 continue
c
C     Call the optimization package.
C
      CALL SMLC02 (SMLCFG, N,M,MEQ,A,LDA,B,XL,XU,X,ACC,
     *  IW(IIACT),IW(INACT),W(IPAR),IPRINT, NFMAX,
     *  IW(IINFO), IW(IITERC), IW(INFVAL),
     *  W(IG),W(IZ),W(IU),W(IXBIG),W(IRESKT),W(KTNORM),W(IBRES),W(ID),
     *  W(IZTG),W(IGM),W(IXS),W(IGS), W(IFVAL), W(IW20))
c
      TEMP = 0.0E0
      do 20 I = IRESKT, IRESKT+N-1
         TEMP = TEMP + W(I)**2
   20 continue
      W(KTNORM) = sqrt(TEMP)
      RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC02 (SMLCFG, N,M,MEQ,A,LDA,B,XL,XU,X,
     *  ACC,IACT,NACT,PAR, IPRINT, NFMAX, INFO, ITERC, NFVALS,
     *  G,Z,U,XBIG,RESKT, ENRMKT, BRES,D,ZTG,GM,XS,GS, FVAL, W20)
c
c     Main subroutine to control the Powell constrained minimization
c     package.
c     1990-07-19 CLL  Added NFMAX, ITERC, NFVALS, and FVAL to arg list.
c        Also added FVAL to arg list of SMLC05 that is called
c        from this subr.
c        NFMAX provides limit on no. of func evaluations, but zero means
c        no limit.  This was previously overloaded onto INFO.
c        ITERC & NFVALS give output of counts of iterations and
c        function evaluations.
c        FVAL provides output of the final objective function value.
c     ------------------------------------------------------------------
c                                 Arguments
c
c  SMLCFG, N,M,MEQ,A(),LDA,B(),XL(),XU(),X(),
c  ACC,IACT(),NACT,PAR(),
c  IPRINT [in, integer]  Contains print parameters packed as follows:
c           IPRINT = IPFREQ*100 + IPTOL*8 + IPFRST*4 + IPMORE*2 + IPLAST
c  NFMAX [in, integer]  Limit on No. of function evaluations.  But zero
c         means no limit.
c  INFO [out, integer]  Termination status flag.
c  ITERC [out, integer]  Count of No. of iterations.  Initialized to 0
c        in this subr.  Incremented in SMLC05.
c  NFVALS [out, integer]  Count of number of function evaluations.
c  G(),Z(),U(),XBIG(),
c  RESKT()
c  ENRMKT  Euclidean norm of RESKT().
c  BRES(),D(),
c  ZTG(),GM(),XS(),GS(),
c  FVAL [out, float]  Final (best) value of F.
c  W20  [out, work, float] Working storage for SMLC20.
c     ------------------------------------------------------------------
c                    Descriptions of some of the internal variables.
c
c  GMODE [integer]  Initialized to 0 here.  Sent to SMLC05 for eventual
c        use by SMLC20.
c  HAVEG [logical]  Arg in call to SMLCFG, not used in this subroutine.
c  NFVPR [integer]  Used to avoid printing results from the same
c        function evaluation more than once.  Initialized to -1 in this
c        subr.  Modified in SMLC05.  Used in SMLC02 and DNLC05.
c     ------------------------------------------------------------------
      external SMLCFG

      integer GMODE, I, IACT(*), INFO
      integer IPFREQ, IPFRST, IPLAST, IPMORE, IPRINT
      integer IPTOL, ITERC, K, LDA, M, MEQ, MEQL, MP, MSAT
      integer MTOT, N, NACT, NFMAX, NFVALS, NFVPR

      real             A(LDA,*),ACC, B(*),BRES(*),D(*),ENRMKT, FVAL
      real             G(*), GM(*), GS(*), PAR(*), RELACC, RESKT(*)
      real             SSQKT, TOL, U(*)
      real             W20(0:*), XBIG(*), XL(*), XU(*), X(*), XS(*)
      real             Z(*), ZTG(*), ZZNORM
      logical HAVEG
c     ------------------------------------------------------------------
C     Initialize ZZNORM, ITERC, NFVPR, NFVALS, INFO
C
      ZZNORM=-1.0E0
      W20(0) = -1.0e0
      W20(1) = -1.0e0
      GMODE = 0
      ITERC=0
      NFVPR = -1
      NFVALS=0
      INFO = 1
c
c                         Decompose IPRINT into 5 integers:
c           IPRINT = IPFREQ*100 + IPTOL*8 + IPFRST*4 + IPMORE*2 + IPLAST
C
      IPFREQ = IPRINT/100
      I = IPRINT - IPFREQ * 100
      IPTOL = I/8
      I = I - IPTOL * 8
      IPFRST = I/4
      I = I - IPFRST * 4
      IPMORE = I/2
      IPLAST = I - IPMORE * 2
c
C                      Check the bounds on N, M and MEQ.
C
      if(N .lt. 1 .or. M .lt. 0 .or. MEQ .lt. 0 .or. M .lt. MEQ) then
         INFO = 4
         call IERM1('SMLC02', INFO, 0,'Bad value for M, N, or MEQ.',
     *        'M',M, ',')
         call IERV1('N',N,',')
         call IERV1('MEQ',MEQ,'.')
         GOTO 40
      END IF
C
C                      Initialize RELACC, Z, U and TOL.
C
      CALL SMLC15 (N,M,XL,XU,X,IACT,MEQL,INFO,Z,U,XBIG,RELACC)
      IF (INFO .EQ. 4) go to 40
      TOL=max(0.01E0,10.0E0*RELACC)
      if(IPFREQ .ne. 0 .or. IPFRST .ne. 0 .or.
     *   IPLAST .ne. 0 .or. IPTOL .ne. 0) then
         print'(/6x,''Beginning subroutine SMLC02, called from SMLC01:''
     *      /6x,''N ='',i4,'',  M ='',i4,'',  MEQ  ='',i4,'',  ACC ='',
     *      g11.3/6x,''RELACC ='',g11.3,'',  TOL = '',g11.3)',
     *      N, M, MEQ, ACC,  RELACC, TOL
      endif
C
C     Add any equality constraints to the active set.
C
      IF (MEQ .GT. 0) THEN
          CALL SMLC03 (N,M,MEQ,A,LDA,B,XU,IACT,MEQL,INFO,Z,U,RELACC,XS,
     1      GS)
          IF (INFO .EQ. 5) THEN
            call ERMSG('SMLC02',INFO,0,
     *           'Equality constraints are inconsistent.','.')
            GOTO 40
          END IF
      END IF
      NACT=MEQL
      MSAT=MEQL
C
C     Add the bounds to the list of constraints.
C
      MTOT=NACT
      DO 10 I=1,N
      IF (XL(I) .LT. XU(I)) THEN
          MTOT=MTOT+2
          IACT(MTOT-1)=M+I
          IACT(MTOT)=M+N+I
      END IF
   10 CONTINUE
C
C     Try to satisfy the bound constraints.
C
      CALL SMLC04 (N,M,A,LDA,B,XL,XU,X,IACT,NACT,PAR,INFO,G,Z,U,XBIG,
     1  RELACC,TOL,MEQL,MSAT,MTOT,BRES,D,ZTG,GM,RESKT,XS,GS,W20)
      IF (MSAT .LT. MTOT) THEN
         INFO=6
         call ERMSG('SMLC02',INFO,0,
     *           'Equalities and bounds are inconsistent.','.')
         GOTO 40
      END IF
C
C     Add the ordinary inequalities to the list of constraints.
C
      IF (M .GT. MEQ) THEN
          MP=MEQ+1
          DO 20 K=MP,M
          MTOT=MTOT+1
   20     IACT(MTOT)=K
      END IF
c     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   30 continue
c                                    Begin main loop
C
C                          Correct any constraint violations.
C
      CALL SMLC04 (N,M,A,LDA,B,XL,XU,X,IACT,NACT,PAR,INFO,G,Z,U,XBIG,
     1  RELACC,TOL,MEQL,MSAT,MTOT,BRES,D,ZTG,GM,RESKT,XS,GS,W20)
      IF (MSAT .LT. MTOT) THEN
         INFO=7
         call ERMSG('SMLC02',INFO,0,
     *           'Constraints are inconsistent.','.')
         GOTO 40
      ELSE IF (MEQL .EQ. N) THEN
c
c        Here the soln is determined just by the constraints.
c        PAR() and RESKT() are not defined on return in this case.
c        FVAL has not yet been evaluated.  We evaluate FVAL here in
c        order to return its value.  We leave G() not computed if HAVEG
c        is false, however it would be easy to compute G by calling
c        SMLC20 if that seemed preferable.
c
C%%        (*smlcfg)( n, x, fval, g, &haveg );
         call SMLCFG(N, X, FVAL, G, HAVEG)
         NFVALS = NFVALS+1
         ENRMKT = 0.0e0
         INFO = 9

         if(IPFREQ .ne. 0 .or. IPFRST .ne. 0 .or.
     *      IPLAST .ne. 0 .or. IPTOL .ne. 0) then
            print '(/'' [1] SMLC02..''/
     *    '' The solution is determined by the equality constraints.'')'
            call SMLC21(IPMORE, .false., N, ITERC, NFVALS,
     *              FVAL, TOL, X, G, NACT, IACT, PAR, RESKT, ENRMKT)
         endif
         GOTO 40
      END IF
C
C     Minimize the objective function in the case when constraints are
C       treated as degenerate if their residuals are less than TOL.
C
c          SMLC05 is MINFUN
      CALL SMLC05 (SMLCFG, N,M,A,LDA,B,XL,XU,X,ACC,IACT,NACT,
     *  PAR, IPFREQ, IPTOL, IPFRST, IPMORE, INFO, G, Z,
     *  U,XBIG,RELACC,ZZNORM,TOL,MEQL,MTOT,ITERC, NFVPR, NFVALS,
     *  NFMAX,RESKT, SSQKT, BRES,D,ZTG,GM,XS,GS, FVAL, GMODE,
     *  HAVEG, W20)
C
C     Reduce TOL if necessary.
C
      IF (TOL .GT. RELACC .AND. NACT .GT. 0) THEN
          if (NFMAX .le. 0 .or. NFVALS .lt. NFMAX) then
C                  SMLC13 is ADJTOL
              CALL SMLC13 (N,M,A,LDA,B,XL,XU,X,IACT,NACT,XBIG,RELACC,
     *                     TOL, MEQL)
              GOTO 30
          else
              INFO=8
          endif
      END IF
c
c           Here when INFO = 1, 2, 3, or 8.
c           We treat 1 and 2 as normal conditions and 3 and 8 as errors.
c
      if (INFO .EQ. 1) then
         if (IPLAST .ne. 0) print*,
     *       '[1]  SMLC01 quitting because requested accuracy',
     *       ' has been attained.'

      elseif (INFO .EQ. 2) then
         if (IPLAST .ne. 0) print*,
     *       '[2]  SMLC01 quitting due to limitation of',
     *       ' computational precision.'

      elseif (INFO .EQ. 3) then
         call ERMSG('SMLC02', INFO, 0,
     *   'F not decreasing, although gradient indicates it should.',',')
         if(HAVEG) then
            call ERMOR('Could be due to limitation of precision',',')
            call ERMOR('or to incorrect code for the gradient.','.')
         else
            call ERMOR('Could be due to limitation of precision.','.')
         endif

      elseif (INFO .EQ. 8) then
         call IERM1('SMLC02', INFO, 0,
     *   'Reached specified max number of function evaluations.',
     *   'Count',NFVALS, ',')
         call IERV1('NFMAX',NFMAX,'.')
      endif
c
c                           Test to print results before quitting.
c
      ENRMKT = sqrt(SSQKT)
      if(IPLAST .ne. 0 .and. NFVALS .ne. NFVPR) then
c        print*,'SMLC02.. Debug.. Call C21 on leaving C02.'
         call SMLC21(IPMORE, .true., N, ITERC, NFVALS,
     *           FVAL, TOL, X, G, NACT, IACT, PAR, RESKT, ENRMKT)
      endif
c
   40 return
      end
c     ==================================================================
      SUBROUTINE SMLC03 (N,M,MEQ,A,LDA,B,XU,IACT,MEQL,INFO,Z,U,RELACC,
     1  AM,CGRAD)
C
C     Try to add the next equality constraint to the active set.
c     ------------------------------------------------------------------
      integer N, M, MEQ, LDA, IACT(*), MEQL, INFO
      integer I, IZ, J, JM, K, KEQ, NP
      real             A(LDA,*),B(*),XU(*)
      real             RELACC, RHS, SUM, SUMABS, VMULT
      real             Z(*),U(*),AM(*),CGRAD(*)
c     ------------------------------------------------------------------
      DO 50 KEQ=1,MEQ
      IF (MEQL .LT. N) THEN
          NP=MEQL+1
          IACT(NP)=KEQ
          CALL SMLC12 (N,M,A,LDA,IACT,MEQL,Z,U,RELACC,NP,AM,CGRAD)
          IF (MEQL .EQ. NP) GOTO 50
      END IF
C
C     If linear dependence occurs then find the multipliers of the
C       dependence relation and apply them to the right hand sides.
C
      SUM=B(KEQ)
      SUMABS=abs(B(KEQ))
      IF (MEQL .GT. 0) THEN
          DO 10 I=1,N
   10     AM(I)=A(KEQ,I)
          K=MEQL
   20     VMULT=0.0E0
          IZ=K
          DO 30 I=1,N
          VMULT=VMULT+Z(IZ)*AM(I)
   30     IZ=IZ+N
          VMULT=VMULT*U(K)
          J=IACT(K)
          IF (J .LE. M) THEN
              DO 40 I=1,N
   40         AM(I)=AM(I)-VMULT*A(J,I)
              RHS=B(J)
          ELSE
              JM=J-M-N
              AM(JM)=AM(JM)-VMULT
              RHS=XU(JM)
          END IF
          SUM=SUM-RHS*VMULT
          SUMABS=SUMABS+abs(RHS*VMULT)
          K=K-1
          IF (K .GE. 1) GOTO 20
      END IF
C
C     Error return if the constraints are inconsistent.
C
      IF (abs(SUM) .GT. RELACC*SUMABS) THEN
          INFO=5
          GOTO 60
      END IF
   50 CONTINUE
   60 RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC04 (N,M,A,LDA,B,XL,XU,X,IACT,NACT,PAR,INFO,G,Z,
     1  U,XBIG,RELACC,TOL,MEQL,MSAT,MTOT,BRES,D,ZTG,GM,GMNEW,PARNEW,
     2  CGRAD,W20)
C
C     Make the correction to X for the active constraints.
c     ------------------------------------------------------------------
      integer N, M, LDA, IACT(*), NACT, INFO, MEQL, MSAT, MTOT
      integer I, ITEST, MSATK, INDXBD
      real             A(LDA,*),B(*),XL(*),XU(*),X(*),PAR(*),G(*)
      real             U(*),XBIG(*),BRES(*),D(*)
      real             GM(*),GMNEW(*),PARNEW(*), CGRAD(*)
      real             W20(0:*), RELACC, STEPCB, SUMRES, SUMRSK, TOL
      real             Z(*), ZTG(*)
c     ------------------------------------------------------------------
      INFO=0
   10 CALL SMLC11 (N,M,A,LDA,B,XL,XU,X,IACT,NACT,INFO,Z,U,XBIG,RELACC,
     1  TOL,MEQL)
      IF (INFO .GT. 0) MSAT=NACT
      IF (MSAT .EQ. MTOT) GOTO 60
C
C     Try to correct the infeasibility.
C
   20 MSATK=MSAT
      SUMRSK=0.0E0
   30 CALL SMLC06 (N,M,A,LDA,B,XL,XU,X,IACT,NACT,PAR,G,Z,U,XBIG,BRES,
     1  D,ZTG,RELACC,TOL,STEPCB,SUMRES,MEQL,MSAT,MTOT,INDXBD,GM,GMNEW,
     2  PARNEW,CGRAD,W20)
C
C     Include the new constraint in the active set.
C
      IF (STEPCB .GT. 0.0E0) THEN
          DO 40 I=1,N
          X(I)=X(I)+STEPCB*D(I)
   40     XBIG(I)=max(XBIG(I),abs(X(I)))
          CALL SMLC12 (N,M,A,LDA,IACT,NACT,Z,U,RELACC,INDXBD,GMNEW,
     *                 CGRAD)
      END IF
C
C     Test whether to continue the search for feasibility.
C
      IF (MSAT .LT. MTOT) THEN
          IF (STEPCB .EQ. 0.0E0) GOTO 50
          IF (MSATK .LT. MSAT) GOTO 20
          IF (SUMRSK .EQ. 0.0E0 .OR. SUMRES .LT. SUMRSK) THEN
              SUMRSK=SUMRES
              ITEST=0
          END IF
          ITEST=ITEST+1
          IF (ITEST .LE. 2) GOTO 30
C
C     Reduce TOL if it may be too large to allow feasibility.
C
   50     IF (TOL .GT. RELACC) THEN
              CALL SMLC13 (N,M,A,LDA,B,XL,XU,X,IACT,NACT,XBIG,RELACC,
     1          TOL,MEQL)
              GOTO 10
          END IF
      END IF
   60 RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC05 (SMLCFG, N,M,A,LDA,B,XL,XU,X,ACC,IACT,NACT,
     *  PAR, IPFREQ, IPTOL, IPFRST, IPMORE,
     *  INFO,G,Z,U,XBIG,RELACC,ZZNORM,TOL,MEQL,MTOT,ITERC,NFVPR,NFVALS,
     *  NFMAX,RESKT, SSQKT, BRES,D,ZTG,GM,XS,GS, FVAL, GMODE,
     *  HAVEG, W20)
C
C     MINFUN: Solve the minimization problem using the current value of
c             TOL.
c     1990-07-19 CLL Added FVAL to argument list to return final value
c     of F.
c     1990-07-23 CLL Removed IPRINT from arg list and added IPFREQ,
c        IPTOL, IPFRST, IPMORE, and NFVPR.  NFVPR keeps a record of
c        last function evaluation which printing has been done.
c     1990-07-23 CLL Added logical variable TOLPRT.
c     ------------------------------------------------------------------
c                     Arguments
c
c  SMLCFG, N,M,A(),LDA,B(),XL(),XU(),X(),ACC,IACT(),NACT,
c  PAR()
c  IPFREQ [in, integer]  Zero or positive.  If positive, printing of
c        intermediate results using SMLC21 will be done on iterations
c        0, IPFREQ, 2*IPFREQ, etc.
c  IPTOL [in, integer]  = 0 or 1.  1 means to print the new TOL value
c        and items printed by SMLC21 each time this subr is entered.
c  IPFRST [in, integer]  = 0 or 1.  1 means to print using SMLC21 on
c        the first iteration, i.e. iteration No. 0.
c  IPMORE [in, integer]  = 0 or 1.  Will be passed to SMLC21 to cause
c        printing to include more items when = 1 than when = 0.
c  INFO [inout, integer]  Status flag.
c  G(),Z(),U(),XBIG(),RELACC,ZZNORM,TOL,MEQL,MTOT,
c  NFVALS [inout, integer]  Count of calls for function evaluation.
c        Only counts calls to SMLCFG when the algorithm directly needs a
c        function value, not calls to SMLCFG that occur in SMLC20 for
c        estimation of the gradient.
c  NFMAX [in, integer]  Limit on No. of function evaluations, but zero
c        means no limit.
c  RESKT()
c  SSQKT    Sum of Squares of elts of RESKT().
c  BRES(),D(),ZTG(),GM(),XS(),GS(),
c  FVAL [out, float]  On return set to current (best) value of F.
c  GMODE [inout, integer]  For use by SMLC20.
c  HAVEG [out, logical]  Primarily an internal variable in this subr,
c     but returned so calling subr can use it to select proper error msg
c     when INFO = 3.
c     Set true by SMLCFG if SMLCFG provides the gradient
c     vector and false otherwise.  If false, this subr calls SMLC20 to
c     compute a finite difference approximation for the gradient.
c  W20  [out, work, float] Working storage for SMLC20.
c     ------------------------------------------------------------------
c                     Description of some of the internal variables.
c
c  DOPRNT [logical]  When IPTOL = 1 this subr prints TOL on entry and
c     sets DOPRNT = true.  Also if IPFRST = 1 and ITERC = 0 this subr
c     sets DOPRNT = true.
c     Having DOPRNT = true causes subsequent printing of intermediate
c     results for the current iteration, after PAR() and RESKT() have
c     been computed.
c  HAVEG [logical]  Set true by SMLCFG if SMLCFG provides the gradient
c     vector and false otherwise.  If false, this subr calls SMLC20 to
c     compute a finite difference approximation for the gradient.
c     ------------------------------------------------------------------
      external R1MACH, SMLCFG
      integer N, M, LDA, IACT(*), NACT, IPFREQ, IPTOL, IPFRST, IPMORE,
     *  INFO, MEQL, MTOT, ITERC, NFVPR, NFVALS, NFMAX, GMODE
      integer I, ITERK, K, MSAT, INDXBD
      real             R1MACH
      real             A(LDA,N), ACC, B(*), BRES(*), DDOTG, DIFF
      real             D(*), EPS, F, FPREV, FVAL, G(N), GM(*), GS(*)
      real             PAR(*), RELACC, RELAXF, RESKT(*)
      real             SSQKT, STEP, STEPCB, SUM, TOL
      real             U(*), W20(0:*)
      real             X(*), XBIG(*), XL(N), XS(*), XU(N)
      real             Z(*), ZTG(*), ZZNORM
      logical DOPRNT, HAVEG
      save EPS, F
      data EPS / 0.0e0 /
c     ------------------------------------------------------------------
c                  Initialize the minimization calculation.
c
*     print'(/''SMLC05.. Debug.. On entry, INFO = '',i6,'',  ITERC ='',
*        i6)', INFO, ITERC
      if(EPS .eq. 0.0e0) then
         EPS = R1MACH(4)
      endif
      MSAT=MTOT
      ITERK=ITERC
      IF (NFVALS .EQ. 0 .OR. INFO .EQ. 1) THEN
c++ CODE for ~.C. is active
          CALL SMLCFG (N,X,F,G, HAVEG)
c++ CODE for .C. is inactive
C%%        (*smlcfg)( n, x, &f, g, haveg );
C          HAVEG = HAVEG
c++ END
*         print'('' SMLC05 798.. X()='',2g23.15/(19x,2g23.15))',
*    *      (X(I),I=1,N)
*         print'('' .. F ='',g23.15)', F
*         print'('' ............ G()='',2g23.15/(19x,2g23.15))',
*    *      (G(I),I=1,N)

          NFVALS=NFVALS+1
          if(.not. HAVEG) CALL SMLC20(SMLCFG,N,X,F,G,XL,XU,
     *                         GMODE, NFVALS, W20)
      END IF
      FPREV=abs(F+F+1.0E0)
c                         Setting DIFF here is not needed for the
c                         algorithm, but is convenient when doing
c                         debug printing.  CLL 3/26/92
      DIFF = 1.0e0
      IF (IPTOL .ne. 0) THEN
          print'(/'' New value of TOL = '',g13.5)', TOL
          DOPRNT = .true.
      elseif(IPFRST .ne. 0 .and. ITERC .eq. 0) then
          DOPRNT = .true.
      else
          DOPRNT = .false.
      END IF
C
C     Calculate the next search direction.
C          SMLC06 is CONRES
   10 CALL SMLC06 (N,M,A,LDA,B,XL,XU,X,IACT,NACT,PAR,G,Z,U,XBIG,BRES,D,
     1  ZTG,RELACC,TOL,STEPCB,DDOTG,MEQL,MSAT,MTOT,INDXBD,GM,RESKT,XS,
     2  GS,W20)
C
C     Calculate the Kuhn Tucker residual vector.
C          SMLC16 is KTVEC
      CALL SMLC16 (N,M,A,LDA,IACT,NACT,PAR,G,RESKT,Z,U,BRES,RELAXF,MEQL,
     1  SSQKT,XS,GS)
c
c                     Test for printing results every IPFREQ iterations.
c
      if(IPFREQ .ne. 0) then
         if( mod(ITERC, IPFREQ) .eq. 0) DOPRNT = .true.
      endif
c
      if(DOPRNT) then
         call SMLC21(IPMORE, .true., N, ITERC, NFVALS,
     *           F, TOL, X, G, NACT, IACT, PAR, RESKT, sqrt(SSQKT))
         NFVPR = NFVALS
         DOPRNT = .false.
      endif
C
C     Test for convergence.
C
*     print'(/'' SMLC05 849..''/'' SMLC05 849..'',3g13.5/
*    *   ''             F, FPREV, FPREV-F='',3g13.5/
*    *   ''                          DIFF='',g13.5/
*    *   ''             TOL, RELACC, NACT='',2g13.5,i5)',
*    *   ACC**2, SSQKT, DDOTG, F, FPREV, FPREV-F, DIFF,
*    *   TOL, RELACC, NACT
      IF (SSQKT .LE. ACC*ACC) THEN
          INFO=1
          GOTO 70
      END IF
      IF (DDOTG .GE. 0.0E0) THEN
          INFO=2
          GOTO 70
      END IF
C
C     Test for termination due to no decrease in F.
C
      IF (F .GE. FPREV) THEN
          IF (TOL .EQ. RELACC .OR. NACT .EQ. 0) THEN
              IF (DIFF .GT. 0.0E0) GOTO 20
          END IF
*         print'(/'' SMLC05 869.. DDOTG, EPS*F='',2g13.5)', DDOTG, EPS*F
          if(abs(DDOTG) .le. EPS * abs(F)) then
             INFO = 2
          else
             INFO=3
          endif
          GOTO 70
      END IF
   20 continue
      DIFF=FPREV-F
      FPREV=F
C
C     Test that more calls of SMLCFG are allowed.
C
      IF (NFMAX .gt. 0 .and. NFVALS .ge. NFMAX) THEN
          INFO=8
          GOTO 70
      END IF
C
C                            Test whether to reduce TOL.
C
      IF (TOL .GT. RELACC .AND. ITERC .GT. ITERK .AND.
     1  0.1E0*RELAXF .GE. max(DIFF,-0.5E0*DDOTG)) then
*        print'('' SMLC05.. Debug.. return to reduce TOL.''/
*    *   '' ........ TOL='',g13.5,'',   RELACC='',g13.5,'',  INFO='',
*    *   i5)', TOL, RELACC, INFO
         GOTO 70
      endif
c
C     Calculate the step along the search direction.
C
      ITERC=ITERC+1
*     print'(/'' SMLC05 895.. ITERC = '',i6)', ITERC
C          SMLC09 is LSRCH
      CALL SMLC09 (SMLCFG, N,X,G,D,XS,GS,RELACC,STEPCB,DDOTG,F,
     *             STEP,NFVALS, NFMAX,BRES, XL, XU, GMODE, W20)
      IF (STEP .EQ. 0.0E0) THEN
*         print'(/'' SMLC05 918.. DDOTG, EPS*F='',2g13.5)', DDOTG, EPS*F
          INFO=3
          if(abs(DDOTG) .le. EPS * abs(F)) then
             INFO = 2
          else
             SUM=0.0E0
             DO 50 I=1,N
   50        SUM=SUM+abs(D(I)*GS(I))
*            print'(/'' SMLC05 890..      DDOTG, SUM='',2g13.5/
*    *         '' RELACC*SUM, DDOTG+RELACC*SUM='',2g13.5)',
*    *         DDOTG, SUM, RELACC*SUM, DDOTG+RELACC*SUM
             IF (DDOTG+RELACC*SUM .GE. 0.0E0) INFO=2
          endif
          GOTO 70
      END IF
C
C     Revise XBIG.
C
      DO 60 I=1,N
   60 XBIG(I)=max(XBIG(I),abs(X(I)))
C
C     Revise the second derivative approximation.
C
      CALL SMLC19 (N,X,NACT,G,Z,ZTG,XS,GS,ZZNORM)
C
C     Add a constraint to the active set if it restricts the step.
C
      IF (STEP .EQ. STEPCB) THEN
          K=IACT(INDXBD)
          IF (K .GT. M) THEN
              K=K-M
              IF (K .LE. N) THEN
                  X(K)=XL(K)
              ELSE
                  X(K-N)=XU(K-N)
              END IF
          END IF
          CALL SMLC12 (N,M,A,LDA,IACT,NACT,Z,U,RELACC,INDXBD,XS,GS)
      END IF
      GOTO 10
C
   70 continue
      FVAL = F
      return
      end
c     ==================================================================
      SUBROUTINE SMLC06 (N,M,A,LDA,B,XL,XU,X,IACT,NACT,PAR,G,Z,U,XBIG,
     1  BRES,D,ZTG,RELACC,TOL,STEPCB,SUMRES,MEQL,MSAT,MTOT,INDXBD,
     2  GM,GMNEW,PARNEW,CGRAD,W20)
C
c     CONRES
C     Calculate and partition the residuals of the inactive constraints,
C       and set the gradient vector when seeking feasibility.
C     W20 has length 4*N+1.  Only locations 1 to N are used here.
c     ------------------------------------------------------------------
      integer N,M,LDA,IACT(*),NACT,MEQL,MSAT,MTOT,INDXBD
      integer I, IDIFF, J, JM, K, KL, MDEG, MSATK
      real             A(LDA,*),B(*), DDOTG, PAR(*)
      real             G(*), U(*),XBIG(*),BRES(*)
      real             D(*), GM(*),GMNEW(*),PARNEW(*), CGRAD(*)
      real             RELACC, RES, RESABS, STEPCB, SUM, SUMRES
      real             TEMP, TOL
      real             W20(0:*), XL(*),XU(*),X(*)
      real             Z(*), ZTG(*)
c     ------------------------------------------------------------------
      IDIFF=MTOT-MSAT
C
      IF (IDIFF .GT. 0) THEN
          DO 10 I=1,N
   10     G(I)=0.0E0
          SUMRES=0.0E0
      END IF
      MSATK=MSAT
      MDEG=NACT
      MSAT=NACT
      KL=MEQL+1
      DO 50 K=KL,MTOT
      J=IACT(K)
C
C     Calculate the residual of the current constraint.
C
      IF (J .LE. M) THEN
          RES=B(J)
          RESABS=abs(B(J))
          DO 20 I=1,N
          RES=RES-X(I)*A(J,I)
   20     RESABS=RESABS+abs(XBIG(I)*A(J,I))
      ELSE
          JM=J-M
          IF (JM .LE. N) THEN
              RES=X(JM)-XL(JM)
              RESABS=abs(XBIG(JM))+abs(XL(JM))
          ELSE
              JM=JM-N
              RES=XU(JM)-X(JM)
              RESABS=abs(XBIG(JM))+abs(XU(JM))
          END IF
      END IF
      BRES(J)=RES
C
C     Set TEMP to the relative residual.
C
      TEMP=0.0E0
      IF (RESABS .NE. 0.0E0) TEMP=RES/RESABS
      IF (K .GT. MSATK .AND. TEMP .LT. 0.0E0) THEN
          IF (TEMP+RELACC .GE. 0.0E0) THEN
              IF (J .LE. M) THEN
                  SUM=abs(B(J))
                  DO 30 I=1,N
   30             SUM=SUM+abs(X(I)*A(J,I))
              ELSE
                  JM=J-M
                  IF (JM .LE. N) THEN
                      SUM=abs(X(JM))+abs(XL(JM))
                  ELSE
                      SUM=abs(X(JM-N))+abs(XU(JM-N))
                  END IF
              END IF
              IF (abs(RES) .LE. SUM*RELACC) TEMP=0.0E0
          END IF
      END IF
C
C     Place the residual in the appropriate position.
C
      IF (K .LE. NACT) GOTO 50
      IF (K .LE. MSATK .OR. TEMP .GE. 0.0E0) THEN
          MSAT=MSAT+1
          IF (MSAT .LT. K) THEN
              IACT(K)=IACT(MSAT)
          END IF
          IF (TEMP .GT. TOL) THEN
              IACT(MSAT)=J
          ELSE
              MDEG=MDEG+1
              IACT(MSAT)=IACT(MDEG)
              IACT(MDEG)=J
          END IF
C
C     Update the gradient and SUMRES if the constraint is violated when
C       seeking feasibility.
C
      ELSE
          IF (J .LE. M) THEN
              DO 40 I=1,N
   40         G(I)=G(I)+A(J,I)
          ELSE
              J=J-M
              IF (J .LE. N) THEN
                  G(J)=G(J)-1.0E0
              ELSE
                  G(J-N)=G(J-N)+1.0E0
              END IF
          END IF
          SUMRES=SUMRES+abs(RES)
      END IF
   50 CONTINUE
C
C     Seek the next search direction unless SMLC06 was called from
C     SMLC04 [GETFES] and feasibility has been achieved.
C
      STEPCB=0.0E0
      IF (IDIFF .GT. 0 .AND. MSAT .EQ. MTOT) GOTO 60
c          GETD
      CALL SMLC07 (N,M,A,LDA,IACT,NACT,PAR,G,Z,U,D,ZTG,RELACC,DDOTG,
     *             MEQL, MDEG,GM,GMNEW,PARNEW,CGRAD,W20)
C
C     Calculate the (bound on the) step-length due to the constraints.
C
      IF (DDOTG .LT. 0.0E0) THEN
          CALL SMLC18 (N,M,A,LDA,IACT,BRES,D,STEPCB,DDOTG,MDEG,MSAT,
     1      MTOT,INDXBD)
      END IF
      IF (IDIFF .EQ. 0) SUMRES=DDOTG
   60 RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC07 (N,M,A,LDA,IACT,NACT,PAR,G,Z,U,D,ZTG,RELACC,
     1  DDOTG,MEQL,MDEG,GM,GMNEW,PARNEW,CGRAD,W20)
C
c     GETD
C     Initialize GM and cycle backwards through the active set.
C     W20 has length 4*N+1.  Only locations 0 to N are used here.
c     If W20(1) is nonnegative it means [D/S]MLC20 has been called and
c     has stored into W20(1:N) estimates of the errors in the gradient
c     values stored in G(1:N).
c     If W20(1) is negative the user is computing the gradients and
c     W20(1:N) is not being otherwise used.
c     This subr stores a value into W20(0).  This will only be used if
c     [D/S]MLC20 is called.
c     ------------------------------------------------------------------
      integer N, M, LDA, IACT(*), NACT, MEQL, MDEG
      integer I, IZ, J, JM, K
      real             A(LDA,*), CGRAD(*), D(*), DDOTG, DDOTGM
      real             G(*), GM(*), GMNEW(*)
      real             PAR(*), PARNEW(*), RELACC, SIZE, TEMP, U(*)
      real             W20(0:*), Z(*), ZTG(*)
c     ------------------------------------------------------------------
   10 DO 20 I=1,N
   20 GM(I)=G(I)
      K=NACT
   30 IF (K .GT. 0) THEN
C
C     Set TEMP to the next multiplier, but reduce the active set if
C       TEMP has an unacceptable sign.
C
          TEMP=0.0E0
          IZ=K
          DO 40 I=1,N
          TEMP=TEMP+Z(IZ)*GM(I)
   40     IZ=IZ+N
          TEMP=TEMP*U(K)
          IF (K .GT. MEQL .AND. TEMP .GT. 0.0E0) THEN
              CALL SMLC14 (N,M,A,LDA,IACT,NACT,Z,U,RELACC,K)
              GOTO 10
          END IF
C
C     Update GM using the multiplier that has just been calculated.
C
          J=IACT(K)
          IF (J .LE. M) THEN
              DO 50 I=1,N
   50         GM(I)=GM(I)-TEMP*A(J,I)
          ELSE
              JM=J-M
              IF (JM .LE. N) THEN
                  GM(JM)=GM(JM)+TEMP
              ELSE
                  GM(JM-N)=GM(JM-N)-TEMP
              END IF
          END IF
          PAR(K)=TEMP
          K=K-1
          GOTO 30
      END IF
C
C     Calculate the search direction and DDOTG.
C
      DDOTG=0.0E0
      IF (NACT .LT. N) THEN
c             SDEGEN
         CALL SMLC08 (N,M,A,LDA,IACT,NACT,PAR,Z,U,D,ZTG,GM,RELACC,
     1      DDOTGM,MEQL,MDEG,GMNEW,PARNEW,CGRAD)
         IF (DDOTGM .LT. 0.0E0) THEN
            SIZE = 0.0E0
            DO 60 I=1,N
               TEMP = D(I) * G(I)
               DDOTG=DDOTG + TEMP
               if (W20(1) .ge. 0.0E0) then
                  SIZE = SIZE + abs(D(I)*W20(I))
               else
                  SIZE = SIZE + abs(TEMP)
               endif
   60       continue

            if(W20(1) .lt. 0.0e0) SIZE = RELACC * SIZE
            if (DDOTG .ge. -SIZE) then
               DDOTG = 0.0E0
               W20(0) = 0.0E0
            else
               W20(0) = SIZE/abs(DDOTG)
            endif
         ELSE
            W20(0) = 0.0E0
         END IF
      END IF
      RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC08 (N,M,A,LDA,IACT,NACT,PAR,Z,U,D,ZTG,GM,RELACC,
     1  DDOTGM,MEQL,MDEG,GMNEW,PARNEW,CGRAD)
C
c     SDEGEN
C     Calculate the search direction and branch if it is not downhill.
c     ------------------------------------------------------------------
      integer N, M, LDA, IACT(*), NACT, MEQL, MDEG
      integer I, IDROP, ITEST, IZ, J, JM, K, KU, MP, NP
      real             A(LDA,*), CGRAD(*), D(*), DDOTGM, DTEST
      real             GM(*), GMNEW(*)
      real             PAR(*), PARNEW(*), U(*)
      real             RATIO, RELACC, SUM, TEMP, THETA
      real             Z(*), ZTG(*)
c     ------------------------------------------------------------------
      MP=MEQL+1
      DTEST=0.0E0
C
   10 CALL SMLC17 (N,NACT,Z,D,ZTG,GM,RELACC,DDOTGM)
      IF (DDOTGM .EQ. 0.0E0) GOTO 120
C
C     Branch if there is no need to consider any degenerate constraints.
C     The test gives termination if two consecutive additions to the
C       active set fail to increase the predicted new value of F.
C
      IF (NACT .EQ. MDEG) GOTO 120
      NP=NACT+1
      SUM=0.0E0
      DO 20 J=NP,N
   20 SUM=SUM+ZTG(J)**2
      IF (DTEST .GT. 0.0E0 .AND. SUM .GE. DTEST) THEN
          IF (ITEST .EQ. 1) GOTO 120
          ITEST=1
      ELSE
          DTEST=SUM
          ITEST=0
      END IF
C
C     Add a constraint to the active set if there are any significant
C       violations of degenerate constraints.
C
      K=NACT
      CALL SMLC10 (N,M,A,LDA,IACT,NACT,Z,U,D,RELACC,MDEG,GMNEW,PARNEW,
     1  CGRAD)
      IF (NACT .EQ. K) GOTO 120
      PAR(NACT)=0.0E0
C
C     Calculate the new reduced gradient and Lagrange parameters.
C
   30 DO 40 I=1,N
   40 GMNEW(I)=GM(I)
      K=NACT
   50 TEMP=0.0E0
      IZ=K
      DO 60 I=1,N
      TEMP=TEMP+Z(IZ)*GMNEW(I)
   60 IZ=IZ+N
      TEMP=TEMP*U(K)
      PARNEW(K)=PAR(K)+TEMP
      IF (K .EQ. NACT) PARNEW(K)=min(PARNEW(K),0.0E0)
      J=IACT(K)
      IF (J .LE. M) THEN
          DO 70 I=1,N
   70     GMNEW(I)=GMNEW(I)-TEMP*A(J,I)
      ELSE
          JM=J-M
          IF (JM .LE. N) THEN
              GMNEW(JM)=GMNEW(JM)+TEMP
          ELSE
              GMNEW(JM-N)=GMNEW(JM-N)-TEMP
          END IF
      END IF
      K=K-1
      IF (K .GT. MEQL) GOTO 50
C
C     Set RATIO for linear interpolation between PAR and PARNEW.
C
      RATIO=0.0E0
      IF (MP .LT. NACT) THEN
          KU=NACT-1
          DO 80 K=MP,KU
          IF (PARNEW(K) .GT. 0.0E0) THEN
              RATIO=PARNEW(K)/(PARNEW(K)-PAR(K))
              IDROP=K
          END IF
   80     CONTINUE
      END IF
C
C     Apply the linear interpolation.
C
      THETA=1.0E0-RATIO
      DO 90 K=MP,NACT
   90 PAR(K)=min(THETA*PARNEW(K)+RATIO*PAR(K),0.0E0)
      DO 100 I=1,N
  100 GM(I)=THETA*GMNEW(I)+RATIO*GM(I)
C
C     Drop a constraint if RATIO is positive.
C
      IF (RATIO .GT. 0.0E0) THEN
          CALL SMLC14 (N,M,A,LDA,IACT,NACT,Z,U,RELACC,IDROP)
          DO 110 K=IDROP,NACT
  110     PAR(K)=PAR(K+1)
          GOTO 30
      END IF
C
C     Return if there is no freedom for a new search direction.
C
      IF (NACT .LT. N) GOTO 10
      DDOTGM=0.0E0
  120 RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC09 (SMLCFG, N,X,G,D,XS,GS,RELACC,STEPCB, DDOTG,
     *                   F,STEP, NFVALS,NFMAX,GOPT, XL, XU, GMODE, W20)
c     LSRCH  Line search
c     4/18/91 CLL changed handling of ICOUNT and NFVALS.  Added GMODE.
c     ------------------------------------------------------------------
C                                  Subroutine Arguments
c
c     Input: SMLCFG, N, D(), RELACC, STEPCB, DDOTG, NFMAX, XL(), XU()
c     Inout: X(), G(), F, NFVALS, GMODE, W20()
c     Out:   GS(), STEP
c     Work space: XS(), GOPT()
c
c     SMLCFG, N,X(),G(),D()
c     XS()  [out]  Copy of the initial X().
c     GS()  [out]  Copy of the initial G().
c     RELACC,STEPCB, DDOTG,
c     F,STEP, NFVALS,NFMAX,GOPT(), XL(), XU(),
c     GMODE [integer,inout]  For use by SMLC20.
c     W20() [float,inout]  For use by SMLC20.
c     ------------------------------------------------------------------
      external SMLCFG
      integer N, NFVALS, NFMAX, GMODE
      integer I, ICOUNT
      real             D(*)
      real             DDOTG, DDOTGB, DGHGH, DGKNOT, DGLOW, DGMID, DGOPT
      real             F, FBASE, FHGH, FLOW, FOPT
      real             G(*), GOPT(*), GS(*)
      real             RATIO, RELACC, RELINT, SBASE, SIZE, STEP, STEPCB
      real             STPHGH, STPLOW, STPMIN, STPOPT, TEMP
      real             W20(0:*), X(*), XL(*), XS(*), XU(*)
      logical HAVEG, KEEP, NOBACK
c     ------------------------------------------------------------------
C
C     Initialization.
c
*     print'(/'' SMLC09 1290.. STEPCB='',g13.5/
*    *   ''               .. D()='',3g13.5/(22x,3g13.5))',
*    *   STEPCB, (D(I),I=1,N)
      RELINT=0.9E0
      ICOUNT=0
      RATIO=-1.0E0
      DO 10 I=1,N
      XS(I)=X(I)
      GS(I)=G(I)
      GOPT(I)=G(I)
      IF (D(I) .NE. 0.0E0) THEN
          TEMP=abs(X(I)/D(I))
          IF (RATIO .LT. 0.0E0 .OR. TEMP .LT. RATIO) RATIO=TEMP
      END IF
   10 CONTINUE
      STEP=min(1.0E0,STEPCB)
      STPMIN=max(RELACC*RATIO,1.0E-12*STEP)
      STEP=max(STPMIN,STEP)
      SBASE=0.0E0
      FBASE=F
      DDOTGB=DDOTG
      STPLOW=0.0E0
      FLOW=F
      DGLOW=DDOTG
      STPHGH=0.0E0
      STPOPT=0.0E0
      FOPT=F
      DGOPT=abs(DDOTG)
      NOBACK = .false.
*     print'('' SMLC09 1318.. RATIO,STPMIN='',2g13.5/
*    *   ''                     DDOTG ='',g13.5)',
*    *   RATIO, STPMIN, DDOTG
C
C     Calculate another function and gradient value.
C
   20 continue
      if(NOBACK .and. STEP .lt. STPOPT) then
*        print*,' SMLC09 1355.. Ending SMLC09 on NOBACK.'
         go to 70
      endif
      NOBACK = .false.
      DO 30 I=1,N
   30    X(I)=XS(I)+STEP*D(I)
C%%        (*smlcfg)( n, x, f, g, &haveg );
         call SMLCFG(N, X, F, G, HAVEG)
      NFVALS = NFVALS+1
      if(.not. HAVEG) CALL SMLC20(SMLCFG,N,X,F,G,XL,XU,GMODE,NFVALS,W20)
      ICOUNT=ICOUNT+1
*         print'(/'' SMLC09 1321..  ICOUNT='',i3/
*    *      ''   .. STEP,STPOPT ='',2g23.15)', ICOUNT, STEP, STPOPT
*         print'(''            .. X()='',2g23.15/(19x,2g23.15))',
*    *      (X(I),I=1,N)
*         print'(''             .. F ='',g23.15)',F
*         print'(''            .. G()='',2g23.15/(19x,2g23.15))',
*    *      (G(I),I=1,N)
      DGMID=0.0E0
      SIZE = 0.0e0
      DO 40 I=1,N
         TEMP = D(I)*G(I)
         DGMID =  DGMID + TEMP
         SIZE = SIZE + abs(TEMP)
   40 continue
      SIZE = SIZE * RELACC
*     print'(/'' SMLC09 1335..       FOPT - F ='',g13.5/
*    *   ''            ..          DGMID ='',g13.5/
*    *   ''            ..           SIZE ='',g13.5/
*    *   ''        .. DGOPT - abs(DGMID) ='',g13.5)',
*    *    FOPT - F, DGMID, SIZE, DGOPT - abs(DGMID)
      IF (F .LE. FOPT) THEN
         if(ICOUNT .eq. 1 .and. DGMID .le. 0.0e0) then
*           print '('' SMLC09 1381.. Setting NOBACK on new test.'')'
            NOBACK = .true.
            KEEP = .true.
         else
            KEEP = F .LT. FOPT .OR. abs(DGMID) .LT. DGOPT
         endif
         if(KEEP) then
              STPOPT=STEP
*             print'(/'' SMLC09 1384.. New STPOPT='',g13.5)', STPOPT
              FOPT=F
              DO 50 I=1,N
   50         GOPT(I)=G(I)
              DGOPT=abs(DGMID)
          END IF
      END IF
      IF (NFMAX .gt. 0 .and. NFVALS .ge. NFMAX) GOTO 70
C
C      Modify the bounds on the steplength or convergence.
C
      IF (F .GE. FBASE+0.1E0*(STEP-SBASE)*DDOTGB) THEN
          IF (STPHGH .GT. 0.0E0 .OR. F .GT. FBASE .OR. DGMID .GT.
     1      0.5E0*DDOTG) THEN
              STPHGH=STEP
              FHGH=F
              DGHGH=DGMID
              GOTO 60
          END IF
          SBASE=STEP
          FBASE=F
          DDOTGB=DGMID
      END IF
      IF (DGMID .GE. 0.7E0*DDOTGB) GOTO 70
      STPLOW=STEP
      FLOW=F
      DGLOW=DGMID
   60 IF (STPHGH .GT. 0.0E0 .AND. STPLOW .GE. RELINT*STPHGH) GOTO 70
C
C     Calculate the next step length or end the iterations.
C
*     print'(/'' SMLC09 1366.. ICOUNT, STEP='',i23,g23.15/
*    *   ''             STPLOW, STPHGH='',2g23.15)',
*    *   ICOUNT, STEP, STPLOW, STPHGH
      IF (STPHGH .EQ. 0.0E0) THEN
          IF (STEP .EQ. STEPCB) GOTO 70
          TEMP=10.0E0
          IF (DGMID .GT. 0.9E0*DDOTG) TEMP=DDOTG/(DDOTG-DGMID)
          STEP=min(TEMP*STEP,STEPCB)
*         print'('' SMLC09 1374.. To 20 with STEP='',g23.15)', STEP
          GOTO 20
      ELSE IF (ICOUNT .EQ. 1 .OR. STPLOW .GT. 0.0E0) THEN
          DGKNOT=2.0E0*(FHGH-FLOW)/(STPHGH-STPLOW)-0.5E0*(DGLOW+DGHGH)
          IF (DGKNOT .GE. 0.0E0) THEN
              RATIO=max(0.1E0,0.5E0*DGLOW/(DGLOW-DGKNOT))
          ELSE
              RATIO=(0.5E0*DGHGH-DGKNOT)/(DGHGH-DGKNOT)
          END IF
          STEP=STPLOW+RATIO*(STPHGH-STPLOW)
*         print'('' SMLC09 1384.. To 20 with STEP='',g23.15)', STEP
          GOTO 20
      ELSE
          STEP=0.1E0*STEP
          if (STEP .GE. STPMIN) then
*         print'('' SMLC09 1389.. To 20 with STEP='',g23.15)', STEP
             GOTO 20
          endif
      END IF
C
C     Return from subroutine.
C
   70 continue
      IF (STEP .NE. STPOPT) THEN
          STEP=STPOPT
          F=FOPT
          DO 80 I=1,N
             X(I)=XS(I)+STEP*D(I)
   80     G(I)=GOPT(I)
      END IF
      RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC10 (N,M,A,LDA,IACT,NACT,Z,U,D,RELACC,MDEG,ZZDIAG,
     1  GMNEW,CGRAD)
c     NEWCON
c     ------------------------------------------------------------------
      integer N, M, LDA, IACT(*), NACT, MDEG
      integer I, IADD, IZ, J, JM, JMV, K, KHIGH, NP
      real             A(LDA,*), CGRAD(*), CVIOL, CVMAX, D(*), GMNEW(*)
      real             RELACC, SAVABS, SAVSUM, SUM, SUMABS, SUMD, TEMP
      real             U(*)
      real             Z(*), ZZDIAG(*)
c     ------------------------------------------------------------------
C
C     Initialization.
C
      NP=NACT+1
      KHIGH=MDEG
      IZ=0
      DO 20 I=1,N
      ZZDIAG(I)=0.0E0
      DO 10 J=NP,N
   10 ZZDIAG(I)=ZZDIAG(I)+Z(IZ+J)**2
   20 IZ=IZ+N
C
C     Calculate the scalar products of D with its constraints.
C
   30 CVMAX=0.0E0
      DO 50 K=NP,KHIGH
      J=IACT(K)
      IF (J .LE. M) THEN
          SUM=0.0E0
          SUMABS=0.0E0
          SUMD=0.0E0
          DO 40 I=1,N
          TEMP=D(I)*A(J,I)
          SUM=SUM+TEMP
          SUMABS=SUMABS+abs(TEMP)
   40     SUMD=SUMD+ZZDIAG(I)*A(J,I)**2
      ELSE
          JM=J-M
          IF (JM .LE. N) THEN
              SUM=-D(JM)
          ELSE
              JM=JM-N
              SUM=D(JM)
          END IF
          SUMABS=abs(SUM)
          SUMD=ZZDIAG(JM)
      END IF
C
C     Pick out the most violated constraint, or return if the
C       violation is negligible.
C
      IF (SUM .GT. RELACC*SUMABS) THEN
          CVIOL=SUM*SUM/SUMD
          IF (CVIOL .GT. CVMAX) THEN
              CVMAX=CVIOL
              IADD=K
              SAVSUM=SUM
              SAVABS=SUMABS
          END IF
      END IF
   50 CONTINUE
      IF (CVMAX .LE. 0.0E0) GOTO 140
      IF (NACT .EQ. 0) GOTO 120
C
C     Set GMNEW to the gradient of the most violated constraint.
C
      J=IACT(IADD)
      IF (J .LE. M) THEN
          JMV=0
          DO 60 I=1,N
   60     GMNEW(I)=A(J,I)
      ELSE
          JMV=J-M
          DO 70 I=1,N
   70     GMNEW(I)=0.0E0
          IF (JMV .LE. N) THEN
              GMNEW(JMV)=-1.0E0
          ELSE
              JMV=JMV-N
              GMNEW(JMV)=1.0E0
          END IF
      END IF
C
C     Modify GMNEW for the next active constraint.
C
      K=NACT
   80 TEMP=0.0E0
      IZ=K
      DO 90 I=1,N
      TEMP=TEMP+Z(IZ)*GMNEW(I)
   90 IZ=IZ+N
      TEMP=TEMP*U(K)
      J=IACT(K)
      IF (J .LE. M) THEN
          DO 100 I=1,N
  100     GMNEW(I)=GMNEW(I)-TEMP*A(J,I)
      ELSE
          JM=J-M
          IF (JM .LE. N) THEN
              GMNEW(JM)=GMNEW(JM)+TEMP
          ELSE
              GMNEW(JM-N)=GMNEW(JM-N)-TEMP
          END IF
      END IF
C
C     Revise the values of SAVSUM and SAVABS.
C
      SUM=0.0E0
      SUMABS=0.0E0
      DO 110 I=1,N
      TEMP=D(I)*GMNEW(I)
      SUM=SUM+TEMP
  110 SUMABS=SUMABS+abs(TEMP)
      SAVSUM=min(SAVSUM,SUM)
      SAVABS=max(SAVABS,SUMABS)
      K=K-1
      IF (K .GE. 1) GOTO 80
C
C     Add the new constraint to the active set if the constraint
C       violation is still significant.
C
      IF (JMV .GT. 0) D(JMV)=0.0E0
      IF (SAVSUM .LE. RELACC*SAVABS) GOTO 130
  120 K=NACT
      CALL SMLC12 (N,M,A,LDA,IACT,NACT,Z,U,RELACC,IADD,GMNEW,CGRAD)
      IF (NACT .GT. K) GOTO 140
C
C     Seek another constraint violation.
C
      IADD=NP
  130 IF (NP .LT. KHIGH) THEN
          K=IACT(KHIGH)
          IACT(KHIGH)=IACT(IADD)
          IACT(IADD)=K
          KHIGH=KHIGH-1
          GOTO 30
      END IF
  140 RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC11 (N,M,A,LDA,B,XL,XU,X,IACT,NACT,INFO,Z,U,XBIG,
     1  RELACC,TOL,MEQL)
c     SATACT
c     ------------------------------------------------------------------
      integer N, M, LDA, IACT(*), NACT, INFO
      integer I, IDROP, IZ, J, JX, K, MEQL
      real             A(LDA,*),B(*)
      real             RELACC, RES, RESABS, RESBIG, SAVEX, SCALE
      real             TEMP, TEMPA, TOL, U(*)
      real             X(*), XBIG(*), XL(*),XU(*), Z(*)
c     ------------------------------------------------------------------
      IF (NACT .EQ. 0) GOTO 50
      DO 30 K=1,NACT
C
C     Calculate the next constraint residual.
C
      J=IACT(K)
      IF (J .LE. M) THEN
          RES=B(J)
          RESABS=abs(B(J))
          RESBIG=RESABS
          DO 10 I=1,N
          TEMPA=A(J,I)
          TEMP=TEMPA*X(I)
          RES=RES-TEMP
          RESABS=RESABS+abs(TEMP)
   10     RESBIG=RESBIG+abs(TEMPA)*XBIG(I)
      ELSE
          JX=J-M
          IF (JX .LE. N) THEN
              RES=X(JX)-XL(JX)
              RESABS=abs(X(JX))+abs(XL(JX))
              RESBIG=XBIG(JX)+abs(XL(JX))
              SAVEX=XL(JX)
          ELSE
              JX=JX-N
              RES=XU(JX)-X(JX)
              RESABS=abs(X(JX))+abs(XU(JX))
              RESBIG=XBIG(JX)+abs(XU(JX))
              SAVEX=XU(JX)
          END IF
      END IF
C
C     Shift X if necessary.
C
      IF (RES .NE. 0.0E0) THEN
          TEMP=RES/RESABS
          IF (K .LE. MEQL) TEMP=-abs(TEMP)
          IF (TOL .EQ. RELACC .OR. TEMP+RELACC .LT. 0.0E0) THEN
              INFO=1
              SCALE=RES*U(K)
              IZ=K
              DO 20 I=1,N
              X(I)=X(I)+SCALE*Z(IZ)
              IZ=IZ+N
   20         XBIG(I)=max(XBIG(I),abs(X(I)))
              IF (J .GT. M) X(JX)=SAVEX
C
C     Else flag a constraint deletion if necessary.
C
          ELSE IF (RES/RESBIG .GT. TOL) THEN
              IACT(K)=-IACT(K)
          END IF
      END IF
   30 CONTINUE
C
C     Delete any flagged constraints and then return.
C
      IDROP=NACT
  40  IF (IACT(IDROP) .LT. 0) THEN
          IACT(IDROP)=-IACT(IDROP)
          CALL SMLC14 (N,M,A,LDA,IACT,NACT,Z,U,RELACC,IDROP)
      END IF
      IDROP=IDROP-1
      IF (IDROP .GT. MEQL) GOTO 40
   50 RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC12 (N,M,A,LDA,IACT,NACT,Z,U,RELACC,INDXBD,ZTC,
     1  CGRAD)
c     ADDCON
c     ------------------------------------------------------------------
      integer N, M, LDA, IACT(*), NACT, INDXBD
      integer I, ICON, INEWBD, IPIV, IZ, IZNBD, J, JP, NP
      real             A(LDA,*), CGRAD(*)
      real             RELACC, SUM, SUMABS, TEMP, TEMPA, TEMPB
      real             U(*), WCOS, WPIV, WSIN
      real             Z(*), ZTC(*)
c     ------------------------------------------------------------------
      NP=NACT+1
      ICON=IACT(INDXBD)
      IACT(INDXBD)=IACT(NP)
      IACT(NP)=ICON
C
C     Form ZTC when the new constraint is a bound.
C
      IF (ICON .GT. M) THEN
          INEWBD=ICON-M
          IF (INEWBD .LE. N) THEN
              TEMP=-1.0E0
          ELSE
              INEWBD=INEWBD-N
              TEMP=1.0E0
          END IF
          IZNBD=INEWBD*N-N
          DO 10 J=1,N
   10     ZTC(J)=TEMP*Z(IZNBD+J)
C
C     Else form ZTC for an ordinary constraint.
C
      ELSE
          DO 20 I=1,N
   20     CGRAD(I)=A(ICON,I)
          DO 30 J=1,N
          ZTC(J)=0.0E0
          IZ=J
          DO 30 I=1,N
          ZTC(J)=ZTC(J)+Z(IZ)*CGRAD(I)
   30     IZ=IZ+N
      END IF
C
C     Find any Givens rotations to apply to the last columns of Z.
C
      J=N
   40 JP=J
      J=J-1
      IF (J .GT. NACT) THEN
          IF (ZTC(JP) .EQ. 0.0E0) GOTO 40
          IF (abs(ZTC(JP)) .LE. RELACC*abs(ZTC(J))) THEN
              TEMP=abs(ZTC(J))
          ELSE IF (abs(ZTC(J)) .LE. RELACC*abs(ZTC(JP))) THEN
              TEMP=abs(ZTC(JP))
          ELSE
              TEMP=abs(ZTC(JP))*sqrt(1.0E0+(ZTC(J)/ZTC(JP))**2)
          END IF
          WCOS=ZTC(J)/TEMP
          WSIN=ZTC(JP)/TEMP
          ZTC(J)=TEMP
C
C     Apply the rotation when the new constraint is a bound.
C
          IZ=J
          IF (ICON .GT. M) THEN
              DO 50 I=1,N
              TEMP=WCOS*Z(IZ+1)-WSIN*Z(IZ)
              Z(IZ)=WCOS*Z(IZ)+WSIN*Z(IZ+1)
              Z(IZ+1)=TEMP
   50         IZ=IZ+N
              Z(IZNBD+JP)=0.0E0
C
C     Else apply the rotation for an ordinary constraint.
C
          ELSE
              WPIV=0.0E0
              DO 60 I=1,N
              TEMPA=WCOS*Z(IZ+1)
              TEMPB=WSIN*Z(IZ)
              TEMP=abs(CGRAD(I))*(abs(TEMPA)+abs(TEMPB))
              IF (TEMP .GT. WPIV) THEN
                  WPIV=TEMP
                  IPIV=I
              END IF
              Z(IZ)=WCOS*Z(IZ)+WSIN*Z(IZ+1)
              Z(IZ+1)=TEMPA-TEMPB
   60         IZ=IZ+N
C
C     Ensure orthogonality of Z(.,JP) to CGRAD.
C
              SUM=0.0E0
              IZ=JP
              DO 70 I=1,N
              SUM=SUM+Z(IZ)*CGRAD(I)
   70         IZ=IZ+N
              IF (SUM .NE. 0.0E0) THEN
                  IZ=IPIV*N-N+JP
                  Z(IZ)=Z(IZ)-SUM/CGRAD(IPIV)
              END IF
          END IF
          GO TO 40
      END IF
C
C     Test for linear independence in the proposed new active set.
C
      IF (ZTC(NP) .EQ. 0.0E0) GOTO 90
      IF (ICON .LE. M) THEN
          SUM=0.0E0
          SUMABS=0.0E0
          IZ=NP
          DO 80 I=1,N
          TEMP=Z(IZ)*CGRAD(I)
          SUM=SUM+TEMP
          SUMABS=SUMABS+abs(TEMP)
   80     IZ=IZ+N
          IF (abs(SUM) .LE. RELACC*SUMABS) GOTO 90
      END IF
C
C     Set the new diagonal element of U() and return.
C
      U(NP)=1.0E0/ZTC(NP)
      NACT=NP
   90 RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC13 (N,M,A,LDA,B,XL,XU,X,IACT,NACT,XBIG,RELACC,TOL,
     1  MEQL)
C
C     ADJTOL:  Change the tolerance TOL to a smaller value, if possible.
c     May also recompute XBIG().
c     ------------------------------------------------------------------
      integer N, M, LDA, IACT(*), NACT, MEQL
      integer I, J, JM, K, KL
      real             A(LDA,*), B(*)
      real             RELACC, RES, RESABS, TOL, VIOL
      real             X(*), XBIG(*), XL(*), XU(*)
c     ------------------------------------------------------------------
C     Set VIOL to the greatest relative constraint residual of the first
C       NACT constraints.
      VIOL=0.0E0
      IF (NACT .GT. MEQL) THEN
          KL=MEQL+1
          DO 20 K=KL,NACT
          J=IACT(K)
          IF (J .LE. M) THEN
              RES=B(J)
              RESABS=abs(B(J))
              DO 10 I=1,N
              RES=RES-A(J,I)*X(I)
   10         RESABS=RESABS+abs(A(J,I)*XBIG(I))
          ELSE
              JM=J-M
              IF (JM .LE. N) THEN
                  RES=X(JM)-XL(JM)
                  RESABS=XBIG(JM)+abs(XL(JM))
              ELSE
                  JM=JM-N
                  RES=XU(JM)-X(JM)
                  RESABS=XBIG(JM)+abs(XU(JM))
              END IF
          END IF
          IF (RES .GT. 0.0E0) VIOL=max(VIOL,RES/RESABS)
   20     CONTINUE
      END IF
C
C     Adjust TOL.
C
      TOL=0.1E0*min(TOL,VIOL)
      IF (TOL .LE. RELACC+RELACC) THEN
          TOL=RELACC
          DO 30 I=1,N
   30     XBIG(I)=abs(X(I))
      END IF
      RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC14 (N,M,A,LDA,IACT,NACT,Z,U,RELACC,IDROP)
C
C     DELCON: Cycle through the constraint exchanges that are needed.
c     ------------------------------------------------------------------
      integer N, M, LDA, IACT(*), NACT, IDROP
      integer I, IBD, ICON, IPIV, ISAVE, IZ, IZBD, J, JP, NM
      real             A(LDA,*), DENOM
      real             RELACC, RJJP, SUM, TEMP, TEMPA, TEMPB
      real             U(*), UJP, WCOS,  WPIV, WSIN
      real             Z(*)
c     ------------------------------------------------------------------
      NM=NACT-1
      IF (IDROP .EQ. NACT) GOTO 60
      ISAVE=IACT(IDROP)
C
C     Cycle through the constraint exchanges that are needed.
C
      DO 50 J=IDROP,NM
      JP=J+1
      ICON=IACT(JP)
      IACT(J)=ICON
C
C     Calculate the (J,JP) element of R.
C
      IF (ICON .LE. M) THEN
          RJJP=0.0E0
          IZ=J
          DO 10 I=1,N
          RJJP=RJJP+Z(IZ)*A(ICON,I)
   10     IZ=IZ+N
      ELSE
          IBD=ICON-M
          IF (IBD .LE. N) THEN
              IZBD=IBD*N-N
              RJJP=-Z(IZBD+J)
          ELSE
              IBD=IBD-N
              IZBD=IBD*N-N
              RJJP=Z(IZBD+J)
          END IF
      END IF
C
C     Calculate the parameters of the next rotation.
C
      UJP=U(JP)
      TEMP=RJJP*UJP
      DENOM=abs(TEMP)
      IF (DENOM*RELACC .LT. 1.0E0) DENOM=sqrt(1.0E0+DENOM*DENOM)
      WCOS=TEMP/DENOM
      WSIN=1.0E0/DENOM
C
C     Rotate Z when a bound constraint is promoted.
C
      IZ=J
      IF (ICON .GT. M) THEN
          DO 20 I=1,N
          TEMP=WCOS*Z(IZ+1)-WSIN*Z(IZ)
          Z(IZ)=WCOS*Z(IZ)+WSIN*Z(IZ+1)
          Z(IZ+1)=TEMP
   20     IZ=IZ+N
          Z(IZBD+JP)=0.0E0
C
C     Rotate Z when an ordinary constraint is promoted.
C
      ELSE
          WPIV=0.0E0
          DO 30 I=1,N
          TEMPA=WCOS*Z(IZ+1)
          TEMPB=WSIN*Z(IZ)
          TEMP=abs(A(ICON,I))*(abs(TEMPA)+abs(TEMPB))
          IF (TEMP .GT. WPIV) THEN
              WPIV=TEMP
              IPIV=I
          END IF
          Z(IZ)=WCOS*Z(IZ)+WSIN*Z(IZ+1)
          Z(IZ+1)=TEMPA-TEMPB
   30     IZ=IZ+N
C
C     Ensure orthogonality to promoted constraint.
C
          SUM=0.0E0
          IZ=JP
          DO 40 I=1,N
          SUM=SUM+Z(IZ)*A(ICON,I)
   40     IZ=IZ+N
          IF (SUM .NE. 0.0E0) THEN
              IZ=IPIV*N-N+JP
              Z(IZ)=Z(IZ)-SUM/A(ICON,IPIV)
          END IF
      END IF
C
C     Set the new diagonal elements of U.
C
      U(JP)=-DENOM*U(J)
      U(J)=UJP/DENOM
   50 CONTINUE
C
C     Return.
C
      IACT(NACT)=ISAVE
   60 NACT=NM
      RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC15 (N,M,XL,XU,X,IACT,MEQL,INFO,Z,U,XBIG,RELACC)
c     INITZU:  Initialize RELACC, Z(), U().  Adjust X().  Set XBIG().
c     1990-07-20 CLL Changed error handling and setting of INFO.
c     Expect INFO to be set to 1 before entering this subr.  This subr
c     sets INFO = 4 if it finds XL(j) > XU(j) for some j.
c     ------------------------------------------------------------------
      external R1MACH
      integer N, M, IACT(*), MEQL, INFO
      integer I, IZ, J, JACT, NN
      real             R1MACH
      real             RELACC
      real             U(*)
      real             XL(*),XU(*),X(*), XBIG(*), Z(*)
c     ------------------------------------------------------------------
C
C                                                Set RELACC.
c     July 1990 CLL Commented out following 6 lines and replaced with
c     reference to R1MACH(4) which is the smallest x such that the
c     stored value of 1+x is greater than 1.  Efforts to determine
c     such an x with portable code such as the following 6 lines have
c     generally eventually failed on some new computer.  Also using
c     R1MACH allows adjustments to be made for known deficiencies
c     in particular computers, for example for the Cray X/MP & Y/MP.
c
*     ZTPAR=100.0E0
*     RELACC=1.0E0
*  10 RELACC=0.5E0*RELACC
*     TEMPA=ZTPAR+0.5E0*RELACC
*     TEMPB=ZTPAR+RELACC
*     IF (ZTPAR .LT. TEMPA .AND. TEMPA .LT. TEMPB) GOTO 10
      RELACC = 100.0E0 * R1MACH(4)
C
C     Seek bound inconsistencies and bound equality constraints.
C
      MEQL=0
      DO 20 J=1,N
         IF (XL(J) .GT. XU(J)) then
            INFO = 4
            call IERM1('SMLC15',INFO,0,'Bad bounds: XL(j) > XU(j)',
     *                 'j',J,',')
            call SERV1('XL(j)',XL(J),',')
            call SERV1('XU(j)',XU(J),'.')
            return
         endif
      IF (XL(J) .EQ. XU(J)) MEQL=MEQL+1
   20 CONTINUE
C
C     Initialize U, Z and XBIG.
C
      JACT=0
      NN=N*N
      DO 30 I=1,NN
   30 Z(I)=0.0E0
      IZ=0
      DO 40 I=1,N
      IF (XL(I) .EQ. XU(I)) THEN
          X(I)=XU(I)
          JACT=JACT+1
          U(JACT)=1.0E0
          IACT(JACT)=I+M+N
          J=JACT
      ELSE
          J=I+MEQL-JACT
      END IF
      Z(IZ+J)=1.0E0
      IZ=IZ+N
   40 XBIG(I)=abs(X(I))
      RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC16 (N,M,A,LDA,IACT,NACT,PAR,G,RESKT,Z,U,BRES,
     *  RELAXF, MEQL,SSQKT,PARW,RESKTW)
C
C     Calculate the Lagrange parameters and the residual vector.
c
c     Input:  N, M, A(), LDA, IACT, NACT, G(), Z(), U(), BRES(), MEQL
c     Output: PAR(), RESKT, RELAXF, SSQKT
c     Work space: PARW(), RESKTW()
c     ------------------------------------------------------------------
      integer N, M, LDA, IACT(*), NACT, MEQL
      integer I, ICASE, IZ, J, JM, K, KK, KL
      real             A(LDA,*), BRES(*), G(*)
      real             PAR(*), PARW(*)
      real             RELAXF, RESKT(*), RESKTW(*)
      real             SSQKT, SSQKTW, TEMP, U(*), Z(*)
c     ------------------------------------------------------------------
      DO 10 I=1,N
   10 RESKT(I)=G(I)
*     print'(/'' SMLC16 1993.. G()='',2g23.15/(19x,2g23.15))',
*    *   (G(I),I=1,N)
*     print'('' .. U()='', 3g23.15/(8x,3g23.15))', (U(I),I=1,3)
*     print'('' .. Z()='', 2g23.15/(8x,2g23.15))', (Z(I),I=1,16)
*     print'('' .. A(,)='',2g23.15/(9x,2g23.15))',((A(I,J),J=1,4),I=1,3)
      IF (NACT .GT. 0) THEN
          ICASE=0
   20     DO 50 KK=1,NACT
          K=NACT+1-KK
          J=IACT(K)
          TEMP=0.0E0
          IZ=K
          DO 30 I=1,N
          TEMP=TEMP+Z(IZ)*RESKT(I)
*       print'('' SMLC16 1991.. Z(),RESKT(),TEMP='',3g13.5)',
*    *      Z(IZ),RESKT(I),TEMP
   30     IZ=IZ+N
          TEMP=TEMP*U(K)
*       print'('' SMLC16 1995.. U(K),TEMP='', 2g13.5)', U(K), TEMP
          IF (ICASE .EQ. 0) PAR(K)=0.0E0
          IF (K .LE. MEQL .OR. PAR(K)+TEMP .LT. 0.0E0) THEN
              PAR(K)=PAR(K)+TEMP
*           print'('' SMLC16 1999.. TEMP,PAR(K)='',2g13.5)',
*    *         TEMP,PAR(K)
          ELSE
              TEMP=-PAR(K)
              PAR(K)=0.0E0
          END IF
          IF (TEMP .NE. 0.0E0) THEN
              IF (J .LE. M) THEN
                  DO 40 I=1,N
                     RESKT(I)=RESKT(I)-TEMP*A(J,I)
*          print'('' SMLC16 2009.. TEMP,A='',2g13.5/
*    *       ''                  RESKT(I) ='',g13.5)',
*    *       TEMP, A(J,I), RESKT(I)
   40             continue
              ELSE
                  JM=J-M
                  IF (JM .LE. N) THEN
                      RESKT(JM)=RESKT(JM)+TEMP
*          print'('' SMLC16 2018.. TEMP,RESKT(JM)='',2g13.5)',
*    *       TEMP,RESKT(JM)
                  ELSE
                      RESKT(JM-N)=RESKT(JM-N)-TEMP
*          print'('' SMLC16 2022.. TEMP,RESKT(JM-N)='',2g13.5)',
*    *       TEMP,RESKT(JM-N)
                  END IF
              END IF
          END IF
   50     CONTINUE
C
C     Calculate the sum of squares of the KT residual vector.
C
          SSQKT=0.0E0
          IF (NACT .EQ. N) GOTO 130
*         print'(/'' SMLC16 2014.. RESKT()='',4g12.4/(23x,4g12.4))',
*    *       (RESKT(I),I=1,N)
          DO 60 I=1,N
   60     SSQKT=SSQKT+RESKT(I)**2
*        print'('' SMLC16 2018.. SSQKT='',g13.5)', SSQKT
C
C     Apply iterative refinement to the residual vector.
C
          IF (ICASE .EQ. 0) THEN
              ICASE=1
              DO 70 K=1,NACT
   70         PARW(K)=PAR(K)
              DO 80 I=1,N
   80         RESKTW(I)=RESKT(I)
              SSQKTW=SSQKT
              GOTO 20
          END IF
C
C     Undo the iterative refinement if it does not reduce SSQKT.
C
          IF (SSQKTW .LT. SSQKT) THEN
              DO 90 K=1,NACT
   90         PAR(K)=PARW(K)
              DO 100 I=1,N
  100         RESKT(I)=RESKTW(I)
              SSQKT=SSQKTW
          END IF
C
C     Calculate SSQKT when there are no active constraints.
C
      ELSE
          SSQKT=0.0E0
          DO 110 I=1,N
  110     SSQKT=SSQKT+G(I)**2
      END IF
C
C     Predict the reduction in F if one corrects any positive residuals
C       of active inequality constraints.
C
      RELAXF=0.0E0
      IF (MEQL .LT. NACT) THEN
          KL=MEQL+1
          DO 120 K=KL,NACT
          J=IACT(K)
          IF (BRES(J) .GT. 0.0E0) THEN
              RELAXF=RELAXF-PAR(K)*BRES(J)
          END IF
  120     CONTINUE
      END IF
  130 RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC17 (N,NACT,Z,D,ZTG,GM,RELACC,DDOTGM)
c     SDIRN:  Compute a search direction.
c     ------------------------------------------------------------------
      integer N, NACT
      integer I, IZ, J, NP
      real             D(*), DDOTGM, GM(*)
      real             RELACC, SUM, SUMABS, TEMP
      real             Z(*), ZTG(*)
c     ------------------------------------------------------------------
      DDOTGM=0.0E0
      IF (NACT .GE. N) GOTO 60
C
C     Premultiply GM by the transpose of Z.
C
      NP=NACT+1
      DO 20 J=NP,N
      SUM=0.0E0
      SUMABS=0.0E0
      IZ=J
      DO 10 I=1,N
      TEMP=Z(IZ)*GM(I)
      SUM=SUM+TEMP
      SUMABS=SUMABS+abs(TEMP)
   10 IZ=IZ+N
      IF (abs(SUM) .LE. RELACC*SUMABS) SUM=0.0E0
   20 ZTG(J)=SUM
C
C     Form D by premultiplying ZTG by -Z.
C
      IZ=0
      DO 40 I=1,N
      SUM=0.0E0
      SUMABS=0.0E0
      DO 30 J=NP,N
      TEMP=Z(IZ+J)*ZTG(J)
      SUM=SUM-TEMP
   30 SUMABS=SUMABS+abs(TEMP)
      IF (abs(SUM) .LE. RELACC*SUMABS) SUM=0.0E0
      D(I)=SUM
   40 IZ=IZ+N
C
C     Test that the search direction is downhill.
C
      SUMABS=0.0E0
      DO 50 I=1,N
      TEMP=D(I)*GM(I)
      DDOTGM=DDOTGM+TEMP
   50 SUMABS=SUMABS+abs(TEMP)
      IF (DDOTGM+RELACC*SUMABS .GE. 0.0E0) DDOTGM=0.0E0
   60 RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC18 (N,M,A,LDA,IACT,BRES,D,STEPCB,DDOTG,MDEG,MSAT,
     1  MTOT,INDXBD)
c     STEPBD:
C     Set steps to constraint boundaries and find the least positive
C     one.
c
c>> 1990-04-02 CLL Changes to avoid jumping into scope of Block If.
c     ------------------------------------------------------------------
      integer N, M, LDA, IACT(*), MDEG, MSAT, MTOT, INDXBD
      integer I, IFLAG, J, JM, K, KL
      real             A(LDA,*),BRES(*),D(*), DDOTG, SP, STEPCB, TEMP
c     ------------------------------------------------------------------
      IFLAG=0
      STEPCB=0.0E0
      INDXBD=0
      K=MDEG
   10 K=K+1
      IF (K .gt. MTOT) go to 30
c
c                    Use remote code to compute scalar product.
      go to 80
   20 continue
C
C     Set BRES(J) to indicate the status of the j-th constraint.
C
          IF (SP*BRES(J) .LE. 0.0E0) THEN
              BRES(J)=0.0E0
          ELSE
              BRES(J)=BRES(J)/SP
              IF (STEPCB .EQ. 0.0E0 .OR. BRES(J) .LT. STEPCB) THEN
                  STEPCB=BRES(J)
                  INDXBD=K
              END IF
          END IF
          GO TO 10
   30 continue
C
C     Try to pass through the boundary of a violated constraint.
C
   40 continue
      IF (INDXBD .le. MSAT) go to 70
          IFLAG=1
          K=INDXBD
c                    Use remote code to compute scalar product.
          go to 80
   50     continue
          MSAT=MSAT+1
          IACT(INDXBD)=IACT(MSAT)
          IACT(MSAT)=J
          BRES(J)=0.0E0
          INDXBD=MSAT
          DDOTG=DDOTG-SP
          IF (DDOTG .LT. 0.0E0 .AND. MSAT .LT. MTOT) THEN
C
C     Seek the next constraint boundary along the search direction.
C
              TEMP=0.0E0
              KL=MDEG+1
              DO 60 K=KL,MTOT
              J=IACT(K)
              IF (BRES(J) .GT. 0.0E0) THEN
                  IF (TEMP .EQ. 0.0E0 .OR. BRES(J) .LT. TEMP) THEN
                      TEMP=BRES(J)
                      INDXBD=K
                  END IF
              END IF
   60         CONTINUE
              IF (TEMP .GT. 0.0E0) THEN
                  STEPCB=TEMP
                  GOTO 40
              END IF
          END IF
   70 continue
      RETURN
c     ------------------------------------------------------------------
c         This is a remote block of code to compute the
C         scalar product of D with the current constraint normal.
C
   80     J=IACT(K)
          IF (J .LE. M) THEN
              SP=0.0E0
              DO 90 I=1,N
   90         SP=SP+D(I)*A(J,I)
          ELSE
              JM=J-M
              IF (JM .LE. N) THEN
                  SP=-D(JM)
              ELSE
                  SP=D(JM-N)
              END IF
          END IF
C                         Return from remote block is selected by IFLAG.
          IF (IFLAG .EQ. 0) then
             go to 20
          ELSE
             go to 50
          ENDIF
      END
c     ==================================================================
      SUBROUTINE SMLC19 (N,X,NACT,G,Z,ZTG,XS,GS,ZZNORM)
C
C     Test if there is sufficient convexity for the update.
c     ------------------------------------------------------------------
      integer N,NACT
      integer I, IZ, K, KP, KM, NP
      real             DD, DG, G(*), GS(*), SUM, TEMP, WCOS, WSIN
      real             X(*), XS(*), Z(*),ZTG(*), ZZNORM
c     ------------------------------------------------------------------
      DD=0.0E0
      DG=0.0E0
      TEMP=0.0E0
      DO 10 I=1,N
      XS(I)=X(I)-XS(I)
      DD=DD+XS(I)**2
      TEMP=TEMP+GS(I)*XS(I)
      GS(I)=G(I)-GS(I)
   10 DG=DG+GS(I)*XS(I)
      IF (DG .LT. 0.1E0*abs(TEMP)) GOTO 90
C
C     Transform the Z matrix.
C
      K=N
   20 KP=K
      K=K-1
      IF (K .GT. NACT) THEN
          IF (ZTG(KP) .EQ. 0.0E0) GOTO 20
          TEMP=abs(ZTG(KP))*sqrt(1.0E0+(ZTG(K)/ZTG(KP))**2)
          WCOS=ZTG(K)/TEMP
          WSIN=ZTG(KP)/TEMP
          ZTG(K)=TEMP
          IZ=K
          DO 30 I=1,N
          TEMP=WCOS*Z(IZ+1)-WSIN*Z(IZ)
          Z(IZ)=WCOS*Z(IZ)+WSIN*Z(IZ+1)
          Z(IZ+1)=TEMP
   30     IZ=IZ+N
          GOTO 20
      END IF
C
C     Update the value of ZZNORM.
C
      IF (ZZNORM .LT. 0.0E0) THEN
          ZZNORM=DD/DG
      ELSE
          TEMP=sqrt(ZZNORM*DD/DG)
          ZZNORM=min(ZZNORM,TEMP)
          ZZNORM=max(ZZNORM,0.1E0*TEMP)
      END IF
C
C     Complete the updating of Z.
C
      NP=NACT+1
      TEMP=sqrt(DG)
      IZ=NP
      DO 40 I=1,N
      Z(IZ)=XS(I)/TEMP
   40 IZ=IZ+N
      IF (NP .LT. N) THEN
          KM=NP+1
          DO 80 K=KM,N
          TEMP=0.0E0
          IZ=K
          DO 50 I=1,N
          TEMP=TEMP+GS(I)*Z(IZ)
   50     IZ=IZ+N
          TEMP=TEMP/DG
          SUM=0.0E0
          IZ=K
          DO 60 I=1,N
          Z(IZ)=Z(IZ)-TEMP*XS(I)
          SUM=SUM+Z(IZ)**2
   60     IZ=IZ+N
          IF (SUM .LT. ZZNORM) THEN
              TEMP=sqrt(ZZNORM/SUM)
              IZ=K
              DO 70 I=1,N
              Z(IZ)=TEMP*Z(IZ)
   70         IZ=IZ+N
          END IF
   80     CONTINUE
      END IF
   90 RETURN
      END
c     ==================================================================
      SUBROUTINE SMLC20 (SMLCFG, N, X, F, G, XL, XU, GMODE, NFVALS, W)
c>> 1991-04-15 FTK & CLL  Made FIRST an argument of SMLC20
c>> 1990-09-11 Fred T. Krogh, Jet Propulsion Laboratory, Pasadena, CA.
c
c This subr computes a finite difference approximation to a gradient
c vector.  Values of F are not computed outside the bounds given by XL()
c and XU().  For any I for which XL(I) = XU(I), G(I) will be set to zero
c and computation to estimate G(I) will be skipped.
c On entry F must have already been evaluated at X().
c The algorithm used is based on the following observations.
c 1. Since the gradient vector is being used in an iteration, there is
c    no point to computing it more accurately than is useful in the
c    iteration.  An extra N function values to get better error
c    estimates is not likely to pay for itself.
c 2. If the delta x used for the difference is in the direction of the
c    next move, then discretization error in approximating the gradient
c    is likely to help rather than hurt.
c 3. The sign of the change in X(I) for the next move is likely to be
c    the same as it was on the last move.
c 4. Because of 3 and 4, the increment used is larger than would be
c    suggested from a consideration of round off and discretization
c    errors.
c 5. When the distance between values on successive iterations gets very
c    small it is useful to have second derivative information in order
c    to balance the effects of round off and discretization errors.
c
c *************************** Variables ********************************
c
c D      The difference between the current X(I) and the value on the
c        previous iteration.
c DXN    L2 norm of the move just taken.
c SMLCFG [in, subroutine name]  Name of user-provided subroutine for
c    computing function values.  When called from this subr, SMLCFG must
c    compute F, set HAVEG = .false.,  and not store into G().
c EPSR   Save variable = relative machine precision.
c F      [in, float]  On entry F will contain the function value
c    evaluated at the given X().
c FAC    Save variable = sqrt(relative machine precision)
c FACX   Save variable, 4. * FAC.
c G()    [out, float]  On return G() will contain an approximation of
c    the N-dimensional gradient of f with respect to x at X() computed
c    using one-sided finite differences.
c GMODE [inout, integer] Has value 0, or 1.
c       0 means initial entry.  Compute 1-sided differences, assuming no
c         saved info, and set GMODE to 1.
c       1 Compute differences, using saved info.
c H      Increment in X(I) used for computing gradient.
c HB     Lower bound desired for value of abs(H).
c KOUNT  Counts interations.  If multiple of 10, two sided differences
c    are computed.
c LGL    W(LGL+I) contains the value of G(I) from previous iteration.
c LNOPP  Value of NOPP for start of next iteration.
c LPP    W(LPP+I) contains estimate of the second derivative (d/dv)G(I),
c    where v is the variable consisting of the linear combinations of
c    X's that gave the last move.
c LXL    W(LXL+I) contains X() from the previous iteration.
c MODE1  .true. if have 1-sided difference, = .false. if have 2-sided.
c N      [in, integer]  Number of components in X(), G(), XL(), & XU().
c NFVALS [inout, integer]  Counter of number of calls to SMLCFG.
c NOPP   Logical variable set .true. if second derivative information is
c    not available.
c TP     Temporary storage.
c X()    [in, work, float]  On entry contains the current N-dimensional
c    parameter vector.  This subr will alter components in X() but will
c    reset X() to its original contents on return.
c XI     Current base value for X(I) when computing F for a gradient.
c XL()   [in, float]  Lower bounds for components of X().
c XNEW   Current value for X(I) in computing F for a gradient.  Also
c    used for temporary storage of 1-sided gradient.
c XU()   [in, float]  Upper bounds for components of X().
c W()    [inout, float] Work array of dimension at least 4N+1.
c    W(0) and W(1) are initialized to -1.0 in [D/S]MLC02.  If this
c    subr is never called, i.e., if the user provides computed
c    gradients, then W(1) will remain at this initial value.
c    If this subr is called, it will (generallly) set W(1) positive.
c    [D/S]MLC07 will (generally) set W(0) positive for subsequent
c    testing when and if this subr is called.
c    W(1:N) contains an estimate of the error in G(I) for use in
c    [D/S]MLC07.
c    W(N+1 : 4*N) is used as temporary work space in this subr.  See the
c    usage of the indices LGL, LPP, LXL.
c     ------------------------------------------------------------------
c
c ********************** Variable Declarations *************************
c
      external R1MACH, SMLCFG
      integer GMODE, LGL, LPP, LXL, N
      integer I, KDEBUG, KOUNT, NFVALS
      real             R1MACH
      real             D, DXN, EPSR
      real             F, FAC, FACX, FNEW, G(N), H, HB, TP
      real             X(N), XI, XL(N), XNEW, XU(N), W(0:*)
      logical MODE1, NOPP, LNOPP
c%%     LOGICAL32 haveg;
      logical HAVEG
      save EPSR, FAC, FACX, KOUNT, LNOPP
      save KDEBUG
      data KDEBUG / 0 /
      data FAC / 0.0E0 /, KOUNT / 0 /
 1000    format ('0Gradient #=', I3, '  F=', 1p,e24.17, '  DXN=',
     1      1p,e15.8 / '  I', 11x, 'X', 14X, 'DX', 10x, 'G', 10x, 'ERR',
     2      9X, 'H', 9X, 'dG/dV')
c
c ********************** Start of Executable Code **********************
c
      if(FAC .eq. 0.0E0) then
c                             Get machine constants
         EPSR = R1MACH(4)
         FAC = sqrt(EPSR)
         FACX = 4.E0*FAC
      endif
c                             Set up base locations in W()
      LXL = N
      LGL = LXL + LXL
      LPP = LGL + LXL
      DXN = 0.E0
      if (GMODE .eq. 0) then
         GMODE = 1
         KOUNT = 1
         NOPP = .true.
      else
         KOUNT = KOUNT + 1
         if (mod(KOUNT,10) .eq. 1) then
            if (W(0) .gt. 1.E-5) KOUNT = 0
         else
            if (W(0) .gt. 0.01E0) KOUNT = 0
         end if
         do 10 I = 1, LXL
            DXN = DXN + (X(I) - W(LXL+I))**2
   10    continue
         DXN = sqrt(DXN)
         NOPP = LNOPP
      end if
      LNOPP = .false.
      if (KDEBUG .ne. 0) then
         print 1000, KDEBUG, F, DXN
         KDEBUG = KDEBUG + 1
      end if
      do 40 I = 1, LXL
         XI = X(I)
         D = XI - W(LXL+I)
         if (NOPP) then
            HB = FAC * (1.E0 + abs(XI))
         else
            HB = FAC * (sqrt(abs(F/W(LPP+I))) + FACX*(abs(X(I))+FACX))
         end if
   20    if ((W(LPP+I) .lt. 0.E0) .or. (mod(KOUNT, 10) .eq. 0)) then
            if ((XI + HB .le. XU(I)) .and. (XI - HB .ge. XL(I))) then
               MODE1 = .false.
               H = sign(min(100.E0*HB, min(XU(I)-XI, XI-XL(I))), D)
               go to 30
            end if
         end if
         MODE1 = .true.
         H = sign(max(HB, abs(1.E-4*D)), D)
   30    XNEW = XI + H
c                                 Adjust H to keep XNEW within bounds.
         if (XNEW .gt. XU(I)) then
            XNEW = XU(I)
            if (XNEW - XI .lt. HB) then
               if (XI - XL(I) .gt. XNEW - XI)  XNEW = max(XL(I), XI-HB)
            end if
            H = XNEW - XI
         else if (XNEW .lt. XL(I)) then
            XNEW = XL(I)
            if (XI - XNEW .lt. HB) then
               if (XU(I) - XI .gt. XI - XNEW)  XNEW = min(XU(I), XI+HB)
            end if
            H = XNEW - XI
         end if
         if(H .eq. 0.E0) then
            G(I) = 0.E0
            W(I) = 0.E0
         else
            X(I) = XNEW
C%%           (*smlcfg)( lxl, x, &fnew, g, &haveg );
            CALL SMLCFG(LXL, X, FNEW, G, HAVEG)
            NFVALS = NFVALS+1
            G(I) = (FNEW - F) / H
            W(I) = EPSR * (abs(F) + abs(FNEW)) / abs(H)
            if (.not. MODE1) then
               XNEW = XI - H
               X(I) = XNEW
C%%              (*smlcfg)( lxl, x, &fnew, g, &haveg );
               CALL SMLCFG(LXL, X, FNEW, G, HAVEG)
               NFVALS = NFVALS+1
               XNEW = (FNEW - F) / (XNEW - XI)
               W(I) = W(I) + 5.E-4 * abs(XNEW - G(I))
               G(I) = .5E0 * (G(I) + XNEW )
            end if
            X(I) = XI
         end if
         if (DXN .eq. 0.E0) then
            LNOPP = .true.
            if (MODE1) W(I) = 0.E0
         else
            W(LPP+I) = max(0.E0, abs((G(I) - W(LGL+I)) - 10.E0 *
     1         abs(W(I))) / DXN) + FAC * abs(F)
            TP = 10.E0 * abs(H) * W(LPP+I)
            if (MODE1) then
               if (TP .gt. abs(G(I))) then
                  if (mod(KOUNT, 10) .ne. 0) then
                     KOUNT = 0
                     go to 20
                  end if
               end if
               W(I) = W(I) + TP
            else
               if (abs(XNEW-G(I)) .gt. min(10.E0*TP, 1.E-3 * abs(G(I))))
     1             W(LPP+I) = -W(LPP+I)
            end if
         end if
         if (KDEBUG .ne. 0) then
            print '(I3, 1P,E22.14, E10.2, E12.4, E12.4, E11.3, E10.2)',
     *            I,X(I),X(I)-W(LXL+I),G(I),W(I),H,W(LPP+I)
         end if
         W(LXL+I) = XI
         W(LGL+I) = G(I)
   40 continue
      return
      end
c     ==================================================================
      SUBROUTINE SMLC21(IPMORE, ALL, N, ITERC, NFVALS,
     *           F, TOL, X, G, NACT, IACT, PAR, RESKT, ENRMKT)
c>> 1990-07-11 CLL
c     This subr prints results (first feasible, intermediate, and
c     final).
c     ------------------------------------------------------------------
c                                 Arguments
c
c  IPMORE [in, integer]  = 0 or 1.  0 means print ITERC, NFVALS, F, X(),
c        and G().  1 means also print IACT(), PAR(), and RESKT().
c        However see ALL for an exception.
c  ALL [in, logical]  False means G(), PAR(), and RESKT() might not
c        contain valid data and therefore will not be printed.
c  N, ITERC, NFVALS,
c  F, TOL, X(), G(), NACT, IACT(), PAR(), RESKT(),
c  ENRMKT [in]  Euclidian norm of RESKT().
c     ------------------------------------------------------------------
c%%   long int i, k;
      integer I
      integer IPMORE, N, ITERC, NFVALS, NACT, IACT(*)
      real             ENRMKT, F, X(N), G(N), PAR(*), RESKT(N), TOL
      logical ALL
c     ------------------------------------------------------------------
      print'(/1x,i6,'' iterations. '',i6,'' function evals.    F = '',
     * g13.5/1x,''  TOL ='',g13.5,''   Norm of RESKT ='',g13.5)',
     * ITERC, NFVALS, F, TOL, ENRMKT
C%%     printf( "\n     X =" );
C%%     for (i = 0; i < n; i+=5){
C%%         for (k = i; k < (i < n - 4 ? i+5 : n); k++)
C%%            printf( "%13.5g", x[k] );
C%%         if (i < n - 4) printf ( "\n        " );}
      print'(''     X ='',5g13.5/(8x,5g13.5))', (X(I),I=1,N)
      if (IPMORE .gt. 0) then
         if(ALL) then
C%%         printf( "\n     G =" );
C%%         for (i = 0; i < n; i+=5){
C%%             for (k = i; k < (i < n - 4 ? i+5 : n); k++)
C%%                printf( "%13.5g", g[k] );
C%%             if (i < n - 4) printf ( "\n        " );}
            print'(a,5g13.5/(8x,5g13.5))','     G =',(G(I),I=1,N)
            if (NACT .eq. N) then
               print*,'    Kuhn-Tucker residual vector is zero.'
            else
C%%            printf( "\n RESKT =" );
C%%            for (i = 0; i < n; i+=5){
C%%                for (k = i; k < (i < n - 4 ? i+5 : n); k++)
C%%                   printf( "%13.5g", reskt[k] );
C%%                if (i < n - 4) printf ( "\n        " );}
             print'('' RESKT ='',5g13.5/(8x,5g13.5))', (RESKT(I),I=1,N)
            end if
         end if
         if (NACT .eq. 0) then
            print*,'    No active constraints.'
         else
C%%         printf( "\n  IACT =" );
C%%         for (i = 0; i < nact; i+=12){
C%%             for (k = i; k < (i < nact - 11 ? i+12 : nact); k++)
C%%                printf( "%5ld", iact[k] );
C%%             if (i < nact - 11) printf ( "\n        " );}
            print'(a,12i5/(8x,12i5))','  IACT =',(IACT(I),I=1,NACT)
            if (ALL) then
C%%           printf( "\n   PAR =" );
C%%           for (i = 0; i < nact; i+=5){
C%%               for (k = i; k < (i < nact - 4 ? i+5 : nact); k++)
C%%                  printf( "%13.5g", par[k] );
C%%               if (i < nact - 4) printf ( "\n        " );}
C%%           printf( "\n" );
            print'(''   PAR ='',5g13.5/(8x,5g13.5))', (PAR(I),I=1,NACT)
            end if
         end if
      end if
      return
      end






