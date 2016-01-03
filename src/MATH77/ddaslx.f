      subroutine ddaslx (ddasf, neq, t, y, yprime, tout, info, rtol,
     &                   atol, idid, rwork, lrw, iwork, liw)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2009-10-31 ddaslx Krogh  Insured that lenpd was defined.
c>> 2008-11-13 ddaslx Hanson correct out of date comments about starting
c>> 2008-08-27 ddaslx Hanson correct leading dimension for banded problems
c>> 2008-08-26 ddaslx Hanson add argument of leading dimension to ddasf
c>> 2008-04-18 ddaslx Hanson allowed INFO(11) =0,1,2 for new startup
c>> 2006-05-19 ddaslx Hanson flag (-29 -> -31) & new const errs -29, -30
c>> 2006-04-24 ddaslx Krogh  Added logical variable, nomat.
c>> 2003-07-15 ddaslx Hanson Install Soderlind stepsize code.
c>> 2002-06-26 ddaslx Krogh  Insured iwork(lk) has current value.
c>> 2001-12-29 ddaslx Krogh  Many changes in usage and documentation.
c>> 2001-12-12 ddaslx Krogh  Changed code for reverse communication
c>> 2001-11-23 ddaslx Krogh  Changed many names per library conventions.
c>> 2001-11-04 ddaslx Krogh  Fixes for F77 and conversion to single
c>> 2001-11-01 ddaslx Hanson Provide code to Math a la Carte.
c--D replaces "?":?daslx, ?daswt, ?das1, ?dasnm, ?dastp,
c--& ?dasin, ?daswt, ?dasf, ?dasdb, ?mess
c***BEGIN PROLOGUE  DDASLX
c***PURPOSE  This code solves a system of differential/algebraic
c            equations of the form G(T,Y,YPRIME) = 0.
c***LIBRARY   SLATEC (DDASLX)
c***CATEGORY  I1A2
c***TYPE      DOUBLE PRECISION (SDASSL-S, DDASLX-D)
c***KEYWORDS  BACKWARD DIFFERENTIATION FORMULAS, DASSL,
c             DIFFERENTIAL/ALGEBRAIC, IMPLICIT DIFFERENTIAL SYSTEMS
c***AUTHOR  Petzold, Linda R., (LLNL)
c             Computing and Mathematics Research Division
c             Lawrence Livermore National Laboratory
c             L - 316, P.O. Box 808,
c             Livermore, CA.    94550
c             Changes made by R. J. Hanson, Math a la Carte.
c***DESCRIPTION
c  Subroutine DDASLX uses the backward differentiation formulas of
c  orders one through five to solve a system of the above form for Y and
c  YPRIME.  Values for T and Y at the initial time must be given as
c  input.  The subroutine solves the system from T to TOUT.  It is easy
c  easy to continue the solution to get results at additional TOUT.
c  This is the interval mode of operation.  Intermediate results can
c  also be obtained easily by using the intermediate-output capability.
c  The code automatically selects the order and step size.
c
c *Usage:
c
c      EXTERNAL DDASF
c      INTEGER NEQ, INFO(N), IDID, LRW, LIW, IWORK(LIW),
c      DOUBLE PRECISION T, Y(NEQ), YPRIME(NEQ), TOUT, RTOL, ATOL,
c     *   RWORK(LRW)
c
c      CALL DDASLX (DDASF, NEQ, T, Y, YPRIME, TOUT, INFO, RTOL, ATOL,
c         IDID, RWORK, LRW, IWORK, LIW)
c
c ************************* Arguments **********************************
c
c  DDASF:EXT  -- Depending on how INFO(5) is set, this routine may be
c     used to compute g, compute the iteration matrix, and/or to factor
c     and solve the linear systems as requested.  It has the form
c         SUBROUTINE DDASF(T,Y,YPRIME,DELTA,PD,LDD,CJ,IRES,RWORK,IWORK)
c     The actions to be taken depend on the value of the IRES argument
c     to DDASF.  IRES also provides a way for the user to signal certain
c     things to DDASLX.  The values of IRES are
c   = 0 Initialize subroutine parameters or any storage requirements.
c       If this does not apply, then ignore this value and return
c       without any further action.  Initial conditions for t, y and y'
c       can be assigned to T, Y(:) and YPRIME(:).  Do not alter
c       DELTA, D or C when IRES = 0.
 
c       When reverse communication is used, IRES is in IWORK(3), DELTA
c       starts in RWORK(IWORK(4)), the matrix PD starts in
c       RWORK(IWORK(5)), and CJ is in RWORK(1).
c       In your code you can ignore any values for IRES which can not
c       occur for the value of INFO(5) which is set.
 
c   = 1 Evaluate the residual of the differential/algebraic system.
c       Place values in the array DELTA = g}(t, y, y').
 
c   = 2 This case occurs for |INFO(5)| = 2, 4, 5, 6, 8, and 10.
 
c       Evaluate the partial derivatives (p used for partial
c       symbol), D(i,j) = (pg_i / py_j) + c (pg_i) / py'_j.
c       The value LDD is the row dimension of D(*,*).  The
c       scalar c is an input argument to DDASF.  For the given values of
c       T, Y, YPRIME, the routine must evaluate the non-zero partial
c       derivatives for each equation, and store these values in the
c       matrix D.  For dense or banded matrices, stored in the work
c       space RWORK(:), the elements of D are set to zero before each
c       call to DDASF so only non-zero elements need to be defined.  Do
c       not alter T, Y, YPRIME, or C.  You must dimension D with a first
c       first dimension of LDD.  The way you must store the elements
c       into D depends on the structure of the matrix, indicated by
c       INFO(5) as follows.
 
c       INFO(5)=2, 8, and 12: Full (dense) matrix.  When you evaluate
c            the (non-zero) partial derivative of equation i with
c            respect to variable j, you must store it in D according
c            to D(i,j) = (p g_{i} / py_{j})+c (pg_{i}/py'_{j}).
c       INFO(5)=4, 10, and 14: Banded Jacobian with ML lower and MU
c            upper diagonal bands (refer to INFO(6) description of ML
c            and MU). Give D a first dimension of 2*ML+MU+1.  When you
c             evaluate the (non-zero) partial derivative of equation i
c             with respect to variable j, you must store it in D
c             according to irow=i-j+ ML+MU+1, D(irow,j) = (pg_i /py_j) +
c             c (pg_i / py'_j).
c       INFO(5) = 5 and 6: The array D is an unused dummy argument.
c             Save the partials (pg_i / py_j) + c (p g_{i} / py'_j) in
c             the subroutine DDASF.  This case requires that you factor
c             the matrix (if you ever do) at this time as the value
c             IRES=3 is not provided in this case.  As for that case you
c             should set IRES = 0 to flag the fact that there were no
c             problems in obtaining the factorization.
 
c   = 3 This case occurs for |INFO(5)| > 6.  Factor the matrix of
c       partials.  Prepare to solve systems involving the matrix D =
c       pg_i / py_j) + c (pg_i / py'_j).  The solution method can be a
c       convenient one of the user's  choice.  If the matrix is non-
c       singular it is important to return IRES = 0 as a signal.
c       Otherwise return IRES = 3, if the system is nearly singular.
 
c   = 4 Solve a linear algebraic system using the matrix of partials D,
c       i.e. solve Dw = r for w.  The vector r is input in array
c       DELTA(:).  The solution w overwrites DELTA(:).  If for any
c       reason the system cannot be solved, return w = r as the
c       approximate solution.  This may cause the integrator to take
c       corrective action such as reducing the step-size or the order of
c       the formulas used.  This situation may occur when iteratively
c       solving a linear system, but requiring an excessive number of
c       iterations.
 
c   = 5 Compute the residual and partials for projecting the solution
c       Y(:) onto the constraints after a step has been computed and the
c       corrector equation has converged.  If you are handling the
c       linear algebra (|INFO(5)| .ge. 5) then you should also compute
c       the projection and store it in DELTA(:).  The DDASLX code
c       applies the projection, Y(:) = Y(:) - DELTA(:).  (Note that the
c       flag is given to DDASF if INFO(5) .ge. 0, and else is provided
c       using reverse commiunication.  If for example you are using
c       forward communication for derivatives and doing you own linear
c       algebra using reverse communication, you will need to either
c       deal with the linear algebra in DDASF, or call a routine from
c       there that will do the job.)
 
c       Ordinarily subroutine DDASF should not change the value of IRES.
c       The following values can be set for special cases.
c     0   This must be set if you are factoring the iteration matrix, to
c         let DDASLX know that your matrix is not singular.
c    -1   Some kind of difficulty has been encountered. This causes
c         DDASLX to reduce the stepsize or order which may cure the
c         problem.
c    -2   Return immediately to the main program, for one reason or the
c         other it is time to quit.
c   <-2   This has the same effect as setting INFO(6) to the negative of
c         this number.  It provides a way of turning on debug print at
c         any time.
 
 
c
c  NEQ:IN   This is the number of equations to be solved.  NEQ .ge. 1.
c
c  T:INOUT  This is the current value of the independent variable.
c      Initialize to the value at the initial point.
c
c  Y(.ge. NEQ):INOUT  This array contains the solution components at T.
c      Initialize to the value at the initial point.
c
c  YPRIME(.ge. NEQ):INOUT  This array contains the derivatives of the
c      solution components at T.  You can either provide intial values,
c      or use INFO(11) = 1 to have them computed.
c
c      If =1, use the method given in the origial DDASL code of Petzold.
c      If =2, use the Newton method of Krogh and Hanson, 
c      Solving Constrained Differential-Algebraic Systems Using
c      Projections, 2008.  This is recommended but requires more storage
c      than the option with vaue =1.
c
c  TOUT:IN  This is the first point at which a solution is desired.  The
c      value of TOUT relative to T defines the direction of the
c      integration, so TOUT = T is not allowed at the intial point.
c
c  INFO(.ge. 16):IN  The basic task of the code is to solve the system
c     from T to TOUT and return an answer at TOUT.  INFO is an integer
c     array which is used to communicate exactly how you want this task
c     to be  carried out.  You must provide inputs for the first 16
c     entries. The simplest use of the code is obtained by setting all
c     entries to 0, although you probably want to check INFO(5) at
c     least
c
c   INFO(1) - This parameter enables the code to initialize itself. You
c     must set it to indicate the start of every new problem.
c
c          **** Is this the first call for this problem ...
c                Yes - Set INFO(1) = 0
c                 No - Not applicable here.
c                      See below for continuation calls.  ****
c
c   INFO(2) - How much accuracy you want of your solution is specified
c     by the error tolerances RTOL and ATOL.  The simplest use is to
c     take them both to be scalars.  To obtain more flexibility, they
c     can both be vectors.  The code must be told your choice.
c
c          **** Are both error tolerances RTOL, ATOL scalars ...
c                Yes - Set INFO(2) = 0
c                      and input scalars for both RTOL and ATOL
c                 No - Set INFO(2) = 1
c                      and input arrays for both RTOL and ATOL ****
c
c   INFO(3) - The code integrates from T in the direction of TOUT by
c     steps. If you wish, it will return the computed solution and
c     derivative at the next intermediate step (the intermediate-output
c     mode) or TOUT, whichever comes first. This is a good way to
c     proceed if you want to see the behavior of the solution.  If you
c     must have solutions at a great many specific TOUT points, this
c     code will compute them efficiently.  You can change this mode
c     at any time.
c
c          **** Do you want the solution only at
c                TOUT (and not at the next intermediate step) ...
c                 Yes - Set INFO(3) = 0
c                  No - Set INFO(3) = 1 ****
c
c   INFO(4) - To handle solutions at a great many specific values TOUT
c     efficiently, this code may integrate past TOUT and interpolate to
c     obtain the result at TOUT.  Sometimes it is not possible to
c     integrate beyond some point TSTOP because the equation changes
c     there or it is not defined past TSTOP. Then you must tell the code
c     not to go past.  The code will never give values beyond TSTOP, but
c     you can change this mode at any time.  However when you set
c     INFO(4) to 1, you must always set RWORK(1) to a value past the
c     current time.
c
c          **** Can the integration be carried out without any
c               restrictions on the independent variable T ...
c                Yes - Set INFO(4)=0
c                 No - Set INFO(4)=1
c                      and define the stopping point TSTOP by
c                      setting RWORK(1)=TSTOP ****
c
c   INFO(5) - To solve differential/algebraic problems it is necessary
c     to use a matrix of partial derivatives for the system of
c     differential equations.  This flag must be set to define your
c     mtarix.
c  = 1 (or 0) Matrix is full and dense and numerical derivatives are
c     generated internally.  Although it is less trouble for you to have
c     the code compute derivatives by numerical differencing, the
c     solution will be more reliable if you provide the derivatives
c     using formulas.
c  = 2  Matrix is full and dense and the user is computing derivatives.
c  = 3  As for 1, but the matrix, (p stands for the partial operator)
c    D = pg/py+ c (pg}/py', is banded. Here c is a factor determined
c     by DDASLX. The differential equation is said to have
c     half-bandwidths ML (lower) and MU (upper) if D involves only some
c     of the unknowns y_{J} with i - ML .le. J .le. i + MU for all
c     i=1,...,NEQ.  Thus, ML and MU are the widths of the lower and
c     upper parts of the band, with the main diagonal being excluded.
c     If the matrix is stored in the RWORK(:) array and you do not
c     indicate that the equation has a banded matrix of partial
c     derivatives, the code works with a full array of NEQ**2 elements,
c     stored in the conventional Fortran array style.  Computations with
c     banded matrices typically require less time and storage than with
c     full matrices, if 2 * ML + MU < NEQ.  If you tell the code that
c     the matrix of partial derivatives has a banded structure and you
c     want to provide subroutine DDASF to compute the partial
c     derivatives, then you must store the elements of the matrix in the
c     Linpack band-matrix specification, indicated in the description of
c     DDASF.
c     Provide the lower ML and upper MU bandwidths by setting
c     IWORK(1) = ML and IWORK(2) = MU.
c  = 4 As for 3, but derivatives are to be computed by the user.
c  = 5 The user is going to take care of all matters connected with the
c      factorization and solving of the systems.
c  = 6 As for 5, except the linear algebra is to be accomplished using
c      reverse communication.
c  = 7 - 10 As for 1 - 4, except user is doing linear algebra in DDASF.
c  = 11 - 14 As for 7 - 10, except using reverse communication.
c  = k<0 Same as -k, except the evaluation of g and all partials is done
c      using reverse communciation rather than by calling DDASF.
c
c   INFO(6) -  Set the level of debugging print.  A value of 0 gives no
c     print. Otherwise this is a 7 digit number d_6d_5d_4d_3d_2d_1d_0
c     defining what to print as follows.
c       d_0 Print on entry to DDASLX
c        = 0  No print
c        = 1  IDID, INFO(1), NEQ, T, TOUT
c        = 2  y, y'
c        = 3  Tolerances
c        = 4  INFO (all of it)
c       d_1 Print on exit from DDASLX. Print is as for d_0
c       d_2 After a call to DDASF (or return from reverse communication
c           that would ordinarily call DDASF.
c        = 0  No print
c        = 1  Print whatever was just computed, except no matrices.
c        = 2  Print whatever was just computed, including matrices.
c       d_3 As for the case above, except print is for what is in the
c           locations about to be stored to.
c       d_4 Internal print inside subroutine DDASTP.
c        = 0  No print
c        = 1 y, y' and corrections.
c        = 2 tolerances
c        = 3 difference tables
c        = 4 integration coefficients
c       d_5 Determines how much of WORK and IWORK are printed, when
c           there is other print.
c        = 0  No print
c        = 1  Always print IWORK(1:16)
c        = 2  Always print WORK(1:9)
c        = 3  Always print both of the above.
c       d_6 For turning off, or limiting the amount of print.
c        = 0  No effect
c        = 1  No effect, but gives a way to specify a value of 0, 1 or 2
c             when passing a negative value of IRES after starting.
c        > 1  Print data for just this many of the first variables, and
c             just this many of the first rows in matrices when
c             variables or matrices are printed.
c
c   INFO(7) - You can specify a maximum (absolute value of) stepsize, so
c     that the code will avoid passing over very large regions.
c
c      ****  Do you want the code to decide on its own maximum stepsize?
c              Yes - Set INFO(7)=0
c               No - Set INFO(7)=1
c                    and define HMAX by setting
c                      RWORK(2)=HMAX ****
c
c   INFO(8) - Differential/algebraic problems may occasionally suffer
c     from severe scaling difficulties on the first step. If you know a
c     great deal about the scaling of your problem, you can help to
c     alleviate this problem by specifying an initial stepsize HO.
c
c      ****  Do you want the code to define its own initial stepsize?
c            Yes - Set INFO(8)=0
c             No - Set INFO(8)=1
c                  and define HO by setting
c                  RWORK(3)=HO ****
c
c   INFO(9) - If storage is a severe problem, you can save some
c     locations by restricting the maximum order MAXORD.  the default
c     value is 5. for each order decrease below 5, the code requires NEQ
c     fewer locations, however it is likely to be slower. In any case,
c     you must have 1 .LE. MAXORD .LE. 5
c
c       ****  Do you want the maximum order to default to 5?
c             Yes - Set INFO(9)=0
c              No - Set INFO(9)=MAXORD .le. 5
c
c   INFO(10) - If you know that the solutions to your equations satisfy
c     some constraints you may want to use this option.  It is expected
c     to be especially useful when one differentiates to reduce the
c     index of a problem.  This option must not be used when INFO(5) =
c     3 or  4.
c
c       ****  Do you want the code to solve the problem without
c             invoking constraints?
c             Yes - Set INFO(10)=0
c             No - Set INFO(10)=k, where k is >0 if one wants
c                  to solve for k constraints using forward
c                  communication, and k < 0 to solve for -k
c                  constraints using reverse communication.
c                  Artificially require |k| .le. 1000.
c
c   INFO(11) - DDASLX normally requires the initial T, Y, and YPRIME to
c     be consistent. That is, you must have G(T,Y,YPRIME) = 0 at the
c     initial time. If you do not know the initial derivative precisely,
c     you can let DDASLX try to compute it.
c
c       ****   Are the initial T, Y, YPRIME consistent?
c              Yes - Set INFO(11) = 0
c              No - Set INFO(11) = 1, and set YPRIME to an initial
c                   approximation to YPRIME.  (If you have no idea what
c                   YPRIME should be, set it to zero. Note that the
c                   initial Y should be such that there must exist a
c                   YPRIME so that G(T,Y,YPRIME) = 0.)
c              No - Set INFO(11) = 2, and set YPRIME to an initial
c                   approximation to YPRIME.  This option uses the 
c                   Krogh/Hanson starting algorithm found in the report
c                   Solving Constrained Differential-Algebraic Systems
c                   Using Projections, 2008.  This is the preferred
c                   option if the code solves for YPRIME.
c
c   INFO(12) - DDASLX} normally allows up to 500 internal steps between
c      each output points.
c          Set INFO(12)=0 for the code to use up to 500 internal steps
c             between output points.
c          Set INFO(12)=k} and the code will use up to k internal steps
c             between output points.
c   INFO(13) - DDASLX} normally uses the smoothed step control algorithm
c      developed by Gustaf Soderlind.
c          Set INFO(13)=0 for the code to use the step control method
c             of Soderlind.
c          Set INFO(13)=1 and the code will use the original step
c             control logic of DASSL.
c
c   INFO(14:16) -- Not used currently by the code, but must be set to 0.
 
c
c  RTOL,ATOL:INOUT  These quantities represent relative and absolute
c     error tolerances which you provide to indicate how accurately you
c     wish the solution to be computed.  You may choose them to be both
c     scalars (INFO(2)=0) or else both vectors (INFO(2)=1).  Caution:
c     In Fortran 77, a scalar is not the same as an array of length 1.
c     Some compilers may require to make the scalars into arrays of
c     length 1.  All components must be non-negative.
c     The tolerances are used by the code in a local error test at each
c     step which requires roughly that
c           ABS(LOCAL ERROR) .LE. RTOL*ABS(Y)+ATOL
c     for each vector component.  (More specifically, a root-mean-square
c     norm is used to measure the size of vectors, and the error test
c     uses the magnitude of the solution at the beginning of the step.)
c
c     The true (global) error is the difference between the true
c     solution of the initial value problem and the computed
c     approximation.  Practically all present day codes, including this
c     one, control the local error at each step and do not even attempt
c     to control the global error directly.  Usually, but not always,
c     the true accuracy of the computed Y is comparable to the error
c     tolerances. This code will usually, but not always, deliver a more
c     accurate solution if you reduce the tolerances and integrate
c     again.  By comparing two such solutions you can get a fairly
c     reliable idea of the true error in the solution at the bigger
c     tolerances.
c
c     Setting ATOL=0. results in a pure relative error test on that
c     component.  Setting RTOL=0. results in a pure absolute error test
c     on that component.  A mixed test with non-zero RTOL and ATOL
c     corresponds roughly to a relative error test when the solution
c     component is much bigger than ATOL and to an absolute error test
c     when the solution component is smaller than the threshold ATOL.
c
c     The code will not attempt to compute a solution at an accuracy
c     unreasonable for the machine being used.  It will advise you if
c     you ask for too much accuracy and inform you as to the maximum
c     accuracy it believes possible.
c
c  IDID:OUT    This scalar quantity is an indicator reporting what the
c     code did.  You must monitor this integer variable to decide what
c     action to take next.  Values returned are as follows.
c                  Task Completed or Ongoing
c      1  A step was successfully taken in the intermediate-output
c         mode. The code has not yet reached TOUT.
c      2  The integration to TSTOP was successfully completed to
c         T = TSTOP by stepping exactly to TSTOP.
c      3  The integration to TOUT was successfully completed T = TOUT by
c         stepping past TOUT.  Y(:)} is obtained by interpolation.
c         YPRIME(:) is obtained by interpolation.
c      4  The integration has paused for reverse communication.  Respond
c         based on the values of IWORK(3).
c                  Task Interupted
c     -1  IRES set to -2 by the user.
c     -2  Accuracy requested exceeds machine precision.  RTOL and ATOL
c         have been increased.
c     -3  There have been too many steps between output points.
c                  Quit or Restart Integration
c     -4  No convergence due to IRES being set to -1.
c     -5  A weight for computing error norms is .le. 0.
c     -6  The error test has failed repeatedly.
c     -7  Repeated failure of the corrector to converge.
c     -8  The iteration matrix is singular.
c     -9  Repeated corrector convergence failures, with singular
c         matrices flagged.
c     -10  Could not solve for the initial y'.
c                  Invalid input
c     -11  An INFO entry has a value not allowed for that option.
c     -12  The number of equations was set .le. 0.
c     -13  The maximum order does not have a value from 1 to 5.
c     -14  The size of RWORK is too small.
c     -15  The size of IWORK is too small.
c     -16  An entry of RTOL is < 0.
c     -17  An entry of ATOL is < 0.
c     -18  All entries of RTOL and ATOL are 0.
c     -19  The value of TOUT is > TSTOP.
c     -20  The maximum stepsize is set .le. 0.
c     -21  The current TOUT is behind T.
c     -22  The initial stepsize has been set to 0.
c     -23  TOUT is too close to the starting T.
c     -24  TSTOP is not consistent with the current T.
c     -25  An illegal bandwidth.
c     -26  The current T and TOUT are equal.
c     -27  Constraints used with band matrices.
c     -28  When solving constraints for a user defined matrix, IRES was
c          not changed from 5.
c     -29  Inconsistency in the constraints when starting.
c     -30  During later integration steps, an inconsistency and rank
c          deficiency were noted. Print amount constraint residual norm
c          and linear system residual norm.
c     -31  No appropriate action was taken after IDID was set < 0.
 
c
c  RWORK:WORK  A real work array of length LRW which provides the
c     code with needed storage space. The data in RWORK is necessary
c     for subsequent calls.  You may find the following locations of
c     interest.
c     RWORK(3)  The step size H to be attempted on the next step.
c     RWORK(4)  The current value of the independent variable, i.e.,
c               the farthest point integration has reached. This will
c               be different from T only when interpolation has been
c               performed (IDID=3).
c     RWORK(7)  The stepsize used on the last successful step.
c
c  LRW:IN  The length of RWORK.  Set it to the declared length of the
c           RWORK array.  You must have LRW .GE. 45+(MAXORD+4)*NEQ
c      + NEQ**2 for the full (dense) JACOBIAN case
c          (INFO(5) =0,1,2,7,8,11,12), or
c      + (2*ML+MU+1)*NEQ for the banded  user-defined JACOBIAN case
c          (INFO(5)=3, 9, 13), or
c      + (2*ML+MU+1)*NEQ+2*(NEQ/(ML+MU+1)+1) for the banded finite-
c           difference-generated JACOBIAN (INFO(5)=4, 10, 14),
c      + 0  For the user defined matrix case.
c
c  IWORK:WORK  An integer work array of length LIW which provides the
c     code with needed storage space.  As for RWORK, you may find the
c     following locations of interest.
c     IWORK(4)  The order of the method to be tried on the next step.
c     IWORK(5)  The order of the method used on the last step.
c     IWORK(8) The number of steps taken so far.
c     IWORK(9) The number of calls to DDASF so far.
c     IWORK(10) The number of evaluations of the matrix of partial
c               derivatives needed so far.
c     IWORK(11) The total number of error test failures so far.
c     IWORK(12) Which contains the total number of convergence test
c               failures so far. (Includes singular iteration matrix
c               failures.)
cc
c  LIW:IN  The length of IWORK.  Set it to the declared length of the
c          IWORK array.  You must have LIW .GE. 30+NEQ
c
c  OPTIONALLY REPLACEABLE NORM ROUTINE:
c
c     DDASLX uses a weighted norm DDASNM to measure the size of vectors
c     such as the estimated error in each step.  A FUNCTION subprogram
c       DOUBLE PRECISION FUNCTION DDASNM(NEQ,V,WT,RWORK,IWORK)
c       DIMENSION V(NEQ),WT(NEQ)
 
c     is used to define this norm. Here, V is the vector whose norm is
c     to be computed, and WT is a vector of weights.  A DDASNM routine
c     has been included with DDASLX which computes the weighted
c     root-mean-square norm given by DDASNM = SQRT((1/NEQ) *
c     SUM(V(I)/WT(I))**2) this norm is suitable for most problems. In
c     some special cases, it may be more convenient and/or efficient to
c     define your own norm by writing a function subprogram to be called
c     instead of DDASNM. This should, however, be attempted only after
c     careful consideration.
c
c ********************* Local Variables *******************************
c
c     NOTE -- Parameter names are given followed by (ii name) where ii
c     is the integer value of the parameter, and name is the array the
c     parameter is used to reference.
c
c icnstr (10 info) Number of constraints.
c idb (6 info) Digits define rules for debugging. From right to left
C     the digits define output for: entry to ddaslx, exit from ddaslx,
C     after actions in ddasf, before actions id ddasf, actions inside
C     ddastp, and what to do about printing iwork and rwork.  The bigger
C     the  digit, the more the print; see above for details.
c ih0 (8 info) If set, the initial stepsize is stored in rwork(lh=3).
c imat (5 info) Defines how the Jacobian is computed and processed.
c     If < 0, then computation of g and of all partials is done using
c     reverse communication.  Absolute value is used as follows:
c   1.Full matrix, derivatives computed using finite differences.
c   2 Full matrix, user computes derivatives.
c   3 Band matrix, ML=iwork(lml=1), MU=iwork(lmu=2) define the left
c     and right bandwidths. Derivative approximated with differences.
c   4 As for 3, but with the derivatives computed with user supplied
c     derivatives.
c   5 User defined matrix.  User does all of the work!
c   6 As for 5, but the work is done using reverse communication.
c  7-10 As for 1 - 4, but user does linear algebra themselves inside
c       ddasf.
c 11-14 As for 7-10, but reverse communication is used.
c imaxh (7 info) If Set, the maximum stepsize has been set by the
c     user in rwork(lhmax=2)
c inityp (11 info) If set computations must be done to get
c     consistent initial value for y'.
c iord (9 info) If set, gives the maximum integration order to use.
c iout (3 info) If set, gives output at the end of every step.
c istop (4 info) If set, then the user has set rwork(tstop=1) to a
c     value that the integrator must not go past.
c itol (2 info) If set, then atol() and rtol() are arrays, else they
c     are scalars.
c ixstep (12 info) If set, then iwork(mxstep=21) has been set to
c info(istop=4) giving the maximum number of steps between outputs.
c lalpha (11 rwork) Start of the alphas used in computing integ. coeff.
c lbeta (17 rwork) Start of the betas used in computing integ. coeff.
c lcj (1 rwork) The constant which multiplies the partials with
c     respect to y' in the iteration matrix.
c lcjold (6 rwork) The previous value of rwork(lcj=5).
c lcnstr (10 iwork) The number of constraints.
c lctf (17 iwork) Counts the total number of various convergence
c     failures.
c ldelt (4, iwwork) Always equal to ldelta below.
c ldelta (46 rwork) Place where residuals and corrections to y are
c     stored.
c le (23 iwork) rwork(iwork(le=22) is where error estimates are stored.
c letf (16 iwork) Contains the number of error test failures so far.
c lgamma (23 rwork) Start of the gammas used in computing integ. coeff.
c lh (4 rwork) Stepsize to be used on the next step.
c lhmax (3 rwork) The largest stepsize permitted.
c lhmin (10 rwork) The minimum stepsize permitted.
c lhold (7 rwork) The previous stepsize.
c lipvt (31 iwork) Used for permutations when solving linear systems.
c lires (3 iwork) Value of IRES for reverse communication.
c ljcalc (19 iwork) This is set to -1 when a new Jacobian is needed,
c     set to 0 when it is computed, and set to 1 when it has been used.
c lk (7 iwork) Contains the current integration order.
c lkold (8 iwork) Contains the previous integration order.
c lmat (9 iwork) Type of matrix, from info(imat).
c lml (1 iwork) Contains the left bandwith (if used).
c lmu (2 iwork) Contains the right bandwidth (if used).
c lmxord (6 iwork) Contains the maximum integration order allowed.
c lnjac (8 rwork) A large number here means we need a new Jacobian.
c     Small number means that convergence is good.
c lnje (15 iwork) Counts the number of Jacobian evaluations so far.
c lnpd (18 iwork) This gives the space required to hold partial
c     derivatives. Includes partials for constraints.
c lnre (14 iwork) Counts the number of evaluations of g.
c lns (11 iwork) Counts the number of steps without a stepsize change.
c lnst (13 iwork) Counts the number of steps taken so far.
c lnstl (12 iwork) Number of steps when last returned to user (not
c     counting computations done in ddasf when not using reverse comm.)
c lphase (20 iwork) This is set to 0 initially allowing rapid order
c     and stepsize increases.  It is set to 1 if either the maximum
c     order is reached, or the order is decreased or there has been some
c     kind of failure.
c lphi (25 iwork) Location of difference tables in rwork.
c lpsi (29 rwork) Start of the psi's used in computing integ. coeff.
c lround (9 rwork) The machine epsilon.
c lsigma (35 rwork) Start of the sigmas used in error estimation.
c ltn (5 rwork) The point up to which we have integrated.  Local
c     variable tn in ddaslx contains this value.
c ltstop (2 rwork) Value that the integrator is not to go past.
c lwm (5 iwork) rwork(iwork(lwm=25)) is the start of matrix data.
c lwt (24 iwork) rwork(iwork(lwt=23)) is the start of weights used
c     in computing error norms.
c mxstep (22 iwork) Contains the maximum number of steps to take
c     between output points.
c nomat  Logical variable set = .true. if matrix storage not used here.
c ntemp (26 iwork) Location relative to work(iwork(lwm=25)) of
c     extra matrix data required to compute finite difference Jacobians.
c revloc (21 iwork) This is nonzero if reverse communication is in
c     the process of being used.  The low order 3 bits define the
c     location to go to in the current routine, the next three for the
c     routine that this one calls, and the next 3 bits for the routine
c     called by that one, etc.
c
c  ---------------------------------------------------------------------
c
c***REFERENCES  A DESCRIPTION OF DASSL: A DIFFERENTIAL/ALGEBRAIC
c                 SYSTEM SOLVER, L. R. PETZOLD, SAND82-8637,
c                 SANDIA NATIONAL LABORATORIES, SEPTEMBER 1982.
c***ROUTINES CALLED  D1MACH, DDAS1, DDASNM, DDASTP, DDASIN, DDASWT,
c                    XERMSG
c***REVISION HISTORY  (YYMMDD)
c   830315  DATE WRITTEN
c   880387  Code changes made.  All common statements have been
c           replaced by a DATA statement, which defines pointers into
c           RWORK, and PARAMETER statements which define pointers
c           into IWORK.  As well the documentation has gone through
c           grammatical changes.
c   881005  The prologue has been changed to mixed case.
c           The subordinate routines had revision dates changed to
c           this date, although the documentation for these routines
c           is all upper case.  No code changes.
c   890511  Code changes made.  The DATA statement in the declaration
c           section of DDASLX was replaced with a PARAMETER
c           statement.  Also the statement S = 100.D0 was removed
c           from the top of the Newton iteration in DDASTP.
c           The subordinate routines had revision dates changed to
c           this date.
c   890517  The revision date syntax was replaced with the revision
c           history syntax.  Also the "DECK" comment was added to
c           the top of all subroutines.  These changes are consistent
c           with new SLATEC guidelines.
c           The subordinate routines had revision dates changed to
c           this date.  No code changes.
c   891013  Code changes made.
c           Removed all occurrences of FLOAT or DBLE.  All operations
c           are now performed with "mixed-mode" arithmetic.
c           Also, specific function names were replaced with generic
c           function names to be consistent with new SLATEC guidelines.
c           In particular:
c              Replaced DSQRT with SQRT everywhere.
c              Replaced DABS with ABS everywhere.
c              Replaced DMIN1 with MIN everywhere.
c              Replaced MIN0 with MIN everywhere.
c              Replaced DMAX1 with MAX everywhere.
c              Replaced MAX0 with MAX everywhere.
c              Replaced DSIGN with SIGN everywhere.
c           Also replaced REVISION DATE with REVISION HISTORY in all
c           subordinate routines.
c   901004  Miscellaneous changes to prologue to complete conversion
c           to SLATEC 4.0 format.  No code changes.  (F.N.Fritsch)
c   901009  Corrected GAMS classification code and converted subsidiary
c           routines to 4.0 format.  No code changes.  (F.N.Fritsch)
c   901010  Converted XERRWV calls to XERMSG calls.  (R.Clemens, AFWL)
c   901019  Code changes made.
c           Merged SLATEC 4.0 changes with previous changes made
c           by C. Ulrich.  Below is a history of the changes made by
c           C. Ulrich. (Changes in subsidiary routines are implied
c           by this history)
c           891228  Bug was found and repaired inside the DDASLX
c                   and DDAS1 routines.  DDAS1 was incorrectly
c                   returning the initial T with Y and YPRIME
c                   computed at T+H.  The routine now returns T+H
c                   rather than the initial T.
c                   Cosmetic changes made to DDASTP.
c           900904  Three modifications were made to fix a bug (inside
c                   DDASLX) re interpolation for continuation calls and
c                   cases where TN is very close to TSTOP:
c
c                   1) In testing for whether H is too large, just
c                      compare H to (TSTOP - TN), rather than
c                      (TSTOP - TN) * (1-4*UROUND), and set H to
c                      TSTOP - TN.  This will force DDASTP to step
c                      exactly to TSTOP under certain situations
c                      (i.e. when H returned from DDASTP would otherwise
c                      take TN beyond TSTOP).
c
c                   2) Inside the DDASTP loop, interpolate exactly to
c                      TSTOP if TN is very close to TSTOP (rather than
c                      interpolating to within roundoff of TSTOP).
c
c                   3) Modified IDID description for IDID = 2 to say
c                      that the solution is returned by stepping exactly
c                      to TSTOP, rather than TOUT.  (In some cases the
c                      solution is actually obtained by extrapolating
c                      over a distance near unit roundoff to TSTOP,
c                      but this small distance is deemed acceptable in
c                      these circumstances.)
c   901026  Added explicit declarations for all variables and minor
c           cosmetic changes to prologue, removed unreferenced labels,
c           and improved XERMSG calls.  (FNF)
c   901030  Added ERROR MESSAGES section and reworked other sections to
c           be of more uniform format.  (FNF)
c   910624  Fixed minor bug related to HMAX (six lines after label
c           525).  (LRP)
c   981110  Major revision on several codes in the package.  Now
c           use one external user routine.  Also support reverse
c           communication.  Install JPL error processor in place of
c           XERMSG.
 
c***END PROLOGUE  DDASLX
c
c**End
c
c     Declare arguments.
c
      integer    neq, ldd, info(16), idid, lrw, iwork(*), liw
      double precision t, y(*), yprime(*), tout, rtol(*), atol(*),
     &           rwork(lrw)
      external   ddasf
c
c     Declare externals.
c
      external   d1mach, ddas1, ddasnm, ddastp, ddasin, ddaswt, dmess
      double precision d1mach, ddasnm
c
c     Declare local variables.
c
      integer    i, infobd(2:16),  itemp, leniw,lenpd, lenrw,
     &     mband, msave, nzflg, ires
      double precision atoli, h, hmax, ho, r, rh, rtoli, tdist,
     &     tn, tnext, tstop, ypnorm
      INTEGER ISMOOT, IFIRST
      logical    done, nomat
c
c The following locations are reserved for future expansion:
c    iwork(26:30), work(41:45),; and info(13:16)
c
c     POINTERS INTO IWORK
      integer  lml, lmu, lires, ldelt, lwm, lmxord, lk, lkold, lmat,
     &     lcnstr, lns, lnstl, lnst, lnre, lnje, letf, lctf, lnpd,
     &     ljcalc, lphase, revloc, mxstep, le, lwt, lphi, ntemp, lipvt
      parameter (lml=1, lmu=2, lires=3, ldelt=4, lwm=5, lmxord=6, lk=7,
     &     lkold=8, lmat=9, lcnstr=10, lns=11, lnstl=12, lnst=13,
     &     lnre=14, lnje=15, letf=16, lctf=17, lnpd=18, ljcalc=19,
     &     lphase=20, revloc=21, mxstep=22, le=23, lwt=24, lphi=25,
     &     ntemp=26, lipvt=31)
c
c     POINTERS INTO RWORK
      integer  lcj, ltstop, lhmax, lh, ltn, lcjold, lhold, lnjac,
     &     lround, lhmin, lalpha, lbeta, lgamma, lpsi, lsigma, ldelta
      parameter  (lcj=1, ltstop=2, lhmax=3, lh=4, ltn=5, lcjold=6,
     &     lhold=7, lnjac=8, lround=9, lhmin=10, lalpha=11, lbeta=17,
     &     lgamma=23, lpsi=29, lsigma=35, ldelta=46)
c
c     POINTERS INTO INFO
      integer  itol, iout, istop, imat, idb, imaxh, ih0, iord, icnstr,
     &     inityp, ixstep
      parameter  (itol=2, iout=3,  istop=4, imat=5, idb=6, imaxh=7,
     &     ih0=8, iord=9, icnstr=10, inityp=11, ixstep= 12)
      parameter   (ISMOOT=13, IFIRST=16)
c
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DDASLX$B
cAB Evaluation terminated integration by returning IRES=-2.$N
c   Current T = $F, Stepsize H = $F.$E
cAC Too much accuracy requested for machine precision.$N
c   RTOL(:) and ATOL(:) replaced by appropriate values.$N
c   Current T = $F. Last and next step BDF order =$I,$I.$E
cAD Did not reach TOUT in MXSTEPS steps.$N
c   Current T = $F, TOUT = $1$F, MXSTEPS=$2$I.$E
cAE Corrector could not converge because evaluation returned $C
c   IRES = -1.$N
c   Current T = $F, Stepsize H = $F.$E
cAF After starting, an entry of WT(:) has become <= 0.0.$N
c   Current T = $F.$E
cAG The error test failed repeatedly.$N
c   Current T = $F, Stepsize H = $F. $N
c   Last and next step BDF order =$I,$I.$E
cAH The corrector failed to converge repeatedly.$N
c   Current T = $F, Stepsize H = $F. $N
c   Last and next step BDF order =$I,$I.$E
cAI The iteration matrix is singular.$N
c   Current T = $F, Stepsize H = $F.$E
cAJ Repeated corrector convergence failures with singular matrices $C
c   flagged.$N
c   Current T = $F, $C
c   Last and next step BDF order =$I,$I.$E
cAK Could not initially solve G(t,y,y')=0 for y'.  Consider using $C
c   larger tolerances.$N
c   Current T = $F, Stepsize H = $F. Last and next $C
c   step BDF order =$I,$I.$E
cAL Setting INFO($4$I) = $I is not an allowed option.$E
cAM Number of equations, NEQ, is <= 0.  NEQ = $12$I.$E
cAN MAXORD, maximum order of BDF used, must be >= 1 and <= 5. $C
c   MAXORD = $3$I.$E
cAO Size of RWORK(:) is too small.$N
c   Now have LRW = $6$I, but need LRW = $I.$E
cAP Size of IWORK(:) is too small.$N
c   Now have LIW = $8$I, but need LIW = $I.$E
cAQ RTOL($4$I) is < 0.$E
cAR ATOL($4$I) is < 0.$E
cAS All entries of RTOL(:) and ATOL(:) = 0.0$E
cAT Your TOUT of $2$F is > than your TSTOP of $1$F whidh is $C
c   not allowed$E
cAU Value of maximum stepsize HMAX = $3$F is <= 0.0.$E
cAV The current T = $F with H=$F is preceded by TOUT = $F.$E
cAW Value of initial stepsize H0 is = 0.$E
cAX With a starting T = $F, the TOUT = $1$F is too close.$E
cAY With the current T=$F and H=$F, the TSTOP=$2$F is inconsistent.$E
cAZ Lower bandwith is $10$I, upper bandwidth is $I and both should $C
c   be .ge. 0, and < neq which is $I.$E
cBA The current T = $F and  TOUT = $1$F are equal.$E
cBB Code does not handle constraints when using a band matrix.$E
cBC Flag not set for computing constraint correction as it must be $C
c   with a user defined matrix and constraints.$E
cBD At the initial point constraints were not consistent.$E
cBE At T = $F, with H = $F, the constraints appear inconsistent.  $C
c   The norm of the residual with maximal perturbation of the $C
c   solution is $F and we did not reduce the norm below $F.$E
cBF Last step terminated  with IDID < 0.  No appropriate action was $C
c   taken. IDID = $I.  Repeated occurrences of illegal input. $C
c   Run terminated. Apparent infinite loop.$E
c   $ D
cBG Adjustments to Y in order to get back on the constraints were:$N$B
c   $
cBH Residuals in the constraints were:$N$B
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM,LTXTAN,LTXTAO,LTXTAP,LTXTAQ,
     * LTXTAR,LTXTAS,LTXTAT,LTXTAU,LTXTAV,LTXTAW,LTXTAX,LTXTAY,LTXTAZ,
     * LTXTBA,LTXTBB,LTXTBC,LTXTBD,LTXTBE,LTXTBF,LTXTBG,LTXTBH
      parameter (LTXTAA=   1,LTXTAB=   9,LTXTAC= 100,LTXTAD= 259,
     *LTXTAE= 341,LTXTAF= 444,LTXTAG= 515,LTXTAH= 624,LTXTAI= 744,
     *LTXTAJ= 813,LTXTAK= 941,LTXTAL=1095,LTXTAM=1146,LTXTAN=1196,
     *LTXTAO=1270,LTXTAP=1344,LTXTAQ=1417,LTXTAR=1437,LTXTAS=1457,
     *LTXTAT=1499,LTXTAU=1568,LTXTAV=1618,LTXTAW=1674,LTXTAX=1712,
     *LTXTAY=1767,LTXTAZ=1832,LTXTBA=1930,LTXTBB=1978,LTXTBC=2038,
     *LTXTBD=2146,LTXTBE=2201,LTXTBF=2378,LTXTBG=   1,LTXTBH=   1)
      character MTXTAA(11) * (231)
      character MTXTAB(1) * (66)
      character MTXTAC(1) * (38)
C++ Save data by elements if ~.C.
      data MTXTAA(1)/'DDASLX$BEvaluation terminated integration by retur
     *ning IRES=-2.$NCurrent T = $F, Stepsize H = $F.$EToo much accuracy
     * requested for machine precision.$NRTOL(:) and ATOL(:) replaced by
     * appropriate values.$NCurrent T = $F. Last and ne'/
      data MTXTAA(2)/'xt step BDF order =$I,$I.$EDid not reach TOUT in M
     *XSTEPS steps.$NCurrent T = $F, TOUT = $1$F, MXSTEPS=$2$I.$ECorrect
     *or could not converge because evaluation returned IRES = -1.$NCurr
     *ent T = $F, Stepsize H = $F.$EAfter starting, an '/
      data MTXTAA(3)/'entry of WT(:) has become <= 0.0.$NCurrent T = $F.
     *$EThe error test failed repeatedly.$NCurrent T = $F, Stepsize H =$
     * $F. $NLast and next step BDF order =$I,$I.$EThe corrector failed$
     * to converge repeatedly.$NCurrent T = $F, Stepsiz'/
      data MTXTAA(4)/'e H = $F. $NLast and next step BDF order =$I,$I.$E
     *The iteration matrix is singular.$NCurrent T = $F, Stepsize H = $F
     *.$ERepeated corrector convergence failures with singular matrices$
     * flagged.$NCurrent T = $F, Last and next step BDF'/
      data MTXTAA(5)/' order =$I,$I.$ECould not initially solve G(t,y,y'
     *')=0 for y''.  Consider using larger tolerances.$NCurrent T = $F,$
     * Stepsize H = $F. Last and next step BDF order =$I,$I.$ESetting IN
     *FO($4$I) = $I is not an allowed option.$ENumber of '/
      data MTXTAA(6)/'equations, NEQ, is <= 0.  NEQ = $12$I.$EMAXORD, ma
     *ximum order of BDF used, must be >= 1 and <= 5. MAXORD = $3$I.$ESi
     *ze of RWORK(:) is too small.$NNow have LRW = $6$I, but need LRW =$
     * $I.$ESize of IWORK(:) is too small.$NNow have LI'/
      data MTXTAA(7)/'W = $8$I, but need LIW = $I.$ERTOL($4$I) is < 0.$E
     *ATOL($4$I) is < 0.$EAll entries of RTOL(:) and ATOL(:) = 0.0$EYour
     * TOUT of $2$F is > than your TSTOP of $1$F whidh is not allowed$EV
     *alue of maximum stepsize HMAX = $3$F is <= 0.0.$E'/
      data MTXTAA(8)/'The current T = $F with H=$F is preceded by TOUT =
     * $F.$EValue of initial stepsize H0 is = 0.$EWith a starting T = $F
     *, the TOUT = $1$F is too close.$EWith the current T=$F and H=$F, t
     *he TSTOP=$2$F is inconsistent.$ELower bandwith is'/
      data MTXTAA(9)/' $10$I, upper bandwidth is $I and both should be .
     *ge. 0, and < neq which is $I.$EThe current T = $F and  TOUT = $1$F
     * are equal.$ECode does not handle constraints when using a band ma
     *trix.$EFlag not set for computing constraint corr'/
      data MTXTAA(10)/'ection as it must be with a user defined matrix a
     *nd constraints.$EAt the initial point constraints were not consist
     *ent.$EAt T = $F, with H = $F, the constraints appear inconsistent.
     *  The norm of the residual with maximal perturbati'/
      data MTXTAA(11)/'on of the solution is $F and we did not reduce th
     *e norm below $F.$ELast step terminated  with IDID < 0.  No appropr
     *iate action was taken. IDID = $I.  Repeated occurrences of illegal
     * input. Run terminated. Apparent infinite loop.$E '/
C End of storing data by elements.
      data MTXTAB/'Adjustments to Y in order to get back on the constrai
     *nts were:$N$B'/
      data MTXTAC/'Residuals in the constraints were:$N$B'/
c End of data generated by pmess.
 
c     Staging arrays for error printing of data:
      double precision edat(5)
      integer    mloc(32), mact(5), mactv(6), idat(14), locate,
     1  mecont, meemes, mefvec, meret, metdig, metext
      parameter  (mecont=50, meemes=52, mefvec=61, meret=51, metdig=22,
     1  metext=53)
      data mloc/ltxtaa, ltxtab, ltxtac, ltxtad, ltxtae, ltxtaf,
     &     ltxtag, ltxtah, ltxtai, ltxtaj, ltxtak, ltxtal, ltxtam,
     &     ltxtan, ltxtao, ltxtap, ltxtaq, ltxtar, ltxtas, ltxtat,
     &     ltxtau, ltxtav, ltxtaw, ltxtax, ltxtay, ltxtaz, ltxtba,
     &     ltxtbb, ltxtbc, ltxtbd, ltxtbe, ltxtbf /
      data mact / meemes, 0, 0, 0, meret /
      data mactv / metdig, 6, metext, mefvec, 0, mecont /
c                   2-4   5        6  7  8  9    10 11       12 13-16
      data infobd / 3*1, 14, 3422444, 1, 1, 5, 1000, 1, 1000000, 4*0 /
      integer lerrab, lerrac, lerrad, lerrae, lerraf, lerrag,
     &     lerrah, lerrai, lerraj, lerrak, lerral, lerram, lerran,
     $     lerrao, lerrap, lerraq, lerrar, lerras, lerrat, lerrau,
     $     lerrav, lerraw, lerrax, lerray, lerraz, lerrba, lerrbb,
     $     lerrbc, lerrbd, lerrbe, lerrbf, lerrbg
 
c     The following parameters define the severity for stopping and
c     printing of the associated error message .
      parameter(lerrab =  32)
      parameter(lerrac =  62)
      parameter(lerrad =  62)
      parameter(lerrae =  84)
      parameter(lerraf =  84)
      parameter(lerrag =  86)
      parameter(lerrah =  88)
      parameter(lerrai =  88)
      parameter(lerraj =  88)
      parameter(lerrak =  88)
      parameter(lerral =  88)
      parameter(lerram =  88)
      parameter(lerran =  88)
      parameter(lerrao =  88)
      parameter(lerrap =  88)
      parameter(lerraq =  88)
      parameter(lerrar =  88)
      parameter(lerras =  88)
      parameter(lerrat =  88)
      parameter(lerrau =  88)
      parameter(lerrav =  88)
      parameter(lerraw =  88)
      parameter(lerrax =  88)
      parameter(lerray =  88)
      parameter(lerraz =  88)
      parameter(lerrba =  88)
      parameter(lerrbb =  88)
      parameter(lerrbc =  88)
      parameter(lerrbd =  88)
      parameter(lerrbe =  88)
      parameter(lerrbf =  99)
      integer maperr(31)
      data maperr / lerrab, lerrac, lerrad, lerrae, lerraf, lerrag,
     &     lerrah, lerrai, lerraj, lerrak, lerral, lerram, lerran,
     $     lerrao, lerrap, lerraq, lerrar, lerras, lerrat, lerrau,
     $     lerrav, lerraw, lerrax, lerray, lerraz, lerrba, lerrbb,
     $     lerrbc, lerrbd, lerrbe, lerrbf /
 
 
      SAVE
c
c***FIRST EXECUTABLE STATEMENT  DDASLX
      if (info(1) .eq. 0) then
c                       Initialization
c ----------------------------------------------------------------------
c     THIS BLOCK IS EXECUTED FOR THE INITIAL CALL ONLY.
c     IT CONTAINS CHECKING OF INPUTS AND INITIALIZATIONS.
c ----------------------------------------------------------------------
 
C     First time flag use in ddastp for step smoothing logic.
         INFO(IFIRST)=0
c
c     FIRST CHECK INFO ARRAY TO MAKE SURE ALL ELEMENTS OF INFO
c     ARE valid.
         idid = -11
         do 10  i=2, 16
            if ((info(i) .gt. infobd(i)) .or. (info(i) .lt. 0)) then
               if ((i .ne. 5) .or. (info(i) .lt. -infobd(i))) then
                  if (i .eq. iord) idid = -13
                  go to 500
               end if
            end if
   10    continue
c     Reverse communication controller is cleared and set iwork(ldelt)
         iwork(revloc) = 0
         iwork(ldelt) = ldelta
         iwork(lkold) = 0
         iwork(lk) = 1
c
         if (neq .le. 0) then
            idid = -12
            go to 510
         end if
c
c     CHECK LENGTH IWORK
         leniw = 30 + neq
         if (liw .lt. leniw) then
            idid = -15
            go to 510
         end if
c
c     Reserve space for constraints (if any)
         iwork(lcnstr) = info(icnstr)
c     May be changed later if Jacobian is banded.         
         ldd = neq + iwork(lcnstr)
c
c     May reset maximum number of steps between output points.
         iwork(mxstep) = info(ixstep)
         if (iwork(mxstep) .le. 0) iwork(mxstep) = 510
c
c     Set maximum order
         if (info(iord) .eq. 0)  then
            iwork(lmxord) = 5
         else
            iwork(lmxord) = info(iord)
         end if
c
c     Compute lenpd,lenrw.check ml and mu.
         iwork(lmat) = info(imat)
         i = abs(iwork(lmat))
         nomat = (i .eq. 5) .or. (i .eq. 6)
         lenrw = (iwork(lmxord)+4+iwork(lcnstr))*neq + 45
         if (i .gt. 4) then
            lenpd = 0
            if (i .le. 6) go to 70
            i = i - 6
            if (i .gt. 4) i = i - 4
         end if
c                     User is not taking care of linear algebra.
         if (i .lt. 3) then
c                     The dense case
            lenpd = neq*(neq+iwork(lcnstr))
            lenrw = lenrw + lenpd
         else
c                     The banded case
            if (iwork(lcnstr) .ne. 0) then
               idid = -27
               go to 510
            end if
            if ((iwork(lml) .lt. 0) .or. (iwork(lml) .ge. neq) .or.
     &          (iwork(lmu) .lt. 0) .or. (iwork(lmu) .ge. neq)) then
               idid = -25
               go to 510
            end if
            ldd = 2*iwork(lml)+iwork(lmu)+1
            lenpd = ldd*neq
            if (i .eq. 3) then
c                        Derivatives computed with finite differences.
               mband = iwork(lml) + iwork(lmu) + 1
               msave = (neq/mband) + 1
               lenrw = lenrw + lenpd+2*msave
            else
               lenrw = lenrw + lenpd
            end if
         end if
c
c     CHECK LENGTH OF RWORK.
 70      continue
         iwork(lnpd) = lenpd
         if (lrw .lt. lenrw) then
            idid = -14
            go to 510
         end if
c     Check consistent logic for storage of derivatives
c     and performing the linear algebra.
c
c     CHECK TO SEE THAT TOUT IS DIFFERENT FROM T
         if (tout .eq. t) then
            idid = -26
            go to 510
         end if
c
c     CHECK HMAX
         if (info(imaxh) .ne. 0) then
         hmax = rwork(lhmax)
         if (hmax .le. 0.0d0) then
            idid = -20
            go to 510
         end if
      end if
c
c     INITIALIZE COUNTERS
         iwork(lnst) = 0
         iwork(lnre) = 0
         iwork(lnje) = 0
c
         iwork(lnstl) = 0
         idid = 1
 
         info(1) = 1
 
c            Flag that we are not completely initialized.
         rwork(lround) = 0.d0
c    A call to the evaluator for setting up or initializing.
c    If reverse communication is used this is either
c    a benign call to the dummy routine or else to the user's code,
         ires = 0
         call ddasf (t, y, yprime, rwork, rwork, LDD,
     &   rwork(lcj), ires, rwork, iwork)
         go to 110
      end if
c
c ----------------------------------------------------------------------
c     THIS BLOCK IS FOR CONTINUATION CALLS
c     ONLY. HERE WE CHECK INFO(1), AND IF THE
c     LAST STEP WAS INTERRUPTED WE CHECK WHETHER
c     APPROPRIATE ACTION WAS TAKEN.
c ----------------------------------------------------------------------
c
      if (info(1) .ne. 1) then
         i = 1
         if (info(1) .ne. -1) then
            idid = -11
            go to 500
         end if
c
c     IF WE ARE HERE, THE LAST STEP WAS INTERRUPTED
c     BY AN ERROR CONDITION FROM DDASTP, AND
c     APPROPRIATE ACTION WAS NOT TAKEN. THIS
c     IS A FATAL ERROR.
         idat(1) = idid
         idid = -31
         go to 520
      end if
      if (iwork(revloc) .gt. 0) then
c                               Using reverse communication
         locate = mod(iwork(revloc), 8)
         iwork(revloc) = iwork(revloc) / 8
         go to (180, 380), locate
      end if
      iwork(lnstl) = iwork(lnst)
c
c ----------------------------------------------------------------------
c     THIS BLOCK IS EXECUTED ON ALL CALLS.
c     THE ERROR TOLERANCE PARAMETERS ARE
c     CHECKED, AND THE WORK ARRAY POINTERS
c     ARE SET.
c ----------------------------------------------------------------------
c
  110 continue
      if (info(idb) .ne. 0) call ddasdb(0, neq, t, y, yprime, info,
     &    rwork, iwork, idid, atol, rtol)
c     CHECK RTOL,ATOL
      nzflg   = 0
      itemp = 1
      if (info(itol) .eq. 1) itemp = neq
      do 120  i=1, itemp
         rtoli   = rtol(i)
         atoli   = atol(i)
         if (rtoli.gt.0.0d0 .or. atoli.gt.0.0d0) nzflg = 1
         if (rtoli .lt. 0.0d0) then
            idid = -16
            go to 510
         end if
         if (atoli .lt. 0.0d0) then
            idid = -17
            go to 510
         end if
  120 continue
      if (nzflg .eq. 0) then
         idid = -18
         go to 510
      end if
c
c     SET UP RWORK STORAGE.IWORK STORAGE IS FIXED
c     IN DATA STATEMENT.
      iwork(le)    = ldelta + neq + iwork(lcnstr)
      iwork(lwt)   = iwork(le) + neq
      iwork(lphi)  = iwork(lwt) + neq
      if (nomat) then
        iwork(lwm)   = lrw
      else
        iwork(lwm)   = iwork(lphi) + (iwork(lmxord)+1)*neq
      end if
      iwork(ntemp) = 1 + iwork(lnpd)
      if (rwork(lround) .ne. 0.d0) go to 220
c
c ----------------------------------------------------------------------
c     THIS BLOCK IS EXECUTED ON THE INITIAL CALL
c     ONLY. SET THE INITIAL STEP SIZE, AND
c     THE ERROR WEIGHT VECTOR, AND PHI.
c     COMPUTE INITIAL YPRIME, IF NECESSARY.
c ----------------------------------------------------------------------
c
      tn   = t
      idid = 1
c
c     SET ERROR WEIGHT VECTOR WT. Pass info() to routine for
c     info(itol), info(ismoot)
      call ddaswt (neq, info, rtol, atol, y, rwork(iwork(lwt)),
     &     rwork,iwork)
      do 130  i=1, neq
         if (rwork(iwork(lwt)+i-1) .le. 0.0d0) then
            idid = -5
            go to 510
         end if
  130 continue
c
c     COMPUTE UNIT ROUNDOFF AND HMIN
      rwork(lround) = d1mach(4)
      rwork(lhmin) = 16.0d0 * rwork(lround) * max(abs(t), abs(tout))
c
c     CHECK INITIAL INTERVAL TO SEE THAT IT IS LONG ENOUGH
      tdist = abs(tout-t)
      if (tdist .lt. rwork(lhmin)) then
         idid = -23
         go to 510
      end if
c
c     CHECK HO, IF THIS WAS INPUT
      if (info(ih0) .ne. 0) then
         ho = rwork(lh)
         h = ho
         if ((tout-t)*h .lt. 0.0d0) then
            idid = -21
            go to 510
         end if
         if (h .eq. 0.0d0) then
            idid = -22
            go to 510
         end if
      else
c
c     COMPUTE INITIAL STEPSIZE, TO BE USED BY EITHER
c     DDASTP OR DDAS1, DEPENDING ON INFO(inityp)
         ho     = 0.001d0*tdist
         ypnorm = ddasnm(neq,yprime,rwork(iwork(lwt)),rwork,iwork)
         if (ypnorm .gt. 0.5d0/ho) ho = 0.5d0/ypnorm
         ho = sign(ho,tout-t)
      end if
c     ADJUST HO IF NECESSARY TO MEET HMAX BOUND
      if (info(imaxh) .eq. 0) go to 160
      rh = abs(ho)/rwork(lhmax)
      if (rh .gt. 1.0d0) ho = ho/rh
c     COMPUTE TSTOP, IF APPLICABLE
  160 if (info(istop) .ne. 0) then
         tstop = rwork(ltstop)
         if ((tstop-t)*ho .lt. 0.0d0) then
            idid = -24
            go to 510
         end if
         if ((t+ho-tstop)*ho .gt. 0.0d0) ho = tstop - t
         if ((tstop-tout)*ho .lt. 0.0d0) then
            idid = -19
            go to 510
         end if
      end if
c
c     COMPUTE INITIAL DERIVATIVE, UPDATING TN AND Y, IF APPLICABLE
      rwork(ltn) = tn
 
c     Set initial error estimates to 0 (temporary??)
      do 170 i = iwork(le), iwork(lwt) - 1
        rwork(i) = 0.d0
 170  continue
c If info(inityp)==0, user has provided initial values of y
c and yprime.   
      if (info(inityp) .eq. 0) go to 190
c REVERSE ENTRY 1:
  180 continue
c If info(inityp)==1, user has provided initial values of y
c and a defined value of yprime.  Use Petzold's starting method.
c The preferred starting method uses info(inityp)==2.
      If(info(inityp) .eq. 1) THEN  
      call ddas1 (rwork(ltn), y, yprime, neq, ldd, ddasf, info, ho,
     &     rwork(iwork(lwt)), idid, rwork(iwork(lphi)), rwork(ldelta),
     &     rwork(iwork(le)), rwork(iwork(lwm)), iwork, rwork)
      ELSE If(info(inityp) .eq. 2) THEN
c Use algorithm of Krogh/Hanson based on Newton method for the
c starting values of YPRIME.
         write(*,*) "This should check that F(t,y,y') is small"    
      END IF
      tn = rwork(ltn)
c     See if reverse communication needed:
      if (iwork(revloc) .ne. 0) then
         iwork(revloc) = 8 * iwork(revloc) + 1
         go to 490
      end if
      if (idid .lt. 0) go to 510
c
c     LOAD H WITH HO.  STORE H IN RWORK(LH)
  190 h = ho
      rwork(lh) = h
c           Flag that we are starting for ddastp.
      iwork(revloc) = -1
c
c     LOAD Y AND H*YPRIME INTO PHI(*,1) AND PHI(*,2)
      itemp = iwork(lphi) + neq
      do 200  i=1, neq
         rwork(iwork(lphi)+i-1) = y(i)
         rwork(itemp+i-1) = h*yprime(i)
  200 continue
c
      go to 320
c
c ------------------------------------------------------
c     THIS BLOCK IS FOR CONTINUATION CALLS ONLY. ITS
c     PURPOSE IS TO CHECK STOP CONDITIONS BEFORE
c     TAKING A STEP.
c     ADJUST H IF NECESSARY TO MEET HMAX BOUND
c ------------------------------------------------------
c
  220 continue
      done   = .false.
      tn     = rwork(ltn)
      h      = rwork(lh)
      if (info(imaxh) .eq. 0) go to 230
      rh = abs(h)/rwork(lhmax)
      if (rh .gt. 1.0d0) h = h/rh
  230 continue
      if (t .eq. tout) then
         idid = -26
         go to 510
      end if
      if ((t-tout)*h .gt. 0.0d0) then
         idid = -21
         go to 510
      end if
      if (info(istop) .eq. 1) go to 260
      if (info(iout) .eq. 1) go to 240
      if ((tn-tout)*h .lt. 0.0d0) go to 310
      call ddasin (tn, tout, y, yprime, neq, iwork(lkold),
     &             rwork(iwork(lphi)), rwork(lpsi))
      t    = tout
      idid = 3
      done = .true.
      go to 480
  240 if ((tn-t)*h .le. 0.0d0) go to 310
      if ((tn-tout)*h .gt. 0.0d0) go to 250
      call ddasin (tn, tn, y, yprime, neq, iwork(lkold),
     &     rwork(iwork(lphi)),rwork(lpsi))
      t    = tn
      idid = 1
      done = .true.
      go to 480
  250 continue
      call ddasin (tn, tout, y, yprime, neq, iwork(lkold),
     &             rwork(iwork(lphi)), rwork(lpsi))
      t    = tout
      idid = 3
      done = .true.
      go to 480
  260 if (info(iout) .eq. 1) go to 270
      tstop = rwork(ltstop)
      if ((tn-tstop)*h .gt. 0.0d0) then
         idid = -24
         go to 510
      end if
      if ((tstop-tout)*h .lt. 0.0d0) then
         idid = -19
         go to 510
      end if
      if ((tn-tout)*h .lt. 0.0d0) go to 290
      call ddasin (tn, tout, y, yprime, neq, iwork(lkold),
     &             rwork(iwork(lphi)), rwork(lpsi))
      t    = tout
      idid = 3
      done = .true.
      go to 480
  270 tstop = rwork(ltstop)
      if ((tn-tstop)*h .gt. 0.0d0) then
         idid = -24
         go to 510
      end if
      if ((tstop-tout)*h .lt. 0.0d0) then
         idid = -19
         go to 510
      end if
      if ((tn-t)*h .le. 0.0d0) go to 290
      if ((tn-tout)*h .gt. 0.0d0) go to 280
      call ddasin (tn, tn, y, yprime, neq, iwork(lkold),
     &     rwork(iwork(lphi)),rwork(lpsi))
      t    = tn
      idid = 1
      done = .true.
      go to 310
  280 continue
      call ddasin (tn, tout, y, yprime, neq, iwork(lkold),
     &             rwork(iwork(lphi)), rwork(lpsi))
      t    = tout
      idid = 3
      done = .true.
      go to 310
  290 continue
c     CHECK WHETHER WE ARE WITHIN ROUNDOFF OF TSTOP
      if (abs(tn-tstop) .gt. 100.0d0 * rwork(lround) * (abs(tn)+abs(h)))
     &     go to 300
      call ddasin (tn, tstop, y, yprime, neq, iwork(lkold),
     &             rwork(iwork(lphi)), rwork(lpsi))
      idid = 2
      t    = tstop
      done = .true.
      go to 310
  300 tnext = tn + h
      if ((tnext-tstop)*h .le. 0.0d0) go to 310
      h         = tstop - tn
      rwork(lh) = h
c
  310 if (done) go to 480
c
c ------------------------------------------------------
c     THE NEXT BLOCK CONTAINS THE CALL TO THE
c     ONE-STEP INTEGRATOR DDASTP.
c     THIS IS A LOOPING POINT FOR THE INTEGRATION STEPS.
c     CHECK FOR TOO MANY STEPS.
c     UPDATE WT.
c     CHECK FOR TOO MUCH ACCURACY REQUESTED.
c     COMPUTE MINIMUM STEPSIZE.
c ------------------------------------------------------
c
  320 continue
c
c     CHECK FOR TOO MANY STEPS
      if ((iwork(lnst)-iwork(lnstl)) .lt. iwork(mxstep)) go to 330
      idid = -3
      go to 390
c
c     UPDATE WT. Pass info() to routine for
c     info(itol), info(ismoot)
 330  call ddaswt (neq, info, rtol, atol, rwork(iwork(lphi)),
     &     rwork(iwork(lwt)),rwork, iwork)
      do 340  i=1, neq
         if (rwork(i+iwork(lwt)-1) .gt. 0.0d0) go to 340
         idid = -5
         go to 390
  340 continue
c
c     TEST FOR TOO MUCH ACCURACY REQUESTED.
      r = ddasnm(neq,rwork(iwork(lphi)),rwork(iwork(lwt)),rwork,iwork) *
     %   100.0d0 * rwork(lround)
      if (r .gt. 1.0d0) then
c     MULTIPLY RTOL AND ATOL BY R AND RETURN
         if (info(itol) .eq. 0) then
            rtol(1) = r*rtol(1)
            atol(1) = r*atol(1)
         else
            do 360  i=1, neq
               rtol(i) = r*rtol(i)
               atol(i) = r*atol(i)
  360       continue
         end if
         idid = -2
         go to 510
      end if
c
c     COMPUTE MINIMUM STEPSIZE
      rwork(lhmin) = 4.0d0 * rwork(lround) * max(abs(tn),abs(tout))
c
c     TEST H VS. HMAX
      if (info(imaxh) .ne. 0) then
         rh = abs(h)/rwork(lhmax)
         if (rh .gt. 1.0d0) h = h/rh
      end if
c REVERSE ENTRY 2:
  380 continue
      call ddastp (rwork(ltn), y, yprime, neq, ldd, ddasf, info, h,
     &     rwork(iwork(lwt)), idid, rwork(iwork(lphi)), rwork(ldelta),
     &     rwork(iwork(le)), rwork(iwork(lwm)), iwork, rwork,
     &     rwork(lalpha), rwork(lbeta), rwork(lgamma), rwork(lpsi),
     &     rwork(lsigma), iwork(lk))
 
      tn = rwork(ltn)
c     See if reverse communication needed:
      if (iwork(revloc) .ne. 0) then
         iwork(revloc) = 8 * iwork(revloc) + 2
         go to 490
      end if
  390 if (idid .lt. 0) go to 510
c
c -------------------------------------------------------
c     THIS BLOCK HANDLES THE CASE OF A SUCCESSFUL RETURN
c     FROM DDASTP (IDID=1).  TEST FOR STOP CONDITIONS.
c -------------------------------------------------------
c
      if (info(istop) .ne. 0) go to 420
      if (info(iout) .ne. 0) go to 410
      if ((tn-tout)*h .lt. 0.0d0) go to 320
  400 call ddasin (tn, tout, y, yprime, neq, iwork(lkold),
     &             rwork(iwork(lphi)), rwork(lpsi))
      idid = 3
      t    = tout
      go to 480
  410 if ((tn-tout)*h .ge. 0.0d0) go to 400
      t    = tn
      idid = 1
      go to 480
  420 if (info(iout) .ne. 0) go to 450
      if ((tn-tout)*h .ge. 0.0d0) go to 400
      if (abs(tn-tstop) .le. 100.0d0 * rwork(lround) * (abs(tn)+abs(h)))
     &     go to 440
      tnext = tn + h
      if ((tnext-tstop)*h .le. 0.0d0) go to 320
      h = tstop - tn
      go to 320
  440 call ddasin (tn, tstop, y, yprime, neq, iwork(lkold),
     &             rwork(iwork(lphi)), rwork(lpsi))
      idid = 2
      t    = tstop
      go to 480
  450 if ((tn-tout)*h .ge. 0.0d0) go to 400
      if (abs(tn-tstop) .le. 100.0d0 * rwork(lround) * (abs(tn)+abs(h)))
     &     go to 440
      t    = tn
      idid = 1
      go to 480
c
c -------------------------------------------------------
c     ALL SUCCESSFUL RETURNS FROM DDASLX ARE MADE FROM
c     THIS BLOCK.
c -------------------------------------------------------
c
  480  continue
      rwork(lh)  = h
      if (info(idb) .ne. 0) call ddasdb(1, neq, t, y, yprime,
     &     info, rwork, iwork, idid, atol, rtol)
      return
c
c ----------------------------------------------------------------------
c     THIS BLOCK HANDLES ALL REVERSE COMMUNICATION RETURNS
c ----------------------------------------------------------------------
  490 continue
      t    = rwork(ltn)
      idid = 4
      return
 
c     One central place to call DMESS to print error messages.:
  500 continue
      idat(5) = i
      idat(6) = info(i)
  510 continue
      idat(1) = iwork(lkold)
      idat(2) = iwork(lk)
      idat(3) = iwork(mxstep)
      idat(4) = iwork(lmxord)
      idat(7) = lrw
      idat(8) = lenrw
      idat(9) = liw
      idat(10) = leniw
      idat(11) = iwork(1)
      idat(12) = iwork(2)
      idat(13) = neq
      idat(14) = idid
c
      edat(1) = tn
      edat(2) = h
      if (IDID .le. -29) then
        if (IDID .eq. -29) then
          mact(5) = mecont
        else if (IDID .eq. -30) then
          edat(3) = rwork(ldelta)
          edat(4) = rwork(ldelta+1)
        end if
      else
        edat(3) = tout
        edat(4) = hmax
        edat(5) = tstop
      end if
c
 520  continue
      mact(2) = maperr(-idid)
      mact(3) = -idid
      mact(4) = mloc(1 - idid)
      call dmess(mact, mtxtaa, idat, edat)
      info(1) = -1
      if (IDID .eq. -29) then
        mact(5) = meret
        mactv(5) = neq
        mactv(6) = mecont
        call dmess(mactv, mtxtab, idat, rwork(ldelta))
        mactv(5) = info(10)
        mactv(6) = meret
        call dmess(mactv, mtxtac, idat, rwork(ldelta+neq))
      end if
      go to 480
c ----------END OF SUBROUTINE DDASLX------------------------------------
      end
