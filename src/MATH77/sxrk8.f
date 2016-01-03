      subroutine SXRK8(TS, Y, OPT, IDAT, DAT, WORK)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c Time-stamp: <2009-10-19 12:24:07 root>
c>> 2009-10-15 SXRK8 Krogh Latest version of new stepsize control.
c>> 2008-12-22 SXRK8 Krogh Many changes to put in new stepsize control.
c>> 2008-02-28 SXRK8 Krogh Lots of changes in usage, bug fixes.
c>> 2007-09-11 SXRK8 Krogh Fixed flag on max step output prior to final.
c>> 2007-09-11 SXRK8 Krogh Fixed DAT(MAXINC) getting too small.
c>> 2007-09-10 SXRK8 Krogh Some options were not processed.
c>> 2007-09-10 SXRK8 Krogh Stored 0 in IDAT at end Err. Tol. Info.
c>> 1997-12-18 SXRK8 Krogh Modifications for MATH77 library.
c>> 1997-02-24 SXRK8 Krogh Initial code.
c--S replaces "?": ?XRK8,?XRK8A,?XRK8F,?XRK8I,?XRK8N,?XRK8O,?XRK8X,?MESS
c
c Solves a system of first order ordinary differential equations
c y'=f(t,y), using an eXplicit Runge-Kutta method of order 8 due to
c Dormand & Prince.
c This code was obtained starting with the code DOP853 by Hairer &
c Wanner, Version of April 25, 1996.  That code is described in
c   E. Hairer, S.P. Norsett and G. Wanner, Solving ordinary differential
c   equations I. Nonstiff Problems. 2nd Edition, Springer-Verlag (1993).
c
c This version written by Fred T. Krogh, has a different user interface,
c avoids the function evaluations required for interpolation on steps
c that do no interpolations, provides a G-Stop feature, uses a slightly
c different algorithm for stepsize selection, and provides a number of
c other options.
c
c ************* User Subprograms & calls to SXRK8I or DXRKIG ***********
c
c If reverse communication is not specified, for computing derivatives,
c the user must code a subroutine of the form:
c    subroutine sxrk8f(TS, Y, F, IDAT)
c And if reverse communication is not used for output, the user must
c code a subroutine of the form:
c    subroutine sxrk8o(T, Y, IDAT, DAT)
c If G-stops are used, then the user will be told to call dxrk8g from
c time to time.  Call looks different depending on whether they are made
c from sxrk8f or from the users code as a result of using reverse
c communication.
c   From sxrk8f:  call dxrk8g(T, Y, F, IDAT)
c   Reverse Comm: call dxrk8g(TS, Y, WORK, IDAT)
c After a return from sxrk8, if one is continuing an integration, one
c should call sxrk8a instead of sxrk8 as follows
c                 call sxrk8a(TS, Y, F, IDAT, DAT, WORK)
c where F is only used if one has specified the option that uses it for
c storing derivatives.
c
c ************************ Calling Sequence Arguments ******************
c
c With the very simplest usage one sets:  TS(1) = initial t;, TS(2) = 0
c (gives automatic selection of initial stepsize); TS(3) = final t; y =
c initial values for y; IDAT(4) = NEQ = number of equations; IDAT(1) = 0
c (indicates starting); IDAT(5) to IDAT(7) to values giving the space
c available in various arrays; and OPT(1) = 0.E0 to indicate no options
c are begin specified.  This gives an integration from TS(1) to TS(3)
c with a default error tolerance, which gives an absolute and relative
c error tolerance = (floating point epsilon)**.75.
c
c TS     [in/out] Array holding information connected with the
c        independent variable.
c   TS(1)  The current time, t.  Initially the starting time.
c   TS(2)  Current stepsize, h.  If h=0.E0 it is selected automatically.
c   TS(3)  Final time, tfinal.  Program won't integrate past this time.
c   TS(k>3) If G-Stops are used G_i is stored in TS(i+3).  The value of
c           TS(1) associated with previous G's is store in the next
c           location, and the previous G's follow it.  The user need not
c           touch these latter locations unless they are doing something
c           unusual such as changing the definiation of G.
c Y      [in/out] Vector containing the current values of the dependent
c        variables, y.  Dimension of NEQ, see IDAT(4).
c OPT    [in] This vector is used for the specification of options.
c  Options are indicated with a floating point integer followed by 0 or
c  more arguments.  The arguments are indicated in parentheses in the
c  order in which they must be given.  If an argument is followed by an
c  "=...", the "..." gives the default value for this option.  Following
c  the arguments, the next option is specified.
c  Options are indicated with integers below, even though they must
c  of course be supplied as floating point numbers.  For clarity in
c  specifying these options one may want to use the names for these
c  parameters.  The start of the declarations below gives declarations
c  for defining these parameters.  Other parameters used for accessing
c  IDAT and DAT are defined there also.  If one is accessing these
c  arrays we recommend that it be done using these parameters.
c
c XRKEND=0  End of options.
c
c *************  Options applying to groups of equations. **************
c  An Option which refers to a group of equations applies to all groups
c  after the option is specified, until another option overrides it.
c  When another option overrides a given option, the prior option
c  applies to equations with indexes from (the last value of A1 when
c  the option was specified + 1) to (the last value of A1 when the
c  option was overridden).  There is an implied A1=0 at the start of
c  the option processing.
c
c XRKEQG=1 (A1=NEQ) Index of last equation in a group of equations.  If
c    the end of the options is reached without getting this option, with
c    A1=NEQ, then it is as if this option appeared with A1 = NEQ =
c    IDAT(4) just before the end of the options.  This option is only
c    necessary if one wants to treat groups of equations differently in
c    some way.  If this option is specified more than once, the values
c    of A1 must be strictly increasing.
c
c  Following the practice in Hairer, Norsett, and Wanner, referenced
c  above, when referring to orders, we are referring to global order.
c  The local order is 1 greater than the global order.  To generate an
c  error estimate, this code obtains E3 and E5 of orders 3 and 5
c  respectively.  We want a estimator of order 8.  This is obtained by
c  forming h * E5 * (E5 / E3), where h is the stepsize.  (Actually we
c  generate the squares of (these estimates / accuracy requested).) It
c  is this order 8 estimator that is used in controlling the stepsize.
c  (Actually E3 is (.01 * the average of recent value to what the error
c  would be with a third order method).  When E5 / E3 is > 1 the code
c  uses h*E5 as the error estimate, and if this occurs too frequently
c  may give a diagnostic that the system appears stiff.  This diagnostic
c  will only be given once.  When forming error estimates we have
c  vectors v3_i and v5_i, i = 1, 2, ..., NEQ.  For each of these v's we
c  form a sum of the form ||v||_e = sum_{i=1}^NEQ (v_i / tol_i)^2, where
c  tol_i is the absolute error tolerance being requested on the i-th
c  equation of the system.  The code attempts to keep the order 8 error
c  estimate divided by the rquested accuracy times DAT(LERTRY) (nominal
c  value 100) at 1, and repeats the step if the square toot of this
c  quatity is >DAT(LERTRY+1) (nominal value is 4).  If using an absolute
c  error tolerance or a relative error tolerance when the solution may
c  get very close to zero, one should insure that the absolute error
c  part of the tolerance does not underflow or overflow, when it is
c  squared.  No checks are made to insure that this is the case.
c
c XRKAE=2 (A2) For the group in which this error tolerance applies,
ctol_i (see just above) is A2.  XRKRE1=3 (A3=e**(.75E0), B3=e**(.75E0))
c(tol_i**2) is (A3**2 + (B3**2) * (sum in group of (y_i)**2)).  Note
cthat this is the default error test with A3 = B3 = e, where e is the
csmallest number > 0 such that the floating point addition 1 + e gives a
cresult different from 1.  XRKRE2=4 (A4, B4) tol_i**2 is (A3**2 +
c(B3*y_i)**2).  XRKAEV=5 (A5) Similar to option 2, except "A2" is now a
cvector, with values stored starting in DAT(A5), where A5 > LOCTY +
c2*NEQ.  (The parameter LOCTY has the value 30.) This has the same
ceffect as option 2 followed by option 1 with A1 increased by 1.E0 each
ctime, for the number of times there are equations in this set.  Space
cused in DAT for storing this kind of error control information should
cbe contiguous and specified with increasing values of A5.  XRKREV=6
c(A6) as for option 5, except applied to option 4.  Thus there are
cvalues ("A3", "B3", "A3", ...) stored starting at DAT(A6).  If both
coptions 5 and 6 are used, the space used by the vectors for option 6
cmust not overlap with the space used by vectors for 5.  XRKNOE=7 Don't
caccumulate anything into the measure of the error for equations in this
cset.  XRKDIG=8 (A8=0.E0) A8 = 0.E0, turns off diagnostic output for an
cequation group, =1.E0 turns on output giving information used in
ccomputing the error measure for the groups.  XRKIN=9 (A9=1.E0) A9 =
c1.E0 turns on interpolation, = 0.E0 turns it off.  If one is doing
cinterpolation to an output point, no valid results are returned for
cequations which are in groups with A9 = 0.E0.  If you have G-Stops,
cmake sure that all the y's used in computing them are interpolated for.
cXRKXP=10 (A10=0.E0) A10 = 1.E0 causes extra precision to be used in
caccumulating Y, =0.E0 uses standard floating point precision.  (This
coption implies that extra precision will also be used in accumulating
cTS(1).  If one only wants the extra precision in TS(1), follow this
coption with option 1, and A1 = 0.E0, and then option 10 with A10=0.E0.)
cXRK???=11 Reserved (???)  ********** End of Options applying to groups
cof equations. ***********
c
c           Other options
c XRKFR1=12 Use reverse communication for computing f(t,y), rather than
c    having a call made to SXRK8F.  When a return is made to the calling
c    program requesting an evaluation of f(t,y), the user should store
c    f(t, y) in F, and call SXRK8A.
c XRKFR2=13 Use reverse communication for computing f(t,y), rather than
c    having a call made to SXRK8F.  Store f(t, y) in WORK(IDAT(2)).
c    This saves copying F to WORK(IDAT(2)) inside SXRK8A.
c XRKOR=14 Use reverse communication for output rather than having a
c    call made to SXRK8O.
c XRKMAX=15 (A15=|tfinal-current value in TS(1)|, tfinal = TS(3)) A15
c    gives absolute value of the maximum stepsize allowed.
c XRKMIN=16 (A16=0.E0) A16 gives the minimum absolute stepsize allowed.
c XRKEPS=17 (A17=mach_eps) A17 gives the precision of the floating point
c    arithmetic.  If one knows that there is a loss of precision in
c    computing the derivatives, one may want to set this to a value
c    larger than the default value.
c XRK???=18 Reserved (Use arc length as independent variable?)
c XRK???=19 Reserved (???)
c
c XRKMXS=20 (A20) A20 = maximum number of steps between output points.
c    If this option is not set, 100,000 steps are taken before giving
c    output with IDAT(1) = 20.
c XRKEOS=21 Give output at end of all steps.
c XRKTI=22 (A22) Interpolate the solution at t = A22.  A new value may
c    be set for the output point when output at this point is indicated.
c XRKTS=23 (A23) As for option 22, except return value at the end of
c    the first step at or past A23 point.
c XRKTDT=24 (A24, B24) As for option 22, except after giving the output,
c    if one does not change the output point, the value of B24 is added
c    to the current output point to get a new one.
c XRKXG=25 (A25).  A25 gives the number of extrapolatory G-Stops.
c XRKIG=26 (A26).  A26 gives the number of interpolatory G-Stops.
c XRKEC=27 (A27=4.6E0, B27=.16E0)  Logic in the code attempts to select
c    the stepsize so that -log(||Error Estimate||^2) is A27, and will
c    reject a step if ||Error Estimate||^2 > B27.  ||Error Estimate|| is
c    computed by dividing error estimates by requested tolerances.
c    These numbers will be adjusted upwards if the requested error can
c    not be obtained.
c
c XRKIFS=28 (A28=.1E0) Fraction of a step by which an interpolation
c    point can be outside the current integration interval.
c
c IDAT   [in/out] Integer vector containing all the integer information
c        needed to do the integration.
c   IDAT(1), IDAT(2) and IDAT(3) define the state of the integration
c        when SXRK8 (or SXRK8A) is called, and when it makes a return to
c        the user, or calls one of the user coded routines.  Changing
c        IDAT in any way other than described here gives undefined
c        results.  IDAT(3) is only used as noted, and should not be
c        changed by the user.
c   IDAT(1) = 0  Starting an integration, not set on return from SXRK8.
c        IDAT(2) and IDAT(3) are not used when starting.
c   IDAT(1) = 1  F = f(T,Y)
c        If reverse communication is used replace T with TS(1).
c        If the reverse communication used is one that does not require
c        an extra copy, replace F with WORK(IDAT(2)).
c        If extrapolating G-Stops are being used, all such G's should
c        be computed prior to computing f, and if dxrk8g sets IDAT(2)
c        < 0, one should immediately return to the integrator.
c   IDAT(1) = 2  Computing G.  (IDAT(3) is 0, except where indicated)
c      IDAT(2) = 0, compute G's indicated below and call dxrk8g.  (In
c            many cases your code will be cleaner if you simply compute
c            all of the G's in all cases and this is allowed.)
c         IDAT(3) is used as follows in this case.
c            = 0  First check, compute all interpolating G's.
c            > 0  A zero of G(IDAT(3)) has been found, compute all G's.
c            =-1  Check of extrapolating G-Stops on partial advanced
c                 step, compute all G's.
c      IDAT(2) > 0, the index of the G whose 0 is currently being
c        iterated for.  Compute at least this G, and call dxrk8g.
c        IDAT(3) is 0 if in the middle of an iteration, and is the
c        same as IDAT(2) if checking ahead for sign persistence.
c      IDAT(2) = -1 computation of G's is complete, return to sxrk8.
c        IDAT(3) gives the index of the G_i which just crossed a 0.
c      IDAT(2) = -2 No iteration of G at the end of this step was
c        necessary, simply return to SXRK8.
c      IDAT(2) =-3 interpolation must be set up, and an iteration
c        started for the G with the index in IDAT(KEOS).  If
c        IDAT(KEOS) is -1 we are setting up to extrapolate.
c      IDAT(2) =-4 the stepsize needs restricting for an extrapolating
c        G-Stop.  WORK(IDAT(LOCINT)-3) contains the multiple of the
c        last stepsize to use.
c   IDAT(1) = 3  Flags some kind of termination condition.
c      IDAT(2) = 0, the integration has reached TS(3) = tfinal.  If
c        TS(3) is changed the integration can be continued.  (Doing
c        this sort of thing only makes sense if no option that requires
c        interpolation has been specified.)
c      IDAT(2) = -1  Set by the user (when processing an output point)
c        to interrupt the integration at the end of the step in a state
c        such that the integration can be continued.  At the completion
c        of the step, a return is made with IDAT(1) = 3, and IDAT(2) =
c        1.  The value of WORK need not be preserved when continuing
c        with IDAT(1) = 3, and IDAT(2) = 1.
c      IDAT(2) = 1   Continue an integration that was interupted when
c        IDAT(1) > 19, and IDAT(2) was set to -1.
c      IDAT(2) > 1 Works as does IDAT(2) = 0, except this value has
c        results when the user set IDAT(1) = 3 and IDAT(2) < -1.  SXRK8
c        then sets IDAT(2) to its absolute value and returns to the
c        user.  The integration can not be continued after this point.
c      If sxrk8o is being called for output points then it will be
c      called with IDAT(1)=3, and IDAT(2) = 0 upon reaching the final
c      point.  Upon return from sxrk8o, if TS(3) has not been changed,
c      a return to the calling program will be made with these same
c      values for the flags.
c   IDAT(1) = 4  An error has occurred.  IDAT(2) identifies what.
c      IDAT(2) = 1  TS(1) = TS(3) at start.
c              = 2  Error estimate too large, and at min. stepsize.
c              = 3  Not used.
c              = 4  Looks like problem is getting stiff
c              = 5  Looks like too much precision has been requested.
c                   On this return IDAT(3) is 0.  If it is not changed,
c                   the code will change the accuracy requested to what
c                   it regards as a more reasonable value.
c              = 6  An extrapolating G-Stop encountered upon taking a
c                   step that didn't change TS(1).
c              = 7  Precision limited by noise in computation.
c#                  (Code has no way to set this now.  Such a test would
c                   probably be based on computing the norm of f, and
c                   flagging condition when (E3/||f||)**(2./3.)<<E5/E3.
c                   Could do this check after taking a few steps.  If
c                   this check indicated a problem one should then
c                   compute a couple of extra close spaced f's to get an
c                   estimate of the noise level.)
c              = 10 Trying to continue an integration that finished.
c              = 11 Equation indexes in options are badly ordered.
c              = 12 Equation index for group is too large (> IDAT(4))
c              = 13 A repeated spec. for the same group of equations.
c              = 14 An undefined option.
c              = 15 Entered with an invalid value for IDAT(1).
c              = 16 Trying to continue after a fatal error reported.
c              = 17 Absolute/Relative error tolerance vectors overlap.
c              = 18 Need to set aside more space in IDAT.
c              = 19 Need to set aside more space in DAT.
c              = 20 Need to set aside more space in WORK.
c              = 21 Option required inside equation group, isn't.
c              = 22 Bad value for IDAT(1) on entry to SXRK8A.
c              = 23 Initial TS(2) has the wrong sign.@'')')
c              = 24 DXRK8G was not called when requested.
c   IDAT(1) = 5  Restarts the integration.  Ordinarily it is better
c        do the restart using IDAT(2) < 0, as described for IDAT(1)>19.
c   IDAT(1) > 19  Values of T and Y have been set for an output point,
c        and IDAT(1) gives the integer index of the option that resulted
c        in this output point.  If 21 < IDAT(1) < 24, then IDAT(2) gives
c        the order in which this option was declared among the options
c        in this range and DAT(IDAT(3)) contains the value of t
c        associated with the output.  Thus if one has a large number of
c        different output points defined, one has a way to tell
c        specifically which is responsible for the current output, and
c        can change the value in DAT(IDAT(3)) (and/or DAT(IDAT(3)+1)
c        when IDAT(1) = 24).  In the case of other values of IDAT(1)>19,
c        IDAT(2) is 1, except in the case of G-Stops when IDAT(2) gives
c        the index of the g function which has a zero.  To get a return
c        to the program calling SXRK8 with IDAT(1) = 3, and IDAT(2) =
c        -1, simply set IDAT(2) = -1.  If one would like to restart the
c        integration at the current value of T and Y, set IDAT(2) = -2
c        to use the same stepsize, and IDAT(2) = -3 to get the new one
c        picked automatically.  If one would like to interpolate
c        immediately to an arbitrary value of T, set T (or TS(1)) to the
c        interpolation point and IDAT(2) = -4.  The values at the
c        interpolated point will be returned with IDAT(1) = 20, IDAT(2)
c        = 1, and IDAT(3) = 0.  One can continue setting IDAT(3) = -4 as
c        many times as desired.
c   IDAT(4) [in only] Contains NEQ, the number of differential
c        equations.  Dimension of Y and F (if used) must be at least
c        this large.
c   IDAT(5) [in/out] The space at the beginning of IDAT that may be
c        referenced by SXRK8.  If one provides a negative number here,
c        its absolute value is used, and the value in IDAT(5) is
c        replaced by the space in IDAT actually required by SXRK8.
c        LOCERR is a parameter with a current value of 44, Let I8 =
c        number of times option 8 (XRKDIG) has been turned "on" and I9
c        and I10 be defined similarly for options 9 (XRKIN) and 10
c        (XRKXP).  Let IE be the number of times options 2-7 (error
c        control) have been specified (including the implied
c        specification for the first group of equations if an explicit
c        specification is not given) and IO the number of options for
c        I/O (21 to 24) that have been specified.  Then one should have
c        IDAT(5) .ge.  LOCERR + 10 + 2*(I8+I9+I10+IO) + 3*IE.  The
c        actual space needed may be smaller by 1 or 2.  If you are
c        concerned with such details, run once with IDAT(5) < 0.  If you
c        are concerned that minor future perturbations to your code in
c        the future may result in slightly more storage being required,
c        make these numbers a little larger than required.
c   IDAT(6) [in/out] As for IDAT(5), except for DAT.  Make IDAT(6) .ge.
c        LOCTY (=30) + 2 * IDAT(4) + space for all error tolerances +
c        space for defining t output values and delta t for option 24
c        (XRKTDT) + if using extra precision, 1 for t and for each y
c        that is kept in extended precision.
c   IDAT(7) [in/out] As for IDAT(5), except for WORK.  Define LWI =
c        IDAT(4) if there is no interpolation being done to arbitrary
c        points, and LWI = max(IDAT(4), 6 + 8 * (number of components of
c        Y interpolated to arbitrary points (default is IDAT(4))).  Make
c        IDAT(7) .ge. 9*IDAT(4) + LWI.
c
c Starting in IDAT(10) are integer variables used internally.  These
c locations are referenced in the code with IDAT(<name>), where <name>
c is a parameter name.  Such names are defined with IDAT under the
c section "Variable Definitions" below.  Beyond this area of IDAT is
c space used for the processing of various options.
c
c DAT    As for IDAT, except starting in DAT(1) are floating point
c   data needed during the integration.  This data must be preserved if
c   an integration is interrupted by some other computation.  As for
c   IDAT there are parameter names defined below for accessing scalar
c   data in DAT.
c WORK   Data that need not be saved from one step to the next if the
c   integration is interupted for another task.  This data must be
c   preserved if the integration is interrupted by some other means than
c   using option 3.  See WORK under "Variable Definitions" for more
c   details.
c
c ************************* Variable Definitions ***********************
c
c Note that names used in subscripts of DAT and IDAT are defined in
c alphabetical order under DAT and IDAT respectively.
c DAT    Formal argument, see above.  Below is an alphabetical listing
c   of parameter names that are used in referencing DAT.  Thus to find
c   what reference to DAT(<name>) is about, one should start by looking
c   for <name> in the list below, before looking for the parameter name
c   directly.  The parameter declarations for these names gives the
c   values of the associated parameters.  The user should only alter
c   values in these locations of DAT with careful reflection on all
c   possible consequences and after verifying that the values of the
c   parameters used in the code are what they expect them to be.  The
c   end of DAT is used for data required for the processing of options.
c
c <name>    Contents of DAT(<name>)
c HA     contains the absolute values of the stepsize.
c LAH    The value of AH used in defining the current stepsize.
c LASTE3 contains last estimate for the 3-rd order error estimate
c    (multiplied by a factor and not including the factor of h).
c LEPS   contains EPS = smallest number such that 1.E0 + EPS .ne. 1.E0.
c        Option XRKEPS=17 can be used to change the above value.
c LERR   DAT(LERR:LERR+2) contain information associated with
c        stepsize control.  Exponential average is based on the
c        parameter W.
c    DAT(LERR)   = "constant" part of exponential average of errors.
c    DAT(LERR+1) = "linear" part of exponential average of errors.
c    DAT(LERR+2) = "quadratic" part of exponential average of errors.
c    This area is also used to store some values when computing the
c    initial stepsize.
c LERR1  =1 if the first equation index with error control is 1, else it
c    is 2.
c LERTRY An attempt is made to keep DAT(LERTRY) * ||estimated error /
c   requested error|| at 1.  If this is > DAT(LERTRY+1)the step is
c   rejected.  If there is no error control, DAT(LERTRY) = 0.E0
c LHMAX  Current maximum stepsize.  (= DAT(LHMAXU) unless the code
c   thinks there is a risk of stability problems.
c LHMAXU contains the maximum stepsize (= inteval length if not set by
c   the user).
c LHMIN  contains the minimum stepsize.
c LINT   IDAT(IDAT(LINT)) to IDAT(IDAT(LINT)+1) contain data defining
c   the current T interpolation point.  If IDAT(LINT) is 3, we are at
c   the final output point.
c LINTX  When interpolating, the interpolation point is allowed to be
c        outside the interval of the current step by DAT(LINTX) * |the
c        stepsize|.
c LOCSAV This and following locations are used to save a few floating
c        point quantities when a recoverable error return is made.
c        Also used to save output T for user interpolation interupts.
c LOCTGS contains TS(1) at G-Stop that is preceded by a T-Stop.
c LOCTY  The value of t at the beginning of the current step.  The
c        vector y at the beginning of the current step follows
c        immediately, and the contents of f at the beginning of the
c        step follow immediately after y.
c LOCTXG The value of T we want to use when extrapolating to check on
c        extrapolating G-Stops.
c LPHI   last value computed for PHI
c LSAVEH value of stepsize saved when taking small step to final t or
c        for an extrapolating G-Stop.
c LTOUT  value of the independent variable at the next output point.
c NEGPOS -1.E0 if the stepsize if < 0, and 1.E0 otherwise.
c NEXTH  value to be used for the stepsize on the next step.
c NEXTT  value to be used for the base time on the next step.
c PHI    The current value for phi, the coefficient expected for
c   the error term.

c
c ************************* End if DAT parameters **********************
c
c DATERR Used for temp. storage of floating pt. data for error messages.
c DIFFTS The difference TS(3) - TS(1).
c EN     Passed to SXRK8N.  EN(1) is the square of the fifth order error
c   estimate, and EN(2) is the square of the third order estimate.
c ERR    Ratio of (error estimate / error request)**2.
c ERR1   Used for storing error option values when processing options.
c ERR2   Same as for ERR1, but value from next location in OPT.
c ERR3   Starts as square of the 3rd order error estimate.
c ERR5   Starts as squere of the 5th order error estimate.
c GOTINT True if interpolation might be required, false otherwise.
c I      Temporary integer.
c I1     Temporary integer.
c I2     Temporary integer.
c II     Temporary integer.
c IBEG   Index of next location available for storing option data in
c   DAT.
c IDAT   Formal argument, see above.  Below is an alphabetical listing
c   of parameter names that are used in referencing IDAT.  Thus to find
c   what reference to IDAT(<name>) is about, one should start by looking
c   for <name> in the list below, before looking for the parameter name
c   directly.  The parameter declarations for these names give the
c   values of the associated parameters.  The user should only alter
c   values in these locations of IDAT with careful reflection on all
c   possible consequences and after verifying that the values of the
c   parameters used in the code are what they expect them to be.  The
c   end of IDAT is used for data required for the processing of options.
c
c <name>    Contents of IDAT(<name>)
c INTSAV Points to the initial location for saving information about
c        output points
c KDZERO Used in DXRK8G for flag used in call to DZERO.
c KEOS   Gives the state of processing at the end of a step.  Set from
c        IDAT(LOCEOS).  Possible values are defined there.  This is
c        also used to hold the index of the current G-Stop when
c        iterating for a G-Stop and set to -1 when extrapolating
c        because of an extrapolating G-Stop.
c KHMIN  Number of times we have had H = DAT(LHMIN) and error to big.
c KFAIL  Starts at 0 and incremented by 1 on each step failure.
c KSTEP  Number of (accepted) steps taken.
c KSTEPM Value for IDAT(KSTEP) at which a user return is called for.
c        On such a return, IDAT(KSTEPM) is increased by IDAT(KSTEPM+1).
c KSTEPX When IDAT(KSTEP) .ge. IDAT(KSTEPX), we have enough accepted
c        steps so that something special needs to be done.  Set to -1
c        if user want to save the solution.
c KSTIFF Count of times in a row we think stiffness is an issue.
c KWORK  Amount of space used in WORK.
c LAST0  Index of last location in IDAT to set 0 initially.
c LDAT   Length of DAT available = | IDAT(6) |
c LIDAT  Length of IDAT available = | IDAT(5) |
c LOCEOS Gives the initial value assigned to IDAT(KEOS) when done with
c        a step.
c       = -3 End of step then G-Stop Check
c       = -2 End of step then TOUT
c       = -1 G-Stop Check then TOUT
c       =  0 TOUT then finish
c       =  1 Finish End of step actions.
c LOCEPR If nonzero, points to a list: (i1, j1), (i2, j2), ..., 0.
c        Error estimates are printed for equations i_k to j_k for
c        k = 1, 2, ...
c LOCERR This must be the last location specified as a constant in IDAT.
c        Starts a list of actions defining how to compute the square
c        of the norm of a vector relative to the error tolerance.  This
c        list has the form k_1, e_1, l1, k_2, ...., e_?=NEQ, l_?.  The
c        k_i correspond to options 2-7 for specifying error criteria,
c        the e_i give the indexes for the last equation for which the
c        specification applies, and the l_i (not used if k_i = 7) point
c        to where the error tolerance information is actually stored in
c        DAT.  (When the error information is a vector, the value of
c        l_i is such that when the index of the equation (or 2 times
c        the index for the case of option 6) when added to l_i gives
c        the value to use for the error tolerance.
c LOCINT Points to area in WORK where information required to do
c        interpolations is stored.  Let NIY = IDAT(NUMINT), t0 and t1
c        be the values of t at the beginning and end of the last step
c        taken, and y[t0], y[t0,t1], ...  y[t0,t1,t0,t1,t0,t1,t0,t1]
c        denote estimated divided differences of y.  The first NIY
c        locations contain the values of y[t0], required, the next NIY
c        locations contain the values of y[t0,t1] required, etc.  Let
c        LI = IDAT(LOCINT), then WORK(LI-3:LI-1) are used to contain:
c        Value for t at which an interpolation is to be made, value of
c        t at beginning of the step with respect to which the
c        interpolation is being done, and the (signed) length of the
c        step.  In addition in the case of G-Stops,  WORK(LI-6) is used
c        to save a value of G used in the iterations, WORK(LI-5) is
c        used for the corresponding value of T, and WORK(LI-3) is
c        used to store the value of T associated with a possible zero.
c LOCOUT Points to a list of actions needed to find the next output
c        point after informing the user that a given output point has
c        been reached.  This list has the form: i_1, k_1, i_2, k_2, ...
c        30, 0, where i_n gives the index for the option, and DAT(k_i)
c        contains the value of the output point.  This list is always
c        kept ordered so that DAT(k_n) should be output before or is
c        the same as DAT(k_{n+1}).
c LOCXP  If nonzero, points to a list: k, (i1, j1), (i2, j2), ...,
c        NEQ, where DAT(K) contains the extra precision part of TS(1)
c        and succeeding locations contain the extra precision parts of
c        Y that are needed, and extra precision is maintained in Y for
c        equations i_k to j_k for k = 1, 2, ...  If this option is
c        specified, extra precision is always maintained for TS(1).
c LREVF  If 0, call SXRK8F for computing f.
c        If -1, use reverse communication for computing f, user stores
c               f in WORK(IDAT(3)).
c        If 1, treat as for -1, except user stores f in F.
c LREFO  If 0, call SXRK8O for going output.
c        If 1, use reverse communication for output.
c LSTLAS Value of IDAT(KSTEP), the last time a step was taken with
c     order 3 error estimate relatively small relative to the order 5
c     error estimate.
c LTFIN  Location in the list starting with LOCOUT for the final point.
c LWORK  Length of WORK available = | IDAT(7) |
c LWHYF  Index giving the current reason for computing the derivatives.
c    1-12  Identifies the stage of the Runge-Kutta process.
c   13-15  Used to obtain the extra derivatives required when
c          computing the data needed for doing interpolation.
c      16  At the initial point.
c      17  Extra derivative used in automatic selection of the initial
c          stepsize.
c      18  Just got last derivative required to complete a step.
c      19  Got a G sign change, take a small step prior to iterating.
c      21  Used as flag to let us know dxrk8g was called at end of step.
c NERRSV = -2 on the first step, -1 on the second, 1 just after a
c         rejected step after getting started and 0 othewise.
c NGHI   Absolute value gives the highest index of a G-Stop found at a
c        given value for TS(1).  See NGLO below.
c NGLO   As for NGHI, but the lowest indexed G-Stop for a given value of
c        TS(1).  The highest indexed G-Stop is reported to the user
c        first.  Signs on NGHI and NGLO are used as follows.
c  NGLO  NGHI
c   0         No G-stops active at this value of t.
c   >0    >0  Interpolation is giving values to output.
c   >0    <0  Iterpolation for t output followed by a G-Stop.
c   <0    >0  Interpolation to start iterating for interpolating G-Stop.
c   <0    <0  Extrapolation to start iterating for extrapolating G-Stop.
c NTGS   Starts equal to the number of interpolating G-Stops.  Later the
c        index in TS of the last G-Stop, 0 if none.
c NUMINT The number of y's computed when interpolating.  If data needs
c        to be computed before doing an interpolation, IDAT(NUMINT) is
c        negated.
c NXGS   Starts equal to the number of extrapolating G-Stops.  Later the
c        index in TS of the last extratpolating G-Stop.
c ************************ End if IDAT parameters **********************
c
c IERR   Internal index for an error message.
c IERROR Array used to map IERR to the external error messaged index.
c ISTOP  Array used to map IERR to a stopping index for error messages.
c ITEST  Index of last location in current area for saving data in
c   DAT.  If no more space is available, this is set to 0.
c J      Temporary integer.
c K      Temporary integer.
c KACT   An array of integers used in processing options.  The contents
c     of KACT(i) = 10*c + s, where
c   s is the space required in OPT to specify option i.
c   c is the category of option i, with the following possibilities:
c    =0  End of options -- May need final actions for some categories.
c    =1  Equation group -- Special actions for categories 2-5.
c    =2  Category for tracking error control specs.  See LOCERR.
c    =3  Category for tracking diagnostic output on error estimation
c        for individual equations.  See LOCEPR.
c    =4  Category for tracking which dependent variables are to be
c        maintained with extra precision.  See LOCXP.
c    =5  No longer used.  Might be used later?
c    =6  Category for tracking actions to take to determine the next
c        value of the independent variable which is an output point.
c        See LOCOUT.
c    =7  Option specifying an action that takes place on the end of a
c        step.
c    =8  Option not defined.
c   >10  Option that sets a value in IDAT(c) (value is 1 if s=1).
c >1000  Option that sets values in DAT(c-1000) & if s=3 in DAT(c-999).
c    Data in KACT is set as follows
c Opt 10*c+s     Brief Description
c  0     1   End of options.
c  1    12   (A1=NEQ) Index of last equation in a group of equations.
c  2    22   (A2) A2 gives an absolute error test tolerance
c  3    23   (A3=e**(.75E0), B3=e**(.75E0)) Relative error
c  4    23   (A4, B4)  Similar to option 3
c  5    22   (A5) A5 is pointer to absolute error data.
c  6    22   (A6) A6 is pointer to relative error data.
c  7    21   Don't accumulate anything into the measure of the error
c  8    32   (A8=0) A8 = 0, turns off diagnostic output, =1 turns on
c  9    52   This category is no longer used.  It might be used later?
c 10    42   Use extra precision in accumulating Y.
c 11    81   Reserved (???)
c 12 10*LREVF+1 Reverse communication for computing f(t,y).
c 13 10*LREVF+1 Rev. comm. for computing f(t,y), no copy needed.
c 14 10*LREVO+1 Reverse communication for output.
c 15 10*(LHMAXU+100)+2  (A15=TS(3)-value in TS(1)) A15 gives hmax.
c 16 10*(LHMIN+100)+2  (A16=0) A16 gives hmin.
c 17 10*(LEPS+100)+2   (A17=mach_eps) Floating point precision.
c 18    81   Reserved (Use arc length as independent variable?)
c 19    81   Reserved (???)
c 20 10*KSTEPM+2 (A20) A20 = max number of steps between output points.
c 21    71   Return at end of every step.  (Sets IDAT(LOCEOS) =
c            IDAT(LOCEOS) - 2 if IDAT(LOCEOS) > -2)
c 22    62   (A22) Interpolate the solution at t = A22.
c 23    62   (A23) As option 22, except return value at the end of step.
c 24    63   (A24, B24)  A24 gives the increment between output points
c 25    72   (A25).  A25 gives the number of extrapolatory G-Stops.
c            This and option below, subtract 1 from IDAT(LOCEOS) if
c            IDAT(LOCEOS) is 0 or -2
c 26    72   (A26).  A26 gives the number of interpolatory G-Stops.
c 27 10*(LERTRY+100)+3  (A27=100, B27=4) Params. for error control
c 28 10*(LINTX+100)+2   (A28=.1E0) Fraction of step by which
c            interpolation can be outside an interval.
c KAT    Index giving the category.  See KACT above for categories.
c KRK??? Integer values for the parameter names starting with XRK....
c KSTATE Array indexed by KAT giving states in the processing of
c   options.  Usually KSTATE(KAT) is 0 for an option that is off and 1
c   for an option that is on.
c KUSE   When first checking options this accumulates the number of
c   times the option is set.  On the second pass this points to where
c   data defining the option is to be stored.  Indexed by KAT, the
c   category of the option.
c L      Temporary integer.
c LERMSG Array used to hold the locations of error messages.
c LEQ    Index of last equation for a group when processing options.
c LEQL   Array indexed by the category that give the value of LEQ the
c    last time that category was used.
c LLERR  In initialization gives LO on the last error control option.
c LO     Index in OPT in the initialization.
c LOCBF  F(i) at he start of a step is saved in DAT(LOCBF+I).
c MACT   Array used to hold error messaged actions.
c MACT1 -- MACT3  As for MACT.
c MAXLER MAXLER(-5) contains the largest index in DAT required for
c   storing absolute error tolerance vectors.
c MAXLOC 1 + Index of last location used in DAT for storing vectors used
c   for error control.  If no such vectors are used, this is 0.
c MEEMES Paramater used to flag the start of an error messaged.
c METEXT Parameter used to output text for an error messaged.
c MEFDAT Parameter giving used to set start of floating point data for
c     error message processor.
c MEFVEC Param. used to output floating pt. vector for error messages.
c MEIVEC Parameter used to output an integer vector for error messages.
c MENTXT Parameter giving the value to set index of next text to output.
c MERET  Parameter used to flag the end of actions for error messages.
c MINLER As for MAXLER, except for the smallest index in DAT that is
c   used.
c NEQ    Number of equations.
c NEQNOE Keeps count of number of equations with no error control.
c   Used for getting DAT(LERTRY)
c NEXT   Track next location available in IDAT when processing options.
c NIY    The number of components of y that are being interpolated for.
c TP     Temporary floating point quantity.
c TP1    Temporary floating point quantity.
c TP2   Temporary floating point quantity.
c XRK??? Used for parameter names of the values used to define options.
c WORK   Formal argument, see above.  The data required in WORK depends
c   on where we are in the process of taking a step.  Let Ki, i = 1, 2,
c   ...  denote the derivatives computed in the process of taking a
c   step, where K1 is the derivative at the beginning of the step, and
c   is stored in DAT(LOCBF+1:LOCBF+NEQ), LOCBF = LOCTY + NEQ.  (Ki) is
c   used to indicate that Ki is not used on the current stage, but will
c   be needed later.  Let y denote the solution at the beginning of the
c   step, y* the solution at the end of the step, and dy = y* - y.  Let
c   Si denote the state when computing the i-th y value of the step,
c   where the first y value is kept in DAT(LOCTY+1:LOCTY+NEQ).  Let dy
c   and err denote computations used to compute dy and the estimated
c   error made on the step.  Let I1, I2, I3, I4 be similarly defined for
c   the stages involved in computing the values necessary to do
c   interpolation.  Let "Next" denote what is needed in making the
c   transition to a new step.  Let err1 and err2 denote the 3rd and 5th
c   order error estimators, and "Int." denote space used to store data
c   needed for interpolation.  Finally let H1 and H2 denote the stages
c   involved in getting an initial stepsize.
c
c          WORK(*)   Ni = i*NEQ    Ni+ = Ni+1                   DAT
c        N0+: N1+: N2+: N3+: N4+: N5+: N6+: N7+: N8+: N9+: LOCTY+ LOCBF+
c        N1   N2   N3   N4   N5   N6   N7   N8   N9   N10
c  S2                                                        y      K1
c  S3    K2                                                  y      K1
c  S4         K3                                             y      K1
c  S5         K3   K4                                        y      K1
c  S6              K4   K5                                   y      K1
c  S7              K4   K5   K6                              y      K1
c  S8              K4   K5   K6   K7                         y      K1
c  S9              K4   K5   K6   K7   K8                    y      K1
c  S10             K4   K5   K6   K7   K8   K9               y      K1
c  S11             K4   K5   K6   K7   K8   K9   K10         y      K1
c  S12   K11       K4   K5   K6   K7   K8   K9   K10         y      K1
c  dy    K11  K12  K4   K5   K6   K7   K8   K9   K10        (y)     K1
c  errs  K11  K12            K6   K7   K8   K9   K10        (y)     K1
c  |err|           dy  err2                           err1  (y)
c  I1    K11  K12  y*   K13  K6   K7   K8   K9   K10  Int.   y      K1
c  I2    K11  K12 (y*)  K13  K6   K7   K8  (K9)  K14  Int.   y      K1
c  I3    K15      (y*)  K13  K6   K7   K8   K9   K14  Int.   y      K1
c  I4    K15      (y*)  K13  K16                 K14  Int.
c Next       [K12] y*   K13
c  H1    K1                                                  y     (K1)
c  H2    K2                                                  y      K1
c
c      When interpolating, WORK(1) = DAT(LINTX), and 3+8*NIY locations
c      beyond WORK(9*NEQ) are used to save information required for
c      doing the interpolation, where NIY is the number of components of
c      y that are interpolated for.  If in addition the G-Stop feature
c      is used an additional 3 locations are required.
c
c ******************** Variable Declarations ***************************
c
c Formal arguments
      integer IDAT(*)
      real             TS(*), Y(*), OPT(*), DAT(*), WORK(*)
c Parameters used for options
      real             XRKEND, XRKEQG, XRKAE, XRKRE1, XRKRE2, XRKAEV,
     1  XRKREV, XRKNOE, XRKDIG, XRKIN, XRKXP, XRKFR1, XRKFR2, XRKOR,
     2  XRKMAX, XRKMIN, XRKEPS, XRKMXS, XRKEOS, XRKTI, XRKTS, XRKTDT,
     3  XRKXG, XRKIG,XRKEC, XRKIFS
      parameter (XRKEND=0.E0, XRKEQG=1.E0, XRKAE=2.E0, XRKRE1=3.E0,
     1  XRKRE2=4.E0, XRKAEV=5.E0, XRKREV=6.E0, XRKNOE=7.E0,
     2  XRKDIG=8.E0, XRKIN=9.E0, XRKXP=10.E0, XRKFR1=12.E0,
     3  XRKFR2=13.E0, XRKOR=14.E0, XRKMAX=15.E0, XRKMIN=16.E0,
     4  XRKEPS=17.E0, XRKMXS=20.E0, XRKEOS=21.E0, XRKTI=22.E0,
     5  XRKTS=23.E0, XRKTDT=24.E0, XRKXG=25.E0, XRKIG=26.E0,
     6  XRKEC=27.E0, XRKIFS=28.E0)
c Integer values for these parameter used internally.
      integer KRKEND, KRKEQG, KRKAE, KRKRE1, KRKRE2, KRKAEV,
     1  KRKREV, KRKNOE, KRKDIG, KRKIN, KRKXP, KRKFR1, KRKFR2, KRKOR,
     2  KRKMAX, KRKMIN, KRKEPS, KRKMXS, KRKEOS, KRKTI, KRKTS, KRKTDT,
     3  KRKXG, KRKIG, KRKEC, KRKIFS
      parameter (KRKEND=0, KRKEQG=1, KRKAE=2, KRKRE1=3, KRKRE2=4,
     1  KRKAEV=5, KRKREV=6, KRKNOE=7, KRKDIG=8, KRKIN=9, KRKXP=10,
     2  KRKFR1=12, KRKFR2=13, KRKOR=14, KRKMAX=15, KRKMIN=16,
     3  KRKEPS=17, KRKMXS=20, KRKEOS=21, KRKTI=22, KRKTS=23, KRKTDT=24,
     4  KRKXG=25, KRKIG=26, KRKEC=27, KRKIFS=28)
c
c Parameters used to reference DAT.
c    Sorted by value:
c      1   LEPS
c      2   LHMAXU
c      3   LHMIN
c      4   LOCTGS
c      5   LTOUT
c      6   NEGPOS
c      7   NEXTH
c      8   NEXTT
c      9   LINTX
c     10   LSAVEH
c     11   HMAX
c     12   LAH
c     13   LASTE3
c     14   LERTRY (Two locations)
c     16   LERR  (Three locations)
c     19   PHI
c     20   HLAST
c     21   Not used
c     22   HA
c     23   LOCTXG
c     24   LPHI
c     25   HCAU
c     26   LOCSAV
c     27-29 Not used
c     30   LOCTY
      integer HA, LASTE3, LEPS, LERR, HMAX, LERTRY, LAH,
     1  HCAU, LHMAXU, LHMIN, LINTX, LOCTGS, LOCSAV, LOCTY, LOCTXG,
     2  LPHI, LSAVEH, LTOUT, NEGPOS, NEXTH, NEXTT, PHI, HLAST
      parameter (HA=22, LASTE3=13, LEPS=1, LERR=16,
     1  HMAX=11, LERTRY=14, LAH=12, HCAU=25, LHMAXU=2, LHMIN=3,
     2  LINTX=9, LOCTGS=4, LOCSAV=26, LOCTY=30, LOCTXG=23, LPHI=24,
     3  LSAVEH=10, LTOUT=5, HLAST=20, NEGPOS=6, NEXTH=7, NEXTT=8,
     4  PHI=19)
c
c Parameters used to reference IDAT.  (Lots flagged as availble.)
c    Sorted by value:
c     10   LINT
c     11   LOCEPR
c     12   LOCXP
c     13   LBF
c     14   LOCOUT
c     15   LSTLAS
c     16   NXGS
c     17   NTGS
c     18   LOCEOS
c     19   KEOS
c     20   KSTIFF
c     21   KDZERO
c     22   Available
c     23   LREVF
c     24   LREVO
c     25   Available
c     26   NERRSV
c     27   KSTEP
c     28   KFAIL
c     29   Not used
c     30   KHMIN
c     31   NGLO
c     32   NGHI  (Also LAST0)
c     33   Available
c     34   KSTEPM
c     35   Used along with KSTEPM
c     36   KSTEPX
c     37   Available
c     38   LOCINT
c     39   NUMINT
c     40   LWHYF
c     41   INTSAV
c     42   LTFIN
c     44   LOCERR  (Must be last.)
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
      integer LAST0
      parameter (LAST0=NGHI)
c Local variables
      logical GOTINT
      integer I, IBEG, II, ITEST, J, K, KACT(0:28), KAT,
     1  KSTATE(2:6), KUSE(2:6), KWORK, L, LDAT, LERR1, LEQ, LEQL(2:6),
     2  LIDAT, LLERR, LO, LWORK, MAXLER, MINLER, N, NEQ, NEQNOE, NEXT
      external R1MACH
      real             R1MACH, ERR1, ERR2, DIFFTS
c Parameters for setting value in KACT.
      integer KACT12, KACT13, KACT14, KACT15, KACT16, KACT17, KACT20,
     1  KACT27, KACT28
      parameter (KACT12=10*LREVF+1,        KACT13=10*LREVF+1,
     1  KACT14=10*LREVO+1,        KACT15=10*(LHMAXU+100)+2,
     2  KACT16=10*(LHMIN+100)+2,  KACT17=10*(LEPS+100)+2,
     3  KACT20=10*KSTEPM+2,       KACT27=10*(LERTRY+100)+3,
     4  KACT28=10*(LINTX+100)+2)
c
c Parameters for error messages
      integer MENTXT, MECONT, MERET, MEEMES, METEXT, MEIVEC, MEFVEC
      parameter (MENTXT=23, MECONT= 50, MERET=51, MEEMES=52, METEXT=53,
     1  MEIVEC=57, MEFVEC=61)
c Other Stuff for error processing
      integer IERR, IERROR(10), LERMSG(10), MACT(5), MACTV(7)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA SXRK8$B
cAB Undefined option.$E
cAC Equation indexes for groups are out of order or out of range.$E
cAD Option required inside equation group is outside the last group.$E
cAE Option specification made twice in the same group..$E
cAF Error tolerance vectors overlap in DAT.$E
cAG Options must be turned on with 1.E0 and off with 0.E0, you $C
c   used $F.$E
cAH IDAT(5) must be at least $I.$E
cAI Could not find space in DAT for all options.  IDAT(6) must be $C
c   > $I.$E
cAJ IDAT(7) must be at least $I.$E
cAK The paramters used in option 27, must have the first at least 4 $C
c   times bigger than the second.$E
c   $
cAL OPT up to the point where we are seeing problems.$N
c   OPT: $B
cAM IDAT(4:7):$B
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM
      parameter (LTXTAA=  1,LTXTAB=  8,LTXTAC= 27,LTXTAD= 90,LTXTAE=156,
     * LTXTAF=209,LTXTAG=250,LTXTAH=319,LTXTAI=349,LTXTAJ=418,
     * LTXTAK=448,LTXTAL=  1,LTXTAM= 59)
      character MTXTAA(3) * (181)
      character MTXTAB(1) * (70)
      data MTXTAA/'SXRK8$BUndefined option.$EEquation indexes for groups
     * are out of order or out of range.$EOption required inside equatio
     *n group is outside the last group.$EOption specification made ','t
     *wice in the same group..$EError tolerance vectors overlap in DAT.$
     *EOptions must be turned on with 1.E0 and off with 0.E0, you used $
     *F.$EIDAT(5) must be at least $I.$ECould not find',' space in DAT f
     *or all options.  IDAT(6) must be > $I.$EIDAT(7) must be at least $
     *I.$EThe paramters used in option 27, must have the first at least$
     * 4 times bigger than the second.$E'/
      data MTXTAB/'OPT up to the point where we are seeing problems.$NOP
     *T: $BIDAT(4:7):$B'/
c **** End of automatically generated text
c                     1   2  3  4       5
      data MACT /MEEMES, 47, 0, 0, MECONT /
c                       1       2  3       4       5  6      7
      data MACTV / METEXT, MEFVEC, 0, METEXT, MEIVEC, 4, MERET /
c                        1       2       3       4      5        6
      data LERMSG / LTXTAB, LTXTAC, LTXTAD, LTXTAE, LTXTAF, LTXTAG,
     1  LTXTAH, LTXTAI, LTXTAJ, LTXTAK /
c            7       8       9      10
c                    1   2   3   4   5   6   7   8   9  10
      data IERROR / 14, 11, 21, 13, 17, 26, 18, 19, 20, 27 /
c
c                 0   1   2   3   4   5   6   7   8   9  10  11      12
      data KACT / 1, 12, 22, 23, 23, 22, 22, 21, 32, 81, 42, 81, KACT12,
     1  KACT13, KACT14, KACT15, KACT16 , KACT17, 81, 81, KACT20, 71, 62,
     2  62, 63, 72, 72, KACT27, KACT28 /
c
c ********************** Start of Executable Code **********************
c
      if (IDAT(1) .ne. 0) go to 600
      NEQ = IDAT(4)
      LIDAT = abs(IDAT(5))
      LDAT = abs(IDAT(6))
      LWORK = abs(IDAT(7))
      DAT(LEPS) = R1MACH(4)
c      Set  IDAT(2) to signal no errors.
      IDAT(2) = 0
c         Initialize IDAT and DAT.
      do  20 I = LOCEPR, LAST0
        IDAT(I) = 0
 20   continue
      DIFFTS = TS(3) - TS(1)
      DAT(NEGPOS) = SIGN(1.E0, DIFFTS)
      DAT(LHMAXU) = abs(DIFFTS)
      DAT(LHMIN) = 0.E0
      IDAT(KFAIL) = 0
      IDAT(KSTEP) = 0
      IDAT(KSTEPM) = 100000
c
c ****************** Process options, First phase **********************
c
c Processing is done in two phases.  First phase checks for valid input
c and determines how much space in needed for internal processing of the
c options.  The second phase stores data for later processing of an
c option.
c
      GOTINT = .false.
      do 40  I = 2, 6
c Indexes, I, here, correspond to the categories mentioned above.
        KUSE(I) = 0
        LEQL(I) = 0
        KSTATE(I) = 0
 40   continue
c Need a special flag for extra precision so we know to turn it on.
      LEQL(4) = -1
      KUSE(6) = 2
c Need to know if no error tolerances have been set.
      KSTATE(2) = -1
      IBEG = LOCTY + 2*NEQ + 1
      LERR1 = 2
      MINLER = 0
      MAXLER = IBEG - 1
c             Flags that no extra precision is used.
      IDAT(LOCXP) = -1
      LEQ = -2
      LO = 1
      L = 0
 100  LO = LO + L
      K = nint(OPT(LO))
      if ((K .lt. 0) .or. (K .gt. KRKIFS)) then
c                       An undefined option
        IERR = 1
        go to 700
      end if
      J = KACT(K)
c            Category of the option.
      KAT = J / 10
c            Space required by the option.
      L = J - 10 * KAT
      if (KAT .le. 5) then
        if (KAT .le. 1) then
c                              Reached end of a group
          I = LEQ
          if (KAT .eq. 0) then
c                              End of all options
            LEQ = NEQ + 1
            go to 130
          else
c                Set LEQ = index of equation for end of a group.
            LEQ = nint(OPT(LO+1))
          end if
          if ((LEQ .gt. I) .and. (LEQ .le. NEQ)) go to 100
c                Equation indexes not strictly increasing or too big.
          IERR = 2
          go to 700
        else
c    Option needing extra space depending on when option 1 is specified.
          if (LEQ .gt. NEQ) then
c                Options can not be specified after the last group.
            IERR = 3
            go to 700
          end if
          if (LEQL(KAT) .eq. LEQ) then
c                Two specifications for item in a group.
            IERR = 4
            go to 700
          end if
          if (KAT .eq. 2) go to 130
          J = nint(OPT(LO+1))
          if ((J .ne. 0) .and. (J .ne. 1)) then
            WORK(1) = OPT(LO+1)
            IERR = 6
            go to 700
          end if
          if (KSTATE(KAT) .ne. J) then
            if (KAT .eq. 4) then
c    Get total space in DAT that is needed into IDAT(LOCXP)
              if (J .eq. 1) then
                if (LEQL(4) .le. 0) then
                  IDAT(LOCXP) = abs(IDAT(LOCXP)) + NEQ
                else
                  IDAT(LOCXP) = abs(IDAT(LOCXP)) + NEQ - LEQ + 1
                end if
              else if (LEQL(4) .gt. 0) then
                IDAT(LOCXP) = IDAT(LOCXP) - NEQ + LEQ
              end if
            end if
            KSTATE(KAT) = J
            if (J .eq. 1) then
              KUSE(KAT) = KUSE(KAT) + 2
            else if ((KAT .eq. 5) .and. (LEQ .le. 0)) then
              KUSE(5) = KUSE(5) - 2
            end if
          end if
          LEQL(KAT) = LEQ
          go to 100
        end if
      else if (KAT .le. 7) then
c  Interp. point, no interp. needed if end of step value is returned.
        if ((K .ne. KRKTS) .and. (K .ne. KRKEOS)) GOTINT = .true.
        if (KAT .eq. 6) KUSE(6) = KUSE(6) + 2
      else if (KAT .eq. 8) then
        IERR = 1
        go to 700
      else if (KAT .gt. 10) then
        if (KAT .lt. 100) then
c                     Data to store in IDAT.
          if (L .eq. 1) then
            IDAT(KAT) = 1
            if (K .eq. KRKFR2) IDAT(KAT) = -1
          else
            IDAT(KAT) = nint(OPT(LO+1))
          end if
        else
c                      Data to store in DAT.
          DAT(KAT - 100) = OPT(LO+1)
          if (L .eq. 3) then
            DAT(KAT - 99) = OPT(LO+2)
            if (DAT(KAT-100) .lt. 4.E0 * DAT(KAT-99)) then
              IERR = 10
              go to 700
            end if
          end if
        end if
      end if
      go to 100
c
c                    For error control
c  End here if processing for errors is not finished.
 130  if (LEQL(2) .gt. 0) then
        J = nint(OPT(LLERR))
        if ((J .eq. KRKAEV) .or. (J .eq. KRKREV)) then
c                Track space used by vectors for error control
          I = nint(OPT(LLERR+1))
          if (I .gt. MAXLER) then
            MAXLER = I + LEQ - LEQL(2) - 1
            if (MINLER .eq. 0) then
              MINLER = I
            end if
            if (MAXLER .le. LDAT) go to 140
          end if
c Problem with space for error vectors.
          IERR = 5
          go to 700
        end if
      end if
 140  if (KAT .ne. 0) then
        if (LEQ .eq. 1) LERR1 = 1
        LEQL(2) = max(LEQ, 1)
        KUSE(2) = KUSE(2) + 3
        LLERR = LO
        LERR1 = 1
        go to 100
      end if
      if (LERR1 .ne. 1) KUSE(2) = KUSE(2) + 3
c
c Set up starting locations for the second pass
c                      Set start of available space in DAT and WORK
      KWORK = 10 * NEQ
c                      Can't use space beyond DAT(ITEST).
      ITEST = LDAT
      if (MINLER .ne. 0) ITEST = MINLER - 1
c                      Set pointers for internal groups.
c             Space for error control.
      if (KUSE(2) .eq. 0) KUSE(2) = 3
      NEXT = LOCERR + KUSE(2)
      KUSE(2) = LOCERR
      if (KUSE(3) .ne. 0) then
        IDAT(LOCEPR) = NEXT
c             Space for diagnostic print.
        NEXT = NEXT + KUSE(3) + 1
        KUSE(3) = IDAT(LOCEPR)
c   Set first location so we can recognize an off prior to the first on.
        IDAT(KUSE(3)) = 0
      end if
      if (IDAT(LOCXP) .ge. 0) then
c             K is the amount of space needed in DAT
        K = IDAT(LOCXP)
        IDAT(LOCXP) = NEXT
c              Space for extra precision specifications.
        NEXT = NEXT + KUSE(4) + 2
        KUSE(4) = IDAT(LOCXP) + 1

        if (K .le. ITEST - IBEG) then
          J = IBEG
          IBEG = IBEG + K
        else if (K .le. LDAT - MAXLER) then
          J = MAXLER + 1
          MAXLER = MAXLER + J
        else
          go to 640
        end if
c         Set extra precision parts to 0.
        IDAT(KUSE(4)-1) = J
        do 170 I = J, J + K - 1
          DAT(I) = 0.E0
 170    continue
      else
        IDAT(LOCXP) = 0
      end if
c        Remember the initial start for the list of output points
      IDAT(INTSAV) = NEXT
c        And the current start for the above list
      IDAT(LOCOUT) = NEXT
c              Space for remembering output points.
      NEXT = NEXT + KUSE(6)
      KUSE(6) = IDAT(LOCOUT)
      if (NEXT .gt. LIDAT) then
c                    Error -- Dimension of IDAT is too small
        IERR = 7
        IDAT(1) = NEXT - 1
        go to 700
      end if
      if (IDAT(5) .lt. 0) IDAT(5) = NEXT
c
c ****** Process the options -- Second phase -- Space looks o.k. *******
c
      do 200  I = 2, 6
c Indexes, I, here, correspond to the categories mentioned above.
        LEQL(I) = 0
        KSTATE(I) = 0
 200  continue
c Need a special flag for extra precision so we know to turn it on.
      LEQL(4) = -1
      NEQNOE = 0
      LEQ = 0
      LO = 1
      L = 0
 300  LO = LO + L
      K = nint(OPT(LO))
      J = KACT(K)
c            Category of the option.
      KAT = J / 10
c            Space required by the option.
      L = J - 10 * KAT
      if (KAT .le. 5) then
        if (KAT .le. 1) then
c         Reached end of a group and if KAT is 0, end of options.
          if (KAT .eq. 0) then
            if (LERR1 .eq. 1) go to 400
c         If no error control, we need to insert the default.
            LERR1 = 1
            KAT = 2
            L = 3
            K = 0
            LEQ = NEQ + 1
          else
            LEQ = nint(OPT(LO+1))
            go to 300
          end if
        end if
        if (KAT .eq. 2) then
 320      J = KUSE(2)
          if (KSTATE(2) .ne. 0) then
            IDAT(J-2) = LEQ - 1
          else
            if (LEQ .gt. 1) then
c                 Default actions for this group
              II= KRKRE1
              ERR1 = DAT(LEPS)**.75E0
              ERR2 = ERR1
              N = 2
              go to 330
            end if
            KSTATE(2) = 1
          end if
          II = K
          if (K .ge. KRKAEV) then
c                Want 1 less to have a base address
            IDAT(J+2) = nint(OPT(LO+1)) - 1
            if (K .eq. KRKNOE) NEQNOE = 1
            go to 340
          end if
          N = L - 1
          ERR1 = OPT(LO+1)
          ERR2 = OPT(LO+2)
c                 Getting space in DAT to store data.
 330      if (IBEG + N .gt. ITEST) then
            if (ITEST .eq. LDAT) go to 640
            ITEST = LDAT
            IBEG = MAXLER + 1
            go to 330
          end if
          IDAT(J+2) = IBEG
          DAT(IBEG) = ERR1
          if (N .eq. 2) DAT(IBEG+1) = ERR2
          IBEG = IBEG + N
c              Get here for vector case or no error control
 340      continue
          IDAT(J) = II
          IDAT(J+1) = NEQ
          KUSE(2) = KUSE(2) + 3
          if (KSTATE(2) .ne. 0) go to 300
c If we had to set the defaults, we still need to do the current.
          KSTATE(2) = 1
          if (K .eq. 0) go to 400
          go to 320
        else
c                Options we can turn on or off.
          II = nint(OPT(LO+1))
          if (KSTATE(KAT) .ne. II) then
            if (II .eq. 0) then
c                 We are turning this one off
              IDAT(KUSE(KAT)-1) = LEQ - 1
            else
c Turning this one on.  Last is set to NEQ, reset if turned off.
              IDAT(KUSE(KAT)) = max(1, LEQ)
              IDAT(KUSE(KAT)+1) = NEQ
              LEQL(KAT) = LEQ
              KUSE(KAT) = KUSE(KAT) + 2
            end if
            KSTATE(KAT) = II
          end if
          go to 300
        end if
      end if
      if (KAT .eq. 6) then
c            Save data for tracking output points
        N = L - 1
 360    if (IBEG + N - 1 .gt. ITEST) then
          if (ITEST .ge. LDAT) go to 640
          ITEST = LDAT
          IBEG = MAXLER + 1
          go to 360
        end if
        IDAT(KUSE(6)) = K
        IDAT(KUSE(6)+1) = IBEG
        KUSE(6) = KUSE(6) + 2
        DAT(IBEG) = OPT(LO+1)
        if (N .eq. 2) DAT(IBEG+1) = OPT(LO+2)
        IBEG = IBEG + N
        go to 300
      else if (KAT .eq. 7) then
        if (K .eq. KRKEOS) then
c  Return at end of every step. (0 => -2, -1 => -3)
          if (IDAT(LOCEOS) .gt. -2) IDAT(LOCEOS) = IDAT(LOCEOS) - 2
        else
c  G_Stop of some type (-2 => -3, 0 => -1)
          if (abs(IDAT(LOCEOS)+1).eq.1) IDAT(LOCEOS) = IDAT(LOCEOS)-1
c  Save the count in the proper location
          IDAT(NXGS + K - KRKXG) = nint(OPT(LO+1))
          GOTINT = .true.
        end if
      end if
      go to 300
c
c ******************* Set final termination flags, Etc. ****************
c
c  Finish up list of output points
 400  IDAT(KUSE(6)) = 30
      IDAT(KUSE(6)+1) = 0
      IDAT(LTFIN) = KUSE(6)
c                      Set final 0 for diagnostic output
      if (IDAT(LOCEPR) .ne. 0) IDAT(KUSE(3)) = 0
c                      Get space in WORK used for interpolation
      if (GOTINT) then
        KWORK = KWORK + 7*NEQ + 6
        IDAT(NTGS) = IDAT(NXGS) + IDAT(NTGS)
        if (IDAT(NTGS) .ne. 0) then
          IDAT(NXGS) = IDAT(NXGS) + 3
          IDAT(NTGS) = IDAT(NTGS) + 3
        end if
      end if
      IDAT(NUMINT) = -NEQ
c
      if (IDAT(LOCXP) .ne. 0)  then
        IDAT(KUSE(4)) = NEQ
      end if
c Set IBEG to the space actually needed (instead of next available)
      IBEG = IBEG - 1
      if (KWORK .gt. LWORK) then
        IERR = 9
        IDAT(1) = KWORK
        go to 700
      end if
c  If requested, tell user about space needed.  (IDAT(5) is above.)
      if (IDAT(6) .lt. 01) then
        IDAT(6) = IBEG
      end if
      if (IDAT(7) .lt. 0) then
        IDAT(7) = KWORK
      end if
c             Initialize some stuff that depends on user input.
      IDAT(LOCINT) = 9*NEQ + 7
      IDAT(KSTEPM+1) = IDAT(KSTEPM)
      IDAT(KSTEPX) = IDAT(KSTEPM)
      IDAT(LBF) = NEQ + LOCTY
      DAT(HCAU) = DAT(LHMAXU)
      DAT(HMAX) = DAT(LHMAXU)
      if (NEQNOE .ne. 0) then
        NEQNOE = 0
        do 530 I = IDAT(LOCERR), 1000000, 3
          if (IDAT(I+1) .ge. NEQ) go to 540
          if (IDAT(I) .eq. KRKNOE) then
            NEQNOE = NEQNOE + IDAT(I+4) - IDAT(I+1)
          end if
 530    continue
 540    continue
      end if
      DAT(LERTRY) = 0.E0
      if (NEQ .ne. NEQNOE) then
        DAT(LERTRY) = 100.E0 / real(NEQ - NEQNOE)
      end if
      DAT(LERTRY+1) = 6.E0
c             Set count on derivative evaluations.
c
c **************************** Integrate *******************************
c
 600  call SXRK8A(TS, Y, OPT, IDAT, DAT, WORK)
      return
c
c *********************** Error Processing *****************************
c
c   Entry when insufficient space in DAT
 640  IERR = 8
      IDAT(1) = IBEG
 700  MACT(3) = IERROR(IERR)
      MACT(4) = LERMSG(IERR)
      call SMESS(MACT, MTXTAA, IDAT, WORK)
      MACTV(3) = LO + L
      call SMESS(MACTV, MTXTAB, IDAT(4), OPT)
      IDAT(1) = 4
      IDAT(2) = MACT(3)
      return
      end

      subroutine SXRK8A(TS, Y, F, IDAT, DAT, WORK)
c Main action routine for solving differential equations with the
c Dormand-Prince 8th Order Runge_Kutta formulas.
c
c ************************* Variable Declarations **********************
c
c Formal arguments
      integer IDAT(*)
      real             TS(3), Y(*), F(*), DAT(*), WORK(*)
c
c Parameters used to reference DAT
      integer HA, LASTE3, LEPS, LERR, HMAX, LERTRY, LAH,
     1  HCAU, LHMAXU, LHMIN, LINTX, LOCTGS, LOCSAV, LOCTY, LOCTXG,
     2  LPHI, LSAVEH, LTOUT, NEGPOS, NEXTH, NEXTT, PHI, HLAST
      parameter (HA=22, LASTE3=13, LEPS=1, LERR=16,
     1  HMAX=11, LERTRY=14, LAH=12, HCAU=25, LHMAXU=2, LHMIN=3,
     2  LINTX=9, LOCTGS=4, LOCSAV=26, LOCTY=30, LOCTXG=23, LPHI=24,
     3  LSAVEH=10, LTOUT=5, HLAST=20, NEGPOS=6, NEXTH=7, NEXTT=8,
     4  PHI=19)
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
c Parameters used in exponential averaging for the stepsize adjustment.
      real             P, P5PINV, PINV, W
      parameter (P=8.E0, PINV=1.E0/P, P5PINV=.5E0*PINV, W=.1E0)
      real             WI1A, WI1B, WI2A, WI2B
      parameter (WI1A=W/(1.E0-W)**2)
      parameter (WI1B=(1.E0-2.E0*W)/(1.E0-W)**2)
      parameter (WI2A=(2.E0*W)/(1.E0-W)**3)
      parameter (WI2B=(1.E0-3.E0*W)/(1.E0-W)**3)
c      parameter (WI3A=(3.E0*W)/(1.E0-W)**4)
c      parameter (WI3B=(1.E0-4.E0*W)/(1.E0-W)**4)
c      real              WQ1A,WQ2A,WQ3A,WQ1B,WQ2B,WQ3B,WQ1C,WQ2C,WQ3C
c      parameter(WQ1A=(1.E0-W)*(W**2+W+1.E0)/W**2)
c      parameter(WQ2A=(1.E0-W)*(W**2+W-2.E0)/W**2)
c      parameter(WQ3A=(1.E0-W)**3/W**2)
c      parameter(WQ1B=(1.E0-W)**2*(-W-2.E0)/W**2)
c      parameter(WQ2B=(1.E0-W)**2*(-W**2-3.E0*W+4.E0)/W**2)
c      parameter(WQ3B=-2.E0 * (1.E0-W)**4/W**2)
c      parameter(WQ1C=((1.E0-W)**3)/W**2)
c      parameter(WQ2C=(-2.E0*(1.E0-W)**4)/W**2)
c      parameter(WQ3C=((1.E0-W)**5)/W**2)

      real             WL1A, WL2A
      parameter (WL1A=(1.E0-W**2)/W, WL2A=-(1.E0-W)**2/W)
c      parameter (WL1B=-(1.E0-W)**2/W, WL2B=(1.E0-W)**3/W)

c      real             AQ, BQ, CQ, AL, BL
c These don't need to be in DAT as we only need them on rejected steps.
      save ERR, EN
      real             AH

c Local Variables
      integer I, I1, J, K, LOCBF, NEQ
      real             EN(2), ERR, H, TP, TP1, TP2
      external SXRK8X
      real             SXRK8X
c
c Integration coefficients
      real             C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C14,
     1  C15, C16, B1, B6, B7, B8, B9, B10, B11, B12, BHH1, BHH2, BHH3,
     2  ER1, ER6, ER7, ER8, ER9, ER10, ER11, ER12, A21, A31, A32, A41,
     3  A43, A51, A53, A54, A61, A64, A65, A71, A74, A75, A76, A81,
     4  A84, A85, A86, A87, A91, A94, A95, A96, A97, A98, A101, A104,
     5  A105, A106, A107, A108, A109, A111, A114, A115, A116, A117,
     6  A118, A119, A1110, A121, A124, A125, A126, A127, A128, A129,
     7  A1210, A1211, A141, A147, A148, A149, A1410, A1411, A1412,
     8  A1413, A151, A156, A157, A158, A1511, A1512, A1513, A1514,
     9  A161, A166, A167, A168, A169, A1613, A1614, A1615, D41, D46,
     A  D47, D48, D49, D410, D411, D412, D413, D414, D415, D416, D51,
     B  D56, D57, D58, D59, D510, D511, D512, D513, D514, D515, D516,
     C  D61, D66, D67, D68, D69, D610, D611, D612, D613, D614, D615,
     D  D616, D71, D76, D77, D78, D79, D710, D711, D712, D713, D714,
     E  D715, D716
      parameter ( C2  = 0.526001519587677318785587544488E-01,
     1  C3  = 0.789002279381515978178381316732E-01,
     2  C4  = 0.118350341907227396726757197510E+00,
     3  C5  = 0.281649658092772603273242802490E+00,
     4  C6  = 0.333333333333333333333333333333E+00,
     5  C7  = 0.25E+00,
     6  C8  = 0.307692307692307692307692307692E+00,
     7  C9  = 0.651282051282051282051282051282E+00,
     8  C10 = 0.6E+00,
     9  C11 = 0.857142857142857142857142857142E+00,
     A  C14 = 0.1E+00,
     B  C15 = 0.2E+00,
     C  C16 = 0.777777777777777777777777777778E+00 )
      parameter ( B1 =   5.42937341165687622380535766363E-2,
     1  B6 =   4.45031289275240888144113950566E0,
     2  B7 =   1.89151789931450038304281599044E0,
     3  B8 =  -5.8012039600105847814672114227E0,
     4  B9 =   3.1116436695781989440891606237E-1,
     5  B10 = -1.52160949662516078556178806805E-1,
     6  B11 =  2.01365400804030348374776537501E-1,
     7  B12 =  4.47106157277725905176885569043E-2 )
      parameter ( BHH1 = 0.244094488188976377952755905512E+00,
     1  BHH2 = 0.733846688281611857341361741547E+00,
     2  BHH3 = 0.220588235294117647058823529412E-01 )
      parameter ( ER1 =  0.1312004499419488073250102996E-01,
     1  ER6  = -0.1225156446376204440720569753E+01,
     2  ER7  = -0.4957589496572501915214079952E+00,
     3  ER8  =  0.1664377182454986536961530415E+01,
     4  ER9  = -0.3503288487499736816886487290E+00,
     5  ER10 =  0.3341791187130174790297318841E+00,
     6  ER11 =  0.8192320648511571246570742613E-01,
     7  ER12 = -0.2235530786388629525884427845E-01 )
      parameter ( A21 =    5.26001519587677318785587544488E-2,
     1  A31 =    1.97250569845378994544595329183E-2,
     2  A32 =    5.91751709536136983633785987549E-2,
     3  A41 =    2.95875854768068491816892993775E-2,
     4  A43 =    8.87627564304205475450678981324E-2,
     5  A51 =    2.41365134159266685502369798665E-1,
     6  A53 =   -8.84549479328286085344864962717E-1,
     7  A54 =    9.24834003261792003115737966543E-1,
     8  A61 =    3.7037037037037037037037037037E-2,
     9  A64 =    1.70828608729473871279604482173E-1,
     A  A65 =    1.25467687566822425016691814123E-1,
     B  A71 =    3.7109375E-2,
     C  A74 =    1.70252211019544039314978060272E-1,
     D  A75 =    6.02165389804559606850219397283E-2,
     E  A76 =   -1.7578125E-2 )
      parameter ( A81 =    3.70920001185047927108779319836E-2,
     1  A84 =    1.70383925712239993810214054705E-1,
     2  A85 =    1.07262030446373284651809199168E-1,
     3  A86 =   -1.53194377486244017527936158236E-2,
     4  A87 =    8.27378916381402288758473766002E-3,
     5  A91 =    6.24110958716075717114429577812E-1,
     6  A94 =   -3.36089262944694129406857109825E0,
     7  A95 =   -8.68219346841726006818189891453E-1,
     8  A96 =    2.75920996994467083049415600797E1,
     9  A97 =    2.01540675504778934086186788979E1,
     A  A98 =   -4.34898841810699588477366255144E1,
     B  A101 =   4.77662536438264365890433908527E-1,
     C  A104 =  -2.48811461997166764192642586468E0,
     D  A105 =  -5.90290826836842996371446475743E-1,
     E  A106 =   2.12300514481811942347288949897E1,
     F  A107 =   1.52792336328824235832596922938E1,
     I  A108 =  -3.32882109689848629194453265587E1,
     J  A109 =  -2.03312017085086261358222928593E-2 )
      parameter ( A111 =  -9.3714243008598732571704021658E-1,
     1  A114 =   5.18637242884406370830023853209E0,
     2  A115 =   1.09143734899672957818500254654E0,
     3  A116 =  -8.14978701074692612513997267357E0,
     4  A117 =  -1.85200656599969598641566180701E1,
     5  A118 =   2.27394870993505042818970056734E1,
     6  A119 =   2.49360555267965238987089396762E0,
     7  A1110 = -3.0467644718982195003823669022E0,
     8  A121 =   2.27331014751653820792359768449E0,
     9  A124 =  -1.05344954667372501984066689879E1,
     A  A125 =  -2.00087205822486249909675718444E0,
     B  A126 =  -1.79589318631187989172765950534E1,
     C  A127 =   2.79488845294199600508499808837E1,
     D  A128 =  -2.85899827713502369474065508674E0,
     E  A129 =  -8.87285693353062954433549289258E0,
     F  A1210 =  1.23605671757943030647266201528E1,
     G  A1211 =  6.43392746015763530355970484046E-1 )
      parameter ( A141 =  5.61675022830479523392909219681E-2,
     1  A147 =  2.53500210216624811088794765333E-1,
     2  A148 = -2.46239037470802489917441475441E-1,
     3  A149 = -1.24191423263816360469010140626E-1,
     4  A1410 =  1.5329179827876569731206322685E-1,
     5  A1411 =  8.20105229563468988491666602057E-3,
     6  A1412 =  7.56789766054569976138603589584E-3,
     7  A1413 = -8.298E-3 )
      parameter ( A151 =  3.18346481635021405060768473261E-2,
     1  A156 =  2.83009096723667755288322961402E-2,
     2  A157 =  5.35419883074385676223797384372E-2,
     3  A158 = -5.49237485713909884646569340306E-2,
     4  A1511 = -1.08347328697249322858509316994E-4,
     5  A1512 =  3.82571090835658412954920192323E-4,
     6  A1513 = -3.40465008687404560802977114492E-4,
     7  A1514 =  1.41312443674632500278074618366E-1,
     8  A161 = -4.28896301583791923408573538692E-1,
     9  A166 = -4.69762141536116384314449447206E0,
     A  A167 =  7.68342119606259904184240953878E0,
     B  A168 =  4.06898981839711007970213554331E0,
     C  A169 =  3.56727187455281109270669543021E-1,
     D  A1613 = -1.39902416515901462129418009734E-3,
     E  A1614 =  2.9475147891527723389556272149E0,
     F  A1615 = -9.15095847217987001081870187138E0 )
      parameter ( D41  = -0.84289382761090128651353491142E+01,
     1  D46  =  0.56671495351937776962531783590E+00,
     2  D47  = -0.30689499459498916912797304727E+01,
     3  D48  =  0.23846676565120698287728149680E+01,
     4  D49  =  0.21170345824450282767155149946E+01,
     5  D410 = -0.87139158377797299206789907490E+00,
     6  D411 =  0.22404374302607882758541771650E+01,
     7  D412 =  0.63157877876946881815570249290E+00,
     8  D413 = -0.88990336451333310820698117400E-01,
     9  D414 =  0.18148505520854727256656404962E+02,
     A  D415 = -0.91946323924783554000451984436E+01,
     B  D416 = -0.44360363875948939664310572000E+01)
      parameter ( D51  =  0.10427508642579134603413151009E+02,
     1  D56  =  0.24228349177525818288430175319E+03,
     2  D57  =  0.16520045171727028198505394887E+03,
     3  D58  = -0.37454675472269020279518312152E+03,
     4  D59  = -0.22113666853125306036270938578E+02,
     5  D510 =  0.77334326684722638389603898808E+01,
     6  D511 = -0.30674084731089398182061213626E+02,
     7  D512 = -0.93321305264302278729567221706E+01,
     8  D513 =  0.15697238121770843886131091075E+02,
     9  D514 = -0.31139403219565177677282850411E+02,
     A  D515 = -0.93529243588444783865713862664E+01,
     B  D516 =  0.35816841486394083752465898540E+02)
      parameter ( D61 =  0.19985053242002433820987653617E+02,
     1  D66 = -0.38703730874935176555105901742E+03,
     2  D67 = -0.18917813819516756882830838328E+03,
     3  D68 =  0.52780815920542364900561016686E+03,
     4  D69 = -0.11573902539959630126141871134E+02,
     5  D610 =  0.68812326946963000169666922661E+01,
     6  D611 = -0.10006050966910838403183860980E+01,
     7  D612 =  0.77771377980534432092869265740E+00,
     8  D613 = -0.27782057523535084065932004339E+01,
     9  D614 = -0.60196695231264120758267380846E+02,
     A  D615 =  0.84320405506677161018159903784E+02,
     B  D616 =  0.11992291136182789328035130030E+02 )
      parameter ( D71  = -0.25693933462703749003312586129E+02,
     1  D76  = -0.15418974869023643374053993627E+03,
     2  D77  = -0.23152937917604549567536039109E+03,
     3  D78  =  0.35763911791061412378285349910E+03,
     4  D79  =  0.93405324183624310003907691704E+02,
     5  D710 = -0.37458323136451633156875139351E+02,
     6  D711 =  0.10409964950896230045147246184E+03,
     7  D712 =  0.29840293426660503123344363579E+02,
     8  D713 = -0.43533456590011143754432175058E+02,
     9  D714 =  0.96324553959188282948394950600E+02,
     A  D715 = -0.39177261675615439165231486172E+02,
     B  D716 = -0.14972683625798562581422125276E+03)
c
c Parameters for error messages
      integer MEDDIG, MENTXT, MEFDAT, MERET, MEEMES, METEXT, MEFVEC
      parameter (MEDDIG=12, MENTXT=23, MEFDAT=25, MERET=51, MEEMES=52,
     1  METEXT=53, MEFVEC=61)
c
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA SXRK8$B
cAB IDAT(1) = $I is an error on  entry to SXRK8A.$E
cAC Too much precision requested.$E
cAD DXRK8G was not called as was requested.$E
cAE Can not continue after integration done.$E
cAF LAST WARNING -- At T=$F, the Error Est. is $F which is too $C
c   big, but the stepsize has its minimum value which is $F.$E
cAG Initial TS(2)=$F has the wrong sign.$E
cAH LAST WARNING -- Looks like problem is stiff near TS(1) = $F, $C
c   with H = $F.  Consider using an integrator designed for stiff $C
c   equations.$E
cAI If you are going to ignore my error flags, I quit!$E
cAJ A sign change in TS($I) at TS(1)=$F is skipped due to $C
c   inconsistent computation of its value.$E
c   $
cAK SXRK8$B
cAL Error tolerance increased from $(E10.3) times that requested to $C
c   $G times that requested.$E
c   $
cAM $NRejected step at T=$F with stepsize of $(E12.5), ERR=$(E10.3)$C
c   , E5/E3=$G, AH=$F, PHI=$F$E
c   $
cAN $NAH=$F, adjusted to $F with KSTIFF=$I, Last AH=$F, E5/E3=$F$E
c   $
cAO T=$(E15.8)  H=$(E12.5) ERR=$(E10.3) PHI=$G  E5/E3=$G HMAX=$G $C
c   HCAU=$G$E
c   $
cAP Raw($I:)/H**2 =$B
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM,LTXTAN,LTXTAO,LTXTAP
      parameter (LTXTAA=  1,LTXTAB=  8,LTXTAC= 55,LTXTAD= 86,LTXTAE=127,
     * LTXTAF=169,LTXTAG=286,LTXTAH=324,LTXTAI=460,LTXTAJ=512,
     * LTXTAK=  1,LTXTAL=  8,LTXTAM=  1,LTXTAN=  1,LTXTAO=  1,
     * LTXTAP=  1)
      character MTXTAA(3) * (202)
      character MTXTAB(1) * (98)
      character MTXTAC(1) * (90)
      character MTXTAD(1) * (62)
      character MTXTAE(1) * (71)
      character MTXTAF(1) * (17)
      data MTXTAA/'SXRK8$BIDAT(1) = $I is an error on  entry to SXRK8A.$
     *EToo much precision requested.$EDXRK8G was not called as was reque
     *sted.$ECan not continue after integration done.$ELAST WARNING -- A
     *t T=$F, the Error',' Est. is $F which is too big, but the stepsize
     * has its minimum value which is $F.$EInitial TS(2)=$F has the wron
     *g sign.$ELAST WARNING -- Looks like problem is stiff near TS(1) =$
     * $F, with H = $F.  Consi','der using an integrator designed for st
     *iff equations.$EIf you are going to ignore my error flags, I quit!
     *$EA sign change in TS($I) at TS(1)=$F is skipped due to inconsiste
     *nt computation of its value.$E '/
      data MTXTAB/'SXRK8$BError tolerance increased from $(E10.3) times$
     * that requested to $G times that requested.$E'/
      data MTXTAC/'$NRejected step at T=$F with stepsize of $(E12.5), ER
     *R=$(E10.3), E5/E3=$G, AH=$F, PHI=$F$E'/
      data MTXTAD/'$NAH=$F, adjusted to $F with KSTIFF=$I, Last AH=$F, E
     *5/E3=$F$E'/
      data MTXTAE/'T=$(E15.8)  H=$(E12.5) ERR=$(E10.3) PHI=$G  E5/E3=$G$
     * HMAX=$G HCAU=$G$E'/
      data MTXTAF/'Raw($I:)/H**2 =$B'/
c **** End of automatically generated text
c
c                      1  2  3  4      5
c Other Stuff for error processing
      real             DATERR(8)
      integer IERR, IERROR(9), ISTOP(9), LERMSG(10),
     1  MACT(5), MACT1(2), MACT2(6), MACT3(5), MACT4(4)
      data MACT / MEEMES, 0, 0, 0, MERET /
      data LERMSG / LTXTAB, LTXTAC, LTXTAD, LTXTAE, LTXTAF, LTXTAG,
     1  LTXTAH, LTXTAI, LTXTAJ, LTXTAK /
c                    1   2   3   4   5   6   7   8   9
      data IERROR / 22,  5, 24, 10,  2, 23,  4, 99,  3 /
      data  ISTOP / 88, 25, 88, 88, 25, 88, 14, 99, 14/
      data MACT1 / METEXT, MERET /
c                       1       2  3       4  5      6
      data MACT2 / METEXT, MEFDAT, 0, MEFVEC, 0, MERET /
      data MACT3 / MEEMES, 24, 0, 0, MERET /
      data MACT4 / MEDDIG, 4, METEXT, MERET /
c++  Default STAT=.F.
c++  Code for STAT is inactive
C      real             ESUMSQ, EMAXER
C      save ESUMSQ, EMAXER
c++  End
c
c
c ************************* Start of Executable Code *******************
c
      NEQ = IDAT(4)
      LOCBF = IDAT(LBF)
      if (IDAT(1) .eq. 1) then
        if (IDAT(LREVF) .gt. 0) then
c                            Copy F to place wanted in WORK.
          if (IDAT(2) .gt. 0) then
            do 40  I = 1, NEQ
              WORK(IDAT(2) + I - 1) = F(I)
 40         continue
          end if
        end if
        H = TS(2)
      end if
      go to 400
c Computing derivatives -- Starting location depends on which deriv.
 200  IDAT(2) = IDAT(2) + NEQ
 210  IDAT(LWHYF) = IDAT(LWHYF) + 1
 220  continue
      if (IDAT(LREVF) .ne. 0) return
      call SXRK8F(TS(1), Y, WORK(IDAT(2)), IDAT)
c
c ****************** Main Loop, Branch on value of IDAT(1) *************
c
 400  continue
      H = TS(2)
c               1     2     3     4
      go to (1000, 4320, 3000, 3100), IDAT(1)
      if (IDAT(1) .ge. 19) go to 3700
      if (IDAT(1) .le. 0) then
        if (IDAT(1) .eq. 0) go to 500
        if (IDAT(1) .ge. -2) then
          IDAT(KSTEPX) = IDAT(1)
          IDAT(1) = IDAT(IDAT(LINT))
          if (IDAT(NGLO) .gt. 0) then
            if (IDAT(NGHI) .gt. 0) IDAT(1) = 25
          end if
          go to 400
        else
          IDAT(2) = -IDAT(1)
          IDAT(1) = 3
          return
        end if
      end if
      IERR = 1
      go to 5000
c
c ****************** Starting or Restarting an integration *************
c
 500  IDAT(1) = 1
      IDAT(2) = 1
      IDAT(LWHYF) = 16
c       Negative IDAT(NTGS) so G-Stops know we are starting.
      IDAT(NTGS) = -abs(IDAT(NTGS))
      IDAT(NGLO) = 0
      DAT(LINTX) = .1E0
c++  Code for STAT is inactive
C      ESUMSQ = 0.E0
C      EMAXER = 0.E0
c++  End
      go to 220
c
c *************************** Computed derivatives *********************
c
 1000 continue
c               2    3    4    5    6    7    8    9   10   11   12
      go to (1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,
     1  2300,2400,2500,2600,2700,3580,2850), IDAT(LWHYF)-1
c         13   14   15   16   17   18,  19
c
c ******************* Compute Y for the Various Stages *****************
c
c Stage 2
 1200 do 1210 I=1,NEQ
        Y(I)=DAT(LOCTY+I) + H*(A31*DAT(LOCBF+I) + A32*WORK(I))
 1210 continue
      TS(1) = DAT(LOCTY) + C3 * H
      go to 200
c Stage 3
 1300 do 1310  I=1,NEQ
        Y(I)=DAT(LOCTY+I) + H*(A41*DAT(LOCBF+I) + A43*WORK(I+NEQ))
 1310 continue
      TS(1) = DAT(LOCTY) + C4 * H
      go to 200
c Stage 4
 1400 do 1410  I=1,NEQ
        Y(I)=DAT(LOCTY+I) + H*(A51*DAT(LOCBF+I) +  A53*WORK(I+NEQ) +
     1    A54*WORK(I+2*NEQ))
 1410 continue
      TS(1) = DAT(LOCTY) + C5 * H
      go to 200
c Stage 5
 1500 do 1510  I=1,NEQ
        Y(I)=DAT(LOCTY+I) + H*(A61*DAT(LOCBF+I) + A64*WORK(I+2*NEQ) +
     1    A65*WORK(I+3*NEQ))
 1510 continue
      TS(1) = DAT(LOCTY) + C6 * H
      go to 200
c Stage 6
 1600 do 1610  I=1,NEQ
        Y(I)=DAT(LOCTY+I) + H*(A71*DAT(LOCBF+I) + A74*WORK(I+2*NEQ) +
     1    A75*WORK(I+3*NEQ) + A76*WORK(I+4*NEQ))
 1610 continue
      TS(1) = DAT(LOCTY) + C7 * H
      go to 200
c Stage 7
 1700 do 1710  I=1,NEQ
        Y(I)=DAT(LOCTY+I) + H*(A81*DAT(LOCBF+I) + A84*WORK(I+2*NEQ) +
     1    A85*WORK(I+3*NEQ) + A86*WORK(I+4*NEQ) + A87*WORK(I+5*NEQ))
 1710 continue
      TS(1) = DAT(LOCTY) + C8 * H
      go to 200
c Stage 8
 1800 do 1810  I=1,NEQ
        Y(I)=DAT(LOCTY+I) + H*(A91*DAT(LOCBF+I) + A94*WORK(I+2*NEQ) +
     1    A95*WORK(I+3*NEQ) + A96*WORK(I+4*NEQ) + A97*WORK(I+5*NEQ) +
     3    A98*WORK(I+6*NEQ))
 1810 continue
      TS(1) = DAT(LOCTY) + C9 * H
      go to 200
c Stage 9
 1900 do 1910 I=1,NEQ
        Y(I)=DAT(LOCTY+I) + H*(A101*DAT(LOCBF+I) + A104*WORK(I+2*NEQ) +
     1    A105*WORK(I+3*NEQ) + A106*WORK(I+4*NEQ) +
     2    A107*WORK(I+5*NEQ) + A108*WORK(I+6*NEQ) +
     3    A109*WORK(I+7*NEQ))
 1910 continue
      TS(1) = DAT(LOCTY) + C10 * H
      go to 200
c Stage 10
 2000 do 2010 I=1,NEQ
        Y(I)=DAT(LOCTY+I) + H*(A111*DAT(LOCBF+I) + A114*WORK(I+2*NEQ) +
     1    A115*WORK(I+3*NEQ) + A116*WORK(I+4*NEQ) +
     2    A117*WORK(I+5*NEQ) + A118*WORK(I+6*NEQ) +
     3    A119*WORK(I+7*NEQ) + A1110*WORK(I+8*NEQ))
 2010 continue
      TS(1) = DAT(LOCTY) + C11 * H
      IDAT(2) = 1
      go to 210
c Stage 11
 2100 continue
c      print '(''Start stage 11'')'
      do 2110  I=1,NEQ
         Y(I) = DAT(LOCTY+I)+H*(A121*DAT(LOCBF+I)+A124*WORK(I+2*NEQ)+
     1      A125*WORK(I+3*NEQ)+A126*WORK(I+4*NEQ)+A127*WORK(I+5*NEQ)+
     2      A128*WORK(I+6*NEQ)+A129*WORK(I+7*NEQ)+A1210*WORK(I+8*NEQ)+
     3      A1211*WORK(I))
 2110 continue
c      print '(''End stage 11'')'
      TS(1) = DAT(LOCTY) + H
      go to 200
c
c ******* Step Almost Finished -- Check Errors & Adjust Stepsize *******
c
 2200 continue
c  Set new value for Y, correction from start of step, and the
c  error estimate.
      do 2210  I=1,NEQ
c   The correction for the final Y / h.
        WORK(I+2*NEQ) = B1*DAT(LOCBF+I) +  B6*WORK(I+4*NEQ) +
     1    B7*WORK(I+5*NEQ) + B8*WORK(I+6*NEQ) + B9*WORK(I+7*NEQ) +
     2    B10*WORK(I+8*NEQ) + B11*WORK(I) + B12*WORK(I+NEQ)
c                    The error estimate of order 3.
        WORK(I+9*NEQ) = WORK(I+2*NEQ) - BHH1*DAT(LOCBF+I) -
     1    BHH2*WORK(7*NEQ+I) - BHH3*WORK(I+NEQ)
c                    The error estimate of order 5.
        WORK(I+3*NEQ) = ER1*DAT(LOCBF+I) + ER6*WORK(I+4*NEQ) +
     1    ER7*WORK(I+5*NEQ) + ER8*WORK(I+6*NEQ) + ER9*WORK(I+7*NEQ) +
     2    ER10*WORK(I+8*NEQ) + ER11*WORK(I) + ER12*WORK(I+NEQ)
 2210 continue
      go to 3260
c
c *********** Processing derivatives required for interpolation ********
c
c     Got the first function value required only for interpolation.
 2300 do 2310 I = 1, NEQ
        Y(I)=DAT(LOCTY+I) + H*(A151*DAT(LOCBF+I) +
     1    A156*WORK(4*NEQ+I) + A157*WORK(5*NEQ+I) +
     2    A158*WORK(6*NEQ+I) + A1511*WORK(I) + A1512*WORK(NEQ+I) +
     3    A1513*WORK(3*NEQ+I) + A1514*WORK(8*NEQ+I))
 2310 continue
      TS(1) = DAT(LOCTY) + C15 * H
      IDAT(2) = 1
      go to 210
c Just got second derivative required for interpolation now get third
 2400 do 2410  I = 1, NEQ
        Y(I)=DAT(LOCTY+I) + H*(A161*DAT(LOCBF+I) +
     1    A166*WORK(4*NEQ+I) + A167*WORK(5*NEQ+I) +
     2    A168*WORK(6*NEQ+I) + A169*WORK(7*NEQ+I) +
     3    A1613*WORK(3*NEQ+I) + A1614*WORK(8*NEQ+I) + A1615*WORK(I))
 2410 continue
      TS(1) = DAT(LOCTY) + C16 * H
      IDAT(2) = 4*NEQ + 1
      go to 210
c Just got the last derivative required for interpolation
 2500 continue
c    Final preparation for interpolation.
      J = IDAT(LOCINT)
      WORK(J - 2) = DAT(LOCTY)
      WORK(J - 1) = H
      do 2520 I = 1, NEQ
        WORK(4*NEQ+J) = H * (WORK(4*NEQ+J) + D413*WORK(3*NEQ+I) +
     1    D414*WORK(8*NEQ+I) + D415*WORK(I) + D416*WORK(4*NEQ+I))
        WORK(5*NEQ+J) = H * (WORK(5*NEQ+J) + D513*WORK(3*NEQ+I)+
     1    D514*WORK(8*NEQ+I) + D515*WORK(I) + D516*WORK(4*NEQ+I))
        WORK(6*NEQ+J) = H * (WORK(6*NEQ+J) + D613*WORK(3*NEQ+I)+
     1    D614*WORK(8*NEQ+I) + D615*WORK(I) + D616*WORK(4*NEQ+I))
        WORK(7*NEQ+J) = H * (WORK(7*NEQ+J) + D713*WORK(3*NEQ+I)+
     1    D714*WORK(8*NEQ+I) + D715*WORK(I) + D716*WORK(4*NEQ+I))
        J = J + 1
 2520 continue
c          Go save state and then take care of interpolation.
      if (IDAT(NGLO) .ne. 0) then
c                 Interpolation was needed for a G-Stop
        TS(1) = DAT(LOCTGS)
        DAT(LINTX) = DAT(LSAVEH) / H
      else
        TS(1) = DAT(LTOUT)
      end if
      go to 4200
c
c ****** Got derivative at start, get initial stepsize if needed *******
c
c            Save initial t, y, and f in their base locations.
 2600 DAT(LOCTY) = TS(1)
      DAT(NEXTT) = TS(1)
      do 2610 I = 1, NEQ
        DAT(LOCTY+I) = Y(I)
        DAT(LOCBF+I) = WORK(I)
 2610 continue
      IDAT(NERRSV) = -2
      IDAT(KEOS) = IDAT(LOCEOS)
      DAT(LASTE3) = 0.E0
c       Setting DAT(HA) so it has some initialized value.
      DAT(HA) = 1.E0
c Get square of scaled norms of F and Y in DAT(LERR), and DAT(LERR+1)
c (Most of the Y arguments do no contribute to the desired results.)
      call SXRK8N(IDAT, DAT, WORK, Y, Y, Y, DAT(LERR))
      if ((DAT(LEPS)*DAT(LERR)) * DAT(LEPS) .ge. DAT(LERTRY+1)**2) then
c Error request too small.  Save data to enable reset of error allowed.
        DAT(LPHI) = (DAT(LEPS) * DAT(LERR+1)) * DAT(LEPS)
        IERR = 2
        IDAT(3) = 0
        go to 5000
      end if
c If have stepsize, get next output point and then to end of step stuff.
c            Following "go to" so that code can convert to a case.
      go to 3600
c
c  Just got an extra derivative for use in getting initial stepsize.
 2700 do 2710 I = 1, NEQ
c                 F at new point - F at initial point
        WORK(I + NEQ) = WORK(I) - DAT(LOCBF + I)
 2710 continue
c Get the error scaled norm squared of the differences in F, Y (/H**2)
      call SXRK8N(IDAT, DAT, WORK(NEQ+1), DAT(LOCBF+1), DAT(LOCTY+1),
     1  DAT(LOCTY+1),  EN)
      if (EN(1) .eq. 0.E0) then
c                        No change in f, try increasing stepsize by 100.
        DAT(HA) = 100.E0 * DAT(HA)
      else
        if (DAT(LERR+1)**2 .gt. DAT(LERR)) then
c             TP2 is sqrt(y/y'') = sqrt(y/(df/h)) = h sqrt(y / df)
          TP2 = DAT(HA) * sqrt(DAT(LERR+1) / EN(1))
        else
c             TP2 is y'/y'' = f / (df/h) = hf/df
          TP2 = DAT(HA) * sqrt(DAT(LERR) / EN(1))
        end if
      end if
c          5 picked as it works reasonably well
      TP2 = 5.E0 * TP2 * (EN(1) * DAT(HA))**(-.125E0)
c                Reduce if concern for stiffness
      if (EN(2) .ne. 0.E0) TP2 = min(TP2, DAT(HA)* sqrt(EN(2)/EN(1)))
      DAT(HA) = min(max(TP2, DAT(LHMIN)), DAT(HMAX))
      TS(2) = DAT(HA) * DAT(NEGPOS)
      DAT(LERR) = 0.E0
c                      Restore data as if finished a step.
      TS(1) = DAT(LOCTY)
      do 2740  I = 1, NEQ
        Y(I) = DAT(LOCTY+I)
 2740 continue
      AH = -1.E2

c Use below when we have HMAX small and want a plot
c     open (FILE='plot.out', UNIT=31)

      go to 3650

c End of getting initial stepsize
c                End of cases for computing derivatives

c G sign change, Try a small step to almost reach the sign change.
 2850 TP = abs(DAT(NEXTH))
      H = TS(2)
      if (abs(TP - H) .lt. DAT(LEPS) * 10.E0 * abs(TP)) then
c Things are messed up here.  We give an error message and ignore the
c sign change in g.
        TS(IDAT(NTGS)-IDAT(NGHI)-2) = TS(-IDAT(NGHI))
        H = TP
        IERR = 9
        DATERR(1) = TS(1)
        IDAT(1) = -IDAT(NGHI)
        go to 5000
      end if
      DAT(LSAVEH) = DAT(NEXTH)
      DAT(LOCTGS) = TS(1)
      DAT(LOCTXG) = DAT(LOCTGS)
      go to 4140
c ************* Just processed G-Stops
c
c
c ******** Attempt to continue an integration that has finished. *******
c
 3000 if (IDAT(2) .eq. 2) go to 4100
      IERR = 4
      go to 5000
c
c ***************** An error condition has been reported ***************
c
 3100 H = TS(2)

      if (IDAT(2) .eq. 2) then
c                         Error estimate too large at min. stepsize
        if ((DAT(LERTRY+1) .gt. 0) .or. (DAT(HA) .gt. DAT(LHMIN)))
     1    IDAT(KHMIN) = 0
        DAT(LERTRY+1) = abs(DAT(LERTRY+1))
        go to 3280
      else if (IDAT(2) .eq. 3) then
c                  Too many steps, just continue unless it would loop
        if (IDAT(KSTEPM) .ne. IDAT(KSTEP)) go to 4000
      else if (IDAT(2) .eq. 4) then
c                          Just gave stiff or too much precision warning
        IDAT(1) = 1
        IDAT(2) = 1
        go to 3320
      else if (IDAT(2) .eq. 5) then
c                          Just gave diagnostic for too much precision.
        TP1 = DAT(LERR+1)
        IDAT(1) = 1
        IDAT(2) = 1
        if (IDAT(3) .eq. 0) then
c           Change to loosen up the error requested.
          DATERR(1) = 1.E0 / sqrt(DAT(LERTRY))
          TP = DAT(LERTRY) / DAT(LERTRY+1)**2
          DAT(LERTRY) = DAT(LERTRY) * (DAT(LERTRY+1)**2 / DAT(LPHI))
          TP1 = TP1 / DAT(LPHI)
          DAT(LERR) = DAT(LERR) / DAT(LPHI)
          DATERR(2) = 1.E0 / sqrt(DAT(LERTRY))
          call SMESS(MACT3, MTXTAB, IDAT, DATERR)
        end if
        go to 3600
      end if
      IERR = 8
      go to 5000
c End of cases based on input value of IDAT(1)

c Set up to do interpolation for a G-Stop
 3150 DAT(LOCTGS) = TS(1)
c
c ********************** Set up to do interpolation ********************
c
 3200 H = TS(2)
      J = IDAT(LOCINT)
      WORK(J-3) = TS(1)
      IDAT(NUMINT) = NEQ
      J = IDAT(LOCINT)
      do 3230 I = 1, NEQ
        WORK(J) = DAT(LOCTY+I)
        WORK(NEQ+J) = WORK(2*NEQ+I) - WORK(J)
        WORK(2*NEQ+J) = H * DAT(LOCBF+I) - WORK(NEQ+J)
        WORK(3*NEQ+J) = WORK(NEQ+J)-H*WORK(3*NEQ+I)-WORK(2*NEQ+J)
        WORK(4*NEQ+J) = D41*DAT(LOCBF+I)+D46*WORK(4*NEQ+I)+
     1    D47*WORK(5*NEQ+I) + D48*WORK(6*NEQ+I) +
     2    D49*WORK(7*NEQ+I) + D410*WORK(8*NEQ+I) +
     3    D411*WORK(I) + D412*WORK(NEQ+I)
        WORK(5*NEQ+J) = D51*DAT(LOCBF+I)+D56*WORK(4*NEQ+I)+
     1    D57*WORK(5*NEQ+I) + D58*WORK(6*NEQ+I) +
     2    D59*WORK(7*NEQ+I) + D510*WORK(8*NEQ+I) +
     3    D511*WORK(I) + D512*WORK(NEQ+I)
        WORK(6*NEQ+J) = D61*DAT(LOCBF+I)+D66*WORK(4*NEQ+I)+
     1    D67*WORK(5*NEQ+I) + D68*WORK(6*NEQ+I) +
     2    D69*WORK(7*NEQ+I) + D610*WORK(8*NEQ+I) +
     3    D611*WORK(I) + D612*WORK(NEQ+I)
        WORK(7*NEQ+J) = D71*DAT(LOCBF+I)+D76*WORK(4*NEQ+I)+
     1    D77*WORK(5*NEQ+I) + D78*WORK(6*NEQ+I) +
     2    D79*WORK(7*NEQ+I) + D710*WORK(8*NEQ+I) +
     3    D711*WORK(I) + D712*WORK(NEQ+I)
        J = J + 1
 3230 continue

c Set up for first function value required for interpolation.
      TS(1) = DAT(LOCTY) + C14 * H
      do 3240  I = 1, NEQ
        Y(I)=DAT(LOCTY+I) + H*(A141*DAT(LOCBF+I) +
     1    A147*WORK(5*NEQ+I) + A148*WORK(6*NEQ+I) +
     2    A149*WORK(7*NEQ+I) + A1410*WORK(8*NEQ+I) +
     3    A1411*WORK(I) + A1412*WORK(NEQ+I) + A1413*WORK(3*NEQ+I))
 3240 continue
      IDAT(1) = 1
      IDAT(2) = 8*NEQ + 1
      IDAT(LWHYF) = 13
      go to 220
c
c ************* Error Estimation and Selecting Stepsize ****************
c
c                Skip calculation of error estimate if no error control.
 3260 if (DAT(LERTRY) .eq. 0.E0) go to 3400
      call SXRK8N(IDAT, DAT, WORK(3*NEQ+1), WORK(9*NEQ+1),
     1  Y, DAT(LOCTY+1),  EN)

c Convert order 10 estimator, ERR, to an order 14 estimator, new ERR.

      EN(2) = 1.E-2 * (EN(2) + 1.E-3 * DAT(LASTE3))
      ERR = DAT(HA) * EN(1) / sqrt(EN(2))
      DAT(PHI) = log(ERR) - P * log(DAT(HA))
      if (ERR .lt. DAT(LERTRY+1)) go to 3300
c                       Error is too big -- must repeat step.
      IDAT(KFAIL) = IDAT(KFAIL) + 1
c++  Code for STAT is inactive
C      if (TS(1) .ne. TS(2)) then
C        EMAXER = max(EMAXER, ERR)
C      end if
c++  End

      if (IDAT(NERRSV) .ne. 0) then
        TP1 = DAT(PHI)
        DAT(HLAST) = 0.E0
      else
        IDAT(NERRSV) = 1
        TP1 = .25E0 * DAT(LPHI) + .75E0 * DAT(PHI)
      end if
      DAT(HA) = exp(-PINV * TP1)
      DAT(HMAX) = max(DAT(HLAST), DAT(HA))
      if (DAT(HA) .le. DAT(LHMIN)) then
        IDAT(KHMIN) = IDAT(KHMIN) + 1
        if (IDAT(KHMIN) .eq. 1) then
          IERR = 5
          DATERR(1) = TS(1)
          DATERR(2) = ERR
          DATERR(3) = TS(2)
          DAT(LERTRY+1) = -DAT(LERTRY+1)
          go to 5000
        end if
      end if

 3280 continue
      if (IDAT(LOCEPR) .ne. 0) then
        DATERR(1) = TS(1)
        DATERR(2) = TS(2)
        DATERR(3) = ERR
        DATERR(4) = H * sqrt(EN(2))
        DATERR(5) = H * sqrt(EN(1))
        call SMESS(MACT1, MTXTAC, IDAT, DATERR)
      end if
      DAT(NEXTH) = DAT(NEGPOS) * DAT(HA)
      TS(2) = DAT(NEXTH)
      H = TS(2)
      go to 4140
c
c *************************** Select stepsize **************************
c
 3300 continue
      DAT(HLAST) = DAT(HA)
 3320 continue

c                  Usual case for adjusting stepsize.
      K = 1
c++  Code for STAT is inactive
C      if (TS(1) .ne. TS(3)) then
C        ESUMSQ = ESUMSQ + (ERR - 1.E0)**2
C        EMAXER = max(EMAXER, ERR)
C      end if
c++  End
      DAT(LASTE3) = EN(2)
      if (IDAT(NERRSV) .ne. 0) then
        if (IDAT(NERRSV) .eq. -2) then
          IDAT(NERRSV) = -1
c After "first" step, just a simple rule for stepsize selection
          DAT(HA) = DAT(HA) * ERR**(-PINV)
          DAT(LAH) = DAT(PHI)
          IDAT(KSTIFF) = 0
          go to 3400
        end if
c Get DAT(LERR), DAT(LERR+1), DAT(LERR+2) initialized.
        DAT(LERR) =   WI1A * DAT(LPHI) + WI1B * DAT(PHI)
        DAT(LERR+1) = WI2A * DAT(LPHI) + WI2B * DAT(PHI)
c        DAT(LERR+2) = WI3A * DAT(LPHI) + WI3B * DAT(PHI)
      else
        DAT(LERR) = DAT(PHI) + W * DAT(LERR)
        DAT(LERR+1) = DAT(LERR) + W * DAT(LERR+1)
c        DAT(LERR+2) = DAT(LERR+1) + W * DAT(LERR+2)
      end if
c      AQ = WQ1A*DAT(LERR) + WQ2A*DAT(LERR+1) + WQ3A*DAT(LERR+2)
c      BQ = WQ1B*DAT(LERR) + WQ2B*DAT(LERR+1) + WQ3B*DAT(LERR+2)
c      CQ = WQ1C*DAT(LERR) + WQ2C*DAT(LERR+1) + WQ3C*DAT(LERR+2)


c      AL = WL1A * DAT(LERR) + WL2A * DAT(LERR+1)
c      BL = WL1B * DAT(LERR) + WL2B * DAT(LERR+1)

c      AQ = max(AQ, AL)

      AH = WL1A * DAT(LERR) + WL2A * DAT(LERR+1)
      IDAT(KSTIFF) = IDAT(KSTIFF) - 1
      if (EN(1) .gt. EN(2)) then
        TP = DAT(LAH) + .75E0 * log(1.E-2*EN(1)/EN(2))
        if (TP .gt. AH) then

          if (IDAT(LOCEPR) .ne. 0) then
            DATERR(1) = AH
            DATERR(2) = TP
            DATERR(3) = DAT(LAH)
            DATERR(4) = sqrt(EN(1) / EN(2))
            call SMESS(MACT4, MTXTAD, IDAT(KSTIFF), DATERR)
          end if
          if ((TP .gt. DAT(LAH)) .and. (ERR .lt. 1.E-4)) TP = DAT(LAH)
          AH = TP
          IDAT(KSTIFF) = max(0, IDAT(KSTIFF) + 2)
          if (IDAT(KSTIFF) .eq. 5) then
            IDAT(KSTIFF) = 2001
c               Give the one time diagnostic
            IERR = 7
            DATERR(1) = TS(1)
            DATERR(2) = TS(2)
            go to 5000
          end if
        end if
      end if
      TP = exp(-PINV * AH)
      DAT(LAH) = AH
      if (IDAT(NERRSV) .eq. 1) then
        DAT(HMAX) = DAT(HA)
        if (DAT(HCAU) .eq. DAT(LHMAXU)) DAT(HCAU) = DAT(HMAX)
      end if
      IDAT(NERRSV) = 0

      if (TP .gt. DAT(HMAX)) then
        if (DAT(HA) .ge. DAT(HMAX)) DAT(HMAX) = sqrt(TP * DAT(HMAX))
        DAT(HA) = DAT(HMAX)
      else
        if (DAT(HMAX) .lt. DAT(HCAU)) then
          DAT(HMAX) = DAT(HCAU)
        else
          DAT(HCAU) = DAT(HMAX)
        end if
        DAT(HA) = max(DAT(LHMIN), TP)
      end if

 3400 continue
      DAT(NEXTH) = DAT(HA) * DAT(NEGPOS)
      if (IDAT(LOCEPR) .ne. 0) then
        DATERR(1) = TS(1)
        DATERR(2) = TS(2)
        DATERR(3) = ERR
        DATERR(4) = DAT(PHI)
        DATERR(5) = sqrt(EN(1) / EN(2))
        DATERR(6) = DAT(HMAX)
        DATERR(7) = DAT(HCAU)
c                Print info. about error estimation
        call SMESS(MACT1, MTXTAE, IDAT, DATERR)
        K = IDAT(LOCEPR)
        if (IDAT(K+2) .ne. 0) then
 3420     I1 = IDAT(K)
          if (I1 .ne. 0) then
            MACT2(3) = I1
            MACT2(5) = -IDAT(K+1)
            call SMESS(MACT2, MTXTAF, IDAT(K), WORK(3*NEQ+1))
            K = K + 2
            go to 3420
          end if
        end if
      end if
      DAT(LPHI) = DAT(PHI)

c End of checking errors and adjusting stepsize
c
c ************************ Set new base t and new y ********************
c
      I1 = NEQ
      I = 1
      K = IDAT(LOCXP)
      if (K .ne. 0) then
c             Accumulate t in extra precision
        J = IDAT(K)
        TS(1) = DAT(LOCTY) + (H + DAT(J))
        DAT(J) = SXRK8X(TS(1), DAT(LOCTY), H, DAT(J))
        DAT(NEXTT) = TS(1)
        I1 = IDAT(K+1)
        if (I1 .eq. 1) go to 3550
      else
        DAT(NEXTT) = DAT(LOCTY) + H
      end if
 3520 do 3530 I = I, I1
        WORK(9*NEQ+I) = Y(I)
c                    The new Y
        Y(I) = DAT(LOCTY+I) + H * WORK(I+2*NEQ)
        WORK(I+2*NEQ) = Y(I)
 3530 continue
 3550 if (I .le. NEQ) then
        K = K + 2
        I1 = IDAT(K)
        do 3570 I = I, I1
          WORK(9*NEQ+I) = Y(I)
          J = J + 1
c                    First get the correction to Y.
          TP = H * WORK(I+2*NEQ)
          Y(I) = DAT(LOCTY+I) + (TP + DAT(J))
          DAT(J) = SXRK8X(Y(I), DAT(LOCTY+I), TP, DAT(J))
          WORK(I+2*NEQ) = Y(I)
 3570   continue
        I1 = IDAT(K+1)
        if (I .le. NEQ) go to 3520
      end if
c End of setting new base t and base y.
c
c   Get final derivative value for the step
      IDAT(2) = 3*NEQ + 1
      IDAT(LWHYF) = 18
      go to 220

c
c ****************** Got last derivative on the step *******************
c
 3580 IDAT(KSTEP) = IDAT(KSTEP) + 1
      IDAT(KEOS) = IDAT(LOCEOS)
      go to 4000
c
c **************** Get stepsize for starting the integration ***********

 3600 if (TS(2) .ne. 0.E0) then
        if (DAT(NEGPOS) * TS(2) .ge. 0.E0) go to 3650
        DATERR(1) = TS(2)
        IERR = 6
        go to 5000
      end if
c
c Compute a stepsize for taking an Euler step.  Use the information from
c this step to generate a guess for the initial stepsize.  Algorithm is
c based (on possibly invalid) assumption that the derivatives of y
c decrease (or increase) with a relatively constant ratio.  To the
c extent that this is not true, the initial stepsize will not be
c selected very well.  At worst this should just waste a few function
c evaluations.
c
      if (DAT(LERR) .ne. 0.E0) then
c Set TP = est. (||y||_e / ||f||_e)^2
        TP = max(sqrt(DAT(LERR+1) / DAT(LERR)), 1.E-2)
        DAT(HA)=.05E0 * TP * (DAT(LERR+1)+DAT(LERR))**(-.0625E0)
      else
        DAT(HA) = .05E0
        TP = DAT(LERR+1) ** .0625E0
      end if
c Get DAT(HA) in range.
      DAT(HA) = min(DAT(HA), DAT(HMAX))
c          Compute a new Y
      H = DAT(HA) * DAT(NEGPOS)
      do 3630 I = 1, NEQ
        Y(I) = DAT(LOCTY+I) + H * WORK(I)
 3630 continue
c             Prepare for getting the derivative
      DAT(LERR+1) = TP
      TS(1) = TS(1) + H
      go to 210
c
c Just finished  establishing the initial stepsize
c
 3650 DAT(NEXTT) = TS(1)
      DAT(NEXTH) = TS(2)
      H = TS(2)
c                 Get next output point set.
      go to 3750
c
c *************** Just processed some kind of output point. ************
c
c                 Got output point using some kind of interpolation.
 3700 if (IDAT(1) .lt. 25) then
        if (IDAT(1) .le. 21) go to 3800
c                            Processed some kind of T output point
        J = IDAT(3)
        K = K + 1
        if (DAT(LOCSAV) .eq. DAT(J)) then
          if (IDAT(1) .eq. 24) then
            if (DAT(NEGPOS) * DAT(J+1) .gt. 0.E0) then
              DAT(J) = DAT(J) + DAT(J+1)
              go to 3750
            end if
          end if
          if (2*IDAT(2) .eq. IDAT(LINT) - IDAT(INTSAV)) then
c  The first output we were examining is now inactive.
            IDAT(LOCOUT) = IDAT(LOCOUT) + 2
          else
c  Store a value well beyond TS(3) so this output is ignored later.
            DAT(J) = sign((2.E0 * abs(TS(3)) + 1.E35), DAT(NEGPOS))
          end if
        end if
      else if (IDAT(1) .eq. 30) then
        if (TS(1) .eq. TS(3)) then
c              The integration is finished.
c++  Code for STAT is inactive
C          ESUMSQ = sqrt(ESUMSQ)
C          print '(''W ='', F6.3, ''  KFAIL ='', I4, ''  Norm Diff ='',
C     1F7.1, ''  Max. Diff ='', 1PE8.1)',
C     2      W,IDAT(KFAIL),ESUMSQ,EMAXER/DAT(LERTRY+1)
Cc          Returns value to the test driver.
C          DAT(24) = ESUMSQ
C          DAT(25) = EMAXER
c++  End
          IDAT(1) = 3
          return
        end if
        DAT(NEXTH) = DAT(LSAVEH)
      else
        if (IDAT(2) .le. IDAT(3)) then
c                We have reported all G-Stops at this time.
          if (IDAT(2) .le. IDAT(NXGS)) then
c                  Must restart after an extrapolating G-Stop
            TS(2) = DAT(LSAVEH)
            go to 500
          end if
c              Interpolating  G-Stop
c              Check at end of step for another G-Stop
          IDAT(KEOS) = -1
          IDAT(NGLO) = 0
          go to 3800
        else
c              There should be more G-Stopa for this value of TS(1)
          go to 4310
        end if
      end if
c                Get next DAT(LTOUT) and IDAT(LINT)
 3750 continue
      DAT(LTOUT) = TS(3)
      TP = TS(1)
      IDAT(LINT) = IDAT(LTFIN)
c
      J = IDAT(LOCOUT)
 3780 if (IDAT(J) .ne. 30) then
        I1 = IDAT(J+1)
        if (DAT(NEGPOS) * (DAT(I1) - DAT(LTOUT)) .lt. 0.E0) then
c                        This output point precedes the current pick.
          DAT(LTOUT) = DAT(I1)
          IDAT(LINT) = J
        end if
        J = J + 2
        go to 3780
      end if
      if (IDAT(NGLO) .gt. 0) then
c                 An interpolating G-Stop is in the queue
        if (IDAT(KSTEPX) .eq. -1) go to 500
        TS(1) = DAT(LOCTGS)
        go to 4200
      end if
c Check for user interupt after taking care of interpolation updates.
 3800 continue
c               Check if we want a restart
      if (IDAT(KSTEPX) .eq. -1) go to 500
c
c **********************   Process end of step actions *****************
c
c IDAT(KEOS) = -3 End of step then G-Stop
c            = -2 End of step then TOUT
c            = -1 G-Stop then TOUT
c            =  0 TOUT then finish
c            =  1 Finish
 4000 K = IDAT(KEOS)
      if (K .lt. 0) then
        if (K .lt. -1) then
c                  End of step (on every step) return
          IDAT(1) = 21
          IDAT(2) = 1
          IDAT(KEOS) = IDAT(KEOS) + 2
          go to 4250
        else
c                   Process G-Stops
          IDAT(KEOS) = 0
          if (IDAT(1) .lt. 25) WORK(IDAT(LOCINT)-4) = DAT(LOCTY)
          IDAT(1) = 2
          IDAT(3) = 0
          if (IDAT(NGHI) .lt. 0) then
c We have taken a smaller step for an extrapolating G-Stop.  Now
c interpolate (actually extrapolate) to where we think the sign changes.
            TS(1) = DAT(LOCTGS)
            TS(2) = H
            go to 3200
          end if
c Update solution to the end of the step if it is not already there.
c (Needed if processing after got a G-Stop on the step already.)
          if (TS(1) .ne. DAT(NEXTT)) then
            if (DAT(NEGPOS) * (DAT(NEXTT) - TS(1)) .lt. 0.E0) then
c                        We extrapolated for a G-Stop
              TS(1) = DAT(LOCTXG)
              if (DAT(NEGPOS)*(TS(1)-TS(3)) .gt. 0.E0) TS(1) = TS(3)
              IDAT(NGHI) = -IDAT(NGHI)
              IDAT(NGLO) = IDAT(NGHI)
              go to 4200
            end if
c Restore solution to that at the end of the step to check again.
            TS(1) = DAT(NEXTT)
            do 4030 I = 1, NEQ
              Y(I) = WORK(2*NEQ+I)
 4030       continue
          end if
          J = IDAT(LOCINT)
          WORK(J - 2) = DAT(LOCTY)
          WORK(J - 1) = H
          go to 4300
        end if
      end if
c                  Check for output point
      if (DAT(NEGPOS) * (DAT(NEXTT) - DAT(LTOUT)) .ge. 0.E0) then
c                            End of step passes an output point.
c                       Check if reached the final point.
        IDAT(1) = IDAT(IDAT(LINT))
        if (IDAT(1) .eq. 30) then
          IDAT(2) = 0
          if (TS(1) .ne. TS(3)) then
c Restore solution to that at the end of the step to check again.
            TS(1) = TS(3)
            do 4040 I = 1, NEQ
              Y(I) = WORK(2*NEQ+I)
 4040       continue
          end if
          go to 4250
        end if
        if (IDAT(1) .eq. 23) then
          IDAT(IDAT(LINT)+1) = DAT(NEXTT)
          DAT(LTOUT) = DAT(NEXTT)
        end if
c                         Ready for output if same t.
        if (TS(1) .eq. DAT(LTOUT)) go to 4220
c                         Get ready for interpolation if not ready yet.
        if (IDAT(NUMINT) .lt. 0) go to 3200
c                         Do the interpolation
        TS(1) = DAT(LTOUT)
        go to 4200
      end if
c
      IDAT(KEOS) = 1
      if (DAT(LOCTY) .ne. DAT(NEXTT)) then
c                  Save information required to take the next step.
c            Set new base t, y, and f.
        DAT(LOCTY) = DAT(NEXTT)
        do 4070 I = 1, NEQ
          DAT(LOCTY+I) = WORK(2*NEQ+I)
          DAT(LOCBF+I) = WORK(3*NEQ+I)
 4070   continue
      end if
c                  Essentially finished with the step.
      if (IDAT(KSTEP) .ge. IDAT(KSTEPX)) then
        if (IDAT(KSTEPX) .lt. 0) then
c                 User wants an interupt
          IDAT(2) = -IDAT(KSTEPX)
          IDAT(KSTEPX) = IDAT(KSTEPM)
          IDAT(1) = 3
          return
        end if
c  Step count has reached point where something special is called for.
        if (IDAT(KSTEPM) .le. IDAT(KSTEP)) then
c                        Hit maximum number of steps
          IDAT(KSTEPM) = IDAT(KSTEPM) + IDAT(KSTEPM+1)
          IDAT(KSTEPX) = IDAT(KSTEPM)
          IDAT(1) = 20
          go to 4250
        end if
      end if
c       Standard value for next stepsize and continuation entry.
 4100 continue
      TS(2) = DAT(NEXTH)
      if ( DAT(NEGPOS) * (TS(3) - DAT(LOCTY) - TS(2)) .lt.
     1  1.E-2 * abs(TS(2))) then
        TS(2) = DAT(NEGPOS) * (TS(3) - DAT(LOCTY))
        DAT(HA) = abs(TS(2))
        if (DAT(HA) .le. DAT(LEPS)*abs(DAT(LOCTY))) then
c                         Got to the final point
          IDAT(1) = 3
          IDAT(2) = 0
          TS(1) = TS(3)
          go to 4250
        end if
      end if
c                        Save standard stepsize
      DAT(LSAVEH) = DAT(NEXTH)
      H = TS(2)
c                      Start the next step.
 4140 IDAT(LWHYF) = 2
      IDAT(NUMINT) = -NEQ
      IDAT(1) = 1
      IDAT(2) = 1
      do 4160 I=1,NEQ
        Y(I)=DAT(LOCTY+I) + H*A21*DAT(LOCBF+I)
 4160 continue
      TS(1) = DAT(LOCTY) + C2 * H
      go to 220
c                         Do the interpolation.
 4200 WORK(1) = DAT(LINTX)
      call SXRK8I(TS(1), Y, IDAT, WORK)
 4220 if (IDAT(NGLO) .ne. 0) then
        if (IDAT(NGLO) .lt. 0) then
c             Interpolated for a G-Stop
          IDAT(1) = 2
          IDAT(2) = -1
          IDAT(3) = IDAT(NGHI)
c   If interpolating for an extrapolated stop, we must start fresh.
          if (IDAT(3) .lt. 0) IDAT(2) = 0
          go to 4310
        else
c               G-Stop has been found
          if (IDAT(NGHI) .lt. 0) then
c               Another t output precedes the G-Stop
            IDAT(NGHI) = -IDAT(NGHI)
          else
            IDAT(2) = IDAT(NGHI)
            IDAT(3) = IDAT(NGLO)
            IDAT(1) = 26
            if (IDAT(2) .le. IDAT(NXGS)) then
              IDAT(1) = 25
              DAT(NEXTH) = DAT(LSAVEH)
              TS(2) = DAT(NEXTH)
            end if
            go to 4250
          end if
        end if
      end if
c                        Set flags for T output points
      K = IDAT(LINT)
      IDAT(1) = IDAT(K)
      IDAT(2) = (K + 2 - IDAT(INTSAV)) / 2
      DAT(LOCSAV) = TS(1)
      IDAT(3) = IDAT(K+1)
c
c                         Output to user.
 4250 if (IDAT(LREVO) .ne. 0) return
      call SXRK8O(TS, Y, IDAT, DAT)
      go to 400
c                         Call SXRK8F for a G-Stop
 4300 IDAT(2) = 0
      IDAT(3) = 0
 4310 IDAT(LWHYF) = 1
      if (IDAT(LREVF) .ne. 0) return
      call SXRK8F(TS(1), Y, WORK, IDAT)
 4320 continue
      if ((IDAT(LWHYF) .ne. 21) .or. (IDAT(2) .eq. -1)) then
        DATERR(1) = TS(1)
        IERR = 3
        go to 5000
      end if
c        If IDAT(2) is 0, there was no sign change and no G-Stop
      if (IDAT(2) .eq. 0) go to 4000
c        If IDAT(2) is -2 we need to interpolate for a G-stop.
      if (IDAT(2) .eq. -2) go to 3150
c                                    Got a G-Stop
      DAT(LOCTGS) = TS(1)
c  If G-Stop precedes next output, then output the G-Stop
      if (DAT(NEGPOS) * (TS(1) - DAT(LTOUT)) .le. 0.E0)  go to 4220
c       Output point precedes the G-Stop, save current T, solve to TOUT.
      IDAT(NGHI) = -IDAT(NGHI)
      if (IDAT(IDAT(LINT)) .eq. 30) then
c              Integrate to the end point
        H = TS(3) - DAT(NEXTT)
        TS(2) = H
        go to 4140
      else
        TS(1) = DAT(LTOUT)
      end if
      go to 4200

c               Some kind of error.
 5000 MACT(2) = ISTOP(IERR)
      MACT(3) = IERROR(IERR)
      MACT(4) = LERMSG(IERR)
      call SMESS(MACT, MTXTAA, IDAT, DATERR)
      if (IERR .eq. 9) go to 4140
      IDAT(1) = 4
      IDAT(2) = MACT(3)
      return
      end

      subroutine SXRK8I(T, Y, IDAT, WORK)
c Interpolate to get y(t), using data structures in IDAT and WORK.
c Only certain components of y are set as defined in IDAT.
c
c Formal Arguments
      real             T, Y(*), WORK(*)
      integer IDAT(*)
c
c Local variables
      integer I, NIY
      real             S1
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
      integer J
      real             S

c Parameters for error messages
      integer MENTXT, MERET, MEEMES, METEXT, MEFVEC
      parameter (MENTXT=23, MERET=51, MEEMES=52, METEXT=53,
     1  MEFVEC=61)
c Other Stuff for error processing
      real             DATERR(2)
      integer MACT(5)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA SXRK8$B
cAB Interpolating outside of safe interval by $F of the stepsize $C
c   at T=$F.$E
c   $
cAC SXRK8$B
cAD A bug in sxrk8, sxrk8i called when not allowed at T=$F.$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD
      parameter (LTXTAA= 1,LTXTAB= 8,LTXTAC= 1,LTXTAD= 8)
      character MTXTAA(1) * (78)
      character MTXTAB(1) * (64)
      data MTXTAA/'SXRK8$BInterpolating outside of safe interval by $F o
     *f the stepsize at T=$F.$E'/
      data MTXTAB/'SXRK8$BA bug in sxrk8, sxrk8i called when not allowed
     * at T=$F.$E'/
c **** End of automatically generated text
c
      data MACT / MEEMES, 24, 28, 0, MERET /
c
c Check if ready.
      NIY = IDAT(NUMINT)
      if (NIY .le. 0) then
        MACT(2) = 99
        MACT(3) = 29
        DATERR(1) = T
        call SMESS(MACT, MTXTAB, IDAT, DATERR)
        stop
      end if

      J = IDAT(LOCINT)
      S = (T - WORK(J-2)) / WORK(J-1)
      if ((S .gt. WORK(1) + 1.E0) .or. (S .lt. -WORK(1))) then
c              Error interpolation outside allowed interval.
        DATERR(1) = S
        DATERR(2) = T
        call SMESS(MACT, MTXTAA, IDAT, DATERR)
      end if
      S1 = 1.E0 - S
      do 80 I = 1, NIY
        Y(I) = WORK(J) + S*(WORK(NIY+J) + S1*(WORK(2*NIY+J) +
     1    S*(WORK(3*NIY+J) + S1*(WORK(4*NIY+J) + S*(WORK(5*NIY+J) +
     2    S1*(WORK(6*NIY+J) + S*WORK(7*NIY+J)))))))
        J = J + 1
 80   continue
      return
      end

      subroutine SXRK8N(IDAT, DAT, V1, V2, Y, YOLD, EN)
c Computes squared norms of V1, V2, Y and F as well as Y^T F scaled by
c error tolerances.
c
c Formal arguments
      integer IDAT(*)
      real             DAT(*), V1(*), V2(*), Y(*), YOLD(*), EN(2)
c
c Parameters used to reference DAT
      integer HA, LASTE3, LEPS, LERR, HMAX, LERTRY, LAH,
     1  HCAU, LHMAXU, LHMIN, LINTX, LOCTGS, LOCSAV, LOCTY, LOCTXG,
     2  LPHI, LSAVEH, LTOUT, NEGPOS, NEXTH, NEXTT, PHI, HLAST
      parameter (HA=22, LASTE3=13, LEPS=1, LERR=16,
     1  HMAX=11, LERTRY=14, LAH=12, HCAU=25, LHMAXU=2, LHMIN=3,
     2  LINTX=9, LOCTGS=4, LOCSAV=26, LOCTY=30, LOCTXG=23, LPHI=24,
     3  LSAVEH=10, LTOUT=5, HLAST=20, NEGPOS=6, NEXTH=7, NEXTT=8,
     4  PHI=19)
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
      integer I, I1, I2, J, K, L
      real             SV1, SV2, PV1, PV2, SY, TP, TP1, TP2
      J = LOCERR
      I2 = 0
      SV1 = 8.E-32
      SV2 = 1.E-32
      go to 20
 10   continue
c        Updates when error request is a scalar.
      SV1 = SV1 + PV1 * TP
      SV2 = SV2 + PV2 * TP
 20   if (I2 .eq. IDAT(4)) then
        EN(1) = SV1 * DAT(LERTRY)
        EN(2) = SV2 * DAT(LERTRY)
        return
      end if
      K = IDAT(J)
      I1 = I2 + 1
      I2 = IDAT(J+1)
      L = IDAT(J+2)
      J = J + 3
      if (K .le. 3) then
        PV1 = 0.E0
        PV2 = 0.E0
        if (K .eq. 2) then
c Simple absolute error test
          do 30 I = I1, I2
            PV1 = PV1 + V1(I)**2
            PV2 = PV2 + V2(I)**2
 30       continue
          TP = 1.E0 / DAT(L) ** 2
        else
c Mixed absolute / relative error test
          SY = 0.E0
          do 40 I = I1, I2
            PV1 = PV1 + V1(I) ** 2
            PV2 = PV2 + V2(I) ** 2
            SY = SY + Y(I)**2
 40       continue
          TP = 1.E0 / (DAT(L)**2 + SY * DAT(L+1)**2)
        end if
        go to 10
      else if (K .eq. 4) then
c Separate for each Y -- mixed absolute / relative error test.
        TP1 = DAT(L) ** 2
        TP2 = DAT(L+1) ** 2
        do 50  I = I1, I2
          TP = 1.E0 / (TP1 + TP2 * (Y(I)**2 + YOLD(I)**2))
          SV1 = SV1 + TP * V1(I)**2
          SV2 = SV2 + TP * V2(I)**2
 50     continue
      else if (K .eq. 5) then
c Vector absolute error test
        do 60 I = I1, I2
          TP = 1.E0 / DAT(I+L)
          SV1 = SV1 + (V1(I) * TP)**2
          SV2 = SV2 + (V2(I) * TP)**2
 60     continue
      else if (K .eq. 6) then
c Vector mixed absolute / relative error test
        do 70 I = I1, I2
          TP = 1.E0/(DAT(L+2*I)**2+DAT(L+2*I+1)**2*(Y(I)**2+YOLD(I)**2))
          SV1 = SV1 + TP * V1(I)**2
          SV1 = SV1 + TP * V2(I)**2
 70     continue
      end if
      go to 20
c               End of subroutine SXRK8N
      end


      real             function SXRK8X(VNEW, VOLD, VDIFF, VLOW)
c       Get new least significant part when carrying extra precision.
c       Code could be inline when floating point registers store numbers
c       to the same precision as memory or if one knows the compiler
c       stores intermediate results to memory.
      real             VNEW, VOLD, VDIFF, VLOW
c  With perfect arithmetic this would return 0, if because of round-off
c  VNEW is a little too small, then SXRK8X > 0 which will give a little
c  boost to the Y on the next step.
      SXRK8X = (VOLD - VNEW) + VDIFF + VLOW
      return
      end

