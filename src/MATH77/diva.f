      subroutine DIVA(TSPECS, Y, F, KORD, NEQ, DIVAF, DIVAO, IDIMT,
     1   IDIMY, IDIMF, IDIMK, IOPT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2015-03-15 DIVA  Krogh  Removed extra call divabu after noise test
c>> 2015-03-15 DIVA  Krogh  Forced restart needs more reduction in h.
c>> 2010-02-20 DIVA  Krogh  Fixed calling DIVAOP with array other than F.
c>> 2009-11-03 DIVA  Krogh  Added option 11, more variables initialized.
c>> 2009-10-30 DIVA  Krogh  Gave KSSTRT and ROBND initial values.
c>> 2009-10-30 DIVA  Krogh  Fixed reference to undefined location in F.
c>> 2009-10-21 DIVA  Krogh  Got rid of NaN in diag. print when LSC=3.
c>> 2009-10-15 DIVA  Krogh  A few changes on how noise is handled.
c>> 2002-11-12 DIVA  Krogh  Fixed problem integrating to final output pt
c>> 2002-08-29 DIVA  Krogh  Added test for invalid HMIN/HMAX.
c>> 2002-07-26 DIVA  Krogh  Added KOUTKO to fully support Option 10.
c>> 2002-05-14 DIVA  Krogh  Fix starting prob. for Option 18.
c>> 2002-05-13 DIVA  Krogh  Put exponent letter in  numbers missing them
c>> 2002-05-12 DIVA  Krogh  Added error message for bad option 5 usage.
c>> 2001-09-07 DIVA  Krogh  Changes to allow user tol on G-Stops.
C>> 2001-05-25 DIVA  Krogh  Minor change for making .f90 version.
c>> 2001-05-18 DIVA  Krogh  Less computing with no error test
c>> 2001-05-17 DIVA  Krogh  Fixed so with no error test can't start dump
c>> 2001-04-24 DIVA  Krogh  Inserted comments from ivacom.
c>> 2000-12-01 DIVA  Krogh  Removed (some of) unused C1, MAXSTF, METEXT.
c>> 1999-12-28 DIVA  Krogh  Saved S in DIVACR for output consistency.
c>> 1999-08-19 DIVA  Krogh  Removed superfluous test above label 3520.
c>> 1997-04-22 DIVA  Krogh  Got rid of assigned go to's. F=0 if diag.
c>> 1996-08-26 DIVA  Krogh  Initialize F to 0 if dumping solution.
c>> 1996-08-23 DIVA  Krogh  Print TN not TSPECS(1) in error messages.
c>> 1996-05-30 DIVA  Krogh  Changed DERIVS/OUTPUT to  DIVAF/DIVAO.
c>> 1996-04-27 DIVA  Krogh  Changes to use .C. and C%%.
c>> 1996-03-30 DIVA  Krogh  Added external statement.
C>> 1996-03-25 DIVA  Krogh  Introduced TEXT1 to comply with F77.
c>> 1996-02-27 DIVA  Krogh  Fixed so DUMP not affected by ignored eqs.
c>> 1995-12-18 DIVA  Krogh  Fixed so no solution dump on 0 length integ.
c>> 1995-11-09 DIVA  Krogh  Fixed so char. data at col. 72 is not ' '.
c>> 1995-06-19 DIVA  Krogh  Fixed prob. with discon. just after restart.
c>> 1995-05-09 DIVA  Krogh  Fixed G-Stop/discontinuity code interaction
C>> 1995-04-26 DIVA  Krogh  Use KQMAXS instead of KQMAXI when LDIS>1000.
C>> 1995-04-26 DIVA  Krogh  Keep current KQL on discontinutiy.
C>> 1994-12-16 DIVA  Krogh  Fixed option 12 with K12 < 0.
C>> 1994-11-11 DIVA  Krogh  Declared all vars.
c>> 1994-11-02 DIVA  Krogh  Changes to use M77CON
c>> 1994-09-08 DIVA  Krogh  Added CHGTYP code.
c>> 1994-07-11 DIVA  Krogh  Fix to get same state with/without var. eqs.
c>> 1994-03-07 DIVA  Krogh  Allow larger order in single precision.
c>> 1994-01-14 DIVA  Krogh  Minor change to allow changing TFINAL.
c>> 1993-04-27 DIVA  Krogh  Additions for Conversion to C.
c>> 1993-04-12 DIVA  Krogh  Converted to use slightly altered MESS.
c>> 1993-04-12 DIVA  Krogh  Fixed LSC so sol. saved when HMAX is small.
c>> 1992-10-13 DIVA  Krogh  Fixed G-Stop/discontinuity code interaction.
c>> 1992-09-21 DIVA  Krogh  Fixed bug in discontinuity code.
c>> 1992-09-09 DIVA  Krogh  Fixed bug - Var. Eqs. with discontinuities.
c>> 1992-08-07 DIVA  Krogh  Storage map printed only if option 10 .ne. 0
c>> 1992-07-16 DIVA  Krogh  Restored correct discontinuity code.
c>> 1992-06-16 DIVA  Krogh  Eliminate reuse of storage for option 12.
c>> 1992-04-08 DIVA  Krogh  Removed unused labels, 1020, 2120.
c>> 1992-03-30 DIVA  Krogh  Fixed bug in DIVAOP error message.
c>> 1992-03-12 DIVA  Krogh  Simplified DIVABU, more digits in B's.
c>> 1992-01-16 DIVA  Krogh  Fixed minor bug in error messages.
c>> 1991-12-03 DIVA  Krogh  Major change for improved error checks.
c>> 1991-06-17 DIVA  Krogh  Fixed bug in checking storage allocation.
c>> 1991-04-11 DIVA  Krogh  Fixed minor bug re. option 12 in DIVAOP.
c>> 1991-03-28 DIVA  Krogh  Removed check at label 650 for KORD2I<0.
c>> 1991-02-08 DIVA  Krogh  Changed some floats to generics
c>> 1990-11-08 DIVA  Krogh  Fixed bug on TSPECS on discon.
c>> 1990-09-14 DIVA  Krogh  Fixed bug when discon. and sol. save.
c>> 1990-09-13 DIVA  Krogh  Increased dimension of BETA by 1.
c>> 1990-09-13 DIVA  Krogh  Added one more poss. on rel. error test.
c>> 1990-09-11 DIVA  Krogh  Recent change messed up getting dump output.
c>> 1990-06-05 DIVA  Krogh  Fixed bug in noise test, comments in IVACOM.
c>> 1990-05-08 DIVA  Krogh  Fixed new bug when TMARK hit in DIVAG.
c>> 1990-04-17 DIVA  Krogh  Fixed minor problem in DIVAIN error msg.
c>> 1990-04-10 DIVA  Krogh  Fixed interaction between discon. & dump.
c>> 1990-03-23 DIVA  Krogh  Fixed bug on option "-2", see 1989-12-07.
c>> 1990-03-20 DIVA  Krogh  Fixed rarely occuring loop.
c>> 1990-01-29 DIVA  Krogh  Removed unneeded labels.
c>> 1989-12-14 DIVA  Krogh  Saved common block DIVAEV.
c>> 1989-12-07 DIVA  Krogh  Added option "2" to DIVAOP.
c>> 1989-11-09 DIVA  Krogh  Made GG a save var. in DIVAHC
c>> 1989-08-21 DIVA  Krogh  Fix out of bounds ref. to V in DIVABU
c>> 1989-07-26 DIVA  Krogh  Fix bug in initial dim. check
c>> 1989-07-21 DIVA  Krogh  Code for integrating discontinuities
c>> 1987-12-07 DIVA  Krogh  Initial code.
c
c--D replaces "?": ?IVA,?IVAA,?IVABU,?IVACO,?IVACR,?IVAEV,?IVAF,?IVAHC,
c-- & ?IVAG,?IVAIN,?IVAMC,?IVAO,?IVAOP,?IVAPR,?IVASC,?IVACE,?IVAIE,
c-- & ?IVAPE,?MESS
c
c
c Note a "*" at the start of a name is used to indicate "D" for the
c double precision version and "S" for the single precision version.
c
c When converting between precisions, don't forget to change the value
c of KDIM set in parameter statements in a variety of routines, and to
c adjust comments for the data statements associated with EIBND in
c *IVACR, and B in *IVAHC.
c
c Entries
c  *IVA    Main entry for starting the package.
c  *IVAA   Main program inside the package, calls the other routines,
c          and does checks for output, and noise.  Called by the user
c          if reverse communication is used.
c  *IVABU  Back ups the solution to the current base time, if a step
c          that has been started must be taken over for some reason.
c  *IVACO  Called by user to get certain information from the common
c          blocks.
c  *IVACR  Corrects the solution, estimates errors, and selects order.
c  *IVADB  Subroutine to assist in debugging codes.  Called by user to
c          get a formatted list of all the variables used in the
c          integration.  Not required in usual case.
c  *IVADE  Needed only for delay differential equations.  This is called
c          by the user from the derivative subprogram.
c  *IVAG   Required only if the user has G-Stops, i.e. places to call
c          his output subroutine where certain functions have zeroes.
c  *IVAHC  Compute coefficients that depend on the step size history.
c  *IVAIN  Used to interpolate to arbitrary points.
c  *IVAOP  Used to process user option requests.
c  *IVAPR  Used to update the differences and to predict the solution
c          at the end of the current step.
c
c External Routines
c  *1MACH  Not used in the Fortran 95 version.  ("*" is "D" for double
c          and "R" for single precision.) This returns constants that
c          depend on the floating point arithmetic.  Input arguments of
c          1 to 4 give respectively:  underflow limit, overflow limit,
c          smallest relative difference between two floating point
c          numbers, and the largest relative difference between two
c          floating point numbers.
c DERIVS (formal) Name of subroutine to be called for computing

c  OPTCHK  Used in checking storage allocation.
c  *MESS   Used to output error messages and diaganostic messages.
c          (Just MESS if no floating point is output.)
c  *ZERO   Called only if *IVAG is used.  Iterates to find zeros of
c          arbitrary (continuous) functions.
c
c Common blocks -- As a left over from the distant past, some variables
c   are in common so that they would be saved.
c  *IVAEV  Holds variables that depend on the environment.
c  *IVAMC  The main common block for the package.
c  *IVASC  The secondary common block for the package.  This contains
c          variables that are required for doing interpolation and is
c          separate to simplify saving the variables that are required
c          when the solution is being dumped (saved).
c
c Common variables and local variables
c ALPHA  (*IVAMC) Array with I-th entry = (current step size) / XI(I).
c   Used in computing integration coefficients.
c B      (*IVAHC) Array used to get started on computing integration
c   coefficients.  B(K) = 1. / (K*(K+1))
c BAKMIN (*IVADE) The largest delay at the initial point.
c BETA   (*IVAMC) Array with I-th entry = product (K=1,I-1) of
c   (current (XI(K)) / XI(K) from previous step),  BETA(1)=1.  Used in
c    updating the difference tables.
c C      (*IVAIN) Array used to hold integration/interpolation coeffs.
c C0     Parameter = 0. (in *IVAA,DE,CR,A,G,HC,IN,OP,PR)
c C1     Parameter = 1. (in *IVA,A,CR,DA,HC,IN,OP)
c C10    Parameter = 10. (in *IVAA,CR,OP)
c C1000  Parameter = 1000. (in *IVACR)
c C16    Parameter = 16. (in *IVAA,OP)
c C1M3   Parameter = .001 (in *IVAA)
c C1M5   Parameter = .00001 (in *IVAA)
c C1P125 Parameter = 1.125 (in *IVAA,HC,OP)
c C1P3   Parameter = 1.3 (in *IVAA)
c C1P4   Parameter = 1.4 (in *IVACR)
c C2     Parameter = 2. (in *IVAA,DE,BU,CR,IN,OP)
c C20    Parameter = 20. (in *IVACR)
c C2P5M3 Parameter = .0025 (in *IVAA)
c C4     Parameter = 4. (in *IVACR,OP)
c C40    Parameter = 40. (in *IVACR)
c C4096  Parameter = 4096. (in *IVAA)
c C6     Parameter = 6. (in *IVAA)
c C8M3   Parameter = .008 (in *IVAA)
c CM2    Parameter = -2. (in *IVACR)
c CM8    Parameter = -8. (in *IVACR)
c CMP5   Parameter = -.5 (in *IVACR)
c CMP75  Parameter = -.75 (in *IVAOP)
c CP0625 Parameter = .0625 (in *IVAA)
c CP1    Parameter = .1 (in *IVAA,CR,DA,HC)
c CP125  Parameter = .125 (in *IVACR)
c CP25   Parameter = .25 (in *IVAA,CR,DE,OP)
c CP3    Parameter = .3 (in *IVAA,OP)
c CP4    Parameter = .4 (in *IVAA)
c CP5    Parameter = .5 (in *IVAA,CR,DA,DE,HC,OP)
c CP5625 Parameter = .5625 (in *IVAHC)
c CP625  Parameter = .625 (in *IVAOP)
c CP75   Parameter = .75 (in *IVACR,OP)
c CP8    Parameter = .8 (in *IVACR)
c CP875  Parameter = .875 (in *IVAA, OP)
c CP9    Parameter = .9 (in *IVAOP)
c CP9375 Parameter = .9375 (in *IVACR)
c CQ3125 Parameter = .03125 (in *IVACR)
c CRBQI  Parameter = .421875 (in *IVAHC)  Initial val for computing RBQ.
c CSUM   (*IVAIN) Array used to contain partial sums of the integration
c   coefficients.  This is used to corrrect for a difference table that
c   has not yet been updated.
c D      (*IVAMC) Array to be used later to store coefficients for
c   integrating stiff equations.
c   derivatives.  Not used if option 13 is set.
c DISADJ (*IVAA) Value of stepsize when discontinuity is indicated.
c DNOISE (*IVAMC) Used in determining if noise is limiting the
c   precision.  It is usually |highest difference used in correcting|
c   of the equation with the largest error estimate.
c DS     (*IVAMC) Array to be used later to store coefficients for
c   estimating errors when integrating stiff equations.
c DVC2   (*IVADB) Array used for output of variables HC to TOUT in
c   common block *IVAMC.
c E      (*IVACR) (Estimated error) / (Requested accuracy)
c EAVE   (*IVAMC) This is a weighted average of past values of EIMAX.
c   It is adjusted to account for expected changes due to step changes.
c EEPS10 (*IVAEV) = 10. * (machine epsilon).
c EEPS16 (*IVAEV) = 16. * (machine epsilon).
c EEPS2  (*IVAEV) =  2. * (machine epsilon).
c EEPT75 (*IVAEV) = (machine epsilon) ** (.75)
c EI     (*IVACR) Estimate for what E would be if step size increased.
c EIBND  (*IVACR) Array containing limits on the estimated error with
c   the stepsize increased.  This array tends to make the code a little
c   more conservative on step size increases at low order.
c EIMAX  (*IVAMC) Estimate of (error estimate / error requested) if the
c   step size should be increased.
c EIMIN  (*IVAMC) An error estimate is small enough to allow a step
c   increase if the estimate of ((error with the step size increased) /
c   (error requested)) is less than EIMIN.
c EIMINO (*IVAA) Set to C8M3 and never changed.  When step size is being
c   reduced if EIMIN .le. EIMINO then the reduction factor is set to
c   CP875.  This variable could be a parameter.
c EMAX   (*IVAMC) Largest value computed for (error estimate) / (error
c   requested).
c EOVEP2 (*IVAEV) = EEPS2 * (largest floating point number).
c EPS    (*IVACR) Current absolute error tolerance.  Also used for
c   temporary storage when computing the desired value of EPS.
c ERCOEF (*IVACR) (Error coefficient from formula) / EPS
c EREP   (*IVAMC) If EMAX > EREP, a step is repeated.  Ordinarily
c   this has the value .3.  This is set < 0 if the error tolerance is
c   specified improperly, and is set to a large value if the user
c   requests complete control over the step size.  EREP is also set
c   < 0 after a user specified discontinuity.
c EROV10 (*IVAEV) = 10. / (largest floating point number).
c ETA    (*IVAIN) Array used in computing integration/interp. coeffs.
c EVC    (*IVADB) Array used for output of variables EEPS2 to EROV10 in
c   common block *IVAEV.
c EXR    (*IVAA) Set to CP1 and never changed.  If it is estimated the
c   the (error estimate) / (error requested) on the next step will be
c   .ge. EXR then the step size is reduced.  Could be a parameter.
c F      (formal) Array used to store derivative values, the difference
c   tables, error tolerance requests, and values used by some other
c   options. (in *IVA,A,BU,CR,DA,DB,G,IN,PR)
c FDAT  (*IVAMC) Used to store data for error messages.  (Local array in
c   *IVAIN.)
c FOPT  (formal) in *IVAOP.  Passed as place to save floating point data
c   for options.  This package passes F in for FOPT when calling *IVAOP.
c G      (*IVAMC) Integration coefficients used for predicting solution.
c   G(I, J) gives the I-th coefficient for integrating a J-th order
c   differential equation.  G(1, 1) is equal to the step size.
c GAMMA  (*IVAIN) Array used in computing integration/interp. coeffs.
c GG     (*IVAHC) Array of length = max. differential equation order
c   allowed by code - 1.  GG(K) = (HH**(K+1)) / K!
c GNEW   (formal) in *IVAG.  Current value for vector function g, whose
c   zeroes are to be found.
c GOINT  (*IVACR) Used for assigned go to used in computing integration
c   coefficients.
c GOLD   (*IVAG) Previous value for element of G whose zero search is
c   active.
c GS     (*IVAMC) Integration coefficients used in estimating errors.
c GT     (formal) in *IVAG.  Previous value of GNEW.
c HC     (*IVAMC) Ratio of (new step size) / (old step size)
c HDEC   (*IVAMC) Default value to use for HC when reducing the step
c   size.  (Values closer to 1 may be used some of the time.)
c HH     Equivalenced to G(1,1) = current step size in *IVAA,CR,DA,G,HC.
c HI     (*IVAIN) Step length from the base value of the independent
c   variable for the interpolation.
c HINC   (*IVAMC) Default value to use for HC when increasing the step
c   size.  (Values closer to 1 may be used some of the time.)
c HINCC  (*IVAMC) Actual value used for default value of HC when
c   increasing the step size.  Set to HINC after start is considered
c   complete.  During the start HINCC is set to 1.125.
c HMAX   (*IVAMC) Largest value allowed for abs(step size).  Default
c   value is a very large number.
c HMAXP9 (*IVAMC) .9 * HMAX.
c HMIN   (*IVAMC) Smallest value allowed for abs(step size).  Default
c   value is 0.
c HNEW   (*IVADE) Value of step size when iterating at initial point
c   for delay differential equations.
c I      Used for temporary storage. (*IVAA,BU,CR,DA,DE,G,IN,OP,PR)
c IA     (*IVAOP) absolute value of first integer stored for an option.
c ICF    (*IVAMC) Final index for current loop in *IVACR.  Required by
c   option 18.
c ICI    (*IVAIN) Temporary index, = 0 for interpolation, 1 or 0 for
c   differentiation, and d-1, d-2, ... 0 for integration, where d is the
c   order of the differential equation.  Index of first location
c   in C() used is ICI + an offset.
c ICS    (*IVAMC) Starting index for current loop in *IVACR.
c ID     (formal) Array use to contain integer data from common.  Values
c   are returned in locations 1 to 5 as follows.
c   1    KEMAX = Index of equation with largest error estimate
c   2    KSTEP = Current step number
c   3    NUMDT = Number of differences used for each equation
c   4            Reserved for future use
c   5            Reserved for future use
c IDAT   (*IVAMC) Used to store integer for error messages.  (Also used
c   in *IVAA for temporary storage of KORD(2).  (Local array in *IVAIN.)
c IDE    (*IVADE - formal) Array used to contain past information so
c   that delays can stretch back indefinitely.  If the first location is
c   0, then any interpolations requested must be in the range of the
c   current difference tables.  At present, only the value 0 is allowed
c   in IDE(1).  This array is intended for the support of saving long
c   past histories.  IDE(2) must contain the declared dimension of WDE.
c IDEF   (*IVADE -- formal) Flag giving indicaion of what is going on.
c   = 0  User should compute derivatives and return to the main
c        integrator.
c   = 1  Code is computing additional values in order to get past data
c        necessary for starting.  User should compute derivatives and
c        call *IVADE.
c   < 0  Indicates an error condition.  If *IVADE is called without
c        changing the value of IDEF, the integration is stopped and an
c        error message printed.  Possible error flags are:
c    -1  Difference tables do not span back far enough to compute the
c        past values of Y needed.
c    -2  There is not enough space in WDE to get the required starting
c        values.
c IDIMF  (formal) Declared dimension of F().
c IDIMK  (formal) Declared dimension of KORD().
c IDIMT  (formal) Declared dimension of TSPECS().
c IDIMY  (formal) Declared dimension of Y().
c IDT    (*IVAIN) Used as a base index into the difference table.
c IFLAG  (formal in *IVAG) Used for communication with user.
c   = 1  Continue as if *IVAG was not called.
c   = 2  Check KORD(1) as one would do at start of OUTPUT if no G-Stops
c        were present. (Exit if in DERIVS.)
c   = 3  Return to the integrator.
c   = 4  Compute G and return to *IVAG.
c   = 5  A G-Stop has been found, and NSTOP gives its index.  (If NSTOP
c        < 0, the stop was an extrapolating stop.)
c   = 6  Same as 5, but requested accuracy was not met.
c   = 7  Same as 5, but there is a probable error in computing G.
c   = 8  Fatal error of some type.  (An error message has been printed.)
c IG     (*IVAG)  IG = KORD(2) on the initial entry (0 for extrapolating
c   G-Stops, and 1 for interpolating).
c IGFLG  (*IVAMC) Used primarily in *ivag, but also used in *iva to keep
c   track of the state of GSTOP calculations.
c   = -2 Extrapolatory G's initialized, but not the interpolatory.
c   = -1 Interpolatory G's initialized, but not the extrapolatory.
c   =  0 Set when integration is started or restarted, or option setting
c        GSTOP is set.
c   =  1 Iterating to find a GSTOP.
c   =  2 User told that a GSTOP was found.
c   =  3 Checking G's at point where a GSTOP was located.
c   =  4 Checking G's at a T output point.
c   =  5 Usual case, no sign change detected.
c IGSTOP (*IVAMC) IGSTOP(k) is set in *ivag to the index of the last G
c   with a 0, where k is one for an interpolatory G-Stop, and k is two
c   for an extrapolatory G-Stop.
c IGTYPE (*IVAMC) Array with two elements as for IGSTOP, but this saves
c   a flag giving the nature of convergence to the stop.
c   = 0  All known G-stops completely processed.
c   = 4  Need to compute next value while iterating.
c   = 5  Got good convergence.
c   = 6  Got convergence, but not to desired accuracy.
c   = 7  Problem in getting convergence.
c   = 8  A fatal error of some type.
c IHI    (*IVA) Last location used by the current option.
c ILGREP (*IVAMC) Used when correction to keep track of equations that
c   are to use a certain error tolerance.
c ILGROR (*IVACR) Index of last equation in the current group of
c   equations grouped for selecting integration order.
c ILOW   (*IVA) First location used by the current option.
c INCOM  (*IVADE) Array equivalenced to LDT in the common block *IVASC.
c   Used to simplify saving information in the common block.
c INCOP  (*IVAOP) Array containing data giving the amount of space in
c   IOPT used for each of the options.
c INGS   Current index for G-stop being examined in DIVAG.
c INICAS (*IVADE) Used to track the initialization for a delay equation.
c   = 1  Very beginning.
c   = 2  Getting derivative at the very beginning.
c   = 3  Getting derivatives at points prior to the initial point.
c   = 4  Getting derivative at initial point after iteration is started.
c INTCHK (*IVA) Array passed to OPTCHK containing information on storage
c   allocation.  See comments in OPTCHK for details.
c INTEG  (*IVAIN) Number of integrations being done. (<0 for
c   differentiations and =0 for interpolation.)  Also used as counter
c   when computing integration coefficients.
c        (*IVAPR) Number of integrations being done.
c INTEGS (*IVAPR) = -1 for equations that are not stiff, 0 for those
c   that are stiff.
c INTEGZ (*IVAIN) min(INTEG, 0)
c INTERP (*IVAIN) added to the usual integration order to get the order
c   to be used when interpolating: 3-KQMAXI, if HI=0; 1, if
c   |HI| > |XI(1)| and HI * XI(1) < 0; 0, otherwise -- the usual case.
c IOP10  (*IVAMC) Number of times diagnostic output is to be given when
c   leaving *ivacr (the corrector).
c IOP11  (*IVAMC) Gives current step number of the method.  Tells how
c   many of certain coefficients must be computed. (Has nothing to do
c   with options.) = min(max integ order + 1, KDIM).  Also set when
c   starting to flag that certain memory locations must be set to 0.
c IOP12  (*IVAMC) Points to location in F() where user supplied values
c   of HINC, HDEC, HMIN, and HMAX.  (0 if option 12 not used.)
c IOP13  (*IVAMC) If not zero, reverse communication will be used for
c   getting the values of derivatives.  Associated with option 13.
c IOP14  (*IVAMC) If not zero, reverse communication will be used in
c   place of calls to the output routine.  Associated with option 14.
c IOP15  (*IVAMC) If not zero, a return will be made to the user after
c   the initialization.  Associated with option 15.  This might be used
c   to overlay *iva, some of the user's code, and perhaps *ivaop.
c IOP16  (*IVAMC) Points to location in KORD() where information for
c   specifying the error tolerance is specified.  See option 16.
c IOP17  (*IVAMC) Used in initialization for option 17, afterwards this
c   cell is used by KEXIT which is equivalenced to IOP17.
c IOP18  (*IVAMC) Points to location in KORD() where information for
c   specifying a grouping of equations for derivative evaluation is
c   stored.  See option 18.
c IOP19  (*IVAMC) Points to location in KORD() where information for
c   specifying a grouping of equations for integration order control
c   is stored.  See option 19.
c IOP20  (*IVAMC) Used for option 20, gives first location in F where
c   estimated errors are to be stored.  Expected to be useful in a
c   program for solving boundary value problems using multiple shooting.
c IOP21  (*IVAMC) Was used for stiff equations option (never completely
c   coded).  The optional code still uses this (don't activate it!).
c   Now used to flag the location if F where the user has stored the
c    tolerance to use in finding G-Stops.
c IOP21S (*IVAMC) Was used for stiff equations see above.
c IOP22  (*IVAMC) Set aside for possible option for stiff equations.
c IOP3   (*IVAMC) Value set by option 3.
c   =  0 Interpolate to final point. (The default)
c   =  1 Integrate to final point.
c   = -1 Extrapolate to final point.
c IOP4   (*IVAMC) Value set by option 4.  The output routine is called
c   with KORD(1) = 4, every IOP4 steps.  (Default value for IOP4 is a
c   very large number.
c IOP5   (*IVAMC) Value provided by option 5, used to specify extra
c   output points.
c IOP6   (*IVAMC) Value provided by option 6.  If nonzero, the output
c   routine is called at the end of every step.  If > 0, there are
c   IOP6 interpolating G-Stops.
c IOP7   (*IVAMC) Value provided by option 7.  If > 0, there are K7
c   extrapolating G-Stops.
c IOP8   (*IVAMC) Value provided by option 8.  If nonzero, the output
c   routine is called with KORD(1)=8 whenever the step size is changed.
c IOP9   (*IVAMC) Value provided by option 9.  Used to specify that the
c   user wishes to save the solution.
c IOPIVA (*IVA) Used to save length of IOPT vector for error messages.
c IOPST  (*IVASC) Intended for possible use in stiff equations.
c IOPT   (formal *IVA and IVAOP) Used to specify options.
c IOPTC  (*IVAOP) In *IVAOP equivalenced so that IOPTC(3) is equivalent
c   to IOP3.
c IOPTS  (*IVAOP) Array containing the current default values to be
c   stored into IOPTC.
c IORD   (*IVACR) Index of first equation in the current group of
c   equations grouped for selecting integration order.
c IOUTKO (*IVADC) Used in *IVADI to point to KORD to keep track of
c   equation grouping for diagnostic output.
c ISVCOM (*IVADE) Used to save info. in the common block *IVASC.
c ITERS  (*IVADE) Counts iterations in starting delay differential
c   equations.  Max. value for this is arbitrarily 100.
c ITOLEP (*IVAMC) Used for temporary storage, and for the index of a
c   tolerance relative to the start of tolerances.
c IVC1   (*IVADB) Array used for output of variables IOPST to NUMDT in
c   common block *IVASC.
c IVC2   (*IVADB) Array used for output of variables ICF to NY in
c   common block *IVAMC.
c IWB    (*IVADE) Current base index for saving F values in WDE when
c   starting delay differential equations.
c IY     (*IVAMC) Used for the current index to the Y() array.  (Local
c   variable in *IVAIN used in computing IYI.)  Equivalenced to
c   IZFLAG in *IVAG.
c IYI    (*IVAIN) Y(IYI) is currently being computed.
c IYN    (*IVAIN) Y(IYN) is base Y() corresponding to Y(IYI).
c IYNI   (*IVAIN) Used as base index for computing IYN as IY is for INI.
c IYO    (*IVADE) Points to first base value of Y for current
c   interpolation when getting values for a delay differential equation.
c IZFLAG (*IVAG)  Equivalenced to IY.  Set to 0 initially, and later
c   set to the value returned by *ZERO.
c    = 0  Value set on entry at start of search.
c    = 1  Compute next g again.
c    = 2  Normal terminiation.
c    = 3  Normal termination -- error criterion not satisfied.
c    = 4  Apparent discontinuity -- no zero found.
c    = 5  Couldn't find a sign change.
c    = 6  *ZERO was called with a bad value in IZFLAG.
c J      For temporary storage. (In *IVA,A,BU,CR,DA,DB,DE,HC,IN,OP,PR)
c J1     (*IVAA & DA) Used for temporary storage.
c J2     (*IVAA) Used for temporary storage.
c JL     (*IVA) Used for checking storage.
c JLGREP (*IVACR) Contents of first location of KORD (called LGROUP in
c   *IVACR) for the current error tolerance rule.
c JLGROR (*IVACR) Contents of first location of KORD for the current
c   integration order control.
c JLIM   (*IVA) Used for checking second item in KORD list for options
c   16 and 19.
c K      For temporary storage.  (In *IVA,A,BU,CR,DA,DB,DE,HC,IN,OP,PR)
c KDIM   Parameter giving the largest number of differences supported.
c        Used in all the routines.
c KEMAX  (*IVAMC) Index associated with equation giving the largest
c   value for (estimated error) / (requested error).
c KEXIT  (*IVAMC) Equivalenced to IOP17 which is not used after
c   initialization.  Defines actions when KORD2I = -7.  (Referenced in
c   (*IVAA,DA,G).)
c   =  1  Take the step over with reduced H.
c   =  2  Take the step over.
c   =  3  Do the end of step call to OUTPUT.
c   =  4  Reset TMARK, then do same as for KEXIT = 2.
c   =  5  Reset TMARK, then do same as for KEXIT = 3.
c   =  6  Give the fatal error diagnostic.
c KFERR  (*IVA)  Temporary storage in checking for option 16.
c KGO    (*IVA)  Used to tell from whence a check is being done or an
c   error message is being written.
c   = 1 Checking an equation group for variational equations.
c   = 2 Checking an equation group for diagnostic print.
c   = 3 Checking an equation group for integration order control.
c   = 4 Checking an equation group for error control.
c   = 5 Checking an equation group for specifying ODE orders.
c   = 6 Found a problem with output type for printing.
c   = 7 Found a problem with an output group for printing.
c   = 8 Found a problem with input NEQ.
c   = 9 Order specified for the ODE's in the system is out of range.
c   =10 Option 16 was not used (an error).
c   =11 Error tolerance of 0 specified without proper flags.
c KIS    (*IVAMC) Used to check if it is time to dump the solution.
c   The check involves incrementing KIS at the end of the step, and
c   dumping the solution if KIS is 0.
c   = -1  Set in *ivacr when it is time to dump solution
c   =  0  When starting
c   =  2  After being dumped.
c   This is set to 1000 just after a user specified discontinuity, and
c   counted up from that point.
c KMARK  (*IVAMC) Identifies the type of output associated with the next
c   output point specified by TSPECS.
c KONV   (*IVADE) Counts iterations.  Test for convergence if KONV > 1.
c KORD   (formal in *IVA,A,BU,CR,DA,DB,DE,G,IN,PR) KORD(1) is used to
c   return flags for the user to test, and KORD(2) tells what routine
c   the flag is associated with.  See KORD1I and KORD2I below and the
c   write up for the program.  KORD(3) is used for communicating extra
c   information to the user in some cases.  KORD(4) to KORD(NTE+3) are
c   used for integration order for the equations, and the rest of KORD()
c   is available for user options.
c KORD1I (*IVAMC) Helps in defining the state of the integrator.
c   Frequently has the same value as KORD(1).  Meaning depends on the
c   value of KORD(2), or the value about to be assigned to KORD(2).
c   <  0  Happens when preparing to give output with extrapolation.
c   =  0  Happens when checking F at points for noise test.
c   =  1  (KORD(2)=-1)  End of integration has been reached.
c   =  1  (KORD(2)= 0)  Computing first predicted derivative.
c   =  1  (KORD(2)= 1)  Output for initial point.
c   =  2  (KORD(2)=-1)  Giving diagnostic for noise limiting precision.
c   =  2  (KORD(2)= 0)  Computing corrected derivative.
c   =  2  (KORD(2)= 1)  Output for TSPECS(3).
c   =  3  (KORD(2)=-1)  Diagnostic for step size reduction too fast.
c   =  3  (KORD(2)= 0)  Computing variational derivative.
c   =  3  (KORD(2)= 1)  Output for TSPECS(4).
c   =  4  (KORD(2)=-1)  Error, discontinuity.
c   =  4  (KORD(2)= 1)  Output for certain number of steps.
c   =  5  (KORD(2)= 0)  Get initial derivatives for stiff equations.
c   =  5  (KORD(2)= 1)  Extra output from TSPECS.
c   =  6  (KORD(2)= 1)  End of step output.
c   =  7  (KORD(2)= 0)  Evaluate G before extrapolated output point.
c   =  7  (KORD(2)= 1)  Evaluate G before extrapolated output point.
c                       (Also used when checking for other G's after
c                        finding one.)
c   =  8  (KORD(2)= 1)  Tell user step size has changed.
c   =  9  (KORD(2)= 1)  Request for user to save solution.
c   = 11  (KORD(2)=-1)  Error, step size too small at end of start.
c   = 12  (KORD(2)=-1)  Error, step size is too small.
c   = 13  (KORD(2)=-1)  Error, output points specified badly.
c   = 21  (KORD(2)=-1)  H too small to give reasonable change when added
c                       to T.
c   = 22  (KORD(2)=-1)  Error, bad tolerance.
c   = 23  (KORD(2)=-1)  Set after message for a fatal error.
c   = 24  Set on error message in *iva, along with KORD2I = -4.
c   Also used as an index into MLOC in *IVAA when an error is being
c   processsed, see MLOC below.
c KORD2I (*IVAMC) Helps in defining the state of the integrator.
c   Frequently has the same value as KORD(2).
c   = -3  Set in *ivag, to get a derivative evaluation.
c   = -2  Set in *ivag, to get another entry to OUTPUT.
c   = -1  Return to calling program, done, interrupt, or got an error.
c   =  1  Calling OUTPUT or returning to user for OUTPUT type action.
c   =  0  Calling DERIVS or returning to user for DERIVS type action.
c   = -4  Error message in *iva and in *ivaop, along with KORD1I = 24.
c   = -5  Starting
c   = -6  Starting, getting the initial derivative value or derivatives
c         for the noise test.
c   = -7  Done some extrapolation, KEXIT defines the action to take.
c         Set in *ivag to activate KEXIT action in *iva.
c   = -8  Set when user has requested adjustment of the difference
c         tables for a discontinutiy.
c KORDI  (*IVASC) Order of differential equation being integrated.  If
c   all orders are the same, this set once at the beginning.
c KOUNT   (*IVADE) Count of number of points back from the initial point
c   when solving a delay differential equation.
c KOUNTM  (*IVADE) Largest value currrently allowed for KOUNT.
c KOUNTX  (*IVADE) Largest value allowed for KOUNTM.
c KOUTKO  Used in DIVACR to track where output is wanted.
c KPRED  (*IVAMC) Value assigned to KORD1I when getting a predicted
c   derivative.  (1 used now, 5 planned for use with stiff equations.)
c KQD    (*IVACR) = max(2, integration order)
c KQDCON (*IVAMC) Number of coefficients computed with constant step
c   size for stiff equations.
c KQICON (*IVAMC) Number of coefficients computed with constant step
c   size for nonstiff equations.
c KQL    (*IVACR) Integration order at start of (*IVACR)
c KQLORD (*IVACR) Saved value of KQL when equations are grouped for
c   controlling the integration order.
c KQMAXD (*IVASC) Maximum integration order used for stiff equations.
c KQMAXI (*IVASC) Maximum integration order used for nonstiff equations.
c KQMAXS (*IVAMC) Maximum integration order for equations that have
c   some limit on the error that can be committed.
c KQMXDS (*IVAMC) Used to save KQMAXD in case step is repeated and the
c   solution must be dumped.
c KQMXI  (*IVAIN) Maximum integration order used for integration or
c   interpolation, = KQMAXI+INTERP-1.
c KQMXS  (*IVAIN) Maximum step number, = max(KQMXI, KQMAXD).
c KQMXIL (*IVAMC) Value of KQMAXI the last time integration coefficients
c   were computed.
c KQMXIP (*IVAMC) = KQMAXI + MAXINT, for computing integration coeffs.
c KQMXIS (*IVAMC) Used to save KQMAXI in case step is repeated and the
c   solution must be dumped.
c KQN    (*IVACR) Value of integration order at end of *IVACR.
c KQQ    Used for the integration order for current equation.  (Values
c   < 0 are intended for stiff equations.)  (In *IVA,BU,DA,IN,PR)
c KSC    (*IVAMC) Number of steps that have been taken with a constant
c   step size.
c KSOUT  (*IVAMC) When KSTEP reaches this value, the output routine is
c   called with KORD(1) = 4.  The default value is a very large number.
c KSSTRT (*IVAMC) Set when ending one derivative per step to KSTEP + 2.
c   Checked later in *IVAHC to decide whether to set the step changing
c   factors to their nominal values.
c KSTEP  (*IVAMC) Number of steps taken since the start of integration.
c L      Used for temporary storage.  In *IVAIN, L is the initial value
c   of LDT, except L=1 if LDT=-1, and MAXINT .ge. 0.  (Used in *IVAA,BU
c   CR,DA,DB,IN,PR.)
c LAHAG  (*IVADB) Used to get proper offset into an diagnostic message.
c LAIAG  (*IVADB) Used to get proper offset into an diagnostic message.
c LDIS   (*IVAA) Count of steps since user flagged a discontinuity.
c LDT    (*IVASC) Used to keep track of state of difference table.
c   = -5  Used only on first step to indicate that an extra iteration
c         is desired to get a firm estimate on the error.
c   = -4  Set on initialization before there is any difference table.
c   = -3  Set just after predicting, interpolation is not allowed when
c         this value is set.
c   = -2  Set when difference table is to be updated to the end of the
c         current step, but no interpolation is to be done.  (For
c         dumping the solution.)
c   =  0  Calculations for current step are complete, it is not o.k. to
c         update the difference table.
c   =  1  Difference table has been updated to the end of the current
c         step, e.g. by doing an interpolation.
c   =  2  Set when doing a special interpolation during computation of
c         derivatives.  (For delay equations.)
c LEX    (*IVAMC) Indicates how to get values at next output point:
c   = -1  Extrapolate
c   =  0  Interpolate (The usual case.)
c   =  1  Integrate to the output point, integration is not continued.
c LGO    (*IVAIN) Used as an an assigned go to.  Result is to add in
c   extra correction term when LDT has been set to 2.
c LGROUP (formal) This is a part of KORD passed into *IVACR.  The first
c   location is the start of the information on the grouping of
c   equations for error control.
c LINC   (*IVAMC) Used to indicate state of step size selection.
c   = -10 After computed derivatives at base time, after computing other
c         extra derivatives for the noise test.
c   = -9  After computed second extra derivative for noise test.
c   = -8  After computed first extra derivative for noise test.
c   = -7  Dumping the solution and then doing a user initiated restart,
c         or getting ready to compute extra derivatives for the noise
c         test.
c   = -6  Dumping the solution before a restart.
c   = -5  Set on the first step, and also set when dumping the solution
c         after a discontinuity.
c   = -4  Repeat step with no change in the step size.
c   = -3  Set when the error tolerance is set improperly.
c   = -2  User has complete control of selecting the step size.
c   = -1  Step is being repeated.
c   =  0  Step size is not to be increased on this step.
c   = k>0 Step size can be increased by HINCC**k.
c LINCD  (*IVAMC) Value of smallest k for which HINCC**k .ge. 2.
c   (=-2 if user is specifying all step size changes.)
c LINCQ  (*IVAMC) Value of smallest k for which HINCC**k .ge. 4.
c LIOPT  (*IVAOP) Value of the last index in IOPT on the last call.
c   Used so *IVA can print IOPT in error messages.
c LL     (*IVACR) Temporary variable used when equations are grouped
c   for integration order control.
c LNOTM1 (*IVAIN) Logical variable = L .ne. -1.  If LNOTM1 is true,
c   storage in Y() is different in some way lost to antiquity.  Such
c   a case can only arise in the case of stiff equations.
c LOCF1  (*IVADB) Gives packed data needed for output of tables by the
c   message processor MESS.  See comments there under METABL for defs.
c LOCF2  (*IVADB) As for LOCF1 above.
c LOCM   (*IVAA) Parameter = 32*256, used to unpack integers stored
c   in MLOC for use in error message processing.
c LPRINT (formal, *IVADB) Defines how much printing is to be done in
c   *IVADB.  Let |LPRINT| = 10*N1 + N2     (N1,N2 digits)
c    N1=1   Do not print any variables external to the integrator.
c    N1=2   Print  tspecs, current y, past y, current f, all pertinent
c           contents of KORD, and TOL.
c    N1=3   Above + difference tables up to highest difference used.
c    N1=4   Same as N1=1 + all in storage allocated for differences.
c    N2=1   Do not print any variables internal to the integrator.
c    N2=2   Print all scalar variables in interpolation common block.
c    N2=3   Above + all scalar variables in main integ. common block.
c    N2=4   Same as N1=3 + all used in arrays XI,BETA,ALPHA, first
c           column of G, GS,RBQ,SIGMA
c    N2=5   Same as N1=4 + all used in arrays G,D,DS,V
c LSC    (*IVAMC) Indicates if starting or if noise may be present.
c   =k<0 -k steps have been taken for which noise appears to be limiting
c        the precision.
c   = 0  Usual case
c   = 1  Doing 1 derivative per step after initial part of start.
c   = 2  Used as flag that it is time to set LSC=0.
c   = 3  Third step, hold the order constant.
c   = 4  Second step, increase orders from 2 to 3.
c   = 5  First step, third time through the first step (if required).
c   = 6  First step, second time through.
c   = 7  First step, first time through.
c   = 8  Set on initialization.
c LTXT?? Names of this form are used in setting up data statements for
c   error messages.  These names are generated automatically by PMESS,
c   the program that makes up these messages.
c LX     (*IVAA) Used for temporary storage in computing TMARKA().
c        ( formal *IVADE)  An integer array containing extra
c   information, as follows.
c  LX(1) Points to a location in Y beyond those already in use.  Values
c        of Y requested are computed at TSPECS(1) - Y(LX(1)) and stored
c        starting at Y(LX(1)+1).  If this index is 0, no more extra Y
c        values are to be computed.
c  LX(2) Index of the first equation for which the Y's above are to be
c        computed.  Y(LX(1)+1) will correspond to this first equation
c        index.
c  LX(3) Index of the last equation for which the Y's above are to be
c        computed.  Thus the Y's stored starting at Y(LX(1)+1) will
c        require no more space than half the space ordinarily required
c        for the array Y(), and may require significantly less.
c  LX(4) Maximum number of times to integrate F to get Y.  This should
c        be > 0, and less than or equal to the order of the highest
c        order differential equation.  (= 0 is allowed, but probably
c        not what you want.  It would give a value only for F.)  Space
c        must be set aside for all integrals of F, even if not all are
c        requested.  For a first order system, all Y's are just the
c        first integrals of the corresponding F's.  For higher order
c        equations, the first Y associated with a given F is the d-th
c        integral of the corresponding F, where d is the order of the
c        equation, and the last Y corresponding to the F is the first
c        integral of that F.
c  LX(5) As for LX(4), but gives the index for the fewest number of
c        times to integrate F.  Ordinarily this should be > 0.  If 0 is
c        requested, an estimate for the value of F at the delay point is
c        computed.  This should not be 0 more than once, for equations
c        covering the same index, since later such requests would write
c        over the earlier results.
c  LX(5i+k) , k = 1, 2, ... 5.  Treated as for the cases above.  If i
c        different cases of delayed Y's are to be computed, then
c        LX(5i+1) must be 0.
c LX2    (*IVADE) Value of LX(5i+2), when working on the i-th delay.
c MACT   Used in the programs which call the error message program.
c   This array difines the actions to be taken by that program.  (In
c   (*IVA,A,DA,DE,G,IN,OP)
c MACT0  (*IVADB) Used to call the message program, see MACT.
c MACT?  As for MACT, in (*IVA,CR,DB)
c MACTFV (*IVADB) As for MACT0.
c MAXDIF (*IVASC) Maximum differentiations required for stiff equations.
c MAXINT (*IVASC) Maximum integrations required.  (= max. order of
c   differential equations if equations are not stiff.)
c MAXKQ  (*IVA, BU)e
c MAXKQD (*IVAMC) Largest integration order allowed for stiff equations.
c MAXKQI (*IVAMC) Largest integ. order allowed for nonstiff equations.
c ME???? Parameters defining constants used for interaction with the
c   error message program MESS.  See comments there for definitions.
c   (In *IVA,A,DA,DE,G,IN,OP)
c METHOD (*IVAMC) Defines kind of methods being used.
c   = -1  Only stiff equations are being integrated.
c   =  0  Only nonstiff equations are being integrated.
c   =  1  Both kinds of methods are required.
c MLOC   (*IVA,A,DE) Contains locations in MTEXT for error messages.  In
c   *IVAA this data is packed using MLOC??, see below.
c MLOC?? (*IVAA) Parameters constructed to aid in making up packed data
c   for processing error messages.  Low two digits give the value of
c   KORD1I to use for the error index and later processing, the next two
c   give the error severity level, and the rest point to text used for
c   the message.
c MODF2  (*IVADB) Used in constructing the same kind of packed data as
c   described for LOCF1 above.
c MULTJ  Local to DIVAOP for calls not using F.
c MTEXT  (*IVA,A,CR,IN,OP) Text for error messages.
c MTXT?? (*IVA,A,CR,DA,DB,DE,G,IN,OP) Equivalenced into MTEXT.
c N      Used for temporary storage.  (In *IVAHC,IN,PR)
c NDTF   (*IVASC) Location in F() where difference table starts.
c NE     (*IVAMC) Number of equations in the first group.  (=NTE if
c   option 18 is not used.)
c NEDDIG (*IVADB) Parameter = -MEDDIG.
c NEPTOL (*IVAMC) Used for temporary storage and to save the value of
c   ITOLEP for error messages.
c NEQ    (formal) Total number of equations being integrated.
c NG     (*IVAMC) Used in *ivag for the number of g's in the current
c   context.
c NGSTOP (*IVAG) Dimension 2 array equivalenced to IOP6, and IOP7.  To
c   get the number of interpolating and extrapolating G-Stops.
c NGTOT  (*IVAMC) NGTOT(1) gives the number of interpolating G-Stops,
c   and NGTOT(2) gives the number of extrapolating G-Stops.
c NKDKO  (*IVASC) If this is nonzero (option 17), it gives the location
c   in KORD() where a vector defining the order of each equation is
c   specified.
c NLX    (*IVADE) Temporary index used to keep track of interpolations
c   being done to get Y() values for a delay differential equation.
c NOISEQ (*IVAMC) max(2, order of equation for which (error estimate)/
c   (error requested) is a maximum).
c NOUTKO (*IVAMC) If nonzero, gives the index in KORD where information
c   on what equations are to be included in the diagnostic output is
c   given.   See option 10.
c NSTOP  (formal) In *IVAG.  Index of the G-stop, see IFLAG.
c NTE    (*IVASC) Total number of equations being integrated = NEQ.
c NTEXT  (formao *IVADB) Character variable containing heading text.
c NTOLF  (*IVAMC) First location in F() where tolerance specifying
c   accuracy desired is stored.
c NUMDT  (*IVASC) Maximum allowed number of differences available for
c   doing an integration.
c NXTCHK (*IVA) Equivalenced to INTCHK(1), which gives the next
c   available location in INTCHK for storing data on storage allocation.
c NY     (*IVAMC) Total order of the system.
c NYNY   (*IVASC) Location in Y() where the base value for Y() is saved.
c NYNYO  (*IVADE) Equivalenced to the saved value from common of NYNY.
c OUTPUT (formal) Name of subroutine to be called for the output of
c   data or for computing G-Stops.  Not used if option 14 is set.
c OVD10  (*IVAEV) (largest floating point number) / 10.
c OVTM75 (*IVAEV) (largest floating point number) ** (-.75)
c RBQ    (*IVAMC) Array containing data for the preliminary noise test.
c RD     (formal *IVACO) Array use to contain floating point data from
c   common.  Values are returned in locations 1 to 3 as follows.
c   1    EMAX =  Max. ratio of estimated error to requested error
c   2            Reserved for future use
c   3            Reserved for future use
c REF    (*IVACR) Array of length 3 used for translating error tolerance
c   type into the factor used for exponential averaging for that type.
c RND    (*IVACR) Usually the current estimated error.  Used in deciding
c   if noise is limiting precision.
c RNOISE (*IVACR) Value used in comparison with RBQ() for preliminary
c   noise test.
c ROBND  (*IVAMC) Used to influence the selection of integration order.
c   The larger ROBND, the harder it is to increase the order and the
c   easier it is to decrease it.
c RVC2   (*IVADB) Array used for output of variables DNOISE to SNOISE in
c   common block *IVAMC.  These are variables that don't require a great
c   deal of precision.
c S      (*IVACR) Estimate of (step size) * eigenvalue of Jacobian.
c SIGMA  (*IVAMC) The k-th entry of this array contains a factor that
c   gives the amount the k-th difference is expected to increase if the
c   step size in increased.  These numbers get bigger it there is a past
c   history of increasing the step size.
c SIGMAS (*IVAA) Saved value of SIGMA(k) from the last step, where k =
c   integration order for equation with index KEMAX.
c SNOISE (*IVAMC) Value used in comparison with RBQ() on equation with
c   largest value for (error estimate) / (error requested).
c T      (formal) in *IVAIN. T(1) contains the point to be interpolated
c   to, and T(2) is used in a check that |HI| .le. |T(2)|.  When used by
c   other routines in this package, TSPECS is passed in for T.
c TB      (*IVADE) Base time for current interpolation.
c TC      (*IVADE) Original value of TN when getting past Y's for a
c   delay differential equation.
c TEMP   Used for temporary storage, in *IVAHC,PR
c TEMPA  (*IVACR) Array equivalenced to (TPS1,TPS2,TPS3,TPS4).
c TEMPAO (*IVACR) Array used to accumulate values in TEMPA.
c TG     (*IVAMC) TG(1) gives the last value of TSPECS(1) for which an
c   interpolatory G-Stop has been computed.  TG(2) is defined similarly
c   for extrapolatory G-Stops.
c TGSTOP (*IVAMC) TGSTOP(1) gives the value of TSPECS(1) where the last
c   0 for an interpolatory G-Stop was found.  TGSTOP(2) is defined
c   similarly for extrapolatory G-Stops.
c TMARK  (*IVAMC) Location of the next output point.
c TMARKA (*IVAA)  Array of length 2 equivalenced to TMARK (and TMARKX).
c TMARKX (*IVAMC) Location of the next output point to be found using
c   integration or extrapolation.  This variable must follow immediately
c   after TMARK in the common block.
c TN     (*IVASC) The value of TSPECS(1) at the conclusion of the last
c   step.
c TNEQ   (*IVADB) Array of dimension 1 equivalenced to TN so that an
c   array can be passed to *MESS.
c TOL    (formal) This is a part of F passed into *IVACR.  The first
c   location is the start of the information on the tolerances for error
c   control.
c TOLD   (*IVAG) Value of TSPECS(1) on one side of a zero.
c TOLG   (*IVAMC) Tolerance to pass to dzero when locating G-Stops.
c TOUT   (*IVAMC) Location of next output point defined by value of
c   TSPECS(3).  Such output is given with KORD(1) = 2.
c TP     (*IVA,A,DA,DE,HC) Used for temporary storage.
c TP1    (*IVAA,DA,HC,IN,PR) Used for temporary storage.
c TP2    (*IVAA,DA,HC,PR) Used for temporary storage.
c TP3    (*IVAA) Used for temporary storage.
c TPD    (*IVABU) Used for temporary storage.
c TPP    (*IVACR) Used for temporary storage.  Usually same as TPS3.
c TPS1   (*IVAA,CR) Used for temporary storage.  (In *IVACR is the
c   difference of order KQQ-2)
c TPS2   (*IVAA,CR) Used for temporary storage.  (In *IVACR is the
c   difference of order KQQ-1)
c TPS3   (*IVACR) Contains the difference of order KQQ.  This is the
c   last difference used in the corrector.
c TPS4   (*IVACR) Contains the difference of order KQQ+1.
c TPS5   (*IVACR) Temporary storage.
c TPS6   (*IVACR) Temporary storage.
c TPS7   (*IVACR) Temporary storage.
c TSAVE  (*IVAG) Value of TSPECS(1) before starting the search for a 0.
c TSPECS (formal *IVA,A,DB,DE,G)
c   TSPECS(1) is the current value of the independent variable.
c   TSPECS(2) is the current value of the step size.
c   TSPECS(3) is the increment to use between output points that give
c             output with KORD(1) = 2.
c   TSPECS(4) is the "final" output point.
c V      (*IVAMC) Array used in computing integration coefficients.
c XI     (*IVASC) XI(K) = TSPECS(1) - value of TSPECS(1) K steps
c   previous.
c W      (*IVAHC) Array used in computing integration coefficients.
c WDE    (formal, *IVADE)  Array used for working storage.  This storage
c   is used to save derivative values when iterating to get started.  To
c   be safe one should allow as much space as is allowed for differences
c   in F.  In most cases the start will not require this much space
c   however.  This array is also intended for the support of saving long
c   past histories.
c Y      (formal, *IVA,A,CR,DA,DB,DE,G,IN,PR) Array containing the
c   independent variable and all derivatives up to order one less than
c   the order of the differential equation.  Also use to save these
c   values at the beginning of the current step, the base values.
c YN     (formal, in *IVAPR)  Base values of y, these follow the
c   current values of the dependent variable, y, in Y().
c
c
c++S Default KDIM = 16
c++  Default KDIM = 20
c++  Default MAXORD = 2, MAXSTF = 1
c++  Default INTEGO, VAREQ, OUTPUT, DUMP, GSTOP, EXTRAP
c++  Default STIFF=.F., ARGM=.F., ERRSTO=.F.
c
      integer NEQ, IDIMT, IDIMY, IDIMF, IDIMK
      integer KORD(*), IOPT(*)
c--D Next line special: P=>D, X=>Q
      double precision TSPECS(*), Y(*)
      double precision F(*)
      external DIVAF, DIVAO
c
c *********************** Internal Variables ***************************
c
c Comments for variables used in this package can be found in the file
c   IVACOM.
c
c *********************** Type Declarations ****************************
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT, TOLG
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2
      double precision EEPT75, EOVEP2, OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / DIVAEV /
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,TOLG,HC,HDEC,HINC,
     1   HINCC,HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c
      integer KGO, INTCHK(0:30), NXTCHK
      integer IHI, JL, J, ILOW, K, KQQ, JLIM, KFERR
c
      equivalence (INTCHK(1), NXTCHK)
      double precision CM1
      parameter (CM1 = (-1.D0))
      integer IOPIVA(2)
      save IOPIVA
c
c                      Declarations for error message processing.
c
      character TEXT1(1)*10
      integer  MENTXT,MEIDAT,MEMDA1,MECONT,MERET,MEEMES,METEXT,MEIVEC
      parameter (MENTXT =23)
      parameter (MEIDAT =24)
      parameter (MEMDA1 =27)
      parameter (MECONT =50)
      parameter (MERET  =51)
      parameter (MEEMES =52)
      parameter (METEXT =53)
      parameter (MEIVEC =57)
c
      integer MACT(16), MLOC(12), MACT1(4)
c
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DIVA$B
cAB The interval [1, 10**6], bounds the allowed values for NTE=$I.$E
cAC For option $I, the interval [$I, $I], bounds the allowed $C
c   values for the integration order which is set to $I.$E
cAD Option 16 must be used for error control.$E
cAE F($I) = $F, but it must be -1.0 when skipping the error check.$E
cAF For option $I, the interval [$I, $I] bounds the allowed $C
c   values for KORD($I)=$I, which is used to specify an $B
cAG output type for printing.$E
cAH output group for printing.$E
cAI equation group for variational equations.$E
cAJ order for a differential equation.$E
cAK equation group for diagnostic print.$E
cAL equation group for integration order control.$E
cAM equation group for error control.$E
cAN Option 5 argument must be .le. 0 or .gt. 4.$E
c   $
cAO KORD values for this option starting at KORD($M) are:$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM,LTXTAN,LTXTAO
      parameter (LTXTAA=  1,LTXTAB=  7,LTXTAC= 71,LTXTAD=183,LTXTAE=226,
     * LTXTAF=290,LTXTAG=400,LTXTAH=427,LTXTAI=455,LTXTAJ=498,
     * LTXTAK=535,LTXTAL=573,LTXTAM=620,LTXTAN=655,LTXTAO=  1)
      character MTXTAA(3) * (233)
      character MTXTAB(1) * (55)
      data MTXTAA/'DIVA$BThe interval [1, 10**6], bounds the allowed val
     *ues for NTE=$I.$EFor option $I, the interval [$I, $I], bounds the$
     * allowed values for the integration order which is set to $I.$EOpt
     *ion 16 must be used for error control.$EF($I) = ','$F, but it must
     * be -1.0 when skipping the error check.$EFor option $I, the interv
     *al [$I, $I] bounds the allowed values for KORD($I)=$I, which is us
     *ed to specify an $Boutput type for printing.$Eoutput group for pri
     *nting.$Eequation gro','up for variational equations.$Eorder for a$
     * differential equation.$Eequation group for diagnostic print.$Eequ
     *ation group for integration order control.$Eequation group for err
     *or control.$EOption 5 argument must be .le. 0 or .gt. 4.$E'/
      data MTXTAB/'KORD values for this option starting at KORD($M) are:
     *$E'/

c End of automatically generated error message code.
c
c        for KGO =     1      2      3      4      5      6      7
      data MLOC / LTXTAI,LTXTAK,LTXTAL,LTXTAM,LTXTAJ,LTXTAG,LTXTAH,
     2            LTXTAB,LTXTAC,LTXTAD,LTXTAE, LTXTAN /
c           KGO        8      9     10     11      12
c
c                      1  2  3 4       5  6       7       8       9 10
      data MACT / MEEMES,38,24,0, MENTXT, 0, METEXT, MECONT, MEMDA1,0,
     1  METEXT, MEIDAT,0, MEIVEC,0, MECONT /
c           11      12 13     14 15     16
      data MACT1 / METEXT, MEIVEC, 0, MERET /
      data IOPIVA(1) / 1111 /
      data TEXT1 / 'IOPT()= $B' /
c
c ************** START OF EXECUTABLE CODE ******************
c
c     **** TEST IF CONTINUING AN INTEGRATION
      if (KORD(1) .ne. 0) go to 330
c     **** INITIALIZE VARIOUS SCALARS
      KSTEP = 0
      KQMXIS = 0
      KORD2I = -5
      KORD(2) = -1
      NTE = NEQ
      NE = NTE
      TOLG = 0.D0
c     **** SET UP OPTIONS
      if (IOPT(1) .ne. 0) call DIVAOP(IOPT, F)
      call DIVAOP(IOPIVA, F)
      if (IOPT(1) .eq. 0) IOPIVA(2) = 1
c
      if ((NE .le. 0) .or. (NE .gt. 1000000)) then
         IDAT(1) = NE
         KGO = 8
         go to 650
      end if
c                         Set up diagnostic print on storage allocation.
      INTCHK(0) = 245
      if (IOP10 .ne. 0) INTCHK(0) = 247
c
c     **** CHECK TSPECS STORAGE ALLOCATION
      INTCHK(2) = IDIMT
      INTCHK(3) = 4
      NXTCHK = 4
      if (IOP5 .ne. 0) then
         INTCHK(4) = 5
         INTCHK(5) = 5
         if (IOP5 .gt. 0) then
            INTCHK(6) = IOP5 - 4
            if (IOP5 .lt. 5) then
               KGO = 12
               go to 600
            end if
         else
            IHI = -IOP5
            JL = 4
            do 15 IHI = IHI, IDIMK-3, 3
               J = abs(KORD(IHI))
               if (J .eq. 0) go to 20
               if (abs(KORD(IHI + 2)) .gt. 1) then
                  IDAT(2) = -1
                  IDAT(3) = 1
                  KGO = 6
                  go to 600
               end if
               if ((J .le. JL) .or. (J .gt. KORD(IHI+1))) then
                  KGO = 7
                  IDAT(2) = JL + 1
                  IDAT(3) = KORD(IHI+1)
                  go to 610
               end if
               JL = KORD(IHI+1)
   15       continue
            if (KORD(IHI) .ne. 0) IHI = IHI + 3
   20       INTCHK(6) = JL - 4
         end if
         NXTCHK = 7
      end if
   25 call OPTCHK(INTCHK, IOPT, 'DIVA / TSPECS$E')
      if (NXTCHK .lt. 0) KORD2I = -4
c
c     **** CHECK KORD STORAGE ALLOCATION
      INTCHK(2) = IDIMK
      INTCHK(3) = NE + 3
      NXTCHK = 4
      if (IOP5 .lt. 0) then
         INTCHK(4) = 5
         INTCHK(5) = -IOP5
         INTCHK(6) = IHI + IOP5
         NXTCHK = 7
      end if
c
c++  Code for VAREQ is active
      if (IOP18 .ne. 0) then
         NE = abs(KORD(IOP18))
         INTCHK(NXTCHK) = 18
         ILOW = IOP18
         KGO = 1
c.       **** CHECK OPTION FOR VALID INPUT
         go to 430
      end if
c++  End
   30 continue
      if (NKDKO .ne. 0) then
c                        **** STORAGE ALLOCATED FOR ODE ORDERS
         INTCHK(NXTCHK) = 17
         INTCHK(NXTCHK+1) = NKDKO
         INTCHK(NXTCHK+2) = NTE
         NXTCHK = NXTCHK + 3
      end if
c++  Code for STIFF is inactive
c      IF (IOPST .ne. 0) then
c         INTCHK(NXTCHK) = 17
c         INTCHK(NXTCHK+1) = IOPST
c         INTCHK(NXTCHK+2) = NTE
c         NXTCHK = NXTCHK + 3
c      end if
c++  End
c
c **** SET INITIAL INTEGRATION ORDERS, TEST ODE ORDERS ****
c
      MAXINT = 0
      MAXDIF = 0
      NY = 0
      do 80 K = 1, NTE
         if (NKDKO .ne. 0) KORDI = KORD(NKDKO + K - 1)
         NY = NY + abs(KORDI)
c++  Code for STIFF is inactive
c      IF (IOPST .EQ. 0) GO TO 60
cc.    **** CHECK FOR POTENTIAL STIFF EQUATION
c      JS = abs(KORD(IOPST+K-1)) - 1
c      IF ( JS ) 52,60,54
cc.    **** EQUATION IS NOT ACTIVE
c   52 KQQ = 0
c      GO TO 56
cc.    **** EQUATION USES IMPLICIT METHOD
c   54 KQQ = -1
c      IF (JS .GT. abs(KORDI)) then
c        Set up an error message.
c      end if
c      MAXINT = max(MAXINT, abs(KORDI) - JS)
c   56 IF (KORDI .GE. 0) GO TO 70
c      KORDI = -1 - KORDI
c      JS = JS - 1
c      MAXDIF = max(MAXDIF, JS, 1)
c      GO TO 70
c++  End
c     **** EQUATION IS TO USE AN EXPLICIT METHOD
   60    KQQ = 1
         MAXINT = max(MAXINT, KORDI)
   70    if ((KORDI .gt. MAXORD) .or. (KORDI .le. 0)) then
c                    Set up error message.  KORDI is out of range.
            IDAT(1) = 17
            IDAT(2) = 1
            IDAT(3) = MAXORD
            if (NKDKO .ne. 0) then
               KGO = 5
               ILOW = NKDKO
               IHI = NKDKO + K - 1
               go to 640
            else
               KGO = 9
               IDAT(4) = KORDI
               go to 650
            end if
         end if
         KORD(K + 3) = KQQ
   80 continue
c     **** SET FLAGS WHICH DEPEND ON METHOD USED
c++  Code for STIFF is inactive
c      METHOD = 1
c      IF (MAXINT .GT. 0) IF (MAXDIF) 85,90,85
c      METHOD = -1
c   85 CONTINUE
c      KPRED = 5
c      GO TO 100
c++  End
   90 METHOD = 0
      KPRED = 1
  100 continue
c
c ******* CHECK KORD FOR DIAGNOSTIC OUTPUT CONTROL *********
c
c++  Code for OUTPUT is active
      if (IOP10 .gt. 0) then
         if (NOUTKO .ne. 0) then
            INTCHK(NXTCHK) = 10
            ILOW = NOUTKO
c.    **** Check option for valid input
            KGO = 2
            go to 430
         end if
      end if
c++  End
  110 continue
c
c ********** CHECK KORD FOR INTEGRATION ORDER CONTROL ******
c
c++  Code for INTEGO is active
      if (IOP19 .ne. 0) then
c.           **** Check option for valid input
         INTCHK(NXTCHK) = 19
         ILOW = IOP19
         JLIM = -30
         KGO = 3
         go to 430
      end if
c++  End
  120 continue
c
c ********** CHECK SET UP FOR ERROR TOLERANCES *************
c
      INTCHK(NXTCHK) = 16
      ILOW = IOP16
      JLIM = -5
      KGO = 4
      if (IOP16 .ne. 0) go to 430
c.                      **** IN CURRENT CODE, IOP16=0 IS AN ERROR
      KGO = 10
      go to 650
  150 continue
c     **** CHECK KORD STORAGE ALLOCATION
      call OPTCHK(INTCHK, IOPT, 'DIVA / KORD$E')
      if (NXTCHK .lt. 0) KORD2I = -4
c
c     ******** DONE CHECKING KORD STORAGE ALLOCATION *******
c
c     **** CHECK  Y  STORAGE ALLOCATION
      INTCHK(2) = IDIMY
      INTCHK(3) = NY + NY
      NXTCHK = 4
      NYNY = NY + 1
      call OPTCHK(INTCHK, IOPT, 'DIVA / Y$E')
      if (NXTCHK .lt. 0) KORD2I = -4
c
c     **** CHECK  F  STORAGE ALLOCATION
      INTCHK(2) = IDIMF
      INTCHK(3) = NTE
      NXTCHK = 4
      if (IOP16 .ne. 0) then
c                                Error tolerance info.
         INTCHK(4) = 16
         INTCHK(5) = NTOLF
         INTCHK(6) = IHI - IOP16 + 1
         NXTCHK = 7
      end if
      if (IOP12 .gt. 0) then
         INTCHK(NXTCHK) = 12
         INTCHK(NXTCHK+1) = IOP12
         INTCHK(NXTCHK+2) = 4
         NXTCHK = NXTCHK + 3
      end if
      if (IOP21 .gt. 0) then
         INTCHK(NXTCHK) = 21
         INTCHK(NXTCHK+1) = IOP21
         INTCHK(NXTCHK+2) = 1
         NXTCHK = NXTCHK + 3
      end if
c
c++  Code for ERRSTO is inactive
c      IF (IOP20 .ne. 0) then
cc.                                Space for saving error estimates
c         INTCHK(NXTCHK) = 20
c         INTCHK(NXTCHK) = IOP20
c         INTCHK(NXTCHK) = NTE
c         NXTCHK = NXTCHK + 3
c      end if
c++  Code for STIFF is inactive
c      if (IOP21 .gt. 0) then
cc.                               Info. for stiff equations
c         INTCHK(NXTCHK) = 21
c         INTCHK(NXTCHK+1) = IOP21
c         INTCHK(NXTCHK+2) = IOP21S
c         NXTCHK = NXTCHK + 3
c      end if
c      MAXKQD = min(MAXKQI, 6)
c++  End
c                          Set aside space for the difference tables.
      INTCHK(NXTCHK) = 0
      INTCHK(NXTCHK+1) = -KDIM * NTE
      INTCHK(NXTCHK+2) = 0
      NXTCHK = NXTCHK + 3
      INTCHK(NXTCHK) = -5 * NTE
      call OPTCHK(INTCHK, IOPT, 'DIVA / F$E')
      if (NXTCHK .lt. 0) then
         KORD2I = -4
      else if (KORD2I .ne. -4) then
         do 290 K = NXTCHK+1, INTCHK(NXTCHK)
            if (INTCHK(INTCHK(K)) .eq. 0) then
               NDTF = INTCHK(INTCHK(K)+1)
               NUMDT = min(KDIM, (INTCHK(INTCHK(K)+2)-NDTF+1) / NTE)
               MAXKQI = NUMDT - 1
            else
c         Take a quick return if needed space was not specified by user.
               KORD2I = -4
            end if
  290    continue
      end if
      if (IOP9 + abs(IOP10) + IOP11 .ne. 0) then
c Insure user doesn't get in trouble with F not iniitalized.
         do 300 K = NDTF, NDTF + NTE*NUMDT - 1
            F(K) = 0.D0
  300    continue
      end if
  320 continue
      if ((KORD2I .eq. -4) .or. (IOP10 .ne. 0)) then
         MACT1(3) = IOPIVA(2)
         call MESS(MACT1, TEXT1, IOPT)
         KORD1I = 24
         KORD(1) = 24
      end if
      TMARK = TSPECS(1)
      TMARKX = TSPECS(4) + TSPECS(2)
c
c     **** DONE WITH INITIALIZATION AND CHECKING INPUTS
      if (IOP13 + IOP14 + IOP15 .ne. 0) return
  330 call DIVAA(TSPECS, Y, F, KORD, DIVAF, DIVAO)
      return
c
c ************ LOOP TO CHECK OPTION SPECIFICATIONS *********
c
  430 JL = 0
      do 560 IHI = ILOW, IDIMK
         J = KORD(IHI)
         go to (460, 480, 490, 490), KGO
c     **** CHECK ON VARIATIONAL EQUATIONS
  460    continue
c++  Code for VAREQ is active
         if (J - NTE) 470, 565, 620
  470    if (J .eq. 0) go to 560
         if (J .le. JL) go to 620
c++  End
c     **** Check on diagnostic output option
  480    continue
c++  Code for OUTPUT is active
c.    **** CHECK IF DONE
         if (J .ge. NTE) go to 565
         if (J .le. JL) go to 620
         go to 550
c++  End
  490    continue
c     **** Check integration order control (KGO=3) and
c     **** error tolerance equation grouping (KGO=4).
         if (J - NTE) 500, 565, 620
  500    if (J) 510, 530, 540
  510    if ((JL .le. 0) .and. (IHI .ne. ILOW)) go to 620
         if (J .lt. JLIM) then
c                         Output an error message.
            IDAT(2) = JLIM
            IDAT(3) = 0
            go to 630
         end if
  520    JL = -JL
         go to 560
  530    if (KGO .eq. 3) go to 520
         KFERR = NTOLF + IHI - ILOW
         if (F(KFERR) .eq. CM1) go to 510
c                         Set up error message, TOL must be -1.
            IDAT(1) = KFERR
            KGO = 11
            go to 650
  540    if (abs(JL) .ge. abs(J)) go to 620
  550    JL = J
  560    continue
  565 NXTCHK = NXTCHK + 3
      INTCHK(NXTCHK-2) = ILOW
      INTCHK(NXTCHK-1) = IHI - ILOW + 1
      go to (30, 110, 120, 150), KGO
c
c     **** AN ERROR HAS BEEN MADE
c                  Error in setting up TSPECS for extra output
  600 IHI = IHI + 2
  610 ILOW = -IOP5
      go to 630
c                  Error in KORD indices
  620 IDAT(2) = abs(JL) + 1
      IDAT(3) = NTE
c                  Set up for print of message about KORD
  630 IDAT(1) = INTCHK(NXTCHK)
  640 IDAT(4) = IHI
      IDAT(5) = KORD(IHI)
c
c ***************** Process Errors *************************************
c
  650 KORD2I = -4
      MACT(4) = LTXTAF
      if (KGO .ge. 8) MACT(4) = -1
      MACT(6) = MLOC(KGO)
c--D Next line special: P=>S, X=>D
      CALL DMESS(MACT, MTXTAA, IDAT, FDAT)
      if (KGO .lt. 8) then
         MACT(10) = ILOW
         MACT(13) = ILOW
         MACT(15) = -min(IHI+2, IDIMK)
         CALL MESS(MACT(9), MTXTAB, KORD)
         if (KGO .le. 4) go to 565
      end if
c              5   6   7    8    9   10   11  12
      go to (100, 25, 25, 320, 100, 150, 660, 25), KGO - 4
  660 KGO = 4
      go to 565
      end
c   End of DIVA

      subroutine DIVAA(TSPECS, Y, F, KORD, DIVAF, DIVAO)
c>> 1989-02-24 DIVAA  Krogh   Big error with BETA(2)=1+epsilon -- looped
c>> 1988-07-27 DIVAA  Krogh   Fixed to allow restart on a restart.
c>> 1988-03-07 DIVAA  Krogh   Initial code.
c
c  MAIN SUBROUTINE FOR VARIABLE ORDER INTEGRATION OF ORDINARY
c  DIFFERENTIAL EQUATIONS
c
      integer KORD(*)
c--D Next line special: P=>D, X=>Q
      double precision TSPECS(*), Y(*)
      double precision F(*)
      external DIVAF, DIVAO
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT, TOLG
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2
      double precision EEPT75, EOVEP2, OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / DIVAEV /
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,TOLG,HC,HDEC,HINC,
     1   HINCC,HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c
      double precision CMP75, C0, C1M5, C1M3, C2P5M3, C8M3, CP0625, CP1
      double precision CP25, CP3, CP4, CP5, CP875, C1, C1P125, C1P3, C2
      double precision C6, C10, C16, C4096
      parameter (CMP75 = -.75D0)
      parameter (C0 = 0.D0)
      parameter (C1M5 = 1.D-5)
      parameter (C1M3 = 1.D-3)
      parameter (C2P5M3 = 2.5D-3)
      parameter (C8M3 = 8.D-3)
      parameter (CP0625 = .0625D0)
      parameter (CP1 = .1D0)
      parameter (CP25 = .25D0)
      parameter (CP3 = .3D0)
      parameter (CP4 = .4D0)
      parameter (CP5 = .5D0)
      parameter (CP875 = .875D0)
      parameter (C1 = 1.D0)
      parameter (C1P125 = 1.125D0)
      parameter (C1P3 = 1.3D0)
      parameter (C2 = 2.D0)
      parameter (C6 = 6.D0)
      parameter (C10 = 10.D0)
      parameter (C16 = 16.D0)
      parameter (C4096 = 4096.D0)
c
      integer LDIS, KEXIT, I, J, K, J1, J2, L, LX
      double precision TP, TP1, TP2, TP3, HH, DISADJ
      double precision SIGMAS, TPS1, TPS2, EIMINO, EXR
c--D Next line special: P=>D, X=>Q
      double precision  TMARKA(2), XP, XP1
      equivalence (G(1, 1), HH), (TMARKA(1), TMARK)
      equivalence (KEXIT, IOP17)
      save EIMINO, EXR, SIGMAS, TP, TP1, TP2, TP3, TPS1, TPS2, DISADJ,
     1   LDIS
c
c                      Declarations for error message processing.
c
      integer MACT(17), MLOC(8), MENTXT, MERET, MEEMES, METEXT
      parameter (MENTXT =23)
      parameter (MERET  =51)
      parameter (MEEMES =52)
      parameter (METEXT =53)
c
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DIVAA$B
cAB At: TN=$F, KSTEP=$I, with H=$F$E
cAC A previously reported error was fatal.$E
cAD Print points not properly ordered: TSPEC($I)=$F$E
cAE An error tolerance of 0 requires setting special flags.  $B
cAF Step size reduced too fast, doing a restart.  $B
cAG H is so small that TN + H = TN.  $B
cAH Error tolerance too small.  $B
cAI Step size at end of start < HMIN=$F, $B
cAJ Error estimates require a stepsize < HMIN=$F, $B
cAK (Estimated Error) / (Requested Error) for equation $I is $F.  $B
cAL Tolerance $I is F($I) = $F.$B
cAM Tolerance $I is F($I) * F($I) = $F * $F = $F.$B
cAN   Replacing F($I) with $F.$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM,LTXTAN
      parameter (LTXTAA=  1,LTXTAB=  8,LTXTAC= 40,LTXTAD= 80,LTXTAE=129,
     * LTXTAF=189,LTXTAG=237,LTXTAH=272,LTXTAI=302,LTXTAJ=342,
     * LTXTAK=390,LTXTAL=455,LTXTAM=484,LTXTAN=531)
      character MTXTAA(3) * (186)
c
      integer LOCM, MLOCAC, MLOCAD, MLOCAE, MLOCAF, MLOCAG, MLOCAH,
     1   MLOCAI, MLOCAJ

      parameter (LOCM = 32 * 256)
c                     KORD1I   Severity   Loc. message
      parameter (MLOCAC = 23 + 32 * (99 + 256 * LTXTAC))
      parameter (MLOCAD = 13 + 32 * (38 + 256 * LTXTAD))
      parameter (MLOCAE = 22 + 32 * (38 + 256 * LTXTAE))
      parameter (MLOCAF =  3 + 32 * (14 + 256 * LTXTAF))
      parameter (MLOCAG = 21 + 32 * (38 + 256 * LTXTAG))
      parameter (MLOCAH =  2 + 32 * (25 + 256 * LTXTAH))
      parameter (MLOCAI = 11 + 32 * (38 + 256 * LTXTAI))
      parameter (MLOCAJ = 12 + 32 * (38 + 256 * LTXTAJ))
c
      data MTXTAA/'DIVAA$BAt: TN=$F, KSTEP=$I, with H=$F$EA previously r
     *eported error was fatal.$EPrint points not properly ordered: TSPEC
     *($I)=$F$EAn error tolerance of 0 requires setting special flags. $
     * ','$BStep size reduced too fast, doing a restart.  $BH is so smal
     *l that TN + H = TN.  $BError tolerance too small.  $BStep size at$
     * end of start < HMIN=$F, $BError estimates require a steps','ize <
     * HMIN=$F, $B(Estimated Error) / (Requested Error) for equation $I$
     * is $F.  $BTolerance $I is F($I) = $F.$BTolerance $I is F($I) * F(
     *$I) = $F * $F = $F.$B  Replacing F($I) with $F.$E'/
      data MLOC / MLOCAC, MLOCAD, MLOCAE, MLOCAF, MLOCAG, MLOCAH,
     1   MLOCAI, MLOCAJ /
c
c                      1 2 3 4       5 6       7       8 9      10
      data MACT / MEEMES,0,0,0, MENTXT,0, METEXT, MENTXT,0, METEXT,
     1  MENTXT, 0, METEXT, MENTXT, LTXTAN, METEXT, MERET /
c           11 12      13      14      15      16     17
c
      data EXR, EIMINO / CP1, C8M3 /
      data LDIS / 0 /
c
c ************** START OF EXECUTABLE CODE ******************************
c
  660 if (KORD2I) 670, 1380, 1840
  670 if (KORD2I .eq. -1) go to 2140
      if (KORD2I .eq. -5) go to 720
      if (KORD2I+8) 1840, 710, 1840
c     **** SPECIAL OUTPUT CASE (EXTRAPOLATION OR GSTOP)
  680 go to (1220, 1190, 830, 690, 690, 2190), KEXIT
c     **** RESET TMARK BEFORE GOING WHERE DIRECTED BY KEXIT
  690 KEXIT = KEXIT - 2
      go to 1890
c     **** USER HAS REQUESTED A RESTART
  700 KORD2I = -5
      IGFLG = 0
      if ((KORD(2) .ge. 2) .and. (LSC .lt. 3)) KORD2I = -8
      if ((KORD1I .le. 3) .or. (KORD1I .eq. 5)) go to 1890
      if (KORD2I .eq. -5) go to 720
c                Set up for a discontinuity
  710 XP = TSPECS(1)
      if (KORD(2) .eq. 3) then
c                Adjust for discontinuity in Y
         J = 0
         do 712 I = 1, NTE
            if (NKDKO .ne. 0) KORDI = KORD(NKDKO + I -1)
            K = 1
            J = J + KORDI
            XP1 = Y(J)
  711       Y(NYNY + J - K) = Y(NYNY + J - K) + XP1
            if (K .lt. KORDI) then
               XP1 = Y(J-K) + (TN - XP) * XP1 / dble(K)
               K = K + 1
               go to 711
            end if
  712    continue
      else
         IGFLG = -3
      end if
      DISADJ = HH
      XP1 = (XP - TN) / XI(1)
      if (XP1 .lt. CP25) then
         K = 1
         if (XP1 .lt. CMP75) K = 2
         TSPECS(1) = TN - XI(K)
         TSPECS(2) = 2.D0*(TN - TSPECS(1))
         call DIVAIN(TSPECS(1), Y, F, KORD)
         do 713 J = 1, NY
            Y(NYNY + J - 1) = Y(J)
  713    continue
c          Move difference tables back one step
         TN = TSPECS(1)
  714    call DIVABU(F, KORD)
         if (K .eq. 2) then
            KSC = max(KSC - 1, 1)
            do 715 K = max(1, KSC), IOP11-1
               BETA(K+1) = BETA(K) * (XI(K) / (XI(K+1) - XI(1)))
  715       continue
            K = 0
            go to 714
         end if
      end if
c      Take step to discontinuity.
      HH = (XP - TN)
      EREP = -abs(EREP)
      KIS = 1000
      LDIS = 1
      LSC = 0
      LINC = 0
      HINCC = C1P125
      KSSTRT = KSTEP + 2
      go to 1115
c ********
c INITIALIZE FOR STARTING AN INTEGRATION
c ********
  720 HH = TSPECS(2)
      LSC = 8
      LINC = -5
      LINCD = 64
      LDT = -4
      KQMAXI = 0
      EIMIN = CP3
      EAVE = C10
      XI(1) = C0
      IGFLG = 0
      KIS = 0
      KSSTRT = 0
      ROBND = 0.D0


c GO COMPUTE INITIAL DERIVATIVES
  730 KORD2I = -6
c++  Code for VAREQ is active
      ICS = 0
      ICF = NE
c++  End
  740 KORD1I = KPRED
      go to 1360
c   RETURN AFTER COMPUTING INITIAL (OR NOISE TEST) DERIVATIVES
  750 continue
c++  Code for VAREQ is active
      if (IOP18 .eq. 0) go to 790
c.    **** SPECIAL LOGIC TO COMPUTE VARIATIONAL DERIVATIVES
  760 if (KORD1I .eq. 3) go to 770
      KORD1I = 3
      KORD(3) = 0
  770 if (ICF .eq. NTE) go to 790
      if (KORD2I .eq. -6) then
         if (ICS .eq. 1) go to 790
      end if
      ICS = ICF + 1
  780 KORD(3) = KORD(3) + 1
      ICF = IOP18 + KORD(3)
      ICF = abs(KORD(ICF))
      if (ICF) 1360, 780, 1360
c++  End
  790 ICS = 1
      ICF = NE
c++  Code for VAREQ is active
      if (KORD2I .eq. 0) if (EREP) 2220, 2220, 1430
c++  End
      if (LINC + 5) 1490, 810, 1490
c END OF SPECIAL CODE FOR INITIALIZATION AT THE START
c ********
c UPDATE VARIABLES TO PREPARE FOR NEXT STEP
c ********
  800 LDT = 0
      EIMIN = C2P5M3+EIMIN*(C6*EAVE+EIMAX)/((EIMIN+C1)*(C6*EIMAX+EAVE))
  810 do 820 J = 1, NY
  820    Y(NYNY + J - 1) = Y(J)
      TN = TSPECS(1)
c ********
c TEST FOR VARIOUS TYPES OF OUTPUT
c ********
c     TEST FOR END OF STEP OUTPUT (OR IF DUMP OUTPUT TO BE TESTED FOR)
      if (IOP6 .eq. 0) go to 840
c   SET UP FOR END OF STEP OUTPUT
      KORD1I = 6
      go to 1810
c     SET UP AFTER OTHER COMPUTATIONS HAVE BEEN MADE (TSPECS(1).NE.TN)
  830 KORD1I = 6
      go to 860
c     TEST FOR AN OUTPUT POINT
  840 if (HH * (TMARK - TN)) 1760, 1760, 850
c     TEST FOR TOO MANY STEPS OUTPUT
  850 continue
      if (KSOUT .gt. KSTEP) go to 890
      KORD1I = 4
  860 if (TSPECS(1) .eq. TN) go to 1810
c     GO INTERPOLATE VALUES AT END OF LAST STEP
  870 TSPECS(1) = TN
      go to 1780
c     CONTINUE AFTER TOO MANY STEPS OUTPUT
  880 continue
      KSOUT = KSTEP + IOP4
  890 continue
c++  Code for DUMP is active
      if (IOP9 .eq. 0) go to 920
c++  Code for DUMP & STIFF is inactive
c      KQMXDS=KQMAXD
c++  Code for DUMP is active
      KQMXIS = KQMAXI
      KIS = KIS + 1
      if (KIS .ne. 0) go to 920
c.   TIME TO DUMP THE SOLUTION
  900 KORD1I = 9
c.    SET TO UPDATE THE DIFFERENCE TABLE
      if (LDT .eq. 1) go to 1810
c Note that doing this update can lead to very small differences in the
c results because of round off differences.
      LDT = -2
      go to 1780
c++  End
c RETURN AFTER DUMPING THE SOLUTION
  910 continue
c++  Code for DUMP is active
      KIS = 2
c.    TEST IF SOLUTION DUMP DUE TO RESTART, END, OR
c.    DROP IN INTEG. ORDER
      if (LINC .lt. 0) if (LINC + 6) 1860, 1750, 1180
c++  End
c END OF TESTING FOR VARIOUS TYPES OF OUTPUT
c   TEST IF STEPSIZE MAY BE INCREASED OR IF TESTS SHOULD BE MADE
c   FOR DECREASING THE STEPSIZE (OR IF STARTING OR USER SELECTING H)
  920 KSTEP = KSTEP + 1
      if (LINC) 930, 980, 940
  930 continue
c     **** ONLY POSSIBLE VALUES AT THIS POINT ARE
c          LINC = -2 OR -5
      if (LINC + 5) 1120, 1110, 1120
c ********
c ERROR ESTIMATES INDICATE STEPSIZE CAN BE INCREASED
c ********
  940 HC = HINCC
      if (LINC .gt. 1) HC = HC ** LINC
      HH = HC * HH
c     TEST IF NEW STEPSIZE IS TOO BIG
      if (abs(HH) .gt. HMAX) if (HMAX) 970, 970, 960
  950 EAVE = EIMAX
      ROBND = CP3 + HINCC
      go to 1110
c     NEW STEPSIZE IS TOO BIG
  960 if (abs(XI(1)) .ge. HMAXP9) go to 970
      HH = sign(HMAX, HH)
      go to 950
c     RESTORE THE OLD STEPSIZE
  970 HH = XI(1)
      LINC = 0
      go to 1150
c END OF CODE FOR CASE WHEN ERROR ESTIMATES INDICATE STEPSIZE INCREASE
c ********
c TEST IF ESTIMATED ERRORS INDICATE STEPSIZE SHOULD BE DECREASED
c ********
  980 ROBND = C1P3
      if (EIMAX .le. EAVE) go to 990
      EAVE = EAVE + CP4 * (EIMAX - EAVE)
      if ((EIMAX * EMAX) - C1M3) 1000, 1010, 1010
  990 EAVE = EIMAX
      if ((EIMAX * EMAX) .ge. EIMIN) go to 1010
 1000 ROBND = CP3 + (SIGMA(KQMAXS)/SIGMA(KQMAXS-1))
      go to 1180
c     TEST IF STEPSIZE SHOULD BE REDUCED
 1010 if (EMAX * EIMAX .lt. EXR * EAVE) go to 1180
c ********
c ERROR ESTIMATES INDICATE STEPSIZE SHOULD BE REDUCED
c ********
      HC = HDEC
      if (EIMIN .le. EIMINO) go to 1030
      EIMIN = EIMINO
      HC = CP875
 1030 HH = HC * XI(1)
      if (LSC - 1) 1040, 1080, 1090
 1040 if (abs(HH) .ge. HMIN) go to 1090
      if (abs(CP875*XI(1)) .le. HMIN) if (LINC) 1050, 970, 970
      HH = sign(HMIN, HH)
      go to 1090
c     STEPSIZE IS TOO SMALL TO BE REDUCED
c     SET UP ERROR INDICATORS AND PREPARE FOR RETURN TO USER
 1050 KORD1I = 8
      go to 2240
c     PROCEED WITH CURRENT STEPSIZE DESPITE ERROR BEING TOO BIG
 1070 HH = XI(1)
      TSPECS(2) = HH
      EMAX = C0
      LINC = 0
      if (KORD1I - 2) 1420, 1430, 1430
c     SET LSC TO END STARTING PHASE
 1080 LSC = 2
c     CHECK IF REPEATING A STEP
 1090 if (LINC .ne. -1) go to 1110
c   WHEN REPEATING A STEP, BACK UP THE DIFFERENCES AND STEPSIZE INFO.
 1100 call DIVABU(F, KORD)
c   TEST IF NOISE TEST (LINC = -7) OR IF H IS NOT
c     BEING CHANGED (LINC = -4)
      if (LINC + 4) 1780, 1180, 1110
c ********
c STEPSIZE IS TO BE CHANGED
c ********
 1110 continue
c MODIFY STEPSIZE TO REDUCE ROUNDOFF ERROR IN ACCUMULATING INDEP. VAR.
      TP = C2 * abs(TN) + C4096 * abs(HH)
      TP = (TP + abs(HH)) - TP
      if (TP .ne. C0) HH = sign(TP, HH)
c     TEST IF NEW STEPSIZE SELECTED ACTUALLY GIVES A CHANGE
      if (HH .eq. TSPECS(2)) go to 1140
 1115 TSPECS(2) = HH
      if (IOP8 .eq. 0) go to 1140
c     SETUP TO TELL USER ABOUT STEPSIZE CHANGE (OR TO CHANGE STEPSIZE)
 1120 KORD1I = 8
      go to 860
c     RETURN AFTER TELLING USER ABOUT STEPSIZE CHANGE
 1130 HH = TSPECS(2)
 1140 if (HH .ne. XI(1)) KQICON = -1
 1150 HC = min(EOVEP2, abs(HH)) / EEPS2
c ********
c PREPARE FOR BEGINNING A NEW STEP
c ********
      if (LINC .gt. 0) then
         LINC = min(LINC, LINCQ) + LINCQ
         go to 1190
      end if
 1180 LINC = LINCD
 1190 if (HC .gt. abs(TN)) go to 1200
c     **** GIVE SINGULARITY DIAGNOSTIC
      KORD1I = 5
      go to 2240
 1200 TSPECS(1) = TN + HH
      if (LEX .eq. 0) go to 1250
      if (HH * (TSPECS(1) - TMARKX) .lt. C0) go to 1250
      TSPECS(1) = TMARKX
      HH = TMARKX - TN
      LINC = 64
      if (LEX .gt. 0) go to 1240
      if ((LSC .lt. 4) .and. (HH / XI(1) .lt. CP3)) go to 1230
 1220 HH = CP875 * HH
      go to 1110
c     **** GIVE OUTPUT AT CURRENT TMARK (WITH EXTRAPOLATION)
 1230 KORD1I = -KMARK
      go to 1770
c     **** INTEGRATE TO TMARKX
 1240 KQICON = -1
c   TEST IF SUBROUTINE FOR COMPUTING INTEGRATION COEFF. SHOULD BE CALLED
 1250 continue
c++  Code for STIFF is inactive
c      IF ((KQMAXI .LT.KQICON) .OR. (KQMAXD.LT.KQDCON)) GO TO 1320
c++  Code for ~STIFF is active
      if (KQMAXI .lt. KQICON) go to 1320
c++  End
c   GO COMPUTE COEFFICIENTS REQUIRED FOR THE INTEGRATION
c     TEST IF STARTING
      if (LSC .lt. 7) go to 1310
 1260 KQMAXI = 2
c++  Code for STIFF is inactive
c      IF (METHOD) 1262,1270,1264
c 1262 KQMAXI=0
c 1264 KQMAXD=max(MAXDIF,2)
c      CALL DIVAHC
cc.  SET UP TO GO DO INITIALIZATION FOR CASE OF STIFF EQUATIONS
c      KORD1I=5
c      GO TO 1350
c++  End
c   INITIALIZE FOR EQUATIONS WHICH ARE NOT STIFF
 1270 KQMAXD = 0
      call DIVAHC
      J = NDTF
      do 1300 I = 1, NTE
c++  Code for STIFF is inactive
c         if (KORD(I + 3) .le. 0) go to 1290
c++  End
         KORD(I + 3) = 1
c     INITIALIZE THE DIFFERENCE TABLE
         if (LDT .eq. -4) F(J) = F(I)
         F(J + 1) = C0
         F(J + 2) = C0
 1290    continue
         J = J + NUMDT
 1300    continue
      if (LSC .eq. 5) go to 1340
      LSC = 7
      LDT = 1
      go to 1330
c   INTEGRATION IS NOT BEING STARTED
 1310 K = KORD(KEMAX + 3)
      SIGMAS = SIGMA(K)
      call DIVAHC
c     **** ADJUST EAVE
      TPS1 = BETA(K)
      if (TPS1 .gt. C1) TPS1 = CP5 * TPS1 + CP5
      EAVE = EAVE * TPS1 * (SIGMA(K) / SIGMAS)
c     TEST BELOW USED TO GET SAME RESULTS WITH/WITHOUT EXTRA EQUATIONS
      if (K .gt. KQICON) LSC = max(LSC, -3)
c END OF SPECIAL LOGIC FOR CASE WHEN INTEG. COEFF. ROUTINE IS CALLED
 1320 continue
c ********
c PREDICT Y
c ********
 1330 continue
c++  Code for ~ARGM is active
      call DIVAPR(Y, Y(NYNY), F, KORD)
c++  Code for ARGM is inactive
c      CALL DIVAPE
c++  End
c     GO GET PREDICTED DERIVATIVES
 1340 KORD1I = KPRED
c ********
c CALL DIVAF  (OR RETURN)
c ********
 1350 KORD2I = 0
 1360 KORD(1) = KORD1I
      KORD(2) = 0
      if (IOP13 .ne. 0) return
      call DIVAF(TSPECS(1), Y, F, KORD(1))
c     TEST FOR SPECIAL USER RETURN
 1380 if (KORD(1) .lt. 0) go to 2130
c     TEST FOR SPECIAL CASE
      if (KORD2I .ne. 0) go to 660
c ********
c TRANSFER CONTROL TO PROPER PLACE AFTER COMPUTING DERIVATIVES
c ********
      if (KORD1I - 2) 1400, 800, 1390
 1390 continue
c++  Code for VAREQ is active
      if (ICS - ICF) 1410, 1410, 760
c++  End
c ********
c PREPARE FOR CORRECTING, AND CORRECT Y
c ********
 1400 ITOLEP = 0
      ILGREP = 0
      IY = 1
      EIMAX = C1M5
      EMAX = C0
      KQMAXI = 2
      KQMAXS = 2
c++  Code for STIFF is inactive
c      IF (METHOD) 1404,1410,1406
c 1404 KQMAXI=0
c 1406 KQMAXD=2
c++  End
 1410 continue
c++  Code for ~ARGM is active
      call DIVACR(Y, F, KORD, F(NTOLF), KORD(IOP16))
c++  Code for ARGM is inactive
c      CALL DIVACE
c++  End
c     TEST IF ESTIMATED ERROR IS TOO BIG (OR IF DIAGNOSTIC CALLED FOR)
      if (EMAX .gt. EREP) if (EREP) 2210, 2210, 1670
 1420 continue
c++  Code for VAREQ is active
      if (IOP18 .ne. 0) go to 760
c++  End
 1430 KORD1I = 2
c     TEST IF NOISE APPEARS TO LIMIT PRECISION
      if (EMAX .lt. C0) go to 1470
c++  Code for ~STIFF is active
      if (LSC) 1450, 1360, 1610
c++  Code for STIFF is inactive
c      IF (LSC) 1450,1460,1610
c++  End
c     SET LSC=0 IF NOISE NO LONGER APPEARS TO LIMIT PRECISION
c     OR IF THE END OF THE STARTING PHASE HAS BEEN REACHED
 1450 LSC = 0
 1460 if (METHOD) 800, 1350, 1350
c ********
c NOISE APPEARS TO BE LIMITING PRECISION
c ********
 1470 continue
      if (LSC .le. 0) LSC = max(LSC - 1, -KQMAXS)
      if (LSC .eq. -1) go to 1460
      if (abs(EMAX) .lt. EXR) go to 1590
      LINC = -7
      TPS2 = (C1 + BETA(NOISEQ - 1)) ** NOISEQ
      if (SNOISE .lt. EEPS10 * TPS2) go to 1550
      TP = sign(EEPT75 * abs(TN) + OVTM75, HH)
      if (abs(TP) .gt. abs(HH)) go to 1550
      TSPECS(1) = TN + TP
      KORD1I = 0
c     **** GO TO BACK UP THE DIFFERENCES AND GET F(TSPECS(1))
      go to 1100
c     **** SOLUTION HAS BEEN INTERPOLATED AND F COMPUTED
 1490 continue
      KORD1I = 0
      LINC = LINC - 1
      if (LINC + 9) 1510, 1520, 1500
 1500 TSPECS(1) = TN + (TP + TP)
      TP1 = F(KEMAX)
      TP2 = F(NDTF + NUMDT * KEMAX - NUMDT)
      go to 1780
c     **** COMPUTE 2-ND DIFFERENCE AT CLOSE SPACED T VALUES
 1510 TP2 = TP3
 1520 TP3 = F(KEMAX)
      TPS1 = abs((TP3 - TP1) - (TP1 - TP2))
      if ((C16 * TPS1 * TPS2) .ge. DNOISE) if (LINC + 9) 1550, 870, 1550
 1530 continue
      TPS2 = CP25 * SNOISE / RBQ(NOISEQ)
      do 1540 K = 2, NUMDT
         TPS1 = TPS1 + TPS1
         RBQ(K) = max(TPS1, TPS2 * RBQ(K))
 1540    continue
      LINC = 0

cFTK Next two lines added 2009-10-15
      if (abs(EMAX) .lt. EREP) go to 1460
cFTK  LINC = -1  And then on 2015-03-14 commented out this line

      HH = CP875 * HH
      go to 1040
c     **** SET UP TO GIVE NOISE DIAGNOSTIC
 1550 KORD1I = 6
      go to 2240
c     **** AFTER GIVING NOISE DIAGNOSTIC
 1560 KORD1I = 2
      if (KORD(2) .ge. 0) then
        TPS1 = EEPS10
cFTK Next line added 2009-10-15
        if (TPS1 .lt. .49D0 * RBQ(2)) go to 1530
      end if
c     **** SET NEW VALUE FOR OFFENDING TOL
      F(NTOLF + ITOLEP - 1) = FDAT(7)
      if (LINC + 7) 1180, 1570, 1180
 1570 LINC = 0
 1580 if (LSC) 1460, 1460, 1610
c     **** CHANGE HINCC AND ADJUST SIGMA( )
 1590 if (LSC .ne. -4) go to 1580
      if (HINCC .eq. C1P125) go to 1580
      TPS1 = C1P125 / HINCC
      TPS2 = 1.0D0
      do 1600 K = 2, IOP11
         TPS2 = TPS2 * TPS1
         SIGMA(K) = SIGMA(K) * TPS2
 1600    continue
      EAVE = EAVE * TPS1 ** (1-KORD(KEMAX+3))
      LINCD = 6
      LINCQ = 12
      HINCC = C1P125
      go to 1460
c   END OF CODE FOR CASE WHEN NOISE APPEARS TO LIMIT PRECISION
c ********
c SPECIAL LOGIC FOR STARTING THE INTEGRATION
c ********
 1610 if (LSC .eq. 1) go to 800
      LSC = LSC - 1
      if (LSC - 2) 1620, 1640, 1650
 1620 if (EIMAX .le. (CP0625*EAVE*(SIGMA(KQMAXS)/SIGMAS)*(BETA(KQMAXS+
     1   1))**2)) go to 800
 1630 KSSTRT = KSTEP + 2
c   TEST IF STEPSIZE IS TOO SMALL BEFORE ENDING STARTING PHASE
      if (abs(HH) .ge. HMIN) go to 1450
c     GIVE DIAGNOSTIC FOR STEPSIZE TOO SMALL AT END OF START
      KORD1I = 7
      go to 2240
c   SET LSC TO DO ONE DERIVATIVE EVAL. PER STEP
 1640 LSC = 1
      go to 800
c     TEST IF FIRST TIME THROUGH THE FIRST STEP
 1650 if (LSC .eq. 6) go to 1340
c     END STARTING PHASE IF CONVERGENCE OF CORRECTOR ITERATES TOO SLOW
      if (LDT .eq. -5) go to 1660
      LSC = min(LSC, 4)
      go to 800
 1660 LDT = 0
      if (LSC - 4) 1260, 1630, 1260
c END OF SPECIAL LOGIC FOR STARTING THE INTEGRATION
c ********
c ESTIMATED ERROR IS TOO BIG
c ********
 1670 if (BETA(2) - C1) 1690, 1730, 1680
 1680 HC = C1 / BETA(2)
      if (BETA(2) .ge. C1P125) go to 1740
 1690 if (BETA(2) .gt. CP1) go to 1730
c   REQUIRED STEPSIZE REDUCTION IS TOO RAPID -- GIVE A DIAGNOSTIC
      KORD1I = 4
      go to 2240
c
c     TEST KORD(2) AFTER ABOVE DIAGNOSTIC OR A DISCONTINUITY DIAGNOSTIC
 1700 continue
      if (KORD(2) .eq. 0) go to 1730
c  TEST IF SOLUTION MUST BE DUMPED BEFORE A RESTART
 1710 LINC = -1
c++  Code for DUMP is active
      if (IOP9 .eq. 0) go to 1750
      if (KIS .eq. 2) go to 1750
      LINC = -6
c.    GO DUMP SOLUTION BEFORE REPEATING THE STEP
 1720 KQMAXI = KQMXIS
c++  Code for DUMP & STIFF is inactive
c      KQMAXD=KQMXDS
c++  Code for DUMP is active
      call DIVABU(F, KORD)
      go to 900
c++  End
c   SET UP TO REPEAT THE STEP
 1730 HC = CP5
 1740 LINC = -1
      if (LSC .le. 3) go to 1030
c   RESTART THE INTEGRATION IF ERROR IS TOO BIG ON FIRST OR SECOND STEP
c LOOP TO SELECT A NEW INITIAL STEPSIZE
 1750 LSC = 7
 1755 HH = HH * CP5
      EMAX = EMAX * CP25
      if (EMAX .ge. CP3) go to 1755
      go to 1090
c   END OF SELECTING A NEW INITIAL STEPSIZE
c END OF LOGIC FOR CASE WHEN ESTIMATED ERROR IS TOO BIG
c ********
c INTEGRATION HAS REACHED AN OUTPUT POINT
c ********
 1760 if (KMARK .eq. 0) go to 1920
      KORD1I = min(KMARK, 5)
      KORD(3) = KMARK
      if (TSPECS(1) .eq. TMARK) go to 1790
 1770 TSPECS(1) = TMARK
 1780 call DIVAIN(TSPECS(1), Y, F, KORD)
 1790 continue
      if (KORD1I) 1800, 730, 1810
c   OUTPUT POINT IS OBTAINED BY EXTRAPOLATION
 1800 continue
c++  Code for EXTRAP is active
      KORD1I = -KORD1I
      KORD2I = -7
      KEXIT = 4
c.  TEST IF GSTOP-S ARE PRESENT
c++  Code for EXTRAP &  GSTOP is active
      if (NGTOT .eq. 0) go to 1820
      IGFLG = 4
      KEXIT = 2
      KORD1I = 7
      if (IOP7) 740, 1820, 740
c++  End
c ********
c CALL DIVAO  (OR RETURN)
c ********
 1810 KORD2I = 1
 1820 KORD(1) = KORD1I
      KORD(2) = 1
      if (IOP14 .ne. 0) return
      call DIVAO(TSPECS(1), Y, F, KORD(1))
c     TEST FOR SPECIAL USER RETURN OR FOR A RESTART
c++  Code for ~DUMP is inactive
c 1840 IF (KORD(1)) 2130,700,1880
c++  Code for DUMP is active
 1840 if (KORD(1) .gt. 0) go to 1880
 1850 if (IOP9 .eq. 0) go to 1870
c.    **** GO DUMP THE SOLUTION
      LINC = -7
      ITOLEP = KORD(1)
      IDAT(1) = KORD(2)
      NEPTOL = KORD1I
      if (LSC .ne. 8) go to 900
 1860 LINC = min(0, LINCD)
      KORD1I = NEPTOL
      KORD(1) = ITOLEP
      KORD(2) = IDAT(1)
 1870 if (KORD(1)) 2130, 700, 2100
c++  End
 1880 if (KORD2I .lt. 0) go to (2140, 1810, 1350, 2110, 720, 750, 680,
     1   710), -KORD2I
      if (KORD2I .eq. 0) go to 1380
c ********
c TRANSFER CONTROL TO PROPER PLACE AFTER OUTPUT
c ********
 1890 if (KORD1I - 5) 1910, 1930, 1900
 1900 if (KORD1I - 8) 840, 1130, 910
 1910 if (KORD1I - 3) 1920, 1930, 880
c   GET NEW TOUT
 1920 TOUT = TSPECS(1) + TSPECS(3)
c GET NEW TMARK (NEXT INDEP. VAR. OUTPUT POINT)
 1930 XP = TMARK
      K = KMARK
      TMARK = TOUT
      KMARK = 2
      LEX = 0
      if (IOP5) 1940, 1980, 1970
 1940 I = -IOP5
 1950 I = I + 3
      J1 = KORD(I - 3)
      if (J1) 1950, 1980, 1960
 1960 J2 = KORD(I - 2)
      L = KORD(I - 1)
      go to 1990
 1970 J1 = 5
      J2 = IOP5
      L = 0
      if (J2 .ge. J1) go to 1990
 1980 J1 = 4
      J2 = 4
      L = IOP3
c
c     **** LOOP TO SET NEW TMARK (AND TMARKX)
 1990 do 2060 J = J1, J2
c        **** TEST IF EXTRAPOLATION NOT POSSIBLE
         if (L .eq. 0) go to 2010
         LX = 2
         if (LEX) 2020, 2030, 2020
 2000    LEX = L
 2010    LX = 1
 2020    if (HH * (TSPECS(J) - TMARKA(LX))) 2030, 2060, 2060
 2030    if (J .eq. 4) go to 2050
         if (HH * (TSPECS(J) - XP)) 2060, 2040, 2050
 2040    if ((K .ge. J) .or. (K .eq. 3)) go to 2060
 2050    TMARKA(LX) = TSPECS(J)
         if (LX .eq. 2) go to 2000
         KMARK = J
 2060    continue
      if (IOP5 .lt. 0) go to 1950
      if (J1 .ne. 4) go to 1980
      if (KMARK .eq. 4) KMARK = 3
c     **** TEST IF NEW TMARK IS ACCEPTABLE
      if (HH * (XP - TMARK)) 2070, 2080, 2090
 2070 if (KORD2I - 1) 670, 840, 670
 2080 if (K .ne. KMARK) go to 2070
c++  Code for DUMP is active
      if (KORD1I .eq. 3) go to 1850
c++  Code for ~DUMP is inactive
c      IF (KORD1I .EQ. 3) GO TO 2100
c++  End
 2090 if (KORD1I .eq. 13) go to 2190
c SETUP TO INDICATE ERROR IN SPECIFICATION OF OUTPUT POINTS
      KORD1I = 2
      IDAT(2) = KMARK
      if (KMARK .le. 3) IDAT(2) = KMARK + 1
      FDAT(3) = TSPECS(IDAT(2))
      go to 2240
c     SET KORD1I=1 TO INDICATE THAT END OF INTEGRATION HAS BEEN REACHED
 2100 KORD1I = 1
c ********
c RETURN TO USER
c ********
 2110 KORD2I = -1
      KORD(1) = KORD1I
 2130 KORD(2) = -1
      return
c ********
c TRANSFER CONTROL TO PROPER PLACE AFTER RETURN TO USER
c ********
 2140 if (KORD1I - 2) 2150, 1560, 2160
 2150 KORD2I = 1
      go to 1930
 2160 if (KORD1I - 4) 1700, 2200, 2170
 2170 if (KORD1I - 13) 2180, 1930, 2190
 2180 if (abs(HH) .ge. HMIN) if (KORD1I - 11) 1030, 1450, 1030
      if (KORD(2) .eq. 0) if (KORD1I - 11) 1070, 800, 1070
c   ERROR MESSAGES HAVE BEEN IGNORED -- COMPUTATION CAN NOT CONTINUE
 2190 KORD1I = 1
      go to 2240
c
c        AFTER A DISCONTINUITY RETURN
 2200 LINC = -4
      if (KORD(2)) 1710, 1730, 1100
c ********
c PROBLEM ENCOUNTERED WHEN CORRECTING
c ********
 2210 if (LDIS .eq. 0) go to 2230
c           Extra checks when had a user specified discontinuity.
c++  Code for VAREQ is active
      if (IOP18 .ne. 0) go to 760
c++  End
 2220 KORD1I = 2
      LDIS = LDIS + 1
      TP = DISADJ / HH
      if (KIS .ge. 1000) then
         if (LDIS .eq. 2) then
            if (KQMAXS .le. 3) then
               LDIS = 0
               EREP = abs(EREP)
               TSPECS(2) = HH*min(min(TP, TP**2),
     1            (CP25 * EXR/EMAX)**.333333333D0)
               go to 720
            end if
            LINC = -5
            if (IOP9 .eq. 0) KIS = 1001
            go to 800
         end if
         if (IOP9 .eq. 0) KIS = KIS + 1
         if (KQMAXS .le. LDIS + 2) KIS = LDIS + 1
         LINC = min(LINC, LDIS-2)
      end if
      if (LDIS .gt. 2*KQMAXS) then
         EREP = abs(EREP)
         LDIS = 0
         if (EMAX .gt. EREP) go to 1670
         go to 1430
      end if
      if (TP .ge. HINCC**(LINC+2)) then
         if ((LDIS .ne. 3) .and. (TP .gt. dble(KQMAXS))) LSC = 1
         EIMIN = CP5
         EAVE = EAVE * TP**8
      end if
      if (LSC .eq. 2) go to 1630
      if (EMAX .gt. EXR) go to 1730
      go to 1430
c
 2230 EREP = abs(EREP)
c++  Code for DUMP is active
      if (LINC .lt. -3) go to 1720
c++  End
c     BAD TOL
      KORD1I = 3
c ********
c ERROR PROCESSING
c ********
 2240 FDAT(1) = TN
      FDAT(2) = HH
      IDAT(1) = KSTEP
      ITOLEP = max(NEPTOL, -NEPTOL - 1)
      J = 3
      if (KORD1I .ge. 7) then
         J = 4
         FDAT(3) = HMIN
      end if
      if (KORD1I .le. 3) then
         if (KORD1I .lt. 3) then
            K = 8
         else
            MACT(9) = LTXTAL
            FDAT(3) = C0
            IDAT(2) = ITOLEP
            IDAT(3) = ITOLEP + NTOLF - 1
            K = 11
         end if
      else
         MACT(9) = LTXTAK
         FDAT(J) = EMAX
         IDAT(2) = KEMAX
         IDAT(3) = ITOLEP
         IDAT(4) = ITOLEP + NTOLF - 1
         FDAT(J+1) = F(IDAT(4))
         K = 14
         if (KORD1I .eq. 6) then
            K = 17
            IDAT(5) = IDAT(4)
            FDAT(7) = 32.D0 * abs(EMAX) * FDAT(J+1)
            FDAT(J+2) = FDAT(7)
         end if
         MACT(12) = LTXTAL
         if (NEPTOL .lt. 0) then
            MACT(12) = LTXTAM
            IDAT(6) = IDAT(4)
            IDAT(5) = IDAT(4) + 1
            FDAT(J+2) = F(IDAT(5))
            FDAT(J+3) = FDAT(J+1) * FDAT(J+2)
         end if
      end if
c Set the location for the first part of the message that varies, set
c the error severity, and the index number, print the error and
c return or stop.
      L = MLOC(KORD1I)
      MACT(6) = L / LOCM
      MACT(2) = (L - MACT(6) * LOCM) / 32
      KORD1I = mod(L, 32)
      MACT(3) = KORD1I
      MACT(K) = MERET
c--D Next line special: P=>S, X=>D
      call DMESS(MACT, MTXTAA, IDAT, FDAT)
      MACT(K) = MENTXT
      go to 2110
c
      end
c   End of DIVAA

      subroutine DIVABU(F, KORD)
c>> 1987-12-07 DIVABU Krogh   Initial code.
c
c THIS SUBROUTINE RESTORES THE DIFFERENCE TABLE TO ITS STATE
c AT THE BEGINNING OF THE CURRENT STEP.  IF THE INTEGRATION ORDER
c WAS INCREASED, IT IS REDUCED. THE COMMON ARRAY XI IS ALSO
c RESTORED TO ITS STATE AT THE BEGINNING OF THE STEP. IF THE
c STEPSIZE IS NOT BEING CHANGED, THE ARRAY V USED TO COMPUTE
c INTEGRATION COEFFICIENTS IS RESTORED.
c
      integer KORD(*)
      double precision F(*)
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT, TOLG
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,TOLG,HC,HDEC,HINC,
     1   HINCC,HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c
      integer I, L, KQQ, J, K
      double precision TPD, C0, C2
      parameter (C0 = 0.D0)
      parameter (C2 = 2.D0)
c ********* START OF EXECUTABLE CODE **********
c
c ********
c BEGIN LOOP TO BACK UP DIFFERENCE TABLES
c ********
      L = NDTF - 1
      do 2410 I = 1, NTE
         KQQ = KORD(I + 3)
c++  Code for STIFF is inactive
c         IF (KQQ) 2302,2400,2310
cc.           EQUATION IS STIFF
c 2302    IF (LINC.GE.0) GO TO 2310
c         IF (F(L+1+I)) 2306,2308,2304
cc.     ORDER WAS INCREASED, AND THUS MUST BE DECREASED (KQQ.LT.0)
c 2304    KQQ=KQQ+1
c         KORD(I+3) = KQQ
c         GO TO 2308
cc.     ORDER WAS DECREASED
c 2306    KQQ=KQQ-1
c 2308    KQQ=max(2,-KQQ)
c         GO TO 2350
c++  End
c     EQUATION IS NOT STIFF
 2310    if (KQQ .gt. 2) then
            if (F(L + KQQ) .eq. C0) then
c                 ORDER WAS INCREASED, AND THUS MUST BE DECREASED
               KQQ = KQQ - 1
               KORD(I + 3) = KQQ
            end if
         end if
         J = min(KQQ, KSC)
         KQMAXI = max(KQMAXI, KQQ)
         if (KQQ .ne. 1) F(L + KQQ + 1) = 0.D0
c           BACK UP FOR BACKWARD DIFFERENCES
         do 2360 K = 1, J
            F(L + K) = F(L + K) - F(L + K + 1)
 2360    continue
         if (KQQ .gt. KSC) then
c           BACK UP FOR MODIFIED DIVIDED DIFFERENCES
            do 2390 K = J+1, KQQ
               F(L + K) = (F(L+K) - F(L+K+1)) / BETA(K)
 2390       continue
         end if
 2400    F(L + KQQ + 1) = F(L + KQQ + 1) / BETA(KQQ + 1)
         L = L + NUMDT
 2410 continue
c END OF LOOP TO BACK UP DIFFERENCE TABLES
c ********
c BACK UP XI TO BEGINNING OF THE STEP
c ********
      I = KSC + 1
      if (I - IOP11 - 1) 2420, 2440, 2450
 2420 TPD = XI(1)
c                Check below needed when starting?
      if (TPD .eq. XI(2)) go to 2450
      do 2430 K = I, IOP11
 2430    XI(K - 1) = XI(K) - TPD
 2440 XI(IOP11) = C2 * XI(IOP11 - 1)
      if (IOP11 .ne. 2) XI(IOP11) = XI(IOP11) - XI(IOP11 - 2)
 2450 KQICON = -1
      ICF = NE
      ICS = 1
      LDT = 1
      return
      end
c   End of DIVABU

      subroutine DIVACO(ID, RD)
c>> 1987-12-07 DIVACO Krogh   Initial code.
c
c THIS SUBROUTINE RETURNS THE FOLLOWING DATA FROM COMMON
c ID(1) = KEMAX  =  INDEX OF EQUATION WITH LARGEST ERROR ESTIMATE
c ID(2) = KSTEP  =  CURRENT STEP NUMBER
c ID(3) = NUMDT  =  NUMBER OF DIFFERENCES USED FOR EACH EQUATION
c ID(4) =           RESERVED FOR FUTURE USE
c ID(5) =           RESERVED FOR FUTURE USE
c RD(1) = EMAX   =  MAX. RATIO OF ESTIMATED ERROR TO REQUESTED ERROR
c RD(2) =           RESERVED FOR FUTURE USE
c RD(3) =           RESERVED FOR FUTURE USE
c
      integer ID(5)
      double precision RD(3)
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT, TOLG
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,TOLG,HC,HDEC,HINC,
     1   HINCC,HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c
      ID(1) = KEMAX
      ID(2) = KSTEP
      ID(3) = NUMDT
      RD(1) = EMAX
      return
      end
c   End of DIVACO

      subroutine DIVACR(Y, F, KORD, TOL, LGROUP)
c>> 1988-08-25 DIVACR Krogh   Fix bug in relative error test.
c>> 1988-01-15 DIVACR Krogh   Initial code.
c
c THIS SUBROUTINE
c   1. CORRECTS Y FOR EQUATIONS WHICH ARE NOT STIFF
c   2. ESTIMATES ERRORS
c   3. SELECTS INTEGRATION ORDERS
c   4. TESTS IF NOISE LIMITS THE PRECISION
c
c     Y = VECTOR OF PREDICTED VALUES ON ENTRY, AND OF CORRECTED
c         VALUES WHEN THE RETURN IS MADE.
c LGROUP= VECTOR INDICATING HOW ERROR TOLERANCES ARE TO BE GROUPED
c         (AND POSSIBLY HOW INTEGRATION ORDERS ARE TO BE GROUPED).
c   TOL = VECTOR CONTAINING ERROR TOLERANCES (AND POSSIBLY RELATIVE
c         ERROR FACTORS).
c     F = VECTOR GIVING PREDICTED DERIVATIVE VALUES AND DIFF. TABLES.
c    KD = VECTOR GIVING ORDERS OF THE DIFFERENTIAL EQUATIONS
c         (IF EQUATIONS HAVE DIFFERENT ORDERS).
c    KQ = VECTOR OF INTEGRATION ORDERS.
c
      integer LGROUP(*), KORD(*)
c--D Next line special: P=>D, X=>Q
      double precision Y(*)
      double precision TOL(*), F(*)
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT, TOLG
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,TOLG,HC,HDEC,HINC,
     1   HINCC,HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2, EEPT75, EOVEP2
      double precision OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / DIVAEV /
c
      integer L, I, KQL, KQN, KQD, JLGREP, J, K, ILGROR, ITOLOR, JLGROR,
     1   IORD, KOUTKO, KQLORD, LL, LKQMAX
      double precision CM8, CM2, CMP5, C0, CQ3125, CP1, CP125, CP25, CP5
      double precision CP75, CP8, CP9375, C1, C1P4, C2, C4, C10, C20
      double precision C1000, C40
      parameter (CM8 = -8.D0)
      parameter (CM2 = -2.D0)
      parameter (CMP5 = -.5D0)
      parameter (C0 = 0.D0)
      parameter (CQ3125 = .03125D0)
      parameter (CP1 = .1D0)
      parameter (CP125 = .125D0)
      parameter (CP25 = .25D0)
      parameter (CP5 = .5D0)
      parameter (CP75 = .75D0)
      parameter (CP8 = .8D0)
      parameter (CP9375 = .9375D0)
      parameter (C1 = 1.D0)
      parameter (C1P4 = 1.4D0)
      parameter (C2 = 2.D0)
      parameter (C4 = 4.D0)
      parameter (C10 = 10.D0)
      parameter (C20 = 20.D0)
      parameter (C40 = 40.D0)
      parameter (C1000 = 1000.D0)
      double precision TPP, HH, E, EI, EPS, ERCOEF, RND, RNOISE, S
      double precision TP2, TPS1, TPS2, TPS3, TPS4, TPS5, TPS6, TPS7
      double precision REF(4)
      double precision EIBND(KDIM-1)
c++  Code for INTEGO is active
      double precision TEMPA(4), TEMPAO(4)
c++  End
      save KOUTKO, LKQMAX
      equivalence (TPS1,TEMPA(1)), (TPS2,TEMPA(2)), (TPS3,TEMPA(3)),
     1   (TPS4, TEMPA(4))
      equivalence (G(1, 1), HH)
      integer MACT1(2), MACT2(12)
c             Parameters for Interface to MESS and DMESS
      integer MERET, METEXT, METABL
      parameter (MERET  =51)
      parameter (METEXT =53)
      parameter (METABL =55)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA KSTEP=$(I6) T=$(E15.8) H=$(E12.5) LSC=$(I3) $C
c   EIMIN=$(E8.2) EAVE=$G KSC=$(I2) SIGMA($J)=$G $C
c   RQ=$(E11.5)$G$E
c   $
cAB I$HKQ$HLI$HE$HEI$HEPS$HF$H$H$H$H
c   HIGH ORDER PREDICTED DIFFERENCES$HRNOISE$HSTIFF$HBETA$E
      integer LTXTAA,LTXTAB
      parameter (LTXTAA=  1,LTXTAB=  1)
      character MTXTAA(1) * (104)
      character MTXTAB(1) * (88)
      data MTXTAA/'KSTEP=$(I6) T=$(E15.8) H=$(E12.5) LSC=$(I3) EIMIN=$(E
     *8.2) EAVE=$G KSC=$(I2) SIGMA($J)=$G RQ=$(E11.5)$G$E'/
      data MTXTAB/'I$HKQ$HLI$HE$HEI$HEPS$HF$H$H$H$HHIGH ORDER PREDICTED$
     * DIFFERENCES$HRNOISE$HSTIFF$HBETA$E'/
c
      data MACT1 / METEXT, MERET /
c (rr=repeat, t=3/5 for I/E format)  wwddtrr  wwddtrr  wwddtrr
      data MACT2 / METABL, 1, 0, 14, 0400201, 0300202, 0801503,
     1    1507501, 1103504, 0901501, 1002501, 1205501 /
c         wwddtrr  wwddtrr  wwddtrr  wwddtrr  wwddtrr
c          End of stuff for interface to message processor
c
      data REF(1), REF(2), REF(3), REF(4) / C1, CP9375, CP75, CP5 /
c++ Save data by elements if ~.C.
c++ Of next 20 lines, only the first KDIM-1 are active
      data EIBND(1) / .1D0 /
      data EIBND(2) / .1D0 /
      data EIBND(3) / .14D0 /
      data EIBND(4) / .19D0 /
      data EIBND(5) / .26D0 /
      data EIBND(6) / .36D0 /
      data EIBND(7) / .50D0 /
      data EIBND(8) / .69D0 /
      data EIBND(9) / .94D0 /
      data EIBND(10) / C1 /
      data EIBND(11) / C1 /
      data EIBND(12) / C1 /
      data EIBND(13) / C1 /
      data EIBND(14) / C1 /
      data EIBND(15) / C1 /
      data EIBND(16) / C1 /
      data EIBND(17) / C1 /
      data EIBND(18) / C1 /
      data EIBND(19) / C1 /
C     data EIBND(20) / C1 /
c
c++  Code for ARGM is inactive
c      RETURN
c      ENTRY DIVACE
c++  End
c ********
c START OF CODE
c ********
      L = NDTF - 1
      if (ICS .ne. 1) L = L + (ICS - 1) * NUMDT
      do 3340 I = ICS, ICF
         if (NKDKO .ne. 0) KORDI = KORD(NKDKO + I - 1)
         IY = IY + abs(KORDI)
         KQL = KORD(I + 3)
         KQN = abs(KQL)
         KQD = max(2, KQN)
c ********
c OBTAIN ERROR TOLERANCE SPECIFIED BY THE USER
c ********
         if (I .le. ILGREP) if (KQL) 2600, 3310, 2610
         ITOLEP = abs(ITOLEP) + 1
         EPS = TOL(ITOLEP)
         ILGREP = LGROUP(ITOLEP)
c   TEST IF SIMPLE ABSOLUTE ERROR TEST IS BEING USED
         if (ILGREP .gt. 0) go to 2580
         JLGREP = ILGREP
c     GET OLD RELATIVE ERROR FACTOR
         TPS6 = TOL(ITOLEP + 1)
         ILGREP = LGROUP(ITOLEP + 1)
         ITOLEP = -ITOLEP - 1
c
         if (JLGREP + 1) 2540, 2570, 2510
c   NO CHECK ON THE ERROR ESTIMATE IS TO BE MADE
 2510    if (EPS + C1) 2520, 2590, 2520
c   ERROR TOLERANCE IS SPECIFIED IMPROPERLY
 2520    KEMAX = I
         NEPTOL = ITOLEP
         LINC = -3
         EREP = -abs(EREP)
         return
c   COMPUTE NEW RELATIVE ERROR FACTOR
 2540    continue
         TPS1 = C0
         do 2550 J = I, ILGREP
            TPS1 = TPS1 + abs(F(J))
 2550       continue
         TPS1 = abs(HH) * TPS1 / dble(ILGREP - I + 1)
         if (LSC .le. 2) go to 2560
c     ON FIRST 3 STEPS INCREASE TPS6 WHEN COMPUTING REL. ERROR FACTOR
         TPS6 = max(C4 * TPS1, TPS6)
c     ON 1-ST TIME THROUGH THE FIRST STEP, REL. ERR. FAC. IS NOT STORED
         if (LSC .eq. 7) go to 2570
 2560    continue
         TPS6 = max(TPS1, TPS6)
c   STORE NEW RELATIVE ERROR FACTOR
         TOL(-ITOLEP) = TPS6 * REF(-JLGREP - 1)
c   COMPUTE ABSOLUTE ERROR TOLERANCE
 2570    EPS = EPS * TPS6
 2580    if (EPS .le. C0) go to 2520
 2590    if (KQL) 2600, 3330, 2610
c END OF OBTAINING ERROR TOLERANCE
c ********
c OBTAIN INFORMATION USED FOR ERROR ESTIMATION, ORDER SELECTION, ETC.
c ********
c EQUATION IS STIFF
 2600    continue
c++  Code for STIFF is inactive
c      JS=abs(KORD(NJSKO+I-1))-1
c      JSI=JS
c      TPP=C0
c      TPS4=F(L+KQD+2)
c      TPS3=F(L+KQD+1)
c      TPS2=F(L+KQD)
c      TPS1=F(L+KQD-1)
c      IF (KQD.EQ.2) TPS1=Y(IY-1)
c      E=ABS(TPS3)+ABS(TPS4)
c      EI=E+ABS(TPS2)
c      RND=EI
c      IF (KORDI.GE.0) GO TO 2604
cc.    EQUATION IS IMPLICIT
c      JSI=JSI-1
c      IF (JSI.NE.0) GO TO 2604
c      IF (KORDI.EQ.-1) GO TO 2602
c      ERCOEF=GS(KQN+1)
c      GO TO 2606
c 2602 ERCOEF=.5D0*DS(KQD,1)
c      JSI=1
c      GO TO 2606
cc.    END OF SPECIAL CODE FOR IMPLICIT EQUATIONS
c 2604 ERCOEF = DS(KQD,JSI)
c 2606 ERCOEF = ABS(ERCOEF) / EPS
c      IF (LSC.LE.2)  GO TO 2710
c      IF (LSC-5) 2650,2710,2710
cc.  END OF CODE FOR STIFF EQUATIONS
c++  End
c
c EQUATION IS NOT STIFF
 2610    TPP = F(I) - F(L + 1)
         TPS3 = TPP
         TPS4 = TPP - F(L + KQD + 1)
         TPS2 = TPP + F(L + KQD)
         TPS1 = TPP + F(L + KQD - 1)
         E = abs(TPS3) + abs(TPS4)
         RND = E
         EI = E + abs(TPS2)
         ERCOEF = abs(GS(KQN + 1)) / EPS
         if (KQL .ge. 4) go to 2710
c   TEST IF STARTING OR IF INTEGRATION ORDER IS ONE
         if (LSC .le. 2) if (KQL - 2) 2660, 2710, 2710
c ********
c LOGIC ASSOCIATED WITH STARTING THE INTEGRATION
c ********
         TPS4 = C0
         if (LSC - 4) 2650, 2640, 2620
c FIRST STEP
 2620    E = E * CQ3125
         TPS3 = C0
         F(L + 4) = C0
         S = C0
c   TEST IF FIRST TIME THROUGH THE FIRST STEP
         if (LSC .eq. 7) go to 2690
c   COMPUTE S=ESTIMATE OF H * EIGENVALUE OF JACOBIAN = 2*(F(A)-F(B))/
c   (F(B)-F(C)) WHERE F(A)=CURRENT F(I), AND F(B) AND F(C) PRECEDING
c   VALUES OR ESTIMATES OF F(I)
         TPP = F(I) - F(L + 5)
         TPS4 = TPP
         E = C2 * abs(TPS4)
         if (S .ne. C0) S = (TPS4 + TPS4) / S
         if (S + CP125) 2630, 2700, 2700
c     SET LDT=-5  TO INDICATE POSSIBLE PROBLEMS DUE TO INSTABILITY
 2630    LDT = -5
         go to 2690
c   ADJUST CORRECTION MADE ON SECOND STEP
 2640    TPP = CP8 * TPP
c   ADJUST ESTIMATED ERRORS ON SECOND AND THIRD STEPS
 2650    E = abs(TPS3)
         RND = C4 * E
         go to 2710
c END OF SPECIAL LOGIC FOR STARTING
c ********
c INTEGRATION ORDER =1 IS TREATED AS A SPECIAL CASE
c ********
 2660    TPP = TPP + F(L + 2)
         if (BETA(2) .ge. C1P4) EI = EI * C1000
c   ESTIMATE NEW VALUE FOR S
         S = F(L + 4)
         if (S .eq. C0) go to 2680
         S = max(CM8, C2 * BETA(2) * (TPS1 - TPS2 - F(L + 5)) / S)
         if (S .ge. CMP5) go to 2670
c   MODIFY TPP (TO GET BETTER STABILITY CHARACTERISTICS)
         TPP = TPP * max(CP25, (CM2 - C2 * S) / (S * S))
 2670    TPS4 = TPS4 * abs(S)
 2680    E = CP25 * (E + abs(TPS4))
         EI = EI + abs(TPS4 * S)
c     STORE INFORMATION REQUIRED TO ESTIMATE S ON NEXT STEP
 2690    F(L + 4) = TPP
 2700    F(L + 5) = F(I)
c END OF SPECIAL CODE FOR INTEGRATION ORDER =1
c ********
c CODE FOR NOISE TEST AND GETTING ERROR ESTIMATE
c ********
 2710    E = E * ERCOEF
         RNOISE = C0
         if (EPS .lt. C0) go to 2810
         TPS5 = abs(F(L + 2)) + abs(F(I))
         if (TPS5 .eq. C0) go to 2760
 2720    RNOISE = RND / TPS5
         if (RNOISE .gt. RBQ(KQD)) if (RNOISE - C1) 2760, 2750, 2750
c   NOISE IS APPARENTLY SLOWING CONVERGENCE OF THE DIFFERENCES
c     REDUCE EI
         EI = RND
         TPS5 = abs(EEPS2 * Y(IY - 1)) / EPS
         if (TPS5 .lt. abs(E)) if (LSC) 2730, 2730, 2760
         E = TPS5
         RNOISE = C0
 2730    E = -abs(E)
         if (EIMIN .gt. CP1) EI = (C10 * EIMIN) * EI
c     COMPUTE REDUCTION TO BE MADE IN EI
         if (RNOISE .gt. (C20 * RBQ(KQD))) go to 2760
         K = -6 - LSC
 2740    if (K .le. 0) go to 2760
c     REDUCE EI WHEN NOISE APPARENTLY LIMITS PRECISION
         K = K - 1
         EI = CP5 * EI
         if (EI .gt. EIMIN) go to 2740
         go to 2760
 2750    TPS4 = 1.1D0 * RND
         TPS3 = RND
 2760    continue
c   TEST FOR STIFFNESS GOES HERE WHEN IMPLEMENTED
c *       INGREDIENTS OF TEST MAY INCLUDE --
c *       RNOISE, WHETHER (ABS(TPS4).GT.ABS(TPS3)),
c *       WHETHER EMAX IS INCREASING, RESULT OF TEST ON
c *       PREVIOUS STEPS, ETC.
c
c ********
c COMPUTE ERROR ESTIMATES AND INFORMATION FOR SELECTING THE STEPSIZE
c ********
         if (E .ge. abs(EMAX)) go to 2770
         if (-E .le. abs(EMAX)) go to 2780
         SNOISE = RNOISE
         DNOISE = RND
         NOISEQ = KQD
c   STORE PARAMETERS ASSOCIATED WITH LARGEST VALUE OF E
 2770    EMAX = E
         KEMAX = I
         NEPTOL = ITOLEP
c   DETERMINE HOW MUCH STEPSIZE CAN BE INCREASED
 2780    EI = EI * ERCOEF * SIGMA(KQD)
         EIMAX = max(EIMAX, EI)
         if (LINC .le. 0) go to 2810
         K = 0
 2790    if (EI .ge. min(EIMIN, EIBND(KQN))) go to 2800
         K = K + 1
         if (K .eq. LINC) go to 2810
         EI = EI * SIGMA(KQD)
         go to 2790
 2800    LINC = K
c END OF COMPUTING ERROR ESTIMATES
 2810    continue
c++  Code for ERRSTO is inactive
c      IF (IOP20 .EQ. 0) GO TO 780
cc.********
cc.STORE ERROR ESTIMATE (OPTIONAL)
cc.********
c      F(IOP20+I-1)=TPS3*GS(KQN+1)
cc.END OF STORING ERROR ESTIMATE
c++  Code for INTEGO | ERRSTO is active
         if (IOP19 .eq. 0) go to 3090
c.********
c.EQUATIONS ARE GROUPED TO USE SAME INTEGRATION METHOD (OPTIONAL)
c.********
c++  Code for INTEGO is active
         if (I .gt. 1) if (I - ILGROR) 2900, 2900, 2830
         ITOLOR = IOP19
 2830    JLGROR = KORD(ITOLOR)
         ITOLOR = ITOLOR + 1
         if (JLGROR .gt. 0) go to 2870
         ILGROR = KORD(ITOLOR)
         ITOLOR = ITOLOR + 1
         if (JLGROR + 1) 2840, 2850, 2890
 2840    if (JLGROR .lt. -2) if (KQD + JLGROR) 2850, 2880, 2880
c.INITIALIZE FOR ACCUMULATING VARIABLES USED IN ORDER SELECTION
 2850    IORD = I
         KQLORD = KQL
         do 2860 K = 1, 4
 2860       TEMPAO(K) = abs(TEMPA(K))
         go to 2930
c.ORDERS IN CURRENT GROUP CAN BE DIFFERENT
 2870    ILGROR = JLGROR
         go to 3090
c.ORDER IS NOT GOING TO BE CHANGED
 2880    JLGROR = 0
 2890    if (KQL) 3240, 3270, 3270
c.TAKE ACTION FOR EQUATION WHICH IS NOT THE FIRST IN THE GROUP
 2900    if (JLGROR) 2910, 2890, 3090
c.ACCUMULATE VARIABLES USED IN ORDER SELECTION
 2910    do 2920 K = 1, 4
 2920       TEMPAO(K) = TEMPAO(K) + abs(TEMPA(K))
c.    TEST IF THIS IS LAST EQUATION IN THE GROUP
 2930    if (I .ne. ILGROR) if (KQL) 3310, 3290, 3290
c.SET UP TO GO SELECT INTEGRATION ORDER
         KQL = 0
         do 2940 K = 1, 4
 2940       TEMPA(K) = TEMPAO(K)
         go to 3090
c.INTEGRATION ORDER HAS BEEN SELECTED
c++  Code for INTEGO | STIFF is active
 2950    continue
c++  Code for INTEGO is active
         KQL = KQLORD
         if (KQN - abs(KQL)) 2960, 2980, 3020
c.  TEST IF ORDER CAN BE DECREASED
 2960    if (JLGROR .ge. -2) if (KQL) 3010, 3040, 3040
c.    INTEGRATION ORDER WAS SELECTED OUTSIDE PERMITTED RANGE
 2970    KQN = abs(KQL)
c.    INTEGRATION ORDER IS NOT GOING TO BE CHANGED
 2980    if ((KQL .ne. 1) .or. (LSC .gt. 0)) if (KQL) 3030, 3040, 3040
c.    SET  4-TH ENTRY IN DIFFERENCE TABLES SO THAT STANDARD ADAMS
c.    METHOD IS USED WHEN KQL=1
 2990    do 3000 K = IORD, I
 3000       F(NDTF + K*NUMDT - NUMDT + 3) = C0
         go to 3270
c.  ORDER FOR STIFF EQUATION WAS REDUCED
 3010    continue
c++  Code for INTEGO & STIFF is inactive
c      IF (KQN.LT.JSI) GO TO 990
c      TPP=-C1
c      GO TO 1090
cc.  TEST IF ORDER CAN BE INCREASED
c++  Code for INTEGO is active
 3020    if (JLGROR .eq. -2) go to 2970
c++  Code for INTEGO & STIFF is inactive
c      IF (KQL.GE.0) GO TO 1140
c      IF ((JSI.NE.0).AND.(KQN.GT.(MAXKQD+JSI))) GO TO 990
c      TPP=C1
cc.  STORE RESULTS FOR STIFF EQUATIONS
c++  Code for INTEGO is active
 3030    continue
c++  Code for INTEGO & STIFF is inactive
c      DO 3035 K=IORD,I
c      KORD(K+3) = -KQN
c 3035 F(NDTF+K*NUMDT-NUMDT)=TPP
c      GO TO 3245
cc.  STORE RESULTS FOR EQUATIONS WHICH ARE NOT STIFF
c++  Code for INTEGO is active
 3040    LL = NDTF + NUMDT * IORD - NUMDT
         do 3080 J = IORD, I
            KORD(J + 3) = KQN
            if (KQN - KQL) 3050, 3070, 3060
 3050       F(LL + KQD - 1) = F(LL + KQD - 1) + (F(J) - F(LL))
            go to 3080
 3060       F(LL + KQN) = F(LL + KQD)
 3070       F(LL + KQD) = C0
 3080       LL = LL + NUMDT
         if (KQN - 1) 3270, 2990, 3270
c++  End
c.********
c.SELECT INTEGRATION ORDER
c.********
 3090    if (LSC .le. 0) go to 3120
c. SPECIAL ORDER SELECTION WHEN STARTING
         if (LSC - 3) 3110, 3210, 3100
 3100    if (LSC .eq. 5) if (S + .125D0) 3160, 3130, 3130
         if (LSC - 6) 3130, 3130, 3210
 3110    if (C40 * min(abs(TPS4), abs(TPS3)) .gt. abs(TPS2)) then
            if (EPS .ne. -C1) LSC = 2
         end if
         if (abs(TPS4) .lt. abs(TPS3)) if (C4 * abs(TPS4) - abs(TPS2))
     1      3130, 3130, 3210
c.  CHECK IF ORDER CAN BE INCREASED OR SHOULD BE DECREASED
 3120    TPS5 = ROBND * abs(TPS4)
         TPS6 = ROBND * (TPS5 + abs(TPS3))
         TPS7 = abs(TPS1) + abs(TPS2)
         if (TPS5 .ge. abs(TPS3)) go to 3140
         if (TPS6 .ge. TPS7) go to 3210
 3130    if (KQN .ge. MAXKQI) go to 3210
c.    INCREASE THE INTEGRATION ORDER
         KQN = KQN + 1
c++  Code for INTEGO | STIFF is active
         if (KQL) 3230, 2950, 3250
c++  Code for ~(INTEGO | STIFF) is inactive
c      GO TO 3250
c++  End
c.  CHECK IF ORDER SHOULD BE DECREASED
 3140    if (TPS6 .lt. TPS7) go to 3210
         if (TPS5 .lt. abs(TPS3 - TPS4)) go to 3210
         if ((TPS3.eq.TPS4) .and. (LSC.le.0)) go to 3210
         if (KQN - 2) 3210, 3160, 3180
 3160    KQN = 1
c++  Code for INTEGO | STIFF is active
         if (KQL) 3220, 2950, 3170
c++  End
c.    WHEN ORDER IS REDUCED TO 1 WITH ADAMS METHOD SET F(L+4)=0
 3170    F(L + 4) = C0
         go to 3260
c.    DECREASE THE INTEGRATION ORDER
 3180    KQN = KQN - 1
c++  Code for INTEGO | STIFF is active
         if (KQL) 3220, 2950, 3200
c++  End
 3200    F(L+KQD) = F(L+KQD) + TPP
         go to 3260
c   NO CHANGE IN INTEGRATION ORDER IS BEING MADE
 3210    continue
c++  Code for INTEGO is active
         if (KQL) 3240, 2950, 3270
c++  Code for ~INTEGO is inactive
c         TPS1 = EEPS10
c      GO TO 1530
c++  End
c END OF SELECTING INTEGRATION ORDER
c ********
c COMPUTE MAXIMUM INTEGRATION ORDERS AND SET NEW ONES (IF ANY)
c ********
c EQUATION IS STIFF
c     ORDER WAS DECREASED
c++  Code for INTEGO | STIFF is active
 3220    continue
c++  Code for STIFF is inactive
c      IF (KQN.LT.JSI) GO TO 3236
c      F(L+1)=-C1
c      GO TO 3233
cc.    ORDER WAS INCREASED
c++  Code for INTEGO |  STIFF  is active
 3230    continue
c++  Code for STIFF is inactive
c      IF ((JSI.NE.0).AND.(KQN.GT.(MAXKQD+JSI))) GO TO 3236
c      F(L+1)=C1
c 3233 KORD(I+3) = -KQN
c      GO TO 3245
c      ORDER WAS SET TO AN UNACCEPTABLE VALUE
c 3236 KQN=abs(KQL)
c      ORDER IS NOT BEING CHANGED
c++  Code for STIFF |  INTEGO is active
 3240    continue
c++  Code for STIFF is inactive
c      F(L+1)=C0
c 3245 IF (JSI.NE.0) KQMAXD=max(KQN,KQMAXD)
c      IF (JS.LT.abs(KORDI)) KQMAXI=max(KQN,KQMAXI)
c      GO TO 3290
c++  End
c EQUATION IS NOT STIFF
c     ORDER INCREASED
 3250    F(L + KQN + 1) = -F(L + KQD + 1)
         if (LSC .gt. 0) F(L + KQN + 1) = F(L + 1) - F(I)
c     ORDER CHANGED
 3260    KORD(I + 3) = KQN
 3270    KQMAXI = max(KQN, KQMAXI)
         if (EPS .gt. C0) KQMAXS = max(KQN, KQMAXS)
         F(L + KQD + 1) = C0
 3290    continue
         if (KQN .gt. KIS) go to 3310
c.********
c.DETERMINE IF TIME TO STORE SOLUTION (OPTIONAL)
c.********
         if (KIS .ge. 1000) then
            TP2 = max(1.5D0, dble(KQN) * C2 ** (1001 - KIS)) * abs(TPS4)
 3295       if (TP2 .gt. abs(F(L+KQN))) then
               if (KQN .le. KQL) then
                  KQN = KQN - 1
                  if (KQN .gt. 1) go to 3295
                  KQN = 1
               end if
            end if
            KORD(I+3) = KQN
            if (I .eq. 1) LKQMAX = 0
            LKQMAX = max(KQN, LKQMAX)
            KQMAXI = LKQMAX
            if (KIS .eq. 1000) then
               if (I .eq. KEMAX) EMAX = dble(8 + KQN**2) * abs(EMAX)
               go to 3325
            end if
c++  Code for DUMP is active
         else if ((E .ne. C0) .and. (EPS .gt. C0)) then
            if (IOP9 .gt. 0) if((abs(E)*dble(KIS-KQN+2)**(KQN+1))-1.D-2)
     1         3310, 3310, 3300
 3300       KIS = -1
c++  End
         end if
 3310    continue
c ********
c CORRECT
c ********
         do 3320 K = 1, KORDI
c++  Code for ~{p,x} is active
            Y(IY - K) = Y(IY - K) + G(KQL + 1, K) * TPP
c++  Code for {p,x} is inactive
Cc--D Next line special: P=>D, X=>Q
C            Y(IY - K) = Y(IY - K) + dble(G(KQL + 1, K)) * dble(TPP)
c++  END
 3320    continue
c END OF CORRECTING
 3325 continue
c++  Code for OUTPUT is active
      if (IOP10 .gt. 0) then
         if (I .eq. 1) then
            IDAT(1) = KSTEP
            IDAT(2) = LSC
            IDAT(3) = KSC
            IDAT(4) = IOP11
            FDAT(1) = TN
            FDAT(2) = HH
            FDAT(3) = EIMIN
            FDAT(4) = EAVE
            FDAT(5) = SIGMA(IOP11)
            FDAT(6) = ROBND
            MACT2(3) = NTE
c--D Next line special: P=>S, X=>D
            call DMESS(MACT1, MTXTAA, IDAT, FDAT)
            KOUTKO = NOUTKO
         end if
         if (KOUTKO .ne. 0) then
            if (KORD(KOUTKO) .gt. 0) then
               if (I .lt. KORD(KOUTKO)) go to 3328
               KOUTKO = KOUTKO + 1
            else
               if (I .ge. abs(KORD(KOUTKO))) KOUTKO = KOUTKO + 1
            end if
         end if
         IDAT(1) = I
         IDAT(2) = KQL
         IDAT(3) = LINC
         FDAT(1) = E
         FDAT(2) = EI
         FDAT(3) = EPS
         FDAT(4) = F(I)
         FDAT(5) = TPS1
         FDAT(6) = TPS2
         FDAT(7) = TPS3
         FDAT(8) = TPS4
         FDAT(9) = RNOISE
         FDAT(10) = 0.D0
         if (KQL .eq. 1) FDAT(10) = S
         FDAT(11) = BETA(KQD)
c--D Next line special: P=>S, X=>D
         call DMESS(MACT2, MTXTAB, IDAT, FDAT)
 3328    if (I .eq. NTE) IOP10 = IOP10 - 1
      end if
c++  End
 3330    L = L + NUMDT
 3340    continue
      return
      end
c   End of DIVACR

      subroutine DIVAHC
c>> 1988-05-20 DIVAHC Krogh   Initial code.
c
c SUBROUTINE TO COMPUTE COEFFICIENTS REQUIRED FOR INTEGRATING
c ORDINARY DIFFERENTIAL EQUATIONS
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT, TOLG
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,TOLG,HC,HDEC,HINC,
     1   HINCC,HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2, EEPT75, EOVEP2
      double precision OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / DIVAEV /
c                 K - 1 + 1 / K  is equivalent to max(1, K-1)
      double precision GG(MAXORD - 1 + 1/MAXORD), B(KDIM+MAXORD),
     1   W(KDIM+MAXORD)
      integer  K, N, J
      double precision C0, CP1, CRBQI, CP5, CP5625, C1, C1P125
      parameter (C0 = 0.D0)
      parameter (CP1 = .1D0)
      parameter (CRBQI = .421875D0)
      parameter (CP5 = .5D0)
      parameter (CP5625 = .5625D0)
      parameter (C1 = 1.D0)
      parameter (C1P125 = 1.125D0)
c++  Code for STIFF is inactive
c      INTEGER          GODIF
c++  End
      double precision TP1, TP2, HH, TEMP, TP
      equivalence (G(1, 1), HH)
c
      save GG, W
c
c  B(K)= 1/(K*(K+1))
c++ Save data by elements if ~.C.
c++ Of next 23 lines, only the first KDIM+MAXORD are active
      data B(1)  / 5.000000000000000000000000000000000000000D-1 /
      data B(2)  / 1.666666666666666666666666666666666666667D-1 /
      data B(3)  / 8.333333333333333333333333333333333333333D-2 /
      data B(4)  / 5.000000000000000000000000000000000000000D-2 /
      data B(5)  / 3.333333333333333333333333333333333333333D-2 /
      data B(6)  / 2.380952380952380952380952380952380952381D-2 /
      data B(7)  / 1.785714285714285714285714285714285714286D-2 /
      data B(8)  / 1.388888888888888888888888888888888888889D-2 /
      data B(9)  / 1.111111111111111111111111111111111111111D-2 /
      data B(10) / 9.090909090909090909090909090909090909091D-3 /
      data B(11) / 7.575757575757575757575757575757575757576D-3 /
      data B(12) / 6.410256410256410256410256410256410256410D-3 /
      data B(13) / 5.494505494505494505494505494505494505495D-3 /
      data B(14) / 4.761904761904761904761904761904761904762D-3 /
      data B(15) / 4.166666666666666666666666666666666666667D-3 /
      data B(16) / 3.676470588235294117647058823529411764706D-3 /
      data B(17) / 3.267973856209150326797385620915032679739D-3 /
      data B(18) / 2.923976608187134502923976608187134502924D-3 /
      data B(19) / 2.631578947368421052631578947368421052632D-3 /
      data B(20) / 2.380952380952380952380952380952380952381D-3 /
      data B(21) / 2.164502164502164502164502164502164502165D-3 /
      data B(22) / 1.976284584980237154150197628458498023715D-3 /
C     data B(23) / 1.811594202898550724637681159420289855072D-3 /
c
c ********
c START OF CODE
c ********
c     SET STEP NUMBER OF METHOD
c++  Code for STIFF is inactive
c      IOP11 = MIN(max(KQMAXI,KQMAXD) + 1), KDIM)
c++  Code for ~STIFF is active
      IOP11 = MIN(KQMAXI + 1, KDIM)
c++  End
c     TEST IF STEPSIZE WAS CHANGED
      if (KQICON .ge. 0) go to 3510
c ********
c STEPSIZE JUST CHANGED
c ********
c     SET CONSTANTS DEPENDING ON NEW STEPSIZE
      KQMXIL = KQMAXI
      TP1 = HH
      GG(1) = TP1 * TP1
      G(1, 2) = GG(1) * CP5
      if (MAXINT .le. 2) go to 3450
      do 3440 K = 3, MAXINT
         GG(K - 1) = G(1, K - 1) * TP1
         G(1, K) = GG(K - 1) / dble(K)
 3440    continue
c     SET CONSTANTS INDICATING STEP CHANGE
 3450 KQICON = 0
c++  Code for STIFF is inactive
c      KQDCON=0
c++  End
      KQMXIP = 1
      KSC = 1
      if (LSC .lt. 7) go to 3490
c     SPECIAL SET-UP OF CONSTANTS ON THE VERY FIRST STEP
      HINCC = C1P125
      LINCD = 6
      LINCQ = 12
      if (HINC .gt. C0) go to 3460
      LINCD = -2
      LINC = -2
      ROBND = C1
 3460 SIGMA(1) = 1.0D0
      BETA(1) = C1
      do 3470 N = 1, IOP11
c++  Code for STIFF is inactive
c      D(1,N)=C0
c++  End
         XI(N) = TP1
         ALPHA(N) = C1
         BETA(N + 1) = C1
         SIGMA(N + 1) = dble(N + 1) * SIGMA(N) * HINCC
 3470    continue
      TEMP = EEPS16
      RBQ(1) = C1
      RBQ(2) = CP1
      TP = CRBQI
c     **** IN THE LOOP BELOW RBQ(K) IS COMPUTED TO BE
c          APPROXIMATELY (3/4 ** ((K-1) ** 2 - 1) / 10
c          .5625 = (3/4) ** 2    TP = (3/4) ** (2*K -3)
      do 3480 K = 3, KDIM
         TEMP = TEMP + TEMP
         RBQ(K) = max(TEMP, RBQ(K - 1) * TP)
 3480    TP = TP * CP5625
      go to 3560
c     SET-UP AFTER THE FIRST STEP
 3490 TP2 = XI(1)
      XI(1) = TP1
      BETA(2) = TP1 / TP2
      K = 2
      if (HINCC .eq. HINC) go to 3540
      if ((LSC .ne. 0) .or. ((KSTEP-KSSTRT-KQMAXS) .lt. 10)) go to 3540
      HINCC = C1
      LINCD = 0
 3500 LINCD = LINCD + 1
      HINCC = HINCC * HINC
      if (HINCC .lt. 2.D0) go to 3500
      LINC = (LINC * (LINCD + LINCD)) / LINCQ
      LINCQ = LINCD + LINCD
      HINCC = HINC
      go to 3540
c END OF LOGIC FOR CASE WHEN STEPSIZE JUST CHANGED
c     TEST IF MAXIMUM INTEGRATION ORDER DID NOT INCREASE
 3510 if (KQMAXI .gt. KQMXIL) then
c ********
c INTEGRATION ORDER WAS INCREASED -- GET NEW V'S
c ********
         KQMXIL = KQMAXI
         KQMXIP = KQMXIL + MAXINT
         K = KQMXIP
         V(K) = B(K)
         if (KQICON .eq. 1) go to 3530
c     if (KQICON .eq. K) KQICON = KQICON - 1 --- Removed 1999-08-19
         do 3520 N = 2, KQICON
            K = K - 1
 3520       V(K) = V(K) - ALPHA(N) * V(K + 1)
c END OF GETTING NEW V'S
      else
         IOP11 = max(IOP11, KQMXIL+1)
      end if
 3530 if (IOP11 .le. KSC) go to 3560
c ********
c COMPUTE PARAMETERS WHICH ARE STILL CHANGING AS A RESULT OF
c A CHANGE IN THE STEPSIZE
c ********
      TP2 = XI(KSC)
c     UPDATE CONSTANT STEP COUNTER
      KSC = KSC + 1
      K = KSC
      BETA(K) = C1
 3540 continue
      TEMP = HINCC
c
c   LOOP TO COMPUTE NEW VALUES OF PARAMETERS
      do 3550 N = K, IOP11
         TP1 = TP2 + HH
         TP2 = XI(N)
         XI(N) = TP1
         ALPHA(N) = HH / TP1
         BETA(N + 1) = BETA(N) * (TP1 / TP2)
         TEMP = max(TEMP, dble(N) * (ALPHA(N) * HINCC))
         SIGMA(N) = SIGMA(N - 1) * TEMP
 3550    continue
      if (IOP11 .ne. KDIM) XI(IOP11 + 1) = TP2 + HH
c END OF CODE FOR COMPUTING PARAMETERS WHICH ARE STILL CHANGING
c
 3560 if (KQICON .ge. KQMXIP) go to 3690
c ********
c COMPUTE INTEGRATION COEFFICIENTS WHICH ARE STILL CHANGING
c ********
      KQMXIL = max(KQMAXI, KQMXIL)
      KQMXIP = KQMXIL + MAXINT
      J = KQMXIP - KQICON
      N = KQICON + 1
      KQICON = N
      if (N .ne. 1) go to 3580
c INITIALIZE V AND W
      do 3570 K = 1, J
         V(K) = B(K)
 3570    W(K) = V(K)
      go to 3600
c UPDATE V AND INITIALIZE W
 3580 if (N .eq. KDIM) go to 3690
      do 3590 K = 1, J
         V(K) = V(K) - ALPHA(N) * V(K + 1)
 3590    W(K) = V(K)
c SET TRANSFER FOR LOOP BELOW DEPENDING ON VALUE OF MAXINT
 3600 continue
      go to 3660
c
 3640 J = J - 1
c INNER LOOP FOR COMPUTING INTEGRATION COEFFICIENTS
      do 3650 K = 1, J
 3650    W(K) = W(K) - ALPHA(N) * W(K + 1)
c     STORE INTEGRATION COEFFICIENTS
 3660 G(N + 1, 1) = HH * W(1)
      GS(N + 1) = G(N + 1, 1) - G(N, 1)
c++  Code for MAXORD >= 2 is active
      if (MAXINT .ge. 2) then
         G(N + 1, 2) = GG(1) * W(2)
c++  Code for MAXORD >= 3 is inactive
c        if (MAXINT .gt. 2) then
c           DO 3665 K=3,MAXINT
c3665          G(N+1,K)=GG(K-1)*W(K)
c        end if
c++  Code for MAXORD >= 2 is active
      end if
c++  End
      N = N + 1
      if (N .le. KQMXIL) go to 3640
c END OF COMPUTING INTEGRATION COEFFICIENTS
c
 3690 continue
c++  Code for STIFF is inactive
c      IF (KQDCON.GT.KQMAXD) GO TO 4662
cc.********
cc.COMPUTE DIFFERENTIATION COEFFICIENTS WHICH ARE STILL CHANGING
cc.********
cc.SET TRANSFER FOR LOOP BELOW, DEPENDING ON VALUE OF MAXDIF
c++  Code for STIFF & MAXORD >= 2 is inactive
c      IF (MAXDIF-2) 3693,3692,3691
c 3691 ASSIGN 3696 TO GODIF
c      GO TO 3694
c 3692 ASSIGN 3698 TO GODIF
c      GO TO 3694
c 3693 ASSIGN 3699 TO GODIF
c++  Code for STIFF is inactive
c 3694 KQDCON=KQDCON+1
cc.LOOP FOR COMPUTING DIFFERENTIATION COEFFICIENTS
c      DO 3699 N=KQDCON,KQMAXD
c      DS(N+1,2)=C1/XI(N)
c      D(N+1,1)=DS(N+1,2)+D(N,1)
c      DS(N+1,1)=DS(N+1,2)/D(N+1,1)
c++  Code for STIFF & MAXORD >= 2 is inactive
c      GO TO GODIF, (3696,3698,3699)
c 3696 CONTINUE
c++  Code for STIFF & MAXORD >= 3 is inactive
c      DO 3697 K=3,MAXDIF
c      DS(N+1,K)=D(N,K-2) * (K-1)/XI(N)
c 3697 D(N+1,K-1)=DS(N+1,K) + D(N,K-1)
c++  Code for STIFF is inactive
c 3698 CONTINUE
c++  Code for STIFF & MAXORD >= 2 is inactive
c      D(N+1,MAXDIF)=D(N,MAXDIF) + D(N,MAXDIF-1) * (MAXDIF)/XI(N)
c++  Code for STIFF is inactive
c 3699 CONTINUE
c++  End
c
c END OF COMPUTING DIFFERENTIATION COEFFICIENTS
      return
      end
c   End of DIVAHC

      subroutine DIVAIN(T, Y, F, KORD)
c>> 1988-01-14 DIVAIN Krogh   Initial code.
c
c  SUBROUTINE TO DO INTERPOLATION FOR VARIABLE ORDER INTEG. ROUTINE
c
      integer KORD(*)
c--D Next line special: P=>D, X=>Q
      double precision T(*), Y(*)
      double precision F(*)
      integer KDIM, MAXORD
c++ Substitute for KDIM, MAXORD below
      parameter (KDIM = 20, MAXORD = 2)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
      save / DIVASC /
      integer I, ICI, IDT, INTERP, INTEG, INTEGZ, IY, IYI, IYN, IYNI, J,
     1    K, KQMXI, KQMXS, KQQ, L, N
      double precision C0, C1, C2
      parameter (C0 = 0.D0)
      parameter (C1 = 1.D0)
      parameter (C2 = 2.D0)
      double precision C(KDIM+MAXORD-1), ETA(KDIM)
      double precision GAMMA(KDIM)
      double precision TP1, HI
      double precision CSUM(KDIM+MAXORD-1)
c--D Next line special: P=>D, X=>Q
      double precision XP1
      logical LNOTM1
c
c              Stuff for processing error messages
      integer IDAT(1)
      double precision FDAT(6)
      integer MENTXT, MERET, MEEMES, METEXT
      parameter (MENTXT =23)
      parameter (MERET  =51)
      parameter (MEEMES =52)
      parameter (METEXT =53)
      integer MACT(8)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DIVAIN$B
cAB Interpolating at T(1)=$F with $B
cAC TN=$F, T(2)=$F and H=$F.  T(1) must be in [$F, $F].$E
cAD internal variable LDT = $I.  Interpolation not allowed now.$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD
      parameter (LTXTAA=  1,LTXTAB=  9,LTXTAC= 41,LTXTAD= 94)
      character MTXTAA(1) * (154)
      data MTXTAA/'DIVAIN$BInterpolating at T(1)=$F with $BTN=$F, T(2)=$
     *F and H=$F.  T(1) must be in [$F, $F].$Einternal variable LDT = $I
     *.  Interpolation not allowed now.$E'/
c
c                      1 2 3 4       5 6       7      8
      data MACT / MEEMES,0,0,0, MENTXT,0, METEXT, MERET /
c
c++  Code for ARGM is inactive
c      ENTRY DIVAIE
c++  End
c ********
c START OF CODE -- CHECK ON STATE OF DIFFERENCE TABLE
c ********
      L = LDT
      if (L) 3710, 3730, 3780
 3710 if (L + 2) 4170, 3730, 3720
 3720 if (MAXINT .ge. 0) L = 1
      go to 3840
c ********
c UPDATE DIFFERENCE TABLE TO START OF NEXT STEP
c ********
 3730 K = NDTF
      do 3770 I = 1, NTE
         KQQ = KORD(I + 3)
         if (KQQ .le. 0) go to 3760
c EQUATION IS NOT STIFF
         TP1 = F(I) - F(K)
c LOOP TO DO UPDATING
         N = K + max(abs(KQQ), 2)
         do 3750 J = K, N
 3750       F(J) = F(J) + TP1
 3760    continue
         K = K + NUMDT
 3770    continue
      LDT = 1
      if (L .ne. 0) return
c END OF UPDATING DIFFERENCE TABLE
c ********
c INITIALIZE FOR COMPUTATION OF COEFFICIENTS
c ********
 3780 INTERP = 0
      HI = T(1) - TN
      GAMMA(1) = HI / XI(1)
      if (GAMMA(1)) 3790, 3800, 3810
 3790 if (GAMMA(1) .ge. -C1) go to 3820
      INTERP = 1
      if (abs(HI) - abs(T(2))) 3820, 3820, 4180
 3800 INTERP = 2 - KQMAXI
      go to 3820
 3810 if (GAMMA(1) .gt. C2) if (LDT - 2) 4180, 3820, 3820
 3820 KQMXI = KQMAXI + INTERP - 1
c++  Code for STIFF is inactive
c      KQMXS=max(KQMXI,KQMAXD)
c++  Code for ~STIFF is active
      KQMXS = KQMXI
c++  End
      do 3830 N = 2, KQMXS
 3830    GAMMA(N) = (HI + XI(N-1)) / XI(N)
 3840 LNOTM1 = L .ne. -1
      INTEG = MAXINT
      if (INTEG .le. 0) if (INTEG + MAXDIF) 4160, 3950, 3950
c ********
c COMPUTE INTEGRATION COEFFICIENTS
c ********
c     INITIAL SET-UP
c         COMPUTE INITIAL C VALUES
      do 3850 N = 1, INTEG
         C(N) = HI / dble(N)
 3850 continue
      I = INTEG + 1
      INTEG = INTEG + KQMXI
      do 3860 N = I, INTEG
         C(N) = C(N - 1) * (dble(N - MAXINT) / dble(N))
 3860    continue
c         COMPUTE ETA'S
      do 3870 N = 1, KQMXI
 3870    ETA(N) = HI / XI(N)
c         COMPUTE C(K)'S TO CORRESPOND TO G(K-MAXINT+1,MAXINT),
c         K=MAXINT, MAXINT+1,..., MAXINT+KQMXI-1
      I = INTEG
 3880 J = INTEG
      INTEG = J - 1
      if (INTEG .le. MAXINT) go to 3900
      do 3890 N = J, I
 3890    C(N) = ETA(N - INTEG) * C(N) + C(N - 1)
      go to 3880
 3900 do 3910 N = J, I
 3910    C(N) = ETA(N - INTEG) * C(N)
c         END OF COMPUTING  G(---,MAXINT)
      INTEGZ = 0
      go to 3940
c         COMPUTE C(K)-S TO CORRESPOND TO G(K-INTEG+1,INTEG),
c         K=INTEG+1,INTEG+2,..., INTEG+KQMXI
 3920 do 3930 N = 1, KQMXI
 3930    C(INTEG+N) = GAMMA(N)*C(INTEG+N-1) - ETA(N)*C(INTEG+N)
 3940 ICI = INTEG - 1
      go to 4020
c END OF COMPUTING INTEGRATION COEFFICIENTS
c ********
c COMPUTE COEFFICIENTS FOR INTERPOLATION
c ********
 3950 C(1) = C1
      ICI = 0
      do 3960 N = 1, KQMXS
 3960    C(N + 1) = GAMMA(N) * C(N)
      if (INTEG + 1) 3970, 3990, 4010
c END OF COMPUTING INTERPOLATION COEFFICIENTS
c
c     SET-UP TO COMPUTE DIFFERENTIATION COEFFICIENTS REQUIRED
c     IN ORDER TO GET COEFFICIENTS ACTUALLY USED
 3970 INTEG = 0
      ICI = 1
 3980 INTEG = INTEG - 1
      if (INTEG .eq. MAXINT) ICI = 0
c ********
c COMPUTE DIFFERENTIATION COEFFICIENTS
c ********
 3990 INTERP = max(INTERP, 0)
      TP1 = dble(-INTEG)
      C(1) = TP1 * C(1) / XI(-INTEG)
      J = KQMAXD + INTEG
      do 4000 N = 1, J
 4000    C(N + 1) = (TP1*C(N)) / XI(N - INTEG) + GAMMA(N - INTEG) * C(N)
c     C(N) NOW CORRESPONDS TO THE DIFFERENTIAL COEFFICIENT
c          D(N-INTEG,-INTEG)
 4010 INTEGZ = INTEG
      if (ICI .ne. 0) go to 3980
c END OF COMPUTING DIFFERENTIATION COEFFICIENTS
c ********
c BEGINNING OF LOOP TO DO
c         INTEGRATION       (INTEG.GT.0)
c         INTERPOLATION     (INTEG.EQ.0)
c         DIFFERENTIATION   (INTEG.LT.0)
c TO THE POINT INDICATED BY T.
c ********
c     SET UP INITIAL INDICES
 4020 if (NYNY .lt. 0) then
         IY = -NYNY
         IYNI = NYNY + ICI + 1
         if (LDT .eq. 2) then
            CSUM(ICI+1) = C(ICI+1)
            do 4025 J = ICI+2, INTEG+KQMXI
            CSUM(J) = CSUM(J-1) + C(J)
 4025       continue
         end if
      else
         IY = 1
         IYNI = NYNY + ICI - 1
      end if
      IDT = NDTF - INTEGZ
      do 4140 I = 1, NTE
         if (NKDKO .ne. 0) KORDI = KORD(NKDKO + I - 1)
         IY = IY + abs(KORDI)
         KQQ = KORD(I + 3)
c         GET INDEX OF HIGHEST ORDER DIFFERENCE TO BE USED
         K = max(abs(KQQ) + INTERP, 2)
         IYI = -INTEG
         if (KQQ) 4030, 4130, 4040
c EQUATION IS STIFF
 4030    continue
c++  Code for STIFF is inactive
c      JS=abs(KORD(NJSKO+I-1))-1
c      IYI=IYI-JS
c      IF(LNOTM1) IF (IYI) 4034,4032,4130
c      IF (KORDI.LT.0) IYI=IYI+1
c      IYI=IYI+MAXINT-abs(KORDI)
c      IF (IYI) 4034,4130,4130
cc.      IF EQUATION IS IMPLICIT DO NOT COMPUTE AN F
c 4032 IF (KORDI.LT.0) GO TO 4130
cc.      TEST IF INTEG TOO BIG FOR THIS EQUATION
c 4034 IF (abs(KORDI).LT.-IYI) GO TO 4130
c      IYI=IYI+IY
c      IYN=IYI+IYNI
cc. COMPUTE INNER PRODUCT FOR STIFF EQUATIONS
c      IF (INTEGZ.EQ.0) GO TO ???
cc.    DIFFERENTIATING
c      TP1 = C0
c      DO 4036 J = K+INTEGZ, 1, -1
c         TP1 = TP1 + C(J) * F(IDT+J-1)
c 4036 CONTINUE
cc.    TEST WHETHER TO STORE RESULT IN Y OR F
c      IF (IYI-IY) 4080, 4090, 4080
cc.    INTEGRATING OR INTERPOLATING
c      TP1 = C0
c      DO 4037 J = ICI + K, ICI + 2, -1
c         TP1 = TP1 + C(J) * F(IDT+J-ICI-1)
c 4037 CONTINUE
c      IF (INTEG.EQ.0) GO TO 4120
c      TP1=TP1 + C(ICI+1)*Y(IYN+1)
c++  End
         go to 4100
c END OF SPECIAL CODE FOR STIFF EQUATIONS
c
c EQUATION IS NOT STIFF
 4040    if (LNOTM1) if (IYI) 4050, 4060, 4130
         IYI = IYI + MAXINT - KORDI
         if (IYI .ge. 0) go to 4130
c       TEST IF INTEG TOO BIG FOR THIS EQUATION
 4050    if (KORDI .lt. -IYI) go to 4130
 4060    IYI = IYI + IY
         IYN = IYI + IYNI
c  COMPUTE INNER PRODUCT FOR EQUATION WHICH IS NOT STIFF
         XP1 = C0
         if (LDT .eq. 2) then
            if (KQQ .ne. KQMAXI) XP1 = CSUM(K+INTEGZ+ICI) *
     1         F(IDT+INTEGZ+NUMDT-1)
         end if
         do 4070 J = K + INTEGZ + ICI, ICI + 1, -1
            XP1 = XP1 + C(J) * F(IDT - ICI - 1 + J)
 4070       continue
         if (INTEG) 4080, 4090, 4100
c STORE FINAL RESULT IN Y WHEN DIFFERENTIATING
 4080    continue
         Y(IYI) = XP1
         go to 4130
c STORE INTERPOLATED VALUE IN F (OR STIFF DIFFERENTIATION)
 4090    F(I) = XP1
         go to 4130
c PICK UP EXTRA STUFF TO ADD TO INNER PRODUCT WHEN INTEGRATING
 4100    K = ICI
         if (K .eq. 0) go to 4120
 4110    continue
         XP1 = C(K) * (XP1 + Y(IYN))
         IYN = IYN - 1
         K = K - 1
         if (K .ne. 0) go to 4110
c STORE FINAL RESULT IN Y WHEN INTEGRATING (OR STIFF INTERPOLATION)
 4120    Y(IYI) = XP1 + Y(IYN)
 4130    continue
         IDT = IDT + NUMDT
 4140    continue
c
      INTEG = INTEG - 1
      if (INTEG .ge. -MAXDIF) if (INTEG) 3990, 3950, 3920
 4160 return
c ********
c ERROR PROCESSING
c ********
 4170 MACT(2) = 68
      MACT(3) = 11
      MACT(6) = LTXTAD
      IDAT(1) = LDT
      go to 4190
 4180 MACT(2) = 28
      MACT(3) = 1
      MACT(6) = LTXTAC
      FDAT(2) = TN
      FDAT(3) = T(2)
      FDAT(4) = XI(1)
      FDAT(5) = TN - T(2)
      FDAT(6) = TN + C2 * XI(1)
      if (XI(1) .lt. 0) then
         FDAT(5) = FDAT(6)
         FDAT(6) = TN - T(2)
      end if
 4190 FDAT(1) = T(1)
c--D Next line special: P=>S, X=>D
      call DMESS(MACT, MTXTAA, IDAT, FDAT)
      if (MACT(2) .lt. 50) go to 3820
      return
      end
c   End of DIVAIN

      subroutine DIVAOP(IOPT, FOPT)
c>> 1987-12-07 DIVAOP Krogh   Initial code.
c
c  SUBROUTINE TO SET UP OPTIONS FOR DIFFERENTIAL EQUATION  PACKAGE -IVA
      double precision FOPT(*)
      integer IOPT(*)
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT, TOLG
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,TOLG,HC,HDEC,HINC,
     1   HINCC,HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2, EEPT75, EOVEP2
      double precision OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / DIVAEV /
c
      integer IOPTS(23), INCOP(22), IOPTC(23), I, IA, J, K, LIOPT, MULTJ
      double precision CMP75, C0, CP25, CP3, CP5, CP625, CP75, CP875,
     1   CP9, C1, C1P125, C2, C4, C10, C16
      parameter (CMP75 = (-.75D0))
      parameter (C0 = 0.D0)
      parameter (CP25 = .25D0)
      parameter (CP3 = .3D0)
      parameter (CP5 = .5D0)
      parameter (CP625 = .625D0)
      parameter (CP75 = .75D0)
      parameter (CP875 = .875D0)
      parameter (CP9 = .9D0)
      parameter (C1 = 1.D0)
      parameter (C1P125 = 1.125D0)
      parameter (C2 = 2.D0)
      parameter (C4 = 4.D0)
      parameter (C10 = 10.D0)
      parameter (C16 = 16.D0)
      external D1MACH
      double precision D1MACH
      equivalence (IOPTC(3), IOP3)
      save IOPTS, LIOPT
c
c                      Declarations for error message processing.
c
      integer MECONT, MERET, MEEMES, MEIVEC
      parameter (MECONT =50)
      parameter (MERET  =51)
      parameter (MEEMES =52)
      parameter (MEIVEC =57)
      integer MACT(7), MACT1(5)
c
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DIVAOP$B
cAB Error in IOPT() specifications: IOPT =$E
cAC HMIN = $F is > HMAX = $F.$E
      integer LTXTAA,LTXTAB,LTXTAC
      parameter (LTXTAA= 1,LTXTAB= 9,LTXTAC=49)
      character MTXTAA(1) * (76)
      data MTXTAA/'DIVAOP$BError in IOPT() specifications: IOPT =$EHMIN$
     * = $F is > HMAX = $F.$E'/
c **** End of text generated by pmess
c                      1   2  3   4       5  6      7
      data MACT / MEEMES, 88, 24, 0, MEIVEC, 0, MERET /
      data MACT1 / MEEMES, 28, 24, LTXTAC, MERET /
c
c                      IOP4       IOP17
      data IOPTS / 3*0, 500000, 12*0, 1, 6*0 /
c
c                  1  2    3  8  9 10 11 12  13  16   17 21 22
      data INCOP / 1, 3, 5*2, 1, 2, 3, 1, 2, 3*1, 3, 4*2, 2, 2 /
c
c ********* START OF EXECUTABLE CODE ***********************
c
      MULTJ = 1
      K = 1
 4200 I = IOPT(K)
      IA = abs(I)
c 1 and 6 lines below need 21 changed if more options are added.
      if (IA .le. 21) if (I) 4220, 4520, 4280
      if (IA .ne. 1111) go to 4490
      if (I .lt. 0) then
        MULTJ = -1
        K = K + 1
        go to 4200
      end if
      IOPT(2) = LIOPT
c
c     ****  INITIALIZE FOR STARTING A NEW INTEGRATION
      do 4210 J = 3, 23
 4210    IOPTC(J) = IOPTS(J)
      KSOUT = IOPTS(4)
      KMARK = 1 - IOPTS(1)
      KORDI = IOPTS(17)
      NKDKO = max(-KORDI, 0)
      IOPST = IOPTS(22)
      go to 4260
c
c     **** SET A NOMINAL VALUE
 4220 IOPTS(IA) = 0
      if (IA .eq. 12) go to 4420
      if (IA - 2) 4400, 4240, 4230
 4230 if (IA .eq. 4) IOPTS(4) = 500000
      if (IA .eq. 21) TOLG = 0.D0
      go to 4390
c
c     **** SET ALL OPTIONS TO THEIR NOMINAL VALUES
 4240 IA = 1
      IOPTS(1) = 0
      do 4250 J = 3, 22
         IOPTS(J) = 0
         IOPTC(J) = 0
 4250 continue
      IOPTS(4) = 500000
      IOPTC(4) = IOPTS(4)
      IOPTS(17) = 1
      TOLG = 0.D0
 4260 NGTOT = IOPTS(7) + max(IOPTS(6), 0)
      if (IOPTS(12) .eq. 0) go to 4420
 4270 return
c
c     **** SET SPECIFIED OPTION
 4280 J = IOPT(K + 1)
      if (INCOP(IA) - 2) 4290, 4330, 4300
c     **** OPTION INVOLVES NO EXTRA PARAMETERS
 4290 IOPTS(IA) = 1
      if (IA - 2) 4400, 4400, 4390
c     **** TAKE CARE OF SECOND EXTRA PARAMETER
 4300 if (IA .ne. 10) go to 4310
      NOUTKO = IOPT(K + 2)
      if (NOUTKO) 4500, 4350, 4350
 4310 if (IA .ne. 16) go to 4320
      NTOLF = IOPT(K + 2)
      if (NTOLF) 4500, 4500, 4350
 4320 if (J .eq. 3) then
        if (KMARK .ne. 3) then
           if (XI(1)*(FOPT(IOPT(K+2)) - TMARK) .ge. C0) go to 4400
        end if
      end if
      TMARK = FOPT(IOPT(K+2))
      KMARK = J
      go to 4400
c     **** TAKE CARE OF FIRST EXTRA PARAMETER
 4330 continue
      if (IA .eq. 12) go to 4410
      if (IA .eq. 4) KSOUT = J
      if (IA .eq. 21) TOLG = FOPT(J)
 4350 IOPTS(IA) = J * MULTJ
      if (abs(IA - 7) .gt. 1) go to 4360
c     **** SET SPECIAL PARAMETERS FOR GSTOP-S
      IGFLG = 0
      NGTOT = IOPTS(7) + max(IOPTS(6), 0)
c     **** TEST FOR ERROR
      if (J .gt. 500) go to 4500
 4360 if (J .gt. 0) go to 4390
      if ((IA .eq. 5) .or. (IA .eq. 17)) go to 4390
      if (J + 1) 4500, 4370, 4380
 4370 if (IA .eq. 7) go to 4500
 4380 if ((IA .eq. 4) .or. (IA .eq. 11) .or. (IA .ge. 16)) go to 4500
c     **** STORE SAVED VALUE IN COMMON
 4390 IOPTC(IA) = IOPTS(IA)
c
c     **** INCREMENT K TO GET NEXT OPTION
 4400 K = K + INCOP(IA)
      go to 4200
c
c ******* SET UP INFORMATION FOR CHANGING STEPSIZE *********
c
c     **** TEST IF VALUES ARE ALREADY SET
 4410 if (IOPTS(12) .ne. 0) go to 4430
c     **** SET NOMINAL VALUES FOR VARIABLES ONLY SET ONCE
 4420 EREP = CP3
c     **** SET NOMINAL VALUES FOR STEPSIZE CONTROL AND ENV. CONSTANTS
      EEPS2 = D1MACH(4)
      EEPS16 = C16 * EEPS2
      EEPS10 = CP625 * EEPS16
      EEPT75 = EEPS2 ** CP75
      EEPS2 = EEPS2 + EEPS2
      OVD10 = D1MACH(2)
      EROV10 = C10 / OVD10
      EOVEP2 = OVD10 * EEPS2
      OVTM75 = OVD10 ** CMP75
      OVD10 = OVD10 / C10
      HINC = C2
      HDEC = CP5
      HMIN = EROV10
      HMAX = OVD10
      if (I .ne. 12) go to 4470
 4430 IOPTS(12) = J * MULTJ
      if (J) 4450, 4470, 4460
c     **** SET UP TO GIVE USER COMPLETE STEPSIZE CONTROL
 4450 EREP = C1 / EROV10
      HINC = -C2
      IOP8 = 1
c## Recent code 12/16/94
      LINCD = -2
      LINC = -2
      ROBND = C1
c## Recent code 9/6/2001
      TOLG = 0.D0
c## End of recent code
      go to 4480
c     **** SET USER VALUES FOR STEPSIZE CONTROL
 4460 if (FOPT(J) .ne. C0) HINC = max(C1P125, min(FOPT(J),C4))
      if (FOPT(J + 1) .ne. C0) HDEC = min(CP875, max(FOPT(J + 1), CP25))
      if (FOPT(J + 2) .ne. C0) HMIN = FOPT(J + 2)
      if (FOPT(J + 3) .ne. C0) HMAX = FOPT(J + 3)
      if ((HMIN .gt. HMAX) .or. (HMAX .le. 0.D0)) then
         call DMESS(MACT1, MTXTAA, IOPT, FOPT(J+2))
         KORD2I = -4
      end if
 4470 KQICON = -1
 4480 HMAXP9 = HMAX * CP9
      if (I - 1111) 4400, 4270, 4400
c
c ***************** ERROR  IN  IOPT ************************
c
 4490 IA = 1
 4500 MACT(6) = K + INCOP(IA) - 1
      call MESS(MACT, MTXTAA, IOPT)
      KORD1I = 24
      KORD2I = -4
c Usual return with no error is here.
 4520 LIOPT = K
      return
      end
c   End of DIVAOP

      subroutine DIVAPR(Y, YN, F, KORD)
c>> 1988-01-13 DIVAPR Krogh   Initial code.
c
c THIS SUBROUTINE
c   1. UPDATES THE DIFFERENCE TABLE FROM THE PREVIOUS STEP (IF NOT
c      DONE ALREADY).
c   2. PREDICTS WHAT THE VALUES OF THE DEPENDENT VARIABLES, Y, AND
c      THE DIFFERENCE TABLE, DT, WILL BE AT THE END OF THE CURRENT STEP.
c
c   Y = VECTOR OF PREDICTED VALUES COMPUTED BY THIS SUBROUTINE.
c   YN= VECTOR OF VALUES OF Y COMPUTED ON THE LAST STEP.
c   F = VECTOR OF DERIVATIVE VALUES.
c   DT= ARRAY CONTAINING DIFFERENCE TABLES.
c   KD= VECTOR GIVING ORDERS OF THE DIFFERENTIAL EQUATIONS (IF
c       EQUATIONS HAVE DIFFERENT ORDERS).
c   KQ= VECTOR OF INTEGRATION ORDERS.
c
      integer KORD(*)
c--D Next line special: P=>D, X=>Q
      double precision Y(*), YN(*)
      double precision F(*)
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT, TOLG
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2
      double precision EEPT75, EOVEP2, OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / DIVAEV /
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,TOLG,HC,HDEC,HINC,
     1   HINCC,HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
      double precision C0
      parameter (C0 = 0.D0)
c
      integer I, INTEG, INTEGS, J, K, KQQ, L, N
      double precision TEMP(KDIM)
      double precision TP1
c--D Next line special: P=>D, X=>Q
      double precision XP
      data INTEGS / -1 /
c
c++  Code for ARGM is inactive
c      RETURN
c      ENTRY DIVAPE
c++  End
c ********
c START OF CODE
c ********
      IY = 0
      L = NDTF - 1
      do 4680 I = 1, NTE
         INTEG = KORDI
         if (NKDKO .ne. 0) INTEG = KORD(NKDKO + I - 1)
         KQQ = KORD(I + 3)
         K = max(abs(KQQ), 2)
         if (KQQ) 4530, 4520, 4540
 4520    IY = IY + abs(INTEG)
         go to 4670
c ********
c EQUATION IS STIFF, OR IMPLICIT
c ********
 4530    continue
c++  Code for STIFF is inactive
c      KQQ=-KQQ
c      N=KQQ-1
c      JS=abs(KORD(NJSKO+I-1))-1
c      IMPLIC=INTEG
c      INTEG=abs(IMPLIC)-JS
cc.    SET INTEGS FOR STIFF EQUATIONS
c      INTEGS=0
c      IF (K-KSC) 160,160,140
cc.END OF SET-UP FOR STIFF EQUATIONS
c++  End
c ********
c EQUATION IS NOT STIFF
c ********
 4540    N = KQQ
         if (LDT .ne. 0) if (K - KSC) 4570, 4570, 4550
c     DIFFERENCE TABLE HAS NOT BEEN UPDATED
         TP1 = F(I) - F(L + 1)
         if (K - KSC) 4610, 4610, 4590
c END OF SET-UP FOR EQUATIONS WHICH ARE NOT STIFF
c ********
c GET PREDICTED DIFFERENCES FROM UPDATED DIFFERENCE TABLE
c ********
 4550    F(L + K + 1) = F(L + K + 1) * BETA(K + 1)
         TEMP(K) = F(L + K) * BETA(K)
         F(L + K) = TEMP(K)
c LOOP FOR MODIFIED DIVIDED DIFFERENCES
 4560    K = K - 1
         if (K .le. KSC) go to 4580
         TEMP(K) = F(L + K) * BETA(K)
         F(L + K) = TEMP(K) + F(L + K + 1)
         go to 4560
c CODE FOR BACKWARD DIFFERENCES
 4570    F(L + K + 1) = F(L + K + 1)
         TEMP(K) = F(L + K)
         K = K - 1
c
 4580    TEMP(K) = F(L + K)
         F(L + K) = TEMP(K) + F(L + K + 1)
         K = K - 1
         if (K .ne. 0) go to 4580
         go to 4630
c ********
c UPDATE DIFFERENCE TABLE AND GET PREDICTED DIFFERENCES
c ********
c CODE FOR MODIFIED DIVIDED DIFFERENCES
 4590    F(L + K + 1) = (F(L+K+1) + TP1) * BETA(K + 1)
         TEMP(K) = (F(L + K) + TP1) * BETA(K)
         F(L + K) = TEMP(K)
 4600    K = K - 1
         if (K .le. KSC) go to 4620
         TEMP(K) = (F(L + K) + TP1) * BETA(K)
         F(L + K) = TEMP(K) + F(L + K + 1)
         go to 4600
c CODE FOR BACKWARD DIFFERENCES
 4610    F(L + K + 1) = (F(L+K+1) + TP1)
         TEMP(K) = F(L + K) + TP1
         F(L + K) = TEMP(K)
         K = K - 1
c
 4620    TEMP(K) = F(L + K) + TP1
         F(L + K) = TEMP(K) + F(L + K + 1)
         K = K - 1
         if (K .ne. 0) go to 4620
c ********
c COMPUTE Y-S OBTAINED USING INTEGRATION
c ********
c     TEST IF NEXT Y TO BE OBTAINED BY INTERPOLATION
 4630    continue
c++  Code for STIFF is inactive
c      IF (INTEG.EQ.0) GO TO 4662
c++  End
         IY = IY + 1
c     FORM INNER PRODUCT
         XP = C0
         do 4650 J = INTEGS + N + 1, INTEGS + 2, -1
c++  Code for ~{p,x} is active
            XP = XP + G(J, INTEG) * TEMP(J)
c++  Code for {p,x} is inactive
Cc--D Next line special: P=>D, X=>Q
C            XP = XP + dble(G(J, INTEG)) * dble(TEMP(J))
c++  END
 4650    continue
         K = INTEG + INTEGS
         do 4660 J = K, 1, -1
c++  Code for ~{p,x} is active
            XP = XP + G(1, J) * YN(IY + J)
c++  Code for {p,x} is inactive
Cc--D Next line special: P=>D, X=>Q
C            XP = XP + dble(G(1, J)) * dble(YN(IY + J))
c++  END
 4660    continue
         Y(IY) = YN(IY) + XP
         INTEG = INTEG - 1
         if (K) 4670, 4670, 4630
c END OF COMPUTING Y-S OBTAINED BY INTEGRATION
c ********
c COMPUTE Y-S OBTAINED USING INTERPOLATION AND DIFFERENTIATION
c ********
c++  Code for STIFF is inactive
cc.    RESTORE INTEGS FOR EQUATIONS WHICH ARE NOT STIFF
c 4662 INTEGS=-1
c      IY=IY+1
cc.    COMPUTE Y USING INTERPOLATION
c      Y(IY)=YN(IY) + F(L+2)
c      IF (KQQ.EQ.1) Y(IY)=YN(IY)
c 4663 INTEG=INTEG+1
c      IF (INTEG.EQ.JS) IF (IMPLIC) 4680,4680,4664
cc.    COMPUTE INTEG-TH DERIVATIVE
c      XP = C0
c 4664 DO 4666 J = KQQ+1, INTEG+1, -1
c         XP = XP + D(J, INTEG) * TEMP(J)
c 4666 CONTINUE
c      IF (INTEG.EQ.JS) GO TO 4667
c      IY=IY+1
c      Y(IY)=XP
c      GO TO 4663
cc.STORE PREDICTED VALUE FOR F
c 4667 CONTINUE
c      F(L+NUMDT)=XP
c++  End
 4670    L = L + NUMDT
 4680    continue
      LDT = -3
      return
      end
