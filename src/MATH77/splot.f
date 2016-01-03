C++ CODE for .C. is inactive
C%% static FILE *inunit, *iotemp, *iotmp1, *iotmp2, *iofil;
C++ END
      subroutine SPLOT (XSIZE, YSIZE, X, NX, Y, OPT, COPT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2009-10-30 SPLOT Krogh -- Initialized ARRLEN & CAPLOC(5:6)
c>> 2009-10-27 SPLOT Krogh -- BSLAS1 => BSLAS1(1:1) for NAG compiler.
c>> 2009-10-18 SPLOT Krogh -- Added "save adjin" in splota
c>> 2009-06-02 SPLOT  Krogh -- Read from file sets number of points.
c>> 2005-12-06 SPLOT  Krogh -- Some fixes for conversion to C.
c>> 2005-05-09 SPLOT  Krogh -- More debug, fixed parens & bad test
c>> 2001-05-24 SPLOT  Krogh -- Added commas to some formats.
c>> 2000-01-02 SPLOT   Minor correction to error message text.
c>> 2000-12-30 SPLOT   Added an (int) cast in some comments for C.
c>> 2000-10-23 SPLOT   Changed ")/)" to "/)" in a format.
c>> 2000-01-05 SPLOT   Fixed SPLOTT so IAX is defined before ref.
c>> 1999-12-27 SPLOT   "Saved" ADJOUT in SPLOTA
c>> 1999-11-23 SPLOT   Fixed so don't get empty mfpic groups at the end.
c>> 1998-10-23 SPLOT   Fixed so error index 7 prints.
c>> 1998-02-02 SPLOT   Work around for bug in HP Exemplar Compiler.
c>> 1998-01-29 SPLOT   Fixed bug when no output file given.
c>> 1998-01-21 SPLOT   Work around for \ treated as escape in SGI F77.
c>> 1998-01-14 SPLOT   Krogh Initial code.
c--S replaces "?": ?PLOT, ?PLOT0, ?PLOT1, ?PLOT2, ?PLOT4, ?PLOT5,
c-- &  ?PLOT6, ?PLOT7, ?PLOT8, ?PLOT9, ?PLOTA, ?PLOTB, ?PLOTC, ?PLOTD,
c-- &  ?PLOTE, ?PLOTF, ?PLOTL, ?PLOTN, ?PLOTR, ?PLOTS, ?PLOTT, ?PLOTU,
c-- & ?MESS
C++S Default CTYPE = " (float)"
C++  Default CTYPE = "(double)"
C++ Replace " (float)" = CTYPE
C
c (SPLOTU picked up and modified from earlier code by Van Snyder).
c
c Produce line plots.  Present version produces output for MFpic.
c
c *************************** Formal Arguments *************************
c
c XSIZE [in] Physical horizontal size of the plot.  Default units are
c   inches.
c YSIZE [in] Physical vertical size of the plot.  Default units are
c   inches.
c X [in] Array of NX abscissae.
c NX [in] Number of abscissae in X and ordinates in Y.
c Y [in] Array of NX ordinates.
c OPT [inout] OPT(1) is status output:
c   0.0 means no problems.
c   0.0 < OPT(1) <= 10000.0 means an option index or parameter at
c     OPT(nint(OPT(1))) was out of range, had an improper parameter, or
c     was not permitted at the point it was recognized.
c   OPT(1) = 10001.0 means the output unit could not be opened.
c   OPT(1) $<$ 0.0 means an option code at COPT(-nint(OPT(1))) was not
c     recognized, or had an improper parameter.
c   Beginning in OPT(2) the user provides option specifications
c   as described in the main documentation.  (ch16-03.tex)
c COPT[in] Character options, and character data used by numeric
c   options.
c
c **************** Suggested Parameter Names for Options ***************
c
c PLOUNI=1.E0   Units, continue?, logs?, type, etc.
c PLONY=2.E0    For more than one curve at a time.
c PLOLIN=3.E0   Type of lines
c PLOBOR=4.E0   Border characteristics.
c PLOTIC=5.E0   Tick marks specs.
c PLOWHT=6.E0   Where the major ticks go.
c PLOMUL=7.E0   Multiple data sets.
c PLOXMX=8.E0   Specify X min and max.
c PLOYMX=9.E0   Specify Y min and max.
c PLOSYM=10.E0  Specify symbols, error bars, etc. for data points.
c PLOSY1=11.E0  Specify a single symbol to output.
c PLOARR=12.E0  Length of arrow head (<0 is leave on till turned off).
c PLOWID=13.E0  Various line widths
c PLOTXT=14.E0  A text annotation.
c PLONUM=15.E0  A number to output.
c PLOANL=16.E0  An annotation and/or line to output on border or axis
c PLOBAD=17.E0  For specifying data points to ignore, etc.
c PLOINP=18.E0  Specifies Fortran input unit.
c PLOLIN=19.E0  Draw a line.
c PLOREC=20.E0  Draw a rectangle.
c PLOELL=21.E0  Draw an ellipse.
c PLOPLI=22.E0  Draw a line in physical coordinates.
c PLOPRE=23.E0  Draw a rectangle in physical coordinates.
c PLOPEL=24.E0  Draw an ellipse in physical coordinates.
c PLOFIL=25.E0  Specifies filling for various cases.
c PLORAW=26.E0  Send text directly to the plot file.
c PLODEB=27.E0  Fortran debugging output.
c
c **************************** Procedure Names *************************
c
c *******  In this file, SPLOT
c SPLOT   User entry, defines all actions, processes options, initiator
c   for all actions.
c SPLOTA  Determines scalings, draws axes and tick marks, and outputs
c   axis labels.
c SPLOTF  Selects points for plotting from those provided by the user
c   for continuous curves to possibly reduce the number of points
c   required in the final output.  Also checks and takes care of
c   clipping.
c SPLOTN  Takes care of output of numeric labels.
c SPLOTT  Output of text and checking the size required by text.
c SPLOTR  Converts XY data for calls to SPLOTS (special symbols)
c SPLOTU  Opens output and scratch files.
c
c *******  In file SPLOT0  (Almost all of the device dependent code.)
c SPLOT0  Setup for starting a plot.
c SPLOT1  Specify the pen characteristics.
c SPLOT2  Draw a single straight line in physical coordinates.
c SPLOT4  Output an annotation at a given place in physical coordinates.
c SPLOT5  Draw a rectangle (possibly filled), with pen width given.
c SPLOT6  Draw a ellipse (possibly filled), with pen width given.
c SPLOT7  Take care of requests to fill future regions.
c SPLOT8  Output of tick marks.
c SPLOT9  Finish the plot.
c SPLOTL  Plot a line through a sequence of points.  Also for starting
c   and finishing other types of curves.
c SPLOTS  Plotting of special symbols, error bars, and vector fields.
c
c *************************** Internal Variables ***********************
c
c (Here and elsewhere in this package, a "*" following a name is used to
c indicate the names is in a common block.  Full coverage for variables
c in the common block only appear here.)
c
c ARRLEN* If nonzero, next line or curve is to have an arrow on the end.
c    This give the length of the arrow in points.
c BADPT   Logical variable set = .true. if got a point that requires the
c    curve to be restarted as a result of the bad point.
c BORLOC* Location of border (physical).
c C       Temporary single character.
c CAPTIO  Character array holding captions for borders/axes.
c DATSAV  Set = .true. when NY > 1 and need to save data on second
c    scratch file.
c DEGRAD  Parameter = pi / 180, converts degrees to radians.
c FILL*   Array giving dot size/space, and hatch info.  First 6 locs.
c   are for 3 pairs of info for dots,  next 9 for 3 sets of thatch info.
c FMTNUM Text defining the formatting of numbers (and text).  Indexed
c    as for LENTXT below.
c FPIN   Array where floating point data is unpacked.  Also used for
c    temporary storage.
c FPDAT  Initial values stored in FP.
c I      Temporary index.
c I1     Temporary index.
c IACT   Index for current action begin processed from LINACT.
c IAOPT  absolute value of IOPT.  Extra values of:
c     30   Flags that data for the current set follows.
c     31   Data for one abscissa follows.
c     32   Flags a bad data point, next location gives the index for Y.
c     33   Flags the end of a data set.

c IAX    Used when an axis direction is implied.  1 for x-axis
c   (horizontal), 2 for y-axis (vertical).
c IERR   Used for an error index.
c IERR1  Value to be printed with error message.
c IERR2  Value of IOP for printing of error messages.
c IERR3  Index of last char. in COPT that is O.K. for error message.
c IERR4  Index of last char. in COPT to print with error message.
c INACT  used to define actions to take for a given option.  The same
c   actions are used when reading options as when reading the scratch
c   file but the actions taken may be different.  Each action may be
c   followed by 0 or more items required by the option.
c  = 1  Take care of units/final flag & lines at x,y = 0.
c  = 2  Save border characteristics.  (Includes val. from A3)
c  = 3  Length of tick marks for various borders.
c  = 4  Set default ticks for various borders.
c  = 5  Set ?MAX / ?MIN for current set.
c  = 6  Check NY.
c  = 7  Take care of change in data set.
c  = 8  Set defaults for values .le. 0 on widths.
c  = 9  Special symbol plotting (may be more than 1 N13)
c  =10  Single symbol (may require extra args.)
c  =11  For filling (may have up to 3 for each curve type) (scratch)
c  =14  There follows in INACT, n, w, p, where
c         n = number of integers
c         w = defines where they go when processing options: 1 storage,
c             2 scratch, 3 both  (NOT USED???)
c         p = gives index in IP() where integers start.
c  =15  As for 14, except for floating point data in FP().
c  =16  Processing for text pointer.  In all cases here, the text is
c       acted upon immediately when read from the scratch file.
c       There follows in INACT, t
c        t =  9 For option 15, Sets format for next numeric output.
c        t = 16 Text output for options 14 and 16.
c       On the scratch file this writes, the length of the character
c       string, and the character string.  If this length is 0, it
c       simply means that there is no text, and in the case of t=2
c       and t=3, default actions are to be taken.
c  =17  Indicates an invalid option index in pass 1.  In pass 2, this
c       is used for the raw mfpic output.
c  =18  Take action as implied by the option index when read from the
c       scratch file.  Possibilities are, a line, a rectangle, an
c       ellipse, or any of the above in physical coordinates.
c    Options >17 only occur when reading the scratch file.
c  =20  Setup to read data.  Following locations give, LKURVE, JSET(1:2)
c  =21  Shouldn't see where using INACT in pass 2, gives an error.
c  =22  End of a data set.
c    Option    INACT for the option
c       1   1, 14,7,3,NEXT, 14,1,1,LTYPE
c       2   6, 14,2,3,LYDIM
c       3   14,1,2,LPEN
c       4   2
c       5   3
c       6   4
c       7   7
c       8   5
c       9   5
c      10   9
c      11  10
c      12  15,1,2,LARROW
c      13  8,15,4,1,LWIDTH, 15,1,2,LWIDRE
c      14  15,2,2,LVALS, 14,2,2,LTANNO, 16,16
c      15  15,3,2,LVALS, 14,2,2,LTANNO, 16,9
c      16  15,1,2,LVALS, 14,3,2,LTANNO, 16,16
c      17  15,2,1,LBAD
c      18  17,
c      19  15,4,2,LVALS, 18
c      20  15,4,2,LVALS, 18
c      21  15,5,2,LVALS, 18
c      22  15,4,2,LVALS, 18
c      23  15,4,2,LVALS, 18
c      24  15,5,2,LVALS, 18
c      25  11
c      26  17,
c      27  14,1,1,LDEBUG
c ININ   Array where integer point data is unpacked.
c IOFIL* Unit number used for output to be used for plot device.
c IOP   Current index in OPT when processing options.
c IOP1  Starting index in OPT for the current option.  Set to 0 when
c    processing COPT, set to -1 after processing OPT, and set to -100
c    - error index on an error.
c IOPT  Integer value for an option, nint(OPT(IOPT))
c IOSTA Status of the temporary files.
c   =  0  There is only one scratch file.  (Needed if digit 10^0 of
c         option 1 is a 3.)
c   =  1  There is (or will be) a second scratch file, we are currently
c         reading from the first one.
c   = -1  There is a second scratch file and we are reading from it now.
c IOTEMP Unit number used for scratch file.
c IOTMP1 If IOSTA .ne. 0, this holds unit number of first scratch file.
c IOTMP2 If IOSTA .ne. 0, this is the number of the second scratch file.
c IP*   Integer array used to store various values that are indexed by
c    parameter names.
c IPLOT*  Defines output, 0 for LaTeX, 1 for TeX.
c    Temporarily changed to -100 - IPLOT when want to end one mfpic
c    group and immediately start another.
c IRULE  Constant array mapping option indices to location in LRULE.
c    The value in LRULE and following values up to one less than the
c    location pointed to by the next location in IRULE indentify actions
c    in NRULE to used in unpacking data connected with the option if
c    positive, and if negative identify the number of floating point
c    numbers for the option.
c IY     Index for Y curve being output, when outputting the saved data
c    points.
c J      Temporary index.
c JSET*  JSET(1) gives the current set index for X, and JSET(2) gives
c   the current set index for Y.  JSET(1) starts at 1 and is incrmented,
c   JSET(2) starts at -1 and is decremented.
c K1     Temporary index.
c K2     Temporary index.
c KPT    Count of points that have been stored in XOUT and YOUT.
c KX   Pointer e.g. to NXYLIM, for the current data set for X.
c KY   Pointer e.g. to NXYLIM, for the current data set for Y.
c KURPEN Rule defining the current pen.  Defined as for P3 of option 3.
c   KURPEN = t + 10*(w + 100*(L1 + 100*L2)), where t is 0, 1, or 2 for
c   solid, dotted, or dashed lines.  t = 3 or 4 is as for 1 or 2, except
c   L1 is given in deci-points instead of points, and t = 5-8, is as for
c   1-4, except L2 if in deci-points instead of in points.  w is the
c   width of the line in decipoints, L1 and L2 are not used for solid
c   lines.  Else L1 is the diameter of the dots or the lenght of the
c   dashes, and L2 is the distance between the dots or dashes.
c LKLIP   Set to .true. if the last point saved will be clipped on the
c   next good point.
c LFILL  Array with fill pattern info.  Rows are for successive
c   patterns (usually only one row will be used).  Columns are:
c       1   For curves
c       2   For rectangles
c       3   For ellipses
c       4   Temporary for annotations.
c   Values are:
c       0   For no action, should not be used?
c       1   For fill with black.
c       2   For erase what preceded.
c       3   For shading with dots.
c       4   For shading with hatch lines.
c LRULE  See IRULE (and NRULE).
c K      Temporary index.
c KASE   1-4 for bottom, left, top,right borders, 5 and 6 for x and y
c   axis, 8 for words, 10-15 for captions, 16 for output text.

c        Indicees, 1-16, are for: Borders (bottom, left, top, right),
c   x-axis, y-axis, word alignment (e.g. for option 14), number
c   formatting for option 15, Captions (as for borders), alignment
c   rule for option 16.
c KLIP  Logical array.  Entry is true if this variable set induces
c    clipping, i.e. some points are outise the plotting area.
c KSYMB Defines the kind of symbol being plotted.  Passed to SPLOTS.
c L1     Temporary index.
c L3     Temporary index.
c LAST   Defines how called on the last call.  (Low digit of option 1)
c   = 0-2  Either not called yet, or a plot was finished with units of
c          inches, millimeters, or points in that order.
c   = 3    Curve was finished, this MFPIC finished, plot wasn't.
c   = 4    Curve was finished, plot wasn't.
c   = 5    Continuing on curve not finished on last call.
c LBNDC  Parameter giving the maximum number of characters inside {...}
c    for a caption or a file name.
c LBNDF  Parameter giving the maximum number of characters inside (...)
c    for formatting numbers or text.
c LBNDP  Parameter giving the maximum number of characters inside [...]
c    for indicating position info.
c LBNDT  Parameter giving the maximum number of characters inside {...}
c    for text or number.
c LBOUND Gives lengths allowed for chars. inside [...] (...), { ...no #
c    required}.
c LCURVE Count of the number of curves; not really needed anymore.
c LENCAP Array giving the length of the caption for borders and axes.
c LENTXT* Gives the length of various text strings.  Rows are for
c   FMTNUM, TXTDEF when getting a "#", and after getting all of TXTDEF.
c   A value of -1 means use the default value.  A value of -2 means the
c   default is not needed.  Columms are defined as
c   follows:
c    1-6     B L T R X Y -- For format of Border/Axis Labels
c    7       W -- For formatting words
c    8       N -- For formatting numbers
c    9       Option 15, format for a number to be output.
c   10-15    1-6 -- For Border/Axis Captions and formatting.
c   16       Options 14 & 16, Text to be output.
c   17       F Output file name or mfpic output
c   18       Used for temporary storage of text.
c LINACT Array giving location in INACT where actions for an option
c   begin (and end).
c LOCF   Last index used in FPIN when unpacking data.
c LOCI   Last index used in ININ when unpacking data.
c LPAR   Used to keep track of paren "{}" level.
c LSET  Index of the last index available for NXYLIM(), XYLIM(1:2,),
c   XYBASE(), XYU2PF().
c LTEXT This location in COPT contains the place where the last text
c       ended.  Used if annotation has text pointer of 0.
c LY     The value of IY if not drawing error bars or vector fieles.
c    Else, LY + 1 is the place where the data for the current curve
c    starts in FPIN().
c MANNO* Flags nature of text output.
c      = -1   Output of annotation in user coordinates.
c      =  1   Output of annotation in physical coordinates.
c      =  0   Text output is label, or axis annotation.
c MAXPT  The dimension of XOUT and YOUT, max value for KPT.
c MAXNY  Parameter giving the maximum size for NY, should be > 15.
c MAXSET* Parameter giving the maximum number of data set allowed.
c MFILL*  Absolute value gives the number of fill patterns.  If < 0,
c      then filling is not turned off, otherwise it is.
c MBORD  Defines for the various borders and axes the processing to be
c   done.  Each column corresponds to a different border or axes.  Other
c   data depends on the row index as follows.
c   = 1   From digit 10^0 of option 4 -- Border tick mark actions
c   = 2   From digit 10^1 of option 4 -- Length of arrow head, points.
c   = 3   From digit 10^{2} of option 4 -- Min. dist. to border, pts.
c   = 4   From digit 10^{3:} of option 4 -- Space for labels, points.
c   = 5   From digit 10^{1:0} of word 2 option 4 -- Number minor ticks.
c   = 6   From digit 10^2 of word 2 option 4 -- Expand range rule.
c   = 7   From digit 10^{:3) of word 2 option 4 -- => border caption.
c   = 8   Value of JSET(?) at time action for border was taken.  Columns
c         5 and 6 here are used to track extra space needed on left and
c         right borders needed for annotations.
c MODE   Defines what we are doing when processing the saved data.
c   =  0   Interpolate with Bezier curve.
c   =  1   As for 0, but a closed curve.
c   =  2   As for 1, but the curve is closed with a straight line.
c   =  3   Connect points with a straight line.
c   =  4   As for 2, but close the curve.
c   =  5   Points not connected, likely to be plotted with symbols.
c   =  6   Set when points have been connected, and now they are to be
c          plotted with symbols.
c   =  7   Plotting single symbols.
c   =  8   Doing rectangles and ellipses.
c   =  9   Doing various annotations.
c   = 10   Ready to exit.
c MOREIY  Set=.true. if have more in a set of multiple curves to output.
c NDIMY   Declared dimension of Y (used only when NY > 1)
c NMODE  Defines how we start MODE when done with what we are doing now.
c   NMODE is intialized to 10, and is never increased, and is never set
c   smaller or equal to MODE.
c NOOUT*  Set to .true. when there is no output desired (just getting
c   size required).
c NOTOUT Set .true. when starting to process an option.  Set false when
c   first write the option index to the scratch file.  Also used when
c   checking if input file is opened.
c NRULE()   Unpacking rules.  Integers are unpacked by taking the mod
c   if the integer relative to the value pointed to in NRULE.  This
c   result is saved, the original integer is divided by this same value
c   from NRULE, and the process continues until getting a value of 0, at
c   which time the current integer is saved, and the integer has been
c   unpacked.  This gives integers in the same order as they appear in
c   the documentation.
c NTEXT* The length of the text in TEXT.
c NTYP   Defines the type of text we are looking for.
c   = 1   Position info
c   = 2   Format
c   = 3   Text/number formatting info, before any "#".
c   = 4   Text/number formatting info, after getttng a "#".
c NXYLIM*Array defining what is in the columns of XYLIM and entries in
c   XYU2PF and XYBASE..  If > 0, contains the index for an XDATA set.
c   If < 0, contains the index for a Y data set.  If 0, no data for this
c   data set has been examined.
c NY      Number of curves being plotted.
c OPAQUE* .true. if the label printed is to go into an opaque box.
c OPTLET  Constant character string used to decode option letters.
c OVLAP  Estimated right end of last output number.  If a numeric
c   label looks as if it will overlap with a previous one, it is not
c   printed.
c PHYUSE* Set by option 7.  Columns 1 and 2 for x and y, row 1 gives
c   give place in physical units where points are to map, and row 2
c   give the corresponding user coordinate.  Row < 0 inactivates.
c POS*    Character array holding alignment info.  POS(4*I-3:4*I) for
c   I = 1, 16, holds data for: Borders (bottom, left, top, right),
c   x-axis, y-axis, word alignment (e.g. for option 14), number
c   formatting for option 15, Captions (as for borders), alignment
c   rule for option 16.
c PXO*    X origin of logical coordinate system in physical units.
c PXSIZE* Physical X width of the plot, including outward-pointing tick
c         marks.
c PYO*    Y origin of logical coordinate system in physical units.
c PYSIZE* Physical Y width of the plot, including outward-pointing tick
c         marks.
c SETLIM* Col. 1 for x, 2 for y, row 1 for min, 2 for max.  Give min and
c   max used for current data set.  If the log of the data is taken,
c   these values will be the logs of the input values.
c TEXT* Text to be output.
c TICKS Columns for different borders/axes.  Rows used as follows:
c   = 1  Length of major ticks.
c   = 2  Length of minor ticks.
c   = 3  Offset for major ticks  (Ignore if incrment below is .le. 0)
c   = 4  Increment for major ticks
c TLENH  Set to horizontal length of text in SPLOTN
c TLENV  Set to vertical height of text in SPLOTN.
c TOPTS* Multiplies physical coordinates to get them in units of points.
c TP     Used for tempoaray floating point storage.
c TP1    Used for tempoaray floating point storage.
c TP2    Used for tempoaray floating point storage.
c TPTSET Data used to set TOPTS.
c TXTDEF Text used to control output of text and numbers.
c TXTTST Constant character array indexed by NTYP used to detect escaped
c   characters and to track the level of "{}"'s.
c VHLEN  Array giving the vertical and horizontal space required by
c   output text, set in SPLOTT.
c XOUT   Array used to save absiccas that are ready for output.
c YOUT   Array used to save absiccas that are ready for output.
c XYBASE* See XYU2PF and NXYLIM.
c XYLIM* Rows 1 and 2 contain "minimum value", "maximum value" Columns:
c   1  From the current X data and perhaps XYMAX and XYMIN.
c   2  From the current Y data and perhaps XYMAX and XYMIN.
c  >k  From previous data sets (option 7).  See NXYLIM.
c   Originally contains min/max determined from the data.  Can be
c   changed to SETLIM when clipping is active.  This is changed to
c   physical address value when axis is processed.
c XYMAX  Maximum values set by options 8 and 9, used to set SETLIM.
c XYMIN  Minimum values set by options 8 and 9, used to set SETLIM.
c XYPOS  Use to hold (x,y) position information.
c XYU2PF* Array giving multipliers for converting from user to physical
c    coordinates.  Entries correspond to either an x or a y data set.
c    Let v be the x or y corresponding to XYU2PF(I) (see NXYLIM).  Then
c    v_{physical} = XYBASE(IAX) + v_{user} * XYU2PF(IAX).  If an entry
c    here is nonzero, its value has been determined.

c Parameter defs (integers) (in IP):
c NEXT   10^0 of Opt. 1 -- Units, continue, etc., sets LAST.
c INTERP 10^1 of Opt. 1 -- Connecting points of a curve
c KSET   Opt. 7 -- Current user coordinate set
c LCOOX  10^2 of Opt. 1 -- Type of coordinate X.  A value of 3 is
c    set to 2 to make checking for log transformations simpler.
c LCOOY  10^3 of Opt. 1 -- Type of coordinate Y.  As for X above.
c LDEBUG Opt. 25 -- Debugging print flag.
c LNY    Opt. 2  -- Number of y curves being plotted
c LYDIM  Opt. 2  -- First dimension of Y when NY > 1.
c LPEN   Opt. 3  -- Type of pen
c LTANNO Opts. 14,15 -- Type of coordinates amd OPAQUE, Text pointer
c      In the case of Opt. 16, give tick, border index, pointer to text.
c LTYPE  10^5 of Opt. 1 -- LaTeX, or TeX
c LXBORD From Opt. 1 -- For how horizontal borders and axis are labeled.
c         = 0 Linear
c         = 1 10^{??} For log labeling
c         = 2 Polar in radians (Later??)
c         = 3 Polar in degrees (Later??)
c LXLINE 10^4 of Opt. 1 -- Drawing extra lines
c LYBORD As for LXBORD, except for vertical case.
c NBORD  Opt. 16 -- Index for the border.
c Parameter defs (floating point):
c LARROW Opt. 12 -- Length of arrow head in points
c LBAD(2) Opt. 17 -- Flag on bad data action, and the value.
c LXYSIZ             This and next give XSIZE and YSIZE.
c LASTFP             Gives Last location used in FP.
c LVALS (5) Options 14-16, 20-23 -- Place to save various temp. values.
c LWIDRE Opt. 13 -- Line width for rectangles and ellipses.
c LWIDTH (4) Opt. 13 -- Type of pen for borders, major ticks, minor
c     ticks, and lines drawn at x=0 or y=0.
c LASTFP             Size of the array FP().
c LFDAT              Size of the array FPDAT.
c Parameters for integers in IP
c  INTERP,LCOOX,LCOOY,LXLINE,LTYPE,LXBORD,LYBORD, KSET, LTANNO,
c  LPEN,NBORD, LDEBUG
c
c Parameters for floating point
c  LARROW, LWIDTH (4), LWIDRE
c  LVALS (5), LBAD(2)
c
c **************************** Variable Declarations *******************
c
c Formal Args.
      real             XSIZE, YSIZE, X(*), Y(*), OPT(*)
      integer NX
      character COPT*(*)
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C%%    long ictmp;
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c
c         Parameter pointers for integers in IP.
      integer NEXT, INTERP, LCOOX,LCOOY,LXLINE,LXBORD,LYBORD, LTYPE,
     1  KSET, LTANNO, LPEN, NBORD, LYDIM, LNY, LDEBUG,
     2  LASTIP
      parameter (NEXT=1, INTERP=2, LCOOX=3, LCOOY=4, LXLINE=5,
     1  LXBORD=6, LYBORD=7, LTYPE=8, KSET=9, LTANNO=10, LPEN=13,
     2  NBORD=14, LYDIM=15, LNY=16, LDEBUG=18, LASTIP=LDEBUG)
c          Parameter pointers for floats in FP.
      integer LARROW,LWIDTH,LWIDRE,LBAD,LVALS,LXYSIZ,LASTFP,LFDAT
      parameter (LARROW=1, LWIDTH=2, LWIDRE=6, LBAD=7,
     1  LVALS=9, LXYSIZ=LVALS+5, LASTFP=LXYSIZ+2,LFDAT=LBAD)
c          Parameter for various sizes.
      integer LBNDC, LBNDF, LBNDP, LBNDT, MAXSET
      parameter (LBNDC=128, LBNDF=32, LBNDP=4, LBNDT=64, MAXSET=20)
      real             BORLOC(6), FILL(19), FP(LASTFP), OVLAP,
     1  PHYUSE(2,2), SETLIM(2,2), TLENH, TLENV, TICKS(4,6), TOPTS,
     2  VHLEN(2), XYBASE(MAXSET), XYLIM(2,MAXSET), XYU2PF(MAXSET)
      integer IERR1, IERR2, IERR3, IERR4, IOP1, IP(LASTIP), JSET(2),
     1  LENCAP(6), LENTXT(3,18), MANNO, MBORD(8,6), MFILL(4), NTEXT,
     2  NXYLIM(MAXSET)
      logical  KLIP(MAXSET), NOOUT, OPAQUE
      common / SPLOTB / BORLOC, FILL, FP, OVLAP, PHYUSE, SETLIM, TICKS,
     1  TLENH, TLENV, TOPTS, VHLEN, XYBASE, XYLIM, XYU2PF, IERR1,
     2  IERR2, IERR3, IERR4, IOP1, IP, JSET, LENCAP, LENTXT, MANNO,
     3  MBORD, NTEXT, NXYLIM, KLIP, MFILL, NOOUT, OPAQUE
c
      character FMTNUM(17)*(LBNDF), CAPTIO(6)*(LBNDC), POS*68, TEXT*280,
     1    TXTDEF(18)*(LBNDT)
      common / SPLOTC / FMTNUM, CAPTIO, POS, TEXT, TXTDEF
      save /SPLOTB/, /SPLOTC/
c Locals
      integer MAXNY, MAXPT
      parameter (MAXNY=50, MAXPT=101)
C++ CODE for .C. is inactive
C      integer I, I1, IACT, IAOPT, IAX, IERR,
C++ CODE for ~.C. is active
      integer INUNIT, IOTEMP, IOTMP1, IOTMP2, I, I1, IACT, IAOPT,
C++ END
     1   IAX, IERR, INACT(114), ININ(MAXNY), IOP, IOPT, IOSTA,
     2  IRULE(28), IY, J, K, K1, K2, KPT, KSYMB, KX, KY, L, L1, L3,
     3  LAST, LBOUND(3), LCURVE, LFILL(3,4), LINACT(34), LOCF, LOCI,
     4  LPAR, LRULE(35), LSET, LTEXT, LY, M, MODE, NDIMY, NMODE,
     5  NRULE(18), NTYP, NXI, NY
      real             FPDAT(LFDAT), FPIN(MAXNY), TP, TP1, TP2,
     1   TPTSET(3), XOUT(MAXPT), XYMAX(2), XYMIN(2), XYPOS(2),
     2   YOUT(MAXPT)
      character BNDTST*4, C, HLET*6, TXTTST(4)*4, OPTLET*38, VLET*6
      logical BADPT, DATSAV, LKLIP, MOREIY, NOTOUT
c
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
C++ CODE for ~.C. is active
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
c
      character*4 TXTTS1, TXTTS2, TXTTS3, TXTTS4
      parameter (TXTTS1=BSLASH//']]]', TXTTS2=BSLASH//')))',
     1   TXTTS3=BSLASH//'{#}', TXTTS4=BSLASH//'{{}')

c         For debug printing
      character DB*1

C++ CODE for .C. is inactive
C      character BSLASH
C      parameter (BSLASH='\\')
C++END
c
C++ CODE for .C. is inactive
C      save IOSTA, LCURVE, LAST, LSET, XYMAX, XYMIN
C++ CODE for ~.C. is active
      save INUNIT, IOSTA, IOTEMP, LCURVE, LAST, LSET, XYMAX, XYMIN
C++ END

c Option Index:    1  2  3  4  5   6   7   8   9  10  11  12  13  14
c       15  16  17  18  19  20  21  22  23  24  25  26  27 <end flag>
      data IRULE / 1, 2, 4, 5, 8, 10, 12, 16, 17, 18, 18, 19, 20, 21,
     2  23, 25, 27, 28, 28, 29, 30, 31, 32, 33, 34, 35, 35, 36 /

c Index from IRULE:1   2    4  5        8    10    12         16  17
c       18  19  20  21    23    25    27  28  29  30  31  32  33  34 35
      data LRULE / 2,  7,7, 7, 7,15,12, 7,-2, 7,-2, 6,4,9,-2, -2, -2,
     1  -2, -1, -5, -2,6, -3,6, -1,5, -2, -4, -4, -5, -4, -4, -5, 5, 7 /

c                 1   2   3   4   5   6  7   8    9   10 11   12  13 14
c        15  16   17 18
      data NRULE/10, 10, 10, 10, 10, 10, 0, 10, 100, 100, 0, 100, 10, 0,
     1   10, 10, 100, 0 /

c                   1   2   3   4   5   6   7   8   9  10  11  12  13
c       14  15  16  17  18  19  20  21  22  23   24   25   26   27   28
c       29,  30   31   32   33 end
      data LINACT / 1, 10, 15, 19, 20, 21, 22, 23, 24, 25, 26, 27, 31,
     1  40, 50, 60, 70, 74, 75, 80, 85, 90, 95, 100, 105, 106, 107, 111,
     2 111, 112, 113, 113, 114, 115 /

      data INACT / 1, 14,7,3,NEXT, 14,1,1,LTYPE,
     1   6, 14,2,3,LYDIM,
     2  14,1,2,LPEN,
     3   2,  3,  4,   7,  5,  5,  9,  10,
     4  15,1,2,LARROW,
     5   8,15,4,1,LWIDTH, 15,1,2,LWIDRE,
     6  15,2,2,LVALS, 14,2,2,LTANNO, 16,16,
     7  15,3,2,LVALS, 14,2,2,LTANNO, 16,9,
     8  15,1,2,LVALS, 14,3,2,LTANNO, 16,16,
     9  15,2,1,LBAD,
     A  17,
     B  15,4,2,LVALS, 18,   15,4,2,LVALS, 18,
     C  15,5,2,LVALS, 18,   15,4,2,LVALS, 18,
     D  15,4,2,LVALS, 18,   15,5,2,LVALS, 18,
     F  11,
     G  17,
     H  14,1,1,LDEBUG,
     I  17, 20, 21, 22 /

      data LAST / 0 /
c                             11111111112222222222333333333
c                    12345678901234567890123456789012345678
      data OPTLET / 'QMIFACBLTRXYWN 123456({[qmifacbltrxywn' /
c                    1234
      data BNDTST / '[({#' /
C++ CODE for ~.C. is active
      data TXTTST / TXTTS1, TXTTS2, TXTTS3, TXTTS4 /
C++ CODE for .C. is inactive
c      data TXTTST / '\]]]', '\)))', '\{#}', '\{{}' /
C++ END
      data VLET / 'tcbTCB' /
      data HLET / 'lcrLCR' /
c Initial FP.        1    2     3    4   5     6      7
      data FPDAT / 4.E0,100.E0,.7E0,.5E0,60.E0,30.E0,-1.E0 /

      data LBOUND / LBNDP, LBNDF, LBNDT /
c
c To get TOPTS.
         data TPTSET / 72.27E0, 2.845E0, 1.E0 /
C++ CODE for ~.C. is active
   10 format(1X, A1, I3, ' IAOPT=',I2,'  Len=',I3, '  POS=',A4)
   20 format(1X, A1, I3, ' IAOPT=',I2,' FMTNUM=', A)
   30 format(1X, A1, I3, ' IAOPT=',I2,' TEXT=', A)
   40 format(1X, A1, I3, ' IAOPT=',I2,'LENTXT=',3I4, '   POS=', A4)
   50 format(1X, A1, I3, ' IAOPT=',I2,'  Symbols:', 6I10 / (10X,6I10))
   60 format(1X, A1, I3, ' IAOPT=',I2, 1P,2E16.7, I10/ (4E16.7))
   70 format(1X, A1, I3, ' IAOPT=',I2, 3I4, 1P,5E16.7)
   80 format(1X, A1, I3, ' IAOPT=',I2, '  Integers:', 10I8)
   90 format(1X, A1, I3, ' IAOPT=',I2, '  F.P.:', 1P,6E17.8)
  120 format(1X, A1, I3, ' IAOPT=',I2, 1P,4E16.7 / (8X, 4E16.7))
  130 format(1X, A1, I3, ' New data set, Curve=', I3, '  KX=',I3,
     1  '  KY=',I3: '   MODE=', I2, '   IY=', I2, '   NY=', I2)
C++ CODE for .C. is inactive
C%%   const char fmt10[] = "IAOPT=%li  Len=%li  POS=%.4s\n";
C%%   const char fmt20[] = "IAOPT=%li  FMTNUM=%.*s\n";
C%%   const char fmt30[] = "IAOPT=%li  TEXT=%.*s\n";
C%%   const char fmt40[] = "IAOPT=%li  LENTXT=%4li%4li%4li  POS=%.4s\n";
C%%   const char fmt50[] = "IAOPT=%li  Symbols:";
C%%   const char fmt55[] = "%10li";
C%%   const char fmt60[] = "IAOPT=%li%16.7e%16.7e  %li\n";
C%%   const char fmt65[] = "%16.7e";
C%%   const char fmt70[] = "IAOPT=%li%4li%4li%4li";
C%%   const char fmt80[] = "IAOPT=%li  Integers:";
C%%   const char fmt85[] = "%8li";
C%%   const char fmt90[] = "IAOPT=%li  F.P.:";
C%%   const char fmt95[] = "%17.8e";
C%%   const char fmt120[]= "IAOPT=%li";
C%%   const char fmt125[]= "%15.7e";
C%%   const char fmt130[]= "New data set, Curve=%3li  KX=%3li  KY=%3li\
C%%   MODE=%2li   IY=%2li  NY=%2li\n";
C++ END

c
c ************************ Start of Executable Code ********************
c
c Set the defaults as needed.
C++ CODE for ~.C. is active
      INUNIT = 0
C++ END
      OPT(1) = 0.E0
      IOP1 = 0
      NXI = NX
      if (LAST .le. 4) then
c                     Set the defaults
         if (LAST .le. 2) then
            TXTDEF(17)= 'splot.tex'
            ARRLEN = 0
            LCURVE = 100
            JSET(1) = 1
            JSET(2) = -1
            NXYLIM(1) = 0
            NXYLIM(2) = 0
            XYU2PF(1) = 0.E0
            XYU2PF(2) = 0.E0
            XYMIN(1) = 0.E0
            XYMAX(1) = 0.E0
            XYMIN(2) = 0.E0
            XYMAX(2) = 0.E0
            SETLIM(1, 1) = 0.E0
            SETLIM(2, 1) = 0.E0
            SETLIM(1, 2) = 0.E0
            SETLIM(2, 2) = 0.E0
            PHYUSE(1, 1) = -1.E0
            PHYUSE(1, 2) = -1.E0
            LSET = 2
c                    B   L   T   R   X   Y   W   N  o15  1   2   3   4
c      5   6  o16
            POS =  'bc..cr..bc..cl..bc..cr..bl..cl..cl..bc..cr..bc..cl..
     1rc..tc..lc..  ..'
c                    Flag that FMTNUM and TXTDEF are not needed.
            do 140 I = 1, 17
               LENTXT(1, I) = -1
               LENTXT(2, I) = 0
               LENTXT(3, I) = -1
  140       continue
            LENTXT(3, 17) = 9
c                              Default border actions
            do 160 I = 1, 6
               do 150 J = 1, 8
                  MBORD(J, I) = 0
  150          continue
  160       continue
            MBORD(1,1) = 6
            MBORD(1,2) = 6
            MBORD(1,3) = 1
            MBORD(1,4) = 1
            MBORD(8,1) = 0
c     Default tick lengths, no captions
            do 170 I = 1, 6
               TICKS(1, I) = 4.0E0
               TICKS(2, I) = 2.5E0
               TICKS(3, I) = 0.0E0
               TICKS(4, I) = 0.0E0
               LENCAP(I) = 0
  170       continue

c                              Initialize IP and FP
            do 180 I = 1, LASTIP
              IP(I) = 0
  180       continue
            IP(LNY) = 1
            IP(LPEN) = 50
            do 190 I = 1, LFDAT
              FP(I) = FPDAT(I)
  190       continue
c                              Open the scratch file.
C++ CODE for ~.C. is active
            call SPLOTU (IOTEMP, ' ')
            if (IOP1 .le. -100) go to 1500
            DB = 'W'
C++ CODE for .C. is inactive
C%%         iotemp = tmpfile();
C%%         if (iotemp == NULL) goto L_1500;
C++ END
            IOSTA = 0
            MANNO = 0
         else
            IP(NEXT) = 0
         end if
      end if
      FP(LXYSIZ) = XSIZE
      FP(LXYSIZ+1) = YSIZE
c
c ********************** Process the options in COPT *******************
c
      LTEXT = 0
  200 LTEXT = LTEXT + 1
      C = COPT(LTEXT:LTEXT)
      if (C .eq. ' ') go to 200
      K = index(OPTLET, C)
      if (K .eq. 0) then
c                       Error -- Bad option character
         IERR = 10
         go to 1400
      end if
      if (K .gt. 20) K = K - 24
      if (K .le. 1) go to 290
      NTYP = 2
      K = K - 6
c  Enter here when processing text pointed to by options in OPT.
c                Remember start at I1 for error messages.
  210 I1 = LTEXT
      K1 = K
      if (K .le. 0) then
         if (K .le. -2) then
c                Getting file name (or mfpic output)
            NTYP = 3
            K1 = 19 + K
            K = 17
         else if (K .ne. 0) then
c                Got an A, set to save data in first border.
            K = 1
         else
c                Defaults for captions
            K = 10
         end if

      else if (K .ge. 8) then
         NTYP = 1
      end if
c At this point, K identifies what we are working on as follows.
c   = 1-6     B L T R X Y -- For format of Border/Axis Labels
c   = 7       W -- For formatting words
c   = 8       N -- For formatting numbers
c   = 9       Option 15, format for a number to be output.
c   =10-15    1-6 -- For Border/Axis Captions (or for "C")
c   =16       Options 14 & 16, Text to be output.
c   =17       F -- Output file name or mfpic output
      LENTXT(1, K) = -1
      LENTXT(3, K) = -1
  220 LTEXT = LTEXT + 1
c                Checking
      C = COPT(LTEXT:LTEXT)
C%%   k2 = 0;
C%%   for (j=0; j<4; j++) {
C%%      if (bndtst[j] == c) {
C%%        k2 = j + 1;
C%%        break;}}
      K2 = index(BNDTST(NTYP:4), C)
      if (K2 .eq. 0) then
         if (C .eq. ' ') go to 220
c                        Error -- Bad start of COPT option
         IERR = 11
         go to 1400
      end if
      if (K2 .ne. 1) then
         if (C .eq. '#') then
            if (K .lt. 10) go to 260
c                        Error -- Bad start of COPT option
            IERR = 11
            go to 1400
         end if
         NTYP = NTYP + K2 - 1
      end if
      if ((C .eq. '{') .and. (K1 .ge. 10)) then
         NTYP = 4
         if (COPT(LTEXT+1:LTEXT+1) .eq. BSLASH) LENTXT(3, K) = -2
      end if
      LPAR = 1
      J = LTEXT
  240 LTEXT = LTEXT + 1
      if (LTEXT - I1 .gt. 100) then
c                 Error -- Runaway in COPT, unbalanced (), [], or {}?
         J = I1
         IERR = 12
         go to 1410
      end if
c                    Get the end tag (LPAR counts "paren" levels)
      C = COPT(LTEXT:LTEXT)
      L = index(TXTTST(NTYP), C)
c          Skip uninteresting characters, and those escaped with '\'.
      if (L .eq. 1) LTEXT = LTEXT + 1
      if (L .le. 1) go to 240
      if (NTYP .ge. 3) then
         if (L .ne. 3) then
            LPAR = LPAR - L + 3
            if (LPAR .ne. 0) go to 240
            if (NTYP .eq. 3) then
c                            Error -- Missing #
               IERR = 13
               go to 1400
            end if
         else
            if (NTYP .eq. 3) then
               LENTXT(2, K) = LTEXT - J
               NTYP = NTYP + 1
            end if
            go to 240
         end if
c                      Save the length
         L = LTEXT - J - 1
         if ((K .ge. 10) .and. (K1 .ne. 0)) then
c               Text for file name or border/axis caption or option
            if (L .le. 0) then
c                         Error -- File name or caption is empty
               IERR = 14
               go to 1400
            end if
            if (L .gt. LBNDC) then
c                       Error -- (J:LTEXT) may only contain LBNDC chars.
               IERR1 = LBNDC
               IERR = 15
               go to 1410
            end if
            if (K .eq. 17) then
               if (K1 .eq. 17) then
c                             The output file name.
                  LENTXT(3,17) = L
C%%               memcpy(splotc.txtdef[16], copt+j,(size_t)l);
C%%               splotc.txtdef[16][l]='\0';
                  TXTDEF(17) = COPT(J+1:LTEXT-1)
               else if (K1 .eq. 16) then
c          The input file name, get the unit number, open if needed.
C++ CODE for ~.C. is active
                  inquire(FILE=COPT(J+1:LTEXT-1), NUMBER=INUNIT,
     1                OPENED=NOTOUT)
                  if (.not. NOTOUT) then
                     call SPLOTU(INUNIT,COPT(J+1:LTEXT-1))
                     if (IOP1 .le. -100) go to 1500
                  end if
C++ CODE for .C. is inactive
C                 C = COPT(LTEXT:LTEXT)
C%%               copt[ltext-1] = '\0';
C%%               inunit = fopen(copt+j, "r");
C%%               if (inunit == NULL) goto L_1500;
C                 COPT(LTEXT:LTEXT) = C
C++ END
               else
c                             Data for raw mfpic output.
C%%               ictmp = 29;
C%%               fwrite(&ictmp, sizeof(ictmp), (size_t)1, iotemp);
                  write (IOTEMP) 29
C%%               fwrite(&l, sizeof(l), (size_t)1, iotemp);
c%%               fwrite(copt+j, (size_t)1, (size_t)l, iotemp);
                  write (IOTEMP) L, COPT(J+1: LTEXT-1)
               end if
               go to 200
            end if
            if (K .ge. 16) then
c                            Option 14, or 16, text to output.
              if (NOTOUT) then
                 NOTOUT = .false.
C%%              fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
                 write (IOTEMP) IAOPT
              end if
C%%           fwrite(&splotb.lentxt[k-1][0],
C%%               sizeof(splotb.lentxt[k-1][0]),(size_t)1,iotemp);
C%%           fwrite(&l, sizeof(l), (size_t)1, iotemp);
C%%           fwrite(&splotc.pos[60], (size_t)1, (size_t)4, iotemp);
              write (IOTEMP) LENTXT(1, K), L, POS(61:64)
C++ CODE for .C. is inactive
C%%           if (splotb.ip[LDEBUG-1] > 1) printf (fmt10, iaopt,
C%%             splotb.lentxt[k-1][0], &splotc.pos[60]);
C++ CODE for ~.C. is active
              if (IP(LDEBUG).gt.1)
     1          print 10, DB, IOTEMP, IAOPT,LENTXT(1,K),POS(61:64)
C++ END
              if (LENTXT(1,K) .gt. 0) then
C++ CODE for ~.C. is active
                 write (IOTEMP) FMTNUM(K)(1:LENTXT(1,K))
                 if (IP(LDEBUG).gt.1)
     1              print 20, DB,IOTEMP,IAOPT,FMTNUM(K)(1:LENTXT(1,K))
C++ CODE for .C. is inactive
c%%               fwrite(splotc.fmtnum[k-1], (size_t)1,
c%%                  (size_t)splotb.lentxt[k-1][0], iotemp);
C%%               if (splotb.ip[LDEBUG-1] > 1) printf (fmt20, iaopt,
C%%                  (int)splotb.lentxt[k-1][0], splotc.fmtnum[k-1]);
C++ END
              end  if
              if (L .ne. 0) then
c%%              fwrite(copt+j, (size_t)1, (size_t)l, iotemp);
                 write (IOTEMP) COPT(J+1:LTEXT-1)
C++ CODE for .C. is inactive
C%%              if (splotb.ip[LDEBUG-1] > 1) printf (fmt30, iaopt,
C%%                 (int)l, copt+j);
C++ CODE for ~.C. is active
                 if (IP(LDEBUG).gt.1)
     1             print 30, DB, IOTEMP, IAOPT,COPT(J+1:LTEXT-1)
C++ END
              end if
              go to 400
            else
               LENCAP(K-9) = L
C%%            memcpy(splotc.captio[k-10], copt+j,(size_t)(ltext-j-1));
               CAPTIO(K-9) = COPT(J+1:LTEXT-1)
            end if
         end if
      else
         L = LTEXT - J - 1
         if (L .gt. LBOUND(NTYP)) then
c                Error -- (J:LTEXT) may only contain LBOUND(NTYP) chars.
               IERR1 = LBOUND(NTYP)
               IERR = 15
               go to 1410
         end if
         if (NTYP .eq. 1) then
c                          Check and save position info.
            if ((L .ne. 2) .and. (L .ne. 4)) then
c                             Error -- [...] must contain 2 or 4 letters
               IERR = 16
               go to 1410
            end if
            C = COPT(J+1:J+1)
            I = index(VLET, C)
            if (I .eq. 0) then
c                        Error -- First position must be one of "tcbTCB"
               IERR = 17
               go to 1410
            end if
            if (I .gt. 3) C = VLET(I-3:I-3)
            POS(4*K-3:4*K-3) = C
            C = COPT(J+2:J+2)
            I = index(HLET, C)
            if (I .eq. 0) then
c                       Error -- Second position must be one of "lcrLCR"
               IERR = 18
               go to 1410
            end if
            if (I .gt. 3) C = HLET(I-3:I-3)
            POS(4*K-2:4*K-2) = C
            if (L .eq. 2) then
               POS(4*K-1:4*K) = '  '
            else
               C = COPT(J+4:J+4)
               I = index(HLET, C)
               if (I .gt. 3) C = HLET(I-3:I-3)
               if((I .eq. 0) .or. ((COPT(J+2:J+2) .ne. 'S') .and.
     1             (COPT(J+2:J+2) .ne. 's'))) then
c                              Error -- In third/forth position of [...]
                  IERR = 19
                  go to 1410
               end if
               POS(4*K-1:4*K-1) = 's'
               POS(4*K:4*K) = C
            end if
            go to 250
         end if
         LENTXT(NTYP-1, K) = L
         if (L .ne. 0) then
            if (NTYP .eq. 2) then
C%%            memcpy(splotc.fmtnum[k-1], copt+j,(size_t)(ltext-j-1));
               FMTNUM(K) = COPT(J+1:LTEXT-1)
            else
C%%            memcpy(splotc.txtdef[k-1], copt+j,(size_t)(ltext-j-1));
               TXTDEF(K) = COPT(J+1:LTEXT-1)
            end if
         end if
      end if
  250 NTYP = NTYP + 1
      if (NTYP .le. 4) go to 220
  260 if (K .eq. 9) then
c         Just processed formats for option 15
         if (NOTOUT) then
            NOTOUT = .false.
C%%         fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
            write (IOTEMP) IAOPT
         end if
C%%      fwrite(&splotb.lentxt[8][0], sizeof(splotb.lentxt[8][0]),
C%%          (size_t)2, iotemp);
C%%      fwrite(&splotc.pos[60], (size_t)1, (size_t)4, iotemp);
         write (IOTEMP) LENTXT(1, 9), LENTXT(2, 9), POS(33:36)
C++ CODE for ~.C. is active
         if (IP(LDEBUG).gt.1) print 40, DB, IOTEMP, IAOPT,
     1       (LENTXT(K, 9), K = 1, 3), POS(33:36)
C++ CODE for .C. is inactive
C%%      if (splotb.ip[LDEBUG-1] > 1) printf (fmt40,iaopt,
C%%        splotb.lentxt[8][0], splotb.lentxt[8][1],splotb.lentxt[8][2],
C%%        &splotc.pos[32]);
C++ END
         if (LENTXT(1,9).gt.0) then
C%%        fwrite(splotc.fmtnum[8], (size_t)1,
C%%           (size_t)splotb.lentxt[8][0], iotemp);
           write (IOTEMP) FMTNUM(9)(1:LENTXT(1,9))
C++ CODE for .C. is inactive
C%%        if (splotb.ip[LDEBUG-1] > 1) printf (fmt20, iaopt,
C%%           (int)splotb.lentxt[8][0], splotc.fmtnum[8]);
C++ CODE for ~.C. is active
           if (IP(LDEBUG).gt.1)
     1       print 20, DB, IOTEMP, IAOPT,FMTNUM(9)(1:LENTXT(1,9))
C++ END
         end if
         if (LENTXT(3,9).gt.0) then
C%%        fwrite(splotc.txtdef[8], (size_t)1,
C%%           (size_t)splotb.lentxt[8][2], iotemp);
           write (IOTEMP) TXTDEF(9)(1:LENTXT(3,9))
C++ CODE for .C. is inactive
C%%        if (splotb.ip[LDEBUG-1] > 1) printf (fmt30, iaopt,
C%%           (int)(splotb.lentxt[8][2]), splotc.txtdef[8]);
C++ CODE for ~.C. is active
           if (IP(LDEBUG).gt.1)
     1       print 30, DB, IOTEMP, IAOPT,TXTDEF(9)(1:LENTXT(3,9))
C++ END
         end if
         go to 400
      end if
      if ((K1 .eq. 0) .or. (K1 .eq. -1)) then
c                  Copy stuff for first border to all of them
         I = 10 + 9*K1
         L1 = LENTXT(1,I)
         L3 = LENTXT(3,I)
         do 280 J = I+1, I+5
            do 270 L = 1, 3
               LENTXT(L, J) = LENTXT(L, I)
  270       continue
C%%  if (l1>0) memcpy(splotc.fmtnum[j-1],splotc.fmtnum[i-1],(size_t)l1);
            if (L1 .gt. 0) FMTNUM(J)(1:L1)=FMTNUM(I)(1:L1)
C%%  if (l3>0) memcpy(splotc.txtdef[j-1],splotc.txtdef[i-1],(size_t)l3);
            if (L3 .gt. 0) TXTDEF(J)(1:L3)=TXTDEF(I)(1:L3)
  280    continue
      end if
      go to 200
c         Reduce count by 1 if not a "Q", then save next text pointer.
  290 if (K .ne. 1) LTEXT = LTEXT - 1

c
c ******************** Process the options in OPT **********************
c
      IOP = 1
c              IOP(1) reserved for exit flag.
  300 IOP = IOP + 1
      IOP1 = IOP
      IOPT = nint(OPT(IOP))
C++ CODE for ~.C. is active
      if (IP(LDEBUG).gt.1) print '('' Option: OPT('', I3,'') ='', I3)',
     1    IOP, IOPT
C++ CODE for .C. is inactive
C%%   if (splotb.ip[LDEBUG-1] > 1) printf (" Option: OPT(%li) = %li\n",
C%%      iop, iopt );
C++ END
      if (IOPT .eq. 0) go to 700
c
      IAOPT = abs(IOPT)
      if (IAOPT .gt. 28) go to 520
c                             Unpack associated data
      LOCI = 0
      LOCF = 0
      do 340 J = IRULE(IAOPT), IRULE(IAOPT+1) - 1
         L = LRULE(J)
         if (L .lt. 0) then
c                                Pick up -L floating point numbers
  320       IOP = IOP + 1
            LOCF = LOCF + 1
            FPIN(LOCF) = OPT(IOP)
            L = L + 1
            if (L .ne. 0) go to 320
         else
c                                Pick up and unpack an integer.
            IOP = IOP + 1
            TP = abs(OPT(IOP))
            M = nint(TP)
            if (abs(TP - real(M)) .gt. .2E0) then
c                               Error -- Number specified not an integer
               IERR = 21
               go to 1430
            end if
c                   TP used in later test to see if too big.
            TP = M + 1
  330       LOCI = LOCI + 1
            if (NRULE(L) .ne. 0) then
               ININ(LOCI) = mod(M, NRULE(L))
               M = M / NRULE(L)
               L = L + 1
               go to 330
            else
c                         Last one takes the whole integer
               ININ(LOCI) = M
               if (TP - nint(abs(OPT(IOP))) .ne. 1.E0) then
c                           Error -- Floating number too big for integer
                  IERR = 22
                  go to 1430
               end if
            end if
         end if
  340 continue
c                            IOPT < 0, means don't process the option.
      if (IOPT .lt. 0) go to 300
c                            Option unpacked, now process.
      IACT = LINACT(IAOPT) - 1
      NOTOUT = .true.
      LOCI = 1
      LOCF = 1
  400 IACT = IACT + 1
      if (IACT .ge. LINACT(IAOPT+1)) go to 300
c              1    2    3    4    5    6    7    8    9   10   11
c         12   13   14   15   16   17   18
      go to (410, 420, 430, 430, 450, 460, 470, 480, 490, 500, 510,
     1   520, 520, 540, 550, 560, 520, 400), INACT(IACT)
c =1, Units / Final flag, and linex as x,y = 0, for option 1
  410 IP(NEXT) = ININ(1)
      if (IP(NEXT) .gt. 5) then
c                          Error -- Digit 10^0 of option 1 is too big
         IERR = 23
         go to 1430
      end if
      if (ININ(6) .gt. 1) then
c                         Error -- Type flag must be 0 or 1.
         IERR = 24
         go to 1430
      end if
      ININ(8) = ININ(6)
      if ((ININ(3) .ge. 4) .or. (ININ(4) .ge. 4)) then
c                        Polar coordinates or an error.
c          An error now since polar code is not yet written.
          IERR = 25
          go to 1430
      end if
      do 415 J = 1, 2
c                   Set flags for how the borders/axes are labeled.
         K = ININ(2+J)
         ININ(5+J) = mod(K, 2)
         if (K .eq. 3) ININ(2+J) = 2
  415 continue
      go to 400
c =2  Save border characteristics.  Option 4
  420 K = ININ(1)
  422 J = mod(K, 10)
      if ((J .ne. 0) .and. (J .le. 6)) then
         do 425 I = 1, 6
         MBORD(I, J) = ININ(I+1)
  425    continue
         if (ININ(8) .ne. 0) MBORD(7, J) = ININ(8)
      else
c         Error -- Only digits 1 to 6 can be used for borders.
         IERR = 26
         go to 1430
      end if
      K = K / 10
      if (K .ne. 0) go to 422
      go to 300
c =3,4  Tick info. for various borders. Options 5 and 6
  430 I1 = 2 * IAOPT - 9
      I = ININ(1)
  436 J = mod(I, 10)
      if ((J .ne. 0) .and. (J .le. 6)) then
      TICKS(I1, J) = FPIN(1)
      TICKS(I1+1, J) = FPIN(2)
      else
c         Error -- Only digits 1 to 6 can be used for borders.
         IERR = 26
         go to 1430
      end if
      I = I / 10
      if (I .ne. 0) go to 436
      go to 300

c =5  Set ?MAX / ?MIN for current set. Options 8 and 9
  450 if (FPIN(1) .lt. FPIN(2)) then
         K = IAOPT - 7
         if (XYMIN(K) .lt. XYMAX(K)) then
c             Error -- min/max on x or y specified twice.
            IERR = 27
            go to 1430
         end if
         XYMIN(K) = FPIN(1)
         XYMAX(K) = FPIN(2)
      end if
      go to 300

c =6  Check NY. Option 2
  460 if (IP(LNY) .ne. ININ(2)) then
         if (LAST .eq. 5) then
c                         Error -- NY changed in middle of curve
            IERR = 28
            go to 1430
         end if
      end if
      go to 400
c =7  Change in data set, Option 7
  470 I = ININ(1)
      IAX = 2 - mod(I, 2)
      if (I .le. 4) then
         if (NXYLIM(IAX) .eq. 0) then
c         Error -- Attempting to change data set without providing data.
            IERR = 29
            go to 1430
         end if
c                        Take care of border being replaced
         call SPLOTA(I)
         if (IOP1 .le. -100) go to 1500
c                        Save data for data set being replaced
         LSET = LSET + 1
         NXYLIM(LSET) = NXYLIM(IAX)
         XYLIM(1, LSET) = XYLIM(1, IAX)
         XYLIM(2, LSET) = XYLIM(2, IAX)
         XYBASE(LSET) = XYBASE(IAX)
         XYU2PF(LSET) = XYU2PF(IAX)
         KLIP(LSET) = KLIP(IAX)
c                        Set up for new data set.
         NXYLIM(IAX) = 0
         XYU2PF(IAX) = 0.E0
         XYMIN(IAX) = 0.E0
         XYMAX(IAX) = 0.E0
         MBORD(8, I) = IAX
         do 475 I = 1, 7
         MBORD(I, IAX) = ININ(I+2)
  475    continue
      end if
      PHYUSE(1, IAX) = FPIN(1)
      PHYUSE(2, IAX) = FPIN(2)
      go to 300

c =8  Set defaults for widths on out of range values.
  480 FPIN(LWIDTH+1) = mod(FPIN(LWIDTH+1)/10.E0, 100.E0) / 10.E0
      FPIN(LWIDTH+2) = mod(FPIN(LWIDTH+2)/10.E0, 100.E0) / 10.E0
      do 485 I = 1, 5
         if (FPIN(I) .le. 0.E0) then
            FPIN(I) = FPDAT(LWIDTH-1+I)
         end if
  485 continue
      go to 400
c =9  Special symbol plotting (may be more than 1 N10)
  490 IOP = IOP + 1
      I = IOP
  495 if (OPT(IOP) .lt. 0) then
         IOP = IOP + 1
         go to 495
      end if
      J = IOP
      if (J - I .ge. IP(LNY)) then
c                              Error -- More symbols than allowed
         IERR = 30
         go to 1430
      end if
C%%   fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
      write (IOTEMP) IAOPT
C%%   ictmp = j - i + 1;
C%%   fwrite(&ictmp, sizeof(ictmp), (size_t)1, iotemp);
C%%   for (k = i; k <= j; k++){
C%%   ictmp = abs(nint(opt[k-1]));
C%%   fwrite(&ictmp, sizeof(ictmp), (size_t)1, iotemp);}
      write (IOTEMP) J - I + 1, (nint(abs(OPT(K))), K = I, J)
C++ CODE for .C. is inactive
C%%   if (splotb.ip[LDEBUG-1] > 1) {
C%%     printf (fmt50, iaopt);
C%%     for (k = i; k <= j; k++){
C%%        printf(fmt55, (long)abs(nint(opt[k-1])));}
c%%     printf ("\n");}
C++ CODE for ~.C. is active
      if (IP(LDEBUG).gt.1)
     1  print 50, DB, IOTEMP, IAOPT, (nint(abs(OPT(K))), K = I,J)
C++ END
      go to 300

c =10  Single symbol (may require extra args.)
  500 J = 1
      K = abs(nint(OPT(IOP+1)))
      if (mod(K, 10) .eq. 1) then
         J = mod(K/10, 10) + 3
         if (J .ge. 5)  then
            if (J .gt. 5) then
c                          Error -- Bad value for symbol plotting
               IERR = 31
               go to 1430
            end if
            J = 3
         end if
      end if
      if (NOTOUT) then
         NOTOUT = .false.
C%%      fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
         write (IOTEMP) IAOPT
      end if
C++ CODE for ~.C. is active
      write (IOTEMP) J, FPIN(1), FPIN(2), nint(OPT(IOP+1)),
     1   (OPT(I),I=IOP+2,IOP+J)
      if (IP(LDEBUG).gt.1) print 60, DB, IOTEMP, IAOPT, FPIN(1),
     1    FPIN(2), nint(OPT(IOP+1)), (OPT(I),I=IOP+2,IOP+J)
C++ CODE for .C. is inactive
C%%   fwrite(&j, sizeof(j), (size_t)1, iotemp);
C%%   fwrite(fpin, sizeof(fpin[0]), (size_t)2, iotemp);
C%%   ictmp = nint(opt[iop]);
C%%   fwrite(&ictmp, sizeof(ictmp), (size_t)1, iotemp);
C%%   fwrite(&opt[iop+1], sizeof(opt[0]), (size_t)(j-1), iotemp);
C%%   if (splotb.ip[LDEBUG-1] > 1){
C%%     printf (fmt60, iaopt, fpin[0], fpin[1], nint(opt[iop]));
C%%     for (k=iop+2; k<=iop+j; k++) printf(fmt65,opt[k-1]);
c%%     printf ("\n");}
C++ END
      IOP = IOP + J
      go to 300

c =11  For filling (may have up to 3 for each curve type) (scratch)
  510 J = 0
      if (ININ(1) .gt. 2) J = ININ(1) - 1
      if (J .gt. 3) then
c                   Error -- Digit 10^0 for option 19, must be < 5
      IERR = 32
      go to 1430
      end if
      if (NOTOUT) then
         NOTOUT = .false.
C%%      fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
         write (IOTEMP) IAOPT
      end if
C++ CODE for ~.C. is active
      write (IOTEMP) J,ININ(1),ININ(2),ININ(3), (OPT(I),I=IOP+1,IOP+J)
      if (IP(LDEBUG).gt.1) print 70,
     1  IOTEMP, IAOPT, ININ(1),ININ(2),ININ(3), (OPT(I),I=IOP+1,IOP+J)
C++ CODE for .C. is inactive
C%%   fwrite(&j, sizeof(j), (size_t)1, iotemp);
C%%   fwrite(&inin[0], sizeof(inin[0]), (size_t)3, iotemp);
C%%   fwrite(&opt[iop], sizeof(opt[iop]), (size_t)j, iotemp);
C%%   if (splotb.ip[LDEBUG-1] > 1) {
C%%      printf(fmt70, iaopt, inin[0], inin[1], inin[2]);
C%%      for (i = iop; i < iop+j; i++) printf(fmt65, opt[i]);
c%%      printf ("\n");}
C++ END
      IOP = IOP + J
      go to 300
c =? Invalid option (or maybe a bug in this code?)
  520 IERR = 20
      go to 1430

c =14  There follows in INACT, n, w, p, where
c     n = number of integers
c     w = defines where they go when processing options: 1 storage,
c         2 scratch, 3 both  (NOT USED???)
c     p = gives index in IP() where integers start.
  540 if (INACT(IACT+2) .ge. 2) then
         if (NOTOUT) then
            NOTOUT = .false.
C%%         fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
            write (IOTEMP) IAOPT
         end if
C++ CODE for ~.C. is active
         write(IOTEMP) (ININ(I), I=LOCI,LOCI+INACT(IACT+1)-1)
         if (IP(LDEBUG).gt.1) print 80, DB, IOTEMP, IAOPT,(ININ(I),
     1      I=LOCI,LOCI+INACT(IACT+1)-1)
C++ CODE for .C. is inactive
C%%      fwrite(&inin[loci-1], sizeof(inin[0]), (size_t)inact[iact],
C%%         iotemp);
C%%      if (splotb.ip[LDEBUG-1] > 1) {
C%%         printf(fmt80, iaopt);
C%%         for (i = loci-1; i < loci+inact[iact]-2; i++)
C%%            printf(fmt85, inin[i]);
c%%         printf ("\n");}
C++ END
      end if
      if (INACT(IACT+2) .ne. 2) then
         do 545 I = INACT(IACT+3), INACT(IACT+3) + INACT(IACT+1) - 1
         IP(I) = ININ(LOCI)
         LOCI = LOCI + 1
  545    continue
      else
         LOCI = LOCI + INACT(IACT+1)
      end if
      IACT = IACT + 3
      go to 400

c =15  As for 14, except for floating point data in FPIN().
  550 if (INACT(IACT+2) .ge. 2) then
         if (NOTOUT) then
            NOTOUT = .false.
C%%         fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
            write (IOTEMP) IAOPT
         end if
C++ CODE for ~.C. is active
         write(IOTEMP) (FPIN(I), I=LOCF,LOCF+INACT(IACT+1)-1)
         if (IP(LDEBUG).gt.1) print 90, DB, IOTEMP, IAOPT,(FPIN(I),
     1      I=LOCF,LOCF+INACT(IACT+1)-1)
C++ CODE for .C. is inactive
C%%    fwrite(&fpin[locf-1],sizeof(fpin[0]),(size_t)inact[iact],iotemp);
C%%    if (splotb.ip[LDEBUG-1] > 1) {
C%%       printf(fmt90, iaopt);
C%%       for (i = locf-1; i < locf+inact[iact]-2; i++)
C%%          printf(fmt95, fpin[i]);
c%%       printf ("\n");}
C++ END
      end if
      if (INACT(IACT+2) .ne. 2) then
      do 555 I = INACT(IACT+3), INACT(IACT+3) + INACT(IACT+1) - 1
      FP(I) = FPIN(LOCF)
      LOCF = LOCF + 1
  555 continue
      else
      LOCF = LOCF + INACT(IACT+1)
      end if
      IACT = IACT + 3
      go to 400

c =16  Processing for text pointer.  In all cases here, the text is
c      acted upon immediately when read from the scratch file.
c      There follows in INACT, k
c        k =  9 For option 15, Sets format for next numeric output.
c        k = 16 Text output for options 14 and 16.
  560 IACT = IACT + 1
      K = INACT(IACT)
      I = ININ(LOCI-1)
      if (I .ne. 0) then
         LTEXT = I - 1
      else if (IOPT .ne. 14) then
c                  If not option 14, just flag that there is no text.
C%%      ictmp = -1;
C%%      fwrite(&ictmp, sizeof(ictmp), (size_t)1, iotemp);
C%%      ictmp = 0;
C%%      fwrite(&ictmp, sizeof(ictmp), (size_t)1, iotemp);
C%%      fwrite("  ..", (size_t)1, (size_t)4, iotemp);
         write (IOTEMP) -1, 0, '  ..'
C%%      if (splotb.ip[LDEBUG-1] > 1)
C%%          printf (fmt10, iaopt, (long)0, "  ..");
         if (IP(LDEBUG).gt.1) print 10, DB, IOTEMP, IAOPT, 0, '  ..'
         go to 400
      end if
      POS(61:64) = '  ..'
      go to 210
c
c ***********  Done processing options, take care of X, Y, data ********
c
  700 IOP1 = -1
      I1 = 1
c        I1 is count of data to get when getting it from (X, Y).
      NY = IP(LNY)
      NDIMY = IP(LYDIM)
      do 705 K = 1, 2
         SETLIM(1,K) = XYMIN(K)
         SETLIM(2,K) = XYMAX(K)
c                               Take logs of limmits if necessary.
         if (XYMIN(K) .lt. XYMAX(K)) then
            if (IP(LCOOX) .eq. 2) then
               SETLIM(1, K) = log10(XYMIN(K))
               SETLIM(2, K) = log10(XYMAX(K))
            end if
         end if
  705 continue
      if (NXI .eq. 0) go to 780
C%%   ictmp = 30;
C%%   fwrite(&ictmp, sizeof(ictmp), (size_t)1, iotemp);
      write (IOTEMP) 30
C%%   fwrite(&lcurve, sizeof(lcurve), (size_t)1, iotemp);
C%%   fwrite(splotb.jset, sizeof(splotb.jset[0]), (size_t)2, iotemp);
C%%   fwrite(&ny, sizeof(ny), (size_t)1, iotemp);
      write (IOTEMP) LCURVE, JSET(1), JSET(2), NY
C++ CODE for .C. is inactive
C%%   if (splotb.ip[LDEBUG-1]>1) printf(fmt130, lcurve, splotb.jset[0],
C%%      splotb.jset[1], (long)0, (long)0, ny);
C++ CODE for ~.C. is active
      if (IP(LDEBUG).gt.1)
     1  print 130, DB, IOTEMP, LCURVE,JSET(1),JSET(2),0,0,NY
C++ END
c              Get min/max value and write data
  710 continue
c%%   if (inunit != NULL) {
      if (INUNIT .gt. 0) then
c                             Get data off a file.
C%%      if (!fread(fpin, sizeof(fpin[0]), (size_t)(ny+1), inunit))
C%%        goto L_770;}
         read(INUNIT, *, END=770) FPIN(1), (FPIN(I+1), I = 1, NY)
C%%   else{
      else
         FPIN(1) = X(I1)
         do 720 I = 0, NY-1
            FPIN(I+2) = Y(NDIMY * I + I1)
  720    continue
C%%   };
      end if
c Check for bad data value now to avoid confusion when taking logs.
      if (FP(LBAD) .ge. 0.E0) then
c                Check for and flag bad output points.
         ININ(1) = 0
         do 730 I = 1, NY
            ININ(I+1) = 0
            if (FPIN(I+1) .eq. FP(LBAD+1)) ININ(I+1) = 1
  730    continue
         if (ININ(1).ne.0) then
C%%         ictmp = 30;
C%%         fwrite(&ictmp, sizeof(ictmp), (size_t)1, iotemp);
C%%         fwrite(inin, sizeof(inin[0]), (size_t)(ny+1), iotemp);
            write (IOTEMP) 32, (real(ININ(I)), I=1,NY+1)
         end if
C++ CODE for .C. is inactive
C%%   if (splotb.ip[LDEBUG-1] > 1) {
C%%     printf(fmt120, (long)31);
C%%     for (i = 0; i <= ny; i++) printf(fmt125, (float ) inin[i]);
C%%     printf ("\n");}
C++ CODE for ~.C. is active
         if (IP(LDEBUG).gt.1)
     1     print 120, DB, IOTEMP,  32, (real(ININ(I)), I=1,NY+1)
C++ END
      end if
c                           Check if want logs
      if (IP(LCOOX) .eq. 2) FPIN(1) = log10(FPIN(1))
      if (IP(LCOOY) .eq. 2) then
         do 740 I = 1, NY
            FPIN(I+1) = log10(FPIN(I+1))
  740    continue
      end if
c         Establish initial minimum/maximum values
      TP1 = FPIN(2)
      TP2 = TP1
c
      do 750 I = 2, NY
         TP1 = min(TP1, FPIN(I+1))
         TP2 = max(TP2, FPIN(I+1))
  750 continue
      if (NXYLIM(1) .eq. 0) then
         XYLIM(1,1) = FPIN(1)
         XYLIM(2,1) = FPIN(1)
         XYLIM(1,2) = TP1
         XYLIM(2,2) = TP2
         NXYLIM(1) = JSET(1)
         NXYLIM(2) = JSET(2)
      else
         XYLIM(1,1) = min(XYLIM(1,1), FPIN(1))
         XYLIM(2,1) = max(XYLIM(2,1), FPIN(1))
         XYLIM(1,2) = min(XYLIM(1,2), TP1)
         XYLIM(2,2) = max(XYLIM(2,2), TP2)
      end if
C%%   ictmp = 31;
C%%   fwrite(&ictmp, sizeof(ictmp), (size_t)1, iotemp);
C%%   fwrite(fpin, sizeof(fpin[0]), (size_t)(ny+1), iotemp);
      write (IOTEMP) 31, (FPIN(I), I= 1, NY+1)
C++ CODE for .C. is inactive
C%%   if (splotb.ip[LDEBUG-1] > 1) {
C%%     printf(fmt120, (long)31);
C%%     for (i = 0; i <= ny;i++) printf(fmt125, fpin[i]);
C%%     printf ("\n");}
C++ CODE for ~.C. is active
      if (IP(LDEBUG).gt.1)
     1  print 120, DB, IOTEMP, 31, (FPIN(I), I=1,NY+1)
C++ END
      I1 = I1 + 1
      if (I1 .le. NXI) go to 710
c                     Data now written, if any -- Write end mark
 770  continue
c%%   if (inunit != NULL) {
      if (INUNIT .gt. 0) then
        NXI = I1
C%%     fclose(inunit);
        close(INUNIT)
C%%   }
      end if

      if (IP(NEXT) .lt. 5) LCURVE = LCURVE + 1
  780 LAST = IP(NEXT)
      FPIN(1) = LAST
C%%   ictmp = 33;
C%%   fwrite(&ictmp, sizeof(ictmp), (size_t)1, iotemp);
C%%   fwrite(fpin, sizeof(fpin[0]), (size_t)(ny+1), iotemp);
      write (IOTEMP) 33, (FPIN(I), I = 1, NY+1)
C++ CODE for ~.C. is active
      if (IP(LDEBUG).gt.1)
     1  print 120, DB, IOTEMP, 33, (FPIN(I), I=1,NY+1)
      if (IP(LDEBUG).gt.1) print
     1 '(''**************** End of Inputs *************  LAST ='',I1)',
     2 LAST
C++ CODE for .C. is inactive
C%%   if (splotb.ip[LDEBUG-1] > 1) {
C%%     printf(fmt120, (long)33);
C%%     for (i = 0; i <= ny; i++) printf(fmt125, fpin[i]);
C%%     printf ("\n");}
C%%   if (splotb.ip[LDEBUG-1] > 1) printf(
C%% "**************** End of Inputs *************  LAST = %li\n",last);
C++ END
      if (LAST .gt. 2) then
         if (IOP1 .le. -100) go to 1510
         if (LAST .eq. 3) IOSTA = 1
         return
      end if
      TOPTS = TPTSET(LAST+1)
c
      if (IOSTA .gt. 0) then
C++ CODE for ~.C. is active
         IOTMP1 = IOTEMP
         DB = 'B'
         call SPLOTU (IOTMP2, ' ')
         if (IOP1 .le. -100) go to 1500
         rewind(IOTEMP)
C++ CODE for .C. is inactive
C%%      iotmp1 = iotemp;
C%%      iotmp2 = tmpfile();
C%%      if (iotmp2 == NULL) goto L_1500;
C%%      rewind(iotemp);
C++ END
         if (IP(LDEBUG) .gt. 1) print '(''Rewind IOTEMP'')'
      end if
c
c *********************** Start Processing Saved Data ******************
c
c  Take care of axes, get max's and min's, draw lines a x=0, y=0, etc.
  800 do 820 I = 1, 6
         call SPLOTA(I)
  820 continue
C++ CODE for ~.C. is active
      DB = 'R'
C++ END
      if (IOP1 .le. -100) go to 1500
      NOOUT = .false.
  830 KX = 1
      KY = 1
c                  Set "17" (file name) as already output.
      LENTXT(1, 17) = -1
      LENTXT(3, 17) = -1
c
      IY = 1
      KSYMB = -1
      MODE = 0
  840 NMODE = 10
c                     Points are connected, take care of them.
  860 MOREIY = .false.
      IP(INTERP) = 0

      if (IOSTA .le. 0) then
        if (IP(LDEBUG) .gt. 1) print '(''Rewind IOTEMP'')'
C%%     rewind(iotemp);
        rewind(IOTEMP)
      end if
  890 continue
C%%   fread(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
      read (IOTEMP) IAOPT
      if (IOSTA .ne. 0) then
         if (IOSTA .gt. 0) then
C%%         fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotmp2);
            write (IOTMP2) IAOPT
         else if (IAOPT .ge. 30) then
            if (IAOPT .eq. 33) go to 1300
            if (IY .le. 1) go to 890
         end if
      end if
      IACT = LINACT(IAOPT) - 1
      LOCI = 1
      LOCF = 1
  900 IACT = IACT + 1
      if (IACT .ge. LINACT(IAOPT+1)) go to 890
c              9  10  11  12  13  14  15  16  17  18  19   20   21   22
      go to (920,930,940,910,910,950,960,970,990,980,910,1000,1200,1300)
     1 , INACT(IACT) - 8
  910 go to 900
c
c Special Symbol plotting -- 9
  920 continue
C++ CODE for ~.C. is active
      read (IOTEMP) L, (ININ(I), I = 1, L)
      if (IOSTA .gt. 0) write (IOTMP2) L, (ININ(I), I = 1, L)
      if (IP(LDEBUG).gt.1)
     1  print 50, DB, IOTEMP, IAOPT, (ININ(I), I = 1, L)
C++ CODE for .C. is inactive
C%%   fread(&l, sizeof(l), (size_t)1, iotemp);
C%%   fread(inin, sizeof(inin[0]), (size_t)l, iotemp);
C%%   if (iosta > 0) {
C%%     fwrite(&l, sizeof(l), (size_t)1, iotmp2);
C%%     fwrite(inin, sizeof(inin[0]), (size_t)l, iotmp2);}
C%%   if (splotb.ip[LDEBUG-1] > 1) {
C%%     printf (fmt50, iaopt);
C%%     for (i = 0; i < l; i++) printf(fmt55, inin[i]);
C%%     printf ("\n");}
C++ END
      KSYMB = abs(ININ(min(L, IY)))
      go to 900
c
c Single symbol to plot -- 10
  930 continue
C++ CODE for ~.C. is active
      read (IOTEMP) L, FPIN(1), FPIN(2), J, (FPIN(I-1), I = 4, L+2)
      if (IOSTA .gt. 0) write (IOTMP2) L, FPIN(1), FPIN(2), J,
     1  (FPIN(I-1), I = 4, L+2)
      if (IP(LDEBUG).gt.1) print 60, DB, IOTEMP, IAOPT, FPIN(1),
     1     FPIN(2), J, (FPIN(I-1), I = 4, L+2)
C++ CODE for .C. is inactive
C%%   fread(&l, sizeof(l), (size_t)1, iotemp);
C%%   fread(fpin, sizeof(fpin[0]), (size_t)2, iotemp);
C%%   fread(&j, sizeof(j), (size_t)1, iotemp);
C%%   fread(&fpin[2], sizeof(fpin[0]), (size_t)(l-1), iotemp);
C%%   if (iosta > 0) {
C%%      fwrite(&l, sizeof(l), (size_t)1, iotmp2);
C%%      fwrite(fpin, sizeof(fpin[0]), (size_t)2, iotmp2);
C%%      fwrite(&j, sizeof(j), (size_t)1, iotmp2);
C%%      fwrite(&fpin[2], sizeof(fpin[0]), (size_t)(l-1), iotmp2);}
C%%   if (splotb.ip[LDEBUG-1] > 1){
C%%     printf (fmt60, iaopt, fpin[0], fpin[1], j);
C%%     for (i=2; i<=l+2; i++) printf(fmt65,fpin[i]);
c%%     printf ("\n");}
C++ END
      if (MODE .lt. 7) then
         NMODE = min(NMODE, 7)
      else
         KX = 1
         KY = 2
         call SPLOTR(FPIN, J, KX, KY)
      end if
      go to 900
c
c For Filling -- 11
  940 continue
C++ CODE for ~.C. is active
      read (IOTEMP) L,ININ(1),ININ(2),ININ(3),(FPIN(I),I=1, L)
      if (IOSTA .gt. 0) write (IOTMP2)  L, ININ(1), ININ(2),
     1   ININ(3), (FPIN(I), I=1, L)
      if (IP(LDEBUG).gt.1)
     1  print 70, DB, IOTEMP, IAOPT, ININ(1),ININ(2),ININ(3),
     2  (FPIN(I),I=1, L)
C++ CODE for .C. is inactive
C%%   fread(&l, sizeof(l), (size_t)1, iotemp);
C%%   fread(inin, sizeof(inin[0]), (size_t)3, iotemp);
C%%   fread(fpin, sizeof(fpin[0]), (size_t)l, iotemp);
C%%   if (iosta > 0) {
C%%      fwrite(&l, sizeof(l), (size_t)1, iotmp2);
C%%      fwrite(inin, sizeof(inin[0]), (size_t)3, iotmp2);
C%%      fwrite(fpin, sizeof(fpin[0]), (size_t)l, iotmp2);}
C%%   if (splotb.ip[LDEBUG-1] > 1) {
C%%      printf(fmt70, iaopt, inin[0], inin[1], inin[2]);
C%%      for (i = iop; i < iop+l; i++) printf(fmt65, opt[i]);
c%%      printf ("\n");}
C++ END
      J = ININ(2)
      if (ININ(1) .eq. 0) then
         MFILL(J) = 0
      else
         MFILL(J) = min(MFILL(J)+1, 3)
         LFILL(MFILL(J), ININ(2)) = ININ(1)
         if (ININ(3) .gt. 0) MFILL(J) = -MFILL(J)
         if (L .gt. 0) then
            K = 1
            if (L .eq. 3) K = 7
            FILL(K) = FPIN(1)
            FILL(K+1) = FPIN(2)
            if (L .eq. 3) FILL(K+2) = FPIN(3)
         end if
      end if
      go to 900
c
c Integers to restore.
  950 if (INACT(IACT+2) .ne. 1) then
         L = INACT(IACT+1)
         J = INACT(IACT+3)
C++ CODE for ~.C. is active
         read(IOTEMP) (IP(J+I-1), I=1, L)
         if (IOSTA .gt. 0) write (IOTMP2)  (IP(J+I-1), I=1, L)
         if (IP(LDEBUG).gt.1)
     1     print 80, DB, IOTEMP, IAOPT, (IP(J+I-1), I=1, L)
C++ CODE for .C. is inactive
C%%      fread(&splotb.ip[j], sizeof(splotb.ip[0]), (size_t)l, iotemp);
C%%      if (iosta > 0) fwrite(splotb.ip, sizeof(splotb.ip[0]),
C%%         (size_t)l, iotmp2);
C%%      if (splotb.ip[LDEBUG-1] > 1) {
C%%         printf(fmt80, iaopt);
C%%         for (i = j-1; i < j+l-2; i++) printf(fmt85, splotb.ip[i]);
c%%         printf ("\n");}
C++ END
      end if
      IACT = IACT + 3
      if (IAOPT .eq. 1) then
         if (MODE .le. 5) then
            MODE = IP(INTERP)
C%%         if (splotb.ip[LDEBUG-1]>1) printf("MODE set to %li", mode);
            if (IP(LDEBUG).gt.1) print '('' MODE set to'', I2)', MODE
         end if
      end if
      go to 900
c
c Floating point to restore
  960 if (INACT(IACT+2) .ne. 1) then
         L = INACT(IACT+1)
         J = INACT(IACT+3)
C++ CODE for ~.C. is active
         read(IOTEMP) (FP(J+I-1), I=1, L)
         if (IOSTA .gt. 0) write (IOTMP2)  (FP(J+I-1), I=1, L)
         if (IP(LDEBUG).gt.1)
     1     print 90, DB, IOTEMP, IAOPT, (FP(J+I-1), I=1, L)
C++ CODE for .C. is inactive
C%%      fread(&splotb.fp[j-1], sizeof(splotb.fp[0]),(size_t)l,iotemp);
C%%      if (iosta > 0) fwrite(&splotb.fp[j-1], sizeof(splotb.fp[0]),
C%%         (size_t)l, iotmp2);
C%%      if (splotb.ip[LDEBUG-1] > 1) {
C%%       printf(fmt90, iaopt);
C%%       for (i = j-1; i < j+l-2; i++) printf(fmt95, splotb.fp[i]);
c%%       printf ("\n");}
C++ END
      end if
      IACT = IACT + 3
      go to 900
c
c Text to restore
  970 IACT = IACT + 1
      K = INACT(IACT)
      if (K .ne. 9) then
C++ CODE for ~.C. is active
         read (IOTEMP) LENTXT(1,16), NTEXT, POS(61:64)
         if (IOSTA .gt. 0) write (IOTMP2)  LENTXT(1,16), NTEXT,
     1      POS(61:64)
         if (IP(LDEBUG).gt.1)
     1     print 10, DB, IOTEMP, IAOPT, LENTXT(1,16), POS(61:64)
C++ CODE for .C. is inactive
C%%      fread (&splotb.lentxt[15][0], sizeof(splotb.lentxt[0][0]),
C%%         (size_t)1, iotemp);
C%%      fread(&splotb.ntext, sizeof(splotb.ntext), (size_t)1, iotemp);
C%%      fread(&splotc.pos[60], (size_t)1, (size_t)4, iotemp);
C%%      if (iosta > 0) {
C%%         fwrite(&splotb.lentxt[15][0],
C%%             sizeof(splotb.lentxt[0][0]),(size_t)1,iotmp2);
C%%         fwrite(&splotb.ntext,sizeof(splotb.ntext),(size_t)1,iotmp2);
C%%         fwrite(&splotc.pos[60], (size_t)1, (size_t)4, iotmp2);}
C%%      if (splotb.ip[LDEBUG-1] > 1) printf (fmt10, iaopt,
C%%         splotb.lentxt[15][0], &splotc.pos[60]);
C++ END
         if (LENTXT(1,16) .gt. 0) then
C++ CODE for ~.C. is active
            read (IOTEMP) FMTNUM(16)(1:LENTXT(1,16))
            if (IOSTA .gt. 0) write (IOTMP2) FMTNUM(16)(1:LENTXT(1,16))
            if (IP(LDEBUG).gt.1) print 20,
     1         IOTMP2, IAOPT,FMTNUM(16)(1:LENTXT(1,16))
C++ CODE for .C. is inactive
C%%         fread(splotc.fmtnum[15], (size_t)1,
C%%            (size_t)splotb.lentxt[15][0], iotemp);
c%%         if (iosta > 0) fwrite(splotc.fmtnum[15], (size_t)1,
C%%            (size_t)splotb.lentxt[15][0], iotemp);
C%%         if (splotb.ip[LDEBUG-1] > 0) printf (fmt20, iaopt,
C%%            (int)splotb.lentxt[15][0], splotc.fmtnum[15]);
C++ END
         end if
         if (NTEXT .ne. 0) then
C++ CODE for ~.C. is active
            read (IOTEMP) TEXT(1:NTEXT)
            if (IOSTA .gt. 0) write (IOTMP2) TEXT(1:NTEXT)
            if (IP(LDEBUG).gt.1)
     1        print 30, DB, IOTEMP, IAOPT, TEXT(1:NTEXT)
C++ CODE for .C. is inactive
C%%         fread(splotc.text, (size_t)1, (size_t)splotb.ntext, iotemp);
C%%         if (iosta > 0) fwrite(splotc.text, (size_t)1,
C%%            (size_t)splotb.ntext, iotmp2);
C%%         if (splotb.ip[LDEBUG-1] > 1) printf (fmt30, iaopt,
C%%            (int)(splotb.ntext), splotc.text);
C++ END
         end if
      else if (IAOPT .ne. 14) then
C++ CODE for ~.C. is active
         read(IOTEMP) LENTXT(1, 9), LENTXT(2, 9), POS(33:36)
         if (IOSTA .gt. 0) write (IOTMP2)  LENTXT(1, 9),
     1       LENTXT(2, 9), POS(33:36)
         if (IP(LDEBUG).gt.1) print 10, DB, IOTEMP, IAOPT, 0, '  ..'
C++ CODE for .C. is inactive
C%%      fread(&splotb.lentxt[8][0], sizeof(splotb.lentxt[8][0]),
C%%          (size_t)2, iotemp);
C%%      fread(&splotc.pos[32], (size_t)1, (size_t)4, iotemp);
C%%      if (iosta > 0) {
C%%         fwrite(&splotb.lentxt[8][0], sizeof(splotb.lentxt[8][0]),
C%%            (size_t)2, iotmp2);
C%%         fwrite(&splotc.pos[32], (size_t)1, (size_t)4, iotmp2);}
C%%      if (splotb.ip[LDEBUG-1] > 1) printf (fmt10, iaopt,
c%%         (long)0, "  ..");
C++ END
      else
C++ CODE for ~.C. is active
         read (IOTEMP) (LENTXT(K, 9), K = 1, 3), POS(33:36)
         if (IOSTA .gt. 0) write (IOTMP2)
     1       (LENTXT(K, 9), K = 1, 3), POS(33:36)
         if (IP(LDEBUG).gt.1) print 40, DB, IOTEMP, IAOPT,
     1       (LENTXT(K, 9), K = 1, 3), POS(33:36)
C++ CODE for .C. is inactive
C%%      fread(&splotb.lentxt[8][0], sizeof(splotb.lentxt[8][0]),
C%%          (size_t)3, iotemp);
C%%      fread(&splotc.pos[32], (size_t)1, (size_t)4, iotemp);
C%%      if (iosta > 0) {
C%%      fwrite(&splotb.lentxt[8][0], sizeof(splotb.lentxt[8][0]),
C%%          (size_t)3, iotmp2);
C%%      fwrite(&splotc.pos[32], (size_t)1, (size_t)4, iotmp2);}
C%%      if (splotb.ip[LDEBUG-1] > 1) printf (fmt40, iaopt,
C%%        splotb.lentxt[8][0], splotb.lentxt[8][1],splotb.lentxt[8][2],
C%%        &splotc.pos[32]);
C++ END
         if (LENTXT(1,9).gt.0) then
C++ CODE for ~.C. is active
           read (IOTEMP) FMTNUM(9)(1:LENTXT(1,9))
           if (IOSTA .gt. 0) write (IOTMP2)
     1         FMTNUM(9)(1:LENTXT(1,9))
           if (IP(LDEBUG).gt.1)
     1       print 20, DB, IOTEMP, IAOPT,FMTNUM(9)(1:LENTXT(1,9))
C++ CODE for .C. is inactive
C%%        fread(splotc.fmtnum[8], (size_t)1,
C%%           (size_t)splotb.lentxt[15][0], iotemp);
c%%        if (iosta > 0) fwrite(splotc.fmtnum[8], (size_t)1,
C%%           (size_t)splotb.lentxt[15][0], iotemp);
C%%        if (splotb.ip[LDEBUG-1] > 0)  printf (fmt20, iaopt,
C%%           (int)splotb.lentxt[15][0], splotc.fmtnum[8]);
C++ END
         end if
         if (LENTXT(3,9).gt.0) then
C++ CODE for ~.C. is active
           read (IOTEMP) TXTDEF(9)(1:LENTXT(3,9))
           if (IOSTA .gt. 0) write (IOTMP2) TXTDEF(9)(1:LENTXT(3,9))
           if (IP(LDEBUG).gt.1)
     1       print 30, DB, IOTEMP, IAOPT,TXTDEF(9)(1:LENTXT(3,9))
C++ CODE for .C. is inactive
C%%        fread(splotc.txtdef[8], (size_t)1,
C%%           (size_t)splotb.lentxt[8][2], iotemp);
C%%        if (iosta > 0) fwrite(splotc.txtdef[8], (size_t)1,
C%%           (size_t)splotb.lentxt[8][2], iotmp2);
C%%        if (splotb.ip[LDEBUG-1] > 1) printf (fmt30, iaopt,
C%%           (int)(splotb.lentxt[8][2]), splotc.txtdef[8]);
C++ END
         end if
      end if
      if (MODE .lt. 9) then
         NMODE = min(NMODE, 9)
      else if (MODE .eq. 9) then
c           Output the text
         if (IAOPT .ne. 16) then
            if ((NTEXT .eq. 0) .and. (IAOPT .eq. 14)) go to 900
            I = IP(LTANNO)
            OPAQUE = .false.
            if (I .gt. 3) then
c                      Want placed in an opaque box.
               OPAQUE = .true.
               I = I - 4
            end if
c   Set up for differences between output of numbers and text
            K1 = 4 * K
            L1 = 7
            if (K .eq. 9)  then
               L1 = 8
               FP(LVALS+3) = FP(LVALS)
               FP(LVALS) = FP(LVALS+1)
               FP(LVALS+1) = FP(LVALS+2)
            end if
            K2 = 4 * L1
C%%         if (memcmp(splotc.pos+k1-4, "  ..", (size_t)4) == 0)
C%%             memcpy(splotc.pos+k1-4, splotc.pos+k2-4,(size_t)4);
            if (POS(K1-3:K1) .eq. '  ..') POS(K1-3:K1) = POS(K2-3:K2)
            if (I .ge. 2) then
c                         Set to avoid the formatting.
               I = I - 2
               K = 17
C%%            memcpy(splotc.pos+64, splotc.pos+k1-4,(size_t)4);
               POS(65:68) = POS(K1-3:K1)
            else
c                         Set up the formatting
               if (LENTXT(1, K) .lt. 0) then
                  LENTXT(1, K) = LENTXT(1, L1)
C++ CODE for .C. is inactive
C%%               if (splotb.lentxt[l1 - 1][0] > 0)
C%%                  memcpy(splotc.fmtnum[k-1],splotc.fmtnum[l1-1],
C%%                  splotb.lentxt[l1 - 1][0]);
C++ CODE for ~.C. is active
                  if (LENTXT(1,L1).gt. 0) FMTNUM(K)(1:LENTXT(1,K)) =
     1                FMTNUM(L1)(1:LENTXT(1, L1))
C++ END
               end if
               LENTXT(3, K) = LENTXT(3, L1)
               if (LENTXT(3, L1) .gt. 0) then
                   LENTXT(2, K) = LENTXT(2, L1)
C%%                memcpy(splotc.txtdef[k-1],splotc.txtdef[l1-1],
C%%                   splotb.lentxt[l1-1][2]);
                   TXTDEF(K)(1:) = TXTDEF(L1)(1:LENTXT(3, L1))
               end if
            end if
            do 975 J = 0, 1
               if (IP(LCOOX+J) .eq. 2) then
                  FP(LVALS+J) = log10(FP(LVALS+J))
               end if
  975       continue
            if (I .eq. 0) then
c                      Convert to physical coordinates
              MANNO = -1
              XYPOS(1) = XYBASE(KX) + XYU2PF(KX) * FP(LVALS)
              XYPOS(2) = XYBASE(KY) + XYU2PF(KY) * FP(LVALS+1)
            else
              MANNO = 1
              XYPOS(1) = TOPTS * FP(LVALS)
              XYPOS(2) = TOPTS * FP(LVALS+1)
            end if
            if (L1 .eq. 7) then
               call SPLOTT(K, XYPOS)
            else
               call SPLOTN(FP(LVALS+3), K, XYPOS)
            end if
            MANNO = 0
         else
c      Text for Axis/Border Annotation -- Must be processed in SPLOTA.
            call SPLOTA(IP(LTANNO+1)+10)
            if (IOP1 .le. -100) go to 1500
         end if
      end if
      go to 900
c
c Rectangles ellipses and lines.
  980 if (MODE .lt. 8) then
         NMODE = min(NMODE, 8)
         go to 900
      end if
      if (IAOPT .le. 21) then
c            Convert to physical coordinates (only first two for ellipse
         XOUT(1) = XYBASE(KX) + XYU2PF(KX) * FP(LVALS)
         YOUT(1) = XYBASE(KY) + XYU2PF(KY) * FP(LVALS+1)

         if (IAOPT .ne. 21) then
            XOUT(2) = XYBASE(KX) + XYU2PF(KX) * FP(LVALS+2)
            YOUT(2) = XYBASE(KY) + XYU2PF(KY) * FP(LVALS+3)
         end if
      else
c            Conver physical coordinates to points.
         IAOPT = IAOPT - 3
         XOUT(1) = TOPTS * FP(LVALS)
         YOUT(1) = TOPTS * FP(LVALS+1)
         XOUT(2) = TOPTS * FP(LVALS+2)
         YOUT(2) = TOPTS * FP(LVALS+3)
         if (IAOPT .eq. 21) then
            FP(LVALS+2) = XOUT(3)
            FP(LVALS+3) = XOUT(4)
         end if
      end if
      if ( IAOPT .eq. 19) then
c                  Draw a line
         KURPEN = IP(LPEN)
         call SPLOT2(XOUT(1), YOUT(1), XOUT(2), YOUT(2))
      else if (IAOPT .eq. 20) then
c                  Draw a rectangle
         KURPEN = FP(LWIDRE)
         if (MFILL(2) .ne. 0) call SPLOT7(MFILL(2), LFILL(1,2), FILL)
         call SPLOT5(XOUT(1), YOUT(1), XOUT(2), YOUT(2))
      else
c                  Draw an ellipse
         KURPEN = FP(LWIDRE)
         if (MFILL(3) .ne. 0) call SPLOT7(MFILL(3), LFILL(1,3), FILL)
         call SPLOT6(XOUT(1), YOUT(1), FP(LVALS+2), FP(LVALS+3),
     1      FP(LVALS+4))
      end if
      go to 900
c
c Raw MFPIC output
  990 continue
C%%   fread(&splotb.ntext, sizeof(splotb.ntext), (size_t)1, iotemp);
C%%   fread(splotc.text, (size_t)1, (size_t)(splotb.ntext), iotemp);
      read(IOTEMP) NTEXT, TEXT(1:NTEXT)
C%%   if (iosta > 0) {
C%%     fwrite(&splotb.ntext, sizeof(splotb.ntext), (size_t)1, iotemp);
C%%     fwrite(splotc.text, (size_t)1, (size_t)(splotb.ntext), iotemp);}
      if (IOSTA .gt. 0) write (IOTMP2) NTEXT, TEXT(1:NTEXT)
C%%   if (splotb.ip[LDEBUG-1] > 1) printf (fmt30, iaopt,
C%%      (int)(splotb.ntext), splotc.text);
      if (IP(LDEBUG).gt.1) print 30, DB, IOTEMP, IAOPT, TEXT(1:NTEXT)
      if (MODE .lt. 9) then
         NMODE = min(NMODE, 9)
      else if (MODE .eq. 9) then
c                            Output the text
C%%      fprintf(iofil, "%.*s\n", (int)splotb.ntext, splotc.text);
         write (IOFIL, '(A)') TEXT(1:NTEXT)
      end if
      go to 900
c
c New data start
 1000 continue
C%%   fread(&i, sizeof(i), (size_t)1, iotemp);
C%%   fread(&j, sizeof(j), (size_t)1, iotemp);
C%%   fread(&k, sizeof(k), (size_t)1, iotemp);
C%%   fread(&ny, sizeof(ny), (size_t)1, iotemp);
      read (IOTEMP) I, J, K, NY
C++ CODE for .C. is inactive
C%%   if (splotb.ip[LDEBUG-1] > 1) printf(fmt130,i, j, k, mode, iy, ny);
C++ CODE for ~.C. is active
      if (IP(LDEBUG) .gt. 1)
     1  print 130, DB, IOTEMP, I, J, K, MODE, IY, NY
c++ END
      if (IY .le. NY) then
         LY = IY
c                       Keep  track of last that needs to be done again.
         DATSAV = .false.
         if (IY .lt. NY) then
            MOREIY = .true.
            if (IOSTA .gt. 0) then
C%%   fwrite(&i, sizeof(i), (size_t)1, iotmp2);
C%%   fwrite(&j, sizeof(j), (size_t)1, iotmp2);
C%%   fwrite(&k, sizeof(k), (size_t)1, iotmp2);
C%%   fwrite(&ny, sizeof(ny), (size_t)1, iotmp2);
               write (IOTMP2) I, J, K, NY
               DATSAV = .true.
            end if
         end if
         if (MODE .le. 4) then
            if (KSYMB .ge. 0) then
               NMODE = 6
               KSYMB = -1
            end if
            go to 1020
         else if (MODE .le. 6) then
            if (KSYMB .ge. 0) then
c                        Adjust LY in some cases
               if (mod(KSYMB, 10) .eq. 1) then
                  LY = 3
                  if (mod(KSYMB/10, 10) .eq. 0) LY = 2
                  LY = 1 + LY * (IY - 1)
               end if
               if ((MODE .ne. 6) .or. (IP(INTERP) .lt. 5)) go to 1025
            else if (MODE .ne. 6) then
c                 Points have been provided, but they are not plotted.
                call SPLOTE(1, OPT, COPT)
            end if
         end if
      end if
c               Consume till get to the end of a data set.
 1010 continue

C%%   fread(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
C%%   fread(fpin, sizeof(fpin[0]), (size_t)(ny+1), iotemp);
      read (IOTEMP) IAOPT, (FPIN(J), J = 1, NY+1)
C++ CODE for .C. is inactive
C%%   if (splotb.ip[LDEBUG-1] > 1) {
C%%     printf(fmt120, iaopt);
C%%     for (i = 0; i <= ny; i++) printf(fmt125, fpin[i]);
C%%     printf ("\n");}
C++ CODE for ~.C. is active
      if (IP(LDEBUG) .gt. 1)
     1  print 120, DB, IOTEMP, IAOPT,  (FPIN(J), J = 1,NY+1)
C++ END
      if (IAOPT .ne. 33) go to 1010
      go to 1170
c                  Get pen set
 1020 KURPEN = IP(LPEN)
      call SPLOT1
c                               Process the data
 1025 do 1030 I = 1, LSET
         if (NXYLIM(I) .eq. J) go to 1040
 1030 continue
C%%   puts( "Error -- Internal bug, couldn't find X index" );
C%%   close_units();
C%%   puts( "[Stop]" );
C%%   exit(0);
      stop 'Error -- Internal bug, couldn''t find X index'
 1040 KX = I
      do 1050 I = 1, LSET
         if (NXYLIM(I) .eq. K) go to 1070
 1050 continue
C%%   puts( "Error -- Internal bug, couldn't find Y index" );
C%%   close_units();
C%%   puts( "[Stop]" );
C%%   exit(0);
      stop 'Error -- Internal bug, couldn''t find Y index'
 1070 if (LAST .eq. 5) go to 1120
      KPT = 0
      KY = I
c Set up for type of curve, clipping, etc.

 1080 BADPT = .false.
      LKLIP = .false.
      if (MODE .lt. 5) then
         call SPLOTL(-1-MODE, XOUT, YOUT)
         if (MODE .le. 2) then
c                         Initialize SPLOTF
            K = -1
            if (KLIP(KX) .or. KLIP(KY)) K = -3
            call SPLOTF(K, XOUT, XYLIM(1, KX), XYLIM(1, KY))
            if (IOP1 .le. -100) go to 1500
         end if
      end if
c
      if (IAOPT .eq. 31) go to 1180
 1120 continue
C++ CODE for ~.C. is active
      read (IOTEMP) IAOPT, (FPIN(J), J = 1, NY+1)
      if (DATSAV) write (IOTMP2) IAOPT, (FPIN(J), J = 1, NY+1)
      if (IP(LDEBUG) .gt. 1)
     1  print 120, DB, IOTEMP, IAOPT,  (FPIN(J), J = 1, NY+1)
C++ CODE for .C. is inactive
C%%   fread(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
C%%   fread(fpin, sizeof(fpin[0]), (size_t)(ny+1), iotemp);
C%%   if (datsav) {
C%%     fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotmp2);
C%%     fwrite(fpin, sizeof(fpin[0]), (size_t)(ny+1), iotmp2);}
C%%   if (splotb.ip[LDEBUG-1] > 1) {
C%%     printf(fmt120, iaopt);
C%%     for (i = 0; i <= ny; i++) printf(fmt125, fpin[i]);
C%%     printf ("\n");}
C++ END
c          Check cases: 31 => good data, 32 => bad data.
      if (IAOPT .eq. 31) go to 1180
      if (IAOPT .eq. 32) then
         if (FPIN(LY+1) .eq. 0.E0) go to 1120
c                  Have a bad Y, skip the data points.
C++ CODE for ~.C. is active
         read (IOTEMP) IAOPT, (FPIN(J), J = 1, NY+1)
         if (DATSAV) write (IOTMP2) IAOPT, (FPIN(J), J = 1, NY+1)
         if (IP(LDEBUG) .gt. 1)
     1     print 120, DB, IOTEMP, IAOPT,  (FPIN(J), J=1,NY+1)
C++ CODE for .C. is inactive
C%%      fread(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
C%%      fread(fpin, sizeof(fpin[0]), (size_t)(ny+1), iotemp);
C%%      if (datsav) {
C%%        fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotmp2);
C%%        fwrite(fpin, sizeof(fpin[0]), (size_t)(ny+1), iotmp2);}
C%%      if (splotb.ip[LDEBUG-1] > 1) {
C%%        printf(fmt120, iaopt);
C%%        for (i = 0; i <= ny; i++) printf(fmt125, fpin[i]);
C%%        printf ("\n");}
C++ END
         if ((FP(LBAD).eq.0.E0) .or. (KSYMB.gt.0)) go to 1120
c  Point is not simply ignored.  End this curve and start a new one.
         BADPT = .true.
      else
c                  Curve is being continued
         LAST = nint(FPIN(1))
         if (LAST .eq. 5) go to 890
      end if
c           Finish current curve segment.
      if (MODE .le. 2) then
         if (KPT .gt. 0) call SPLOTF(KPT,XOUT,XOUT,YOUT)
         call SPLOTF(0, XOUT, XOUT, YOUT)
         if (IOP1 .le. -100) go to 1500
      else
         if (KPT .gt. 0) call SPLOTL(KPT, XOUT, YOUT)
         call SPLOTL(0, XOUT, YOUT)
      end if
      if (BADPT) then
c                       Consume till we get a good point.
 1160    continue
C++ CODE for ~.C. is active
         read (IOTEMP) IAOPT, (FPIN(J), J = 1, NY+1)
         if (DATSAV) write (IOTMP2) IAOPT, (FPIN(J), J = 1, NY+1)
         if (IP(LDEBUG) .gt. 1)
     1     print 120, DB, IOTEMP, IAOPT,  (FPIN(J), J=1,NY+1)
C++ CODE for .C. is inactive
C%%      fread(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
C%%      fread(fpin, sizeof(fpin[0]), (size_t)(ny+1), iotemp);
C%%      if (datsav) {
C%%        fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotmp2);
C%%        fwrite(fpin, sizeof(fpin[0]), (size_t)(ny+1), iotmp2);}
C%%      if (splotb.ip[LDEBUG-1] > 1) {
C%%        printf(fmt120, iaopt);
C%%        for (i = 0; i <= ny; i++) printf(fmt125, fpin[i]);
C%%        printf ("\n");}
C++ END
         if (IAOPT .eq. 32) then
            if (FPIN(LY+1) .ne. 0.E0) then
C++ CODE for ~.C. is active
               read (IOTEMP) IAOPT, (FPIN(J), J = 1, NY+1)
               if (DATSAV) write (IOTMP2) IAOPT, (FPIN(J), J = 1, NY+1)
               if (IP(LDEBUG).gt.1)
     1           print 120, DB, IOTEMP, IAOPT,(FPIN(J),J=1,NY+1)
C++ CODE for .C. is inactive
C%%            fread(&iaopt, sizeof(iaopt), (size_t)1, iotemp);
C%%            fread(fpin, sizeof(fpin[0]), (size_t)(ny+1), iotemp);
C%%            if (datsav) {
C%%              fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotmp2);
C%%              fwrite(fpin, sizeof(fpin[0]), (size_t)(ny+1), iotmp2);}
C%%            if (splotb.ip[LDEBUG-1] > 1) {
C%%              printf(fmt120, iaopt);
C%%              for (i = 0; i <= ny; i++) printf(fmt125, fpin[i]);
C%%              printf ("\n");}
C++ END
               go to 1160
            end if
         end if
      end if
c                  If IAOPT .eq. 31, we have a point for a new curve.
      if (IAOPT .eq. 31) go to 1080
c                  Done with the current curve.
 1170 KSYMB = -1
      LAST = nint(FPIN(1))
      if (LAST .le. 3) then
         if (MOREIY) then
            IY = IY + 1
            if (DATSAV) then
               IOSTA = -1
c%%            iotemp = iotmp2;
               IOTEMP = IOTMP2
            else
              if (IP(LDEBUG) .gt. 1) print '(''Rewind IOTEMP'')'
C%%            rewind(iotemp);
              rewind(IOTEMP)
          end if
            go to 860
         end if
c                          Done with one mfpic segment.
         if (IOSTA .gt. 0) then
c                       Switch to second scratch file.
            IOSTA = -1
c                       Following write serves as an endfile.
C%%         fwrite(&iaopt, sizeof(iaopt), (size_t)1, iotmp2);
            write(IOTMP2) IAOPT
c%%         iotemp = iotmp2;
            IOTEMP = IOTMP2
         end if
         MODE = NMODE
         if (MODE .ne. 10) go to 840
         go to 1300
      end if
      if (MODE .le. 5) then
         MODE = 0
         IP(INTERP) = 0
      end if
      go to 890
c
c              Convert to physical coordinates and send point out.
 1180 KPT = KPT + 1
      XOUT(KPT) = XYBASE(KX) + XYU2PF(KX) * FPIN(1)
      YOUT(KPT) = XYBASE(KY) + XYU2PF(KY) * FPIN(LY+1)
      if (MODE .ge. 2) then
c                   Check for clipping
         if (KLIP(KX) .or. KLIP(KY)) then
            if ((XOUT(KPT) .lt. XYLIM(1, KX)) .or.
     1          (XOUT(KPT) .gt. XYLIM(2, KX)) .or.
     2          (YOUT(KPT) .lt. XYLIM(1, KY)) .or.
     3          (XOUT(KPT) .gt. XYLIM(2, KY))) then
               if (KSYMB .ge. 0) go to 1120
               if (LKLIP) then
                  XOUT(1) = XOUT(2)
                  YOUT(1) = YOUT(2)
                  go to 1120
               end if
               LKLIP = .true.
               if (KPT .eq. 1) go to 1120
               K1 = KPT - 1
               K2 = KPT
            else if (LKLIP) then
               LKLIP = .false.
               K1 = 2
               K2 = 1
            else
               go to 1190
            end if
c                     Make up fake point
            FPIN(1) = XOUT(K2)
            FPIN(2) = YOUT(K2)
            if ((FPIN(1) .lt. XYLIM(1, KX)) .or.
     1          (FPIN(1) .gt. XYLIM(2, KX))) then
               XOUT(K2) = max(XYLIM(1,KX), min(FPIN(1),XYLIM(2, KX)))
               YOUT(K2) = YOUT(K1) + (XOUT(K2) - XOUT(K1)) *
     1            (FPIN(2) - YOUT(K1)) / (FPIN(1) - XOUT(K1))
            end if
            FPIN(3) = XOUT(KPT)
            FPIN(4) = YOUT(KPT)
            if ((FPIN(4) .lt. XYLIM(1, KY)) .or.
     1          (FPIN(4) .gt. XYLIM(2, KY))) then
               YOUT(K2) = max(XYLIM(1,KY), min(FPIN(4),XYLIM(2, KY)))
               XOUT(K2) = XOUT(K1) + (YOUT(K2) - YOUT(K1)) *
     1            (FPIN(3) - XOUT(K1)) / (FPIN(4) - YOUT(K1))
            end if
            if (LKLIP) then
               call SPLOTL(KPT, XOUT, YOUT)
               call SPLOTL(0, XOUT, YOUT)
               KPT = 1
               XOUT(1) = FPIN(1)
               YOUT(1) = FPIN(2)
c                     Start a new curve.
               go to 1080
            end if
         end if
 1190    if (KSYMB .ge. 0) then
            KPT = 0
            FP(LY) = FP(1)
            call SPLOTR(FPIN(LY), KSYMB, KX, KY)
            go to 1120
         end if
      end if
      if (KPT .lt. MAXPT) go to 1120
      KPT = 0
      if (MODE .le. 2) then
         call SPLOTF(MAXPT, XOUT, XOUT, YOUT)
         if (IOP1 .le. -100) go to 1500
         go to 1120
      else
         call SPLOTL(MAXPT-1, XOUT, YOUT)
         XOUT(1) = XOUT(MAXPT)
         YOUT(1) = YOUT(MAXPT)
         go to 1120
      end if
 1200 continue
C%%   puts( "Bad action index in processing scratch file." );
C%%   close_units();
C%%   puts( "[Stop]" );
C%%   exit(0);
      stop 'Bad action index in processing scratch file.'
c
c Got to end of current processing
 1300 MODE = NMODE
      if (MODE .ne. 10) go to 840
      K = MBORD(8, 5) + MBORD(8, 6)
      if (IOSTA .lt. 0) then
         if ((K .ne. 0) .or. (LAST .ge. 3)) then
c                      Close out this mfpic group and start next.
            IPLOT = -100 - IPLOT
            call SPLOT9
         end if
         IOSTA = 1

         if (IP(LDEBUG) .gt. 1) print '(''Rewind IOTEMP'')'
C++ CODE for ~.C. is active
         rewind (IOTEMP)
         IOTEMP = IOTMP1
C++ CODE for .C. is inactive
C%%      rewind(iotemp);
c%%      iotemp = iotmp1;
C++ END
         if (LAST .ge. 3) go to 830
         if (K .eq. 0) then
C%%         fclose(iotmp2);
            close (IOTMP2)
         else
           if (IP(LDEBUG) .gt. 1) print '(''Rewind IOTMP2'')'
C%%         rewind(iotmp2);
           rewind (IOTMP2)
         end if
      end if
      if (K .ne. 0) then
         if (MBORD(8, 5) .ne. 0) MBORD(3, 2) = MBORD(3, 2)+MBORD(8, 5)+2
         if (MBORD(8, 6) .ne. 0) MBORD(3, 4) = MBORD(3, 4)+MBORD(8, 6)+2
         do 1320 I = 1, 6
            MBORD(8, I) = 0
 1320    continue
         if (IP(LDEBUG) .gt. 1) print '(''Rewind IOTMP1,IOTMP2,IOFIL'')'
C++ CODE for ~.C. is active
         rewind(IOTMP1)
         rewind(IOTMP2)
         rewind(IOFIL)
C++ CODE for .C. is inactive
C%%      rewind(iotmp1);
C%%      rewind(iotmp2);
C%%      rewind(iofil);
C++ END
         LENTXT(3, 17) = 0
         go to 800
      end if
c                     All done, exit.
      call SPLOT9
C%%   fclose(iofil);
      close (IOFIL)
C%%   fclose(iotemp);
      close (IOTEMP)
      LAST = 0
      if (IOP1 .le. -100) go to 1500
      return
c
c   **************************** Error Processing **********************
c
c                     Set Limits for COPT error message.
 1400 J = LTEXT - 1
 1410 IERR3 = J
      IERR4 = LTEXT
c                     Set limit for OPT error message
 1430 IERR2 = IOP
c                     Output Fatal Error Message
      call SPLOTE(IERR, OPT, COPT)
c                     Error on inner subroutine
 1500 LAST = 0
 1510 OPT(1) = -100 - IOP1
      return
      end


      subroutine SPLOTA(IB)
c Output the single border or axes with index IB, including tick marks,
c labels and captions.  Do any preliminary checks on scaling required
c and open the output file if it is not open yet.
c
c ************************* Usage of internal variables ****************
c
c ADJIN  Space required around borders so points and curves don't
c     interfere with tick marks.
c ADJOUT Length of space outside of borders.  (Not counting captions.)
c ARREXT Parameter giving the amount added to border or axis when there
c     an arrow head.  (All numbers like this are in points.)
c AXADJ  Array used to define direction to move from the border when
c     placing labels.
c CAPLOC Array giving caption locations relative to various borders.  If
c   > 0, caption is centered on its associated axis, else it goes on an
c   end of the associated axis.
c CAPSEP Separation between caption and labels.
c FAC    Gives the lower limit on number of points per major tick that
c        are required when the leading digit of the increment is 1, 2,
c        5, and 10.
c FDIG   Gives the first digit as a function of KDIG.
c IAXP   3-IAX, opposite from the horiz/vert. dirction implied by IB.
c IBCAP  The index of the border where the caption for border I goes.
c KB     Initially set from MBORD(6, IB).  This is used in deciding
c   whether the endpoint should be expanded, and if so, whether to a
c   minor or major tick mark.  If < 3 there is no range expansion, if
c   2 expansion is to a major tick, it 1 it is to the first minor or
c   major tick, and 0 is the same as 1, unless the first major tick
c   is at 0 in which case it expands to 0.
c KDIG   Values of 1, 2, 3, 4, correspond to starting digits of 1, 2, 5,
c   10.
c KLOG   Value from IP(LXBORD or LYBORD), with values from there
c   incremented by 1 if this value is not zero.  A value of 1 is then
c   used to indicate that we don't have room for ticks with logarithmic
c   spacing.
c MINTIC Number of minor tick intervals per major tick interval.  -9 is
c   used to flag logarithmic spacing of the minor tick marks.
c POSLAB Offset of label from border/axis for the 17 usual cases.
c SEPLAB Separation of labels from border or tickmarks.
c SIZEP  Physical size available for plotting in the current direction.
c TICLOG When have logarithmically spaced minor tick marks, give the
c   location of the first minor tick in user coordinates.
c TICMAJ Distance between major tick marks, first in user coordinates,
c   later converted to physcial coordinates in points.
c TICLEN Temp. used to hold tick length.
c TICMIN As for TICMAJ, but for minor tick marks.
c TBASE  Base point for major ticks (nominally 0).  Major ticks are
c   all at TBASE + k * TICMAJ, where k is an integer.  Starts in
c   user coordinates and later is in physical coordinates.
c TP1    Temporary use.
c TP2    Temporary use.
c TP3    Temporary use.
c TLEN   Length of the variable range is user coordinates.
c TPMIN  Used tracking a temporary value for the minimum.
c TPMAX  Used tracking a temporary value for the maximum.
c VAL    Numeric value of label being output.
c XFAC   Physical space available / Length in user coordinates.
c XYPOS  Gives a point (x, y).
c XYPOS1 Gives a point (x_1, y_1).
c XYPOS2 Gives a point (x_2, y_2).
c
c *************************** Variable Declarations ********************
c
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c
c         Parameter pointers for integers in IP.
      integer NEXT, INTERP, LCOOX,LCOOY,LXLINE,LXBORD,LYBORD, LTYPE,
     1  KSET, LTANNO, LPEN, NBORD, LYDIM, LNY, LDEBUG,
     2  LASTIP
      parameter (NEXT=1, INTERP=2, LCOOX=3, LCOOY=4, LXLINE=5,
     1  LXBORD=6, LYBORD=7, LTYPE=8, KSET=9, LTANNO=10, LPEN=13,
     2  NBORD=14, LYDIM=15, LNY=16, LDEBUG=18, LASTIP=LDEBUG)
c          Parameter pointers for floats in FP.
      integer LARROW,LWIDTH,LWIDRE,LBAD,LVALS,LXYSIZ,LASTFP,LFDAT
      parameter (LARROW=1, LWIDTH=2, LWIDRE=6, LBAD=7,
     1  LVALS=9, LXYSIZ=LVALS+5, LASTFP=LXYSIZ+2,LFDAT=LBAD)
c          Parameter for various sizes.
      integer LBNDC, LBNDF, LBNDP, LBNDT, MAXSET
      parameter (LBNDC=128, LBNDF=32, LBNDP=4, LBNDT=64, MAXSET=20)
      real             BORLOC(6), FILL(19), FP(LASTFP), OVLAP,
     1  PHYUSE(2,2), SETLIM(2,2), TLENH, TLENV, TICKS(4,6), TOPTS,
     2  VHLEN(2), XYBASE(MAXSET), XYLIM(2,MAXSET), XYU2PF(MAXSET)
      integer IERR1, IERR2, IERR3, IERR4, IOP1, IP(LASTIP), JSET(2),
     1  LENCAP(6), LENTXT(3,18), MANNO, MBORD(8,6), MFILL(4), NTEXT,
     2  NXYLIM(MAXSET)
      logical  KLIP(MAXSET), NOOUT, OPAQUE
      common / SPLOTB / BORLOC, FILL, FP, OVLAP, PHYUSE, SETLIM, TICKS,
     1  TLENH, TLENV, TOPTS, VHLEN, XYBASE, XYLIM, XYU2PF, IERR1,
     2  IERR2, IERR3, IERR4, IOP1, IP, JSET, LENCAP, LENTXT, MANNO,
     3  MBORD, NTEXT, NXYLIM, KLIP, MFILL, NOOUT, OPAQUE
c
      character FMTNUM(17)*(LBNDF), CAPTIO(6)*(LBNDC), POS*68, TEXT*280,
     1    TXTDEF(18)*(LBNDT)
      common / SPLOTC / FMTNUM, CAPTIO, POS, TEXT, TXTDEF
      save /SPLOTB/, /SPLOTC/
c Locals
      integer IB
      real             ARREXT, CAPSEP, SEPLAB
      parameter (ARREXT=3.E0, CAPSEP = 2.E0, SEPLAB = 3.5E0)

      integer I, I1, I2, IAX, IAXP, IBCAP(6), J, K, KB, KDIG, KLOG,
     1   MINTIC
      real             ADJIN(6), ADJOUT(6), AXADJ(6), CAPLOC(6), FAC(4),
     1   FDIG(4), LOGINT(10), POSLAB(6), SIZEP, TBASE, TICMAJ, TICLEN,
     2   TICLOG, TICMIN, TLEN, TP, TP1, TP2, TP3, TPMIN, TPMAX, VAL,
     3   XFAC, XYPOS(2), XYPOS1(2), XYPOS2(2)
      save ADJOUT, CAPLOC, POSLAB
      data AXADJ / 1.E0, 1.E0, -1.E0, -1.E0, 1.E0, 1.E0 /
      data FAC  / 24.E0, 20.E0, 24.E0, 0.E0 /
      data FDIG / 1.E0, 2.E0, 5.E0, 10.E0 /
      data LOGINT/ 0.E0, .3010299957E0, .47771212547E0, .6020599913E0,
     1    .6989700043E0, .7781512503E0, .8450980400E0, .9030899870E0,
     2    .9542425094E0, 1.E0 /
      save ADJIN
c
c ************************ Start of Executable Code ********************
c
      if (MBORD(8,1) .eq. 0) then
c           First time this routine has been called for this figure.
         if (LENTXT(3, 17) .gt. 0) then
C++ CODE for ~.C. is active
            call SPLOTU (IOFIL, TXTDEF(17)(1:LENTXT(3,17)))
            if (IOP1 .le. -10) return
C++ CODE for .C. is inactive
C%%         iofil = fopen(splotc.txtdef[16], "w");
C%%         if (iofil == NULL) return;
C++ END
         end if
         CAPLOC(1) = 0.E0
         CAPLOC(2) = 0.E0
         CAPLOC(3) = 0.E0
         CAPLOC(4) = 0.E0
         CAPLOC(5) = 0.E0
         CAPLOC(6) = 0.E0
         I1 = 1
         I2 = 6
      else
         if (IB .gt. 10) go to 300
         I1 = IB
         I2 = IB
      end if
      do 10 I = I1, I2
c                       Get initial border characteristics
         IAX = 2 - mod(I, 2)
         if (MBORD(8, I) .ne. 0) go to 30
         if (I .le. 4) MBORD(8, I) = JSET(IAX)
         NOOUT = .true.
c                       Adjustment for tick marks
         ADJOUT(I) = -min(0.E0,min(TICKS(1,I),TICKS(2,I)))
c
         K = MBORD(1, I)
         if ((K .gt. 1) .and. (I .le. 4)) then
c                         Get space for labels.
            if (K .gt. 2) then
              TPMIN = XYLIM(1, IAX)
              TPMAX = XYLIM(2, IAX)
              if (SETLIM(1, IAX) .lt. SETLIM(2, IAX)) then
                 TPMIN = SETLIM(1, IAX)
                 TPMAX = SETLIM(2, IAX)
               end if
               TP = max(abs(TPMIN), abs(TPMAX))
               if (IP(LXBORD+IAX-1) .eq. 1) then
                  TP1 = anint(TP)
                  J = -I
               else
                  K = log10(TP)
                  TP1 = 10.E0 ** K
                  TLEN = TPMAX - TPMIN
                  J = log10(TP / TLEN)
                  if (J .gt. 1) then
                     TP1 = TP1 * 1.1E0**J
                  end if
                  J = I
               end if
               call SPLOTN(sign(TP1,TPMIN), J, XYPOS)
               TP1 = TLENV
               if (IAX .eq. 2) TP1 = TLENH

               POSLAB(I) = ADJOUT(I) + SEPLAB
               ADJOUT(I) = POSLAB(I) + TP1
               if ((I.eq.1) .or. (I.eq.5)) then
                   ADJOUT(I) = ADJOUT(I) - 2.E0
                   POSLAB(I) = POSLAB(I) - 2.E0 + TP1
               end if
            end if
         end if
c              Remember info. on arrows.
         if (MBORD(2, I) .gt. 0) then
            TP1 = MBORD(2, I) + ARREXT
            if (LENCAP(I) .ne. 0) TP1 = TP1 + CAPSEP
c                     Remember adjustment for caption if needed.
            if (CAPLOC(I) .le. 0.E0) CAPLOC(I) = CAPLOC(I) - TP1
         end if
         if (LENCAP(I) .ne. 0) then
c                       Have a caption need to get space required
            NTEXT = LENCAP(I)
C%%         memcpy(splotc.text,splotc.captio[i-1],(size_t)splotb.ntext);
            TEXT(1:NTEXT) = CAPTIO(I)(1:NTEXT)
            call SPLOTT( I+10, XYPOS)
            IAXP = 3 - IAX
            K = 4 * I + 32
            IBCAP(I) = I
    5       if (POS(K+IAXP:K+IAXP) .eq. 'c') then
               if (I .ge. 5) then
c                            Error -- Can't center on x or y axis
                  call SPLOTE(2, XYPOS, ' ')
                  POS(K+IAXP:K+IAXP) = 'r'
                  if (I .eq. 6) POS(K+IAXP:K+IAXP) = 't'
                  go to 5
               end if
               CAPLOC(I) = POSLAB(I) + VHLEN(IAX) + CAPSEP
               ADJOUT(I) = CAPLOC(I)
            else
               J = index('bltr', POS(K+IAXP:K+IAXP))
               IBCAP(I) = J
               TP1 = VHLEN(IAXP) + CAPSEP
               if (mod(J, 2) .eq. 1) TP1 = TP1 - 2.E0
               CAPLOC(I) = CAPLOC(I) - TP1
            end if
         end if
         ADJIN(I) = MBORD(3, I)
         if (MBORD(4, I) .ne. 0) then
            TP = ADJOUT(I)
            ADJOUT(I) = MBORD(4, I)
            if (CAPLOC(I) .gt. 0.E0) then
               CAPLOC(I) = ADJOUT(I)
               POSLAB(I) = POSLAB(I) + .5E0 * (ADJOUT(I) - TP)
            else
               POSLAB(I) = ADJOUT(I)
            end if
         end if
         if (ADJOUT(I) + ADJIN(I) .gt. 100.E0) then
c                             Error -- too much space wasted at border I
           IERR1 = I
           call SPLOTE(3, XYPOS, ' ')
         end if
   10 continue
      if (I1 .ne. I2) then
c                       Special setting the first time.
         do 20 I = 1, 6
            if (CAPLOC(I) .lt. 0) ADJOUT(IBCAP(I)) =
     1         max(ADJOUT(IBCAP(I)), -CAPLOC(I))
   20    continue
         BORLOC(1) = 0.E0
         BORLOC(2) = 0.E0
         BORLOC(3) = TOPTS * FP(LXYSIZ+1) - ADJOUT(1)
         BORLOC(4) = TOPTS * FP(LXYSIZ) - ADJOUT(2)
c                                            Initialize mfpic
         IPLOT = IP(LTYPE)
         PXO = ADJOUT(2)
         PYO = ADJOUT(1)
         PXSIZE = BORLOC(4)
         PYSIZE = BORLOC(3)
         call SPLOT0
         BORLOC(3) = BORLOC(3) - ADJOUT(3)
         BORLOC(4) = BORLOC(4) - ADJOUT(4)
         IAX = 2 - mod(IB, 2)
C++ CODE for ~.C. is active
         if (IP(LDEBUG).gt.0) print '(/'' ADJOUT(1:4)='',1P,4G17.10/)',
     1      (ADJOUT(I), I = 1, 4)
C++ CODE for .C. is inactive
C%%      if (splotb.ip[LDEBUG-1] > 0) printf(
C%%         "\n ADJOUT(1:4)=%17.10g%17.10g%17.10g%17.10g\n", adjout[0],
C%%         adjout[1], adjout[2], adjout[3]);
C++ END
      end if
c                        Process the border or axis
   30 ADJIN(IB) = MBORD(3, IB)
      SIZEP=BORLOC(5-IAX)-BORLOC(3-IAX)-ADJIN(5-IAX)-ADJIN(3-IAX)
      if (SIZEP .le. 10.E0) then
c                           Error -- not enough room for plot
        call SPLOTE(33, XYPOS, ' ')
        return
      end if
      if (IB .gt. 4) then
         if (IB .eq. 6) then
c               Take care of drawing lines at X = 0 and Y = 0, if any.
            if (IP(LXLINE) .ne. 1) then
               I = IP(LXLINE)
               if (I .le. 6) then
                  I = I - 1
                  if (I .lt. 0) I = 6
               end if
               I = I - 1
   40          J = MOD(I, 4) + 1
c                       If 0 in range, draw line off axis J
               K = MBORD(8, J)
               do 50 I1 = 1, MAXSET
                  if (NXYLIM(I1) .eq. K) go to 60
   50          continue
               go to 70
   60          TP = XYBASE(I1)
               I2 = 1 + mod(I1, 2)
               if ((IP(LXBORD+I1-1) .eq. 0) .and.
     1           (TP.gt.BORLOC(I2)) .and. (TP.lt.BORLOC(I2+2))) then
c                                    Draw the line
                  KURPEN = FP(LWIDTH+3)
                  if (I2 .eq. 2) then
                     call SPLOT2 (TP, BORLOC(1), TP, BORLOC(3))
                  else
                     call SPLOT2 (BORLOC(2), TP, BORLOC(4), TP)
                  end if
               end if
               I = I - 5
               if (I .gt. 0) go to 40
               if (I .eq. 0) then
                  I = 4
                  go to 40
               end if
            end if
         end if
c Ignore request for axis if it is not in range of data.
   70    if ((SETLIM(1,IAX).ge.0.E0).or.(SETLIM(2,IAX).lt.0.E0)) return
c                                 Get physical location of axis.
         BORLOC(IB) = XYBASE(IAX)
      else if ((XYU2PF(IAX) .eq. 0.E0) .or. (MBORD(1, IB) .gt. 1)) then
         KLIP(IAX) = .false.
         if (SETLIM(1, IAX) .lt. SETLIM(2, IAX)) then
            KLIP(IAX) = (SETLIM(1, IAX) .gt. XYLIM(1, IAX)) .or.
     1                  (SETLIM(2, IAX) .lt. XYLIM(2, IAX))
            XYLIM(1, IAX) = SETLIM(1, IAX)
            XYLIM(2, IAX) = SETLIM(2, IAX)
         else
            SETLIM(1, IAX) = XYLIM(1, IAX)
            SETLIM(2, IAX) = XYLIM(2, IAX)
         end if
         if (SETLIM(2, IAX) .gt. SETLIM(1, IAX)) then
c                 Usual case, other branch protects against divide by 0.
            XFAC = SIZEP / (SETLIM(2, IAX) - SETLIM(1, IAX))
         else
            XFAC = 1.E0
         end if
      end if
c
c ******* Get location of ticks and adjust limits if necessary *********
c
      if (PHYUSE(1,IAX) .ge. 0.E0) then
c              User coordinate must map to given physical coordinate.
c   TP1 = minimum x_physical, TP2 = loc. user, TP3 = loc. physical
         TP1 = BORLOC(3-IAX) + ADJIN(3-IAX)
         TP2 = PHYUSE(2, IAX)
         TP3 = PHYUSE(1, IAX) * TOPTS
c                  Convert to logs if requested
         if (IP(LCOOX+IAX-1) .eq. 2) TP2 = log10(TP2)
c   TP = maps loc. user to loc. physical with current settings.
         TP = TP1 + XFAC * (TP2 - XYLIM(1,IAX))
         if (TP .gt. TP3) then
            XFAC = (TP3 - TP1) / (TP2 - XYLIM(1,IAX))
            XYLIM(2,IAX) = TP1 + XFAC*(XYLIM(2,IAX)-XYLIM(1,IAX))
         else if (TP .lt. TP3) then
            XYLIM(1,IAX) = TP2 + (TP3 - TP1) / XFAC
         end if
c  No range expansion in this case (otherwise above adjustment fails)
         MBORD(6, IB) = 3
      end if
      TPMAX = XYLIM(2, IAX)
      TPMIN = XYLIM(1, IAX)
      TLEN = TPMAX - TPMIN
      if (TLEN .eq. 0.E0) then
c                         Expand the range
         if (TPMAX .eq. 0) then
            TPMAX = 1.E0
            TPMIN = -1.E0
            TLEN = 2.E0
         else if (TPMAX .gt. 0) then
            TPMIN = 0.E0
            TPMAX = 2.E0 * TPMAX
         else
            TPMAX = 0.E0
            TPMIN = 2.E0 * TPMIN
         end if
      end if
      if (MBORD(1, IB) .lt. 2) go to 170
c                  There are some kind of tick marks.
      KLOG = IP(LXBORD+IB-1)
      if (KLOG .ne. 0) KLOG = KLOG + 1
      TICMAJ = TICKS(4, IB)
      if (TICMAJ .ne. 0.E0) then
c                            Major ticks all specified
         TBASE = TICKS(3, IB)
         if (KLOG .eq. 2) then
            KDIG = TICMAJ
            if (KDIG .ne. 1) KLOG = 1
         else
            KDIG = log10(.98E0 * TBASE)
            KDIG = TICMAJ / 10**KDIG
         end if
         KB = 3
      else
c If the increment between ticks is 0, we need to compute it.
         TBASE = 0.E0
         if (KLOG .eq. 2) then
c                       Logarithmic spacing with minor ticks.
            TICMAJ = 1.E0
            TICMIN = 1.E0
            MINTIC = -9.E0
            if (SIZEP .ge. 24.E0*TLEN) go to 90
c                       Not enough room for minor log ticks
            KLOG = 1
         end if
         K =log10(.4 * TLEN)
c    TICMAJ = first candidate increment (no bigger than needed)
         TICMAJ = 10.E0 ** K
         if (TICMAJ * SIZEP .gt. FAC(3) * TLEN) then
            K = K - 1
            TICMAJ = TICMAJ / 10.E0
         end if
         KDIG = 1
         TP2 = TICMAJ
c Now TP2 is smallest increment (in user coordinates) for major ticks.
c We now decide whether to increase initial size it by 1, 2, 5, or 10.

   80    TP1 = TLEN / TICMAJ
         if (SIZEP .lt. FAC(KDIG) * TP1) then
c   There are less than FAC(KDIG) points per major interval, want more
            KDIG = KDIG + 1
            TICMAJ = FDIG(KDIG) * TP2
            if (KDIG .eq. 2) then
               TP = TLEN / (5.E0*TP2)
               if (abs(ANINT(TP) - TP) .lt. 1.e-5) then
                  if (mod(ANINT(TP), 2.E0) .ne. 0.E0) then
c               Using 5.E0 * TP2 breaks even and 2.E0*TP2 doesn't.
                     KDIG = 3
                     TICMAJ = 5.E0 * TP2
                  end if
               end if
            end if
            go to 80
         end if
      end if
c Have now established TICMAJ as the major tick increment
      MINTIC = MBORD(5, IB)
      if (MINTIC .eq. 0) then
         if ((KDIG .eq. 2) .or. (KDIG .eq. 3)) then
            MINTIC = FDIG(KDIG)
         else
            TP1 = SIZEP * TICMAJ / TLEN
            if (TP1 .ge. 90.E0) then
               MINTIC = 10
            else if (TP1 .ge. 60.E0) then
               MINTIC = 5
            else if (TP1 .ge. 40.E0) then
               MINTIC = 4
            else if (TP1 .ge. 20.E0) then
               MINTIC = 2
            else
               MINTIC = 1
            end if
         end if
      end if
c             And TICMIN is established as the minor tick increment.
      TICMIN = TICMAJ / real(MINTIC)
c Adjust the endpoints -- First insure we don't get points too close
   90 TP3 = (TPMAX - TPMIN) / SIZEP
c                  TP3 used to convert from physical to user coordinates
c Now get the the tick marks on borders if that is desired.
      KB = MBORD(6, IB)
      if (KB .le. 2) then
         TP = TICMIN
         if (KB .eq. 2) TP = TICMAJ
         if (KB .eq. 0) then
            if (TPMIN .gt. 0.E0) then
               KB = 1
               if (TPMIN .le. TICMAJ) TP = TICMAJ
            end if
         end if
         if (KLOG .eq. 2) then
c  Minor ticks are spaced logarithmically, tricky to end on a tick mark.
            J = (TPMIN - TBASE) / TICMAJ
            if (TPMIN .lt. TBASE) J = J - 1
            TP1 = TBASE + J * TICMAJ
            TP2 = TP1
            J = 1
  100       if (TP1 + LOGINT(J) .lt. TPMIN) then
               TP2 = TP1 + LOGINT(J)
               J = J + 1
               go to 100
            end if
            TICLOG = TP2
            go to 120
         end if
         J = (TPMIN - TBASE) / TP
         TP2 = TBASE + real(J)*TP
  110    if (TP2 .gt. TPMIN) then
            TP2 = TP2 - TP
            go to 110
         end if
  120    TPMIN = min(TP2, TPMIN - TP3 * ADJIN(3-IAX))
         if (KB .le. 1) then
c                    If get here, TP = TICMIN
            if (KB .eq. 0) then
               if (TPMAX .lt. 0.E0) then
                  if (TPMAX .ge. -TICMAJ) then
                     TP = TICMAJ
                     KB = 2
                  end if
               end if
            else
               TP = TICMIN
            end if
         end if
         if (KLOG .eq. 2) then
c                               Logarithmic minor ticks.
            J = (TPMAX - TBASE) / TICMAJ
            if (TPMAX .lt. TBASE) J = J - 1
            TP1 = TBASE + J * TICMAJ
            TP2 = TP1
            J = 1
  140       if (TP2 .lt. TPMAX) then
               J = J + 1
               TP2 = TP1 + LOGINT(J)
               go to 140
            end if
            go to 160
         end if
         J = (TPMAX - TBASE) / TP
         TP2 = TBASE + real(J)*TP
  150    if (TP2 .lt. TPMAX) then
            TP2 = TP2 + TP
            go to 150
         end if
      end if
  160 TPMAX = max(TP2, TPMAX + TP3 * ADJIN(5-IAX))
c               Set transformation parameters
  170 if (XYU2PF(IAX) .ne. 0.E0) then
         if (MBORD(1, IB) .le. 1) go to 180
      end if
      XYU2PF(IAX) = (BORLOC(5-IAX) - BORLOC(3-IAX)) / (TPMAX - TPMIN)
      XYBASE(IAX) = -XYU2PF(IAX) * TPMIN
c Let v=x if IAX = 1, and v=y otherwise.  V is mapped to physical
c coordinates by  v_{physical) = XYBASE(IAX) + v * XYU2PF(IAX)
      TBASE = XYBASE(IAX) + XYU2PF(IAX) * TBASE
      TICMIN = TICMIN * XYU2PF(IAX)
      TICMAJ = TICMAJ * XYU2PF(IAX)
      if (KLIP(IAX)) then
         XYLIM(1,IAX) = XYBASE(IAX) + XYU2PF(IAX) * SETLIM(1, IAX)
         XYLIM(2,IAX) = XYBASE(IAX) + XYU2PF(IAX) * SETLIM(2, IAX)
      else
         XYLIM(1,IAX) = XYBASE(IAX) + XYU2PF(IAX) * XYLIM(1, IAX)
         XYLIM(2,IAX) = XYBASE(IAX) + XYU2PF(IAX) * XYLIM(2, IAX)
      end if
c
c ***************** Output Caption, Border/axis, labels ****************
c
c                    First the caption
  180 NOOUT = .false.
      if (LENCAP(IB) .ne. 0) then
c                                Have a caption
         J = IBCAP(IB)
         if (J .eq. IB) then
c                            Caption is being centered
            XYPOS(IAX) = .5E0 * BORLOC(5-IAX)
            XYPOS(3-IAX) = BORLOC(IB) - AXADJ(IB) * CAPLOC(IB)
         else
            XYPOS(IAX) = BORLOC(J) + AXADJ(J) * CAPLOC(IB)
            XYPOS(3-IAX) = BORLOC(IB)
         end if
         NTEXT = LENCAP(IB)
C%%      memcpy(splotc.text, splotc.captio[ib-1],(size_t)splotb.ntext);
         TEXT(1:NTEXT) = CAPTIO(IB)(1:NTEXT)
         call SPLOTT( IB+9, XYPOS)
      end if
      if (MBORD(1, IB) .eq. 0) return
c
c                     Now the Border/axis line
      KURPEN = FP(LWIDTH)
      XYPOS1(1) = BORLOC(2)
      XYPOS1(2) = BORLOC(1)
      XYPOS2(1) = BORLOC(4)
      XYPOS2(2) = BORLOC(3)
      TICLEN = min(TICKS(1,IB), BORLOC(IAX+2) - BORLOC(IAX))
      if (IB .le. 2) then
          XYPOS2(3-IAX) = BORLOC(IB)
      else if (IB .le. 4) then
          XYPOS1(3-IAX) = BORLOC(IB)
          TICLEN = -TICLEN
      else
          XYPOS1(3-IAX) = BORLOC(IB)
          XYPOS2(3-IAX) = BORLOC(IB)
      end if

      if (MBORD(2, IB) .ne. 0) then
         ARRLEN = MBORD(2, IB)
         XYPOS2(IAX) = XYPOS2(IAX) + ARRLEN + ARREXT
      end if

      call SPLOT2 (XYPOS1(1), XYPOS1(2), XYPOS2(1), XYPOS2(2))
      K = MBORD(1, IB)
      if (K .gt. 1) then
c## Code for polar cases yet to be written.
c                                    Major ticks
         TP1 = mod(TBASE - BORLOC(3-IAX), TICMAJ)
         if (TP1 .lt. 0) TP1 = TICMAJ + TP1
         if (TP1 .gt. .99999*TICMAJ) TP1 = TP1 - TICMAJ
         TP1 = TP1 + BORLOC(3-IAX)
         call SPLOT8 (FP(LWIDTH+1), TP1+ADJOUT(3-IAX), TICMAJ,
     1      BORLOC(5-IAX)+ADJOUT(3-IAX)+.1E0, BORLOC(IB)+ADJOUT(IAX),
     2      BORLOC(IB)+TICLEN+ADJOUT(IAX), IAX, -1.E0)
         if (MINTIC .gt. 1) then
c                                    Minor ticks
            TP2 = mod(TBASE - BORLOC(3-IAX), TICMIN)
            if (TP2 .lt. 0) TP2 = TICMIN + TP2
            TP2 = TP2 + BORLOC(3-IAX)
            TICLEN = min(TICKS(2,IB), BORLOC(IAX+2) - BORLOC(IAX))
            if ((IB .eq. 3) .or. (IB .eq. 4)) TICLEN = -TICLEN
            call SPLOT8 (FP(LWIDTH+2), TP2+ADJOUT(3-IAX), TICMIN,
     1         BORLOC(5-IAX)+ADJOUT(3-IAX)+.1E0, BORLOC(IB)+ADJOUT(IAX),
     2         BORLOC(IB)+TICLEN+ADJOUT(IAX), IAX, -1.E0)
         else if (KLOG .eq. 2) then
c                                    Logarithmic minor ticks
            TICLEN = min(TICKS(2,IB), BORLOC(IAX+2) - BORLOC(IAX))
            call SPLOT8 (FP(LWIDTH+2), TP1+ADJOUT(3-IAX), TICMAJ,
     1         BORLOC(5-IAX)+ADJOUT(3-IAX)+.1E0, BORLOC(IB)+ADJOUT(IAX),
     2         BORLOC(IB)+TICLEN+ADJOUT(IAX), IAX,
     3         XYBASE(IAX)+TICLOG*XYU2PF(IAX)+ADJOUT(3-IAX)-.1E0)
         end if
c                                    Labels
         if (K .gt. 2) then
            J = 4*IB - IAX - 1
            if (K .le. 4) then
               TP1 = TP1 + TICMAJ
            else if (TP1 - BORLOC(3-IAX) .lt. 4.E0) then
               POS(J:J) = 'l'
               if (IAX .eq. 2) POS(J:J) = 'b'
            end if
            TP2 = BORLOC(5-IAX) - TICMAJ + .1E0
            OPAQUE = .false.
            OVLAP = -10.E4
            XYPOS(3-IAX) = BORLOC(IB) - AXADJ(IB) * POSLAB(IB)
c
  200       XYPOS(IAX) = TP1
            VAL = (TP1 - XYBASE(IAX)) / XYU2PF(IAX)
            if (abs(VAL) .lt. 1.E-5 * TICMAJ) VAL = 0
            I = IB
            if (IP(LXBORD-1+IAX) .eq. 1) I = -I
            call SPLOTN(VAL, I, XYPOS)
            TP1 = TP1 + TICMAJ
            POS(J:J) = 'c'
            if (TP1 .le. TP2) go to 200
            if (abs(K-4) .ne. 1) then
               if (TP1 .le. BORLOC(5-IAX) + .1E0) then
                  if (TP1 .ge. BORLOC(5-IAX) - 4.E0) then
                     POS(J:J) = 'r'
                     if (IAX .eq. 2) POS(J:J) = 't'
                  end if
                  go to 200
               end if
            end if
         end if
      end if
      return
c
c ************ Option 16 -- Lines and border/axis annotations **********
c
  300 J = IP(LTANNO)
      I = IB - 10
      IAX = 2 - mod(I, 2)
c                         Convert to physical coordinates
      if (IP(LCOOX + IAX - 1) .eq. 2) FP(LVALS) = log10(FP(LVALS))
      XYPOS1(IAX) = XYBASE(IAX) + XYU2PF(IAX) * FP(LVALS)
      if (J .ne. 0) then
c                    Want a tick or some kind of line.
         KURPEN = FP(LWIDTH+min(IP(LTANNO),3))
         if (IP(LTANNO) .eq. 4) KURPEN = KURPEN + 203001
         I1 = I + 2
         if (I1 .gt. 4) I1 = I1 - 4
         XYPOS2(IAX) = XYPOS1(IAX)
         XYPOS1(3-IAX) = BORLOC(I)
         if (J .gt. 2) then
            XYPOS2(3-IAX) = BORLOC(I1)
         else
            XYPOS2(3-IAX) = XYPOS1(IAX) + TICKS(J, I) * AXADJ(IAX)
         end if
         call SPLOT2 (XYPOS1(1), XYPOS1(2), XYPOS2(1), XYPOS2(2))
      end if
      if (NTEXT .eq. 0) return
c                Have an  annotation.
      XYPOS1(3-IAX) = BORLOC(I) - AXADJ(I) * POSLAB(I)
      call SPLOTT( I, XYPOS1)
      return
      end

      subroutine SPLOTE(IERR, OPT, COPT)
c                               Prints Error Messages
c IERR   indicates the error as follows.
c   1 Warning -- Points provided are not being plotted.
c   2 Warning -- Centering on x or y axis not allowed.
c   3 Warning -- Too much space wasted at border I.
c   4 Warning -- Format number out of range.
c   5 Warning -- Unknown format specification:
c   6 Warning -- Caption doesn''t have balanced {...} or $...$:
c  10 Bad option character.
c  11 Bad start of COPT option.
c  12 Runaway in COPT, unbalanced (), [], or {}?.
c  13 Missing #?,
c  14 File name or caption is empty.
c  15 Text inside (), {}, [], may contain at most $I chars.
c  16 [...] must contain 2 or 4 letters.
c  17 First position must be one of "tcbTCB".
c  18 Second position must be one of "lcrLCR.
c  19 Error in third/forth position of [...].
c  20 Bad option index.
c  21 Option value is not an integer.
c  22 Option value is too big to be an integer.
c  23 Digit 10^0 of option 1 is too big.
c  24 Type flag must be 0 or 1.
c  25 Polar coordinates (not implemented) or bad 10^2, 10^3 digit.
c  26 Only digits 1 to 6 can be used for borders.
c  27 Min/max on x or y specified twice.
c  28 NY changed in middle of curve.
c  29 Attempting to change data set without providing data.
c  30 More than NY symbols.
c  31 Bad value for symbol plotting.
c  32 Digit 10^0 for option 19, must be < 5.
c  33 Not enough room for plot.
c  34 Unable to find unused I/O unit number in 10..100.
c  35 Unable to open output file:
c  40 Internal -- Adding points (in SPLOTF) without initialization.
c  41 Internal -- N < -4 on call to SPLOTF.
c  42 Internal -- N < 0 and not in initial state in SPLOTF.
c  43 Internal -- S values must be increasing in SPLOTF.
c OPT    OPT passed in by user.  Only used if 10 < IERR < 33.
c COPT   COPT passed in by user.  Only used if 9 < IERR < 20
c
c Formal Args.
      real             OPT(*)
      integer IERR
      character COPT*(*)
c Common variables
c
c         Parameter pointers for integers in IP.
      integer NEXT, INTERP, LCOOX,LCOOY,LXLINE,LXBORD,LYBORD, LTYPE,
     1  KSET, LTANNO, LPEN, NBORD, LYDIM, LNY, LDEBUG,
     2  LASTIP
      parameter (NEXT=1, INTERP=2, LCOOX=3, LCOOY=4, LXLINE=5,
     1  LXBORD=6, LYBORD=7, LTYPE=8, KSET=9, LTANNO=10, LPEN=13,
     2  NBORD=14, LYDIM=15, LNY=16, LDEBUG=18, LASTIP=LDEBUG)
c          Parameter pointers for floats in FP.
      integer LARROW,LWIDTH,LWIDRE,LBAD,LVALS,LXYSIZ,LASTFP,LFDAT
      parameter (LARROW=1, LWIDTH=2, LWIDRE=6, LBAD=7,
     1  LVALS=9, LXYSIZ=LVALS+5, LASTFP=LXYSIZ+2,LFDAT=LBAD)
c          Parameter for various sizes.
      integer LBNDC, LBNDF, LBNDP, LBNDT, MAXSET
      parameter (LBNDC=128, LBNDF=32, LBNDP=4, LBNDT=64, MAXSET=20)
      real             BORLOC(6), FILL(19), FP(LASTFP), OVLAP,
     1  PHYUSE(2,2), SETLIM(2,2), TLENH, TLENV, TICKS(4,6), TOPTS,
     2  VHLEN(2), XYBASE(MAXSET), XYLIM(2,MAXSET), XYU2PF(MAXSET)
      integer IERR1, IERR2, IERR3, IERR4, IOP1, IP(LASTIP), JSET(2),
     1  LENCAP(6), LENTXT(3,18), MANNO, MBORD(8,6), MFILL(4), NTEXT,
     2  NXYLIM(MAXSET)
      logical  KLIP(MAXSET), NOOUT, OPAQUE
      common / SPLOTB / BORLOC, FILL, FP, OVLAP, PHYUSE, SETLIM, TICKS,
     1  TLENH, TLENV, TOPTS, VHLEN, XYBASE, XYLIM, XYU2PF, IERR1,
     2  IERR2, IERR3, IERR4, IOP1, IP, JSET, LENCAP, LENTXT, MANNO,
     3  MBORD, NTEXT, NXYLIM, KLIP, MFILL, NOOUT, OPAQUE
c Locals
      integer I, IEARR(1), J, J1, J2, K, LCOPT(10:19), LOPT(20:31),
     1   LOTHER(33:39), LWARN(7), MACT1(5), MACT2(5), MACT3(2),
     2   MACT4(2), MACT5(7)
      character TXTCOP(1)*200, TXTOPT(1)*40
c Parameters for error messages
      integer MENTXT, MECONT, MERET, MEEMES, METEXT, MEFVEC
      parameter (MENTXT=23, MECONT=50, MERET=51, MEEMES=52, METEXT=53,
     1   MEFVEC=61)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA SPLOT$B
cAB Warning -- Points provided are not being plotted.$E
cAC Warning -- Centering on x or y axis not allowed.$E
cAD Warning -- Too much space wasted at border $I.$E
cAE Warning -- Format number out of range.$E
cAF Warning -- Unknown format specification: $B
cAG Warning -- Caption doesn't have balanced {...} or $$...$$:$E
cAH Warning -- Caption in physical coordinates does not fit.$E
c   $
cAI SPLOT$B
cAJ Bad option character.$E
cAK Bad start of COPT option.$E
cAL Runaway in COPT, unbalanced (), [], or {}?$E
cAM Missing #?, $E
cAN File name or caption is empty.  $E
cAO Text inside (), {}, [], may contain at most $I chars.  $E
cAP [...] must contain 2 or 4 letters.$E
cAQ First position must be one of "tcbTCB".$E
cAR Second position must be one of "lcrLCR.$E
cAS Error in third/forth position of [...].$E
c   $
cAT SPLOT$B
cAU Bad option index.$E
cAV Option value is not an integer.$E
cAW Option value is too big to be an integer.$E
cAX Digit 10^0 of option 1 is too big.$E
cAY Type flag must be 0 or 1.$E
cAZ Polar coordinates (not implemented) or bad 10^2, 10^3 digit.$E
cBA Only digits 1 to 6 can be used for borders.$E
cBB Min/max on x or y specified twice.$E
cBC NY changed in middle of curve.$E
cBD Attempting to change data set without providing data.$E
cBE More than NY symbols.$E
cBF Bad value for symbol plotting.$E
cBG Digit 10^0 for option 19, must be < 5.$E
c   $
cBH SPLOT$B
cBI Not enough room for plot.$E
cBJ Unable to find unused I/O unit number in 10..100.$E
cBK Unable to open output file: $B
cBL Internal Error -- Adding point (in SPLOTF) without initialization.$E
cBM Internal Error -- N < -4 on call to SPLOTF.$E
cBN Internal Error -- N < 0 and not in initial state in SPLOTF.$E
cBO Internal Error -- S values must be increasing in SPLOTF.$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM,LTXTAN,LTXTAO,LTXTAP,LTXTAQ,
     * LTXTAR,LTXTAS,LTXTAT,LTXTAU,LTXTAV,LTXTAW,LTXTAX,LTXTAY,LTXTAZ,
     * LTXTBA,LTXTBB,LTXTBC,LTXTBD,LTXTBE,LTXTBF,LTXTBG,LTXTBH,LTXTBI,
     * LTXTBJ,LTXTBK,LTXTBL,LTXTBM,LTXTBN,LTXTBO
      parameter (LTXTAA=  1,LTXTAB=  8,LTXTAC= 59,LTXTAD=109,LTXTAE=158,
     * LTXTAF=199,LTXTAG=242,LTXTAH=302,LTXTAI=  1,LTXTAJ=  8,
     * LTXTAK= 31,LTXTAL= 58,LTXTAM=102,LTXTAN=116,LTXTAO=150,
     * LTXTAP=207,LTXTAQ=243,LTXTAR=285,LTXTAS=326,LTXTAT=  1,
     * LTXTAU=  8,LTXTAV= 27,LTXTAW= 60,LTXTAX=103,LTXTAY=139,
     * LTXTAZ=166,LTXTBA=228,LTXTBB=273,LTXTBC=309,LTXTBD=341,
     * LTXTBE=396,LTXTBF=419,LTXTBG=451,LTXTBH=  1,LTXTBI=  8,
     * LTXTBJ= 35,LTXTBK= 86,LTXTBL=116,LTXTBM=184,LTXTBN=229,
     * LTXTBO=291)
      character MTXTAA(2) * (180)
      character MTXTAB(2) * (183)
      character MTXTAC(2) * (245)
      character MTXTAD(2) * (174)
      data MTXTAA/'SPLOT$BWarning -- Points provided are not being plott
     *ed.$EWarning -- Centering on x or y axis not allowed.$EWarning --$
     * Too much space wasted at border $I.$EWarning -- Format numbe','r$
     * out of range.$EWarning -- Unknown format specification: $BWarning
     * -- Caption doesn''t have balanced {...} or $$...$$:$EWarning -- C
     *aption in physical coordinates does not fit.$E '/
      data MTXTAB/'SPLOT$BBad option character.$EBad start of COPT optio
     *n.$ERunaway in COPT, unbalanced (), [], or {}?$EMissing #?, $EFile
     * name or caption is empty.  $EText inside (), {}, [], may contai',
     *'n at most $I chars.  $E[...] must contain 2 or 4 letters.$EFirst$
     * position must be one of "tcbTCB".$ESecond position must be one of
     * "lcrLCR.$EError in third/forth position of [...].$E'/
      data MTXTAC/'SPLOT$BBad option index.$EOption value is not an inte
     *ger.$EOption value is too big to be an integer.$EDigit 10^0 of opt
     *ion 1 is too big.$EType flag must be 0 or 1.$EPolar coordinates (n
     *ot implemented) or bad 10^2, 10^3 digit.$EOnly digits 1 to 6',' ca
     *n be used for borders.$EMin/max on x or y specified twice.$ENY cha
     *nged in middle of curve.$EAttempting to change data set without pr
     *oviding data.$EMore than NY symbols.$EBad value for symbol plottin
     *g.$EDigit 10^0 for option 19, must be < 5.$E'/
      data MTXTAD/'SPLOT$BNot enough room for plot.$EUnable to find unus
     *ed I/O unit number in 10..100.$EUnable to open output file: $BInte
     *rnal Error -- Adding point (in SPLOTF) without initiali','zation.$
     *EInternal Error -- N < -4 on call to SPLOTF.$EInternal Error -- N$
     * < 0 and not in initial state in SPLOTF.$EInternal Error -- S valu
     *es must be increasing in SPLOTF.$E'/
c ********* End of Error message text ***************
c
c                    123456789012345678901
      data TXTOPT / ' O.K. part of OPT:$BError part of OPT:$B' /

      data LWARN / LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH /

      data LCOPT / LTXTAJ, LTXTAK, LTXTAL, LTXTAM, LTXTAN, LTXTAO,
     1   LTXTAP, LTXTAQ, LTXTAR, LTXTAS /

      data LOPT / LTXTAU, LTXTAV, LTXTAW, LTXTAX, LTXTAY, LTXTAZ,
     1   LTXTBA, LTXTBB, LTXTBC, LTXTBD, LTXTBF, LTXTBG /

      data LOTHER / LTXTBI, LTXTBJ, LTXTBK, LTXTBL, LTXTBM, LTXTBN,
     1   LTXTBO /
c                       1  2  3  4      5
      data MACT1 / MEEMES, 0, 0, 0, MERET /
      data MACT2 / MEEMES,47, 0, 0, MECONT /
      data MACT3 / METEXT, MECONT /
      data MACT4 / METEXT, MERET /
      data MACT5 / METEXT, MEFVEC, 0, METEXT, MEFVEC, 0, MERET /
c
c ************************ Start of Executable Code ********************
c
      if ((IERR .eq. 5) .or. (IERR .eq. 35)) MACT1(5) = MECONT
      IEARR(1) = IERR1
      if (IERR .le. 19) then
         if (IERR .le. 7) then
            MACT1(2) = 25
            MACT1(3) = IERR
            MACT1(4) = LWARN(IERR)
            call MESS(MACT1, MTXTAA, IEARR)
            go to 250
         else if (IERR .ge. 10) then
            MACT2(3) = IERR
            MACT2(4) = LCOPT(IERR)
            call MESS(MACT2, MTXTAB, IEARR)
         else
            go to 300
         end if
      else if (IERR .le. 32) then
         MACT2(3) = IERR
         MACT2(4) = LOPT(IERR)
         call MESS(MACT2, MTXTAC, IEARR)
         go to 100
      else
         MACT1(2) = 47
         MACT1(3) = IERR
         I = IERR
         if (I .gt. 35) I = I - 4
         if (I .gt. 39) go to 300
         MACT1(4) = LOTHER(I)
         call MESS(MACT1, MTXTAD, IEARR)
         go to 250
      end if
c                   Take care of COPT part of message.
      J1 = 1
      J2 = IERR3
      if (J2 .le. 0) J2 = IERR4
   10 TXTCOP(1)(1:20) = ' O.K. part of COPT: '
      if (IERR3 .le. 0) TXTCOP(1)(1:6) = 'Error '
      K = 21
   20 do 40 J = J1, J2
         TXTCOP(1)(K:K) = COPT(J:J)
         if (TXTCOP(1)(K:K) .eq. '$') then
            K = K + 1
            TXTCOP(1)(K:K) = '$'
         end if
         K = K + 1
         if (K .gt. 196) then
            TXTCOP(1)(K:K+1) = '$B'
            call MESS(MACT3, TXTCOP, IEARR)
            K = 1
            J1 = J + 1
            go to 20
         end if
   40 continue
      TXTCOP(1)(K:K+1) = '$E'
      if ((IERR3 .lt. 0) .or. ((IOP1 .le. 0) .and. (IERR3 .eq. 0))) then
         call MESS(MACT4, TXTCOP, IEARR)
         go to 200
      end if
      call MESS(MACT3, TXTCOP, IEARR)
      if (IERR3 .gt. 0) then
         IERR3 = 0
         J1 = J
         J2 = IERR4
         go to 10
      end if
c                   Take care of OPT part of message.
  100 MACT5(3) = IOP1 - 1
      MACT5(6) = -IERR2
      call SMESS(MACT5, TXTOPT, IEARR, OPT)
c
  200 IOP1 = -100 - IERR
      return
c                   Check for special case
  250 if (MACT1(5) .eq. MERET) go to 200
c                   Set up for output of format spec. or file name.
      MACT1(5) = MERET
      J1 = 1
      J2 = IERR1
      IERR3 = -1
      K = 1
      go to 20
c                   An internal error
  300 continue
C%%   puts( "Internal error in SPLOT, bad error index." );
C%%   close_units();
C%%   puts( "[Stop]" );
C%%   exit(0);
      stop 'Internal error in SPLOT, bad error index.'
      end

      subroutine SPLOTF(N, S, X, Y)
c### Want to add provision for polar coordinates.
c
c  Selects points for plotting so as to get nice curves without using
c  too many points.
c
c N      Indicates action desired.  One must have provided a negative
c  value for N (to start a new curve) before supplying a positive value.
c   > 0   Number of points provided for plotting.
c   = 0   End of current curve (if any).
c   < 0   Start a new curve as follows:
c     = -1  Just providing pairs of points (X, Y).  X(1:2) give the
c           maximum and minimum value expecter for X with Y(1:2)
c           similarly defined, except for Y.
c     = -2  As for -2, except, providing X and Y as functions of S.
c           Values of S must be monotone increasing or monotone
c           decreasing.
c     = -3  As for -1, except the min and max values are limits and the
c           curve is to be clipped if it exceeds these limits.
c     = -4  As for -2, with the clipping as with -3.
c S()   Only used when the initial value for N = -2, -4, -22, and -24,
c       in which case it is the independent variable.
c X(), Y()  The point pairs being provided, in physical units.
c       If N < -10 gives the range of values for the independent
c       variable.
c
c When S is not provided, an internal value for S is constructed based
c on a polygonal approximation to the length of the curve.  In all cases
c X and Y are thought of as functions of S.  Points are selected in such
c a way that piecewise cubic approximations for X(S) and Y(S) are
c thought to be accurate to within TOL * max|X(S)| and TOL * max|Y(S)|
c respectively, where TOL is 0.01.
c
c ******************* External Reference *******************************
c
c SPLOTL is called for the final stage of outputting points.
c
c ******************* Variable Definitions *****************************
c
c DS     Array used to old S values for computing divided differences.
c DX     Array used to hold divided differences for X.
c DY     Array used to hold divided differences for Y.
c ERRMXL Previous estimate for ERRMAX.
c ERRMAX Estimate for largest error.
c GETS   Logical variable that is true if SPLOTF computes S.
c H      Current step size.  If < 0, then H has not yet been selected.
c HMAX   Maximum value to be considered for H.
c HMIN   Minimum value to be considered for H.
c I      Temporary index.
c I1     Starting index for loop processing new points.
c I2     Final index for loop processing new points.
c IKLIP  Index of input point that causes points to be processed due
c        to clipping.
c ILAST  Index into XI, YI arrays for last values to send out.
c J      Temporary index.
c K      Temporary index.
c KLIP   Used to indicate whether clipping (X or Y values out of bounds)
c        is active.
c    = 0    No checking being done for clipping.
c    =-1    Currently out of range, check for getting in range.
c    = 1    Currently in range, check for getting point out of range.
c    = 2    Got a point our of range in process of getting next one,
c           or processing the points up to the clipping point.
c    =-2    Initialize for clipping.
c KLIPS  If 0, the start of the data is not clipped, else if -1 it is.
c KORD   Parameter giving the degree assumed for the interpolating
c        polynomial.
c KORD1  = KORD + 1
c KORD2  = KORD + 2
c L      Temporary index.
c LOC    Array mapping index of selected point into SI, XI, YI arrays.
c LAST   Flag to be interpreted as follows.
c     -1  Not yet initialized.
c     >-1 Number of points in internal buffer ready for processing.
c MX     Parameter giving the maximum number of points that can be
c        examined at one time.
c N      Formal argument, see above.
c NI     Internal value for N.
c S      Formal argument, see above.
c S1     Value of S for which one is getting interpolated values.
c SI     Internal saved values for S.  (Either input values for S, or
c        ones that have been generated.
c TOL    Parameter giving requested relative accuracy in the
c        intepolation for X and Y.
c TOLLO  Low value for tolerance = TOL / 8.
c TP     Temporary real variable.
c TP1    Temporary real variable.
c TP2    Temporary real variable.
c X      Formal argument, see above.
c X1     Value interpolated for X(S1).
c XD     Temporary storage for difference between X values.
c XI     Internal saved values for X.
c XMAX   Interpolated values with X > XMAX cause clipping.  See N = -5
c        above.
c XMIN   As for XMAX, except for X < XMIN.
c YMAX   As for XMAX, except for value of Y.
c YMIN   As for YMAX, except for Y < YMIN.
c XSCAL  (Largest X - Smallest X) in the graph.
c Y      Formal argument, see above.
c Y1     Value interpolated for Y(S1).
c YD     Temporary storage for difference between Y values.
c YI     Internal saved values for Y.
c YSCAL  (Largest Y - Smallest Y) in the graph.
c
c ******************* Variable Declarations ****************************
c
      integer N
      real             S(*), X(*), Y(*)
      integer I, I1, I2, IKLIP, ILAST, J, K, KLIP, KLIPS, KORD, KORD1,
     1    KORD2, L, LAST, LOC(4), MX, NI
      logical GETS
C++ Default KORD = 2
C++ Substitute for KORD below
      parameter (MX = 101, KORD=2)
      parameter (KORD1=KORD+1, KORD2=KORD+2)
      real             DS(KORD2), DX(KORD2), DY(KORD2), ERRMAX, ERRMXL,
     1    H, HMAX, HMIN, S1, SI(0:MX), TOL, TOLLO, TP, TP1, TP2, X1, Y1,
     2    XD, YD, XI(MX), XMAX, XMIN, YI(MX), XSCAL, YMAX, YMIN, YSCAL
      parameter (TOL = 1.E-3)
      parameter (TOLLO = .25E0 * TOL)
      save DS, DX, DY, GETS, H, KLIP, KLIPS, LAST, SI, XI, YI, XMAX,
     1   XMIN, XSCAL, YMAX, YMIN, YSCAL
      data  SI(0), LAST / 0.E0, -1 /
      data KLIP, XMAX, YMAX / 0,  0.E0, 0.E0 /
c
c ******************* Start of Executable Code *************************
c
      NI = N
  100 if (LAST .eq. -1) then
c                         Initial State
         H = 0.E0
         if (NI .ge. 0) then
            if (NI .eq. 0) return
c                   Trying to add points without initialization
            call SPLOTE(40, S, ' ')
            return
         end if
         if (NI .lt. -4) then
c                             N < -4 on call to SPLOTF
            call SPLOTE(41, S, ' ')
            return
         end if
         LAST = 0
         XMIN = X(1)
         XMAX = X(2)
         YMIN = Y(1)
         YMAX = Y(2)
         XSCAL = max(abs(XMIN), abs(XMAX))
         YSCAL = max(abs(XMIN), abs(XMAX))
         KLIP = 0
         if (NI .lt. -2) then
            KLIP = -2
            NI = NI + 2
         end if
         GETS = NI .eq. -1
         return
      else if (NI .lt. 0) then
c                         N < 0 and not in initial state
         call SPLOTE(42, S, ' ')
         return
      end if
      IKLIP = 0
      I1 = 1
  380 if (KLIP .eq. -2) then
         KLIP = 1
         KLIPS = 0
         if ((X(I1) .lt. XMIN) .or. (X(I1) .gt. XMAX) .or.
     1      (Y(I1) .lt. YMIN) .or. (Y(I1) .gt. YMAX)) then
            KLIP = -1
            KLIPS = -1
         end if
      end if
c                Add points to list
  400 I2 = min(NI, MX - LAST)
      do 420 I = I1, I2
         if (KLIP .ne. 0) then
c          Check clipping -- First check if in range
            if ((X(I) .lt. XMIN) .or. (X(I) .gt. XMAX) .or.
     1         (Y(I) .lt. YMIN) .or. (Y(I) .gt. YMAX)) then
c                         Current point is out of range.
               if (KLIP .eq. -1) then
c                     We are getting points that are out of range.
                  LAST = 0
               else
c      We have got a point out of range after being in range.
                  if (LAST + KLIPS .eq. 1) then
c               No points used if only one is inside the region.
                     KLIP = -1
                     LAST = 0
                     KLIPS = -1
                  else if (KLIP .eq. 1) then
c                     Flag that this is the last I for the current set.
                     KLIP = 2
                     IKLIP = I
                  end if
               end if
            else if (KLIP .ne. 1) then
c                    Just got a point in range
               if (KLIP .eq. -1) then
c                     Flag that we are now getting points in range.
                  KLIP = 1
               end if
            end if
         end if
c                          End of test for clipping
         LAST = LAST + 1
         XI(LAST) = X(I)
         YI(LAST) = Y(I)
         if (GETS) then
            if (LAST .eq. 1) then
               SI(1) = 0.E0
            else
               XD = abs(XI(LAST) - XI(LAST-1))
               YD = abs(YI(LAST) - YI(LAST-1))
               if (XD .lt. YD) then
                  SI(LAST) = SI(LAST-1) + YD * sqrt(1.E0 + (XD/YD)**2)
               else if (XD .eq. 0.E0) then
c                                      Skip the input
                  LAST = LAST - 1
               else
                  SI(LAST) = SI(LAST-1) + XD * sqrt(1.E0 + (YD/XD)**2)
               end if
            end if
         else
            SI(LAST) = S(I)
            if (LAST .ne. 1) then
               if (SI(LAST) .eq. SI(LAST-1)) then
                  LAST = LAST - 1
               else if (SI(LAST) - SI(LAST-1) .lt. 0.E0) then
c                                            S values must be increasing
                  call SPLOTE(43, S, ' ')
                  return
               end if
            end if
         end if
         if (KLIP .eq. 2) go to 430
  420 continue
      I1 = I
      if (NI .gt. 0) then
         if (LAST .lt. MX) return
      else
         if (LAST .eq. 0) return
      end if
c                            Code to take care of clipping
  430 if (KLIP .ne. 0) then
c                          If LAST is < 3 just skip the output.
         if (LAST .lt. 3) go to 880
         if ((KLIPS .lt. 0) .or. (KLIP .gt. 1)) then
            if (KLIPS .lt. 0) then
c Setup to fit quadratic to first three points to get replacement value.
               do 440 J = 1, 3
                  DS(J) = SI(4-J)
                  DX(J) = XI(4-J)
                  DY(J) = YI(4-J)
  440          continue
               I2 = 1
               go to 470
            end if
c Setup to fit quadratic to last three points to get replacement value.
  450       do 460 J = 1, 3
               DS(J) = SI(LAST+J-3)
               DX(J) = XI(LAST+J-3)
               DY(J) = YI(LAST+J-3)
  460       continue
            I1 = I + 1
            I2 = LAST
            KLIP = -1
c Get divided differences, and interpolated values for boundary values.
  470       do 490 K = 1, 2
               do 480 J = 1, 3 - K
                  DX(J) = (DX(J+1) - DX(J)) / (DS(J+K) - DS(J))
                  DY(J) = (DY(J+1) - DY(J)) / (DS(J+K) - DS(J))
  480          continue
  490       continue
c At this point either DX(3), or DY(3) is out of range, and we would
c like to replace the "worst" one with a value on the boundary.
            DS(2) = DS(2) - DS(3)
            DS(1) = DS(1) - DS(3)
            X1 = 0.E0
            Y1 = 0.E0
            if ((DY(3) .lt. YMIN) .or. (DY(3) .gt. YMAX)) then
c    Get TP and TP1 for quadratic:  DY(1)*s^2 - TP1 * s + TP = 0
c    Where s is the increment from DS(3)
               TP = -YMIN
               if (DY(3) .gt. YMAX) TP = -YMAX
               YD = TP
               TP = TP + DY(3)
               TP1 = DY(2) - DY(1) * DS(2)
c                     Get Y1 = smallest root
               TP2 = TP1**2 - 4.E0*DY(1)*TP
               if (TP2 .ge. 0.E0) then
c                     Have real roots, else just ignore problem
                  Y1 = -2.E0 * TP / (TP1 + sign(sqrt(TP2), TP1))
                  if (Y1 * (Y1 - DS(2)) .gt. 0.E0) then
c                 Smallest root not in desired interval try the big one.
                     Y1 = TP / Y1
                     if (Y1 * (Y1 - DS(2)) .gt. 0.E0) Y1 = 0.E0
                  end if
               end if
            end if
            if ((DX(3) .lt. XMIN) .or. (DX(3) .gt. XMAX)) then
c                                             Same as above except for X
               TP = -XMIN
               if (DX(3) .gt. XMAX) TP = -XMAX
               XD = TP
               TP = TP + DX(3)
               TP1 = DX(2) - DX(1) * DS(2)
c                     Get X1 = smallest root
               TP2 = TP1**2 - 4.E0*DX(1)*TP
               if (TP2 .ge. 0.E0) then
c                     Have real roots, else just ignore problem
                  X1 = -2.E0 * TP / (TP1 + sign(sqrt(TP2), TP1))
                  if (X1 * (X1 - DS(2)) .gt. 0.E0) then
c                 Smallest root not in desired interval try the big one.
                     X1 = TP / X1
                     if (X1 * (X1 - DS(2)) .gt. 0.E0) X1 = 0.E0
                  end if
               end if
            end if
            TP = Y1
c                    Pick value that is nearest middle of region
            if (DS(2) * (TP - X1) .lt. 0.E0) then
               TP = X1
               XI(I2) = -XD
c                Insure that high difference doesn't give a bulge.
               TP1 = (TP - DS(2)) * DY(1)
               if (DY(2) .lt. 0.E0) then
                  TP1 = min(TP1, -.75E0*DY(2))
               else
                  TP1 = max(TP1, -.75E0*DY(2))
               end if
               YI(I2) = DY(3) + TP * (DY(2) + TP1)
            else
               YI(I2) = -YD
c                Insure that high difference doesn't give a bulge.
               TP1 = (TP - DS(2)) * DX(1)
               if (DX(2) .lt. 0.E0) then
                  TP1 = min(TP1, -.75E0*DX(2))
               else
                  TP1 = max(TP1, -.75E0*DX(2))
               end if
               XI(I2) = DX(3) + TP * (DX(2) + TP1)
            end if
            SI(I2) = SI(I2) + TP
            if (KLIPS .lt. 0) then
               KLIPS = 0
               if (KLIP .gt. 1) go to 450
            end if
         end if
      end if
      if (H .ne. 0.E0) then
         I = KORD1
         J = KORD1
         go to 800
      end if
c####
c#### If polar coodinates, this is place to do the transformation.
c#### Remember last point transformed.  Think we don't want to touch
c#### points much beyond the half way point until we know we have
c#### seen the end.  Need to recompute s, x, and y.
c####
c                      Need to get the starting H
      ERRMXL = -1.E0
c            Process the points -- First get the starting step size
      HMAX = .25E0 * (SI(LAST) - SI(1))
      HMIN = 0.E0
      H = .25E0 * (SI(min(LAST,8)) - SI(1))
  520 TP = SI(1)
      I = 0
      J = 1
      go to 550
c            Just selected a new point.
  540 if (J .gt. LOC(I) + 1) J = J - 1
  550 I = I + 1
c             I is index for points we are planning to test
c             J is index from which we get the points.
      LOC(I) = J
      if (I .lt. KORD1) then
         TP = TP + H
         do 600 J = J + 1, LAST
c                                Save and process next if gone too far
            if (SI(J) .gt. TP) go to 540
  600    continue
c                                Didn't get a full set of points.
         J = LAST
         if (LOC(I) .ne. J) then
c                                Take the last point if we can.
            I = I + 1
            LOC(I) = J
         end if
         if (I .ne. KORD1) then
            if (J .gt. I) then
c                    We could have got more in the initial set.
               HMAX = .875E0 * H
               H = .5E0 * (H + HMIN)
               go to 520
            end if
            go to 750
         end if
      end if
c                   Check if error is about right
      do 630 J = 1, KORD1
         K = LOC(J)
         DS(J) = SI(K)
         DX(J) = XI(K)
         DY(J) = YI(K)
  630 continue
c                  Get divided differences
      do 650 K = 1, KORD
         do 640 J = 1, KORD1 - K
            DX(J) = (DX(J+1) - DX(J)) / (DS(J+K) - DS(J))
            DY(J) = (DY(J+1) - DY(J)) / (DS(J+K) - DS(J))
  640    continue
  650 continue
c               Check accuracy -- First for the starting stepsize.
      ERRMAX = 0.E0
      J = 2
      TP = SI(LAST)
      do 670 K = 2, LAST
         if (K .eq. LOC(J)) then
            if (J .lt. KORD1) then
               J = J + 1
            else
               TP = SI(K) + H
            end if
         else
            S1 = SI(K)
            if (S1 .gt. TP) go to 680
C++ CODE for KORD == 3 is inactive
C            X1 = DX(4) + (S1 - DS(4)) * (DX(3) + (S1 - DS(3)) *
C     1         (DX(2) + (S1 - DS(2)) * DX(1)))
C            ERRMAX = max(ERRMAX, abs(X1 - XI(K)) / XSCAL)
C            Y1 = DY(4) + (S1 - DS(4)) * (DY(3) + (S1 -  DS(3)) *
C     1                 (DY(2) + (S1 - DS(2)) * DY(1)))
C            ERRMAX = max(ERRMAX, abs(Y1 - YI(K)) / YSCAL)
C++ CODE for KORD == 2 is active
            X1 = DX(3) + (S1 - DS(3)) * (DX(2) + (S1 - DS(2))*DX(1))
            ERRMAX = max(ERRMAX, abs(X1 - XI(K)) / XSCAL)
            Y1 = DY(3) + (S1 - DS(3)) * (DY(2) + (S1 - DS(2))*DY(1))
            ERRMAX = max(ERRMAX, abs(Y1 - YI(K)) / YSCAL)
C++ END
         end if
  670 continue
  680 if (ERRMAX .eq. ERRMXL) HMIN = H
      ERRMXL = ERRMAX
      if (ERRMAX .gt. TOL) then
         if (H .gt. HMIN) then
            HMAX = .857E0 * H
            H = max(HMIN, H * sqrt(sqrt(.5E0 * TOL / ERRMAX)))
            go to 520
         end if
      else if (ERRMAX .lt. TOLLO) then
         if (ERRMAX .ne. 0.E0) then
            if (H .lt. HMAX) then
               HMIN = 1.125E0 * H
               H = min(HMAX, H * sqrt(sqrt(.5E0 * TOL / ERRMAX)))
               go to 520
            end if
         end if
         if ((NI .gt. 0) .and. (LOC(KORD1) .ne. KORD1)) then
            K = 0
            do 690 L = 1, I
               J = LOC(L)
               K = K + 1
               XI(K) = XI(J)
               YI(K) = YI(J)
               SI(K) = SI(J)
  690       continue
c                   Set up to get more points before output.
            do 710 K = K+1, LAST + K - J
               J = J + 1
               XI(K) = XI(J)
               YI(K) = YI(J)
               SI(K) = SI(J)
  710       continue
            LAST = K - 1
c                    Flag that we didn't see enough points.
            H = 0.E0
            if (I1 .gt. NI) return
            go to 400
         end if
      end if
c              Shift data to output place.
  750 do 760 K = 1, I
         J = LOC(K)
         XI(K) = XI(J)
         YI(K) = YI(J)
         SI(K) = SI(J)
  760 continue
c             Get rest of data, checking accuracy as we go.
  800 L = J
      TP = SI(I) + .3333333E0 * (SI(I) - SI(I-KORD))
      TP1 = 1.E0
  830 if (J .lt. LAST) then
         J = J + 1
         S1 = SI(J)
         if (S1 .gt. TP) TP1 = ((S1 - SI(I)) / (TP - SI(I)))**KORD1
         if (KORD .eq. 3) then
            X1 = DX(4) + (S1 - DS(4)) * (DX(3) + (S1 - DS(3)) *
     1         (DX(2) + (S1 - DS(2)) * DX(1)))
            Y1 = DY(4) + (S1 - DS(4)) * (DY(3) + (S1 -  DS(3)) *
     1                 (DY(2) + (S1 - DS(2)) * DY(1)))
         else if (KORD .eq. 2) then
            X1 = DX(3) + (S1 - DS(3)) * (DX(2) + (S1 - DS(2))*DX(1))
            Y1 = DY(3) + (S1 - DS(3)) * (DY(2) + (S1 - DS(2))*DY(1))
         end if
         ERRMAX = TP1 * max(abs(X1-XI(J))/XSCAL, abs(Y1-YI(J))/YSCAL)
         if (ERRMAX .le. TOL) go to 830
         if (J .gt. L+1) J = J - 1
         I = I + 1
c                          Save data
         SI(I) = SI(J)
         XI(I) = XI(J)
         YI(I) = YI(J)
c                          Update the differences
         do 850 L = 1, KORD
            DS(L) = DS(L+1)
            DX(L) = DX(L+1)
            DY(L) = DY(L+1)
  850    continue
         DS(KORD1) = SI(I)
         DX(KORD1) = XI(I)
         DY(KORD1) = YI(I)
         do 870 L = KORD, 1, -1
            DX(L) = (DX(L+1) - DX(L)) / (DS(KORD1) - DS(L))
            DY(L) = (DY(L+1) - DY(L)) / (DS(KORD1) - DS(L))
  870    continue
         go to 800
      end if
      ILAST = I - KORD1
      if (L .lt. J) then
c                         Save last point if not saved yet.
         I = I + 1
         SI(I) = SI(J)
         XI(I) = XI(J)
         YI(I) = YI(J)
      end if
  880 if (KLIP .eq. -1) then
         if (I .gt. 1) then
            ILAST = I
         else
            ILAST = 0
         end if
      else if (NI .eq. 0) then
         ILAST = I
      end if
c            Get output for points I = 1 to I = ILAST
      if (ILAST .ne. 0) call SPLOTL(ILAST, XI, YI)
      if (IKLIP .ne. 0) then
c              Continue with point causing clipping.
         I1 = IKLIP
         LAST = 0
         KLIP = -2
         call SPLOTL(-1, XI, YI)
         go to 380
      end if
      if (NI .eq. 0) then
c                    End of a data set, get into initial state.
         LAST = -1
         call SPLOTL(0, XI, YI)
         go to 100
      end if
      LAST = 0
      do 900 J = ILAST+1, I
c              Set up to start over.
         LAST = LAST + 1
         SI(LAST) = SI(J)
         XI(LAST) = XI(J)
         YI(LAST) = YI(J)
  900 continue
      if (I1 .gt. NI) return
      go to 400
      end


      subroutine SPLOTN(VAL, IKASE, XYPOS)
c For output of numeric labels, F. T. Krogh, JPL, July 18, 1997.
c
c ************************* Arguments passed in ************************
c
c VAL    Value to be printed.
c IKASE  The label case.  See comments for LENTXT in SPLOT above.  If
c    < 0, the value provided is the log10 of the number.
c OPAQUE .true. if the label is to go into an opaque box.
c XYPOS  (Physical coordinate of the absicssa, Etc. for coordinate)
c FMTNUM See main comments in SPLOT.
c LENTXT Length of various strings in FMTNUM and TXTDEF.
c
c ************************* Usage of internal variables ****************
c
c ALIGN  Alignment for the label -- passed into SPLOT4.
c C      Temporary character*1 variable.
c DOL    =.true. if a "$" has been output, =.false. otherwise.
c DIG    Character string for storage of digits.
c EPS1   1 + 4 * machine eps. -- Used to avoid some round off problems.
c FMTSAV Saved value of string used to define the last format.
c HADJ   Used to adjust for different hoizontal positioning when testing
c        for overlap of numeric labels and drawing opaque boxes.
c I      Temorary index.
c INTVAL Equivalenced to: LEXP, NPTSIZ, MINDIG, NAFTU, LZERO
c K      Temorary index.
c KTEN   integer part of log10(|VAL|).  Also used for option values.
c LEXP   Amount to bias in favor of exponent.  > 0 favors exponents, <
c        discourages them.  LEXP = 4 always uses exponents.
c KASE   abs(IKASE)
c LKASE  Last value for KASE
c NTEXT*  Index of last character deposited in TEXT.
c LTEXTF Length of the last format def. processed.  -1 initially.
c LZERO  Number of digits that must precede the decimal point.
c MAXDIG Maximum number of digits printed.
c MINDIG Minimum number of digits that need to be output.
c NAFT   Number of digits required after the decimal point
c NCHAR0 Integer value associated with a '0', i.e. ichar('0').
c NDIG   Number of characters stored in DIG.
c NEEDD  Is .true. if the number must contain a decimal point.
c NLBND  Lower bounds for options: X, F, D, A, and B.  These options
c        define:
c    .   Always print a decimal point.
c   Fn    Fontsize in points.
c   Dn    Number of significant digits which must be printed.
c   An    Number of digits which are required after the decimal point.
c   Bn    Number of digits which are required before the decimal point,
c   Xn    0 < n < 10,  bias for selecting the exponent notation.  If n
c        is not zero, it is replaced with n-5.  The exponent notation is
c        used if there are 4-(final value of n) or more zeros that are
c        serving as place holders, else the usual format is used.  Note
c        that with an input n of 9, which is converted to n=4, there
c        will always be at least 0 zeros, and exponent notation is used.
c NPTSIZ Default point size for this kind of label.
c NUBND  Upper bounds for options: X, F, D, A, and B.
c OVLAP  Estimated right end of last number with KASE = 1, 2, or 5.
c PTSIZ  Real value of NPTSIZ.
c TEXT   The final output TEXT sent to SPLOT4.
c TLENH  Estimated space in units of space required by a single digit.
c        Later the horizontal space required in points.
c TLENV  Estimated vertical space in points.
c V      Initially VAL, then |V|, then contains the tail of V, i.e.
c        digits left to be output.
c
c ************************ Variable Declarations ***********************
c
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c
c         Parameter pointers for integers in IP.
      integer NEXT, INTERP, LCOOX,LCOOY,LXLINE,LXBORD,LYBORD, LTYPE,
     1  KSET, LTANNO, LPEN, NBORD, LYDIM, LNY, LDEBUG,
     2  LASTIP
      parameter (NEXT=1, INTERP=2, LCOOX=3, LCOOY=4, LXLINE=5,
     1  LXBORD=6, LYBORD=7, LTYPE=8, KSET=9, LTANNO=10, LPEN=13,
     2  NBORD=14, LYDIM=15, LNY=16, LDEBUG=18, LASTIP=LDEBUG)
c          Parameter pointers for floats in FP.
      integer LARROW,LWIDTH,LWIDRE,LBAD,LVALS,LXYSIZ,LASTFP,LFDAT
      parameter (LARROW=1, LWIDTH=2, LWIDRE=6, LBAD=7,
     1  LVALS=9, LXYSIZ=LVALS+5, LASTFP=LXYSIZ+2,LFDAT=LBAD)
c          Parameter for various sizes.
      integer LBNDC, LBNDF, LBNDP, LBNDT, MAXSET
      parameter (LBNDC=128, LBNDF=32, LBNDP=4, LBNDT=64, MAXSET=20)
      real             BORLOC(6), FILL(19), FP(LASTFP), OVLAP,
     1  PHYUSE(2,2), SETLIM(2,2), TLENH, TLENV, TICKS(4,6), TOPTS,
     2  VHLEN(2), XYBASE(MAXSET), XYLIM(2,MAXSET), XYU2PF(MAXSET)
      integer IERR1, IERR2, IERR3, IERR4, IOP1, IP(LASTIP), JSET(2),
     1  LENCAP(6), LENTXT(3,18), MANNO, MBORD(8,6), MFILL(4), NTEXT,
     2  NXYLIM(MAXSET)
      logical  KLIP(MAXSET), NOOUT, OPAQUE
      common / SPLOTB / BORLOC, FILL, FP, OVLAP, PHYUSE, SETLIM, TICKS,
     1  TLENH, TLENV, TOPTS, VHLEN, XYBASE, XYLIM, XYU2PF, IERR1,
     2  IERR2, IERR3, IERR4, IOP1, IP, JSET, LENCAP, LENTXT, MANNO,
     3  MBORD, NTEXT, NXYLIM, KLIP, MFILL, NOOUT, OPAQUE
c
      character FMTNUM(17)*(LBNDF), CAPTIO(6)*(LBNDC), POS*68, TEXT*280,
     1    TXTDEF(18)*(LBNDT)
      common / SPLOTC / FMTNUM, CAPTIO, POS, TEXT, TXTDEF
      save /SPLOTB/, /SPLOTC/
c Locals
      real             VAL, XYPOS(2)
      integer   IKASE
      character ALIGN*2
      external  R1MACH
      real             R1MACH
      real             EPS1, HADJ(3), PTSIZ, V
      integer   I, INTVAL(5), K, KASE, KTEN, LEXP, LKASE, LTEXTF, LZERO,
     1   MAXDIG,  MINDIG, NAFT, NCHAR0, NDIG, NLBND(5), NPTSIZ, NUBND(5)
      logical DOL, NEEDD
      character C, DIG*40, FMTSAV*20
      equivalence (INTVAL(1), LEXP), (INTVAL(2), NPTSIZ),
     1  (INTVAL(3), MINDIG), (INTVAL(4), NAFT), (INTVAL(5), LZERO)
      save EPS1, LTEXTF, LKASE, MAXDIG, NCHAR0, NEEDD
c Save statement below, instead of just putting INTVAL above, is to get
c around a bug in the HP Exepmlar Fortran 77 compiler.
      save LEXP, NPTSIZ, MINDIG, NAFT, LZERO
c
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character BTIMES*7
C++ CODE for ~.C. is active
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
c
      parameter (BTIMES=BSLASH//'times ')
C++ CODE for .C. is inactive
C      parameter (BTIMES='\times ')
C++END
c
      data LTEXTF, LKASE / -2, -1 /
c                  X   F   D   A  B
      data NLBND / 0,  5,  1,  0, 0 /
      data NUBND / 9, 30, 20, 20, 1 /
c                   l     c     r
      data HADJ / 0.E0, .5E0, 1.E0 /
c
c ********************** Start of Executable Code **********************
c
c     Alignment is in ALIGN
      KASE = abs(IKASE)
C%%   memcpy(align, splotc.pos+kase*4 - 4, (size_t)2);
      ALIGN = POS(4*KASE-3:4*KASE-2)
c                    Take care of the format.
      if (LTEXTF .eq. LENTXT(1, KASE)) go to 100
      if (LTEXTF .eq. -2) then
c                             Get environmental parameters
         V = R1MACH(4)
         MAXDIG = -log10(V)
         EPS1 = 1.E0 + 8.E0 * V
         NCHAR0 = ichar('0')
      end if
c                         Process the format
      LTEXTF = LENTXT(1, KASE)
c                         Set the default values.
      LZERO = 0
      LEXP = 0
      MINDIG = 1
      NAFT = 0
      NPTSIZ = 9
      NEEDD = .false.
      if (LTEXTF .gt. 0) then
C%%      memcpy(fmtsav, splotc.fmtnum[kase-1], (size_t)ltextf);
         FMTSAV(1:LTEXTF) = FMTNUM(KASE)(1:LTEXTF)
         K = 0
   20    K = K + 1
   30    if (K .lt. LTEXTF) then
c                      12345678901
            I = index('.XxFfDdAaBb', FMTSAV(K:K))
            if (I .ne. 0) then
               if (I .eq. 1) then
                  NEEDD = .true.
                  go to 20
               end if
c                      Get the following number
               KTEN = 0
   40          K = K + 1
               if (K .le. LTEXTF) then
                  C = FMTSAV(K:K)
                  if ((C .ge. '0') .and. (C .le. '9')) then
                     KTEN = 10*KTEN + ichar(C) - NCHAR0
                     go to 40
                  end if
               end if
               if (KTEN .ne. 0) then
c                              Want something other than the default.
                  I = I / 2
                  if ((KTEN.lt.NLBND(I)) .or. (KTEN.gt.NUBND(I))) then
c             Print error message, ignore the option
c                                             Format number out of range
                     call SPLOTE(4, XYPOS, ' ')
                  else
                     INTVAL(I) = KTEN
                     if (I .eq. 1) LEXP = KTEN - 5
                  end if
               end if
               go to 30
            else
c               Unknown format specification
               IERR1 = K
C%%            splote( 5, xypos, fmtsav );
               call SPLOTE(5, XYPOS, FMTSAV(1:K))
            end if
         end if
      end if

c          Convert value to string
  100 TLENH = 0.E0
      V = VAL
      DOL = .false.
      NTEXT = 0
      if (IKASE .lt. 0) then
         NDIG = 1
         KTEN = nint(V)
         DIG(1:1) = '1'
      else
         if (V .eq. 0.E0) then
            KTEN = 0
         else
            if (V .lt. 0.E0) then
c                             Output the "-" sign
               NTEXT = NTEXT + 2
               TEXT(NTEXT-1:NTEXT) = '$-'
               TLENH = 1.2E0
               DOL = .true.
               V = -V
            end if
c Boost up a tiny bit so things close to integers come out as integers.
            V = EPS1 * V
            KTEN = log10(V)
            if (V .lt. 1.E0) KTEN = KTEN - 1
         end if
         V = V * 10.E0 ** (-KTEN)
         NDIG = 0
  120    if (NDIG .lt. MINDIG) then
  130       NDIG = NDIG + 1
            DIG(NDIG:NDIG) = char(NCHAR0 + int(V))
            V = 10.E0 * mod(V, 1.E0)
            if ((V .gt. 1.E-2) .and. (NDIG .lt. MAXDIG)) go to 130
            if (KTEN - NDIG .le. 2 - LEXP) then
c                NDIG - KTEN - 1  is number of digits after the decimal.
               if (NDIG - KTEN .le. NAFT) go to 120
            end if
         end if
      end if
c At this point the number requires NDIG significant digits.
      if ((KTEN .lt. -3 + LEXP) .or. (KTEN - NDIG .gt. 2 - LEXP)) then
c                                           Use the exponent form
         if (.not. DOL) then
            DOL = .true.
            NTEXT = NTEXT + 1
            TEXT(NTEXT:NTEXT) = '$'
         end if
         if ((NDIG .ne. 1) .or. (DIG(1:1) .ne. '1')) then
            NTEXT = NTEXT + 1
            TLENH = TLENH + real(NDIG)
            TEXT(NTEXT:NTEXT) = DIG(1:1)
            if (NDIG .gt. 1) then
               TLENH = TLENH + .4E0
               TEXT(NTEXT+1:NTEXT+1) = '.'
C%%           memcpy(splotc.text+splotb.ntext+1,dig+1,(size_t)(ndig-1));
               TEXT(NTEXT+2:NTEXT+NDIG) = DIG(2:NDIG)
            end if
            NTEXT = NTEXT + NDIG + 7
            TEXT(NTEXT-6:NTEXT) = BTIMES
            TLENH = TLENH + 1.4
         end if
         TEXT(NTEXT+1:NTEXT+4) = '10^{'
         TLENH = TLENH + 2.E0
         NTEXT = NTEXT + 4
         if (KTEN .lt. 0) then
            NTEXT = NTEXT + 1
            TEXT(NTEXT:NTEXT) = '-'
            KTEN = -KTEN
            TLENH = TLENH + 1.2E0
         end if
         K = 10
  140    if (K .le. KTEN) then
            K = 10 * K
            go to 140
         end if
  150    K = K / 10
         if (K .ne. 0) then
c                          Numbers on the exponent.
            TLENH = TLENH + .75E0
            I = KTEN / K
            NTEXT = NTEXT + 1
            TEXT(NTEXT:NTEXT) = char(NCHAR0 + I)
            KTEN = KTEN - 10 * I
            go to 150
         end if
         NTEXT = NTEXT + 1
         TEXT(NTEXT:NTEXT) = '}'
      else
c                                             Numbers without exponents
         if (KTEN .lt. 0) then
c                                             Number is < 1
c                    K introduced here due to bug in Lahey compiler.
            do 160 K = NTEXT+1, NTEXT + LZERO
               TLENH = TLENH + 1.E0
               TEXT(K:K) = '0'
  160       continue
            NTEXT = NTEXT + LZERO + 1
            TEXT(NTEXT:NTEXT) = '.'
            TLENH = TLENH + .4E0
            do 170 K = NTEXT+1, NTEXT - KTEN - 1
               TLENH = TLENH + 1.E0
               TEXT(K:K) = '0'
  170       continue
            NTEXT = NTEXT - KTEN
C%%         memcpy(splotc.text+splotb.ntext-1, dig, (size_t)ndig);
            TEXT(NTEXT:NTEXT+NDIG-1) = DIG(1:NDIG)
            NTEXT = NTEXT + NDIG - 1
         else
c                                             Number is >= 1.
            K = min(NDIG, KTEN+1)
C%%         memcpy(splotc.text+splotb.ntext, dig, (size_t)k);
            TEXT(NTEXT+1:NTEXT+K) = DIG(1:K)
            NTEXT = NTEXT + K
            TLENH = TLENH + real(K)
            if (NDIG .gt. K) then
               NTEXT = NTEXT + 1
               TLENH = TLENH + .4E0 + real(NDIG - K)
               TEXT(NTEXT:NTEXT) = '.'
C%%         memcpy(splotc.text+splotb.ntext, dig+k, (size_t)(ndig-k));
               TEXT(NTEXT+1:NTEXT+NDIG-K) = DIG(K+1:NDIG)
               NTEXT = NTEXT+NDIG-K
            else
               if (KTEN .ge. K) then
                  do 180 NTEXT = NTEXT, NTEXT + KTEN - K
                     TLENH = TLENH + 1.E0
                     TEXT(NTEXT+1:NTEXT+1) = '0'
  180             continue
               end if
               if (NEEDD) then
                  TLENH = TLENH + .4E0
                  NTEXT = NTEXT + 1
                  TEXT(NTEXT:NTEXT) = '.'
               end if
            end if
         end if
      end if
      if (DOL) then
         NTEXT = NTEXT + 1
         TEXT(NTEXT:NTEXT) = '$'
      end if

c     Convert TLENH to physical distance
      PTSIZ = NPTSIZ
      TLENH = .5E0 * TLENH * PTSIZ
      TLENV = PTSIZ
      if ((KASE .le. 2) .or. (KASE .eq. 5)) then
         if (KASE .eq. LKASE) then
c                               Check for overlap
            if (mod(LKASE, 2) .eq. 1) then
               K = index('lLcCrR', ALIGN(2:2))
               if (K .ne. 0) then
                  K = (K + 1) / 2
                  if (OVLAP .gt. XYPOS(1) - TLENH * HADJ(K)) return
c                               Set the new overlap
                  OVLAP = XYPOS(1) + HADJ(4-K) * TLENH
               end if
            end if
         end if
      end if
      if (NOOUT) return
      call SPLOTT( KASE, XYPOS)
      LKASE = KASE
      return
      end

      subroutine SPLOTT( KASE, XYPOS)
c Copyright (c) 1997, California Institute of Technology. U.S.
c Government Sponsorship under NASA Contract NAS7-1260 is acknowledged.
c
c For output ot text, and getting size of such output.
c
c ************************* Calling Sequence variables *****************
c
c KASE   1-4 for bottom, left, top,right borders, 5 and 6 for x and y
c   axis, 8 for words, 10-15 for captions, 16 for output text.

c        Indices, 1-16, are for: Borders (bottom, left, top, right),
c   x-axis, y-axis, word alignment (e.g. for option 14), number
c   formatting for option 15, Captions (as for borders), alignment
c   rule for option 16.


c XYPOS  Gives (x,y), the position for the text in physical coordinates.
c TEXT   The Text to output.
c
c ************************* Usage of internal variables ****************
c
c ADJ1   Used for first point ajustment on box.
c ADJ2   Used for second point ajustment on box.
c ADJH   Used to get index for horizontal adjustment of boxes.
c ADJV   Used to get index for vertical adjustment of boxes.
c FMTSAV Saved value for the last format specification.
c GETSIZ Logical variable that is .true. if need to get size.
c HLEN   Largest horizontal space required if not stacked, and the final
c    value required in any case.
c HLENS  Largest horizontal space required for various vertical cases.
c    Also used as a temp.
c HLSAV  Horizontal space required at the start of a "{" or "$" group.
c LASTL  Length of text for the last format specification.
c LFILL2 Used to pass a length 3 array with a two in the first position,
c    to SPLOT7.
c OUTTXT Final output form (aside from prefix and postfix, which are
c    added in SPLOT
c PTSIZ  Gives the size in points for the text being output.
c VERT   Logical variable that is true if "stacking" the text.
c VLEN   Vertical space required so far.
c
c *************************** Variable Declarations ********************
c
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c
c         Parameter pointers for integers in IP.
      integer NEXT, INTERP, LCOOX,LCOOY,LXLINE,LXBORD,LYBORD, LTYPE,
     1  KSET, LTANNO, LPEN, NBORD, LYDIM, LNY, LDEBUG,
     2  LASTIP
      parameter (NEXT=1, INTERP=2, LCOOX=3, LCOOY=4, LXLINE=5,
     1  LXBORD=6, LYBORD=7, LTYPE=8, KSET=9, LTANNO=10, LPEN=13,
     2  NBORD=14, LYDIM=15, LNY=16, LDEBUG=18, LASTIP=LDEBUG)
c          Parameter pointers for floats in FP.
      integer LARROW,LWIDTH,LWIDRE,LBAD,LVALS,LXYSIZ,LASTFP,LFDAT
      parameter (LARROW=1, LWIDTH=2, LWIDRE=6, LBAD=7,
     1  LVALS=9, LXYSIZ=LVALS+5, LASTFP=LXYSIZ+2,LFDAT=LBAD)
c          Parameter for various sizes.
      integer LBNDC, LBNDF, LBNDP, LBNDT, MAXSET
      parameter (LBNDC=128, LBNDF=32, LBNDP=4, LBNDT=64, MAXSET=20)
      real             BORLOC(6), FILL(19), FP(LASTFP), OVLAP,
     1  PHYUSE(2,2), SETLIM(2,2), TLENH, TLENV, TICKS(4,6), TOPTS,
     2  VHLEN(2), XYBASE(MAXSET), XYLIM(2,MAXSET), XYU2PF(MAXSET)
      integer IERR1, IERR2, IERR3, IERR4, IOP1, IP(LASTIP), JSET(2),
     1  LENCAP(6), LENTXT(3,18), MANNO, MBORD(8,6), MFILL(4), NTEXT,
     2  NXYLIM(MAXSET)
      logical  KLIP(MAXSET), NOOUT, OPAQUE
      common / SPLOTB / BORLOC, FILL, FP, OVLAP, PHYUSE, SETLIM, TICKS,
     1  TLENH, TLENV, TOPTS, VHLEN, XYBASE, XYLIM, XYU2PF, IERR1,
     2  IERR2, IERR3, IERR4, IOP1, IP, JSET, LENCAP, LENTXT, MANNO,
     3  MBORD, NTEXT, NXYLIM, KLIP, MFILL, NOOUT, OPAQUE
c
      character FMTNUM(17)*(LBNDF), CAPTIO(6)*(LBNDC), POS*68, TEXT*280,
     1    TXTDEF(18)*(LBNDT)
      common / SPLOTC / FMTNUM, CAPTIO, POS, TEXT, TXTDEF
      save /SPLOTB/, /SPLOTC/
c Locals
      integer KASE
      real             XYPOS(2)
c
      logical GETSIZ, VERT
      character ADJH*3, ADJV*3, C, FMTSAV*(LBNDF), OUTTXT*256
      integer I, IAX, J, K, L, LASTL, LFILL2(3), LPAR
      real             ADJ1(3), ADJ2(3), HLEN, HLENS, HLSAV, PTSIZ, TP1,
     1  TP2, VLEN
      save LASTL, FMTSAV, PTSIZ
c
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character BSHORT*16
      character BSLAS2*2
      character BSMALL*7
C++ CODE for ~.C. is active
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
c
      parameter (BSHORT=BSLASH//'shortstack[  ]{')
      parameter (BSLAS2=BSLAS1(1:1)//BSLAS1(1:1))
      parameter (BSMALL=BSLASH//'small ')
C++ CODE for .C. is inactive
C      character BSLASH
C      parameter (BSLASH='\\')
C      parameter (BSHORT='\shortstack[  ]{')
C      parameter (BSLAS2='\\')
C      parameter (BSMALL='\small ')
C++END
c
      data LASTL, LFILL2 / -2, 2, 0, 0 /
      data ADJV, ADJH / 'tcb', 'rcl' /
      data ADJ1 / -1.E0, -.5E0, 0.E0 /
      data ADJ2 / 0.E0, .5E0, 1.E0 /
c
c *************************** Start of Executable Code *****************
c
      IAX = 2 - mod(KASE, 2)
      VERT = (POS(4*KASE-1:4*KASE-1) .eq. 's')
      GETSIZ = NOOUT .or. OPAQUE .or. (MANNO .ne. 0)
      if (GETSIZ .or. VERT) then
         if (GETSIZ) then
            VERT = VERT .or. ((IAX .eq. 2) .and. (MANNO .eq. 0) .and.
     1         (POS(4*KASE-3:4*KASE-3) .eq. 'c'))
            L = LENTXT(1, KASE)
            if (L .eq. LASTL) then
c                     Format hasn't changed
C%%  if (memcmp(splotc.fmtnum[kase-1],fmtsav,(size_t)l)==0) goto L_60;
               if (FMTNUM(KASE)(1:L) .eq. FMTSAV(1:L)) go to 60
            end if
            LASTL = L
            PTSIZ = 9.E0
            if (L .gt. 0) then
C%%            memcpy(fmtsav, splotc.fmtnum[kase-1], (size_t)l);
               FMTSAV(1:L) = FMTNUM(KASE)(1:L)
               K = 0
   20          if (K .lt. L) then
                  if ((FMTNUM(KASE)(K:K) .eq. 'F') .or.
     1               (FMTNUM(KASE)(K:K).eq.'f')) then
c                      Get the following number
                     J = 0
   40                K = K + 1
                     if (K .le. LASTL) then
                        C = FMTSAV(K:K)
                        if ((C .ge. '0') .and. (C .le. '9')) then
                           J = 10*J + ichar(C) - ichar('0')
                           go to 40
                        end if
                     end if
                     if ((J.ge.5) .and. (J.le.30)) PTSIZ = J
                  end if
                  K = K + 1
                  go to 20
               end if
            end if
         end if
c        Accumlate sizes and text
   60    VLEN = 0.E0
         HLEN = 0.E0
         HLENS = 0.E0
         LPAR = 0
         I = 0
         J = LBNDT-1
         if (VERT) then
            J = LBNDT + 16
            OUTTXT(LBNDT:J) = BSHORT
         end if
C%%      memcpy(outtxt+j-5, splotc.pos+kase*4-4, (size_t)2);
         OUTTXT(J-4:J-3) = POS(4*KASE-3:4*KASE-2)
   80    I = I + 1
         if (I .le. NTEXT) then
            C = TEXT(I:I)
            if (C .eq. BSLASH) then
               J = J + 1
               OUTTXT(J:J) = BSLASH
c                          Skip '\' commands.
   90          I = I + 1
               C = TEXT(I:I)
               if (((C .ge. 'a') .and. (C .le. 'z')) .or.
     1             ((C .ge. 'A') .and. (C .le. 'Z'))) then
                  J = J + 1
                  OUTTXT(J:J) = C
                  go to 90
               end if
            end if
            if (C .eq. '{') then
               LPAR = LPAR + 1
               if (LPAR .eq. 1) then
                  HLSAV = HLEN
               end if
               go to 100
            else if (C .eq. '}') then
               LPAR = LPAR - 1
               if (LPAR .eq. 0) HLENS = max(HLENS, HLEN - HLSAV)
               go to 100
c               if (LPAR .ne. 0) go to 100
c               HLENS = max(HLENS, HLEN - HLSAV)
c               go to 80
            else if (C .eq. '$') then
               if (LPAR .ge. 100) then
                  LPAR = LPAR - 100
               else
                  if (LPAR .eq. 0) HLSAV = HLEN
                  LPAR = LPAR + 100
               end if
               if (LPAR .eq. 0) HLENS = max(HLENS, HLEN - HLSAV)
               go to 100
            else if (C .eq. '^') then
               HLEN = HLEN - .3E0
               go to 100
            end if
            if (VERT .and. (LPAR .eq. 0)) then
               J = J + 2
               OUTTXT(J-1:J) = BSLAS2
               VLEN = VLEN + 1.E0
            end if
            HLEN = HLEN + 1.E0
  100       J = J + 1
            OUTTXT(J:J) = C
            go to 80
         end if
         if (LPAR .ne. 0) then
c                Error -- Caption doesn''t have balanced {...} or $...$.
            call SPLOTE(6, XYPOS, TEXT)
         end if
         if (NOOUT) then
            if ((IAX.eq.2) .and. (POS(4*KASE:4*KASE).eq.'.')) then
               if (HLENS .lt. HLEN - 2) POS(4*KASE-1:4*KASE) = 'sc'
            end if
            if (POS(4*KASE-1:4*KASE-1).eq.'s') then
               HLEN = HLENS
            else
               VLEN = 1
            end if
            VHLEN(1) = PTSIZ * VLEN
            VHLEN(2) = .5E0 * PTSIZ * HLEN
            return
         end if
         if (VERT) then
            J = J + 1
            OUTTXT(J:J) = '}'
         end if
         if (MANNO .ne. 0) then
c                          Some kind of annotation.
            K = index(ADJV, POS(4*KASE-3:4*KASE-3))
            L = index(ADJH, POS(4*KASE-2:4*KASE-2))
            HLENS = .5E0 * PTSIZ * HLEN
            TP1 = XYPOS(1)+ADJ1(L)*HLENS-.5E0
            TP2 = XYPOS(1)+ADJ2(L)*HLENS+.5E0
            if (MANNO .gt. 0) then
               if ((TP1 .lt. BORLOC(2)) .or. (TP2 .gt. BORLOC(4)))
     1            call SPLOTE(7, XYPOS, TEXT)
            else
               if ((TP1.ge.0.E0) .and. (TP1.lt.BORLOC(2) - MBORD(8,5)))
     1           MBORD(8,5) = BORLOC(2) - TP1
               if ((TP2.ge.0.E0) .and. (TP2.gt.BORLOC(4) + MBORD(8,6)))
     1           MBORD(8,6) = TP2 - BORLOC(4)
            end if
         end if
         if (OPAQUE) then
            I = 1
            call SPLOT7(I, LFILL2, FILL)
            call SPLOT5(XYPOS(1)+ADJ1(L)*HLENS-.5E0,
     1         XYPOS(2)+PTSIZ*ADJ1(K), XYPOS(1)+ADJ2(L)*HLENS+.5E0,
     3         XYPOS(2)+PTSIZ*ADJ2(K))
         end if
      else
c                 Just copy the text -- easy case.
         J = LBNDT
C%%      memcpy(outtxt+j-1, splotc.text, (size_t)splotb.ntext);
         OUTTXT(J:J+NTEXT-1) = TEXT(1:NTEXT)
         J = J + NTEXT-1
      end if
c                Take care of prefix and postfix.
      I = LBNDT
      L = LENTXT(2, KASE)
      K = LENTXT(3, KASE)
      if (K .lt. 0) then
         if ((IP(LTYPE) .eq. 0) .and. (K .eq. -1)) then
c                              The default prefix for LaTeX.
             OUTTXT(I-7:I-1) = BSMALL
             I = I - 7
         end if
      else
         if (L .gt. 0) then
c                              Prefix is specified.
C%%         memcpy(outtxt+i-l-1, splotc.txtdef[kase-1], (size_t)l);
            OUTTXT(I-L:I) = TXTDEF(KASE)(1:L)
            I = I - L
         end if
         if (K .ne. 0) then
c                              Postfix is specified.
C%%         memcpy(outtxt+j, splotc.txtdef[kase-1]+l+1,(size_t)(k-l-l));
            OUTTXT(J+1:J+K-L-1) = TXTDEF(KASE)(L+2:K)
            J = J + K - L - 1
         end if
      end if
c Output the text
C%%   *(outtxt+j) = '\0';
C%%   splot4( Xypos[1], Xypos[2], outtxt+i-1, splotc.pos+kase*4-4);
      call SPLOT4(XYPOS(1),XYPOS(2),OUTTXT(I:J),POS(4*KASE-3:4*KASE-2))
      return
      end

      subroutine SPLOTR(XY, KSYMB, KX, KY)
c     Gets XY converted for call to SPLOTS (Symbols, error bars, arrows)
      real             XY(*)
      integer KSYMB, KX, KY
c
      integer K
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c
c         Parameter pointers for integers in IP.
      integer NEXT, INTERP, LCOOX,LCOOY,LXLINE,LXBORD,LYBORD, LTYPE,
     1  KSET, LTANNO, LPEN, NBORD, LYDIM, LNY, LDEBUG,
     2  LASTIP
      parameter (NEXT=1, INTERP=2, LCOOX=3, LCOOY=4, LXLINE=5,
     1  LXBORD=6, LYBORD=7, LTYPE=8, KSET=9, LTANNO=10, LPEN=13,
     2  NBORD=14, LYDIM=15, LNY=16, LDEBUG=18, LASTIP=LDEBUG)
c          Parameter pointers for floats in FP.
      integer LARROW,LWIDTH,LWIDRE,LBAD,LVALS,LXYSIZ,LASTFP,LFDAT
      parameter (LARROW=1, LWIDTH=2, LWIDRE=6, LBAD=7,
     1  LVALS=9, LXYSIZ=LVALS+5, LASTFP=LXYSIZ+2,LFDAT=LBAD)
c          Parameter for various sizes.
      integer LBNDC, LBNDF, LBNDP, LBNDT, MAXSET
      parameter (LBNDC=128, LBNDF=32, LBNDP=4, LBNDT=64, MAXSET=20)
      real             BORLOC(6), FILL(19), FP(LASTFP), OVLAP,
     1  PHYUSE(2,2), SETLIM(2,2), TLENH, TLENV, TICKS(4,6), TOPTS,
     2  VHLEN(2), XYBASE(MAXSET), XYLIM(2,MAXSET), XYU2PF(MAXSET)
      integer IERR1, IERR2, IERR3, IERR4, IOP1, IP(LASTIP), JSET(2),
     1  LENCAP(6), LENTXT(3,18), MANNO, MBORD(8,6), MFILL(4), NTEXT,
     2  NXYLIM(MAXSET)
      logical  KLIP(MAXSET), NOOUT, OPAQUE
      common / SPLOTB / BORLOC, FILL, FP, OVLAP, PHYUSE, SETLIM, TICKS,
     1  TLENH, TLENV, TOPTS, VHLEN, XYBASE, XYLIM, XYU2PF, IERR1,
     2  IERR2, IERR3, IERR4, IOP1, IP, JSET, LENCAP, LENTXT, MANNO,
     3  MBORD, NTEXT, NXYLIM, KLIP, MFILL, NOOUT, OPAQUE
c
      character FMTNUM(17)*(LBNDF), CAPTIO(6)*(LBNDC), POS*68, TEXT*280,
     1    TXTDEF(18)*(LBNDT)
      common / SPLOTC / FMTNUM, CAPTIO, POS, TEXT, TXTDEF
      save /SPLOTB/, /SPLOTC/
c
      K = mod(abs(KSYMB), 10)
      if (K .eq. 1) then
         K = mod(abs(KSYMB)/10, 10)
         if (K .le. 1) then
            XY(6) = XY(2) + XY(3)
            if (K .eq. 1) XY(2) = XY(2) + XY(4)
            XY(4) = XY(2) - XY(3)
            XY(5) = XY(1)
            XY(3) = XY(1)
            K = 3
         else
            XY(4) = XY(2) + XY(4)
            XY(3) = XY(1) + XY(3)
         end if
      else
         K = 1
      end if
      do 100 K = 1, 2*K, 2
         if (KSYMB .ge. 0) then
c                   Convert to physical and plot
            XY(K) = XYBASE(KX) + XYU2PF(KX) * XY(K)
            XY(K+1) = XYBASE(KY) + XYU2PF(KY) * XY(K+1)
         else
c       Convert to points.
           XY(K) = TOPTS * XY(K)
           XY(K+1) = TOPTS * XY(K+1)
         end if
  100 continue
      call SPLOTS(XY, abs(KSYMB))
      return
      end

C++ CODE for ~.C. is active
      subroutine SPLOTU (NEWU, FILNAM)
c Get an unused unit number, open it for unformatted sequential scratch
c usage if FILNAM is ' ', else open for formatted sequential output.
      integer NEWU
      character FILNAM*(*)
      real             SPACE(1)
      logical OPENED
      integer IORES, NEXTU
      save NEXTU
c Common  Variables
c
c         Parameter pointers for integers in IP.
      integer NEXT, INTERP, LCOOX,LCOOY,LXLINE,LXBORD,LYBORD, LTYPE,
     1  KSET, LTANNO, LPEN, NBORD, LYDIM, LNY, LDEBUG,
     2  LASTIP
      parameter (NEXT=1, INTERP=2, LCOOX=3, LCOOY=4, LXLINE=5,
     1  LXBORD=6, LYBORD=7, LTYPE=8, KSET=9, LTANNO=10, LPEN=13,
     2  NBORD=14, LYDIM=15, LNY=16, LDEBUG=18, LASTIP=LDEBUG)
c          Parameter pointers for floats in FP.
      integer LARROW,LWIDTH,LWIDRE,LBAD,LVALS,LXYSIZ,LASTFP,LFDAT
      parameter (LARROW=1, LWIDTH=2, LWIDRE=6, LBAD=7,
     1  LVALS=9, LXYSIZ=LVALS+5, LASTFP=LXYSIZ+2,LFDAT=LBAD)
c          Parameter for various sizes.
      integer LBNDC, LBNDF, LBNDP, LBNDT, MAXSET
      parameter (LBNDC=128, LBNDF=32, LBNDP=4, LBNDT=64, MAXSET=20)
      real             BORLOC(6), FILL(19), FP(LASTFP), OVLAP,
     1  PHYUSE(2,2), SETLIM(2,2), TLENH, TLENV, TICKS(4,6), TOPTS,
     2  VHLEN(2), XYBASE(MAXSET), XYLIM(2,MAXSET), XYU2PF(MAXSET)
      integer IERR1, IERR2, IERR3, IERR4, IOP1, IP(LASTIP), JSET(2),
     1  LENCAP(6), LENTXT(3,18), MANNO, MBORD(8,6), MFILL(4), NTEXT,
     2  NXYLIM(MAXSET)
      logical  KLIP(MAXSET), NOOUT, OPAQUE
      common / SPLOTB / BORLOC, FILL, FP, OVLAP, PHYUSE, SETLIM, TICKS,
     1  TLENH, TLENV, TOPTS, VHLEN, XYBASE, XYLIM, XYU2PF, IERR1,
     2  IERR2, IERR3, IERR4, IOP1, IP, JSET, LENCAP, LENTXT, MANNO,
     3  MBORD, NTEXT, NXYLIM, KLIP, MFILL, NOOUT, OPAQUE
c
      data NEXTU / 10 /

c
      do 100 NEWU = NEXTU, 100
          inquire (unit=NEWU, opened=OPENED)
          if (.not. OPENED) then
            if (FILNAM(1:1) .eq. ' ') then
               open (unit=NEWU, status='SCRATCH', access='SEQUENTIAL'
     1,           form='UNFORMATTED', iostat=IORES)
               if (IORES .eq. 0) go to 300
               close (unit=NEWU)
            else
              open (unit=NEWU, FILE=FILNAM, status='UNKNOWN'
     1,       form='FORMATTED', access='SEQUENTIAL', iostat=IORES
     2,       err=200)
              go to 300
            end if
          end if
  100 continue
c          Unable to find unused I/O unit number in 10..100
      call SPLOTE(34, SPACE, ' ')
      return
c                  Unable to open output file
  200 IERR1 = len(FILNAM)
      call SPLOTE(35, SPACE, FILNAM)
      return
c                 "Success" exit
  300 NEXTU = NEWU + 1
      return
C++ END
C%%
      end

      subroutine SPLOT0
c Copyright (c) 1996, California Institute of Technology. U.S.
c Government Sponsorship under NASA Contract NAS7-1260 is acknowledged.
c>> 1997-01-09 SPLOT0   Krogh Initial code.
C++ Current has HOW=MFPIC
c
c Much modified from earlier code by Van Snyder.
c Most dependencies of the plot package on mfpic are captured in this
c file.  This code was originally in a separate file.  Files combined
c because of problems in C with iofil being external.
c
c Start the plot.
c
c ***************************** Common Block ***************************
c
c ARRLEN If nonzero, next line or curve is to have an arrow on the end.
c    This give the length of the arrow in points.
c PXO, PYO     Origin of logical coordinate system in physical units.
c PXSIZE, PYSIZE Physical X and Y width of the plot, including outward-
c              pointing tick marks.
c IOFIL Unit number used for output to be used for plot device.
c    Temporarily increased by 1000 when want to end one mfpic group and
c    immediately start another.
c IPLOT Defines output, 0 for LaTeX, 1 for TeX.
c KURPEN Rule defining the current pen.  Defined as for P3 of option 3.
c   KURPEN = t + 10*(w + 100*(L1 + 100*L2)), where t is 0, 1, or 2 for
c   solid, dotted, or dashed lines.  t = 3 or 4 is as for 1 or 2, except
c   L1 is given in deci-points instead of points, and t = 5-8, is as for
c   1-4, except L2 if in deci-points instead of in points.  w is the
c   width of the line in decipoints, L1 and L2 are not used for solid
c   lines.  Else L1 is the diameter of the dots or the lenght of the
c   dashes, and L2 is the distance between the dots or dashes.
c LASPEN The last value assigned to KURPEN.
c
c *************************** Internal Variables ***********************
c
c ISTART  Points to place where text in START starts for a give value
c   in IPLOT.  (Only 0 and 1 supported.)
c START   TeX command to write at the beginning -- \begin{mfpic} or
c         \mfpic.
c
c **************************** Variable Declarations *******************
c
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c Locals
      integer ISTART(0:2)
c
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character START*19
C++ CODE for ~.C. is active
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
      character PSTART*19
      parameter (PSTART=BSLASH//'begin{mfpic}'//BSLASH//'mfpic')
      data START / PSTART /
C++ CODE for .C. is inactive
CC                   12345678901234567890
C      data START / '\begin{mfpic}\mfpic' /
C++END
c Data
      data ISTART / 1, 14, 20 /
c
C%%   const char fmt10[] = " %.*s[ 1.0 ]{%9.3f}{%9.3f}{%9.3f}{%9.3f}\n";
   10 format (1x, a ,'[ 1.0 ]',4('{',f9.3,'}'))
c
c *********     Executable Statements     ******************************
C++ CODE for ~.C. is active
      write (IOFIL, 10) START(ISTART(IPLOT):ISTART(IPLOT+1)-1),
     1   -PXO, PXSIZE, -PYO, PYSIZE
C++ CODE for .C. is inactive
c%%   fprintf(iofil, fmt10, (int)(istart[splotd.iplot+1]-
c%%     istart[splotd.iplot]), start+istart[splotd.iplot]-1,
c%%     -splotd.pxo, splotd.pxsize, -splotd.pyo, splotd.pysize );
C++ END
      LASPEN = 50
      return
      end

c==================================================     SPLOT1     =====
      subroutine SPLOT1
c Specify the pen characteristics
c
c **** Variable Definitions (<name*> means variable is in common) ******
c
c ARRLEN* Length of arrow head to be drawn on next curve.
c DASH    Length of dashes
c DASHSP  Length of space between dashes.
c DOTSP   Length of space between dots.
c DOTSZ   Size of the dots.
c IOFIL*  Output unit.
c IT      Type of Line.  Low digit of KURPEN.
c      0     Solid line
c      1     Dashed line
c      2     Dotted line
c     3:4    As for 1:2, except units for the length of the dashes or
c            dots are given in deci-points instead of in points.
c     5:8    As for 1:4, except units for the length of the spaces are
c            in deci-points instead of in points
c KURPEN* A packed integer, giving information on the kind of curve or
c         line to draw, = IT + 10*(PENWID+10*(length or size + 10*(space
c         between dots or dashes))).
c L       Temp., used for the integer resulting from unpacking KURPEN.
c LASPEN  The previous value of KURPEN.
c PENWID  The width of the last line drawn.
c TP1     For temporary storage and to distinguish point/deci-points.
c TP2     For temporary storage and to distinguish point/deci-points.
c
c **************************** Variable Declarations *******************
c
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c Locals
      integer IT, L
      real             DASH, DASHSP, DOTSZ, DOTSP, PENWID, TP1, TP2
      save DASH, DASHSP, DOTSZ, DOTSP, PENWID, IT
      data DASH, DASHSP, DOTSZ, DOTSP, PENWID /4*-1.E0, .5E0/
c
C++ CODE for ~.C. is active
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
      character*(*) BFMT1, BFMT2, BFMT3, BFMT4, BFMT5, BFMT6
      parameter (BFMT1='('''//BSLASH//'arrow[l '',f6.3,''pt]'')',
     1  BFMT2='('''//BSLASH//'pen{'',F6.3,''pt}'')',
     2  BFMT3='( '''//BSLASH//'dashlen='',F6.3,''pt'')',
     3  BFMT4='( '''//BSLASH//'dashspace='',F6.3,''pt'')',
     4  BFMT5='( '''//BSLASH//'dotsize='',F6.3,''pt'')',
     5  BFMT6='( '''//BSLASH//'dotspace='',F6.3,''pt'')')
      character*(*) BDASH, BDOT
      parameter (BDASH='('''//BSLASH//'dashed'')',
     1  BDOT='('''//BSLASH//'dashed'')')

C++ CODE for .C. is inactive
C%%   const char fmt10[] = "\\arrow[l %6.3fpt]\n";
C%%   const char fmt20[] = "\\pen{%6.3fpt}\n";
C%%   const char fmt30[] = " \\dashlen{%6.3fpt}\n";
C%%   const char fmt40[] = " \\dashspace{%6.3fpt}\n";
C%%   const char fmt50[] = " \\dotsize{%6.3fpt}\n";
C%%   const char fmt60[] = " \\dotspace{%6.3fpt}\n";
C++ END
c
c *********     Executable Statements     ******************************
c
      if (KURPEN .eq. LASPEN) go to 100
      LASPEN = KURPEN
      L = LASPEN
      IT = mod(L, 10)
      L = L / 10
      TP1 = real(mod(L, 100)) / 10.E0
      if (TP1 .eq. 0.E0) TP1 = .5E0
C%%   if (tp1 != penwid) fprintf(iofil, fmt20, tp1);
      if (TP1 .ne. PENWID) write(IOFIL, BFMT2) TP1
      PENWID = TP1
      if (TP1 .eq. 0.E0) return
      L = L / 100
      TP1 = real(mod(L, 100))
      TP2 = real(L / 100)
      if (IT .gt. 0) then
         if (IT .gt. 4) then
            IT = IT - 4
            TP2 = TP2 / 10.E0
         end if
         if (IT .gt. 2) then
            IT = IT - 2
            TP1 = TP1 / 10.E0
         end if
         if (IT .eq. 1) then
            if (TP1 .eq. 0.E0) TP1 = 4.E0
            if (TP2 .eq. 0.E0) TP2 = .5E0 * TP1
C%%         if (tp1 != dash) fprintf(iofil, fmt30, tp1);
            if (TP1 .ne. DASH) write(IOFIL, BFMT3) TP1
            DASH = TP1
C%%         if (tp2 != dashsp) fprintf(iofil, fmt40, tp2);
            if (TP2 .ne. DASHSP) write(IOFIL, BFMT4) TP2
            DASHSP = TP2
         else
            if (TP1 .eq. 0.E0) TP1 = 1.5
            if (TP2 .eq. 0.E0) TP2 = .75E0 * TP1
            TP2 = TP2 + TP1
C%%         if (tp1 != dotsz) fprintf(iofil, fmt50, tp1);
            if (TP1 .ne. DOTSZ) write(IOFIL, BFMT5) TP1
            DOTSZ = TP1
C%%         if (tp2 != dotsp) fprintf(iofil, fmt60, tp2);
            if (TP2 .ne. DOTSP) write(IOFIL, BFMT6) TP2
            DOTSP = TP2
         end if
      end if
  100 if (ARRLEN .ne. 0) then
c                          Want an arrow on the next curve.
C%%      fprintf(iofil, fmt10, splotd.arrlen);
         write (IOFIL, BFMT1) ARRLEN
         ARRLEN = 0
      end if
      if (IT .eq. 0) return
      if (IT .eq. 1) then
C%%      fprintf(iofil, "\\dashed\n");
         write(IOFIL, BDASH)
      else
C%%      fprintf(iofil, "\\dotted\n");
         write(IOFIL, BDOT)
      end if
      return
      end

c==================================================     SPLOT2     =====
      subroutine SPLOT2 (X1, Y1, X2, Y2)

c Draw a single straight line from (X1,Y1) to (X2,Y2) in physical
c coordinates.

c IOFIL*  (In common) Gives Fortran I/O unit number for output file
c
      real             X1, Y1, X2, Y2
c
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c
C++ CODE for ~.C. is active
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
      character*(*) BFMT1
      parameter (BFMT1='('' '//BSLASH//
     1  'lines{('',F9.3,'','',F9.3,''),('',F9.3,'','',F9.3,'')}'')')
C++ CODE for .C. is inactive
C%%   const char fmt10[] = " \\lines{(%9.3f,%9.3f),(%9.3f,%9.3f)}\n";
C++ END
c
c *********     Executable Statements     ******************************
c
      call SPLOT1
C%%   fprintf(iofil, fmt10,x1, y1, x2, y2);
      write (IOFIL, BFMT1)  X1, Y1, X2, Y2
      return
      end

c==================================================     SPLOT4     =====
      subroutine SPLOT4 (X, Y, OTEXT, ALIGN)

c Output an annotation at (X,Y) in physical coordinates.

c X, Y    Physical coordinates of the annotation.
c OTEXT   The annotation
c ALIGN   Characters to control alignment.  The first is for vertical
c         alignment, and may be t (top), c (center) or b (bottom).  The
c         second is for horizontal alignment, and may be l (left),
c         r (right) or c (center).  Otherwise, ALIGN is blank.

      real             X, Y
      character ALIGN*2, OTEXT*(*)
c
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c
C++ CODE for ~.C. is active
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
      character*(*) BFMT1, BFMT2
      parameter (BFMT1='('' '//BSLASH//
     1  'tlabel['',A2, '']('', F9.3, '','', F9.3, ''){'', A, ''}'')',
     2  BFMT2='('' '//BSLASH//
     3  'tlabel('', f9.3, '','', f9.3, ''){'', A,''}'')')
C++ CODE for .C. is inactive
C%%   const char fmt10[] = " \\tlabel[%2.2s](%9.3f,%9.3f){%s}\n";
C%%   const char fmt20[] = " \\tlabel(%9.3f,%9.3f){%s}\n";
C++ END
c
c *********     Executable Statements     ******************************
c
C++ CODE for ~.C. is active
      if (ALIGN .ne. '  ') then
        write (IOFIL, BFMT1) ALIGN, X, Y, OTEXT
      else
        write (IOFIL, BFMT2) X, Y, OTEXT
      end if
C++ CODE for .C. is inactive
C%%   if (*align != ' ' || *(align+1) != ' ')
C%%     fprintf(iofil, fmt10, align, x, y, otext);
C%%   else
C%%     fprintf(iofil, fmt20, x, y, otext);
C++ END
      return
      end

c==================================================     SPLOT5     =====
      subroutine SPLOT5 (X1, Y1, X2, Y2)

c Draw a rectangle with corners at (X1,Y1) and (X2,Y2) in physical
c coordinates, with the fill type, and PENWID given.

c (X1,Y1), (X2,Y2)  Physical coordinates of corners of rectangle.

      real             X1, Y1, X2, Y2
c
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c
C++ CODE for ~.C. is active
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
      character*(*) BFMT1
      parameter (BFMT1='('' '//BSLASH//
     1  'rect{('',F9.3,'','',F9.3,''),('',F9.3,'','',F9.3,'')}'')')
C++ CODE for .C. is inactive
C%%   const char fmt10[] = " \\rect{(%9.3f,%9.3f),(%9.3f,%9.3f)}\n";
C++ END

c *********     Executable Statements     ******************************

      call SPLOT1
C%%   fprintf(iofil, fmt10, x1, y1, x2, y2);
      write (IOFIL,BFMT1) x1, y1, x2, y2
      return
      end

c==================================================     SPLOT6     =====
      subroutine SPLOT6 (X, Y, A, B, ANGLE)

c Draw an ellipse with center at (X,Y) with axes A and B in physical
c coordinates, with axis A rotated ANGLE degrees counterclockwise from
c the positive X-axis direction.

c (X,Y)   Physical coordinates of the center of the ellipse
c A, B    Axis lengths of the ellipse
c ANGLE   A axis is rotated ANGLE degrees counterclockwise from
c         the positive X-axis direction

      real             X, Y, A, B, ANGLE

c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c
C++ CODE for ~.C. is active
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
      character*(*) BFMT1, BFMT2
      parameter (BFMT1='('' '//BSLASH//
     1'ellipse['',f9.3,'']{('',f9.3,'','',f9.3,''),'',f9.3,'','',f9.3,
     2''}'')',
     3  BFMT2='('' '//BSLASH//
     4  'ellipse{('',f9.3,'','',f9.3,''),'',f9.3,'','',f9.3,''}'')')
C++ CODE for .C. is inactive
C%% const char fmt10[]=" \\ellipse[%9.3f]{(%9.3f,%9.3f),%9.3f,%9.3f}\n";
C%% const char fmt20[]=" \\ellipse{(%9.3f,%9.3f),%9.3f,%9.3f}\n";
C++ END
c
c *********     Executable Statements     ******************************
c
      call SPLOT1
      if (ANGLE .ne. 0) then
C%%      fprintf(iofil, fmt10, angle, x, y, a, b);
         write (IOFIL,BFMT1) ANGLE, X, Y, A, B
      else
C%%      fprintf(iofil, fmt20, x, y, a, b);
         write (IOFIL,BFMT2) X, Y, A, B
      end if
      return
      end

c =================================================     SPLOT7     =====
      subroutine SPLOT7(M, LOCFIL, FILDEF)
c                       Takes care of fill requests
c
c HATCHW  Size of lines used for hatch lines.
c FILDEF  Vector giving giving dot size/space, and hatch info.  First 6
c   locs. are for 3 pairs of info for dots,  next 9 for 3 sets of thatch
c   info.
c J       Temp. to track index for fill pattern.
c JFILL   Data telling where to find things in SFILL.
c K       Temp. used to hold a value from LFILL.
c LOCFIL*  Array with fill pattern info.  Entries 1 to m of LOCFIL
c   contain actions indices as follows.
c       0   For no action, should not be used?
c       1   For fill with black.
c       2   For erase what preceded.
c       3   For shading with dots.
c       4   For shading with hatch lines.
c M       Absolute value gives the number of fill patterns.  M is set to
c   min (M, 0) on exit which has the effect of turning off filling after
c   a call when M > 0.
c SFILL  Text for output when LFILL( ,1:2) is 1 or 2.
c SHADEW Size of dots for shading.
c
c **************************** Variable Declarations *******************
c
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c Locals
      integer M, LOCFIL(3)
      real             FILDEF(19)
      integer J, JFILL(3), K
      real             SHADEW, HATCHW
c
      character SFILL*13
C++ CODE for ~.C. is active
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
      character*(*) BFMT1, BFMT2, BFMT3, BFMT4
      parameter (BFMT1='('' '//BSLASH//
     1  'thatch['', F9.3, '','', F9.3,'']'')',
     2  BFMT2='('' '//BSLASH//
     3  'shadewd{'', F9.3, ''}'')',
     4  BFMT3='('' '//BSLASH//
     5  'shade['', F9.3, '']'')',
     6  BFMT4='('' '//BSLASH//
     7  'hatchwd{'', F9.3, ''}'')')
      character PSFILL*13
      parameter (PSFILL=BSLASH//'gfill'//BSLASH//'gclear')
      data SFILL / PSFILL /
C++ CODE for .C. is inactive
CC                   12345678901234
C      data SFILL / '\gfill\gclear' /
C%%    const char fmt10[]=" \\thatch[%9.3f,%9.3f]\n";
C++ END
      data JFILL / 1, 7, 14 /
      data SHADEW, HATCHW / -1.E0, -1.E0 /
c
c *********     Executable Statements     ******************************
c
      do 200 J = 1, abs(M)
         K = LOCFIL(J)
         if (K .le. 2) then

C%%     fprintf(iofil, " %.*s\n",(int)(jfill[k]-jfill[k-1]),
C%%         sfill+jfill[k-1]-1);
            write (IOFIL, '(1X, A)') SFILL(JFILL(K):JFILL(K+1)-1)
         else if (K .eq. 3) then
            if (FILDEF(2*J-1) .ne. SHADEW) then
               SHADEW = FILDEF(2*J - 1)
C%%            fprintf(iofil, " \\shadewd{%9.3f}\n", shadew);
               write (IOFIL, BFMT2) SHADEW
            end if
C%%         fprintf(iofil, " \\shade[%9.3f]\n", fildef[2*j-1]);
            write (IOFIL, BFMT3) FILDEF(2*J)
         else if (K .eq. 4) then
            if (FILDEF(3*J+4) .ne. HATCHW) then
               HATCHW = FILDEF(3*J+4)
C%%            fprintf(iofil, " \\hatchwd{%9.3f}\n", hatchw);
               write (IOFIL, BFMT4) HATCHW
            end if
C%%         fprintf(iofil, fmt10, fildef[3*j + 4], fildef[3*j + 5]);
            write (IOFIL, BFMT1) FILDEF(3*J+5), FILDEF(3*J+6)
         end if
  200 continue
      M = min(M, 0)
      return
      end

c ==========================     SPLOT8     ============================
      subroutine SPLOT8(PENWID,BASE,STEP,TILL,TBEG,TEND,IAX, STRLOG)
c  Outputs tick marks for MFPIC (actually for METAFONT)
c  F. T. Krogh -- JPL -- August 6, 1997
c    PENWID The pen width
c    BASE   The starting point for the thing that varies.
c    STEP   The increment for the above.
c    TILL   The final point for the above.
c    TBEG   The location where the ticks start (constant for all ticks)
c    TEND   Where the ticks end (like TBEG).
c    IAX    = 1 for horizontal case, = 2 for vertical.
c    STRLOG < 0 for usual case.  Else give minimum location for logs.
c    IOFIL* The output unit
c## Maybe use IAX > 2 for polar cases??
c
c **************************** Variable Declarations *******************
c
      integer IAX
      real             PENWID, BASE, STEP, TILL, TBEG, TEND, STRLOG
c
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c
C++ CODE for ~.C. is active
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
      character*(*) BFMT1
      parameter (BFMT1='('' '//BSLASH//
     1'mfsrc{''/'' pickup pencircle scaled '',F6.3,''pt;''/'' for x='',
     2F10.3, '' step '', F10.3, '' until '', F11.3, '':'')')
c
   20 format('  draw(x, ',F11.3,')*pt..(x, ',F11.3,')*pt;'/' endfor;}')
   30 format('  draw(',F11.3,', x)*pt..(',F11.3,', x)*pt;'/' endfor;}')
   40 format('  for j = 2 upto 9:'/'   y:=x+',F11.3,'*mlog j;'/
     1 '   exitif y>', F11.3, ';' /'  if y>=', F11.3, ':')
   50 format('  draw(y, ',F11.3,')*pt..(y, ',F11.3,')*pt;'/
     1  '  fi' / '  endfor;'/'  endfor;}')
   60 format('  draw(',F11.3,', y)*pt..(',F11.3,', y)*pt;'/
     1  '  fi' / '  endfor;'/'  endfor;}')
C++ CODE for .C. is inactive
C%%  const char fmt10[]=" \\mfsrc{\n pickup pencircle scaled %6.3fpt;\n\
C%% for x=%10.3f step %10.3f until %11.3f:\n";
C%%   const char fmt20[]="  draw(x, %11.3f)*pt..(x, %11.3f)*pt;\n\
C%% endfor;}\n";
C%%   const char fmt30[]="  draw(%11.3f, x)*pt..(%11.3f, x)*pt;\n\
C%% endfor;}\n";
C%%   const char fmt40[]="  for j = 2 upto 9:\n   y:=x+%11.3f*mlog j;\n\
C%%   exitif y>%11.3f;\n  if y>=%11.3f:\n";
C%%   const char fmt50[]="  draw(y, %11.3f)*pt..(y, %11.3f)*pt;\n  fi\n\
C%% endfor;\n  endfor;}\n";
C%%   const char fmt60[]="  draw(%11.3f, y)*pt..(%11.3f, y)*pt;\n  fi\n\
C%% endfor;\n  endfor;}\n";
C++ END

c
      if (STRLOG .lt. 0.E0) then
c                           Regular ticks
C%%      fprintf(iofil, fmt10, penwid, base, step, till);
         write(IOFIL, BFMT1) PENWID, BASE, STEP, TILL
         if (IAX .eq. 1) then
C%%         fprintf(iofil, fmt20, tbeg, tend);
            write (IOFIL, 20) TBEG, TEND
         else
C%%         fprintf(iofil, fmt30, tbeg, tend);
            write (IOFIL, 30) TBEG, TEND
         end if
      else
c                           Logarithmic ticks
C%%      fprintf(iofil, fmt10, penwid, base - step, step, till);
         write(IOFIL, BFMT1) PENWID, BASE-STEP, STEP, TILL
C%%      fprintf(iofil, fmt40, .00169646282*step, till, strlog);
         write(IOFIL, 40) .00169646282*STEP, TILL, STRLOG
         if (IAX .eq. 1) then
C%%         fprintf(iofil, fmt50, tbeg, tend);
            write (IOFIL, 50) TBEG, TEND
         else
C%%         fprintf(iofil, fmt60, tbeg, tend);
            write (IOFIL, 60) TBEG, TEND
         end if
      end if
      return
      end

c==================================================     SPLOT9     =====
      subroutine SPLOT9
c                       Finish the plot.
c
c *************************** Internal Variables ***********************
c
c IFIN    Points to place where text in START starts for a give value
c   in IPLOT.  (Only 0 and 1 supported.)
c FIN     TeX command to write at the end -- \end{mfpic} or \endmfpic.
c
c **************************** Variable Declarations *******************
c
      integer IFIN(0:2)
c
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c
      character FIN*20
C++ CODE for ~.C. is active
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
c
      character*(*) BFMT1
      parameter (BFMT1='(1X,A,''{'//BSLASH//
     1  'hskip '',F9.3,''pt'//BSLASH//'relax}%'')')
      character PFIN*20
      parameter (PFIN=BSLASH//'end{mfpic}'//BSLASH//'endmfpic')
      data FIN / PFIN /
C++ CODE for .C. is inactive
CC                 123456789012345678901
C      data FIN / '\end{mfpic}\endmfpic' /
C%%    const char fmt10[]=" %.*s{\\hskip %9.3fpt\\relax}%%\n";
C++ END
      data IFIN / 1, 12, 21 /
c  Format below works for both TeX and LaTeX  (LaTeX could use \hspace).
c
c *********     Executable Statements     ******************************

      if (IPLOT .lt. 0) then
         IPLOT = -100 - IPLOT
         if (IPLOT .gt. 1) return
C%%      fprintf(iofil, fmt10, (int)(ifin[splotd.iplot+1]-
C%%       ifin[splotd.iplot]), fin+ifin[splotd.iplot]-1,
C%%       -splotd.pxo - splotd.pxsize);
      write (IOFIL, BFMT1) FIN(IFIN(IPLOT):IFIN(IPLOT+1)-1),-PXO-PXSIZE
         call SPLOT0
      else
C%%      fprintf(iofil, " %.*s\n", (int)(ifin[splotd.iplot+1] -
C%%         ifin[splotd.iplot]), fin + ifin[splotd.iplot] - 1);
         write (IOFIL, '(1X,A)') FIN(IFIN(IPLOT):IFIN(IPLOT+1)-1)
      end if
      return
      end

c==================================================     SPLOTL     =====
      subroutine SPLOTL (MANY, X, Y)

c     Plot a line through a sequence of points.

c>> 1996-12-18 SPLOTL Snyder  Initial code for MFPIC

c MANY [in] Defines action
c   .le. 0  End previous curve if any -- X and Y not used.  Then
c       if -1 start a new open curve.
c       if -2 start a new closed curve.
c       if -3 start a curve that is closed with a straight line.
c       if -4 start a new polyline.
c       if -5 start a new polygon.
c   > 0     Output for plotting of MANY points.
c   No message is produced if MANY <= 0 twice in a row -- the second
c   MANY is used silently.
c X [in] is an array of one or more abscissae.
c Y [in] is an array of one or more ordinates.  The number of ordinates
c        must be the same as the number of abscissae.

      integer MANY
      real             X(*), Y(*)

c     *****     External References     ********************************

c ERMSG   Print error messages.

c     *****     Local Variables     ************************************

c FORMAT  output format when finishing a curve.
c I       is a loop inductor and subscript.
c IXPREF  index of PREFIX and LPREFX to use.
c K       count of items to print.
c LPREFX  Points to start of text in PREFIX, for various cases.
c IOFIL*  The logical unit number to use for plot output.
c OLDX, OLDY the last X and Y value on the previous call.
c PREFIX  Character strings used for headers.
c STATE   The number of points saved.  If -1, no curve is started.
c         Else 0 <= STATE <= 3.

      integer I, IXPREF, K, LPREFX(-5:1)
      save IXPREF, LPREFX
      real             OLDX(3), OLDY(3)
      save OLDX, OLDY
      integer STATE
      save STATE
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/

c     *****     Data Statements     ************************************

C++ CODE for ~.C. is active
      character*(62) FORMAT(3)
      save FORMAT
      data FORMAT /
     1'(a,''('',f9.4,'','',f9.4,'')}'')' ,
     2'(a,''('',f9.4,'','',f9.4,''),('',f9.4,'','',f9.4,'')}'')',
     3'(a,''('',f9.4,'','',f9.4,'')'',2('',('',f9.4,'','',f9.4,'')''),
     4''}'')'/
C++ END
c
      character PREFIX*48
C++ CODE for (HOW==MFPIC) & ~.C. is ACTIVE
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
c
      character PPREFX*48
      parameter (PPREFX=BSLASH//'polygon{'//BSLASH//'lines{'//BSLASH//
     1  'lclosed '//BSLASH//'curve{'//BSLASH//'cyclic{'//BSLASH//
     2  'curve{,')
      data PREFIX / PPREFX /
C++ CODE for (HOW==MFPIC) & .C. is INACTIVE
CC                            1111111111222222222233333333334444444444
CC                   1234567890123456789012345678901234567890123456789
C      data PREFIX /'\polygon{\lines{\lclosed \curve{\cyclic{\curve{,'/
C++ END
      data STATE /-1/
      data LPREFX / 1, 10, 17, 33, 41, 48, 49 /
C++ CODE for ~.C. is active
   10 format (a, '(', f9.4, ',', f9.4, ')',2(',(', f9.4, ',', f9.4, ')'
     1:)/(3(',(', f9.4, ',', f9.4, ')')))
C++ END

c     *****     Executable Statements     ******************************

      if (MANY .le. 0) then
C++ CODE for ~.C. is active
        if (STATE .gt. 0) write (IOFIL, FORMAT(STATE))
     1     PREFIX(LPREFX(IXPREF):LPREFX(IXPREF+1)-1),
     2     (OLDX(I), OLDY(I), I = 1, STATE)
C++ CODE for .C. is inactive
C%%     if (state > 0){
C%%        fprintf(iofil, "%.*s", (int)(lprefx[ixpref+6] -
C%%        lprefx[ixpref+5]), prefix+lprefx[ixpref+5]-1);
C%%        for (i = 0; i < state; i++) {
C%%           if (i != 0) fprintf(iofil, ",");
C%%           fprintf(iofil, "(%9.4f,%9.4f)", oldx[i], oldy[i]);}
C%%        fprintf(iofil, "}\n");}
C++ END
        IXPREF = MANY
        STATE = 0
        if (MANY .eq. 0) STATE = -1
      else if (STATE .ge. 0) then
        K = MANY - 1 - mod(MANY + STATE - 1, 3)
        if (STATE + K .ge. 3) then
C++ CODE for ~.C. is active
          write (IOFIL, 10) PREFIX(LPREFX(IXPREF):LPREFX(IXPREF+1)-1),
     1      (OLDX(I), OLDY(I), I = 1, STATE), (X(I), Y(I), I = 1, K)
C++ CODE for .C. is inactive
C%%        fprintf(iofil, "%.*s", (int)(lprefx[ixpref+6] -
C%%        lprefx[ixpref+5]), prefix+lprefx[ixpref+5]-1);
C%%        for (i = 0; i < state; i++) {
C%%           if (i != 0) {
C%%             if (i%3 == 0) fprintf(iofil, "\n");
C%%             fprintf(iofil, ",");}
C%%           fprintf(iofil, "(%9.4f,%9.4f)", oldx[i], oldy[i]);}
C%%        for (i = 0; i < k; i++) {
C%%           if (i + state != 0) {
C%%             if ((i+state)%3 == 0) fprintf(iofil, "\n");
C%%             fprintf(iofil, ",");}
C%%           fprintf(iofil, "(%9.4f,%9.4f)", x[i], y[i]);}
C%%        fprintf(iofil, "\n");
C++ END
          IXPREF = 0
          STATE = 0
        end if
        do 50 I = max(K,0) + 1, MANY
          STATE = STATE + 1
          OLDX(STATE) = X(I)
          OLDY(STATE) = Y(I)
50      continue
      else
         stop
     1     'SPLOTL (Internal bug) Adding points without initialization.'
      end if
      return
      end

c==================================================     SPLOTS     =====
      subroutine SPLOTS (XY, KSYMB)
c Plot a symbol or error bars or vectors at (XY(1), XY(2).  XY contains
c extra data for error bars or vectors as follows:
c For error bars:
c  XY(1:2) is the mid point.
c  XY(3:4) is the bottom.
c  XY(5:6) is the top.
c For Arrows:
c  XY(1:2) is the tail.
c  XY(3:4) is the head.
c
c KSYMB is an integer with digit defining what is to be drawn.
c   KSYMB = i1 + 10 * (i2 + 10 * (i3 + 10 * (i4 + 10*i5)))
c   if (i2 is not 1) then
c      i1 is number of vertices for a polygon
c      i2 is the number to "skip" when drawing the symbol
c      i3 defines angle of rotation for the first point - 45 * i3 / i1.
c      i4 width of line to use in drawing, in deci-points.
c      i5 The diameter of the circle, if 0, 6 is used.
c   else if (i1 is 0 or 1) then   (let i5 = i6 = 10 * i7)
c      i3 length of horizontal line for top/bottom error bar in points.
c      i4 length of horizontal line in middle in points.
c      i6 width in deci-points for the cross hatch lines.
c      i7 width in deic-points for the vertical line, if 0, 3 is used.
c   else                          (let i5 = i6 = 10 * i7)
c      i3 length of the arrow head for an arrow.
c      i4 size of circle in points to be drawn at the tail of the arrow.
c      i6 width in decipoints for line used to draw the circle.
c      i7 width in decipoints of the line use to draw the arrow.
c   end if
c
c ****************  Variables Definitions ******************************
c
c A     Angle of current vertex, in degrees.
c A0    Angle of initial vertex, in degrees.
c AI    Angle increment, in degrees.
c ARRLEN* Length of an arrow head.
c ARRLOC  Local length of arrow head.
c BARMID  Length of middle bar for error bars.
c BTBARS  Legth of top and bottom bars for error bars.
c CLEAR   Logical variable set = .true. when symbol is drawn twice, the
c       time to clear the space, before drawing the symbol.
c CLEARI  The initial value used for CLEAR.
c COMMA Either ',' or ' ', depending on whether the last point in a
c       polygon or line is being emitted.
c D2R   Degrees to radians.
c IGCD  The gcd of (NVERT, NSKIP).
c I, J, K  Loop inductors.
c KPENDF  Default value used for KURPEN.
c KURPEN* Line width parameter, from KSYMB.
c LOCPEN The pen width saved for symbols.
c LPENA   Value of KURPEN for line used to draw an arrow.
c LPENC   Value of KURPEN for line used to draw an circle for vector
c    fields.
c LPENH   Value of KURPEN for line used to draw an horizontal lines for
c    error bars.
c LPENV   Value of KURPEN for line used to draw an vertical lines for
c    error bars.
c NP    Number of points to plot.
c NSKIP Number of vertices to skip, from KSYMB.
c NVERT Number of vertices, from KSYMB.
c R     Circumcircle radius, 0.5 * max(W, SIZE-W)
c ROTATE Amount the first point is rotated from the positive x-axis.
c SIZCIR  Diameter of circle use for vector fields.
c WW    0.01 * KURPEN = line width in points.
c XA, YA  Average of XMAX, XMIN etc.
c XMAX, XMIN, YMAX, YMIN  Obvious.
c XW, YW  Working values for X and Y.
c
c Formals
      real             XY(*)
      integer KSYMB
c Common
c  For SPLOT0
      real             ARRLEN, PXO, PXSIZE, PYO, PYSIZE
C++ CODE for ~.C. is active
      integer IOFIL, IPLOT, KURPEN, LASPEN
      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
     1   IOFIL, IPLOT, KURPEN, LASPEN
C++ CODE for .C. is inactive
C      integer IPLOT, KURPEN, LASPEN
C      common / SPLOTD / ARRLEN, PXO, PXSIZE, PYO, PYSIZE,
C     1   IPLOT, KURPEN, LASPEN
C++ END
      save /SPLOTD/
c Locals
      logical CLEAR, CLEARI
      character COMMA
      integer I, IGCD, J, K, KPENDF, LOCPEN, LPENA, LPENC, LPENH, LPENV,
     1   LSYMB, NP, NSKIP, NVERT
      real             A, A0, AI, ARRLOC, BARMID, BTBARS, D2R, DIAMET,
     1   R, ROTATE, SIZCIR, WW, XA, XMAX, XMIN, XW, YA, YMAX, YMIN, YW
      save ARRLOC, BARMID, BTBARS, CLEARI, DIAMET, LPENA, LPENC, LPENH,
     1   LPENV, LOCPEN, LSYMB, NSKIP, NVERT, ROTATE, SIZCIR
      parameter (KPENDF = 30)
      parameter (D2R=.01745329251994329576923690768488612713442871889e0)
c
      data LSYMB / -1 /
c
c
C++ CODE for ~.C. is active
c Weird stuff to take care of "\" being treated as an escape character
c on SGI Fortran compilers
      character BSLAS1*(*), BSLASH
      parameter (BSLAS1 = '\\')
      parameter (BSLASH = BSLAS1(1:1))
      character*(*) BFMT1, BFMT3
      parameter (BFMT1='('' '//BSLASH//
     1  'circle{('', F10.3, '','', F10.3, ''),'', F10.3,''}'')',
     2  BFMT3='('' '//BSLASH//
     3  'lines{('',F12.5,'','',F12.5,''),'',''('',F12.5,'','',F12.5,
     4'')}'')')
   20 format (2x, '(', f12.5, ',', f12.5, ')', a)
      character*(*) BFILL, BCLEAR, BPOLY, BLINES
      parameter (BFILL='('' '//BSLASH//'gfill'')',
     1  BCLEAR= '('' '//BSLASH//'gclear'')',
     2  BPOLY= '('' '//BSLASH//'polygon{'')',
     2  BLINES= '('' '//BSLASH//'lines{'')')
C++ CODE for .C. is inactive
C%%  const char fmt10[]=" \\circle{(%10.3f,%10.3f),%10.3f}\n";
C%%  const char fmt20[]="  (%12.5f,%12.5f)%c\n";
C%%  const char fmt30[]=" \\lines{(%12.5f,%12.5f),(%12.5f,%12.5f)}\n";
C++ END
c
c *********     Executable Statements     *****************************
c
      if (KSYMB .ne. LSYMB) then
c                           Unpack the data.
         LSYMB = KSYMB
         K = LSYMB
         NVERT = mod(K, 10)
         K = K / 10
         NSKIP = mod(K, 10)
         K = K / 10
         if (NVERT .ne. 1) then
c                           Got a symbol
            if (NVERT .ne. 0) ROTATE = real(mod(K, 10)*45) / real(NVERT)
            K = K / 10
            LOCPEN = 10*mod(K, 10)
            if (LOCPEN .eq. 0) then
               LOCPEN = KPENDF
            else if (LOCPEN .eq. 90) then
               LOCPEN = 0
            end if
            DIAMET = real(K / 10)
            CLEARI = .false.
            if (DIAMET .ge. 100.E0) then
               DIAMET = mod(DIAMET, 100.E0)
               CLEARI = .true.
            end if
            if (DIAMET .eq. 0.E0) DIAMET = 6.E0
         else if (NSKIP .le. 1) then
c                               Error Bars -- two types
            BTBARS = mod(K, 10)
            K = K / 10
            BARMID = mod(K, 10)
            K = K / 10
            LPENH = 10 * mod(K, 10)
            LPENV = 10 * (K / 10)
         else if (NSKIP .eq. 2) then
c                               Vector field
            ARRLOC = mod(K, 10)
            K = K / 10
            SIZCIR = mod(K, 10)
            K = K / 10
            LPENC = 10 * mod(K, 10)
            if (LPENC .eq. 0) LPENC = 20
            LPENA = 10 * (K / 10)
         else
c              Perhaps do text in the future?
            return
         end if
      end if
      if (NVERT .ne. 1) then
         CLEAR = CLEARI
         KURPEN = LOCPEN
         call SPLOT1
  100    WW = .01E0 * real(LOCPEN)
         R = .5E0 * max(WW, DIAMET - WW)
         if (NVERT .eq. 0) then
            if (LOCPEN .eq. 0) then
C%%            fprintf(iofil, " \\gfill\n");
               write (IOFIL, BFILL)
               CLEAR = .false.
            else if (CLEAR) then
C%%            fprintf(iofil, " \\gclear\n");
               write (IOFIL, BCLEAR)
            end if
C%%         fprintf(iofil, fmt10, xy[0], xy[1], r);
            write (IOFIL, BFMT1) XY(1), XY(2), R
         else
            AI = real((NSKIP+1)*360) / real(NVERT)
            if (NSKIP .gt. NVERT) then
               NSKIP = NVERT
               IGCD = NVERT
            else
c                         Get the GCD of NSKIP, NVERT
               IGCD = NSKIP + 1
               K = NVERT
  120          I = mod(K, IGCD)
               if (I .ne. 0) then
                  K = IGCD
                  IGCD = I
                  go to 120
               end if
            end if
            NP = NVERT / IGCD
            XA = 0.0
            YA = 0.0
            XMAX = 0.0
            XMIN = 0.0
            YMAX = 0.0
            YMIN = 0.0
            do 400 K = 1, 2
c                                 K = 1 => get XMIN etc; K = 2 => draw.
               A0 = ROTATE
               do 300 I = 1, IGCD
                 if (K .eq. 2) then
                    if (NSKIP .ne. NVERT) then
                       if (NP .ne. 2) then
                          if (LOCPEN .eq. 0) then
C%%                           fprintf(iofil, " \\gfill\n");
                              write (IOFIL, BFILL)
                          else if (CLEAR) then
C%%                           fprintf(iofil, " \\gclear\n");
                              write (IOFIL, BCLEAR)
                          end if
C%%                       fprintf(iofil, " \\polygon\n");
                          write (IOFIL, BPOLY)
                       else if (CLEAR) then
C%%                       fprintf(iofil, " \\gclear\n");
                          write (IOFIL, BCLEAR)
C%%                       fprintf(iofil, fmt10, xy[0], xy[1], r);
                          write (IOFIL, BFMT1) XY(1), XY(2), R
                          go to 400
                       else
C%%                       fprintf(iofil, " \\lines\n");
                          write (IOFIL, BLINES)
                       end if
                    else if (CLEAR) then
C%%                    fprintf(iofil, fmt10, xy[0], xy[1], r);
                       write (IOFIL, BFMT1) XY(1), XY(2), R
                       go to 400
                    end if
                 end if
                 A = A0
                 COMMA = ','
                 do 200 J = 1, NP
                    XW = XA + R * cos(D2R*A)
                    YW = YA + R * sin(D2R*A)
                    if (K .eq. 1) then
                       XMIN = min(XMIN, XW)
                       XMAX = max(XMAX, XW)
                       YMIN = min(YMIN, YW)
                       YMAX = max(YMAX, YW)
                    else
                       if (NSKIP .ne. NVERT) then
                          if (J .eq. NP) comma = ' '
C%%                       fprintf(iofil, fmt20, xw, yw, comma);
                          write (IOFIL, 20) XW, YW, COMMA
                       else
C%%                       fprintf(iofil, fmt30, xa, ya, xw, yw );
                          write (IOFIL, BFMT3) XA, YA, XW, YW
                       end if
                    end if
                    A = A + AI
200              continue
                 if ((K .eq. 2) .and. (NSKIP .ne. NVERT)) then
C%%                 fprintf(iofil, "  }\n");
                    write (IOFIL, '(2X,''}'')')
                 end if
                 A0 = A0 + (360.E0 / NVERT)
300            continue
               XA = XY(1) - 0.5E0 * (XMIN + XMAX)
               YA = XY(2) - 0.5E0 * (YMIN + YMAX)
400        continue
         end if
         if (CLEAR .and. (LOCPEN .ne. 0)) then
            CLEAR = .false.
            go to 100
         end if
      else if (NSKIP .le. 1) then
c                           Error bars.
         KURPEN = LPENV
         call SPLOT2(XY(3), XY(4), XY(5), XY(6))
         if (LPENH .ne. 0) then
            KURPEN = LPENH
            XA = XY(1) - .5E0 * BTBARS
            XW = XY(1) + .5E0 * BTBARS
            call SPLOT2(XA, XY(4), XW, XY(4))
            call SPLOT2(XA, XY(6), XW, XY(6))
            call SPLOT2(XY(1)-.5E0*BARMID,XY(2),XY(1)+.5E0*BARMID,XY(2))
         end if
      else
c                           Draw arrows.
         ARRLEN = ARRLOC
         KURPEN = LPENA
         XW = XY(1)
         YW = XY(2)
         if (SIZCIR .ne. 0.E0) then
           R = SIZCIR / sqrt((XY(3)-XW)**2 + (XY(4)-YW)**2)
           XW = XW + R * (XY(3) - XW)
           YW = YW + R * (XY(4) - YW)
         end if
         call SPLOT2(XW, XW, XY(3), XY(4))
         if (SIZCIR .ne. 0.E0) then
c                             Add a little circle.
            KURPEN = LPENC
            if (LPENC .eq. 90) then
C%%            fprintf(iofil, " \\gfill\n");
               write (IOFIL, BFILL)
               KURPEN = 0
            end if
            call SPLOT1
C%%         fprintf(iofil, fmt10, xy[0], xy[1], sizcir);
            write (IOFIL, BFMT1) XY(1), XY(2), SIZCIR
         end if
      end if
      return
      end


