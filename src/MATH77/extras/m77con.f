      program M77CON
c     .  Copyright (C) 1989, California Institute of Technology.
c     .  All rights reserved.  U. S. Government sponsorship under
c     .  NASA contract NAS7-918 is acknowledged.
c Time-stamp: <2009-10-28 12:20:46 m>
c>> 2009-10-28 M77CON  Krogh  Added .D., .S., and .I.
c>> 2009-06-17 M77CON  Krogh  Fixed bug intoduced on 2005-12-09.
c>> 2008-10-25 M77CON  Krogh  Fixed "SUBSTITUTE" to work for lower case.
c>> 2008-10-24 M77CON  Krogh  Avoid spurious gfortran error message.
c>> 2005-12-09 M77CON  Krogh  Fixed C conversion problem (double/float)
c>> 2005-03-28 M77CON  Krogh  Changed PACK to PACKUP (g77 prob.)
c>> 2002-01-24 M77CON  Krogh  Fixed prob. with char. data => C.
c>> 2001-11-23 M77CON  Krogh  Fixes do c---... is normal line.
c>> 2001-03-31 M77CON  Krogh  Fixed prob. when LOUT set and no c--.
c>> 2000-12-03 M77CON  Krogh  Replaced an 80 with LENLIN on a loop.
c>> 1998-11-20 M77CON  Krogh  Generalized C data conversion for chars.
c>> 1998-01-14 M77CON  Krogh  C formats converted to double o.k. now.
c>> 1997-12-18 M77CON  Krogh  Fixed conv of real( starting a cont. line.
c>> 1996-04-27 M77CON  Krogh  Added CCODE logic.
c>> 1996-01-24 M77CON  Krogh  Added C++(expression) Default ...
c>> 1996-01-19 M77CON  Krogh  Made subtle changes to handling of type.
c>> 1996-01-09 M77CON  Krogh  Added comments for some unreachable code.
c>> 1995-12-18 M77CON  Krogh  Fixed result of string compare.
c>> 1995-11-29 M77CON  Krogh  Added conversion of DATA state. for C conv
c>> 1995-11-21 M77CON  Krogh  Fixed test for blank in specified keyword.
c>> 1995-11-20 M77CON  Krogh  Fixed prob. in converting LIN="Z" to "C".
c>> 1995-11-16 M77CON  Krogh  Fixed minor prob. in conv. F.P. constants.
c>> 1995-11-10 M77CON  Krogh  Allowed blank line in M77JOB.
c>> 1995-10-30 M77CON  Krogh  .y. gives output type & "REPLACE" always
c>> 1995-10-17 M77CON  Krogh  Moved formats up for conversion to C.
c>> 1995-09-11 M77CON  Krogh  Removed unused var. MAPLFP
c>> 1995-05-08 M77CON  Krogh  Allow same name for input & output files.
c>> 1995-01-20 M77CON  Krogh  Removed "\" from format and comment.
c>> 1994-12-01 M77CON  Krogh  Fixed precedence on "~".
c>> 1994-11-23 M77CON  Krogh  Fixed bug, added "STOP", use M77JOB.
c>> 1994-11-22 M77CON  Krogh  Added "C-- (Type)Replace..." and .Y.
c>> 1994-11-03 M77CON  Krogh  Initial Code.
c
c M77CON converts between different versions of Fortran or SFTRAN source
c code that satisfy conventions used for MATH77.  It combines
c functionality from earlier programs known as "The Specializer",
c CHGTYP, and MARVEL.
c
c Note that all comparisons done by this code are case insensitive, all
c replacements are done in the same case as for the text found, and
c blanks are removed before comparisons are made in the source code.
c
c To simplify the description below, we introduce the following:
c L, L1, ... Li  Letters
c T, T1, ... Ti  Letters used to denote types.
c I, I1, ... Ii  Integer constants.
c V, V1, ... Vi  Variables
c E, E1, ... Ei  Expressions
c A, A1, ... Ai  Assignments
c
c The types denoted by T, T1, ... are: S, D, Q, I, C, Z, and W, which
c are used for Single precision, Double precision, Quadruple precision
c (or at least some precision greater than double), Integer, Complex,
c Double precision complex, and quadruple precision complex
c respectively.  An "L" equal to one of these letters is used for a code
c of the corresponding type.  Other letters are used for mixtures of
c types.  In particular, by convention, P (Partial double) is used for a
c code that uses some D in a code that is primarily S (perhaps to save
c on storage), and X (eXtended) is used for a code that is primarily in
c D, but uses some Q.  No other types are supported.  (It is possible E
c (Extended) might be used for code that is primarily Z, with some W or
c that L might be added for Logical if a need for either should arise.)
c
c Expressions may use the following operators all of which have the same
c meaning and precedence as their Fortran counterparts.:
c Arithmetic:        +  -  *  /  **
c Relational:        <  >  == /= <= >=
c Alt. Relational:         =  ~=
c Logical:           | (or)   & (and)   ~ (not)
c Concatenation:     //
c Grouping letters:  {  }
c Other:             (  )  ,
c
c The "{" and "}" are used to define a special operand.  If the letter
c L1 from the "MAKE..." line, see below, appears inside the {...}, then
c this operand has the value true, and otherwise has the value false.
c The letters inside the {...} may or may not be separated by commas.
c
c We recommend using "==" rather than "=" in relational expressions.
c "=" is allowed for compatibility with comments written for MARVEL.
c


c There are integer, logical, and string metavariables and constants.
c An integer constant is defined as in Fortran.  The logical constants
c are .T., .F., .U., .D., .S., .I., and .C. Of course .T. is always true
c , and .F. is false; .U. is undefined (see below); .D., .S., and
c .I. have a value of .T. if the upper case of the letter L1 in a MAKE
c command matches the letter between the dots, has a value of .F. if L1
c is defined but does not match, and otherwise has the value .U.; and
c .C. has the value .T. if a version of the code used as input to the
c Fortran to C converter is being generated, and is .F.  otherwise.
c Initially .C. =.F.; its value can be set to .T. with the statement SET
c .C. in m77job, and can be reset to .F. with the statement SET ~.C..
c (With T=.T., F=.F., and U=.U., we have, U|F=.U., U|T=.T., U&F=.F.,
c U&T=.U., ~U=.U.  If U is on either side of a relational operator, the
c result is .U.)

c .X., .Y. and .N. give strings of length one, containing the letter
c used for floating point exponents in the output, the letter used for
c types in the output, and the letter replacing the "?" in names
c respectively.
c Other strings are defined as either an arbitrary sequence of
c characters enclosed by either apostrophes ('), quotes ("), or has the
c same form as a variable, that is any number of nonoperators, not all
c of which are digits, and not the letters "TFUXN" preceded and followed
c by a ".".  The value of a string is just the sequence of characters
c making up the string except for the delimiters, if any.  If a logical
c value of a string is required by the context, a string has the value
c .U.
c
c An expression can be formed as one would expect using the operators
c that are available.  A diagnostic is given if one tries to mix types
c in a way that make no sense.  Integer and logical are converted to
c strings when they appear as an operand of "//".
c
c As assignment has the form: V = E, or simply V.  The latter form is
c equivalent to writing V=.T.
c
c Note that the first "=" seen in an assignment is an assignment
c operator, while in all other cases, an "=" is a relational operator.
c
c In describing syntax below, we use [text] to indicate "text" is
c optional; the brackets are not part of what one writes.  The default
c used if "text" is not present is indicated in the descriptions.  And
c "text" is referred to as if it were present.
c
c The program begins by reading the control file, m77job which defines
c what files to process and the actions to be taken on these files.
c m77job consists of lines of the form:
c
c "C..." is a comment and can appear anywhere in the control file.
c A comment of the form "C>>xxxxxxxxxxyy...y" (the 10 x's should have
c the form "yyyy-mm-dd") has the effect of inserting a comment of the
c form "ww...wxxxxxxxxxxxxzz...zyy...y" ahead of the first line of a
c file with a comment that starts with same first three characters.
c (The case used for the C is not significant.)  The w's and z's are
c taken from the comment which this is being inserted ahead of.  ww...w
c are the characters from column 1 to 3 or to the end of the first
c region containing blanks if column 4 contains a blank. The zz...z is
c the text and surrounding blanks between the first two blank regions
c after column 10 of the comment that this is inserted ahead of.)
c
c "MAKE L1 [with type T1] [from L2 [with type T2]]"
c L1 is the letter that is used in names describing the code that is to
c be created.  T1 is the corresponding type. If T1 is not given it is
c assumed to be L1 if L1 is one of the types mentioned above, is assumed
c to be S or D, for L1 = P or X respectively, and otherwise an error
c results.  The L2 is defined as for L1 part, except it is used to get
c the name of the code used to generate the "L1" code.  If the from
c clause is missing, L2 is D, except when L1 is D, in which case L2 is
c S. The case of letters is not significant, except for L1.  The case
c used here determines the case used when substituting any letter into a
c file name, and thus the case is significant on systems where case is
c significant in a file name.
c
c "T1 Defined by: L3, keyword, type_conv, mach_const"
c "T2 Defined by: keyword, type_conv, conjugate, imag_part"
c One of these lines is needed only if the input or output code makes
c use of a nonstandard type: namely, Q, Z, or W.  Note that W makes use
c of Q, and thus if it is used, Q must also be defined.  The first type
c of line is used to define real (as opposed to complex) types.  L3 is
c the letter used to define the exponent part of floating point
c constants, keyword is the text used to declare a variable of type T,
c type_conv is the name of the function used for type conversion to type
c T, and mach_const is the name of the function used for getting machine
c constants.  The standard S, D, and C types are initialized as if
c one had input:
c "S Defined by: E, real, real, r1mach",
c "D Defined by: D, double precision, dble, d1mach", and
c "C Defined by: complex, cmplx, conjg, aimag"
c
c Note that at most one embedded blank is allowed inside a keyword, and
c if such a blank is desired it must be included on this line.  Also
c recall that if either the input or output file uses Q, Z, or W, these
c must be defined.  Internally I defines the keyword "integer", and
c type_conv as "int".
c
c "VERIFY path" if present will compare the lines of the result files
c with lines in the files of the same name as the result files preceded
c by "path", and when lines are different, print both lines.  The text
c in "path" is taken exactly as given, and is considered terminated by
c either a blank or an end of line.
c
c "SET A1, A2, ... Ai" is used to set values for the variables defined
c by the assignments.  One can have any number of "SET..." lines.
c
c "STOP ..." Stops the processing of the control file.
c
c "FILE name[, ext]" is used to specify the name of a file to be
c processed.  A "?" in name may be used to parameterize the name if
c desired.  L2 is substituted for the "?" (if any) and a file of this
c name is processed as the input file. Then L1 is substituted for "?" to
c obtain the name of the output file.  If the ",  ext" is present this
c name is further modified by searching backward from the end of "name"
c looking for the first character in "ext".  When this character is
c found, this character and the following characters in "name" are
c replaced by "ext". The input and output files can have the same name.
c If the "?" is missing, there is no need to have had a "MAKE..." line.
c Any characters except "," can appear in "name" or "ext", as long as
c the results will serve as valid file names.
c
c Processing a "FILE..." line involves carrying out conversions on the
c input file that has been named and placing the result in the output
c file.  If a "VERIFY..." line is present, the output file will be
c compared with the file specified there.  After processing a "FILE ..."
c line, the next line from the control file, m77job is read, and
c actions taken as requested there.  Variables defined in previous SET
c instructions become undefined, if there is a new SET instruction, but
c otherwise the old values are kept.
c
c When processing code there are two special kinds of comments that may
c be in the source code that involve special processing by this program.
c (Not counting the "C>>..." comments which are discussed fully above.)
c Those headed by "C++" are intended primarily to support different
c versions of a program.  Those headed  by "C--" are intended primarily
c to support different precisions, which involves changing types in the
c other Fortran statements.  "C++..." lines have the form:
c
c (1) "C++  Code for E is active"
c (2) "C++  Code for E is inactive"
c (4) "C++  END"
c (3) "C++  Of next I lines, only the first E are active."
c (5) "C++  Default A1, A2, ... Ai", or
c     "C++L1' L2' ... Default A1, A2, ... Ai"
c     "C++(E) Default A1, A2, ... Ai"
c (6) "C++  Current has A"
c (7) "C++  Substitute for V1, V2, ... Vi below"
c (8) "C++  Replace A1, A2, ... Ai"
c
c In either case (1) or (2), if E=.U., no change is made in this line or
c in the following code.
c
c In the case of (1), if E=.T. there is no change, and if it is .F., the
c word "active" is changed to "inactive", and a C is inserted at column
c 1 of the following lines.  An error message is printed if the new line
c extends beyond column LENLIN.
c
c In the case of (2), if E=.F. there is no change, and else the word
c "inactive" is replaced by "active", and "C" is deleted from column 1,
c of the following lines, and the rest of the line shifted left.
c
c Either of the above used after a (1) or (2) either changes this
c action to the new one specified.  Neither of these types of actions
c can be active when the end of a program unit is reached.
c
c Case (3) just turns off any active or inactive state set by lines (1)
c or (2).  There should not be a "C++..." between this line and the
c preceding line of type (1) or (2).
c
c In case (4), one must have I (an integer constant) > 0, and E must
c have an integer value .le. I.  The following I lines in the code must
c be valid Fortran statement if Col. 1 is set to a blank and there must
c be I lines preceding the last line of the program unit.  This causes
c the action of storing a ' ' in Col. 1 of the following E lines, and
c then storing a 'C' in Col. 1 for the remaining I - E lines.
c
c In Case (5), if any letters precede "Default", and none of them is the
c same as L1 (except for case), then this line is not processed.
c Similarly if there is an (E) and E is not a logical expression with
c the value .T., this line is not processed.
c Otherwise it is treated just like the "SET" lines in the control file,
c except if the Vi on the left of the first "=" in Ai is already
c defined, then its occurrence here has no effect on its value.
c
c In case (6), the A must have an "=" sign.  If the V on the left side
c of the "="  in A is not defined this works just like a "C++ Default A"
c line.  If V is defined, then the text on the right of the "=" is
c replaced by the current value of V.
c
c In case (7), the following line is examined for each of the Vi's in
c turn.  If such a variable name is found followed by an "=" sign, and
c an integer value, the integer is replaced by the integer value of Vi.
c If the "=" sign is followed by a logical value, this value is replaced
c by the corresponding Fortran logical constant for Vi.  A type mismatch
c in either of these cases results in a diagnostic, with no change.  A
c value of .U. in this case is treated as a mismatch.  (At present
c variables with string values are considered a mismatch, but if there
c is a use for it, they too could be substituted.)
c
c In case (8), the leftside of each Ai must be a constant character
c string, i.e. either 'xx...x' or "xx...x", and the right side must be
c an expression that evaluates to a string value.  Every occurence of
c "xx...x" from this line down is replaced by the value for E.
c
c The actions for "C++..." lines precede other actions, and thus a
c "C++..." line could result in turning off a "C--..." line.
c
c The "C--..." lines have the form:
c
c (1) "C--L4 [(Type)]Replaces "?": V1, V2, ..."
c (2) "C--& v1, v2, ..."
c (3) "C--L4 Next line special: S1, S2, ..."
c (4) "C-- Begin mask code changes"
c (5) "C-- End mask code changes"
c
c If there was no "MAKE..." line, type conversion is turned off, and
c "C--..." lines are simply treated like any other comment.
c
c At most one of (1) can occur with (Type) absent, and at most one can
c occur with (Type) present.  If one with the (Type) present occurs it
c must occur after immediately after a (1) or a (2) (No embedded
c comments.)  All lines of type (2) must follow immediately after a (1)
c or a (2).  If (Type) is absent, L4 denotes the value of L1 for the
c current version of the code.  (See the description of the "MAKE..."
c line above.)  If L4 = L1, then no names are changed.  When L4=L2 (from
c the "MAKE..." line) the L4 in this statement is replaced by L1.  Each
c of the Vi, or vi, must be Fortran names with one of the letters
c replaced by a "?".  These names with the ? replaced by L2 are saved
c for later use (see below).  The case when (Type) is present is
c similar, except that in the description L1 is replaced by T1, and L2
c is replaced by T2.
c
c Case (3) is used to indicate either that something different than the
c usual value for L4 is to be used on the next line, or that conversion
c to or from integer or one of the complex types is allowed.
c Conversions to or from integer types occur only on lines so marked.
c Conversion to or from one of the  complex types are assumed to be
c between the corresponding real types ((C, S), (Z, D), (W, Q)), except
c for lines marked in this manner.  L4 indicates the letter used in the
c current source for the following line.  (This letter in turn
c determines things like keywords, type conversions, etc.)  If this line
c results in a change, the L4 will be replaced by the new value in the
c output code. The Si either have a single Li, or an expression of the
c form Li => Li'. If L1 is the same as any of these Li, then the
c following line is treated as special based on this Si.  If there is no
c "=> Li'" part then presumably Li is I, C, Z, or W, and the following
c line is modifiable to suit these types.  If the "=> Li'" part is
c present, then the following line is to be converted using Li',
c and L4 set accordingly.  If none of the Li match the L1 from the
c "MAKE..." line, the following line is treated just like any other
c line.
c
c Lines of type (4) and (5) enable and disable changes for different
c types respectively.
c
c Let "out" denote things defined, perhaps by default, by L2 in
c a "MAKE..." line or by preceding "C--..." lines in the code.  (If
c there is no "MAKE..." line, then there is no type conversion.)  Let
c "in" denote things defined by L4 in a similar way.  When type
c conversion results in substitutions, if the "out" thing is longer than
c the "in" thing, characters to the right of the "in" thing are shifted
c right to leave enough blank space to insert the "out" thing.  A
c diagnostic results if in the process nonblank characters are shifted
c beyond column LENLIN (and thus lost).  If the "in" thing is the
c longer, the extra characters in the "in" thing are blank filled.  When
c the "out" thing is copied, the case of letters is set to have the case
c of the last letter seen up to and including the position being copied
c to.
c
c When type conversion is on (as it is if there is a "MAKE..." line in
c the control file), then all lines, except those with "++" or "--" in
c columns 2 and 3, are examined in turn for the following:
c
c If the "in" keyword is found, it is replaced by the "out" keyword.
c
c Then "in" default functions for type conversion and getting machine
c constants (See "Q Defined..." lines above.) are searched for, and when
c found are replaced by the corresponding "out" functions.  When
c searching for names (here and below) they are only found if they are
c preceded and followed by a nonletter or nondigit.  Functions which do
c not have ``mach'' as their last four letters must also be followed by
c a ')'.
c
c Then the Vi's and vi's generated from the "C--L..." and "C--&" are
c searched for.  When found, L1 replaces the letter in the position
c of the "?", preserving the case of the letter that was there.
c
c Then, those strings defined by the ``C++ Replace...'' lines are
c searched for and replaced if found.
c
c Then, floating point constants are converted.  These are assumed to
c consist of (not a letter).(0 or more digits)(the letter used for
c floating point constants in the current version)(an optional + or
c -)(a digit).  If such a string is found, the letter is replaced as
c specified.
c
c Whether type conversion is on or not, lines consisting of only "END",
c or starting with "ENDPROGRAM" are searched for.  When found, it checks
c if there are any "C++..." actions still active or if the processing is
c not in "usual precision" mode.  In either of these cases a diagnostic
c given and the processing returns to normal mode.
c ______________________________________________________________________
c
c Most processing is done with a buffer which contains the line with
c blanks removed, and with letters converted to upper case.  In the
c case of "C++...", "C--...", and some lines from the control file,
c operators have also been removed from the processing buffer.
c
c *************************** Units and Files **************************
c
c   8  m77job  For reading the control information from m77job
c   9  input   For reading the current file.  (Also used for a scratch
c              file when input and output files are the same.)
c  10  output  For writing the generated file.
c  11  m77tmp  A scratch file used to save lines prior to getting a "C-
c  -"
c  12          For reading the comparison file.
c
c *************************** Variable Definitions *********************
c
c BUF1   Buffer used to hold an input line.
c BUF2   Buffer used to hold input line with blanks squeezed out, and
c    all lower case letters replaced by upper case letters.
c BUF3   Used as a temporary buffer.
c BUFDAT Buffer used to save last line when LDATA is not 0.
c C      Tempoary use of a character.
c CCODE  Logical variable set true when converting to C.
c CHGLIN Set .false. when starting the processing of a line, set .true.
c    if contents of the line change.
c CHRDAT Set true if data conversion of for character data.
c CNTRL  Buffer used to hold the names looked for in "C++" lines.  Also
c    see KTYPE, KVAL, and NAMEND.
c CDDEC  Set to .true. if converting to Fortran version used as input
c    for conversion to C, else set to .false.
c CONVOK This is .true. if conversion looks to be o.k., is .false. for
c    types I, P, X, C, Z,  and W, if we have not encountered a "C-- T4
c    Next line special..." line referencing the current type.
c DATHD  Part of BUF1, up to the first '(' for converting a data
c    generated for C, back to Fortran.
c I      Temporary index.
c I1     Temporary index.
c I2     Temporary index.
c ICVAL  Array holding values for names contained in CNTRL.
c II     Temporary index.
c INDXOP Array used to hold indices of operators corresponding to the
c    locations defined in LOCOP.
c INFDIF = starting location in INFUNC for the output intrinsics -
c    starting location for the input intrinsics.
c INFUNC = character array containing intrinsic function names that need
c    to be converted.
c IOP    Current operator index.
c ITYPE  Set as follows when getting the value of an operand.
c      < -4    -4 - IVAL points to the string in STRSAV, IVAL contains a
c              pointer to the end of the string.
c        -4    an integer value in IVAL
c  -3,-2,-1    logical values, .F., .U., .T., IVAL is not used.
c     = 0      set on an error
c     >0       a string in BUF2 starting at ITYPE, ending at IVAL.  This
c              is the result returned for a name that is not found.
c IVAL   Used in conjunction with ITYPE as described above.
c J      Temporary index.
c K      Temporary index.
C KDATA  Index assigned to array in next data statement.  See LDATA.
c KEYWRD Array holding possible keyword.  Locations 1, 2, and 3 for S,
c    D, And Q respectively.
c KEYBLK Array giving location of letter preceding a blank for
c    corresponding keyword in KEYWRD, = 0 if there is no blank.
c KNT    Used to track the state in converting old "C--..." lines.
c      = 0     Initial state.
c      = 1     Seen at least one of the first set.
c      = 2     Seen at least one of the second set.
c KOUNTB Count of lines left to blank when KVERS = 7.
c KOUNTC Count of lines left to comment when KVERS = 8.
c KOMINS Tells whether we are inserting a header comment.
c     =  0   We are not.
c     =  1   Insert done for the current file.
c     =  2   Insert is needed.
c KPT    The character corresponding to the i-th character in BUF2, can
c    be found in BUF1(KPT(i):KPT(i)).  KPT(0) is always 0.
c KSET   Index of last character used in saving the texti's from SET (or
c    default) instructions into STRSAV.
c KSETBG Value of KSET when starting a file.
c KTYPE  An array whose i-th entry is defined by the value of the name
c    starting in the i-th entry of CNTRL as follows.
c      < -4    -4 - KTYPE(i) points to the string in STRSAV, KVAL(i)
c              points to the end of the string.
c        -4    an integer value in KVAL(i).
c  -3,-2,-1    logical values, .F., .U., .T., KVAL(i) is not used.
c         0    A name does not start in this location.
c      >  0    A name does not start in this location, and the name
c              starting in location i-1 has this many characters.
c KVAL   Used in conjunction with KTYPE as described above.
c KVERS  defines the state of the program as follows.
c   1.  Usual state.
c   2.  Leaving actives active.
c   3.  Turning a block of inactive on.
c   4.  Turning a block of actives off.
c   5.  Leaving inactives inactive.
c   6.  Substitute on next line.
c   7.  Blanking column 1.
c   8.  Placing a C in column 1.
c   9.  Comment out lines till get one without '%%' in columns 1 and 2,
c       then uncomment the next line.
c  10.  Uncomment lines till get one without '%%' in columns 2 and 3,
c       then comment out the next line.
c   9.  Next line gets column 1 blanked.
c  10.  Next line get a C in column 1.
c        Also KVERS is negated when processing a "C++ Current has..."
c        line and is increased by 100 when processing a "C++ Replace..."
c        line.
c KXCERR Counter, counting down to 0 on comparison errors.
c L      Temporary index.
c LAST   Index in BUF2 of the last character on the line just read.
c LDAT   Number of characters saved in BUFDAT.
c LDATA  Flag to tell state of processing special data statements.
c     = 0    Usual case, nothing special being done for data statements.
c     = 1    Data is to be saved element by element, haven't seen first.
c     = 2    As for 1, after seeing the initial line of the data state.
c     =-1    Data to saved as an array, haven't seen first.
c     =-2    As for -1 after the initial line of the data statement.
c LDATHD Length of data stored in DATHD.
c LEND   Last character in name currently being examined.
c LENFUN Array giving the length of names in the INFUNC array.
c LENHED Number of characters in the heading text.
c LENKEY Gives the length of corresponding keyword in KEYWRD.  If 0, the
c    precision has not been defined.
c LENLIN Maximum number of characters on a line.
c LENNAM Array giving the length of each name in the NAMES array.
c LESSPR Usually 0, set to 1 if less precision is allowed.
c LETDIF Amount to add to index of upper case letter to get
c   corresponding index for a lower case letter.
c LETFUP Gives 'ED?' for getting floating point letter.
c LETTYP Characters used for type stored here, LETTYP = 'SDQICZW'.
c LIN    Letter for the file that is to be read.
c LINE   The current line number.
c LINEI  The initial line number -1 for the current program unit.
c LINLO  LIN as lower case.
c LINU   Value to which LINUP is restored on temporary changes.
c LINUP  LIN as upper case.
c LKEYI  Number of characters in the input type spec. keyword.
c LKEYO  Number of characters in the output type spec. keyword.
c LNAMDA Length of the array name for data, see LDATA.
c LNAMU  Last value of NUMNAM for usual name conversion.  Later
c   entries replace the "?" with the output type letter.
c LOCOP  Array used to hold locations of operators when a line is
c    scanned for operators.  The i-th location in LOCOP gives the
c    location of the i-th operator found in BUF2.
c LOCFUN Array containing starting indices for the intrinsic function
c    checks for the various data types.
c LOOK   Location in BUF1 at current point of processing.
c LOP    Last location used in LOCOP and INDXOP.
c LOUT   Letter for the object to be made, = ' ' means not yet set.
c LOUTLO LOUT as lower case.
c LOUTU  Value to which LOUTUP is restored on temporary changes.
c LOUTUP LOUT as upper case.
c LPATH  Number of character in the path name for the comparison file, 0
c    if there is no comparison file.
c LSTAT  defines the state of the program as follows.
c  -5   Processing the control file
c  -4   Looking for the first "C--..." line.
c  -3   After -4, looking for a non-"C--&..." line.
c  -2   Type conversion is not activated.
c  -1   Not used
c   0   The usual type conversion.
c   1   Special type conversion on current (or next) line.
c LSTOLD Array giving the locations for strings that are being replaced.
c   The i-th location gives starting location for the string i + 1.
c LSTNEW As for LSTOLD, except for strings that are inserted.
c LTYCI  Current index for the input type.  Associated letter is the
c     LTYCI character in the string 'SDQICZWPX'.
c LTYCO  As for LTYCI, but for the output type.
c LTYIN  Index for the input type as for LTYCI.
c LTYOUT As for LTYIN, but for the output type.
c LTYUI  Usual index for the the input type.
c LTYUO  As for LTYUI, but for the output type.
c MAP    An array of integers giving the "mapped" value for characters.
c     For a character, C, MAP(ichar(C)) is set as indicated by the
c     parameter names listed below.
c MAPPED Result of taking MAP(BUF2).
c MAPxxx gives parameter names corresponding to these "mapped" values.
c   MAPL   2  Lower case letters.
c   MAPUC   4  Upper case letters.
c   MAPDIG  5  A digit.
c   MAPOTH  6  Anything that is not mapped to something else below.
c   MAPDOT  7  .
c   MAPQM   8  ?
c   MAPNOT  9  ~
c   MAPLT  10  <
c   MAPGT  11  >
c   MAPDIV 12  /
c   MAPEQ  13  =
c   MAPMUL 14  *
c   MAPAND 15  &
c   MAPOR  16  |
c   MAPCOM 17  ,
c   MAPRP  18  )
c   MAPLP  19  (
c   MAPPLU 20  +
c   MAPMIN 21  -
c   MAPSTR 22  ' or "  (String delimiters)
c   MAPCAT 23 // value used for concatenation operator.
c MOREPR Usually 0, set to 1 if more precision is allowed.
c NAMBEG Value of NAMEND when starting a file.
c NAMDAT Name of array in modified data statement.
c NAMEND Index of last character of last name in CNTRL.  This is set to
c     its negative after processing a file.  If a SET instruction
c     appears between files, then the count is set back to 0 before
c     processing the SET values.  Before processing a file KSET is set
c     to its absolute value.
c NAMES  Array of names for comparisons from "C--" instructions.  Also
c     see NUMNAM, LENNAM, and NAMMRK.
c NAMMRK Array giving the location of the blanks in the names stored in
c     NAMES.  Gives offset from start of name -- 0 is first letter.
c NAMREF Array counting how many times a name is referenced.
c NERR   Number of errors encountered.
c NFILE  Number of files processed.
c NPDFT  Total number of intrinsics defined for all precisions.
c MXCERR Parameter giving the maximum number of errors on comparisons.
c NUMNAM Number of entries in the NAMES array, from the "MAKE..." line,
c     and from "C--..." statements.
c NUMSTR Number of replacement strings.
c MXCPPC Maximum number of characters in all names used by "C++" lines.
c    Values for this > 255 will work on most systems, but fail on some.
c MXNAME Maximum number of names allowed.
c MXVAL  Maximum number of values allowed by names in CNTRL.
c MXOP   Maximum number of operators on a line.
c MXST R  Maximum number of strings for replacement.
c PATH   Path part for the comparison file.
c SCRIN  Set = .true. when reading the scratch file.
c STROLD Used to store strings that are being replaced, see NUMSTR and
c    LSTOLD.
c STRNEW As for STROLD, but strings doing the replacing.
c STRSAV Character string used to save string values of variables.
c
c ********************** Specifications ********************************
c
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
      logical CHRDAT, SCRIN
      integer I, I1, I2, II, J, K, KXCERR, L, LDAT, LDATHD, LENHED,
     1  LINEI, LNAMDA
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
      integer MXCERR
      parameter (MXCERR = 10)
      character BUFDAT*72, DATHD*40, NAMDAT*20, LETTYP*9
      data LETTYP / 'SDQICZWPX' /
c Added for dealing with C conversion.
      character CCON(2)*6
      integer LCCON(2), ICT
      data CCON / 'FLOAT', 'DOUBLE' /
      data LCCON / 5, 6 /
c End of additions for C conversion


c Extra to flag problems evidently due to g77 bug.
      character FILNAM*20

c
c ******************** Formats *****************************************
c
 9010 format(' Finished processing, ', I4, ' files processed, ', I4,
     1  ' errors.')
c
c
c ****************** Start of Executable Code **************************
c
      OPEN(8, FILE='m77job', STATUS='OLD')
      call INIT
      call NAMCHK
 100  LSTAT = -5
      read(8, '(A)', END = 1100) BUF1
      KVERS = 1
      call PACKUP
      if ((BUF2(1:1) .eq. 'C') .or. (BUF2(1:1) .eq. '*')) then
        if (BUF1(2:3) .eq. '>>') then
          if (BUF1(4:) .gt. ' ') then
            KOMINS = 0
            HEADER = BUF1
            do 110 LENHED = LENLIN, 2, -1
              if (HEADER(LENHED:LENHED) .gt. ' ') go to 120
 110        continue
 120        if (LENHED .gt. 14) KOMINS = 1
          end if
        end if
        go to 100
      end if
c
      if ((BUF2(1:4).eq.'MAKE') .and. (KPT(4) .eq. 4)) then
        if (LAST .eq. 4) then
          LOUT = ' '
          go to 100
        end if
        LOOK = 5
        call GETMAK(LOUTUP, LOUTLO, LOUT, LTYOUT,LTYUO)
        LOUTU = LOUTUP
        if (LOOK .gt. LAST) then
          if (LOUTUP .eq. 'D') then
            LINUP = 'S'
            LINLO = 's'
            LTYIN = 1
          else
            LINUP = 'D'
            LINLO = 'd'
            LTYIN = 2
          end if
          LTYUI = LTYIN
          LIN = char(ichar(LINUP) + ichar(LOUT) - ichar(LOUTUP))
        else
          if (BUF2(LOOK:LOOK+3) .ne. 'FROM') call ERROUT(
     1      'Confused in "from" part of a "MAKE..." line')
          LOOK = LOOK + 4
          call GETMAK(LINUP, LINLO, LIN, LTYIN, LTYUI)
        end if
        LINU = LINUP
        LTYCO = LTYUO
        LTYCI = LTYUI
        go to 100
c
      else if ((BUF2(1:3) .eq. 'SET') .and. (KPT(3) .eq. 3)) then
        call PACKOP
        LOOK = 4
        IOP = 0
        if (BUF2(4:6) .eq. '.C.') then
          CCODE = INDXOP(1) .ne. MAPNOT
        else
          call GETASG
        end if
        go to 100
c
      else if ((BUF2(1:6).eq.'VERIFY').and.(KPT(6) .eq. 6)) then
        K = 6
 220    K = K + 1
        if (BUF1(K:K) .le. ' ') go to 220
        LPATH = index(BUF1(K:), ' ') - 1
        PATH = BUF1(K:K+LPATH)
        go to 100
c
      else if (BUF2(2:11) .eq. 'DEFINEDBY:') then
        call DEFTYP
        go to 100
c
      else if (BUF2(1:4) .eq. 'STOP') then
        go to 1100
c
      else if ((BUF2(1:4).ne.'FILE').or.(KPT(4) .ne. 4)) then
        if (BUF2 .le. ' ') go to 100
        call ERROUT('Unrecognized line')
      end if
      LSTAT = -4
      L = index(BUF1, '?')
      if (LOUT .le. ' ') then
        LOUTUP = ' '
        LSTAT = -2
        if (L.ne.0) call ERROUT('No processing -- No letter for "?"')
      else if (LENKEY(LTYUO) .eq. 0) then
        if (LOUTUP .ne. 'I') call ERROUT(
     1    'FATA error, Q precision is not defined')
      end if
      J = index(BUF1, ',')
      if (J .eq. 0) then
        K = index(BUF1(KPT(5):), ' ')
        if (K .ne. 0) then
          do 230 K = K + KPT(5), K + KPT(5) + 10
            if (BUF1(K:K) .gt. ' ') then
              J = K - 1
              go to 240
            end if
 230      continue
 240      continue
        end if
      end if
      K = 4
 300  K = K + 1
      if (BUF1(K:K) .le. ' ') go to 300
      if (L .ne. 0) BUF1(L:L) = LIN
      if (J .eq. 0) then
        I = K
 310    I = I + 1
        if (BUF1(I:I) .gt. ' ') go to 310
        I = I - 1
      else
        I = J
 320    I = I - 1
        if (BUF1(I:I) .le. ' ') go to 320
      end if
      BUF3 = BUF1(K:I) // ' ==> '
      II = I - K + 7
      if (L+J .eq. 0) then
c     No "?" or "ext" in "FILE" statement, copy input to a scratch file.
        OPEN(10, FILE=BUF1(K:I), STATUS='OLD')
        FILNAM = BUF1(K:I)


        OPEN(9, STATUS='SCRATCH')
 325    read(10, '(A)', END = 328) BUF2
        do 326 J = LENLIN, 2, -1
          if (BUF2(J:J) .gt. ' ') go to 327
 326    continue
 327    write (9, '(A)') BUF2(1:J)
        go to 325
 328    rewind(9)
        rewind(10)
      else
        OPEN(9, FILE=BUF1(K:I), STATUS='OLD')
c Set up letter for the output file.
        if (L .ne. 0) BUF1(L:L) = LOUT
        if (J .ne. 0) then
 330      J = J + 1
          if ((BUF1(J:J) .le. ' ') .and. (J .lt. LENLIN)) go to 330
          C = BUF1(J:J)
          if ((C .ne. '.') .and. (BUF1(J+1:J+1) .ne. '.')) then
            I = index(BUF1(J:), ' ') - 1
            BUF1(1:I) = BUF1(J:J+I-1)
            K = 1
          else
            L = J
 340        L = L + 1
            if (BUF1(L:L) .le. ' ') go to 340
 350        I = I - 1
            if (BUF1(I:I) .ne. C) then
              if (I .ge. K) go to 350
              stop 'No processing -- bad "ext" in "FILE" statement.'
            end if
            BUF2(1:L-J) = BUF1(J:L-1)
            BUF1(I:I+L-J-1) = BUF2(1:L-J)
            I = I + L - J - 1
          end if
        end if
        OPEN(10, FILE=BUF1(K:I), STATUS='UNKNOWN')
      end if
      print '(1X,A)', BUF3(1:II) // BUF1(K:I)
      if (NFILE .eq. 0) then
        OPEN(11, FILE='m77tmp', STATUS='UNKNOWN', FORM='UNFORMATTED')
      else
        rewind(11)
      end if
      KXCERR = 0
      if (LPATH .ne. 0) then
        OPEN(12, FILE=PATH(1:LPATH)//BUF1(K:I), STATUS='OLD')
        KXCERR = MXCERR
      end if
c
c ********* Main loop to read lines
c
      NAMEND = abs(NAMEND)
      NAMBEG = NAMEND
      KSETBG = KSET
      KOMINS = 2 * KOMINS
      KVERS = 1
      SCRIN = .false.
      CONVOK = LTYCO .le. 3
      LINEI = 0
      LINE = 0
      go to 560
c                      Get end of an upacked line.
 360  LAST = 1
      do 370 I = LENLIN, 2, -1
        if (BUF1(I:I) .gt. ' ') go to 375
 370  continue
 375  KPT(1) = I
 380  if (KOMINS .eq. 2) then
        if (BUF1(2:3) .eq. '>>') then
          BUF3(1:3) = BUF1(1:3)
          K = 3
 390      K = K + 1
          if (BUF1(K:K) .le. ' ') then
            BUF3(K:K) = ' '
            go to 390
          end if
          if (KOMINS .eq. 2) then
            KOMINS = -1
            I1 = K
            K = K + 9
            go to 390
          else if (KOMINS .lt. 0) then
            KOMINS = 1
            II = K
            K = K + index(BUF1(K:), ' ') - 1
            go to 390
          end if
          write(10, '(A)') BUF1(1:I1-1) // HEADER(4:13) //
     1      BUF1(II-1:K-1) // HEADER(14:LENHED)
        end if
      end if
 400  if (LDATA .ne. 0) then
c                         Take care of fixing data statements
        if (BUF1(1:1) .gt. ' ') then
          if (abs(LDATA) .eq. 1) go to 480
        end if
        if (BUF1(6:6) .le. ' ') go to 430
c                Checking if we have a continued line.
        if (LDATA .lt. 0) then
          if (BUF1(1:5) .ne. '     ') go to 430
        else
c If not character data, no continuations, else a messy test.
          if (.not. CHRDAT) go to 430
          K = index(BUF1(7:), '''')
          if (K .gt. 8) then
            K = K + 5
            if (BUF1(7:K) .le. ' ') then
 420          K = K + 2
              if (BUF1(K:K) .ne. '''') go to 430
              if (BUF1(K+1:K+1) .eq. '''') go to 420
            end if
          end if
        end if
c  Continued lines don't change what we already have for the prev.
        write(10, '(A)') BUFDAT(1:LDAT)
        LDAT = KPT(1)
        BUFDAT(1:LDAT) = BUF1(1:LDAT)
        go to 490
c
 430    if (abs(LDATA) .eq. 2) then
          call PACKUP
          if (LDATA .eq. 2) then
            if ((BUF1(1:5) .ne. '     ') .or.
     1        (BUF1(7:6+LNAMDA) .gt. ' ')) then
c                          End of last line is not changed.
              LDATA = 0
              go to 440
            end if
            if (LDAT .eq. 72) then
              BUFDAT(72:72) = '/'
            else
              BUFDAT(LDAT:LDAT+1) = ' /'
              LDAT = LDAT + 1
            end if
          else
            if ((BUF1(1:5) .ne. '     ') .or. (BUF2(1:LNAMDA) .ne.
     1        NAMDAT(1:LNAMDA))) then
c                          End of last line is not changed.
              LDATA = 0
              go to 440
            end if
            if (BUFDAT(LDAT-1:LDAT) .eq. ' /') LDAT = LDAT - 1
            BUFDAT(LDAT:LDAT) = ','
          end if
 440      write(10, '(A)') BUFDAT(1:LDAT)
        else if (BUF1(1:1) .gt. ' ') then
          go to 480
        else
          call PACKUP
          if (BUF2(1:4) .ne. 'DATA') go to 480
          LNAMDA = index(BUF2(1:LAST), '/')
          if (LNAMDA .eq. 0) then
            call ERROUT('Missing "/" in DATA statement')
            LDATA = 0
            go to 480
          end if
          CHRDAT = BUF2(LNAMDA+1:LNAMDA+1) .eq. ''''
          K = index(BUF2(1:LNAMDA), '(')
          if (((LDATA .eq. 1) .and. (K .ne. 0)) .or.
     1      ((LDATA .eq. -1) .and. (K .eq. 0))) then
c   Same kind of output, just leave code as it is.
            LDATA = 0
            go to 480
          end if
          if (LDATA .eq. -1) then
            LNAMDA = K
          else
            LDATA = 2
            LDATHD = index(BUF1, '/')
            BUF1(LDATHD:LDATHD) = ' '
 450        LDATHD = LDATHD - 1
            if (BUF1(LDATHD:LDATHD) .le. ' ') go to 450
            LDATHD = LDATHD + 1
            DATHD(1:LDATHD) = BUF1(1:LDATHD-1) // '('
          end if
          LNAMDA = LNAMDA - 1
          NAMDAT(1:LNAMDA) = BUF2(1:LNAMDA)
        end if
        if (LDATA .ne. 0) then
c              Get start of line for the different cases.
          LDAT = KPT(LAST)
          BUFDAT(1:LDAT) = BUF1(1:LDAT)
          if (LDATA .gt. 0) then
c                                Generate usual Fortran code.
            BUFDAT(1:LDATHD) = DATHD(1:LDATHD)
            K = LDATHD
            II = KDATA
            KDATA = KDATA + 1
            if (II .lt. 0) then
              II = -II
              BUFDAT(K:K) = '-'
              K = K + 1
            end if
            I1 = 1
 460        I1 = 10 * I1
            if (I1 .le. II) go to 460
 470        I1 = I1 / 10
            K = K + 1
            if (BUFDAT(K:K) .gt. ' ') call errout(
     1        'Overwriting code in converting C data to Fortran')
            BUFDAT(K:K) = char((II / I1) + NUM0)
            II = mod(II, I1)
            if (I1 .gt. 1) go to 470
            K = K + 1
            if (BUFDAT(K:K+1) .ne. '  ') call errout(
     1        'Overwriting code in converting C data to Fortran')
            if (BUFDAT(K+2:K+2) .le. ' ') then
              BUFDAT(K:K+2) = ') /'
            else
              BUFDAT(K:K+1) = ')/'
            end if
          else
            K = index(BUFDAT, '/')
            if (K .ne. 0) then
              if (LDATA .eq. -1) then
                I1 = index(BUFDAT, '(')
                if (I1 .lt. K) then
                  LDATA = -2
                  BUFDAT(I1:K) = ' '
                  BUFDAT(I1+1:I1+1) = '/'
                else
                  call errout(
     1              'Some kind of problem with data conversion.')
                end if
              else
                BUFDAT(6:K) = '* '
              end if
            else
              call errout(
     1          'Some kind of problem with data conversion, K=0.')
            end if
          end if
          go to 490
        end if
      else
        if (BUF1(2:2) .eq. '%') then
          if (KVERS .eq. 1) then
            if (CCODE) then
              if (BUF1(3:3) .eq. '%') then
                write(10, '(A)') BUF1(2:KPT(LAST))
                KVERS = 10
                go to 490
              end if
            else
              if (BUF1(1:1) .eq. '%') then
                write(10, '(A)') 'C' // BUF1(1:KPT(LAST))
                KVERS = 9
                go to 490
              end if
            end if
          end if
        end if
      end if
 480  write(10, '(A)') BUF1(1:KPT(LAST))
 490  if (BUF2(1:3) .eq. 'END') then
        if ((LAST .eq. 3) .or. (BUF2(4:10)  .eq. 'PROGRAM') .or.
     1    (BUF2(4:4) .eq. '!')) then
          LINEI = LINE
          if (LSTAT .gt. 0) then
            call ERROUT('Altered type conversion at end of program')
            LSTAT = 0
            LTYCO = LTYUO
            LTYCI = LTYUI
            if (LTYCO * LTYCI .eq. 0) then
              INFDIF = 0
            else
              INFDIF = LOCFUN(LTYCO) - LOCFUN(LTYCI)
            end if
          end if
          if (LSTAT .ne. -2) LSTAT = 0
          if (KVERS .ne. 1) then
            call ERROUT('C++ spec. active at end of program unit.')
            KVERS = 1
          end if
        end if
      end if
      if (KXCERR .gt. 0) then
c                             Comparing files.
 510    read(12, '(A)', END=1050) BUF3
        if ((BUF3(2:3) .eq. '--') .and. (BUF3(4:4) .ne. '-')) then
          if (BUF1(2:3).ne.'--') go to 510
        end if
 520    if (BUF1 .ne. BUF3) then
          if (BUF1(1:1) .ne. BUF3(1:1)) then
            if (abs(ichar(BUF1(1:1)) - ichar(BUF3(1:1)))
     1        .eq. LETDIF) then
              if (MAP(ichar(BUF1(1:1))) .le. MAPUC) then
                BUF3(1:1) = BUF1(1:1)
                go to 520
              end if
            end if
          end if
c May want to put check here for the header comment in BUF1, and if it
c is skip the diagnostic and one read of BUF3.
          I = 0
          J = 0
 530      I = I + 1
          if (I .le. LENLIN) then
            if (BUF1(I:I) .le. ' ') go to 530
          end if
 540      J = J +1
          if (J .le. LENLIN) then
            if (BUF3(J:J) .le. ' ') go to 540
          end if
          if ((I .le. LENLIN) .and. (J .le. LENLIN))then
            if (BUF1(I:I) .eq. BUF3(J:J)) go to 530
          else if (I .eq. J) then
            go to 560
          end if
          if ((BUF1(2:3) .ne. '--') .and. (BUF3(2:3) .ne. '++')) then
            call ERROUT(
     1        'Compare failed -- New file then compare file.')
            KXCERR = KXCERR - 1
            if (KXCERR .eq. 0) then
              call ERROUT(
     1          'Too many compare failures -- NO MORE WILL PRINT!')
            end if
          end if
        end if
      end if
 560  LINE = LINE + 1
      CHGLIN = .false.
      if (SCRIN) then
        read(11, END=570) BUF1
        go to 580
 570    SCRIN = .false.
      end if
      read(9, '(A)', END=1000, IOSTAT=I, ERR=990) BUF1
 580  if (BUF1(2:3) .eq. '++') then
        if (BUF1(4:4) .ne. '+') then
          if (SCRIN) go to 360
          call PROCPP
          if (KVERS .lt. 100) then
            if (LSTAT .ge. -2) go to 400
            go to 890
          end if
          KVERS = KVERS - 100
          go to 770
        end if
      end if
c
c                  Out of the initial state.
c              1    2    3    4    5    6    7    8    9   10
      go to (660, 660, 620, 600, 610, 630, 640, 650, 590, 615), KVERS
 590  if (BUF1(1:2) .ne. '%%') then

        KVERS = 1
        go to 620
      end if
c Turning a block of actives off.
 600  BUF2 = BUF1
      if (BUF2(LENLIN:LENLIN) .gt. ' ') then
        call ERROUT(
     1    'About to shift character off right end of line')
      end if
      BUF1 = 'C' // BUF2(1:LENLIN-1)
      if (BUF1 .eq. 'C ') BUF1(1:1) = ' '
 610  if ((BUF1(3:4) .eq. '--') .and. (BUF1(5:5) .ne. '-')) go to 670
      go to 660
c
 615  if (BUF1(2:3) .ne. '%%') then
        KVERS = 1
        go to 600
      end if
c Turning a block of inactives on.
 620  if (index('Cc*!', BUF1(1:1)) .eq. 0) then
        if (BUF1 .ne. ' ') call ERROUT('Expected a comment here')
      else
        BUF2 = BUF1
        BUF1 = BUF2(2:LENLIN)
      end if
      go to 660
c Doing a substituttion
 630  call SUBLIN
      go to 660
c Blanking column 1
 640  BUF1(1:1) = ' '
      KOUNTB = KOUNTB - 1
      if (KOUNTB .eq. 0) KVERS = 8
      go to 660
c Commenting column 1
 650  BUF1(1:1) = 'C'
      KOUNTC = KOUNTC - 1
      if (KOUNTC .eq. 0) KVERS = 1
c
 660  if (LSTAT .lt. 0) then
        if (LSTAT .lt. -2) go to 870
        if (LSTAT .eq. -2) then
          if ((BUF1(2:3) .eq. '--') .and. (BUF1(4:4) .ne. '-')) then
            if (LOUTUP .le. ' ') then
              if (index(BUF1, '?') .ne. 0) then
                call PACKUP
                if (MAPPED(4) .le. MAPUC) LOUTUP = BUF2(4:4)
              end if
            end if
          end if
          if (NUMSTR .eq. 0) go to 360
          go to 770
        end if
c              Just after seen an 'END' or 'ENDPROGRAM'
c## The code below appears to be unreachable.  It apparently is intended
c## that LSTAT could be set to -1 at the end of a subprogram, and the
c## the state set back to the initial state at that time.  Looks like we
c## could keep this feature, either by letting the user tell the code
c## somehow that they wanted it to work in that way, or we could make
c## a small change below and have the code use different names if there
c## were a "c--" line immediately following a subprogram.
        if ((BUF1(2:3) .eq. '--') .and. (BUF1(4:4) .ne. '-')) then
          if (index(BUF1, '?') .eq. 0) then
            LSTAT = 0
            LTYCO = LTYUO
            LTYCI = LTYUI
            if (LTYCO * LTYCI .eq. 0) then
              INFDIF = 0
            else
              INFDIF = LOCFUN(LTYCO) - LOCFUN(LTYCI)
            end if
            go to 680
          end if
        end if
c                   Reset the state, starting the scratch file.
        call NAMCHK
        rewind(11)
        LSTAT = -4
        go to 580
c## End of currently unreachable code.
      end if
      if ((BUF1(2:3) .ne. '--') .or. (BUF1(4:4) .eq. '-')) go to 680
c                    Do nothing for one of the initial "C--..." lines
 670  if (index(BUF1, '?') .ne. 0) go to 360
      call PACKUP
      if (BUF2(4:4) .ne. '-') call PROCMM
      go to 380
 680  call PACKUP
      if (INFDIF .le. 0) then
        if (INFDIF .eq. 0) go to 730
        if (INFDIF .eq. -1000) go to 840
      end if
c
c                                Take care of type conversion
c                           First the Keywords.
      L = LENKEY(LTYCI)
      K = index(BUF2(1:LAST), KEYWRD(LTYCI)(1:L))
      if (K .ne. 0) then
        if (BUF2(LENKEY(LTYCI)+K:LENKEY(LTYCI)+K) .ne. '(') then
          if (KPT(K) .lt. 7) go to 700
          if ((K .gt. 8) .and. (KPT(K-1) .gt. 6)) then
            if (index(BUF2(1:LAST), 'IMPLICIT') .ne. K - 8)
     1        go to 700
          else
            do 690 I = 2, K-1
              if (MAPPED(I) .ne. MAPDIG) go to 700
 690        continue
          end if
c                                               Treat as a keyword
          if (KEYBLK(LTYCI) .ne. 0) L = L + 1
          call COPY(KEYWRD(LTYCO), KPT(K), LENKEY(LTYCO),
     1      L, KEYBLK(LTYCO))
          BUF2(K:K+LENKEY(LTYCI)-1) = ' '
        end if
      end if
c          Take care of the processor defined functions.
 700  do 720 I = LOCFUN(LTYCI), LOCFUN(LTYCI+1) - 1
        J = 1
 710    K = index(BUF2(J:LAST), INFUNC(I)(1:LENFUN(I)))
        if (K .ne. 0) then
          K = K + J - 1
          J = K + 1
          I1 = KPT(K)
          if (MAP(ichar(BUF1(I1-1:I1-1))) .gt. MAPDIG) then
            I1 = K + LENFUN(I)
            if ((BUF2(I1:I1) .eq. '(') .or.
     1        (BUF2(I1-4:I1-1) .eq. 'MACH') .or.
     2        (index(BUF2(1:LAST), 'INTRINSIC') .ne. 0)) then
c                     Think we have an intrinsic function name.
              if (I + INFDIF .ge. LOCFUN(LTYCO+1)) then
                call ERROUT(
     1            'No output intrinsic for the input intrinsic')
              else
                call COPY (INFUNC(I+INFDIF), KPT(K),
     1            LENFUN(I+INFDIF), LENFUN(I), 0)
              end if
            end if
          end if
          if (J .lt. LAST) go to 710
        end if
 720  continue
      if (BUF1(1:1) .ne. ' ') then
c                  Special check for type conversion in C.
        if (LTYCI .le. 2) then
          J = 1
 725      continue
          K = index(BUF2(J:LAST), CCON(LTYCI))
          if (K .ne. 0) then
            K = K + J - 1
            J = K + 1
            if (BUF2(K-1:K-1) .eq. '(') then
              I1 = KPT(K)
              ICT = INFDIF / 2
              if (MAP(ichar(BUF1(I1-1:I1-1))) .gt. MAPDIG) then
                I1 = K + LCCON(I)
                call COPY (CCON(LTYCI+ICT), KPT(K),
     1            LCCON(LTYCI+ICT), LCCON(LTYCI), 0)
              end if
            end if
            if (J .lt. LAST) go to 725
          end if
        end if
      end if
c
c        Take care of the user names
 730  do 760 I = 1, NUMNAM
        J = 1
 740    K = index(BUF2(J:LAST), NAMES(I)(1:LENNAM(I)))
        if (K .ne. 0) then
          K = K + J - 1
          J = K + LENNAM(I)
          I1 = KPT(K)
          if (I1 .gt. 1) then
            if (MAP(ichar(BUF1(I1-1:I1-1))) .gt. MAPDIG) then
              I1 = KPT(J - 1) + 1
              if (I1 .lt. LENLIN) then
                if (MAP(ichar(BUF1(I1:I1))).le.MAPDIG) go to 750
              end if
              NAMREF(I) = NAMREF(I) + 1
              K = K + NAMMRK(I)
              II = KPT(K)
              C = BUF1(II:II)
              CHGLIN = .true.
              if (I .le. LNAMU) then
                if (C .eq. LINUP) then
                  BUF1(II:II) = LOUTUP
                else
                  BUF1(II:II) = LOUTLO
                end if
              else
                BUF1(II:II) = LETTYP(LTYOUT:LTYOUT)
                if (C .ne. LETTYP(LTYIN:LTYIN)) then
                  BUF1(II:II) = char(ichar(BUF1(II:II)) + LETDIF)
                end if
              end if
            end if
          end if
 750      if (J .le. LAST) go to 740
        end if
 760  continue
c
 770  if (NUMSTR .ne. 0) then
c                 Take care of string replacements
        J = 1
        BUF3 = ' '
        do 800 I = 1, NUMSTR
          II = J
          J = LSTOLD(I)
          L = 1
 780      K = index(BUF1(L:), STROLD(II:J-1))
          if (K .ne. 0) then
c   Found the old string, set K=loc., I1=len. of old, L=end of old.
            K = K + L - 1
            I1 = J - II
            L = K + I1 - 1
            if (BUF3(K:L) .gt. ' ') go to 780
            BUF1(K:L) = ' '
            I2 = LSTNEW(I) - LSTNEW(I-1)
            if (I1 .gt. I2) then
c When old is bigger try to sop up extra blanks.
              I1 = min(KPT(LAST), I2 + K + 1)
 790          L = L + 1
              if ((BUF1(L:L).le.' ') .and. (L.lt.I1)) go to 790
              L = L - 1
              I1 = L - K
            end if
            call COPY(STRNEW(LSTNEW(I-1):), K, I2, I1, 0)
            if (KPT(LAST) .lt. K+I2) KPT(LAST) = K + I2 - 1
            go to 780
          end if
 800    continue
        if (LSTAT .lt. -2) go to 890
        if (LSTAT .eq. -2) go to 360
      end if
c                       Look for floating point constants
      I1 = 1
 810  K = index(BUF2(I1:LAST), '.')
      if (K * LTYCI .ne. 0) then
        K = I1 + K - 1
        if (MAPPED(K-1) .gt. MAPUC) then
          do 820 K = K + 1, LAST - 1
            if (MAPPED(K) .ne. MAPDIG) then
              if (BUF2(K:K) .eq. LETFUP(LTYCI:LTYCI)) then
                if ((MAPPED(K+1).eq.MAPDIG) .or. (BUF2(K+1:K+1).eq.
     1            '-') .or. (BUF2(K+1:K+1) .eq. '+')) then
                  J = KPT(K)
                  C = LETFUP(LTYCO:LTYCO)
                  if (MAP(ichar(BUF1(J:J))) .le. MAPLC) C =
     1              char(ichar(C) + LETDIF)
                  if (C .le. ' ') then
                    call ERROUT(
     1                'Trying to convert floating constant to integer')
                  else
                    BUF1(J:J) = C
                    CHGLIN = .true.
                  end if
                end if
              end if
              go to 830
            end if
 820      continue
        end if
 830    I1 = K + 1
        if (I1 .lt. LAST) go to 810
      end if
 840  if (LSTAT .eq. 0) go to 360
      LSTAT = 0
c
c          Set indices for type conversion
 850  LTYCO = LTYUO
      LOUTUP = LOUTU
      if (LINUP .ne. LINU) then
        LINUP = LINU
        do 860 I = 1, min(LNAMU, NUMNAM)
          K = NAMMRK(I) + 1
          NAMES(I)(K:K) = LINUP
 860    continue
      end if
      if (LTYCI .ne. LTYUI) then
        LTYCI = LTYUI
        C = LETTYP(LTYCI:LTYCI)
        do 865 I = LNAMU+1, NUMNAM
          K = NAMMRK(I) + 1
          NAMES(I)(K:K) = C
 865    continue
      end if
      if (LTYCO * LTYCI .eq. 0) then
        INFDIF = 0
      else
        INFDIF = LOCFUN(LTYCO) - LOCFUN(LTYCI)
      end if
      if (.not. SCRIN) go to 380
      go to 560
c
c                              In the initial state
 870  if ((BUF1(2:3) .eq. '--') .and. (BUF1(4:4) .ne. '-')) go to 900
 890  if (LSTAT .eq. -3) then
c                      Done with the initial phase
        LSTAT = 0
        go to 950
      else
        write(11) BUF1
        go to 560
      end if
 900  call PACKUP
c
      C = BUF2(4:4)
      if (index(BUF1, '?') .eq. 0) then
        if (index(BUF2, 'MASKCODECHANGE') .ne. 0) go to 890
        if (BUF2(5:20) .eq. 'NEXTLINESPECIAL:') go to 890
c    Following test is temporary until we get rid of these lines.
        if (BUF2(5:30) .eq. 'USEDFORUSUALPRECISIONBELOW') go to 970
c                 Take care of old form "C--..." lines.
        LOOK = 0
        call GETNAM
        if (KNT .eq. 1) go to 560
        K = index(BUF2(1:LAST), 'VERSIONUSES')
        if (K .eq. 0) then
          if (BUF2(4:4) .ne. '&') then
c               call ERROUT('Unrecognized "C--..." line ignored')
            go to 890
          end if
        end if
        LTYUI = 2
        if (LTYUO .eq. 2) LTYUI = 1
        LTYCI = LTYUI
        I = index(BUF1, 'real')
        if (I .eq. 0) I = index(BUF1, 'REAL')
        if (I .eq. 0) I = index(BUF1, 'dble')
        if (I .eq. 0) I = index(BUF1, 'DBLE')
        if (I .ne. 0) then
          J = I + 3
 910      J = J + 1
          if (J .lt. LENLIN) then
            if ((BUF1(J:J) .le. ' ') .or. (BUF1(J:J) .eq. ','))
     1        go to 910
            BUF3 = BUF1
            BUF1(I:) = BUF3(J:)
          else
            BUF1(I:) = ' '
          end if
        end if
        I = index(BUF1, 'r1mach')
        if (I .eq. 0) I = index(BUF1, 'R1MACH')
        if (I .eq. 0) I = index(BUF1, 'd1mach')
        if (I .eq. 0) I = index(BUF1, 'D1MACH')
        if (I .ne. 0) then
          J = I + 5
 920      J = J + 1
          if (J .lt. LENLIN) then
            if ((BUF1(J:J) .le. ' ') .or. (BUF1(J:J) .eq. ','))
     1        go to 920
            BUF3 = BUF1
            BUF1(I:) = BUF3(J:)
          else
            BUF1(I:) = ' '
          end if
        end if
        I = LENLIN
 930    I = I - 1
        if (BUF1(I:I) .le. ' ') go to 930
        if (BUF1(I:I) .eq. ',') BUF1(I:I) = ' '
c                          Replace the "version uses" text
        if (K .ne. 0) then
          BUF3 = BUF1(KPT(K+11):)
          BUF1(4:) = LOUTUP // ' replaces "?": ' // BUF3
        end if
        go to 960
      else if (MAPPED(4) .le. MAPUC) then
        if (LSTAT .eq. -3) then
          if (BUF2(5:22) .eq. '(TYPE)REPLACES"?":') then
            if (LETTYP(LTYIN:LTYIN) .ne. C) then
              call ERROUT(
     1          'Stopping, wrong letter for type in input file.')
              go to 1100
            end if
            BUF1(4:4) = LETTYP(LTYOUT:LTYOUT)
            if (MAPPED(4) .le. MAPLC) BUF1(4:4) =
     1        char(ichar(BUF1(4:4)) + LETDIF)
            LNAMU = NUMNAM
            LOOK = 23
            call GETNAM
            go to 960
          end if
          call ERROUT('"C-- letter ..." line used more than once')
        end if
        if (BUF2(5:16) .eq. 'REPLACES"?":') then
          if (LINUP .ne. C) then
            call ERROUT('Stopping, wrong letter in input file.')
            go to 1100
          end if
          BUF1(4:4) = LOUTUP
          if (MAPPED(4) .le. MAPLC) BUF1(4:4) =
     1      char(ichar(LOUTUP) + LETDIF)
          LSTAT = -3
          LOOK = 17
          call GETNAM
          go to 960
        end if
      else if (BUF2(4:4) .eq. '&') then
c            Should be processing continuations.
        LOOK = 5
        call GETNAM
        go to 960
      end if
      call ERROUT('Unknown type of "C--..." line')
 950  SCRIN = .true.
      LINE = LINEI
 960  write(11) BUF1
 970  if (.not. SCRIN) go to 560
      rewind(11)
      go to 850

 990  continue
c 5008 is value gfortran give for reading beyond an end of file.
      if (I .ne. 5008) print
     1  '(''Error with status = '', I14, ''On file '', A)', I, FILNAM
c                             Got an end of file.
 1000 if (LSTAT .ge. -2) then
c             At the end of a program unit as well as EOF.
        call NAMCHK
        NFILE = NFILE + 1
        close(9)
        if (CONVOK) then
          close(10)
        else
          close(10, STATUS = 'DELETE')
          call ERROUT(
     1      'No output file -- no special lines for the output type')
        end if
        if (LPATH .ne. 0) then
 1010     read(12, '(A)', END=1030) BUF1
          if (BUF1 .le. ' ') go to 1010
          call ERROUT('Input file ended before Verify file')
 1030     close(12)
        end if
      else
c            No "C--..." found, reprocess the scratch file.
        LSTAT = -2
        SCRIN = .true.
        LINE = LINEI
        rewind(11)
        go to 560
      end if
      go to 100
c                         End of the comparison file
 1050 if (BUF1 .le. ' ') go to 560
      call ERROUT('Verify file ended prematurely')
      KXCERR = 0
      go to 560
c
c                         End of the control file.
 1100 close (11, STATUS = 'DELETE')
      print 9010, NFILE, NERR
      stop
      end
C   End of main program -- M77CON

      subroutine PACKUP
c Copies the nonblanks from BUF1 into BUF2 in order.  Lower case letters
c are replaced by their upper case equivalents.
c KPT(i) is set to the location in BUF1 corresponding to the i-th
c character in BUF2,  MAPPED(i) is set to the value of the mapping
c associated with the character in BUF2(i:i).  LAST is set to the index
c of the last character in BUF2.
c
      integer I, J
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
      integer K
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
c ISTRT in effect shifts line left when it is shifted right.
      integer ISTRT(0:8)
      data ISTRT / 4*1, 2*2, 3*1 /
c
      LAST = 0
      do 200 I = ISTRT(KVERS), LENLIN
        C = BUF1(I:I)
        if (C .gt. ' ') then
          J = ichar(C)
          K = MAP(J)
          if (K .le. MAPLC) C = char(J - LETDIF)
          LAST = LAST + 1
          BUF2(LAST:LAST) = C
          KPT(LAST) = I
          MAPPED(LAST) = K
        end if
  200 continue
      BUF2(LAST+1:LENLIN) = ' '
      return
      end
c   End of PACKUP

      subroutine COPY(SRC, K1, KOUT, KIN, KBLKO)
c Copy KOUT characters of SRC starting at BUF1(K1:K1), where KIN
c characters of BUF1 are to be replaced.  If KBLKO is not zero, then
c insert a blank after copying KBLKO characters.  Characters in SRC are
c assumed to be upper case if the location copied to in BUF1 constains a
c letter.
c
      character SRC*(*)
      integer K1, KOUT, KIN, KBLKO
      logical LC
      integer I, J, KK
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
c                               Temporary indices.
      integer I1, I2, II, ID, JJ, K
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
c
      CHGLIN = .true.
      K = K1
      KK = KOUT
      if (KBLKO .ne. 0) KK = KK + 1
      if (KK .gt. KIN) then
        do 100 I = K + KIN, K + KK - 1
          if (BUF1(I:I) .gt. ' ') then
c                              Need to shift the line right.
            BUF3(1:) = BUF1(I:LENLIN)
c                       J = place in BUF1 where text in BUF3 goes.
            J = K + KK
            JJ = LENLIN - J + 1
            if (JJ .gt. 0) then
              if (BUF3(JJ+1:) .le. ' ') then
                if (MAP(ichar(BUF1(I:I))) .le. MAPDIG) then
                  BUF1(J:J) = ' '
                  J = J + 1
                end if
                go to 80
              end if
            end if
            call ERROUT('Shifting off text at end of line')
            return
 80         BUF1(J:LENLIN) = BUF3(1:)
            II = J - I
            do 90 I1 = LAST, 1, -1
              KPT(I1) = min(LENLIN, KPT(I1) + II)
              if (KPT(I1) .le. J) go to 110
   90          continue
               go to 105
             end if
  100    continue
  105    if (KPT(LAST) .lt. K+KK) then
c                    Replacement is at the very end of the line.
           if (KBLKO .ne. 0) KK = KK + 1
           KPT(LAST) = K + KK
         end if
       else if (KK .lt. KIN) then
         BUF1(K+KK:K+KIN-1) = ' '
         if (KPT(LAST) .eq. K+KIN-1) KPT(LAST) = K + KK - 1
       end if
 110   ID = K - 1
       II = KOUT
       if (KBLKO .ne. 0) II = KBLKO
       LC = .false.
       I1 = 1
c               Do the copy.
 120   do 150 I = I1, II
         J = I + ID
         if (I .le. KIN) then
           I2 = MAP(ichar(BUF1(J:J)))
           if (I2 .le. MAPUC) LC = I2 .le. MAPLC
         end if
         BUF1(J:J) = SRC(I:I)
         if (map(ichar(BUF1(J:J))) .le. MAPUC) then
           if (LC) BUF1(J:J) = char(ichar(BUF1(J:J)) + LETDIF)
         end if
  150  continue
       if (II .eq. KOUT) return
       BUF1(J+1:J+1) = ' '
       II = KOUT
       I1 = I
       ID = K
       go to 120
       end
c   End of COPY

      subroutine DEFTYP
c This subroutine gets things connected with a type.
c
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
      integer I, K
c
c ****************** Start of Executable Code **************************
c
      call PACKUP
      I = index('SDQICZW', BUF2(1:1))
      if (I .eq. 0) call ERROUT('Unknown type specification')
      if (BUF2(2:11) .ne. 'DEFINEDBY:') call ERROUT(
     1  'Spelling problem with ''Q Defined...''?')
      if (MAPPED(12) .ge. MAPDIG) call ERROUT(
     1  'Need a letter here')
      if (MAPPED(12) .ge. MAPDIG) call ERROUT(
     1  'Need a letter here')
      LOOK = 12
      if (I .le. 3) then
c                       Get the letter for floating point.
        LETFUP(I:I) = BUF2(12:12)
        LETFUP(I+4:I+4) = BUF2(12:12)
        if (BUF2(13:13) .ne. ',') go to 40
        LOOK = 14
      end if
      if (LOOK .gt. LAST) go to 40
      LEND = index(BUF2(LOOK:LAST), ',') - 1
      if (LEND .lt. 0) go to 40
      LENKEY(I) = LEND
      LEND = LOOK + LEND - 1
      KEYWRD(I) = BUF2(LOOK:LEND)
      KEYBLK(I) = 0
      if (LEND - LOOK .lt. KPT(LEND) - KPT(LOOK))
     1  KEYBLK(I) = index(BUF1(KPT(LOOK):), ' ')
      K = LOCFUN(I) - 1
 20   K = K + 1
      if (K .eq. LOCFUN(I+1)) return
      LOOK = LEND + 2
      if (LOOK .ge. LAST) go to 40
      LEND = index(BUF2(LOOK:LAST), ',') - 2
      if (LEND .lt. 0) then
        if (K .ne. LOCFUN(I+1) - 1) go to 40
        LEND = LAST
      else
        LEND = LOOK + LEND
      end if
      INFUNC(K) = BUF2(LOOK:LEND)
      LENFUN(K) = LEND - LOOK + 1
      go to 20
c                            Problems with ','s
 40   LOOK = KPT(LOOK)
      call ERROUT('Expecting a '','' after this point')
      return
      end
c   End DEFTYP

      subroutine ERROUT(MSG)
c   Prints error messages, errors in control file stop.  Assumes that
c   KPT(LOOK) is pointing someplace close to the error.
      character MSG*(*)
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
c
 10   format(' Following line in control file aborted processing.'/ 1X,
     1  A / A, '^^^' / ' Approximate location in line indicated above.')
 20   format(1X, A/ I5, ': ',A)
c
      if (LSTAT .eq. -5) then
        print '(1X, A)', MSG
        BUF3 = ' '
        print 10, BUF1, BUF3(1:max(1, KPT(LOOK)))
        stop
      else
        NERR = NERR + 1
        print 20, MSG, LINE, BUF1
        if (MSG(1:14) .eq. 'Compare failed') then
          print '(7X, A)', BUF3
        end if
      end if
      end
c   End of ERROUT

      subroutine GETAND
c Enter with LOOK pointing to the start of the item in BUF2, and LEND
c pointing to its end.  Exits with LOOK pointing to the next operand in
c BUF2, and with IVAL set depending on how ITYPE is set as described in
c the header comments way above.
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
      integer I, J, K, II
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
      character LETTYP*9
      data LETTYP / 'SDQICZWPX' /
c
      if (KPT(LOOK) .gt. 1) then
        if (MAPPED(LOOK) .eq. MAPSTR) then
c                           Got a character string.
          ITYPE = LOOK + 1
          IVAL = LEND - 1
          LOOK = LEND + 1
          return
        end if
      end if
      IVAL = 0
      do 100 I = LOOK, LEND
        if (MAPPED(I) .ne. MAPDIG) go to 200
        IVAL = 10 * IVAL + ichar(BUF2(I:I)) - NUM0
  100 continue
      LOOK = I
      ITYPE = -4
      return
c
 200  if (BUF2(LOOK:LOOK) .eq. '.') then
        if (LEND - LOOK .eq. 2) then
          if (BUF2(LOOK+2:LOOK+2) .eq. '.') then
c                                    May be logical constant
            C = BUF2(LOOK+1:LOOK+1)
            if (C .eq. 'T') then
              ITYPE = -1
            else if (C .eq. 'F') then
              ITYPE = -3
            else if (C .eq. 'C') then
              ITYPE = -3
              if (CCODE) ITYPE = -1
            else if (C .eq. 'U') then
              ITYPE = -2
            else if (C .eq. 'X') then
              ITYPE = LOOK + 1
              IVAL = ITYPE
              BUF2(IVAL:IVAL) = LETFUP(LTYOUT:LTYOUT)
            else if (C .eq. 'N') then
              ITYPE = LOOK + 1
              IVAL = ITYPE
              BUF2(IVAL:IVAL) = LOUTUP
            else if (C .eq. 'Y') then
              ITYPE = LOOK + 1
              IVAL = ITYPE
              BUF2(IVAL:IVAL) = LETTYP(LTYOUT:LTYOUT)
            else if ((C.eq.'D') .or. (C.eq.'S') .or. (C.eq.'I')) then
              if (C .eq. LOUTUP) then
                ITYPE = -1
              else if (LOUTUP .eq. ' ') then
                ITYPE = -2
              else
                ITYPE = -3
              end if
            else
              go to 210
            end if
            LOOK = LOOK + 3
            if (ITYPE .gt. 0) then
              if (MAPPED(LOOK-2) .eq. MAPLC) BUF2(IVAL:IVAL) =
     1          char(ichar(BUF2(IVAL:IVAL)) + LETDIF)
            end if
            return
  210       continue
          end if
        end if
      else if (BUF2(LOOK:LOOK) .eq. '{') then
        K = index(BUF2(LOOK:LAST), '}')
        if (K .eq. 0) then
          K = LAST
          ITYPE = -2
        else
          K = LOOK + K - 1
          ITYPE = index(BUF2(LOOK:K), LOUTUP) - 3
          if (ITYPE .ne. -3) ITYPE = -1
        end if
 220    if (LOCOP(IOP) .lt. K) then
          IOP = IOP + 1
          go to 220
        end if
        LOOK = K + 1
        return
      end if
c                                    Looks like a name
      ITYPE = LOOK
      IVAL = LEND
      LOOK = LEND + 1
      I = index(CNTRL, BUF2(ITYPE:LEND))
      if (I .ne. 0) then
 230    J = KTYPE(I+1)
        if (J .lt. 0) J = 1
        if ((KTYPE(I) .ge. 0) .or. (J .ne. LOOK - ITYPE)) then
c                     If first match didn't work out, check for another.
          II = index(CNTRL(I+1:), BUF2(ITYPE:LEND))
          if (II .ne. 0) then
            I = I + II
            go to 230
          end if
        else
c                                    Found a name
          ITYPE = KTYPE(I)
          IVAL = KVAL(I)
        end if
      end if
      return
      end
c   End of GETAND

      subroutine GETASG
c Subroutine to process a assignments.  Enter with LOOK = the
c location of the first operand in BUF2, for the first assignment.
c
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
      integer I, J, K, LBEQ, NOP
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
      character CHRLOG(-3:-1)*3
      data CHRLOG / '.F.', '.U.', '.T.' /
c
      if (NAMEND .le. 0) then
c                      Get KTYPE initialized.
        KSET = 0
        NAMEND = 0
        do 10 I = 2, MXCPPC
          KTYPE(I) = 0
  10    continue
        CNTRL = ' '
      end if
c
 20   IOP = IOP + 1
      if (IOP .gt. LOP) return
      NOP = INDXOP(IOP)
      LEND = LOCOP(IOP)
      if (LEND .lt. LOOK) then
c              Did not start assignment with an operand.
        call ERROUT('Assignent did not start with an operand')
        return
      end if
      CALL GETAND
      if (ITYPE .le. 0) then
c          Found something defined on the left side of an assignment.
        if (LSTAT .eq. -5) then
c                                On a "SET..." line
          call ERROUT('Variable is already defined')
        end if
        if (KVERS .lt. 0) then
c           Want to set right hand side for a "C++ Current has..." line.
          K = KPT(LEND) + 1
          J = index(BUF1(K:), '=')
          if (J .eq. 0) then
            call ERROUT('Missing "=" in "C++ Current has..." line')
          else
            LOOK = J + K
            BUF1(LOOK:) = ' '
            if (ITYPE .lt. -4) then
              BUF3 = STRSAV(-4-ITYPE:IVAL)
              K = IVAL + ITYPE + 5
            else
              K = 3
              BUF3(1:3) = CHRLOG(ITYPE)
            end if
 30         call COPY(BUF3, LOOK+1, K, 0, 0)
          end if
          return
        end if
c                Skip to next assignment.
 40     IOP = IOP + 1
        LOOK = LOCOP(IOP)
        if (INDXOP(IOP) .ne. MAPCOM) go to 40
        LOOK = LOOK + 1
        go to 20
      end if
      if (KVERS .ge. 100) then
c                     Process "C++ Replace..." instruction.
        if (NUMSTR .ge. MXSTR-1) then
          call ERROUT('Too many replacements -- Make MXSTR bigger')
          stop'Abort processing -- Recompile M77CON with bigger MXSTR'
        end if
        LSTOLD(NUMSTR+1) = LSTOLD(NUMSTR) + IVAL - ITYPE + 1
        if (LSTOLD(NUMSTR+1) .gt. MXCPPC) go to 200
        STROLD(LSTOLD(NUMSTR):LSTOLD(NUMSTR+1)-1) = BUF2(ITYPE:IVAL)
        if (NOP .eq. MAPEQ) then
          call GETEXP
          if (ITYPE .le. 0) then
            if (ITYPE .lt. -4) then
              LSTNEW(NUMSTR+1) = LSTNEW(NUMSTR)+IVAL+ITYPE+5
              if (LSTNEW(NUMSTR+1) .ge. MXCPPC) go to 200
              STRNEW(LSTNEW(NUMSTR):LSTNEW(NUMSTR+1)-1) =
     1          STRSAV(-4-ITYPE:IVAL)
            else
              call ERROUT('Right hand side not defined')
              return
            end if
          else
            LSTNEW(NUMSTR+1) = LSTNEW(NUMSTR) + IVAL - ITYPE + 1
            if (LSTNEW(NUMSTR+1) .ge. MXCPPC) go to 200
            STRNEW(LSTNEW(NUMSTR):LSTNEW(NUMSTR+1)-1) =
     1        BUF2(ITYPE:IVAL)
          end if
          NUMSTR = NUMSTR + 1
          go to 20
        end if
        call ERROUT('Missing "=" sign in "REPLACE..." line')
        return
      end if
c                               Got character string on left of "=
c            Enter the name at LBEQ in CNTRL
 60   LBEQ = NAMEND + 1
c            Save the length of the name.
      KTYPE(LBEQ+1) = IVAL - ITYPE + 1
      NAMEND = NAMEND + KTYPE(LBEQ+1)
      if (NAMEND .gt. MXCPPC) go to 200
c        Save the text and assume a value of .true.
      CNTRL(LBEQ:NAMEND) = BUF2(ITYPE:IVAL)
      KTYPE(LBEQ) = -1
      if (NOP .eq. MAPCOM) go to 20
      if (NOP .eq. MAPEQ) then
        call GETEXP
        if (ITYPE .lt. 0) then
c                                    Found a name
          KTYPE(LBEQ) = ITYPE
          KVAL(LBEQ) = IVAL
        else if (ITYPE .ne. 0) then
c                                    Save the string.
          K = IVAL - ITYPE + 1
          J = KSET + 1
          KSET = KSET + K
          if (KSET .le. MXCPPC) then
            STRSAV(J:KSET) = BUF2(ITYPE:IVAL)
            KTYPE(LBEQ) = -J - 4
            KVAL(LBEQ) = KSET
          end if
        end if
        go to 20
      else
        call ERROUT('Syntax on assignment is confusing')
 100    IOP = IOP + 1
        if (IOP .lt. LOP) then
          if (INDXOP(IOP) .eq. MAPCOM) go to 20
          go to 100
        end if
      end if
      return
 200  call ERROUT('Out of name space -- Make MXCPPC bigger')
      stop 'Abort processing -- Recompile M77CON with bigger MXCPPC'
      end
c   End of GETASG

      subroutine GETEXP
c Evaluates expresstions, returns a type and value (ITYPE, IVAL) as for
c GETAND.
c Operators have the same indices as they have in MAPPED(), except
c for those which are generated here.  Namely the unary operators,
c the ( paren that is placed on the stack, and ** when it is placed on
c the stack.  The indices for these
c generated operators, and the ones not obtained directly from MAP()
c are as follows:
c   1 Unary +      2 Unary -        3 "(" on stack   4 ~=
c   5 <=           6 >=             7 /=             8 ==
c   0 **           22 ** on stack  23 //
c The remaining starting with      9 ~      are as generated by MAP.
c
c IANDIV Gives value for IVAL on the operand stack.
c IANDTY Gives value for ITYPE on the  operand stack.
c ISTKOP Contains the indices for the operators on the operator stack.
c LAMAP  is mapping giving the value for a logical "and" expression.
c LAND   Index of the top of the operand  stack.
c LENSTK is a parameter giving the length of the operator and operand
c   stacks.
c LOMAP  is mapping giving the value for a logical "or" expression.
c LOR    Index of the top of the operator stack.
c LPREC gives the precedence of the operators, a higher index means the
c   operator binds more tightly.  An operator is pushed on the operator
c   stack  if it has a higher precedence than the operator on the stack.
c   If it has a lower precedence the operator on the operator stack is
c   evaluated.
c XAND  is .true. when expecting an operand, and  .false. when expecting
c   an operator.   If .true. and get an operator, it is unary.
c
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
      integer MAPEXP
      parameter (MAPEXP = 0)
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
c
      integer LENSTK
      parameter (LENSTK = 20)
      logical XAND
      integer IANDIV(0:LENSTK), IANDTY(0:LENSTK), ISTKOP(LENSTK),
     1  LOMAP(9), LAMAP(9), LPREC(0:23)
      integer IT1, IT2, IV1, IV2, J, K, L, LAND, LOR, NOPS, NOPX
c                  TT  TU  TF  UU  xx  UF  xx  xx  FF
      data LOMAP / -1, -1, -1, -2, -2, -2, -2, -2, -3 /
      data LAMAP / -1, -2, -3, -2, -2, -3, -2, -2, -3 /
c Sx means on stack, Ux means unary.
c                   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14
c                  ** S( U+ U- ~= <= >= /= ==  ~  <  >  /  =  *
      data LPREC / 18, 4,20,20,10,10,10,10,10,12,10,10,14,10,14,
     1  8, 8, 2, 4,22,12,12,16, 6 /
c         &  |  ,  )  (  +  - S** //
c        15 16 17 18 19 20 21 22  23
c
c ****************** Start of Executable Code **************************
c
      LAND = 0
      LOR = 1
c Initialize stacks.
      IANDIV(0) = 0
      IANDTY(0) = 0
      ISTKOP(1) = MAPCOM
c
 10   IOP = IOP + 1
 20   XAND = .true.
 30   if (LOCOP(IOP) .ge. LOOK) then
c                                  Got a operand
        if (XAND) then
c                                  Get value on operand stack.
          LEND = LOCOP(IOP)
          call GETAND
          LAND = LAND + 1
          IANDTY(LAND) = ITYPE
          IANDIV(LAND) = IVAL
          XAND = .false.
          go to 30
        end if
        stop 'Bug -- M77CON should insure operands are separated.'
      end if
c                                 Process an operator.
      NOPX = INDXOP(IOP)
 100  if (LPREC(ISTKOP(LOR)) .lt. LPREC(NOPX)) then
c                                 Put new operator on the stack
        if (XAND) then
c                                 Operator is unary or an error.
          if (NOPX .ne. MAPNOT) then
            if (NOPX .le. MAPRP) then
c                   An error -- this operator wants to be binary.
              call ERROUT('Adjacent operators in expression')
              go to 710
            end if
c                   Convert to index on the stack
            NOPX = NOPX - MAPRP
          end if
        else if (NOPX .eq. MAPEXP) then
c                       "**" needs different precedence on the stack.
          NOPX = 22
        end if
        LOR = LOR + 1
        ISTKOP(LOR) = NOPX
        go to 10
      end if
c
c                        Evaluate the operator on top of the stack.
      IT2 = IANDTY(LAND)
      IT1 = IANDTY(LAND-1)
      IV2 = IANDIV(LAND)
      IV1 = IANDIV(LAND-1)
      NOPS = ISTKOP(LOR)
      LOR = LOR - 1
c              (  U+  U-  ~=  <=  >=  /=  ==   ~  <    >   /  =   *
      go to (210,200,200,220,220,220,220,220,500,220,220,530,220,530,
     1  510,510,520,180,180,530,530,530, 640), NOPS
c          &   |  ,    )   (   +   -  **   //
 180  stop 'Bug -- Impossible operator index on the stack.'
c
c Unary + or -
 200  if (IT2 .ne. -4) go to 700
      if (NOPS .eq. 3) IANDIV(LAND) = -IANDIV(LAND)
      go to 100
c
c (
 210  if (NOPX .ne. MAPRP) then
        call ERROUT('Parentheses are not balanced')
        go to 710
      end if
      IOP = IOP + 1
      XAND = .false.
      go to 30
      
c Check relational operators
 220  K = 0
      if (IT1 .gt. 0) then
c                            Operand 1 is a string in BUF2
        L = IV1 - IT1
        if (IT2 .gt. 0) then
          if ((BUF2(IT1:IV1) .ne. BUF2(IT2:IV2)) .or.
     1      (L .ne. IV2-IT2)) then
c                            Don't match
            K = -3
          else
c                            Do match
            K = -1
          end if
          go to 430
        else if (IT2 .lt. -4) then
          if ((BUF2(IT1:IV1) .ne. STRSAV(-4-IT2:IV2)) .or.
     1      (L .ne. IV2+IT2+4)) then
            K = 1
            if (BUF2(IT1:IV1) .lt. STRSAV(-4-IT2:IV2)) K = -1
          end if
        else
          if (IT2 .ne. 0) then
            K = -2
            go to 430
          end if
          go to 700
        end if
      else if (IT1 .eq. -4) then
        if (IT2 .eq. -4) go to 320
        go to 700
      else if (IT1 .lt. -4) then
c                            Operand 1 is a string in STRSAV
        J = -4 - IT1
        L = IV1 - J
        if (IT2 .gt. 0) then
          if ((STRSAV(J:IV1) .ne. BUF2(IT2:IV2)) .or.
     1      (L .ne. IV2-IT2)) then
            K = 1
            if (STRSAV(J:IV1) .lt. BUF2(IT2:IV2)) K = -1
          end if
        else if (IT2 .lt. -4) then
          if ((STRSAV(J:IV1) .ne. CNTRL(IT2:IV2)) .or.
     1      (L .ne. IV2-IT2)) then
            K = 1
            if (STRSAV(J:IV1) .lt. CNTRL(IT2:IV2)) K = -1
          end if
        else
          go to 700
        end if
      else
        if (IT2 .gt. 0) IT2 = -2
        if ((IT1 .eq. -2) .or. (IT2 .eq. -2)) then
c                    Result is .U.
          K = -2
          go to 430
        end if
        if ((IT2 .lt. 0) .and. (IT2 .ge. -3)) then
c                                         Comparing logicals
          IV1 = IT1
          IV2 = IT2
c                   ~=  <=  >=  /=  ==   ~  <    >   /   =  / can't hap.
          go to (320,230,230,320,320,230,230,230,700,320), NOPS - 3
        end if
  230   continue
        go to 700
      end if
      IV1 = K
      IV2 = 0
 320  K = -3
c             ~=  <=  >=  /=  ==   ~  <    >   /    =     ~/  can't hap.
      go to (350,360,370,350,380,700,390,400,700, 380), NOPS - 3
 350  if (IV1 .ne. IV2) K = -1
      go to 430
 360  if (IV1 .le. IV2) K = -1
      go to 430
 370  if (IV1 .ge. IV2) K = -1
      go to 430
 380  if (IV1 .eq. IV2) K = -1
      go to 430
 390  if (IV1 .lt. IV2) K = -1
      go to 430
 400  if (IV1 .gt. IV2) K = -1
      go to 430
c
 430  LAND = LAND - 1
 440  IANDTY(LAND) = K
      go to 100
c
c Unary not
 500  K = IANDTY(LAND)
      if ((K .ge. 0) .or. (K .lt. -3)) then
        if (K .le. 0) go to 700
        K = -2
      end if
      K = -4 - K
      go to 440
c
c Logical operators
 510  if((IT1.eq.0).or.(IT2.eq.0).or.(IT1.lt.-3).or.(IT2.lt.-3))
     1  go to 700
      if (IT1 .gt. 0) IT1 = -2
      if (IT2 .gt. 0) IT2 = -2
      K = IT1 * IT2
      if (NOPS .eq. MAPAND) then
        K = LAMAP(K)
      else
        K = LOMAP(K)
      end if
      go to 430
c
c ,
 520  if ((NOPS .eq. MAPCOM) .and. (LAND .eq. 1)) then
c                       All done, return result
        ITYPE = IANDTY(1)
        IVAL = IANDIV(1)
        return
      end if
      call ERROUT('Something is confusing in an expression')
      go to 710
c
c Take care of arithmetic operators
 530  if ((IT1 .eq. -4) .and. (IT2 .eq. -4)) then
c                 /   =   *   &   |   ,   )   (   +   -  **
        go to (570,540,560,540,540,540,540,540,580,590,600), NOPS-MAPGT
  540   continue
        stop 'BUG -- Code should not get to label 540 in GETEXP'
 560    K = IV1 * IV2
        go to 610
 570    if (IV2 .ne. 0) then
          K = IV1 / IV2
          go to 610
        end if
        call ERROUT('Trying to divide by 0 in expression')
        go to 710
 580    K = IV1 + IV2
        go to 610
 590    K = IV1 - IV2
        go to 610
 600    K = IV1 ** IV2
 610    LAND = LAND - 1
        IANDIV(LAND) = K
        go to 100
      end if
      call ERROUT('Problem with types in arithmetic expression')
      go to 710
c                     Take care of concatenation.
 640  L = 1
 650  if (IT1 .eq. 1)then
        L = IV1 + 1
        IT1 = IT2
        IV1 = IV2
      end if
      K = IV1
      if (IT1 .lt. -4) then
c                         A string in STRSAV
        IV1 = IV1 + IT1 + L + 4
        BUF2(L:IV1) = STRSAV(-4-IT1:K)
      else if (IT1 .eq. -4) then
c                        An integer
        IV1 = abs(IV1)
        J = 21
 660    J = J - 1
        BUF3(J:J) = char(mod(IV1, 10) + NUM0)
        IV1 = IV1 / 10
        if (IV1 .ne. 0) go to 660
        if (K .lt. 0) then
          J = J - 1
          BUF3(J:J) = '-'
        end if
        IV1 = L + 20 - J
        BUF2(L:IV1) = BUF3(J:20)
      else if (IT1 .le. 0) then
c                        A logical
        if (IT1 .eq. -1) then
          IV1 = L + 6
          BUF2(L:IV1) = '.TRUE.'
        else if (IT1 .eq. -2) then
          IV1 = L + 7
          BUF2(L:IV1) = '.FALSE.'
        else
          call ERROUT('Can''t evalutate term in string expression')
          go to 710
        end if
      else if (IT1 .gt. 1) then
c                         A string in BUF2
        IV1 = IV1 - IT1 + L
        BUF3(1:K-IT1+1) = BUF2(IT1:K)
        BUF2(L:IV1) = BUF3(1:K-IT1+1)
      end if
      IT1 = 1
      if (L .eq. 1) go to 650
      LAND = LAND - 1
      IANDTY(LAND) = 1
      IANDIV(LAND) = IV1
      go to 100
c
 700  call ERROUT('Types in expression are not compatible')
 710  IOP = IOP + 1
      if (LOCOP(IOP) .lt. LAST) then
        if (INDXOP(IOP) .ne. MAPCOM) go to 710
        go to 20
      end if
      ITYPE = 0
      return
      end
c   End of GETEXP

      subroutine GETMAK(LUP, LLO, LC, LTY, LTYU)
c              Get parameter from a  "MAKE..." line.
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
      character LUP, LLO, LC
      integer LTY, LTYU
      character LETTYP*9
      data LETTYP / 'SDQICZWPX' /
c
      LEND = KPT(LOOK)
      if (MAPPED(LOOK) .ge. MAPDIG) call ERROUT(
     1  'Letter required to define file')
      LUP = BUF2(LOOK:LOOK)
      LLO = char(ichar(LOUTUP) + LETDIF)
      LC = BUF1(LEND:LEND)
      LTY = index(LETTYP, LUP)
      LOOK = LOOK + 1
      if (BUF2(LOOK:LOOK+7) .eq. 'WITHTYPE') then
        LTY = index(LETTYP, BUF2(LOOK+8:LOOK+8))
        if ((LTY .eq. 0) .or. (LTY .gt. 7)) call ERROUT(
     1    'Don''t know type specified')
        LOOK = LOOK + 9
      end if
      LTYU = LTY
      if (LTYU .gt. 6) LTYU = LTYU - 6
      if (LTYU .ge. 4) LTYU = LTYU - 4
      return
      end
c   End of GETMAK

      subroutine GETNAM
c Enter names into NAME.
      integer I, J, L, K, NAME1
      save NAME1
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
      character LETTYP*9
      data LETTYP / 'SDQICZWPX' /
c
      if (LOOK .eq. 0) then
c                         Special code to convert old form
        LOOK = 5
        if ((KNT .eq. 0) .or. (BUF2(4:4) .ne. '&')) then
          K = index(BUF2(1:LAST), 'VERSIONUSES')
          if (K .eq. 0) then
c               call ERROUT('Unknown type of "C--..." line')
            return
          end if
          LOOK = K+11
          KNT = KNT + 1
          if (KNT .eq. 1) then
            NAME1 = 0
          else
            LSTAT = -3
          end if
        end if
      end if
 20   if (LOOK .gt. LAST) return
      K = index(BUF2(LOOK:LAST), ',')
      if (K .eq. 0) K = LAST - LOOK + 2
      LEND = LOOK + K - 2
      I = index(BUF2(LOOK:LEND), '?')
      if (I .eq. 0) then
        if (KNT .gt. 0) then
          if ((BUF2(LOOK:LEND) .eq. 'REAL') .or.
     1      (BUF2(LOOK:LEND) .eq. 'DBLE') .or.
     2      (BUF2(LOOK:LEND) .eq. 'R1MACH') .or.
     3      (BUF2(LOOK:LEND) .eq. 'D1MACH')) then
            LOOK = LEND + 2
            go to 20
          end if
          if (KNT .eq. 2) then
            NAME1 = NAME1 + 1
            K = 0
            L = 0
            J = LOOK - 1
            do  100 I = 1, LENNAM(NAME1)
              if (NAMES(NAME1)(I:I) .ne. BUF2(J+I:J+I)) then
                K = K +1
                L = I
              end if
  100       continue
            if (K .eq. 1) then
              NAMREF(NAME1) = 0
              NAMMRK(NAME1) = L - 1
              NAMES(NAME1)(L:L) = LINUP
              if (NAME1 .gt. LNAMU) NAMES(NAME1)(L:L) =
     1          LETTYP(LTYIN:LTYIN)
              I = KPT(J + L)
              BUF1(I:I) = '?'
            else
              call ERROUT('On C--... lines, names do not match up')
            end if
            LOOK = LEND + 2
            go to 20
          else if (NUMNAM .lt. MXNAME) then
            NUMNAM = NUMNAM + 1
            LENNAM(NUMNAM) = K - 1
            NAMES(NUMNAM) = BUF2(LOOK:LEND)
            LOOK = LEND + 2
            go to 20
          end if
        else
          call ERROUT('Name in "C--..." line has no "?"')
        end if
      else if (NUMNAM .lt. MXNAME) then
        NUMNAM = NUMNAM + 1
        LENNAM(NUMNAM) = K - 1
        NAMES(NUMNAM) = BUF2(LOOK:LEND)
        NAMREF(NUMNAM) = 0
        NAMMRK(NUMNAM) = I - 1
        NAMES(NUMNAM)(I:I) = LINUP
        if (LNAMU .lt. NUMNAM) NAMES(NUMNAM)(I:I) = LETTYP(LTYIN:LTYIN)
        LOOK = LEND + 2
        go to 20
      end if
      call ERROUT('Out of space for names')
      stop 'ABORT PROCESSING -- Increase MXNAME and recompile M77CON'
      end
c   End GETNAM

      subroutine INIT
c Get the table, MAP, for mapping characters and other initialization.
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
      integer I
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
c
      LETDIF = ichar('a') - ichar('A')
      NUM0 = ichar('0')
      do 100 I = 1, 128
        MAP(I) = MAPOTH
  100 continue
      MAP(ichar('a')) = MAPLC
      MAP(ichar('b')) = MAPLC
      MAP(ichar('c')) = MAPLC
      MAP(ichar('d')) = MAPLC
      MAP(ichar('e')) = MAPLC
      MAP(ichar('f')) = MAPLC
      MAP(ichar('g')) = MAPLC
      MAP(ichar('h')) = MAPLC
      MAP(ichar('i')) = MAPLC
      MAP(ichar('j')) = MAPLC
      MAP(ichar('k')) = MAPLC
      MAP(ichar('l')) = MAPLC
      MAP(ichar('m')) = MAPLC
      MAP(ichar('n')) = MAPLC
      MAP(ichar('o')) = MAPLC
      MAP(ichar('p')) = MAPLC
      MAP(ichar('q')) = MAPLC
      MAP(ichar('r')) = MAPLC
      MAP(ichar('s')) = MAPLC
      MAP(ichar('t')) = MAPLC
      MAP(ichar('u')) = MAPLC
      MAP(ichar('v')) = MAPLC
      MAP(ichar('w')) = MAPLC
      MAP(ichar('x')) = MAPLC
      MAP(ichar('y')) = MAPLC
      MAP(ichar('z')) = MAPLC
      MAP(ichar('A')) = MAPUC
      MAP(ichar('B')) = MAPUC
      MAP(ichar('C')) = MAPUC
      MAP(ichar('D')) = MAPUC
      MAP(ichar('E')) = MAPUC
      MAP(ichar('F')) = MAPUC
      MAP(ichar('G')) = MAPUC
      MAP(ichar('H')) = MAPUC
      MAP(ichar('I')) = MAPUC
      MAP(ichar('J')) = MAPUC
      MAP(ichar('K')) = MAPUC
      MAP(ichar('L')) = MAPUC
      MAP(ichar('M')) = MAPUC
      MAP(ichar('N')) = MAPUC
      MAP(ichar('O')) = MAPUC
      MAP(ichar('P')) = MAPUC
      MAP(ichar('Q')) = MAPUC
      MAP(ichar('R')) = MAPUC
      MAP(ichar('S')) = MAPUC
      MAP(ichar('T')) = MAPUC
      MAP(ichar('U')) = MAPUC
      MAP(ichar('V')) = MAPUC
      MAP(ichar('W')) = MAPUC
      MAP(ichar('X')) = MAPUC
      MAP(ichar('Y')) = MAPUC
      MAP(ichar('Z')) = MAPUC
      MAP(ichar('0')) = MAPDIG
      MAP(ichar('1')) = MAPDIG
      MAP(ichar('2')) = MAPDIG
      MAP(ichar('3')) = MAPDIG
      MAP(ichar('4')) = MAPDIG
      MAP(ichar('5')) = MAPDIG
      MAP(ichar('6')) = MAPDIG
      MAP(ichar('7')) = MAPDIG
      MAP(ichar('8')) = MAPDIG
      MAP(ichar('9')) = MAPDIG
      MAP(ichar('.')) = MAPDOT
      MAP(ichar('?')) = MAPQM
      MAP(ichar('~')) = MAPNOT
      MAP(ichar('<')) = MAPLT
      MAP(ichar('>')) = MAPGT
      MAP(ichar('/')) = MAPDIV
      MAP(ichar('=')) = MAPEQ
      MAP(ichar('*')) = MAPMUL
      MAP(ichar('&')) = MAPAND
      MAP(ichar('|')) = MAPOR
      MAP(ichar(',')) = MAPCOM
      MAP(ichar(')')) = MAPRP
      MAP(ichar('(')) = MAPLP
      MAP(ichar('+')) = MAPPLU
      MAP(ichar('-')) = MAPMIN
      MAP(ichar(''''))= MAPSTR
      MAP(ichar('"')) = MAPSTR
      NUMSTR = 0
      KNT = 0
      KOMINS = 0
      NAMEND = 0
      KSET = 0
      LPATH = 0
      NFILE = 0
      NERR = 0
      KPT(0) = 0
      LOUT = ' '
      CCODE = .false.
      return
      end
c   End of INIT

      block data INIDAT
c                               Define initial data
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
c
      data KEYWRD / 'REAL', 'DOUBLEPRECISION', ' ', 'INTEGER',
     1  'COMPLEX', ' ', ' ' /
      data LENKEY / 0, 4, 15, 0, 7, 7, 0, 0 /
      data KEYBLK / 0,  6, 0, 0, 0, 0, 0 /
      data INFUNC / 'REAL', 'R1MACH', 'DBLE', 'D1MACH', 2*' ', 'INT',
     1  'CMPLX', 'CONJG', 'AIMAG', 6 * ' ' /
      data LENFUN / 4, 6, 4, 6, 0, 0, 3, 5, 5, 5, 6*0 /
c                   S  D  Q  I  C   Z   W
      data LOCFUN / 1, 3, 5, 7, 8, 11, 14, 17 /
      data LETFUP / 'EDQ EDQ' /
      data LSTOLD(0), LSTNEW(0) / 1, 1 /
      data INDXOP(0) / MAPCOM /
      end
c   End of INIDAT

      subroutine NAMCHK
c       Checks and prints names that have not been referenced.
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
c
 1000 format(' The following names were not referenced:', (/ 7A10))
c
      integer I, L
c
      L = 0
      do 20 I = 1, NUMNAM
        if (NAMREF(I) .eq. 0) then
          L = L + 1
          NAMES(L) = NAMES(I)
        end if
   20 continue
      if (L .ne. 0) then
         NERR = NERR + 1
         print 1000, (NAMES(I), I = 1, L)
      end if
      NUMNAM = 0
      LNAMU = 999999
      NAMEND = -NAMBEG
      do 60 I = NAMBEG+1, MXCPPC
         KTYPE(I) = 0
   60 continue
      KSET = KSETBG
      NUMSTR = 0
      LSTAT = 0
      KNT = 0
      MOREPR = 0
      LESSPR = 0
      return
      end
c   End NAMCHK

      subroutine PACKOP
c Copies the nonblanks and nonoperators from BUF1 into BUF2 in order.
c Lower case letters are replaced by their upper case equivalents, and
c '?' is replaced by LOUTUP wherever it appears.  When an operator (the
c characters '~><=&|,') is removed, the location of the last location
c used in BUF2 is saved in LOCOP, and the index of the operator is saved
c at the corresponding location in INDXOP.
c KPT(i) is set to the location in BUF1 corresponding to the i-th
c character in BUF2, LAST is set to the index of the last character in
c BUF2, and LOP is set to the index last referenced in LOCOP.
c
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
      integer MAPEXP
      parameter (MAPEXP = 0)
      logical STRING
      character CSTR
      integer I, J, K, ILAST
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
c
      LAST = 0
      LOP = 0
      STRING = .false.
      do 100 I = 1, LENLIN
        C = BUF1(I:I)
        J = ichar(C)
        K = MAP(J)
        if (STRING) then
          if (C .eq. CSTR) then
            STRING = .false.
          end if
        else
          if (C .le. ' ') go to 100
          if ((K .lt. MAPQM) .or. (I .lt. 4)) then
            if (K .le. MAPLC)  C = char(J - LETDIF)
          else if (K .gt. MAPQM) then
            ILAST = I
            if (K .eq. MAPEQ) then
              if (INDXOP(LOP) .le. MAPEQ) then
                if (LOCOP(LOP) .eq. LAST) then
                  INDXOP(LOP) = INDXOP(LOP) - 5
                  go to 100
                end if
              end if
            else if (K .eq. MAPMUL) then
              if (INDXOP(LOP) .eq. MAPMUL) then
                if (LOCOP(LOP) .eq. LAST) then
                  INDXOP(LOP) = MAPEXP
                  go to 100
                end if
              end if
            else if (K .eq. MAPDIV) then
              if (INDXOP(LOP) .eq. MAPDIV) then
                if (LOCOP(LOP) .eq. LAST) then
                  INDXOP(LOP) = MAPCAT
                  go to 100
                end if
              end if
            else if (K .eq. MAPSTR) then
              CSTR = C
              STRING = .true.
              go to 90
            end if
            LOP = LOP + 1
            LOCOP(LOP) =  LAST
            INDXOP(LOP) = K
            go to 100
          else
            C = LOUTUP
          end if
        end if
 90     LAST = LAST + 1
        BUF2(LAST:LAST) = C
        KPT(LAST) = I
        MAPPED(LAST) = K
  100 continue
      LOP = LOP + 1
      LOCOP(LOP) = LAST
      INDXOP(LOP) = MAPCOM
      if (LOCOP(LOP-1) .eq. LAST) then
        LAST = LAST + 1
        KPT(LAST) = ILAST
      end if
      if (STRING) call ERROUT('String not terminated')
      return
      end
c   End PACKOP

      subroutine PROCMM
c Process a "C--..." line after the initial processing.
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
      integer  I, K, NUMSV, INFSV
      save NUMSV, INFSV
      character LETTYP*7, C2*2
      data LETTYP / 'SDQICZW' /
c
      K = index(LETTYP, BUF2(4:4))
      if (K .eq. 0) then
        if (BUF2(4:23) .eq. 'BEGINMASKCODECHANGES') then
          NUMSV = NUMNAM
          INFSV = INFDIF
          NUMNAM = 0
          INFDIF = -1000
        else if (BUF2(4:21) .eq. 'ENDMASKCODECHANGES') then
          NUMNAM = NUMSV
          INFDIF = INFSV
        else
          call ERROUT('Invalid letter for precision -- line ignored')
        end if
        return
      end if
      if (LOUT .le. ' ') return
      LINUP = BUF2(4:4)
      LTYCI = K
      LSTAT = 1
      if (BUF2(5:30) .eq. 'USEDFORUSUALPRECISIONBELOW') then
c This particular "if block" could probably be deleted.
        LOUTUP = LOUTU
        LTYCO = LTYUO
        LINU = LINUP
        go to 200
      else if (BUF2(5:20) .eq. 'NEXTLINESPECIAL:') then
        BUF2(20:20) = ','
        C2 = ',' // LOUTUP
        I = index(BUF2(20:LAST), C2)
        if (I .eq. 0) then
          LOUTUP = LOUTU
          LTYCO = LTYUO
        else
          CONVOK = .true.
          if (BUF2(I+21:I+22) .eq. '=>') I = I + 3
          K = index(LETTYP, BUF2(I+20:I+20))
          if (K .ne. 0) then
            LOUTUP = BUF2(I+20:I+20)
            if (LOUTUP .ne. LOUTU) LSTAT = 1
            LTYCO = K
          end if
        end if
        go to 200
      end if
 100  call ERROUT('Badly formed "C--..." line ignored')
      return
 200  if (LENKEY(LTYCO) .eq. 0) then
        call ERROUT('No change -- Output type not defined.')
        LTYCO = LTYUO
        LOUTUP = LOUTU
      else if (LENKEY(LTYCI) .eq. 0) then
        call ERROUT('FATAL ERROR -- Input type not defined.')
        stop
      else
        K = KPT(4)
        BUF1(K:K) = LOUTUP
        if (MAPPED(4) .le. MAPLC) BUF1(K:K) =
     1    char(ichar(LOUTUP) + LETDIF)
        if (LSTAT .gt. 0) then
          if (LTYCO * LTYCI .eq. 0) then
            INFDIF = 0
          else
            INFDIF = LOCFUN(LTYCO) - LOCFUN(LTYCI)
          end if
        end if
        if (LINUP .ne. LINU) then
          do 300 I = 1, min(LNAMU, NUMNAM)
            K = NAMMRK(I) + 1
            NAMES(I)(K:K) = LINUP
  300       continue
         end if
         if (LNAMU .lt. NUMNAM) then
            if (LTYCI .ne. LTYIN) then
               C = LETTYP(LTYCI:LTYCI)
               do 310 I = LNAMU+1, NUMNAM
                  K = NAMMRK(I) + 1
                  NAMES(I)(K:K) = C
  310          continue
             end if
          end if
      end if
      return
      end
c   End PROCMM

      subroutine PROCPP
c Process a "C++..." line.
      integer J, K, L, MAPVER(6)
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
c Based on current (Inactive/Active) state (I or A), and value of
c logical expression (T or U or F) MAPVER gives value for KVERS.
c                   IT IU IF AT AU AF
      data MAPVER / 3, 5, 5, 2, 2, 4 /
c
 10   call PACKOP
      IOP = 0
      K = index(BUF2(1:LAST), 'CODEFOR')
      if (K .ne. 0) then
        L = 0
        J = index(BUF2(1:LAST), 'ISINACTIVE')
        if (J .eq. 0) then
          J = index(BUF2(1:LAST), 'ISACTIVE')
          if (J .eq. 0) then
            call ERROUT('Badly formed "C++...", line ignored')
            return
          else
            L = 3
          end if
        end if
        LOOK = K + 7
        LOCOP(LOP) = J - 1
        call GETEXP
        if (ITYPE .gt. 0) ITYPE = -2
        if ((ITYPE .lt. 0) .and. (ITYPE .ge. -3)) then
c Got a logical type -- Use old state from L and ITYPE to get new state.
          KVERS = MAPVER(L - ITYPE)
          J = KPT(J+2)
          if (KVERS .eq. 3) then
            call COPY ('ACTIVE', J, 6, 8, 0)
          else if (KVERS .eq. 4) then
            call COPY ('INACTIVE', J, 8, 6, 0)
          end if
          return
        end if
        call ERROUT('Line ignored, expressions must be logical')
        return
      end if
      K = index(BUF2(1:LAST), 'OFNEXT')
      if (K .ne. 0) then
        KOUNTC = 0
        J = K + 5
 40     J = J + 1
        if (MAPPED(J) .eq. MAPDIG) then
          KOUNTC = 10 * KOUNTC + ichar(BUF2(J:J)) - NUM0
          go to 40
        end if
        if((KOUNTC.ne.0).and.(BUF2(J:J+16).eq.'LINESONLYTHEFIRST'))then
          L = index(BUF2(1:LAST), 'AREACTIVE')
          if (L .ne. 0) then
            LOOK = J + 17
            LOCOP(1) = LOOK - 1
c                  Need to cover up "," that was part of text.
            INDXOP(1) = MAPPLU
            LOCOP(LOP) = L-1
            call GETEXP
            if (ITYPE .eq. -4) then
              if ((IVAL.ge.0).and.(IVAL.le.KOUNTC)) then
                KOUNTB = IVAL
                KOUNTC = KOUNTC - KOUNTB
                KVERS = 7
                if (KOUNTB .eq. 0) KVERS = 8
                return
              end if
            end if
          end if
        end if
        call ERROUT('Line ignored -- improper line counts')
        return
      end if
      K = index(BUF2(1:LAST), 'END')
      if (K .eq. 4) then
        if ((KVERS .eq. 1) .or. (KVERS .ge. 6)) then
          call ERROUT('Unexpected "C++END" line')
        end if
        KVERS = 1
        return
      end if
      K = index(BUF2(1:LAST), 'REPLACE')
      if (K .ne. 0) then
        LOOK = K + 7
        KVERS = KVERS + 100
        call GETASG
        return
      end if
      K = index(BUF2(1:LAST), 'DEFAULT')
      if (K .eq. 0) then
        if (BUF2(4:13) .ne. 'CURRENTHAS') then
          K = index(BUF2(1:LAST), 'SUBSTITUTEFOR')
          if (K .ne. 0) then
            call PACKUP
            K = index(BUF2(1:LAST), 'SUBSTITUTEFOR')
            J = index(BUF2(1:LAST), 'BELOW')
            BUF3 = BUF2(K+13:J-1)//','
            BUF2 = BUF3
            KVERS = 6
            return
          end if
          K = index(BUF2(1:LAST), 'SAVEDATABYELEMENTSIF')
          if (K .ne. 0) then
            LEND = K - 1
            KDATA = 1
            if (K .gt. 16) then
              K = index(BUF2(1:LAST), 'WITHFIRSTINDEX')
              if (K .ne. 0) then
                LOOK = K + 14
                call getexp
                if (ITYPE .ne. -4) go to 80
                KDATA = IVAL
              end if
            end if
            LOOK = LEND + 21
            call getexp
            if (ITYPE .eq. -3) then
              LDATA = -1
            else if (ITYPE .eq. -1) then
              LDATA = 1
            else
              go to 80
            end if
            return
          end if
          if (BUF2(4:6) .eq. 'SET') then
c                           Convert old form "C++ SET instruction".
            call COPY('CURRENT HAS', KPT(4), 11, 3, 0)
            go to 10
          end if
 80       call ERROUT('Badly formed "C++...", line ignored')
          return
        end if
        LOOK = 14
        KVERS = -KVERS
      else
        if (K .gt. 4) then
          if (INDXOP(1) .eq. MAPLP) then
            J = 1
            do 90 L = 2, 80
              if (INDXOP(L) .eq. MAPLP) then
                J = J + 1
              else if (INDXOP(L) .eq. MAPRP) then
                J = J - 1
                if (J .eq. 0) then
                  INDXOP(L) = MAPCOM
                  go to 100
                end if
              end if
   90          continue
  100          LOOK = 4
               IOP = 1
               call getexp
               if (ITYPE .ne. -1) return
            else if (index(BUF2(4:K-1), LOUTUP) .eq. 0) then
               return
            end if
         end if
         LOOK = K + 7
      end if
      call getasg
      KVERS = abs(KVERS)
      return
      end
c   End PROCPP

      subroutine SUBLIN
c Substitutes for stuff from previous line.
      integer II, J, K, L, LNB
      character TEMPC*20
      integer MAPLC, MAPUFP, MAPUC, MAPDIG, MAPOTH, MAPDOT,
     1  MAPQM, MAPNOT, MAPLT, MAPGT, MAPDIV, MAPEQ, MAPMUL, MAPAND,
     2  MAPOR, MAPCOM, MAPRP, MAPLP, MAPPLU, MAPMIN, MAPSTR, MAPCAT
      parameter    ( MAPLC  = 2, MAPUFP = 3,  MAPUC = 4,
     1  MAPDIG = 5, MAPOTH = 6, MAPDOT = 7, MAPQM  = 8, MAPNOT = 9,
     2  MAPLT  =10, MAPGT  =11, MAPDIV =12, MAPEQ  =13, MAPMUL =14,
     3  MAPAND =15, MAPOR  =16, MAPCOM =17, MAPRP  =18, MAPLP  =19,
     4  MAPPLU= 20, MAPMIN =21, MAPSTR =22, MAPCAT =23 )
c Declarations required for common
      integer NPDFT, NUMTYP
      parameter (NPDFT = 16, NUMTYP = 7)
      integer LENLIN, MXCPPC, MXNAME, MXOP, MXSTR, MXVAL
      parameter (LENLIN=72, MXCPPC=255, MXNAME=200, MXOP=30,
     1  MXSTR=20, MXVAL=50)
c
      character BUF1*(LENLIN), BUF2*(LENLIN), BUF3*(LENLIN), C*1,
     1  CNTRL*(MXCPPC), HEADER*(LENLIN), INFUNC(NPDFT)*8,
     2  KEYWRD(NUMTYP)*20, LETFUP*(NUMTYP), LIN, LINLO, LINU, LINUP,
     3  LOUT, LOUTLO, LOUTU, LOUTUP, NAMES(MXNAME)*8, PATH*68,
     4  STROLD*(MXCPPC), STRNEW*(MXCPPC), STRSAV*(MXCPPC)
      integer INDXOP(0:MXOP), INFDIF, IOP, ITYPE, IVAL, KDATA,
     1  KEYBLK(NUMTYP), KOMINS, KOUNTB, KOUNTC, KSET, KTYPE(MXCPPC+2),
     2  KPT(0:LENLIN), KNT, KSETBG, KVAL(MXCPPC), KVERS, LAST, LDATA,
     3  LEND, LENFUN(NPDFT), LENKEY(0:NUMTYP), LENNAM(MXNAME), LESSPR,
     4  LETDIF, LINE, LNAMU, LOCOP(0:MXOP), LOCFUN(NUMTYP+1), LPATH,
     5  LSTAT, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     6  MAP(128), MAPPED(LENLIN), MOREPR, NAMBEG, NAMEND, NERR, NFILE,
     7  NUM0, NUMNAM, NUMSTR, LSTOLD(0:MXSTR), LSTNEW(0:MXSTR),
     8  NAMMRK(MXNAME), NAMREF(MXNAME)
      logical CCODE, CHGLIN, CONVOK
      common /M77C1/ BUF1, BUF2, BUF3, C, CNTRL, HEADER, INFUNC, KEYWRD,
     1  LETFUP, LIN, LINLO, LINU, LINUP, LOUT, LOUTLO, LOUTU, LOUTUP,
     2  NAMES, PATH, STROLD, STRNEW, STRSAV
      common /M77C2/ INDXOP, INFDIF, IOP, ITYPE, IVAL, KDATA, KEYBLK,
     1  KOMINS, KSET, KSETBG, KTYPE, KNT, KOUNTB, KOUNTC, KPT, KVAL,
     2  KVERS, LAST, LDATA, LEND, LENFUN, LENKEY, LENNAM, LESSPR,
     3  LETDIF, LINE, LNAMU, LOCOP, LOCFUN, LPATH, LSTAT, LSTOLD,
     4  LSTNEW, LTYCI, LTYCO, LTYIN, LTYOUT, LTYUI, LTYUO, LOOK, LOP,
     5  MAP, MAPPED, MOREPR, NAMBEG, NAMEND, NERR, NFILE, NUM0, NUMNAM,
     6  NUMSTR, NAMMRK, NAMREF, CCODE, CHGLIN, CONVOK
c End of declarations required for common
c
      KVERS = 1
      LOOK = 1
      BUF3 = BUF2
      call PACKUP
 20   L = index(BUF3(LOOK:), ',')
      if (L .ne. 0) then
        LEND = LOOK + L - 2
        J = index(BUF2, BUF3(LOOK:LEND))
        if (J .ne. 0) then
          J = KPT(J)
          if (MAP(ichar(BUF1(J-1:J-1))) .gt. MAPDIG) then
            J = J + L - 2
 40         J = J + 1
            if (BUF1(J:J) .le. ' ') go to 40
            if (BUF1(J:J) .eq. '=') then
 60           J = J + 1
              if (BUF1(J:J) .le. ' ') go to 60
              BUF2 = BUF3
              call GETAND
              if (ITYPE .eq. -4) then
c                           Copy over an integer
                K = 20
                II = IVAL
                if (II .lt. 0) II = -II
 80             TEMPC(K:K) = char(mod(II, 10) + NUM0)
                K = K - 1
                II = II / 10
                if (II .ne. 0) go to 80
                if (IVAL .lt. 0) then
                  TEMPC(K:K) = '-'
                  K = K - 1
                end if
                LNB = 0
                do 100 II = J, LENLIN
                  if (BUF1(II:II) .gt. ' ') then
                    if (MAP(ichar(BUF1(II:II))) .ne. MAPDIG)
     1                go to 110
                    LNB = II
                  end if
  100           continue
 110            if (LNB .eq. 0) go to 200
                call COPY(TEMPC(K+1:), J, 20 - K, LNB-J+1, 0)
                go to 180
              end if
              if (ITYPE .eq. -1) then
                L = 6
                TEMPC(1:6) = '.true.'
              else
                if (ITYPE .ne. -3) go to 200
                L = 7
                TEMPC(1:7) = '.false.'
              end if
c                                        Looking for logical
              if (BUF1(J:J) .eq. '.') then
                do 140 II = J+1, LENLIN
                  if (BUF1(II:II) .gt. ' ') then
                    if (MAP(ichar(BUF1(II:II))) .ge. MAPUC) then
                      if (BUF1(II:II) .eq. '.') go to 160
                      go to 200
                    end if
                  end if
  140           continue
 160            call COPY(TEMPC, J, L, II - J + 1, 0)
              end if
            end if
          end if
        end if
 180    LOOK = LEND + 2
        go to 20
 200    call ERROUT('Substitution not made -- values not compatible')
      end if
      return
      end
