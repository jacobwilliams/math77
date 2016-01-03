      program GLOCH1
c     .  Copyright (C) 1996, California Institute of Technology.
c     .  All rights reserved.  U. S. Government sponsorship under
c     .  NASA contract NAS7-918 is acknowledged.
c>> 2005-12-24 GLOCH1  Krogh  Added NEWTOK to save stmt. in PACK.
c>> 1998-10-31 GLOCH1  Krogh  Many fixes discoverd doing "mangle"
c>> 1997-06-25 GLOCH1  Krogh  Fixed probs with "implicit" & "intrinsic".
c>> 1997-06-25 GLOCH1  Krogh  Fix if "DO 10, I =" (didn't work with ",")
c>> 1997-04-30 GLOCH1  Krogh  Fix if (...) end line, and "character*"
c>> 1997-04-10 GLOCH1  Krogh  Fix blank common & char. substr. as arg.
c>> 1997-04-02 GLOCH1  Krogh  Fix bugs assoc. with character functions.
c>> 1997-04-01 GLOCH1  Krogh  Allow common name=var. name
c>> 1996-06-11 GLOCH1  Krogh  Initial Code
c Does the initial processing for the list of files in glnames.
c The execution of this program is meant to be followed by execution of
c the program gloch2.  These two programs were originally one routine,
c but were broken up as the code was getting too long for convenient
c processing in a DOS environment on a PC.
c Files used and their associated unit numbers are:
c glnames 7  Input list of file names.  Full path names can be used
c            here.  The file name is remembered as that part of the line
c            from the last "/" or "\", or beginning of line to the text
c            just before the following "." or the end of line (up to a
c            maximum of 8 characters).
c gldiagl 8  Output file for local diagnostics.
c gldiagg 8  Output file for global diagnostics. (in GLOCH2)
c glsave  9  Data saved required for later processing.
c gltemp 10  Temporary data for final checking of global interfaces.
c glents 11  If the "E" option is used for diagnostics, this is where
c            the data listing all of the entries is written.
c The files glsave and gltemp are required to complete the checking of
c the global interfaces.  Only the file glsave is required for getting
c the various cross reference listings.
c
c If the first character of a line in glnames is a blank, it is assumed
c that the rest of the line contains letters defining options.  A upper
c case letter gives the option, a lower case one turns the option off.
c Global diagnostics are always given, the defaults for the others are
c Lfeip777.
c
c     L Local diagnostics        |  F diagnose Fortran 90 features.
c     E Output Entry statements  |  I Intrinsic function diagnostics.
c     P unreferenced Parameters. |
c In addition, G can be used to read glsave which will initialize
c internal tables to the values they had after the run which generated
c the data in glsave.
c
c One can change options at any time, although in most cases one should
c only want to set them on the first line.
c
c After running GLOCH1, one should run GLOCH2.  This will first complete
c processing of global data information, and then prompt for additional
c inputs.  These inputs are of the form:

c      [output] [file_name] action First_item Second_item (if any)
c
c The result of the action is written to file_name which must contain
c more than one letter.  If file_name is missing, output is to the
c last file used for output.  (gldiagg if no file is ever given.)
c The actions are denoted with a single letter as follows (case of
c letters is not significant):
c   Q  Ends the processing.
c   R  Restore data from a previous save.  The letter for this action
c      must be preceded by the file with the data.
c   S  Save data for later use by a restore action. The letter for this
c      action must be preceded by the name of the file to which the
c      data is written.
c   E  Entry sort.  When a collection of entries are printed they are
c      printed in the order given by the sort.  The "ILP" options under
c      action D and the P action are not effected by the sort
c      specifications.  If not enough is specified in a sort to
c      determine an order, the name of the entry is used for the last
c      key.  The sort specification applies for following actions, until
c      getting a new specification.  First_item is a string defining the
c      keys, with the key sorted on first given first.  The following
c      are used to specify keys.
c      F  use the order of the File containing the entry.  (If this is
c         used it should be the first key.)
c      L  sort those with smallest maximum distance to a Leaf first.
c         (A leaf is an entry that calls no other entry.)
c      R  sort those with smallest maximum distance to a Root first.
c         (Not allowed if L is used.  A root is an entry that is called
c         by no other entry.)
c      N  sort on the Name of the entry.
c      -  Reverse the sort order for the key following.
c   D  Generate global diagnostics.
c      A  "D A" will print out all of the global interface information;
c      the simple "D" just provides information on questionable
c      interfaces.
c   F  File sort.  As for the entry sort, except there is no F key, the
c      N key is used to indicate the name of the file, a leaf is defined
c      as a file with no calls to entries that are not in the file, and
c      a root as a file that contains no entries called from other
c      files.
c   L  List what is specified in First_item, which consists of one or
c      more of the following letters.  (If there is more than one of
c      these letters, the listing is done for the first, followed by
c      that for the second, etc.
c      C  Common block names
c      E  Entries
c      F  Files
c      U  Undefined entries, i.e. those for which there is no code.
c   C  Cross reference, list requirements of First_items in terms of
c      second items.  First_item and Second_item are either E or F.
c      E is for entries.
c      F is for files.
c      Thus  "C E E", lists all the entries and for each entry lists
c      the entries it calls directly.  The Second_item can be followed
c      by a one of "+-*" and/or a #.
c      + recur with a list
c      - recur with a list, but with each item on a separate line.
c      * recur for each item on the list each on a separate line.
c      # Only for "C E E" or "C F F", include numbers with names listed
c        in form #.name and number the items from First_item.
c      Thus "C E F+" lists all file required by an entry, "C E E*" and
c      gives the complete call tree.
c   W  Who calls; as for C, but for each First_item, list the second
c      items that use it.
c The optional field output consists of integers followed by L or I.
c Both may be specified, but no blanks may intervene.
c   L  The line width when listing items on a line.  (initially = 79)
c   I  The amount to indent lines after the first.  (initially = 3)
c One can also type "?" to get help.
c
c ************************ Routines  ***********************************
c
c  Main program
c  subroutine GLOENT
c     Processing of a global entry declaration.
c  subroutine HASH (HNAME)
c  subroutine INIT
c     Initializes many variables.
c  subroutine LOCREF
c     Locates similar references (for entries and common)
c  subroutine PACK
c     Analyzes the input lines.
c  subroutine TXTOUT(ERRNAM, ERRTXT)
c     Prints both error messages, and requested output.
c  subroutine LOCSTR (CTYP, LS)
c
c ******************** Variable Definitions ****************************
c
c BUF1   Contains the initial input line.
c BUF2   Contains line of upper case  names on the line.
c    the other.  If max(I1, I2) .lt. 24, and I1/4 = I2/4 then the types
c    are compatible and the resulting type is the letter associated with
c    the larger of the two.
c    See NAMTYP for a description of letters associated with types.
c CSTK   Used to save information about types in the stack.
c CTYPA  Used to accumulate type information.
c CTYPT  Also used in accumulating type information.
c CURFIL File name being worked on.
c DEFIO  Array used in PACK to define "name="'s in I/O statement that
c   assign a value to the token to the right of the "=".
c ERRNAM Name for an error message, blank if none.  The following names
c   have a special meaning.
c  ' 1'   Too many files in the input.
c  ' 2'   Output of type error info. for entries.
c  ' 3'   Output of type error info for types.
c ERRTXT Text for an error message.
c FILNAM Array of file names (8 bytes), described above.  Also see LENT.
c GLTYPE Character string used to convert an index in NTYPA, to a type
c   letter.
c GLTYPC As for GLTYPE, but used for checking global interfaces.
c IMPL1  Used in Pack to remember the first in a range of letters used
c    in an implicit statement.
c IMPTYP Array of length 26 giving the implicit type of variables based
c    on their first letter.
c C      Temporary storage of a single character.
c CLAST  In PACK the last character processed.
c I      Temporary index.
c I1,I2  Used as temporary indices.
c IH     Location of a symbol table entry (obtained from HASH).
c INTEST Used to decide what to do with an intrinsic.  If INTEST(KTSA):
c   = -1  Then if followed by a "(" it is not an intrinsic.
c   =  0  Never an intrinsic.
c   =  1  Then if not followed by a "(" it is not an intrinsic.
c INTRab are parameters defining types for intrinsic functions.
c      a = specifies the type of function using the first letter of
c        Generic, Logical, Character, Integer, Real, Double precision,
c        and complex (which Z instead of a C).
c      b either missing, or an R to indicate an intrinsic that could be
c        replaced by its generic counterpart.
c IOENT  Index of output unit for output of lines defining entries in
c    the input.  0 if not opened, 11 if active, -11 if turned off.
c ISTA   Used in PACK to save current KSTA, before keyword look up.
c ITOK   Index in KTOK where read last token.
c J      Temporary index.
c J1     Temporary index.
c K      Temporary index.
c K0,K1,K2,K3  Used as temporary indices.
c KA     The current value of KTOK(I).
c KAxxx  Parameter names used for actions as follows.
c   KAETYP = 1 Flag error if declared, then set the type.
c   KAITYP = 2 Set the implicit type if type is not set.
c   KAFARG = 3 Flag that name is a formal argument.
c   KAREF  = 4 Flag that name is referenced.
c   KABOTH = 5 Flag both referenced and defined.
c   KADEF  = 6 Flag that name is defined.
c   KAWRIT = 7 KADEF if character, else KAREF.
c   KACOM  = 8 Flag that name is in common.
c   KAAARG = 9 Flag that name is used as actual argument.
c   KAPARM =10 Flag that in a parameter statement and defined.
c   KAPUSH =11 Push KSTA and KOUNT, set new KSTA based on prev. type.,
c              and set KOUNT = 0.
c   KAPOP  =12 Depending on KSTA may save KOUNT and type.  Pop KSTA,
c              and KOUNT
c   KAUPTY =13 Update the type information.
c   KAKSTA =14 Get new KSTA from next token in list.
c   KAENDT =15 Ends previous token
c   KAEXPR =16 Set NTYP from next token
c   KATYPE =17 Get update to NTYPA using next token in list.
c   KASTMT =18 Start a new statement.
c   KALINE =19 End of a line of input.
c   KAEND  =20 End of a subprogram, save pertinent things.
c   KAEOF  =21 End of a file.
c KB     Used in PACK for index in BUF1, at point of current processing.
c KBxxxx Parameters used to define bits and fields in symbol table
c   entries.  See NAMTYP below.
c KCSTK  Count of type characters saved in CSTK.
c KDEL   Points to first entry in symbol table to delete.
c KEYIDX In PACK, the index of the keyword for the statement.
c KGLENT Pointer to the symbol table entry for a global name.
c KGLFRO Pointer to a place in LREF where a list of references from this
c    entry are stored.  If < 0, it points to the primary entry for the
c    program unit.
c KGLNXT Pointer to next entry in the list for a file.  Last entry in
c    list is negative, and its absolute value points to the file.
c KGLTO  Pointer to a place in LREF where a list of references to this
c    entry are stored.  If this is < 0, this is a common block, and the
c    absolute value points to the reference list for the common.
c KGREF  Index of current global entry.
c KIN    Formal argument to LOCREF.  Points to LREF info. for a global
c    entry.  <0 implies a common block.
c KOUNT  Count of number of arguments for an external or an array ref.
c    One extra for type of the reference on an external reference.
c KPAREN Used in PACK to track parentheses.  0 initially, +1 for every
c   "(", -1 for every ")".  Also see NPAREN.
c KPT    Set to |KIN| in LOCREF, update to point to current LREF info.
c KSAVGL Array used to hold symbol table entries for names referenced in
c   the current subprogram.
c KSAVEN Array used to hold pointers to the formal arguments and common
c   variables for the current program unit.
c KSTA   Contains a value indicating what state we are in while
c   processing.  Parameters used here and their meanings are:
c    Name  Val.   Whats going on          Actions for this state
c   KSSTRT=-1 Starting next statement, or checking on "DO".
c   KSNONE= 0 Nothing to do until next statement.
c   KSTYPE= 1 Processing type declaration          KAETYP
c   KSDIM=  2 Processing "dimension"               KAITYP
c   KSCOMM= 3 Processing "common"                  KACOM
c   KSDIMD= 4 Inside Dimension declarator          KAITYP, KAREF
c   KSPARM= 5 Processing parameter                 KAPARM
c   KSDATA= 6 Processing data list part            KADEF
c   KSSTUF= 7 Processing "stuff"                   KAREF
c   KSBOTH= 8 Setting both referenced & defined    KABOTH
c   KSLEFT= 9 Left of equal sign                   KADEF
c   KSWRIT=10 KADEF if character, else KAREF       KAWRIT
c   KSAREF=11 Array reference                      KAITYP, KAREF
c   KSHEAD=12 Entry header (formal args)           KAFARG
c   KSXREF=13 External reference.                  KAITYP, KAAARG
c   KSDCOM=14 Common declaration                   KAETYP
c   KSENTR=15 Entry statement                      KAETYP
c   KSSUBR=16 Subroutine statement                 KAETYP
c   KSFUNC=17 Function header                      KAETYP
c   In addition, KSTA is set to 0 briefly when an arithmetic statement
c   function is seen, and is increased by 1000 while making up an global
c   entry for a program unit with no header.
c KSTK   Index telling how much has been pushed on the stack.
c KSTK1  Stack for KSTA.
c KSTK2  Stack for KOUNT.
c KSTK3  Stack for IH.  (Set to -IH for substring reference.)
c KSxxx  See KSTA above.
c KTOK   Array giving tokens from PACK.  Values are as follows.
c    >0  KTOK(.) give the last index in BUF2 containing characters for
c       for a symbol to be looked up.  (LTOK always contains the index
c       for the first character.)
c    <0 The negative of one of the KAxxx names defined above.  Some
c       of these tokens use the next value in KTOK to set another value.
c KTxxxx Parameters used to define types.  See NAMTYP below.
c KXINFO Count of number of times data has been written to the scratch
c   file which is used determine if formal arguments or variables in
c   common get used.
c LACT   Location of last action taken from NXTACT.
c LASFIL Used in TXTOUT to remember if we have written already written
c   a message for the current file.
c LASGLO Index of last global entry definition.
c LBUF2  Used in PACK for the index of last character in BUF2.
c LENLN2 Parameter two bigger than the length of an imput line.
c LENT   Array giving the start of a list of entry names associated
c   with a file.
c LERREQ Equivalenced to LERRI, LERRL, LERRP, LERRF
c LERRF  If .true. Fortran 90 comments and implicit are diagnosed.
c LERRI  If .true. specific intrinsics that could be generic are flagged
c LERRL  If .true. local problems are to be diagnosed.
c LERRP  If .true. (and LERRL) unreferenced parameters are diagnosed.
c LETDIF Gives the amount to subtract from the index for a lower case
c   character to get the index for an upper case character.
c LETIDX ichar('A') -1, used to get the index of a letter..
c LFILE  Index of last file name in FILNAM (and LENT).
c LFREEN NAMTYP(LFREEN) is a free (unused) symbol table entry.
c   NAMTYP(LFREEN) points to the next free one in the chain.
c LGLOB  Last index used for KGLENT, KGLTO, and KGLFRO.
c LGLOBX As for LGLOB, but one less than last used for a statement
c   function, which are stored starting with high indices first.
c LIMPTY Used in PACK for the type associated with "IMPLICIT" letters.
c LKTOK  Used in PACK to track last location used in KTOK.
c LKTOK1 Used in PACK to save the starting value of LKTOK.
c LNAME  Last index actually used in NAME for storing names.
c LOCACT Array which maps value of KSTA into starting place in NXTACT.
c LONG   Parameter giving the length for a long interface string.
c   Strings this long or longer will have one copy stored for each use,
c   those shorter will share space, and thus must have a new copy when
c   changed.
c LREF   Array used to track local references.  An item pointed to is
c   one of four types (k indicates value of pointer pointing to entry):
c     Start of "to" list, pointed to by KGLTO:
c         LREF(k)   Pointer to next LREF entry.
c         LREF(k-1) Pointer to type info. for entries.  (See NAMTYP.)
c         LREF(k-2) Pointer to alternate type information (0 if none).
c         LREF(k-3) Pointer to type info. for entry declaration.
c     Common block information entry.
c         Same as above, except,
c         LREF(K-3) Index of last entry making reference to this common.
c         LREF(K-4) Index of next entry of this type.  (=0, if this is
c                   the last case for this common.)
c     Type information entry:
c         Same as "to" list, but without the LREF(k-3).  Pointed to by
c         an LREF(k-2).
c     Item entry, pointed to by KGLFRO or an LREF(k) entry.
c         LREF(k)   Pointer to next LREF entry (0 if none)
c         LREF(k-1) Pointer to global entry for this item.
c LREFL  Last index used in LREF (0 initially).
c LREFLX As for LREFL, but one less than last used for a statement
c   function, which are stored starting with high indices first.
c LSAVEN Last location used in KSAVEN.
c LSAVGL Last location used in KSAVGL.
c LSTATE Tracks the state of the processing in PACK.
c  LSSTMT = 1  At the start of the statement, or accumulating a keyword.
c  LSVARN = 2  Working on a variable name.
c  LSDOT  = 3  After a "." for an operand.
c  LSEXP  = 4  After an exponent letter.
c  LSEXP2 = 5  After digit or +/i following an exponent letter.
c  LSINT  = 6  Working on an integer.
c  LSLOG  = 7  Working on logical
c  LSNEW  = 8  Looking for token to start.
c  LSIMPL = 9  Processing IMPLICIT
c  LSCLIT =10  In a string literal
c  LSNEXT =11  Get to start of next statement.
c LT     In PACK for the last location in BUF2 used for a defined token.
c LTOK   Index in BUF2 where the next name starts.
c LTYPST Last index used in the character array TYPSTR.
c MAPxxx Parameters for character mappings.
c  MAPLC=1   Lower case letter
c  MAPUC=2   Upper case letter
c  MAPDIG=3  Digit
c  MAPDOT=4  "."
c  MAPUPM=5  O.k. in exponent(Flag unary if start of token)
c  MAPOTH=6  (End token, else skip)
c  MAPSTR=7  (Start apostrophe search)
c  MAPEXC=8  ("!" means skip the rest of the line.)
c  MAPDIV=9  (New state for after common name
c  MAPEQ=10  (New state for after equal)
c  MAPRP=11  (Pop stack)
c  MAPLP=12  (Push stack) (Don't flag if does not follow a name.)
c  MAPCOM=13 (save types)
c MENTRY Global index for the last "primary" entry name declared.
c MSAVGL Parameter giving the dimension of KSAVGL.
c MSAVEN Parameter giving the dimension of KSAVEN.
c MXFILE Parameter giving the maximum number of files that can be
c   processed.
c MXLREF Parameter giving the dimension of LREF.
c MXNAME Parameter giving the dimension of NAME, and NAMTYP.  Also used
c   for the size of the hash table, so it should be a prime.
c MXSTK  Maximum number of items on stack for dealing with parentheses
c   and the call tree.
c MXTLEN Parameter giving the most arguments we can handle in a given
c   argument list or common block.
c MXGLOB Parameter giving the maximum number of global names.
c NAME   A character*8 array of names, the symbol table of names.
c NAMTYP NAMTYP(k) gives information about NAME(k).  Bits from rightmost
c   to left are used as follows.
c   Right most bit
c                0  4 bits for type (Expression type have 3 more bits.)
c             KTYUND   0 not defined
c             KTYLOG   1 logical
c             KTYCHR   2 character
c             KTYINT   3 integer
c             KTYSP    4 real
c             KTYDP    5 double precision
c             KTYCOM   6 complex
c             KTYSUB   7 subroutine
c             KTYEXT   8 other external
c                      9 -14 function with type ordered as above.
c   name =value       15 generic intrinsic
c   KBDEC=16     4  declared 1 bit
c   KBREF=32     5  referenced 1 bit
c   KBDEF=64     6  defined
c   KBARG=128    7  argument in a call
c   KBDUM=256    8  formal argument
c   KBCOM=512    9  in common
c   KBPAR=1024  10  parameter
c   KBDIM=2048  11  4 bits for array dim., 11 is used for a common block
c                   name, 12 for main programs and block data, 13 for
c                   other external names, 14 for intrinsic, and 15 for
c                   obsolete intrinsic..
c   KBEXT=32768 15  Set if high order bits point to saved external info.
c   KBGLO=65536 16  16 bits to point to global info, or saved global info.
c   KBDIMW = KBEXT / KBDIM = largest integer in dimension field.
c
c   Additional types can occur in NTYPA, see below.
c   Note that we are not tracking the length of characters, or the
c   number of dimensions in the arrays.  Abbreviations in the table
c   below: S=scalar, A=array, E expression, R referenced, D value
c   defined, N = neither R nor D, Ae = array element, x = not used, Dr =
c   Defined, might be referenced, and, U=unknown (not defined, might
c   be referenced, passed as actual argument in such a way the state is
c   unknown).
c
c  Actual arg. /     Dummy arg. /  Common Variable
c  ____ S        /       SN       /       SN
c  | ____ E        /       SR       /       SR
c  | | ____ x        /       SDr      /       SD
c  | | | ____ x        /       SU       /       SDR
c  | | | | ___ Ae        /       AN       /       AN
c  | | | | | ___ A         /       AR       /       AR
c  | | | | | | ___ x         /       ADr      /       AD
c  | | | | | | | ___ x         /       AU       /       ADR
c  | | | | | | | | ___ Function  /    Function    /       x
c  | | | | | | | | |
c  m l k j M L K J 1  logical
c  v u t s V U T S 2  character
c  i h g f I H G F 3  Integer
c  r q p o R Q P O 4  Real (single precision)
c  d c b a D C B A 5  Double precision
c  z y x w Z Y X W 6  Complex
c  e               E  e is an external, E is a subroutine
c NAxxxx Same as for the names defined with KAxxxx, except these have
c   a value = -KAxxxx.  Possible names are: NAETYP, NAITYP, NAFARG,
c   NAREF, NADEF, NACOM, NAAARG, NAPARM, NAPUSH, NAPOP, NAUPTY, NAKSTA,
c   NAENDT, NAEXPR, NATYPE, NALINE, NAEND, NAEOF.
c NCOMM  Used in PACK to keep track of where we are in a common block.
c   (/ toggles the state and output a token to change the state.)  When
c   0, not in a common block, else has a value of KSDCOM or KSCOMM
c   depending on the state to output on the next "/".
c NEWGLO Index of first global entry that is not obtained from previous
c   processing.
c NEWOP  Used in PACK together with NEWTOK to track types.  Set by
c   examining an operand.
c NEWTOK Used in PACK to track the type of expressions.  If a token is
c   output that defines part of an expression, this gives the type.
c NEWTYP Type being assigned to variables as part of statement.
c NPAREN NPAREN(KPAREN) is used in PACK to give the action on a ")".
c    NPNONE=0  No special action.
c    NPPOP= 1  Output a "pop" token.
c    NPSUBS=2  ")" is associated with a substring specification (kk:kk).
c    NPIF=  3  ")" ends an "if" statement.
c    NPPAR= 4  ")" ends a "parameter" statement.
c    NPKEYP=5  ")" used for many keyword that are followed by a "("
c    NPREAD=6  ")" (or ",") changes state to KSLEFT.
c    NPWRIT=7  Special actions for write statement.
c    100 is added to an NPAREN value if we have set the state to flag
c    the next token has defined, and this state should be reset on the
c    next "," or ")".
c NPxxxx Parameters used for NPAREN actions, see above.
c NSCOMM Used in PACK to track action to take in a common statement
c   when a "/" is encountered.  Want to change state to processing
c   data part or processing variable part.
c NTYP   Used to hold the current type information.  The initial value
c   is sometimes obtained from NAMTYP, and is updated, perhaps combined
c   into NTYPA, and ultimately may get stored back into NAMTYP.
c NTYPA  New accumulated type.  The low order 3 bits are described under
c   NAMTYP above.  The next 3 bits are used as follows.
c  NTSCAL =  0   Scalar
c  NTFUNC =  8   Function
c  NTAELT = 16   Array element
c  NTEXPR = 24   Expression
c  NTSUBS = 32   Substring (x:y)
c  NTARR  = 40   Array
c NXTACT Array giving the actions to take for a symbol table entry
c   associated with certain states.  A positive value means there is a
c   following action, a negative value indicated the end of the actions.
c   See KSTA for list of states and actions.
c TMPNAM Character string to be hashed.
c TYPSTR Array of character strings containing type information about
c   arguments, and common blocks.
c
c ********************** Specifications ********************************
c
c Parameters for portability between unix and standard conforming comp.
      character BS*1, BS4*4
      parameter (BS4 = '\\\\')
      parameter (BS = BS4)
      external PACK
c Common and parameters required in both GLOCH1 and GLOCH2
      integer MXGLOB, MXLREF, MXNAME, MXTYST, MXTLEN
c    If MXNAME is changed, keep it prime.  (Note it must also be defined
c    in GLOCH2.)
      parameter (MXGLOB=2020, MXLREF=25000, MXNAME=2333, MXTYST=200,
     1   MXTLEN=256)
c    Misc. Parameters
      integer LONG, MXFILE
      parameter (LONG=7, MXFILE=1500)
c  Parameters used for character mappings
      integer MAPLC, MAPUC, MAPDIG, MAPDOT, MAPUPM, MAPOTH, MAPSTR,
     1  MAPEXC, MAPDIV, MAPEQ, MAPRP, MAPLP, MAPCOM, MAPCOL
      parameter (MAPLC=1, MAPUC=2, MAPDIG=3, MAPDOT=4, MAPUPM=5,
     1 MAPOTH=6, MAPSTR=7, MAPEXC=8, MAPDIV=9, MAPEQ=10,
     2 MAPLP = 11, MAPRP=12, MAPCOM=13, MAPCOL=14)
c Parameters associated with bits in symbol table entries.
      integer KBDEC, KBREF, KBDEF, KBARG, KBDUM, KBCOM, KBPAR, KBDIM,
     1   KBEXT, KBGLO, KBDIMW
      parameter (KBDEC=16, KBREF=32, KBDEF=64, KBARG=128, KBDUM=256,
     1  KBCOM=512, KBPAR=1024, KBDIM=2048, KBEXT=32768, KBGLO=65536)
      parameter (KBDIMW = KBEXT / KBDIM)
c
      character CTYPA*(MXTLEN), FILNAM(MXFILE)*8, GLTYPC*56,
     1   NAME(MXNAME)*8, TYPSTR(MXTYST)*(MXTLEN)
      integer KGLENT(MXGLOB), KGLFRO(MXGLOB), KGLNXT(MXGLOB),
     1   KGLTO(MXGLOB), LENT(MXFILE), LFILE,
     2   LGLOB, LNAME, LREF(MXLREF), LREFL, LTYPST, MAP(128)
      common /GLCHRB/ CTYPA, FILNAM, GLTYPC, NAME, TYPSTR
      common /GLINTB/ KGLENT, KGLFRO, KGLNXT, KGLTO,
     1   LENT, LFILE, LGLOB, LNAME, LREF, LREFL,
     2   LTYPST, MAP

c Common and parameters required only in GLOCH1
c  Parameters used for actions.
      integer KAETYP, KAITYP, KAFARG, KAREF, KABOTH, KADEF, KAWRIT,
     1   KACOM, KAAARG, KAPARM, KAPUSH, KAPOP, KAUPTY, KAKSTA, KAENDT,
     2   KAEXPR, KATYPE, KASTMT, KALINE, KAEND, KAEOF
      parameter (KAETYP=1, KAITYP=2, KAFARG=3, KAREF=4, KABOTH=5,
     1   KADEF=6, KAWRIT=7, KACOM=8, KAAARG=9, KAPARM=10, KAPUSH=11,
     2   KAPOP =12, KAUPTY=13, KAKSTA=14, KAENDT=15, KAEXPR=16,
     3   KATYPE=17, KASTMT=18, KALINE=19, KAEND=20, KAEOF=21)
c    Negative of the KAxxxx parameters.
      integer NAETYP, NAITYP, NAFARG, NAREF, NABOTH, NADEF, NAWRIT,
     1   NACOM, NAAARG, NAPARM, NAPUSH, NAPOP, NAUPTY, NAKSTA, NAENDT,
     2   NAEXPR, NATYPE, NASTMT, NALINE, NAEND, NAEOF
      parameter (NAETYP=-KAETYP, NAITYP=-KAITYP, NAFARG=-KAFARG,
     1   NAREF=-KAREF, NABOTH=-KABOTH, NADEF=-KADEF, NAWRIT=-KAWRIT,
     2   NACOM=-KACOM, NAAARG=-KAAARG, NAPARM=-KAPARM, NAPUSH=-KAPUSH,
     3   NAPOP =-KAPOP, NAUPTY=-KAUPTY, NAKSTA=-KAKSTA, NAEXPR=-KAEXPR,
     4   NAENDT=-KAENDT, NATYPE=-KATYPE, NASTMT=-KASTMT, NALINE=-KALINE,
     5   NAEND=-KAEND, NAEOF=-KAEOF)
c  Parameters used for types.
      integer KTYUND, KTYLOG, KTYCHR, KTYINT, KTYSP, KTYDP, KTYCOM,
     1   KTYSUB, KTYEXT
      parameter (KTYUND=0, KTYLOG=1, KTYCHR=2, KTYINT=3, KTYSP=4,
     1   KTYDP=5, KTYCOM=6, KTYSUB=7, KTYEXT=8)
c  Parameters used to track the state of processing.
      integer KSSTRT, KSNONE, KSTYPE, KSDIM, KSCOMM, KSDIMD, KSPARM,
     1   KSDATA, KSSTUF, KSBOTH, KSLEFT, KSWRIT, KSAREF, KSHEAD, KSXREF,
     2   KSDCOM, KSENTR, KSSUBR, KSFUNC
      parameter (KSSTRT=-1, KSNONE=0, KSTYPE=1, KSDIM=2, KSCOMM=3,
     1   KSDIMD=4,  KSPARM=5, KSDATA=6, KSSTUF=7, KSBOTH=8, KSLEFT=9,
     2   KSWRIT=10, KSAREF=11, KSHEAD=12, KSXREF=13, KSDCOM=14,
     3   KSENTR=15, KSSUBR=16, KSFUNC=17)
c  Parameters used for extended types.
      integer NTSCAL, NTFUNC, NTAELT, NTEXPR, NTSUBS, NTARR
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=32,
     1   NTARR=40)
c  Parameter for input line length, and for saving information.
      integer LENLN2, MSAVEN, MSAVGL
      parameter (LENLN2 = 74, MSAVEN=2000, MSAVGL=800)
c
c  Common block
      character BUF1*(LENLN2), BUF2*64, CURFIL*8
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, IMPTYP(26), IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN(MSAVEN), KSAVGL(MSAVGL), KSTA, KTOK(66), KXINFO, LASGLO,
     2   LETDIF, LETIDX, LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL,
     3   LTOK, MENTRY, NAMTYP(MXNAME), NEWTYP, NTYP
      common /GLCHR1/ BUF1, BUF2, CURFIL
      common /GLINT1/ IH, IMPTYP, IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN, KSAVGL, KSTA, KTOK, KXINFO, LASGLO, LETDIF, LETIDX,
     2   LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL, LTOK, MENTRY,
     3   NAMTYP, NEWTYP, NTYP, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)
c
c   Locals
      integer MXSTK
      parameter (MXSTK=30)
      integer  I, I1, I2, INTEST(17), J, J1, K, K0, K1, K2, K3,
     1   KA, KDEL, KCSTK, KSTK, KSTK1(MXSTK), KSTK2(MXSTK),
     2   KSTK3(MXSTK), LACT, LOCACT(17), NEWGLO, NTYPA, NXTACT(20)
      character CSTK*256, GLTYPE*47, TMPNAM*8
c
c ************************ Data Statements *****************************
c
c                        1       2      3      4      5        6      7
c            8       9     10      11      12     13      14      15
c            16      17      18      19      20
      data NXTACT / NAETYP, NAITYP, NACOM, KAITYP, NAREF, NAPARM, NADEF,
     1   NAREF, NABOTH, NADEF, NAWRIT, KAITYP, NAREF, NAFARG, KAITYP,
     2   NAAARG, NAETYP, NAETYP, NAETYP, NAETYP /
c                   1 2 3 4 5 6 7 8  9 10 11 12 13 14 15 16 17
      data LOCACT / 1,2,3,4,6,7,8,9,10,11,12,14,15,17,18,19,20 /
c                             1         2         3         4
c                    12345678901234567890123456789012345678901234567
      data GLTYPE / '?mvirdzEe123456*?MVIRDZ*?luhqcy?###:##???LUHQCY' /
c                   1   2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
      data INTEST /-1, -1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0/

c
c ********************** Start of Executable Code **********************
c
c                  Start main processing
      call INIT
      NEWGLO = 1
c               Get the next file name (or option spec.)
  100 continue
      BUF1 = ' '
      read (7, '(A)', END = 800) BUF1
      I1 = 1
  150 if (BUF1(1:1) .eq. '!') go to 100
      if (BUF1(1:1) .eq. ' ') then
c                         Option specification
         I = 1
  160    I = I + 1
         if (BUF1(I:I) .eq. ' ') then
            if (BUF1(I:) .eq. ' ') go to 100
            go to 160
         end if
c                   Set flags for diagnostics
c                   12345678901
         J = index('iIlLpPfFeEgG', BUF1(I:I))
         if (J .ne. 0) then
            if (J .gt. 8) then
               if (J .gt. 11) then
                  open (9, FILE='glsave', FORM='UNFORMATTED',
     1               STATUS='OLD')
                  read (9) LFILE, LGLOB, LREFL, LNAME, LTYPST, NEWGLO
                  NEWGLO = LGLOB + 1
                  read (9) (FILNAM(I2), I2 = 1, LFILE)
                  read (9) (LENT(I2), I2 = 1, LFILE)
                  read (9) (KGLENT(I2), I2 = 1, LGLOB)
                  read (9) (KGLFRO(I2), I2 = 1, LGLOB)
                  read (9) (KGLNXT(I2), I2 = 1, LGLOB)
                  read (9) (KGLTO(I2), I2 = 1, LGLOB)
                  read (9) (LREF(I2), I2 = 1, LREFL)
                  read (9) (NAME(I2), I2 = 1, LNAME)
                  read (9) (TYPSTR(I2), I2 = 1, LTYPST)
                  IH = 0
                  call HASH(' RESTORE')
                  close (9)
               else if (J .eq. 10) then
                  if (IOENT .eq. 0) open (11, FILE='glents',
     1               STATUS='UNKNOWN')
                  IOENT = 11
               else
                  IOENT = -IOENT
               end if
            else
               LERREQ((J+1) / 2) = mod(J, 2) .eq. 0
            end if
         else
            print '(1X, A1, A)', BUF1(I:I),
     1         ' is an unknown diagnostic request.'
         end if
         go to 160
      end if
c               Get the root name for the file.
      I2 = index(BUF1(I1:), '/')
      if (I2 .eq. 0) I2 = index(BUF1(I1:), BS)
      if (I2 .eq. 0) I2 = index(BUF1(I1:), ':')
      if (I2 .ne. 0) then
         I1 = I1 + I2
         go to 150
      end if
      J = index(BUF1(I1:), '.')
      if (J .eq. 0) j = index(BUF1, ' ')
      if (J - I1 .gt. 8) stop 'GLOCH1 -- Filename is too long.'
      if (J .eq. 1) then
         if (I1 .ne. 1) print '(1X,A, A)', BUF1(1:I1+1),
     1       ' is not a valid file name.'
         go to 100
      end if
      TMPNAM = BUF1(I1:I1+J-2)
      LFILE = LFILE + 1
      if (LFILE .gt. MXFILE) then
         if (LFILE .eq. MXFILE + 1) call TXTOUT('1', BUF1)
         go to 800
      end if
      CURFIL = TMPNAM(1:J-1)
      FILNAM(LFILE) = CURFIL
      LENT(LFILE) = 0
      open(9, FILE=BUF1(1:index(BUF1, ' ')-1), STATUS='OLD')
c no-print      print '(1x, A)', BUF1(1:index(BUF1, ' ')-1)
      LASGLO = 0
      LINE = 0
c                       Starting the next program unit.
  210 KSTA = KSSTRT
      LSAVGL = 0
      LSAVEN = 0
      LGLOBX = MXGLOB
      LREFLX = MXLREF
c                       Set up the default implicit types.
      do 220 I = 1, 6
         IMPTYP(I) = KTYSP
         IMPTYP(I+8) = KTYINT
         IMPTYP(I+14) = KTYSP
         IMPTYP(I+20) = KTYSP
  220 continue
      IMPTYP(7) = KTYSP
      IMPTYP(8) = KTYSP
c
c               Get next line of the file and scan it.
  230 continue
      call PACK
c               Look up and enter the tokens on the line.
      if (LASGLO .eq. 0) then
         if (KSTA .le. KSDCOM) then
            if (KTOK(1) .eq. NAEOF) go to 100
c                  Make up a global entry using the file name.
            call HASH(CURFIL)
            IH = abs(IH)
            KGREF = 1
            LSAVGL = 1
            KSAVGL(1) = 0
            NTYP = KBGLO + KTYEXT
            KSTA = KSTA + 1000
            call GLOENT
            NTYP = NTYP + 13 * KBDIM + 8
            KSTA = KSTA - 1000
            NAMTYP(IH) = NTYP
         end if
      end if
      go to 260
c               Start of a new statement
  240 KSTK = 0
      KCSTK = 0
      KOUNT = 0
      NTYPA = 0
  260 continue
      ITOK = ITOK + 1
      KA = KTOK(ITOK)
      if (KA .lt. 0) then
         LACT = 0
         go to 290
      end if
      TMPNAM = BUF2(LTOK:KA)
      LTOK = KA + 1
      call HASH(TMPNAM)

      if (IH .gt. 0) then
c                       Seen before.
         NTYP = NAMTYP(IH)
         if (mod(NTYP/KBEXT, 2) .ne. 0) then
c                       Seen global entry for the first time.
            LSAVGL = LSAVGL + 1
            if (LSAVGL .gt. MSAVGL) stop
     1  'GLOCH1 -- Parameter MSAVGL (several places) must be bigger.'
            if (mod(NTYP/KBDIM, 16) .le. 13) then
               KSAVGL(LSAVGL) = NTYP
               NTYP = KBGLO*LSAVGL
               go to 270
            else
c                 Ignore intrinsics on intrinsic statement.
               if (NEWTYP .eq. 9) then
                  LSAVGL = LSAVGL - 1
                  go to 260
               end if
c                   Preserve type info. for intrinsics.
               if (NTYP .ge. KBGLO) then
                  KSAVGL(LSAVGL) = NTYP - KBEXT
                  NTYP = mod(NTYP, KBEXT)
               else
                  NTYP = NTYP - KBEXT
                  KSAVGL(LSAVGL) = NTYP + KBGLO*IH
               end if
               NTYP = NTYP + KBGLO*LSAVGL
            end if
            NAMTYP(IH) = NTYP
         else if (mod(NTYP/KBDIM, KBDIMW) .le. 13) then
            go to 270
         end if
         if (INTEST(KSTA) .gt. 0) then
            if (KTOK(ITOK+1) .eq. NAPUSH) go to 270
         else if (INTEST(KSTA) .lt. 0) then
            if (KTOK(ITOK+1) .ne. NAPUSH) go to 270
            if (LERRI) call TXTOUT(TMPNAM,
     1         ', an intrinsic is in above type stmt')
         end if
c                Evidently intrinsic should be a variable.
         if (LERRI) then
            if (mod(NTYP/KBREF, 4) .ne. 0) call TXTOUT(TMPNAM,
     1              ', was intrinsic, now is a variable.')
         end if
         NTYP = NTYP - mod(NTYP, KBGLO)
      else
c                      Seen for the first time.
         IH = -IH
         LSAVGL = LSAVGL + 1
         if (LSAVGL .gt. MSAVGL) stop
     1  'GLOCH1 -- Parameter MSAVGL (several places) must be bigger.'
         KSAVGL(LSAVGL) = IH
         NTYP = KBGLO*LSAVGL
      end if
  270 LACT = LOCACT(KSTA)
  280 KA = NXTACT(LACT)
      if (KA .gt. 0) go to 300
      LACT = -LACT
c
  290 KA = -KA
c              1   2   3   4   5   6   7   8   9  10  11  12  13  14
  300 go to (310,320,330,340,340,350,338,360,370,480,490,500,610,620,
     1   630, 640, 650, 240, 230, 710, 710), KA
c         15   16   17   18   19   20   21

c   KAETYP = 1  Flag error if declared, then set the type.
  310 J = mod(NTYP, KBDEC)
      if (mod(NTYP/KBDEC, 2) .eq. 1) then
         if (mod(NTYP/KBDIM, 16) .ge. 14) then
            if (NEWTYP .lt. 8) then
               if (KTOK(ITOK+1) .ne. NAPUSH) then

                  if (J .eq. 15) then
                     if (LERRI) call TXTOUT(TMPNAM,
     1', in the above type stmt. is assumed to be a generic intrinsic.')
                     go to 700
                  end if
                  if (LERRI) call TXTOUT(TMPNAM,
     1                ', an intrinsic is in above type stmt.')
                  if (mod(J, 8) .eq. NEWTYP) then
                     NTYP = (NTYP - mod(NTYP, 16)) + J
                     go to 700
                  end if
               end if
            end if
c                           Replace the global with a local variable.
            NTYP = (NTYP - mod(NTYP, KBGLO)) + KBDEC
         else
            if (NEWTYP .ge. 7) then
               if (NEWTYP .eq. 7) then
                  if (J .eq. 7) go to 700
                  if (J .eq. 8) then
                     J = 0
                     NTYP = NTYP - 8
                  end if
                  if (J .eq. 0)  then
                     NTYP = NTYP + 13 * KBDIM + 7
                     go to 700
                  end if
               else if (J .lt. 8) then
                  NTYP = NTYP + 13 * KBDIM + 8
                  if (mod(NTYP/KBREF, 2) .eq. 0) NTYP = NTYP + KBREF
                  go to 700
               end if
            end if
            if ((J .ne. 8) .and. (NEWTYP .ne. 8)) then
               if (LERRL) call TXTOUT(NAME(IH),' was declared already.')
            end if
         end if
      else
c           Flag that name has been declared or referenced.
         if (NEWTYP .ge. 7) then
            if (mod(NTYP/KBDEC, 2) .ne. 0) then
               if (KSTA .gt. KSDCOM) call TXTOUT (NAME(IH),
     1               ' is an entry already declared.')
            else if (NEWTYP .ne. 8) then
               NTYP = NTYP + KBDEC
            else if (KSTA .eq. KSTYPE) then
c                             Name in an external statement
               KGREF = NTYP/KBGLO
               call GLOENT
            end if
            if (mod(NTYP/KBREF, 2) .eq. 0) NTYP = NTYP + KBREF
            if (KSTA .ge. KSDCOM) then
c                                 Make up stuff for entry header
               KGREF = NTYP/KBGLO
               call GLOENT
               if (KSTA .eq. KSDCOM) go to 700
            else if (NEWTYP .eq. 7) then
               if (KTOK(ITOK+1) .ne. NAPUSH) then
                  KOUNT = 1
                  CTYPA(1:2) = 'E '
                  call LOCREF
               end if
            end if
            NTYP = NTYP + KBDIM * (13 - mod(NTYP/KBDIM, KBDIMW))
         else
            NTYP = NTYP + KBDEC
            if (J .ge. 8) NTYP = NTYP + 8
         end if
      end if
      NTYP = (NTYP - J) + NEWTYP
      go to 700
c   KAITYP = 2  Set the implicit type if type is not set.
  320 if (mod(NTYP, 16) .eq. 0) NTYP = NTYP +
     1   IMPTYP(ichar(NAME(IH)(1:1)) - LETIDX)
      go to 700
c   KAFARG = 3  Flag that name is a formal argument.
  330 if (mod(NTYP/KBDUM, 2) .eq. 0) NTYP = NTYP + KBDUM
c            Save address in list of formal arguments.
  335 LSAVEN = LSAVEN + 1
      if (LSAVEN .gt. MSAVEN) stop
     1   'GLOCH1 -- Increase value of parameter MSAVEN (all places).'
      KSAVEN(LSAVEN) = IH
      go to 700
c   KAWRIT = 7  KADEF if character, else KADEF
  338 if (mod(NTYP, 8) .eq. KTYCHR) go to 350
c   KAREF  = 4  Flag that name is referenced and maybe defined.
  340 if (mod(NTYP/KBREF, 2) .eq. 0) NTYP = NTYP + KBREF
      if (KA .ne. 5) go to 700
c   KADEF  = 6  Flag that name is defined.
  350 if (mod(NTYP/KBDEF, 2) .eq. 0) NTYP = NTYP + KBDEF
      go to 700
c   KACOM  = 8  Flag that name is in common.
  360 if (mod(NTYP/KBCOM, 2) .ne. 0) then
c                             Flag error name is in common twice.
         if (LERRL) call TXTOUT(TMPNAM, ' was already in common.')
      else
         NTYP = NTYP + KBCOM
         go to 335
      end if
      go to 700
c   KAAARG = 9  Flag that name is used as actual argument if approp.
  370 if (ITOK .gt. 2) then
         if ((KTOK(ITOK-1).ne.NAPUSH) .and. (KTOK(ITOK-1).ne.NAUPTY))
     1      go to 340
      end if
      K1 = ITOK
  380 K1 = K1 + 1
      if (KTOK(K1) .eq. NAKSTA) then
  385    K1 = K1 + 1
         if (KTOK(K1) .eq. NALINE) go to 340
         if (KTOK(K1) .ne. NAKSTA) go to 385
         K1 = K1 + 1
         go to 380
      else if (KTOK(K1) .eq. NAPUSH) then
         K2 = 0
  390    K2 = K2 + 1
  400    K1 = K1 + 1
         if (KTOK(K1) .eq. NALINE) go to 340
         if (KTOK(K1) .eq. NAPUSH) go to 390
         if (KTOK(K1) .ne. NAPOP) go to 400
         K2 = K2 - 1
         if (K2 .ne. 0) go to 380
         go to 380
      end if
      if ((KTOK(K1) .ne. NAPOP) .and. (KTOK(K1) .ne. NAUPTY)) go to 340
c Think we have an actual arg. rather than an expression.  (Actually, if
c if a substring ends on the next line, or the trailing delimiter
c falls on the next line, we do not get the actual argument flagged.
      if (mod(NTYP/KBARG, 2) .eq. 0) NTYP = NTYP + KBARG
      if ((mod(NTYP/KBPAR, 2).ne.0) .and. (NTYPA.eq.0)) NTYPA = NTEXPR
      K2 = mod(NTYP/KBDEF, 16)
      if ((K2.ge.4).and.(mod(K2,2).eq.0).and.(mod(NTYP,16).lt.7))then
         KGREF = NAMTYP(KSTK3(KSTK)) / KBGLO
         K2 = KSAVGL(KGREF)
c                      Skip Arithmetic Statement functions
         if (K2 .gt. KBGLO * LGLOBX) go to 700
         if (K2 .lt. KBGLO) then
c     If not entered yet, get entered.
            K1 = IH
            IH = KSTK3(KSTK)
            call GLOENT
            IH = K1
            K2 = KSAVGL(KGREF)
         end if
c                  Save info. on for later processing of global data.
         K2 = K2 - mod(K2, KBGLO) + KOUNT
         K1 = 2
  410    do 420 K = K1, LSAVEN
            if (IH .eq. KSAVEN(K)) go to 430
  420    continue
         if (K1 .eq. 2) stop
     1     'GLOCH1 -- Couldn''t find formal arg. or common variable.'
         go to 700
  430    K1 = K + 1
         do 440 I1 = K, 1, -1
            if (KSAVEN(I1) .lt. 0) go to 450
  440    continue
  450    I2 = K - I1
         K3 = KSAVEN(I1)
         J = 0
         if (KGLTO(-K3) .lt. 0) then
c                         Need to set proper count for common blocks.
  460       J = J + 1
            do 470 I1 = I1-1, 1, -1
               if (KSAVEN(I1) .eq. K3) go to 460
  470       continue
            I2 = I2 + 1024 * J
         end if
         KXINFO = KXINFO + 1
c ITEM1 is KBGLO*(index for known global item) + argument #
c ITEM2 is KBGLO*(index of thing called) + argument index.
c ITEM3 is KBGLO*(index of file) + line number in file.
         write (10) I2-KBGLO*K3, K2, KBGLO*LFILE+min(KBGLO-1,LINE)
c                    Entries may have more than one to set.
         if (KGLTO(-K3) .gt. 0) go to 410
      end if
      go to 700
c   KAPARM = 10  Flag that in a parameter statement and defined.
  480 if (mod(NTYP/KBPAR, 2) .ne. 0) then
c                             Flag mult. def. of parameter
         if (LERRL) call TXTOUT(TMPNAM,' was a parameter already.')
      else
         NTYP = NTYP + KBPAR + KBDEF
         if (.not. LERRP) then
            if (mod(NTYP/KBREF, 2) .eq. 0) NTYP = NTYP + KBREF
         end if
      end if
      go to 320
c   KAPUSH = 11  Push KSTA, KOUNT, IH, and types
  490 KSTK = KSTK + 1
      if (KSTK .gt. MXSTK) stop
     1   'GLOCH1 -- Increase value of parameter MXSTK (all places).'
      KSTK1(KSTK) = KSTA
      KSTK2(KSTK) = KOUNT
      KSTK3(KSTK) = IH
      if (KOUNT .gt. 0) then
         CTYPA(KOUNT:KOUNT) = '-'
         CSTK(KCSTK+1:KCSTK+KOUNT) = CTYPA(1:KOUNT)
         KCSTK = KCSTK+KOUNT
      end if
      KOUNT = 1
      CTYPA = ' '
c    Set the current state based on the last token and KSTA.
      if (IH .eq. 0) then
         KSTA = KSSTUF
         go to 260
      end if
      NTYP = NAMTYP(IH)
      K = mod(NTYP/KBDIM, 16)
      if ((K .eq. 0) .and. (KSTA .ge. KSSTUF)) then
         if (mod(NTYP,16) .eq. KTYCHR) then
c                                    Check if substring
            if ((KTOK(ITOK+1) .eq. NAEXPR) .and.
     1         (KTOK(ITOK+2) .eq. NTSUBS)) then
c                                    Flag substring
               KSTK3(KSTK) = -IH
               go to 495
            end if
         end if
         NTYP = NTYP + KBDIM * 13 + 8
         K = 13
      end if
  495 if (K .eq. 13) then
         if (KSTA .gt. KSDCOM) then
            KSTA = KSHEAD
         else
            KSTA = KSXREF
         end if
         NTYPA = mod(NTYP, 16)
         CTYPA(KOUNT:KOUNT) = GLTYPE(NTYPA+1:NTYPA+1)
         if (KTOK(ITOK+1) .eq. NAPOP) go to 700
         KOUNT = KOUNT + 1
      else
         KSTA = KSAREF
      end if
      NTYPA = 0
      NTYP = 0
      go to 700
c   KAPOP  = 12  POP
  500 CTYPA(KOUNT:KOUNT) = GLTYPE(NTYPA+1:NTYPA+1)
      NTYP = NTYPA
      NTYPA = KCSTK
      if (KCSTK .ne. 0) NTYPA = index(GLTYPE, CSTK(KCSTK:KCSTK)) - 1
      IH = KSTK3(KSTK)
      if (IH .le. 0) then
         if (IH .lt. 0) then
            if ((KOUNT .ne. 1) .or. (index('ihI:', CTYPA(1:1)) .eq. 0))
     1        call TXTOUT(NAME(-IH),' has peculiar substring notation.')
         end if
         if (NTYP / 8 .eq. 4) NTYP = 2
         go to 600
      end if
      NTYP = NAMTYP(IH)
      J = mod(NTYP/KBDIM, 16)
      K = mod(NTYP, 16)
      if (K .le. 6) then
c                        An array (probably)
         if (J .eq. 0) then
c                        Something seen for the first time.
            if (KSTK1(KSTK) .le. KSCOMM) then
c                                    Array is being declared.
               if (KOUNT .gt. 10) then
c                       Error too many array dimensions
                  if (LERRL) call TXTOUT(NAME(IH),
     1                ' has too many dimensions declared.')
               else
                  NTYP = NTYP + KBDIM * KOUNT
               end if
            else if (CTYPA(KOUNT:KOUNT) .eq. '#') then
               go to 600
            else
               if (KSTK1(KSTK) .eq. KSLEFT) then
c                              Set KSTA = 0 to flag statement function.
                  KSTA = 0
               else if (LERRL) then
c                     Error -- function entry is not declared
                  call TXTOUT(NAME(IH),
     1               ' has not been declared as external.')
               end if
               J = 13
               NTYP = NTYP - K
               if (K .eq. 0) K = IMPTYP(ichar(NAME(IH)(1:1)) - LETIDX)
               if (K .lt. 8) K = K + 8
               NTYP = NTYP + K + 13 * KBDIM
               NAMTYP(IH) = NTYP
               go to 520
            end if
            NAMTYP(IH) = NTYP
         else if (KOUNT .ne. J) then
c                     Error -- different number of array dimensions.
            if (LERRL) call TXTOUT(NAME(IH),
     1         ' has different number of dimensions.')
         end if
         do 510 K = 1, KOUNT
            if (index('ihI:', CTYPA(K:K)) .eq. 0) then
c       Error bad type in subscript (Note ':' is from a substring decl.)
               if (LERRL) call TXTOUT(NAME(IH),
     1            ' has a noninteger type as subscript.')
            end if
  510    continue
         NTYP = mod(NTYP,8) + NTAELT
         go to 600
      end if
  520 if (J .ge. 14) then
c      If intrinsic could be replaced by generic intrinsic print error.
          if ((J .eq. 15) .and. LERRI) call TXTOUT(NAME(IH),
     1       ' has a generic intrinsic replacement')
         if (KOUNT .gt. 1) then
            if (KOUNT .gt. 2) then
               if (LERRI) call TXTOUT(NAME(IH),
     1        ', has more than 2 arguments (warning for C conversion).')
            end if
            K2 = mod(index(GLTYPE, CTYPA(1:1)) - 1, 8)
            do 530 K1 = 2, KOUNT
               if (K2 .ne. mod(index(GLTYPE, CTYPA(K1:K1)) - 1, 8)) then
                  if (LERRL .and. (NAME(IH) .ne. 'INDEX'))
     1               call TXTOUT(NAME(IH),
     2            ' is an intrinsic with arguments of different types.')
                  go to 540
               end if
  530       continue
         end if
  540    if (K .eq. 15) then
c                                Generic intrinsic
            NTYP = index(GLTYPE, CTYPA(1:1)) - 1
c                    Check for bad type in intrinsic
            if (NTYP .le. 0) then
               if (LERRL) call TXTOUT(NAME(IH),
     1            ' has an unknown generic type.')
            end if
         end if
      else if (KSTK1(KSTK) .ge. KSDCOM) then
c            End of an entry or common definition
          go to 600
      else
c                  End of an external reference
         call LOCREF
      end if
      NTYP = mod(NTYP, 8) + NTEXPR
  600 KSTA = KSTK1(KSTK)
      KOUNT = KSTK2(KSTK)
      KSTK = KSTK - 1
      if (KOUNT .gt. 0) then
         KCSTK = KCSTK - KOUNT
         CTYPA(1:KOUNT) = CSTK(KCSTK+1:KCSTK+KOUNT)
      end if
      go to 650

c   KAUPTY = 13 Update the type information.
  610 CTYPA(KOUNT:KOUNT) = GLTYPE(NTYPA+1:NTYPA+1)
      KOUNT = KOUNT + 1
  615 NTYPA = 0
      NTYP = 0
      go to 260
c   KAKSTA = 14 Get new KSTA from next token in list.
  620 ITOK = ITOK + 1
      KSTA = KTOK(ITOK)
      go to 615
c   KAENDT = 15 Ends previous token
  630 IH = 0
      go to 615
c   KAEXPR = 16 Set NTYPA from next token
  640 ITOK = ITOK + 1
      NTYP = KTOK(ITOK)
c   KATYPE = 17 Get update to NTYPA using current token in list.
  650 if (NTYP .ge. KBEXT) NTYP = mod(NTYP, 16)
      if (NTYPA .eq. 0) then
         NTYPA = NTYP
      else

         K1 = mod(NTYP, 8)
         K2 = mod(NTYPA, 8)
         K3 = max(NTYP-K1, NTYPA-K2, NTEXPR)
         if (K1 .ne. K2) then
            if (K1 .gt. K2) then
               J1 = K1
               K1 = K2
               K2 = J1
            end if
            if (K2 .eq. 7) K2 = K1
            if (K1 .le. KTYCHR) then
               if (K1 .ne. 0) then
                  K2 = K1
                  if (K1 .eq. KTYCHR) K2 = KTYCHR - K3
               end if
            end if
            if ((K3 .eq. NTSUBS) .and. (KSTK .gt. 0)) then
               if (K2.ne.KTYINT) call TXTOUT(' ',
     1            ' Problem getting types on above line.')
            end if
         end if
         NTYPA = K2 + K3
      end if
      go to 260
c     After action associated with symbol table entry.
  700 if (LACT .le. 0) then
         if (LACT .ne. 0) then
c                Save info. for symbol table.
            K = NTYP
            NAMTYP(IH) = K
            if (KTOK(ITOK+1) .gt. 0) then
               NTYPA = 0
               NTYP=0
            else if (KTOK(ITOK+1) .eq. NAPUSH) then
               go to 260
            else
               NTYP = mod(K, 16)
               if (NTYP .ge. 7) go to 650
               K = mod(K/KBDIM, KBDIMW)
               if (K .ne. 0) then
                  if (K .lt. 13) then
                     NTYP = NTYP + NTARR
                  else
                     NTYP = NTYP + NTFUNC
                  end if
               end if
               go to 650
            end if
         end if
         go to 260
      end if
c                More actions to process.
      LACT = LACT + 1
      go to 280
c                    Done with subprogram or program -- Clean up
  710 continue
c              Process the formals and the common block.
      K1 = 0
      LSAVEN = LSAVEN + 1
      if (LSAVEN .gt. MSAVEN) stop
     1   'GLOCH1 -- Increase value of parameter MSAVEN (all places).'
      KSAVEN(LSAVEN) = 0
      call wait
      do 740 I = 1, LSAVEN
         I1 = KSAVEN(I)
         if (I1 .le. 0) then
            if (K1 .gt. 0) then
c                          Save the type info.
               K1 = K1 + 1
               CTYPA(K1:K1) = ' '
c Search for the string and enter if not in list.
               if (KGLTO(I2) .lt. 0) then
c                                          A common block
                  KOUNT = K1 - 1
                  NTYP = NAMTYP(KGLENT(I2))
                  call LOCREF
               else
c                                          An entry
                  call LOCSTR (CTYPA(1:K1), KGLTO(I2)-3)
               end if
            end if
            if (I1 .eq. 0) go to 740
            K1 = 0
            I2 = -I1
            K0 = KGLTO(I2)
            if (K0 .lt. 0) go to 740
            I1 = KGLENT(I2)
            if (mod(NAMTYP(I1),16).eq.0) NAMTYP(I1) = NAMTYP(I1)+KTYEXT
         end if
         K1 = K1 + 1
         K2 = NAMTYP(I1)
         K3 = mod(K2 / KBDIM, KBDIMW)
         J = mod(K2, 16)
         if (J .eq. 0) J = IMPTYP(ichar(NAME(I1)(1:1)) - LETIDX)
         if (J .lt. 7) then
            J = 8 * J - 7
            if (K3 .ne. 0) J = J + 4
            if (mod(K2/KBDEF, 2) .ne. 0) then
               J = J + 2
c     If a formal argument is defined, don't flag that it is referenced.
               if (K0 .gt. 0) go to 730
            end if
            J = J + mod(K2/KBREF, 2)
         else if (J .eq. 7) then
            J = 56
         else
            J = 49 + mod(J, 8)
         end if
  730    CTYPA(K1:K1) = GLTYPC(J:J)
  740 continue
      KDEL = 0
      do  780 I = 1, LSAVGL
         K1 = KSAVGL(I)
         if (K1 .ge. KBGLO) then
c                            An associated global entry.
            J = K1 / KBGLO
            if (mod(K1/KBEXT, 2) .eq. 0) then
c                                         An intrinsic
               NAMTYP(J) = K1 - KBGLO * J + KBEXT
               go to 780
            else
               IH = KGLENT(J)
               if (J .le. LGLOBX) then
                  NAMTYP(IH) = KBGLO*J + KBEXT
                  go to 780
               end if
            end if
c                   Must be a statement function -- delete it.
         else
c                                A local variable
            IH = K1
            I1 = NAMTYP(IH)
         end if
c                             Add to delete list.
         NAMTYP(IH) = KDEL
         KDEL = IH
c                          I1 = local symbol table entry
  760    if (LERRL) then
c                      A local entry.
            K2 = mod(I1, KBPAR)
            if (K2 .lt. KBARG) then
c             Local variables should be both referenced and defined.
               if (mod(K2/KBREF,2) .eq. 0) call TXTOUT(NAME(IH),
     1            ' was never referenced.')
               if (mod(K2/KBDEF,2) .eq. 0) call TXTOUT(NAME(IH),
     1            ' was never assigned a value.')
            else if (mod(K2 / KBDUM, 2) .eq. 1) then
c              Dummy arguments should not be in common or ignored.
               if (K2 .ge. KBCOM) then
                  call TXTOUT(NAME(IH),
     1               ' is a formal argument and in common.')
               else if (mod(K2/KBREF, 8) .eq. 0) then
                  call TXTOUT(NAME(IH),
     1               ', a formal argument is not used.')
               end if
            end if
         end if
  780 continue
      if (KDEL .ne. 0) then
c                                Delete from the hash table.
         IH = -KDEL
         call HASH('        ')
      end if
      if (KA .ne. KAEOF) go to 210
c                    Done with the file
      go to 100
c
c  Done processing input files
  800 continue
      close(8)
      close(7)
      if (IOENT .ne. 0) close (11)
      if (KXINFO .eq. 0) then
         close (10, STATUS='DELETE')
      else
         close (10)
      end if
      open (9, FILE='glsave', FORM='UNFORMATTED', STATUS='UNKNOWN')
      write (9) LFILE, LGLOB, LREFL, LNAME, LTYPST, NEWGLO
      write (9) (FILNAM(J), J = 1, LFILE)
      write (9) (LENT(J), J = 1, LFILE)
      write (9) (KGLENT(J), J = 1, LGLOB)
      write (9) (KGLFRO(J), J = 1, LGLOB)
      write (9) (KGLNXT(J), J = 1, LGLOB)
      write (9) (KGLTO(J), J = 1, LGLOB)
      write (9) (LREF(J), J = 1, LREFL)
      write (9) (NAME(J), J = 1, LNAME)
      write (9) (TYPSTR(J), J = 1, LTYPST)

      if (.false.) then
      print '(''   LFILE,  LGLOB,  LREFL,  LNAME, LTYPST, NEWGLO:''/
     1 6I8)', LFILE, LGLOB, LREFL, LNAME, LTYPST, NEWGLO


      do J = 1, LFILE
        print '(''J,FILNAM,LENT:'',I5,1X,A8,I6 )',J, FILNAM(J),LENT(J)
      end do

      print '(''      J, KGLENT, KGLPRO, KGLNXT,  KGLTO'')'
      do J = 1, LGLOB
        print '(5I8)', J, KGLENT(J), KGLFRO(J), KGLNXT(J), KGLTO(J)
      end do

      do J = 1, LREFL
        print '(''J, LREF:'', 2I8)', J, LREF(J)
      end do

      do J = 1, LNAME
        print '(''J, NAME:'', I8, 1X, A8)', J, NAME(J)
      end do

      do J = 1, LTYPST
        print '(''J,TYPSTR:'', I4,2x,A)', J, TYPSTR(J)
      end do

      print '(''MXNAME, LFREEN:'', 2I8)', MXNAME, LFREEN
      print '(''   K, Type, Use, Dim, X, Pt_glo'')'
      do K = 1, MXNAME
        if (NAMTYP(K)/65536 .ne. 0) then
          print '(I5, I6, I5, I5, I3, I7)',
     1      K, mod(NAMTYP(K), 16), mod(NAMTYP(K)/16, 2048/16),
     2      mod(NAMTYP(K)/2048,16), mod(NAMTYP(K)/32768, 2),
     3      NAMTYP(K) / 65536
        end if
      end do

c      do K = 1, MXNAME
c        print '(''K, HTABLE:'', 2I8)', K, HTABLE(K)
c      end do
      end if


      IH = 0
      call HASH(' SAVE   ')
      print 900, LFILE, LGLOB, LREFL, LNAME, LTYPST, KXINFO
  900 format(/' First phase of processing is complete.'/
     1  ' LFILE =', I5, '   LGLOB =', I5, '   LREFL =', I6/
     2  ' LNAME =', I5, '  LTYPST =', I5, '  KXINFO =', I6/
     3  ' Continue processing by running GLOCH2.')
      close (9)
      stop
c                   End of GLOCH1
      end

      subroutine GLOENT
c Enters information for a global name reference.
c
c Common and parameters required in both GLOCH1 and GLOCH2
      integer MXGLOB, MXLREF, MXNAME, MXTYST, MXTLEN
      parameter (MXGLOB=2020, MXLREF=25000, MXNAME=2333, MXTYST=200,
     1   MXTLEN=256)
c    Misc. Parameters
      integer LONG, MXFILE
      parameter (LONG=7, MXFILE=1500)
c  Parameters used for character mappings
      integer MAPLC, MAPUC, MAPDIG, MAPDOT, MAPUPM, MAPOTH, MAPSTR,
     1  MAPEXC, MAPDIV, MAPEQ, MAPRP, MAPLP, MAPCOM, MAPCOL
      parameter (MAPLC=1, MAPUC=2, MAPDIG=3, MAPDOT=4, MAPUPM=5,
     1 MAPOTH=6, MAPSTR=7, MAPEXC=8, MAPDIV=9, MAPEQ=10,
     2 MAPLP = 11, MAPRP=12, MAPCOM=13, MAPCOL=14)
c Parameters associated with bits in symbol table entries.
      integer KBDEC, KBREF, KBDEF, KBARG, KBDUM, KBCOM, KBPAR, KBDIM,
     1   KBEXT, KBGLO, KBDIMW
      parameter (KBDEC=16, KBREF=32, KBDEF=64, KBARG=128, KBDUM=256,
     1  KBCOM=512, KBPAR=1024, KBDIM=2048, KBEXT=32768, KBGLO=65536)
      parameter (KBDIMW = KBEXT / KBDIM)
c
      character CTYPA*(MXTLEN), FILNAM(MXFILE)*8, GLTYPC*56,
     1   NAME(MXNAME)*8, TYPSTR(MXTYST)*(MXTLEN)
      integer KGLENT(MXGLOB), KGLFRO(MXGLOB), KGLNXT(MXGLOB),
     1   KGLTO(MXGLOB), LENT(MXFILE), LFILE,
     2   LGLOB, LNAME, LREF(MXLREF), LREFL, LTYPST, MAP(128)
      common /GLCHRB/ CTYPA, FILNAM, GLTYPC, NAME, TYPSTR
      common /GLINTB/ KGLENT, KGLFRO, KGLNXT, KGLTO,
     1   LENT, LFILE, LGLOB, LNAME, LREF, LREFL,
     2   LTYPST, MAP

c Common and parameters required only in GLOCH1
c  Parameters used for actions.
      integer KAETYP, KAITYP, KAFARG, KAREF, KABOTH, KADEF, KAWRIT,
     1   KACOM, KAAARG, KAPARM, KAPUSH, KAPOP, KAUPTY, KAKSTA, KAENDT,
     2   KAEXPR, KATYPE, KASTMT, KALINE, KAEND, KAEOF
      parameter (KAETYP=1, KAITYP=2, KAFARG=3, KAREF=4, KABOTH=5,
     1   KADEF=6, KAWRIT=7, KACOM=8, KAAARG=9, KAPARM=10, KAPUSH=11,
     2   KAPOP =12, KAUPTY=13, KAKSTA=14, KAENDT=15, KAEXPR=16,
     3   KATYPE=17, KASTMT=18, KALINE=19, KAEND=20, KAEOF=21)
c    Negative of the KAxxxx parameters.
      integer NAETYP, NAITYP, NAFARG, NAREF, NABOTH, NADEF, NAWRIT,
     1   NACOM, NAAARG, NAPARM, NAPUSH, NAPOP, NAUPTY, NAKSTA, NAENDT,
     2   NAEXPR, NATYPE, NASTMT, NALINE, NAEND, NAEOF
      parameter (NAETYP=-KAETYP, NAITYP=-KAITYP, NAFARG=-KAFARG,
     1   NAREF=-KAREF, NABOTH=-KABOTH, NADEF=-KADEF, NAWRIT=-KAWRIT,
     2   NACOM=-KACOM, NAAARG=-KAAARG, NAPARM=-KAPARM, NAPUSH=-KAPUSH,
     3   NAPOP =-KAPOP, NAUPTY=-KAUPTY, NAKSTA=-KAKSTA, NAEXPR=-KAEXPR,
     4   NAENDT=-KAENDT, NATYPE=-KATYPE, NASTMT=-KASTMT, NALINE=-KALINE,
     5   NAEND=-KAEND, NAEOF=-KAEOF)
c  Parameters used for types.
      integer KTYUND, KTYLOG, KTYCHR, KTYINT, KTYSP, KTYDP, KTYCOM,
     1   KTYSUB, KTYEXT
      parameter (KTYUND=0, KTYLOG=1, KTYCHR=2, KTYINT=3, KTYSP=4,
     1   KTYDP=5, KTYCOM=6, KTYSUB=7, KTYEXT=8)
c  Parameters used to track the state of processing.
      integer KSSTRT, KSNONE, KSTYPE, KSDIM, KSCOMM, KSDIMD, KSPARM,
     1   KSDATA, KSSTUF, KSBOTH, KSLEFT, KSWRIT, KSAREF, KSHEAD, KSXREF,
     2   KSDCOM, KSENTR, KSSUBR, KSFUNC
      parameter (KSSTRT=-1, KSNONE=0, KSTYPE=1, KSDIM=2, KSCOMM=3,
     1   KSDIMD=4,  KSPARM=5, KSDATA=6, KSSTUF=7, KSBOTH=8, KSLEFT=9,
     2   KSWRIT=10, KSAREF=11, KSHEAD=12, KSXREF=13, KSDCOM=14,
     3   KSENTR=15, KSSUBR=16, KSFUNC=17)
c  Parameters used for extended types.
      integer NTSCAL, NTFUNC, NTAELT, NTEXPR, NTSUBS, NTARR
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=32,
     1   NTARR=40)
c  Parameter for input line length.
      integer LENLN2, MSAVEN, MSAVGL
      parameter (LENLN2 = 74, MSAVEN=2000, MSAVGL=800)
c
c  Common block
      character BUF1*(LENLN2), BUF2*64, CURFIL*8
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, IMPTYP(26), IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN(MSAVEN), KSAVGL(MSAVGL), KSTA, KTOK(66), KXINFO, LASGLO,
     2   LETDIF, LETIDX, LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL,
     3   LTOK, MENTRY, NAMTYP(MXNAME), NEWTYP, NTYP
      common /GLCHR1/ BUF1, BUF2, CURFIL
      common /GLINT1/ IH, IMPTYP, IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN, KSAVGL, KSTA, KTOK, KXINFO, LASGLO, LETDIF, LETIDX,
     2   LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL, LTOK, MENTRY,
     3   NAMTYP, NEWTYP, NTYP, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)
c
c   Locals
      integer K
c
c **************************** Start of Executable Code ****************
c
   30 if (mod(KSAVGL(KGREF)/KBEXT, 2) .eq. 0) then
c                        Global entry does not yet exist.
         LGLOB = LGLOB + 1
         if (LGLOB .gt. MXGLOB) stop
     1      'GLOCH1 -- Need to increase parameter MXGLOB'
         KGLENT(LGLOB) = IH
         LREFL = LREFL + 4
         KGLTO(LGLOB) = LREFL
         if (KSTA .eq. KSDCOM) then
            LREFL = LREFL + 1
            LREF(LREFL-3) = 0
            LREF(LREFL-4) = 0
            KGLTO(LGLOB) = -LREFL
         end if
         if (LREFL .gt. LREFLX) stop
     1      'GLOCH1 -- Need to increase parameter MXLREF'
         LREF(LREFL-2) = 0
         LREF(LREFL-1) = 0
         LREF(LREFL) = 0
         KGLFRO(LGLOB) = 0
         if (mod(KSAVGL(KGREF)/KBDIM, KBDIMW) .ge. 14) then
             K = LASGLO
             LASGLO = LGLOB
             call TXTOUT(NAME(IH),
     1          ' is no longer intrinsic, is now external.')
             LASGLO = K
             KSAVGL(KGREF) = mod(KSAVGL(KGREF), KBGLO)
         end if
         KSAVGL(KGREF) = KSAVGL(KGREF) +  LGLOB * KBGLO + KBEXT
         if (KSTA .le. KSXREF) return
         K = LGLOB
      else
         if (KSTA .le. KSXREF) return
         K = KSAVGL(KGREF) / KBGLO
      end if
c                                K is the global data entry.
      if (KSTA .eq. KSDCOM) then
         NTYP = NTYP - mod(NTYP, KBEXT) + 11 * KBDIM + 8 + KBREF
         go to 50
      end if
c                                Get KSTA, set NTYP
      if (LASGLO .ne. 0) then
         KGLNXT(LASGLO) = K
         if (KSTA .eq. KSENTR) then
            KGLFRO(K) = -MENTRY
c              Get KSTA from the main entry, this is a multiple entry
            KSTA = KSSUBR
            if (mod(NAMTYP(KGLENT(MENTRY)), 8) .ne. 7) KSTA = KSFUNC
            go to 40
         end if
      end if
      MENTRY = K
   40 LASGLO = K
      if (LENT(LFILE) .eq. 0) LENT(LFILE) = LASGLO
      if (KSTA .gt. 1000) go to 60
c         Set up for saving arguments, and set count to 0
   50 LSAVEN = LSAVEN + 1
      if (LSAVEN .gt. MSAVEN) stop
     1   'GLOCH1 -- Increase value of parameter MSAVEN (all places).'
      KSAVEN(LSAVEN) = -K
   60 KGLNXT(K) = -LFILE
      return
c                     End of GLOENT
      end

      subroutine HASH (HNAME)
c             Hash a name.
c
c ******************** Variable Definitions ****************************
c
c HNAME  The name being looked up, always 8 characters.
c HTABLE If HTABLE(k) = 0, there is nothing in this hash table location,
c    if it is > 0, it is the first in a possible chain of entries.  If
c    it is < 0, it contains an entry of a chain, that did not hash to
c    this location.
c K, K1, K2  Temporary indices.
c IH     The index of the symbol table found.  (Negated if this is a new
c    entry.)  On entry this must be > 0 if doing a lookup.
c L, L1, L2, L3  Temporary indices.
c LOOP1 logical var. used to catch loop if hash table is full.
c
c ******************** Variable Declarations ***************************
c
c Common and parameters required in both GLOCH1 and GLOCH2
      integer MXGLOB, MXLREF, MXNAME, MXTYST, MXTLEN
      parameter (MXGLOB=2020, MXLREF=25000, MXNAME=2333, MXTYST=200,
     1   MXTLEN=256)
c    Misc. Parameters
      integer LONG, MXFILE
      parameter (LONG=7, MXFILE=1500)
c  Parameters used for character mappings
      integer MAPLC, MAPUC, MAPDIG, MAPDOT, MAPUPM, MAPOTH, MAPSTR,
     1  MAPEXC, MAPDIV, MAPEQ, MAPRP, MAPLP, MAPCOM, MAPCOL
      parameter (MAPLC=1, MAPUC=2, MAPDIG=3, MAPDOT=4, MAPUPM=5,
     1 MAPOTH=6, MAPSTR=7, MAPEXC=8, MAPDIV=9, MAPEQ=10,
     2 MAPLP = 11, MAPRP=12, MAPCOM=13, MAPCOL=14)
c Parameters associated with bits in symbol table entries.
      integer KBDEC, KBREF, KBDEF, KBARG, KBDUM, KBCOM, KBPAR, KBDIM,
     1   KBEXT, KBGLO, KBDIMW
      parameter (KBDEC=16, KBREF=32, KBDEF=64, KBARG=128, KBDUM=256,
     1  KBCOM=512, KBPAR=1024, KBDIM=2048, KBEXT=32768, KBGLO=65536)
      parameter (KBDIMW = KBEXT / KBDIM)
c
      character CTYPA*(MXTLEN), FILNAM(MXFILE)*8, GLTYPC*56,
     1   NAME(MXNAME)*8, TYPSTR(MXTYST)*(MXTLEN)
      integer KGLENT(MXGLOB), KGLFRO(MXGLOB), KGLNXT(MXGLOB),
     1   KGLTO(MXGLOB), LENT(MXFILE), LFILE,
     2   LGLOB, LNAME, LREF(MXLREF), LREFL, LTYPST, MAP(128)
      common /GLCHRB/ CTYPA, FILNAM, GLTYPC, NAME, TYPSTR
      common /GLINTB/ KGLENT, KGLFRO, KGLNXT, KGLTO,
     1   LENT, LFILE, LGLOB, LNAME, LREF, LREFL,
     2   LTYPST, MAP

c Common and parameters required only in GLOCH1
c  Parameters used for actions.
      integer KAETYP, KAITYP, KAFARG, KAREF, KABOTH, KADEF, KAWRIT,
     1   KACOM, KAAARG, KAPARM, KAPUSH, KAPOP, KAUPTY, KAKSTA, KAENDT,
     2   KAEXPR, KATYPE, KASTMT, KALINE, KAEND, KAEOF
      parameter (KAETYP=1, KAITYP=2, KAFARG=3, KAREF=4, KABOTH=5,
     1   KADEF=6, KAWRIT=7, KACOM=8, KAAARG=9, KAPARM=10, KAPUSH=11,
     2   KAPOP =12, KAUPTY=13, KAKSTA=14, KAENDT=15, KAEXPR=16,
     3   KATYPE=17, KASTMT=18, KALINE=19, KAEND=20, KAEOF=21)
c    Negative of the KAxxxx parameters.
      integer NAETYP, NAITYP, NAFARG, NAREF, NABOTH, NADEF, NAWRIT,
     1   NACOM, NAAARG, NAPARM, NAPUSH, NAPOP, NAUPTY, NAKSTA, NAENDT,
     2   NAEXPR, NATYPE, NASTMT, NALINE, NAEND, NAEOF
      parameter (NAETYP=-KAETYP, NAITYP=-KAITYP, NAFARG=-KAFARG,
     1   NAREF=-KAREF, NABOTH=-KABOTH, NADEF=-KADEF, NAWRIT=-KAWRIT,
     2   NACOM=-KACOM, NAAARG=-KAAARG, NAPARM=-KAPARM, NAPUSH=-KAPUSH,
     3   NAPOP =-KAPOP, NAUPTY=-KAUPTY, NAKSTA=-KAKSTA, NAEXPR=-KAEXPR,
     4   NAENDT=-KAENDT, NATYPE=-KATYPE, NASTMT=-KASTMT, NALINE=-KALINE,
     5   NAEND=-KAEND, NAEOF=-KAEOF)
c  Parameters used for types.
      integer KTYUND, KTYLOG, KTYCHR, KTYINT, KTYSP, KTYDP, KTYCOM,
     1   KTYSUB, KTYEXT
      parameter (KTYUND=0, KTYLOG=1, KTYCHR=2, KTYINT=3, KTYSP=4,
     1   KTYDP=5, KTYCOM=6, KTYSUB=7, KTYEXT=8)
c  Parameters used to track the state of processing.
      integer KSSTRT, KSNONE, KSTYPE, KSDIM, KSCOMM, KSDIMD, KSPARM,
     1   KSDATA, KSSTUF, KSBOTH, KSLEFT, KSWRIT, KSAREF, KSHEAD, KSXREF,
     2   KSDCOM, KSENTR, KSSUBR, KSFUNC
      parameter (KSSTRT=-1, KSNONE=0, KSTYPE=1, KSDIM=2, KSCOMM=3,
     1   KSDIMD=4,  KSPARM=5, KSDATA=6, KSSTUF=7, KSBOTH=8, KSLEFT=9,
     2   KSWRIT=10, KSAREF=11, KSHEAD=12, KSXREF=13, KSDCOM=14,
     3   KSENTR=15, KSSUBR=16, KSFUNC=17)
c  Parameters used for extended types.
      integer NTSCAL, NTFUNC, NTAELT, NTEXPR, NTSUBS, NTARR
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=32,
     1   NTARR=40)
c  Parameter for input line length.
      integer LENLN2, MSAVEN, MSAVGL
      parameter (LENLN2 = 74, MSAVEN=2000, MSAVGL=800)
c
c  Common block
      character BUF1*(LENLN2), BUF2*64, CURFIL*8
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, IMPTYP(26), IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN(MSAVEN), KSAVGL(MSAVGL), KSTA, KTOK(66), KXINFO, LASGLO,
     2   LETDIF, LETIDX, LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL,
     3   LTOK, MENTRY, NAMTYP(MXNAME), NEWTYP, NTYP
      common /GLCHR1/ BUF1, BUF2, CURFIL
      common /GLINT1/ IH, IMPTYP, IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN, KSAVGL, KSTA, KTOK, KXINFO, LASGLO, LETDIF, LETIDX,
     2   LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL, LTOK, MENTRY,
     3   NAMTYP, NEWTYP, NTYP, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)
c
c   Locals
      logical LOOP1
      character HNAME*8
      integer K, K1, K2, L, L1, L2, L3, NEXTH
      integer HTABLE(MXNAME)
      save HTABLE, NEXTH
c
c **************************** Start of Executable Code ****************
c
      if (HNAME(1:1) .eq. ' ') then
         if (IH .eq. 0) then
            IH = 1
            if (HNAME .eq. ' ') then
c                 Set up free space pointers and initialize HTABLE
               do 10 K = 1, MXNAME
                  HTABLE(K) = 0
   10          continue
            else if (HNAME .eq. ' SAVE') then
               write (9) MXNAME, LFREEN
               write (9) (NAMTYP(K), K = 1, LNAME)
               write (9) (HTABLE(K), K = 1, MXNAME)

               if (.false.) then
                 print '(''MXNAME ='' I6, ''  LFREEN ='', I6)',
     1             MXNAME, LFREEN
                 do 20 K = 1, LNAME
                   print '(''K,NAMTYP:'', 2I7)', K, NAMTYP(K)
 20              continue
                 do 22 K = 1, MXNAME
                   print '(''K,HTABLE:'', 2I7)', K, MXNAME
 22              continue
               end if

            else if (HNAME .eq. ' RESTORE') then
               read (9) K1, LFREEN
               if (K1 .ne. MXNAME) stop
     1            'GLOCH1 -- Incombatible hash table formats'
               read (9) (NAMTYP(K), K = 1, LNAME)
               read (9) (HTABLE(K), K = 1, MXNAME)
               NEXTH = 1
            else
               stop 'GLOCH1 -- Unknown hash table request'
            end if
            return
         end if
c
c             Get rid of entries marked to delete
         K = -IH
   30    continue
         K1 = ichar(NAME(K)(1:1)) * ichar(NAME(K)(2:2)) *
     1      ichar(NAME(K)(3:3)) * ichar(NAME(K)(4:4))
         K2 = ichar(NAME(K)(5:5)) * ichar(NAME(K)(6:6)) *
     1      ichar(NAME(K)(7:7)) * ichar(NAME(K)(8:8))
         L = abs(mod(K1+K2, MXNAME)) + 1
         L1 = L
         K1 = HTABLE(L1)
         if (K1 .eq. K) then
c                           Single entry in the chain.
            HTABLE(L) = 0
         else
   40       if (K .ne. mod(K1, KBGLO)) then
               L1 = K1 / KBGLO
               K1 = -HTABLE(L1)
               if (K1 .gt. 0) go to 40
               stop 'GLOCH1 -- Inconsistent hash table entry on delete.'
            end if
            L2 = K1 / KBGLO
            K1 = abs(HTABLE(L2))
            L3 = K1 / KBGLO
            if (L1 .eq. L3) then
c                           Only one in the chain now.
               HTABLE(L1) = 0
               HTABLE(L2) = 0
               HTABLE(L) = mod(K1, KBGLO)
            else if (HTABLE(L2) .lt. 0) then
c                           Shift one not at head of chain.
               HTABLE(L1) = sign(HTABLE(L2), HTABLE(L1))
               HTABLE(L2) = 0
            else
c                           Shift one after head of chain.
               K1 = mod(HTABLE(L3), KBGLO)
               HTABLE(L1) = K1 - L2 * KBGLO
               HTABLE(L2) = mod(HTABLE(L2), KBGLO) - (HTABLE(L3) - K1)
               HTABLE(L3) = 0
            end if
         end if
   50    K1 = NAMTYP(K)
         NAMTYP(K) = LFREEN
         LFREEN = K
         if (K1 .eq. 0) return
         K = K1
         go to 30
      end if
c
c                                          Do the Look up
      K1 = ichar(HNAME(1:1)) * ichar(HNAME(2:2)) * ichar(HNAME(3:3)) *
     1   ichar(HNAME(4:4))
      K2 = ichar(HNAME(5:5)) * ichar(HNAME(6:6)) * ichar(HNAME(7:7)) *
     1   ichar(HNAME(8:8))
      if (KSTA .eq. KSDCOM) K2 = K2 + 1
      L = abs(mod(K1+K2, MXNAME)) + 1
      if (HTABLE(L) .eq. 0) then
         HTABLE(L) = LFREEN
         go to 450
      end if
      K = HTABLE(L)
      if (K .lt. 0) then
c                    Need to move entry in current hash table location.
         L2 = L
   60    L1 = L2
         L2 = abs(HTABLE(L2)) / KBGLO
         if (L2 .ne. L) go to 60
         HTABLE(L) = LFREEN
      else
c                  Check if in chain for this hash table entry.
   80    K1 = mod(K, KBGLO)
         if (NAME(K1) .eq. HNAME) go to 500
         L1 = L
         L = K / KBGLO
         if (L .ne. 0) then
            K = -HTABLE(L)
            if (K .gt. 0) go to 80
         else
            L = L1
         end if
c Want to insert new entry into the chain.
         K = -LFREEN - KBGLO * L
      end if
c K has old entry to save
c L1 => entry that used to point to L and should now point to new one.
      LOOP1 = .false.
  100 NEXTH = NEXTH + 1
      if (NEXTH .gt. MXNAME) then
         if (LOOP1) stop 'Parameter MXNAME in GLOCH1 needs larger value'
         LOOP1 = .true.
         NEXTH = 1
      end if
      if (HTABLE(NEXTH) .ne. 0) go to 100
      HTABLE(L1) = mod(HTABLE(L1),KBGLO) + sign(NEXTH,HTABLE(L1))*KBGLO
      HTABLE(NEXTH) = K
c               Put in new entry
  450 if (LFREEN .eq. 0) stop
     1   'Larger value needed for MXNAME in GLOCH1 (several places)'
      NAME(LFREEN) = HNAME
      LNAME = max(LNAME, LFREEN)
      IH = -LFREEN
      LFREEN = NAMTYP(LFREEN)
      return
c               Found an entry
  500 IH = K1
      return
c              End of HASH
      end

      subroutine INIT
c             Get everything initialized.
c Common and parameters required in both GLOCH1 and GLOCH2
      integer MXGLOB, MXLREF, MXNAME, MXTYST, MXTLEN
      parameter (MXGLOB=2020, MXLREF=25000, MXNAME=2333, MXTYST=200,
     1   MXTLEN=256)
c    Misc. Parameters
      integer LONG, MXFILE
      parameter (LONG=7, MXFILE=1500)
c  Parameters used for character mappings
      integer MAPLC, MAPUC, MAPDIG, MAPDOT, MAPUPM, MAPOTH, MAPSTR,
     1  MAPEXC, MAPDIV, MAPEQ, MAPRP, MAPLP, MAPCOM, MAPCOL
      parameter (MAPLC=1, MAPUC=2, MAPDIG=3, MAPDOT=4, MAPUPM=5,
     1 MAPOTH=6, MAPSTR=7, MAPEXC=8, MAPDIV=9, MAPEQ=10,
     2 MAPLP = 11, MAPRP=12, MAPCOM=13, MAPCOL=14)
c Parameters associated with bits in symbol table entries.
      integer KBDEC, KBREF, KBDEF, KBARG, KBDUM, KBCOM, KBPAR, KBDIM,
     1   KBEXT, KBGLO, KBDIMW
      parameter (KBDEC=16, KBREF=32, KBDEF=64, KBARG=128, KBDUM=256,
     1  KBCOM=512, KBPAR=1024, KBDIM=2048, KBEXT=32768, KBGLO=65536)
      parameter (KBDIMW = KBEXT / KBDIM)
c
      character CTYPA*(MXTLEN), FILNAM(MXFILE)*8, GLTYPC*56,
     1   NAME(MXNAME)*8, TYPSTR(MXTYST)*(MXTLEN)
      integer KGLENT(MXGLOB), KGLFRO(MXGLOB), KGLNXT(MXGLOB),
     1   KGLTO(MXGLOB), LENT(MXFILE), LFILE,
     2   LGLOB, LNAME, LREF(MXLREF), LREFL, LTYPST, MAP(128)
      common /GLCHRB/ CTYPA, FILNAM, GLTYPC, NAME, TYPSTR
      common /GLINTB/ KGLENT, KGLFRO, KGLNXT, KGLTO,
     1   LENT, LFILE, LGLOB, LNAME, LREF, LREFL,
     2   LTYPST, MAP

c Common and parameters required only in GLOCH1
c  Parameters used for actions.
      integer KAETYP, KAITYP, KAFARG, KAREF, KABOTH, KADEF, KAWRIT,
     1   KACOM, KAAARG, KAPARM, KAPUSH, KAPOP, KAUPTY, KAKSTA, KAENDT,
     2   KAEXPR, KATYPE, KASTMT, KALINE, KAEND, KAEOF
      parameter (KAETYP=1, KAITYP=2, KAFARG=3, KAREF=4, KABOTH=5,
     1   KADEF=6, KAWRIT=7, KACOM=8, KAAARG=9, KAPARM=10, KAPUSH=11,
     2   KAPOP =12, KAUPTY=13, KAKSTA=14, KAENDT=15, KAEXPR=16,
     3   KATYPE=17, KASTMT=18, KALINE=19, KAEND=20, KAEOF=21)
c    Negative of the KAxxxx parameters.
      integer NAETYP, NAITYP, NAFARG, NAREF, NABOTH, NADEF, NAWRIT,
     1   NACOM, NAAARG, NAPARM, NAPUSH, NAPOP, NAUPTY, NAKSTA, NAENDT,
     2   NAEXPR, NATYPE, NASTMT, NALINE, NAEND, NAEOF
      parameter (NAETYP=-KAETYP, NAITYP=-KAITYP, NAFARG=-KAFARG,
     1   NAREF=-KAREF, NABOTH=-KABOTH, NADEF=-KADEF, NAWRIT=-KAWRIT,
     2   NACOM=-KACOM, NAAARG=-KAAARG, NAPARM=-KAPARM, NAPUSH=-KAPUSH,
     3   NAPOP =-KAPOP, NAUPTY=-KAUPTY, NAKSTA=-KAKSTA, NAEXPR=-KAEXPR,
     4   NAENDT=-KAENDT, NATYPE=-KATYPE, NASTMT=-KASTMT, NALINE=-KALINE,
     5   NAEND=-KAEND, NAEOF=-KAEOF)
c  Parameters used for types.
      integer KTYUND, KTYLOG, KTYCHR, KTYINT, KTYSP, KTYDP, KTYCOM,
     1   KTYSUB, KTYEXT
      parameter (KTYUND=0, KTYLOG=1, KTYCHR=2, KTYINT=3, KTYSP=4,
     1   KTYDP=5, KTYCOM=6, KTYSUB=7, KTYEXT=8)
c  Parameters used to track the state of processing.
      integer KSSTRT, KSNONE, KSTYPE, KSDIM, KSCOMM, KSDIMD, KSPARM,
     1   KSDATA, KSSTUF, KSBOTH, KSLEFT, KSWRIT, KSAREF, KSHEAD, KSXREF,
     2   KSDCOM, KSENTR, KSSUBR, KSFUNC
      parameter (KSSTRT=-1, KSNONE=0, KSTYPE=1, KSDIM=2, KSCOMM=3,
     1   KSDIMD=4,  KSPARM=5, KSDATA=6, KSSTUF=7, KSBOTH=8, KSLEFT=9,
     2   KSWRIT=10, KSAREF=11, KSHEAD=12, KSXREF=13, KSDCOM=14,
     3   KSENTR=15, KSSUBR=16, KSFUNC=17)
c  Parameters used for extended types.
      integer NTSCAL, NTFUNC, NTAELT, NTEXPR, NTSUBS, NTARR
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=32,
     1   NTARR=40)
c  Parameter for input line length.
      integer LENLN2, MSAVEN, MSAVGL
      parameter (LENLN2 = 74, MSAVEN=2000, MSAVGL=800)
c
c  Common block
      character BUF1*(LENLN2), BUF2*64, CURFIL*8
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, IMPTYP(26), IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN(MSAVEN), KSAVGL(MSAVGL), KSTA, KTOK(66), KXINFO, LASGLO,
     2   LETDIF, LETIDX, LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL,
     3   LTOK, MENTRY, NAMTYP(MXNAME), NEWTYP, NTYP
      common /GLCHR1/ BUF1, BUF2, CURFIL
      common /GLINT1/ IH, IMPTYP, IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN, KSAVGL, KSTA, KTOK, KXINFO, LASGLO, LETDIF, LETIDX,
     2   LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL, LTOK, MENTRY,
     3   NAMTYP, NEWTYP, NTYP, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)
c
c   Locals
      character INTRF(85)*6, TMPNAM*8
      integer I, INTRTY(85)
c
c  Fortran intrinsic functions
      integer INTRL, INTRIR, INTRC, INTRI, INTRR, INTRD, INTRZ, INTRG,
     1   INTRRR, INTRDR, INTRZR
      parameter (INTRL=14*KBDIM+KBEXT+KBDEC+KTYLOG+8,
     1   INTRIR=15*KBDIM+KBEXT+KBDEC+KTYINT+8)
      parameter (INTRC=INTRL+1, INTRI=INTRL+2, INTRR=INTRL+3,
     1   INTRD=INTRL+4, INTRZ=INTRL+5, INTRG=INTRL+6)
      parameter (INTRRR=INTRIR+1, INTRDR=INTRIR+2, INTRZR=INTRIR+3)
      data INTRF / 'INT   ', 'IFIX  ', 'IDINT ', 'REAL  ', 'FLOAT ',
     1   'SNGL  ', 'DBLE  ', 'CMPLX ', 'ICHAR ', 'CHAR  ', 'AINT  ',
     2   'DINT  ', 'ANINT ', 'DNINT ', 'NINT  ', 'IDNINT', 'ABS   ',
     3   'IABS  ', 'DABS  ', 'CABS  ', 'MOD   ', 'AMOD  ', 'DMOD  ',
     4   'SIGN  ', 'ISIGN ', 'DSIGN ', 'DIM   ', 'IDIM  ', 'DDIM  ',
     5   'DPROD ', 'MAX   ', 'MAX0  ', 'AMAX1 ', 'DMAX1 ', 'AMAX0 ',
     6   'MAX1  ', 'MIN   ', 'MIN0  ', 'AMIN1 ', 'DMIN1 ', 'AMIN0 ',
     7   'MIN1  ', 'LEN   ', 'INDEX ', 'AIMAG ', 'CONJG ', 'SQRT  ',
     8   'DSQRT ', 'CSQRT ', 'EXP   ', 'DEXP  ', 'CEXP  ', 'LOG   ',
     9   'ALOG  ', 'DLOG  ', 'CLOG  ', 'LOG10 ', 'ALOG10', 'DLOG10',
     A   'SIN   ', 'DSIN  ', 'CSIN  ', 'COS   ', 'DCOS  ', 'CCOS  ',
     B   'TAN   ', 'DTAN  ', 'ASIN  ', 'DASIN ', 'ACOS  ', 'DACOS ',
     C   'ATAN  ', 'DATAN ', 'ATAN2 ', 'DATAN2', 'SINH  ', 'DSINH ',
     D   'COSH  ', 'DCOSH ', 'TANH  ', 'DTANH ', 'LGE   ', 'LGT   ',
     E   'LLE   ', 'LLT   ' /
      data INTRTY /   INTRI,   INTRIR,   INTRIR,    INTRR,   INTRRR,
     1     INTRRR,    INTRD,    INTRZ,    INTRI,    INTRC,    INTRG,
     2     INTRDR,    INTRG,   INTRDR,    INTRI,   INTRIR,    INTRG,
     3     INTRIR,   INTRDR,   INTRZR,    INTRG,   INTRRR,   INTRDR,
     4      INTRG,   INTRIR,   INTRDR,    INTRG,   INTRIR,   INTRDR,
     5      INTRD,    INTRG,   INTRIR,   INTRRR,   INTRDR,    INTRR,
     6      INTRI,    INTRG,   INTRIR,   INTRRR,   INTRDR,    INTRR,
     7      INTRI,    INTRI,    INTRI,    INTRR,    INTRZ,    INTRG,
     8     INTRDR,   INTRZR,    INTRG,   INTRDR,   INTRZR,    INTRG,
     9     INTRRR,   INTRDR,   INTRZR,    INTRG,   INTRRR,   INTRDR,
     A      INTRG,   INTRDR,   INTRZR,    INTRG,   INTRDR,   INTRZR,
     B      INTRG,   INTRDR,    INTRG,   INTRDR,    INTRG,   INTRDR,
     C      INTRG,   INTRDR,    INTRG,   INTRDR,    INTRG,   INTRDR,
     D      INTRG,   INTRDR,    INTRG,   INTRDR,    INTRL,    INTRL,
     E      INTRL,    INTRL /
c
c ****************** Start of Executable Code **************************
c
c   Open the names file.
      open (7, FILE='glnames', STATUS='OLD')
c        Set up the mapping table.
      LETIDX = ichar('A') - 1
      LETDIF = ichar('a') - LETIDX - 1
      do 150 I = 1, 128
         MAP(I) = MAPOTH
  150 continue
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
      MAP(ichar('=')) = MAPEQ
      MAP(ichar(')')) = MAPRP
      MAP(ichar('(')) = MAPLP
      MAP(ichar('+')) = MAPUPM
      MAP(ichar(',')) = MAPCOM
      MAP(ichar('-')) = MAPUPM
      MAP(ichar('/')) = MAPDIV
      MAP(ichar(''''))= MAPSTR
      MAP(ichar(':')) = MAPCOL
c             Next two lines different due to bug in Lahey F77l compiler.
      I = ichar('!')
      MAP(I) = MAPEXC

c           Initialize the hash table.
      IH = 0
      call HASH ('        ')
c           Get initial intrinsics, etc.
c           Initialize free space pointers for the symbol table.
      LFREEN = 1
      do 230 I = 2, MXNAME
         NAMTYP(I-1) = I
  230 continue
      NAMTYP(MXNAME) = 0
c           Enter the Fortran intrinsic functions.
      do 250 I = 1, 85
         TMPNAM = INTRF(I)
         call HASH(TMPNAM)
         IH = -IH
         NAMTYP(IH) = INTRTY(IH)
  250 continue
      BUF1 = ' '
      LERRL = .true.
      LERRI = .false.
      LERRP = .false.
      LERRF = .false.
      IOENT = 0
c Start at 2 since first two locations might be used in output phase.
      LREFL = 2
      LTYPST = 0
      LFILE = 0
      LGLOB = 0
      LNAME = 0
      KXINFO = 0
      MENTRY = 0
c                      1         2         3         4         5
c             12345678901234567890123456789012345678901234567890123456
      GLTYPC='mlkjMLKJvutsVUTSihgfIHGFrqpoRQPOdcbaDCBAzyxwZYXWe123456E'
c
      open (8, FILE='gldiagl', STATUS='UNKNOWN')
      open (10, FILE='gltemp', STATUS='UNKNOWN', FORM='UNFORMATTED')
      return
c        End of subroutine INIT
      end

      subroutine LOCREF
c          Find a reference (if any) that matches, else make a new one.
c
c Common and parameters required in both GLOCH1 and GLOCH2
      integer MXGLOB, MXLREF, MXNAME, MXTYST, MXTLEN
      parameter (MXGLOB=2020, MXLREF=25000, MXNAME=2333, MXTYST=200,
     1   MXTLEN=256)
c    Misc. Parameters
      integer LONG, MXFILE
      parameter (LONG=7, MXFILE=1500)
c  Parameters used for character mappings
      integer MAPLC, MAPUC, MAPDIG, MAPDOT, MAPUPM, MAPOTH, MAPSTR,
     1  MAPEXC, MAPDIV, MAPEQ, MAPRP, MAPLP, MAPCOM, MAPCOL
      parameter (MAPLC=1, MAPUC=2, MAPDIG=3, MAPDOT=4, MAPUPM=5,
     1 MAPOTH=6, MAPSTR=7, MAPEXC=8, MAPDIV=9, MAPEQ=10,
     2 MAPLP = 11, MAPRP=12, MAPCOM=13, MAPCOL=14)
c Parameters associated with bits in symbol table entries.
      integer KBDEC, KBREF, KBDEF, KBARG, KBDUM, KBCOM, KBPAR, KBDIM,
     1   KBEXT, KBGLO, KBDIMW
      parameter (KBDEC=16, KBREF=32, KBDEF=64, KBARG=128, KBDUM=256,
     1  KBCOM=512, KBPAR=1024, KBDIM=2048, KBEXT=32768, KBGLO=65536)
      parameter (KBDIMW = KBEXT / KBDIM)
c
      character CTYPA*(MXTLEN), FILNAM(MXFILE)*8, GLTYPC*56,
     1   NAME(MXNAME)*8, TYPSTR(MXTYST)*(MXTLEN)
      integer KGLENT(MXGLOB), KGLFRO(MXGLOB), KGLNXT(MXGLOB),
     1   KGLTO(MXGLOB), LENT(MXFILE), LFILE,
     2   LGLOB, LNAME, LREF(MXLREF), LREFL, LTYPST, MAP(128)
      common /GLCHRB/ CTYPA, FILNAM, GLTYPC, NAME, TYPSTR
      common /GLINTB/ KGLENT, KGLFRO, KGLNXT, KGLTO,
     1   LENT, LFILE, LGLOB, LNAME, LREF, LREFL,
     2   LTYPST, MAP

c Common and parameters required only in GLOCH1
c  Parameters used for actions.
      integer KAETYP, KAITYP, KAFARG, KAREF, KABOTH, KADEF, KAWRIT,
     1   KACOM, KAAARG, KAPARM, KAPUSH, KAPOP, KAUPTY, KAKSTA, KAENDT,
     2   KAEXPR, KATYPE, KASTMT, KALINE, KAEND, KAEOF
      parameter (KAETYP=1, KAITYP=2, KAFARG=3, KAREF=4, KABOTH=5,
     1   KADEF=6, KAWRIT=7, KACOM=8, KAAARG=9, KAPARM=10, KAPUSH=11,
     2   KAPOP =12, KAUPTY=13, KAKSTA=14, KAENDT=15, KAEXPR=16,
     3   KATYPE=17, KASTMT=18, KALINE=19, KAEND=20, KAEOF=21)
c    Negative of the KAxxxx parameters.
      integer NAETYP, NAITYP, NAFARG, NAREF, NABOTH, NADEF, NAWRIT,
     1   NACOM, NAAARG, NAPARM, NAPUSH, NAPOP, NAUPTY, NAKSTA, NAENDT,
     2   NAEXPR, NATYPE, NASTMT, NALINE, NAEND, NAEOF
      parameter (NAETYP=-KAETYP, NAITYP=-KAITYP, NAFARG=-KAFARG,
     1   NAREF=-KAREF, NABOTH=-KABOTH, NADEF=-KADEF, NAWRIT=-KAWRIT,
     2   NACOM=-KACOM, NAAARG=-KAAARG, NAPARM=-KAPARM, NAPUSH=-KAPUSH,
     3   NAPOP =-KAPOP, NAUPTY=-KAUPTY, NAKSTA=-KAKSTA, NAEXPR=-KAEXPR,
     4   NAENDT=-KAENDT, NATYPE=-KATYPE, NASTMT=-KASTMT, NALINE=-KALINE,
     5   NAEND=-KAEND, NAEOF=-KAEOF)
c  Parameters used for types.
      integer KTYUND, KTYLOG, KTYCHR, KTYINT, KTYSP, KTYDP, KTYCOM,
     1   KTYSUB, KTYEXT
      parameter (KTYUND=0, KTYLOG=1, KTYCHR=2, KTYINT=3, KTYSP=4,
     1   KTYDP=5, KTYCOM=6, KTYSUB=7, KTYEXT=8)
c  Parameters used to track the state of processing.
      integer KSSTRT, KSNONE, KSTYPE, KSDIM, KSCOMM, KSDIMD, KSPARM,
     1   KSDATA, KSSTUF, KSBOTH, KSLEFT, KSWRIT, KSAREF, KSHEAD, KSXREF,
     2   KSDCOM, KSENTR, KSSUBR, KSFUNC
      parameter (KSSTRT=-1, KSNONE=0, KSTYPE=1, KSDIM=2, KSCOMM=3,
     1   KSDIMD=4,  KSPARM=5, KSDATA=6, KSSTUF=7, KSBOTH=8, KSLEFT=9,
     2   KSWRIT=10, KSAREF=11, KSHEAD=12, KSXREF=13, KSDCOM=14,
     3   KSENTR=15, KSSUBR=16, KSFUNC=17)
c  Parameters used for extended types.
      integer NTSCAL, NTFUNC, NTAELT, NTEXPR, NTSUBS, NTARR
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=32,
     1   NTARR=40)
c  Parameter for input line length.
      integer LENLN2, MSAVEN, MSAVGL
      parameter (LENLN2 = 74, MSAVEN=2000, MSAVGL=800)
c
c  Common block
      character BUF1*(LENLN2), BUF2*64, CURFIL*8
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, IMPTYP(26), IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN(MSAVEN), KSAVGL(MSAVGL), KSTA, KTOK(66), KXINFO, LASGLO,
     2   LETDIF, LETIDX, LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL,
     3   LTOK, MENTRY, NAMTYP(MXNAME), NEWTYP, NTYP
      common /GLCHR1/ BUF1, BUF2, CURFIL
      common /GLINT1/ IH, IMPTYP, IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN, KSAVGL, KSTA, KTOK, KXINFO, LASGLO, LETDIF, LETIDX,
     2   LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL, LTOK, MENTRY,
     3   NAMTYP, NEWTYP, NTYP, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)
c
c Locals
      character CTYPT*(MXTLEN)
      integer KIN
      integer I1, I2, I3, I4, I5, I6, J, K1, K2, K3, K4, KPT
c
c************************ Start of Executable Code *********************
c
      KGREF = NTYP/KBGLO
      I1 = KSAVGL(KGREF)
      if (mod(I1/KBEXT, 2) .eq. 0) then
         if (I1 .ge. KBGLO) then
c                           Preserve information for an intrinsic.
            KSAVGL(KGREF) = mod(I1, KBEXT)
         else
            KSAVGL(KGREF) = 0
         end if
         if (KSTA .eq. 0) then
c                        Set up to put statement function at the end.
            K3 = LGLOB
            K4 = LREFL
            LREFL = LREFLX - 6
            if (LREFL .lt. K4) LREFL = MXLREF
            LGLOBX = LGLOBX - 1
            if (LGLOBX .le. LGLOB) LGLOBX = MXGLOB
            LGLOB = LGLOBX
            call GLOENT
            LGLOB = K3
            LREFLX = LREFL - 4
            KIN = 0
            KPT = LREFL - 2
            KGREF = LGLOB
            go to 60
         else
            call GLOENT
         end if
      end if
      KGREF = KSAVGL(KGREF) / KBGLO
      KIN = KGLTO(KGREF)
      KPT = abs(KIN)
      if (KIN .lt. 0) then
   10    if (LREF(KPT-3) .eq. MENTRY) then
            K1 = KPT
            KPT = LREF(KPT-4)
            if (KPT .ne. 0) go to 10
c         Make up an entry for the next occurrence of the common block.
            LREFL = LREFL + 5
            KPT = LREFL
            if (LREFL .gt. LREFLX) stop
     1         'GLOCH1 -- Need to increase parameter MXLREF'
            LREF(KPT) = 0
            LREF(KPT-1) = 0
            LREF(KPT-2) = 0
            LREF(K1-4) = KPT
         end if
         LREF(KPT-3) = MENTRY
      else if (KGREF .eq. MENTRY) then
c            Must be a function subprogram setting the value.
         return
      end if
      if (LREF(KPT-1) .eq. 0) go to 60
c                          Check for compatibility
   20 K2 = LREF(KPT-1)
      K1 = K2 / 1024
      K2 = K2 - 1024 * K1
      K3 = 1
      CTYPT(1:KOUNT+1) = CTYPA(1:KOUNT+1)
   30 if (CTYPT(K3:K3) .ne. TYPSTR(K1)(K2:K2)) then
         I1 = index(GLTYPC, CTYPT(K3:K3)) - 1
         I2 = index(GLTYPC, TYPSTR(K1)(K2:K2)) - 1
         I3 = min(I1, I2)
         I4 = max(I1, I2)
         I5 = mod(I3, 8)
         I6 = mod(I4, 8)
         if (I1/8 .ne. I2/8) then
            if (I4 .ne. 48) go to 40
            if (I5 .ne. 7) go to 40
            I4 = I3
         else
            if (KIN .lt. 0) then
c                        Check for a common block
               if ((I6 .ge. 4) .and. (I5 .lt. 4)) go to 40
               if (mod(I3, 4) .ne. 0) I4 = I4 - mod(I4, 4) + 3
            else
c                        Check calling sequences.
               if (I6 .eq. 7) go to 40
               if (I6 .eq. 5) then
                  if (I5 .ne. 4) go to 40
               else if (I6 .eq. 4) then
                  I4 = I4 - 4 + I5
               else if (I6 + I5 .ne. 1) then
                  go to 40
               end if
            end if
         end if
         CTYPT(K3:K3) = GLTYPC(I4+1:I4+1)
      else if (CTYPT(K3:K3) .eq. ' ') then
c                                  Found a match
         if (CTYPT(1:KOUNT) .ne. TYPSTR(K1)(K2-KOUNT:K2-1)) then
c                     Change pointer to slightly different string.
            CTYPA(1:KOUNT+1) = CTYPT(1:KOUNT+1)
            if (KOUNT .ge. LONG) then
               TYPSTR(K1)(K2-KOUNT:K2-1) = CTYPT(1:KOUNT)
            else
               call LOCSTR (CTYPA(1:KOUNT+1), KPT-1)
            end if
         end if
c# Comment out line below to get every one of identical external refs.
         if (LREF(LREF(KPT)-1) .eq. LASGLO) return
c                       Add another entry of same characteristics.
         go to 70
      end if
      K2 = K2 + 1
      K3 = K3 + 1
      go to 30
c                   Check out alternate type if any.
   40 if (LREF(KPT-2) .ne. 0) then
         KPT = LREF(KPT-2)
         go to 20
      end if
c                   Make up an alternate type entry.
   50 CTYPA(1:KOUNT+1) = CTYPT(1:KOUNT+1)
      if (KPT .ge. LREFLX) then
         KIN = 0
         K4 = LREFL
         LREFL = LREFLX - 2
         LREFLX = LREFLX - 5
      else
         LREFL = LREFL + 3
         if (LREFL .gt. LREFLX) stop
     1      'GLOCH1 -- Need to increase parameter MXLREF'
      end if
      LREF(KPT-2) = LREFL
      KPT = LREFL
      LREF(KPT-2) = 0
      LREF(KPT) = 0
   60 call LOCSTR (CTYPA(1:KOUNT+1), KPT-1)
c                    Add to "to" list.
   70 J = LREF(KPT)
      if (KPT .ge. LREFLX) then
         if (KIN .eq. 0) then
            if (KSTA .eq. 0) KPT = KPT + 2
            LREFL = LREFL + 2
         else
            K4 = LREFL
            LREFL = LREFLX
            LREFLX = LREFL - 2
            KIN = 0
         end if
      else
         LREFL = LREFL + 2
         if (LREFL .gt. LREFLX) stop
     1      'GLOCH1 -- Need to increase parameter MXLREF'
      end if
      LREF(KPT) = LREFL
      LREF(LREFL) = J
      LREF(LREFL-1) = LASGLO
  100 if (KIN .le. 0) then
c                  Common block or statement function (KIN = 0).
         if (KIN .eq. 0) LREFL = K4
         return
      end if
c             For the "from" list must use first of multiple entries.
      K1 = MENTRY
      J = KGLFRO(K1)
  110 if (J .ne. 0) then
         if (LREF(J-1) .eq. KGREF) return
         J = LREF(J)
         go to 110
      end if
      LREFL = LREFL + 2
      if (LREFL .gt. MXLREF) stop
     1    'GLOCH1 -- Need to increase parameter MXLREF'
      LREF(LREFL) = KGLFRO(K1)
      KGLFRO(K1) = LREFL
      LREF(LREFL-1) = KGREF
      return
c         End of subroutine LOCREF
      end

      subroutine LOCSTR (CTYP, LS)
c Enter a new type string definition
      character CTYP*(*)
      integer LS
c
c Common and parameters required in both GLOCH1 and GLOCH2
      integer MXGLOB, MXLREF, MXNAME, MXTYST, MXTLEN
      parameter (MXGLOB=2020, MXLREF=25000, MXNAME=2333, MXTYST=200,
     1   MXTLEN=256)
c    Misc. Parameters
      integer LONG, MXFILE
      parameter (LONG=7, MXFILE=1500)
c
      character CTYPA*(MXTLEN), FILNAM(MXFILE)*8, GLTYPC*56,
     1   NAME(MXNAME)*8, TYPSTR(MXTYST)*(MXTLEN)
      integer KGLENT(MXGLOB), KGLFRO(MXGLOB), KGLNXT(MXGLOB),
     1   KGLTO(MXGLOB), LENT(MXFILE), LFILE,
     2   LGLOB, LNAME, LREF(MXLREF), LREFL, LTYPST, MAP(128)
      common /GLCHRB/ CTYPA, FILNAM, GLTYPC, NAME, TYPSTR
      common /GLINTB/ KGLENT, KGLFRO, KGLNXT, KGLTO,
     1   LENT, LFILE, LGLOB, LNAME, LREF, LREFL,
     2   LTYPST, MAP
c
c   Locals
      integer J, K, K1, L
c
c************************ Start of Executable Code *********************
c
      if (LTYPST .ne. 0) then
         L = len(CTYP)
         if (L .lt. LONG) then
            do 70 J = 1, LTYPST
               K = index(TYPSTR(J), CTYP)
               if (K .ne. 0) then
                  K1 = K - LONG + L
                  if (K1 .le. 0) go to 90
                  if (index(TYPSTR(J)(K1:K), ' ') .ne. 0) go to 90
               end if
   70       continue
         end if
         K = index(TYPSTR(LTYPST), '  ')
         if (K .ne. 0) then
            K = K + 1
            if (K + L .le. 257) go to 80
         end if
      end if
      LTYPST = LTYPST + 1
      if (LTYPST .gt. MXTYST) stop
     1   'GLOCH1 -- Parameter MXTYST must be bigger.'
      TYPSTR(LTYPST) = ' '
      K = 1
   80 J = LTYPST
      TYPSTR(LTYPST)(K:K+LEN(CTYP)-1) = CTYP
   90 LREF(LS) = 1024 * J + K
      return
c           End of subroutine LOCSTR
      end

      subroutine PACK
c Reads input into BUF1, sets BUF2 to characters needed for symbol table
c lookups (all letters upper case), and KTOK to information about the
c tokens.
c
c Common and parameters required in both GLOCH1 and GLOCH2
      integer MXGLOB, MXLREF, MXNAME, MXTYST, MXTLEN
      parameter (MXGLOB=2020, MXLREF=25000, MXNAME=2333, MXTYST=200,
     1   MXTLEN=256)
c    Misc. Parameters
      integer LONG, MXFILE
      parameter (LONG=7, MXFILE=1500)
c  Parameters used for character mappings
      integer MAPLC, MAPUC, MAPDIG, MAPDOT, MAPUPM, MAPOTH, MAPSTR,
     1  MAPEXC, MAPDIV, MAPEQ, MAPRP, MAPLP, MAPCOM, MAPCOL
      parameter (MAPLC=1, MAPUC=2, MAPDIG=3, MAPDOT=4, MAPUPM=5,
     1 MAPOTH=6, MAPSTR=7, MAPEXC=8, MAPDIV=9, MAPEQ=10,
     2 MAPLP = 11, MAPRP=12, MAPCOM=13, MAPCOL=14)
c Parameters associated with bits in symbol table entries.
      integer KBDEC, KBREF, KBDEF, KBARG, KBDUM, KBCOM, KBPAR, KBDIM,
     1   KBEXT, KBGLO, KBDIMW
      parameter (KBDEC=16, KBREF=32, KBDEF=64, KBARG=128, KBDUM=256,
     1  KBCOM=512, KBPAR=1024, KBDIM=2048, KBEXT=32768, KBGLO=65536)
      parameter (KBDIMW = KBEXT / KBDIM)
c
      character CTYPA*(MXTLEN), FILNAM(MXFILE)*8, GLTYPC*56,
     1   NAME(MXNAME)*8, TYPSTR(MXTYST)*(MXTLEN)
      integer KGLENT(MXGLOB), KGLFRO(MXGLOB), KGLNXT(MXGLOB),
     1   KGLTO(MXGLOB), LENT(MXFILE), LFILE,
     2   LGLOB, LNAME, LREF(MXLREF), LREFL, LTYPST, MAP(128)
      common /GLCHRB/ CTYPA, FILNAM, GLTYPC, NAME, TYPSTR
      common /GLINTB/ KGLENT, KGLFRO, KGLNXT, KGLTO,
     1   LENT, LFILE, LGLOB, LNAME, LREF, LREFL,
     2   LTYPST, MAP

c Common and parameters required only in GLOCH1
c  Parameters used for actions.
      integer KAETYP, KAITYP, KAFARG, KAREF, KABOTH, KADEF, KAWRIT,
     1   KACOM, KAAARG, KAPARM, KAPUSH, KAPOP, KAUPTY, KAKSTA, KAENDT,
     2   KAEXPR, KATYPE, KASTMT, KALINE, KAEND, KAEOF
      parameter (KAETYP=1, KAITYP=2, KAFARG=3, KAREF=4, KABOTH=5,
     1   KADEF=6, KAWRIT=7, KACOM=8, KAAARG=9, KAPARM=10, KAPUSH=11,
     2   KAPOP =12, KAUPTY=13, KAKSTA=14, KAENDT=15, KAEXPR=16,
     3   KATYPE=17, KASTMT=18, KALINE=19, KAEND=20, KAEOF=21)
c    Negative of the KAxxxx parameters.
      integer NAETYP, NAITYP, NAFARG, NAREF, NABOTH, NADEF, NAWRIT,
     1   NACOM, NAAARG, NAPARM, NAPUSH, NAPOP, NAUPTY, NAKSTA, NAENDT,
     2   NAEXPR, NATYPE, NASTMT, NALINE, NAEND, NAEOF
      parameter (NAETYP=-KAETYP, NAITYP=-KAITYP, NAFARG=-KAFARG,
     1   NAREF=-KAREF, NABOTH=-KABOTH, NADEF=-KADEF, NAWRIT=-KAWRIT,
     2   NACOM=-KACOM, NAAARG=-KAAARG, NAPARM=-KAPARM, NAPUSH=-KAPUSH,
     3   NAPOP =-KAPOP, NAUPTY=-KAUPTY, NAKSTA=-KAKSTA, NAEXPR=-KAEXPR,
     4   NAENDT=-KAENDT, NATYPE=-KATYPE, NASTMT=-KASTMT, NALINE=-KALINE,
     5   NAEND=-KAEND, NAEOF=-KAEOF)
c  Parameters used for types.
      integer KTYUND, KTYLOG, KTYCHR, KTYINT, KTYSP, KTYDP, KTYCOM,
     1   KTYSUB, KTYEXT
      parameter (KTYUND=0, KTYLOG=1, KTYCHR=2, KTYINT=3, KTYSP=4,
     1   KTYDP=5, KTYCOM=6, KTYSUB=7, KTYEXT=8)
c  Parameters used to track the state of processing.
      integer KSSTRT, KSNONE, KSTYPE, KSDIM, KSCOMM, KSDIMD, KSPARM,
     1   KSDATA, KSSTUF, KSBOTH, KSLEFT, KSWRIT, KSAREF, KSHEAD, KSXREF,
     2   KSDCOM, KSENTR, KSSUBR, KSFUNC
      parameter (KSSTRT=-1, KSNONE=0, KSTYPE=1, KSDIM=2, KSCOMM=3,
     1   KSDIMD=4,  KSPARM=5, KSDATA=6, KSSTUF=7, KSBOTH=8, KSLEFT=9,
     2   KSWRIT=10, KSAREF=11, KSHEAD=12, KSXREF=13, KSDCOM=14,
     3   KSENTR=15, KSSUBR=16, KSFUNC=17)
c  Parameters used for extended types.
      integer NTSCAL, NTFUNC, NTAELT, NTEXPR, NTSUBS, NTARR
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=32,
     1   NTARR=40)
c  Parameter for input line length.
      integer LENLN2, MSAVEN, MSAVGL
      parameter (LENLN2 = 74, MSAVEN=2000, MSAVGL=800)
c
c  Common block
      character BUF1*(LENLN2), BUF2*64, CURFIL*8
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, IMPTYP(26), IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN(MSAVEN), KSAVGL(MSAVGL), KSTA, KTOK(66), KXINFO, LASGLO,
     2   LETDIF, LETIDX, LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL,
     3   LTOK, MENTRY, NAMTYP(MXNAME), NEWTYP, NTYP
      common /GLCHR1/ BUF1, BUF2, CURFIL
      common /GLINT1/ IH, IMPTYP, IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN, KSAVGL, KSTA, KTOK, KXINFO, LASGLO, LETDIF, LETIDX,
     2   LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL, LTOK, MENTRY,
     3   NAMTYP, NEWTYP, NTYP, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)
c
c   Locals
c    Parameters used to track state in processing tokens.
      integer LSSTMT, LSVARN, LSDOT, LSEXP, LSEXP2, LSINT, LSLOG,
     1  LSNEW, LSIMPL, LSCLIT, LSNEXT
      parameter (LSSTMT=1, LSVARN=2, LSDOT=3, LSEXP=4, LSEXP2=5,
     1   LSINT=6, LSLOG=7, LSNEW=8, LSIMPL=9, LSCLIT=10, LSNEXT=11)
c    Parameters used for state on parentheses (in NPAREN).
      integer NPNONE, NPPOP, NPSUBS, NPIF, NPPAR, NPKEYP, NPREAD, NPWRIT
      parameter (NPNONE=0, NPPOP=1, NPSUBS=2, NPIF=3, NPPAR=4,
     1   NPKEYP=5, NPREAD=6, NPWRIT=7)
c
      character C, CLAST, CTEMP*64, DEFIO*114, FORKEY*106
      integer I, I1, I2, I3, IMPL1, ISTA, J, K, KB, KEYIDX, KPAR1,
     1    KPAREN, KSTAKY(11:20), L1, LIMPTY, LBUF2, LKTOK, LKTOK1,
     2    LSTATE, LT, NCOMM, NEWOP, NEWTOK, NPAREN(0:30)
      integer LOCKEY(42)
      save CLAST, KPAR1, KPAREN, LSTATE, NCOMM, NEWOP, NEWTOK, NPAREN,
     1   ISTA
c
c LOCKEY The low order three digits indicate the index in FORKEY to find
c   the letters after the first four of the keyword.  The next one in
c   the list can be used to get the number of characters after the
c   fourth of the keyword.  The high order digit specifies how the
c   keyword must end as follows.
c    1 no letters follow the keyword
c    2 must have a "(" following
c    3 else or else if
c    4 May be followed by 'FUNCTION', variable name, or "(" (IMPLICIT).
c    5 Letters must follow
c    6 Used for implicit statement
c    7 Special logic for I/O statements.
c    8 Check for an "=" to avoid calling SAVE a keyword.
c    9 Check for "," or "/" to verfify a data statement
c  Data for LOCKEY, and FORKEY are derived from the following list.
c       1   4001   LOGI-CAL
c       2   4004   CHAR-ACTER
c       3   4009   INTE-GER
c       4   4012   REAL
c       5   4012   DOUB-LEPRECISION
c       6   4023   COMP-LEX
c       7   4026   SUBR-OUTINE
c       8   5032   EXTE-RNAL
c       9   4036   INTR-INSIC
c      10   5041   CALL
c      11   2041   PARA-METER
c      12   5046   DIME-NSION
c      13   1051   COMM-ON
c      14   2053   EQUI-VALENCE
c      15   9060   DATA
c      16   5060   ENTR-Y
c      17   5061   FUNC-TION
c      18     65   BLOC-KDATA
c      19     70   PROG-RAM
c      20   6073   IMPL-ICIT
c      21     77   ASSI-GN
c      22   2079   BACK-SPACE
c      23     84   ENDF-ILE
c      24     87   GOTO
c      25   7087   CLOS-E
c      26   7088   INQU-IRE
c      27   7091   OPEN
c      28   7091   PRIN-T
c      29   7092   READ
c      30     92   REWI-ND
c      31   7094   WRIT-E
c      32   1095   THEN
c      33   1095   CONT-INUE
c      34   3099   ELSE/ELSEIF
c      35   1099   ENDI-F
c      36    100   RETU-RN
c      37    102   PAUS-E
c      38    103   STOP
c      39   8103   SAVE
c      40    103   FORM-AT
c      41   1105   ENDD-O
c            106
c
      data LOCKEY / 4001, 4004, 4009, 4012, 4012, 4023, 4026, 5032,
     1  4036, 5041, 2041, 5046, 1051, 2053, 9060, 5060, 5061,   65,
     1    70, 6073,   77, 2079,   84,   87, 7087, 7088, 7091, 7091,
     3  7092,   92, 7094, 1095, 1095, 3099, 1099,  100,  102,  103,
     4  8103,  103, 1105, 107 /
      data FORKEY /'CALACTERGERLEPRECISIONLEXOUTINERNALINSICMETERNSIONON
     1VALENCEYTIONKDATARAMICITGNSPACEILEEIRETNDEINUEFRNEATDO' /
c                      11     12      13      14      15      16
      data KSTAKY /KSPARM, KSDIM, KSDCOM, KSBOTH, KSDATA, KSENTR,
     1   KSFUNC, KSSUBR, KSSUBR, -1/
      data DEFIO /'UNIT=ACCESS=FORM=RECL=BLANK=IOSTAT=EXIST=OPENED=NUMBE
     1R=NAMED=NAME=SEQUENTIAL,DIRECT=FORMATTED=UNFORMMATED=NEXTREC=' /


c
c ********************** Start of Executable Code **********************
c
c                                 Get the next line of input.
   10 LBUF2 = 0
   20 KB = 7
      LINE = LINE + 1
      read (9, '(A)', END=700) BUF1(1:LENLN2-2)
      if (BUF1(1:1) .ne. ' ') then
c        The "%" below is for lines used by the converter to C.
         J = index('Cc*!%', BUF1(1:1))
         if (J .ne. 0) then
            if (J .eq. 4) then
               if (LERRF) call TXTOUT (' ',
     1            ' The above line contains a Fortran 90 comment.')
            end if
            go to 20
         end if
      end if
      LTOK = 1
c# One could do something for tabs here if one knew whether the tab stop
c  was at column 6 or column 7.
      if ((BUF1(6:6) .ne. ' ') .and. (BUF1(6:6) .ne. '0')) then
         if (ITOK .gt. 0) KTOK(1) = KTOK(ITOK)
         ITOK = 1
         if (KSTA .eq. KSSTRT) then
c    Error -- shouldn't have continuation line as start of a statement.
            if (LERRL) call TXTOUT(' ',
     1         ' The above statement starts badly and is ignored.')
            go to 20
         else if (LSTATE .eq. LSCLIT) then
c              Take care of unfinished character literal
            KB = index(BUF1(7:), '''')
            if (KB .eq. 0) go to 20
            KB = KB + 6
            I1 = 0
         end if
         if (LSTATE .eq. LSNEXT) go to 10
         if (LSTATE .eq. LSSTMT) go to 30
      else
         if (BUF1(1:6) .eq. ' ') then
            if (BUF1(7:72) .eq. ' ') go to 20
            KSTA = KSNONE
         else
            KSTA = KSNONE
            if (BUF1(7:72) .eq. ' ') go to 20
         end if
         KPAREN = 0
         NPAREN(0) = NPNONE
         LSTATE = 1
         NEWTOK = KTYUND
         ITOK = 0
         KPAR1 = 0
         KTOK(1) = NASTMT
         NEWOP = 0
      end if
      LKTOK = 1
      LKTOK1 = LKTOK
      LT = 0
c
c      do I = LENLN2, 2, -1
c        if (BUF1(I:I) .ne. ' ') go to 25
c      end do
c 25   continue
c      print '(I4,'': '', A)', LINE, BUF1(1:I)
c

   30 do 400 I = KB, LENLN2
         C = BUF1(I:I)
         if (C .eq. ' ') then
            if (BUF1(I+1:I+1) .ne. ' ') go to 400
            if (BUF1(I+1:LENLN2) .eq. ' ') then
               K = MAPEXC
               go to 60
            end if
            do 50 J = I+2, LENLN2
               if (BUF1(J:J) .ne. ' ') then
                  KB = J
                  go to 30
               end if
   50       continue
            stop 'GLOCH1 -- Fell through loop 50 in PACK'
         end if
         J = ichar(C)
         K = MAP(J)
c                1   2   3   4   5   6   7   8  9  10
   60    go to (70,100,140,150,155,160,170,180,175,225), LSTATE
c Looking for keyword
   70    if (K .gt. MAPUC) then
c                      Keyword checking
            if (LBUF2 .ge. LT+4) go to 450
            if (LBUF2 .lt. LT+2) then
               if (K .eq. MAPDIG) then
                  if (LBUF2 .gt. LT) go to 90
c                               Assume digit after arithmetic if.
                  if (KSTA .eq. KSSTUF) go to 180
               end if
               if (LBUF2 .gt. LT) go to 90
               if (LBUF2 .gt. 0) then
                  if (K .eq. MAPEXC) go to 20
                  go to 190
               end if
c                            Error -- Bad start of statement
               if (LERRL) call TXTOUT(' ',
     1            ' The above statement starts badly and is ignored.')
               LSTATE = LSNEXT
               go to 10
            end if
            if (BUF2(LT+1:LBUF2) .eq. 'IF') then
               if (K .eq. MAPLP) then
c          Set so inside "()" of an "IF" is processed correctly.
                  LSTATE = LSNEW
                  LBUF2 = LT
                  KPAREN = KPAREN + 1
                  NPAREN(KPAREN) = NPIF
                  KSTA = KSSTUF
                  go to 350
               end if
            else if (K .eq. MAPEXC) then
               if (BUF2(1:LBUF2) .eq. 'END') then
                  LKTOK = 1
                  KTOK(1) = NAEND
                  KSTA = KSSTRT
                  go to 410
               end if
            end if
   90       ISTA = KSTA
            go to 520
         end if
c Working on a variable or keyword
  100    if (K .le. MAPDIG) go to 130
         LSTATE = LSNEW
         if ((KPAREN .gt. 0) .and. (C .eq. '=')) then
            if (NPAREN(KPAREN) .gt. NPPAR) then
c                 NAME= ignored inside "()" if not parameter.
c             Set state to assign for next token for some cases.
               CTEMP = BUF2(LT+1:LBUF2) // '='
               J = index(DEFIO, CTEMP(1:LBUF2-LT+1))
               if (J .ne. 0) then
                  I1 = KSLEFT
                  if (J .le. 5) then
                     if (J.eq.1) then
                        if (NPAREN(KPAREN).ne.NPWRIT) go to 110
                        I1 = KSWRIT
                     else if (KEYIDX .ne. 27) then
c                Only get the "assign to" for an inquire statement.
                        go to 110
                     end if
                  end if
                  LKTOK = LKTOK + 2
                  KTOK(LKTOK-1) = NAKSTA
                  KTOK(LKTOK) = I1
                  NPAREN(KPAREN) = NPAREN(KPAREN) + 100
               end if
  110          LBUF2 = LT
               go to 350
            else if (NPAREN(KPAREN) .eq. NPNONE) then
c                  Set to flag variable has both referenced and defined.
               LKTOK = LKTOK + 5
               KTOK(LKTOK-4) = NAKSTA
               KTOK(LKTOK-3) = KSBOTH
               KTOK(LKTOK-2) = LBUF2
               KTOK(LKTOK-1) = NAKSTA
               KTOK(LKTOK) = KSSTUF
               LT = LBUF2
               go to 190
            end if
         else if (LBUF2-LT .gt. 8) then
c Special logic to catch character*(???) function xxx
            if (KEYIDX .eq. 2) then
               if (BUF2(LT+1:LT+8) .eq. 'FUNCTION') then
                  CTEMP(1:LBUF2-LT-8) = BUF2(LT+9:LBUF2)
                  LBUF2 = LBUF2 - 8
                  BUF2(LT+1:LBUF2) = CTEMP(1:LBUF2-LT)
                  KSTA = KSFUNC
                  NEWTYP = 10
               end if
            end if
         end if
         LKTOK = LKTOK + 1
         KTOK(LKTOK) = LBUF2
         LT = LBUF2
         if (C .ne. '(') go to 190
  120    KPAREN = KPAREN + 1
         NPAREN(KPAREN) = NPPOP
         LKTOK = LKTOK + 1
         KTOK(LKTOK) = NAPUSH
         KPAR1 = KPAR1 + 1
         go to 350
  130    if (K .eq. MAPLC) C = char(J - LETDIF)
         LBUF2 = LBUF2 + 1
         BUF2(LBUF2:LBUF2) = C
         go to 380
c After an operand "." (maybe logical or floating)
  140    if (K .le. MAPDIG) then
            if (K .le. MAPUC) then
               LSTATE = LSEXP
               if ((C .eq. 'D') .or. (C .eq. 'd')) NEWOP = KTYDP
               go to 380
            end if
            go to 380
         end if
         LSTATE = LSNEW
         go to 180
c After an exponent letter
  150    LSTATE = LSEXP2
         if ((K .eq. MAPDIG) .or. (K .eq. MAPUPM)) go to 380
         if (K .le. MAPUC) then
            LSTATE = LSLOG
            go to 380
         end if
         LSTATE = LSNEW
         go to 180
c After into the exponent
  155    if (K .eq. MAPDIG) go to 380
         LSTATE = LSNEW
         go to 180
c Working on integer
  160    if (K .eq. MAPDIG) go to 380
         if (K .eq. MAPDOT) then
            LSTATE = LSDOT
            NEWOP = KTYSP
            go to 380
         end if
         if ((KPAREN .eq. 0) .and. (KEYIDX .eq. KTYCHR)) then
            if (K .ge. MAPDIG) go to 190
            NEWTOK = 0
            LSTATE = LSVARN
            go to 130
         end if
         if (K .eq. MAPLC) C = char(J - LETDIF)
         if ((C .eq. 'E') .or. (C .eq. 'D')) then
            NEWOP = KTYSP
            go to 140
         else if (C .eq. 'T') then
c                           Assume part of assign GO TO.
            KB = I
  164       KB = KB + 1
            if (BUF1(KB:KB) .eq. ' ') go to 164
            if ((BUF1(KB:KB) .eq. 'O') .or. (BUF1(KB:KB) .eq. 'o')) then
               KB = KB + 1
               KSTA = KSLEFT
               LSTATE = LSNEW
               go to 30
            end if
         end if
         NEWOP = KTYINT
         LSTATE = LSNEW
         go to 180
c Working on logical
  170    if (K .le. MAPUC) go to 380
         if (K .eq. MAPDOT) then
            NEWOP = KTYLOG
            LSTATE = LSNEW
            go to 380
         end if
         call TXTOUT(' ',
     1      ' Confusion on above line in examining a logical variable.')
         go to 350
c Working on implicit
  175    if (K .le. MAPUC) then
            if (K .eq. MAPLC)  then
               J = J - LETDIF
               C = char(J)
            end if
            BUF2(1:1) = C
            if (IMPL1 .eq. 0) IMPL1 = J - LETIDX
         else if ((C .eq. ',') .or. (C .eq. ')')) then
            do 176 J = IMPL1, ichar(BUF2(1:1)) - LETIDX
               IMPTYP(J) = LIMPTY
  176       continue
            IMPL1 = 0
            if (C .eq. ')') go to 10
         end if
         go to 380
c Looking for something to start.
  180    if (NEWOP .ne. 0) then
            if (NEWTOK .eq. 0) then
               NEWTOK = NEWOP
            else
               I1 = mod(NEWTOK, 8)
               NEWTOK = NEWTOK - I1
               I2 = mod(NEWOP, 8)
               NEWOP = NEWOP - I2
               I3 = max(NEWTOK, NEWOP, NTEXPR)
               if ((I1 .eq. KTYLOG) .or. (I2 .eq. KTYLOG)) then
                  NEWTOK = KTYLOG + I3
               else
                  NEWTOK = max(I1, I2) + I3
               end if
            end if
            NEWOP = 0
         end if
         if (K .le. MAPDIG) then
            if (K .eq. MAPDIG) then
               NEWOP = KTYINT
               LSTATE = LSINT
               go to 380
            end if
            if ((NEWTOK .gt. 0) .and. (KPAR1 .gt. 0)) then
               LKTOK = LKTOK + 1
               KTOK(LKTOK) = NAEXPR
               LKTOK = LKTOK + 1
               if (NEWTOK .lt. NTEXPR) NEWTOK = NEWTOK + NTEXPR
               KTOK(LKTOK) = NEWTOK
            end if
            NEWTOK = 0
            LSTATE = LSVARN
            go to 130
         end if
c After an operand  (Should this be a separate case??
c                 . +/-*etc   '   !   /   =   (   )   ,    :
  190    go to (200,330,330,210,410,230,240,250,260,260, 290), K-MAPDIG

c Got a "."
  200    LSTATE = LSDOT
         go to 380
c Got a "'"
  210    LSTATE = LSCLIT
         KB = I
  220    I1 = index(BUF1(KB+1:), '''')
         if (I1 .eq. 0) then
            LSTATE = LSCLIT
            go to 410
         end if
  225    KB = KB + I1 + 1
         if (BUF1(KB:KB) .eq. '''') go to 220
         NEWOP = KTYCHR
         NEWTOK = NTEXPR
         LSTATE = LSNEW
         go to 30
  230    if (KSTA .ne. KSDCOM) go to 340
c Got a "/" in a common statement.
         if (KPAREN .ne. 0) go to 350
         if (NCOMM .ne. 0) then
            if ((NCOMM .eq. KSCOMM) .and. (CLAST .eq. '/')) then
c                     Use ' ' for the name in blank common.
               LBUF2 = LBUF2 + 1
               BUF2(LBUF2:LBUF2) = '-'
               LKTOK = LKTOK + 1
               KTOK(LKTOK) = LBUF2
            end if
            LKTOK = LKTOK + 2
            KTOK(LKTOK-1) = NAKSTA
            KTOK(LKTOK) = NCOMM
         end if
         if (NCOMM .eq. KSCOMM) then
            NCOMM = KSDCOM
         else
            NCOMM = KSCOMM
         end if
         go to 350
c Got an "="
  240    LKTOK = LKTOK + 2
         KTOK(LKTOK-1) = NAKSTA
         KTOK(LKTOK) = KSSTUF
         go to 350
c Got a "("
  250    if ((KTOK(LKTOK).eq.NAPOP) .or. (KTOK(LKTOK).eq.NAENDT)) then
            if (CLAST .ne. ',') then
c                   Special treatment for x()(), and x()*()
               LKTOK = LKTOK + 1
               KTOK(LKTOK) = NAENDT
               go to 120
            end if
         end if
         KPAREN = KPAREN + 1
         NPAREN(KPAREN) = NPNONE
         go to 350
c Got a "," or a ")"

  260    if (KPAR1 .gt. 0) then
            if (NEWTOK .gt. 0) then
               LKTOK = LKTOK + 2
               KTOK(LKTOK-1) = NAEXPR
               if (NEWTOK .lt. NTEXPR) NEWTOK = NEWTOK + NTEXPR
               KTOK(LKTOK) = NEWTOK
            end if
         end if
         if (NPAREN(KPAREN) .ge. NPPOP) then
            if (NPAREN(KPAREN) .eq. NPPAR) then
               LKTOK = LKTOK + 2
               KTOK(LKTOK-1) = NAKSTA
               KTOK(LKTOK) = KSPARM
               if (C .ne. ')') go to 350
            else if (NPAREN(KPAREN) .eq. NPPOP) then
               if ((KPAREN .gt. 0) .and. (C .eq. ',')) then
                  LKTOK = LKTOK + 1
                  KTOK(LKTOK) = NAUPTY
               end if
            else if (NPAREN(KPAREN) .ge. NPREAD) then
               if (NPAREN(KPAREN) .ge. 100) then
                  NPAREN(KPAREN) = mod(NPAREN(KPAREN), 100)
                  LKTOK = LKTOK + 2
                  KTOK(LKTOK-1) = NAKSTA
                  KTOK(LKTOK) = KSSTUF
               end if
               if (C .eq. ')') then
                  if (NPAREN(KPAREN) .eq. NPREAD) then
                     LKTOK = LKTOK + 2
                     KTOK(LKTOK-1) = NAKSTA
                     KTOK(LKTOK) = KSLEFT
                  end if
               end if
               if (KPAREN .eq. 0) NPAREN(KPAREN) = NPNONE
            end if
         end if
         if (C .eq. ')') then
            if (KPAREN .eq. 0) then
c                     Error message for unbalanced parentheses
               if (LERRL) call TXTOUT(' ',
     1            ' The above statement has unbalanced parentheses.')
            else
               if (NPAREN(KPAREN) .eq. NPPOP) then
c                            Final ")" for a reference
                  LKTOK = LKTOK + 1
                  KTOK(LKTOK) = NAPOP
                  KPAR1 = KPAR1 - 1
               else if (NPAREN(KPAREN) .eq. NPIF) then
c                                            Processing an IF
                  LSTATE = LSSTMT
                  KSTA = KSSTUF
               end if
               KPAREN = KPAREN - 1
            end if
         else
            if (KSTA .eq. KSLEFT) then
               if (BUF2(1:2) .eq. 'DO') then
                  if (MAP(ichar(BUF2(3:3))) .eq. MAPDIG) then
c                          Get positioned at the first variable name.
                     if (C .eq. ',') then
                        if (LKTOK .eq. 2) then
c                         On "," get into initial state without the ",".
                           LSTATE = LSVARN
                           LKTOK = 1
                           LT = 0
                           go to 400
                        end if
                     end if
                     LTOK = 3
  280                LTOK = LTOK + 1
                     if (MAP(ichar(BUF2(LTOK:LTOK))) .ne. MAPUC)
     1                   go to 280
                     BUF2(1:2) = '  '
                     KSTA = KSBOTH
                  end if
               end if
            end if
         end if
         go to 350

c Got a ":"
  290    if (NPAREN(KPAREN) .eq. NPPOP) then
            I1 = 0
            do 300 K = LKTOK, 1, -1
               if (KTOK(K) .eq. NAPUSH) then
                  I1 = I1 - 1
                  if (I1 .lt. 0) go to 310
               else if (KTOK(K) .eq. NAPOP) then
                  I1 = I1 + 1
               end if
               KTOK(K+2) = KTOK(K)
  300       continue
  310       KTOK(K+1) = NAEXPR
            KTOK(K+2) = NTSUBS
            LKTOK = LKTOK + 2
         end if
         go to 380
c                     Flag that we have an expression.
  330    if (C .eq. '*') then
            if (KSTA .le. KSDIMD) then
               if ((KTOK(LKTOK) .eq. NAPUSH) .or.
     1            (KTOK(LKTOK).eq.NAUPTY)) then
                  NEWOP = KTYINT
                  go to 380
               else if (KSTA .eq. KSTYPE) then
                  if (KPAREN .eq. 0) then
                     LKTOK = LKTOK + 1
                     KTOK(LKTOK) = NAENDT
                  end if
               end if
            end if
         end if
  340    if (NPAREN(KPAREN) .eq. NPPOP) NEWOP = NTEXPR
         go to 380
c
  350    NEWTOK = KTYUND
  380    CLAST = C
  400 continue
c
  410 if (C .eq. '!') then
         if (LERRF) call TXTOUT (' ',
     1      ' The above line contains a Fortran 90 comment.')
      end if
      if (KSTA .ne. KSNONE) then
         KTOK(LKTOK+ 1) = NALINE
         if (IOENT .gt. 0) then
            if ((KSTA .ge. KSENTR) .or. (KSTA .eq. KSHEAD)) then
c                        Print the line as a header line.
               do 420 K = 72, 2, -1
                  if (BUF1(K:K) .ne. ' ') go to 430
  420          continue
  430          write(IOENT, '(A)') BUF1(1:K)
            end if
         end if
         return
      end if
      I = LENLN2 + 1
c
  450 ISTA = KSTA
c  For KEYIDX =  1 -  6, type specifications
c             =  7 - 20, special state or actions
c             = 18 - 31, process just to get references
c             = 32 - 38, Done there should no more on the line.
c             > 39       Done, ignore the rest of the statement.
c                        1    2    3    4    5    6    7    8    9   10
c  1    11   12   13   14   15   16   17   18   19   20   21   22   23
c  2   24   25   26   27   28   29   30   31   32   33   34   35   36
c  3  37   38   39   40   41
      KEYIDX =(index('LOGI,CHAR,INTE,REAL,DOUB,COMP,SUBR,EXTE,INTR,CALL,
     1PARA,DIME,COMM,EQUI,DATA,ENTR,FUNC,BLOC,PROG,IMPL,ASSI,BACK,ENDF,G
     2OTO,CLOS,INQU,OPEN,PRIN,READ,REWI,WRIT,THEN,CONT,ELSE,ENDI,RETU,PA
     3US,STOP,SAVE,FORM,ENDD', BUF2(LT+1:LT+4)) + 4) / 5
c
      if (KEYIDX .eq. 0) go to 520
      I1 = LOCKEY(KEYIDX)
      I3 = I1 / 1000
      I1 = I1 - 1000 * I3
      I2 = mod(LOCKEY(KEYIDX+1), 1000)
      J = I2 - I1 + LT + 4
      if (I3 .gt. 0) then
c                    Keyword must end in specific way
         if (I3 .le. 3) then
            if (I3 .eq. 3) then
c                      Either ELSE (then done) or ELSEIF
               if ((LBUF2 .eq. 4) .and. (C .eq. ' ')) go to 10
               if (BUF2(LT+5:LBUF2) .ne. 'IF') go to 520
               if (C .ne. '(') go to 520
c          Set so inside "()" of an "ELSE IF" is processed correctly.
               LSTATE = LSNEW
               LBUF2 = LT
               KPAREN = KPAREN + 1
               NPAREN(KPAREN) = NPIF
               KSTA = KSSTUF
               I = I + 1
               go to 530
            end if
c                        No letters allowed after keyword
            if (J .gt. LBUF2) go to 520
c                        If I3=2, must have following "("
            if ((I3 .ge. 2) .and. (C .ne. '(')) go to 520
         else if (I3 .eq. 7) then
c                        Special for I/O statements
            if ((C .ne. '(') .and. (KEYIDX .eq. 31)) go to 520
         else if (I3 .eq. 8) then
            if (index(BUF1(I:), '=') .ne. 0) go to 520
         else if (I3 .eq. 9) then
c            Special checks  when start with "DATA"
            if (K .ne. MAPEXC) then
               if (LBUF2 .eq. 4) go to 520
               if ((C .ne. ',') .and. (C .ne. '/')) then
                  if (C .eq. '=') go to 520
                  L1 = index(BUF1, 'DATA')
                  if (L1 .eq. 0) L1 = index(BUF1, 'data')
                  if (BUF1(L1+4:L1+4) .ne. ' ') go to 520
               end if
            end if
         else
c                        Must have following letters
            if ((J .eq. LBUF2) .and. (K .ne. MAPEXC)) then
               if ((I3 .ne. 6) .and. (LSTATE .ne. LSIMPL)) then
                  if (KEYIDX .ne. 2) go to 520
                  if (C .ne. '*') go to 520
                  go to 480
               end if
c                        Looks like implicit statement
               if (C .ne. '(') go to 520
               if (I3 .ne. 6) then
c                         Just got the keyword.
                  LIMPTY = KEYIDX
                  LBUF2 = 0
                  IMPL1 = 0
                  KB = I + 1
                  go to 30
               end if
            end if
            if (I3 .eq. 4) then
               if (LBUF2 .gt. J+8) then
c                                        Flag function header
                  if (BUF2(J+1:J+8) .eq. 'FUNCTION') I3 = -1
               end if
            else if (I3 .eq. 6) then
               if (BUF2(1:LBUF2) .eq. 'IMPLICITNONE') then
                  if (LERRF) call TXTOUT (' ',
     1               ' The above line is Fortran 90 statement.')
                  go to 10
               end if
            end if
         end if
      end if
  480 if (J .gt. LT + 4) then
         if (BUF2(LT+5:J) .ne. FORKEY(I1:I2-1)) go to 520
      end if
      if (J .lt. LBUF2) then
c                   There are more
         if (I3 .lt. 0) J = J + 8
         if (LKTOK .eq. LKTOK1) then
c                When at start of line, save all.
            LT = J
            LTOK = J + 1
         else
            CTEMP(1:LBUF2-J) = BUF2(J+1:LBUF2)
            LBUF2 = LBUF2 + LT - J
            BUF2(LT+1:LBUF2) = CTEMP(1:LBUF2-LT)
         end if
         LSTATE = LSVARN
      else
         if (LKTOK .eq. LKTOK1) then
            LT = LBUF2
            LTOK = LBUF2 + 1
         else
            LBUF2 = LT
         end if
         LSTATE = LSNEW
      end if
      if ((I3 .lt. 0) .and. (KEYIDX .gt. 6)) go to 520
      if (KEYIDX .le. 20) then
         if (KEYIDX .le. 10) then
            NEWTYP = KEYIDX
            if (KEYIDX .eq. 10) NEWTYP = 7
            KSTA = KSTYPE
            if (I3 .lt. 0) then
               KSTA = KSFUNC
               NEWTYP = NEWTYP + 8
            else if (KEYIDX .eq. 7) then
               KSTA = KSSUBR
            end if
            go to 530
         else
            KSTA = KSTAKY(KEYIDX)
            if (KSTA .eq. -1) then
c                       Process IMPLICIT statement
                LSTATE = LSIMPL
                go to 450
            else if (KSTA .eq. KSPARM) then
               I = I + 1
               KPAREN = KPAREN + 1
               NPAREN(KPAREN) = NPPAR
            end if
c             NEWTYP only has an effect if type is explicitly set later.
            NEWTYP = 8
            NCOMM = 0
         end if
      else
         if (I3 .eq. 2) then
c              Set special paren state for keyword that require paren.
            KPAREN = KPAREN + 1
            NPAREN(KPAREN) = NPKEYP
            I = I + 1
         end if
         if (KEYIDX .le. 31) then
            if (I3 .eq. 7) then
c                               Special check for I/O statements.
               if (C .eq. '(') then
                  I = I + 1
                  KPAREN = KPAREN + 1
                  if (KEYIDX .eq. 31) then
                     NPAREN(KPAREN) = NPWRIT + 100
                     KSTA = KSWRIT
                     go to 530
                  end if
               end if
               NPAREN(KPAREN) = NPREAD
               KSTA = KSSTUF
            else
c                               Process just to get variable references.
               if (KSTA .eq. KSNONE) then
                  KSTA = KSSTUF
               end if
            end if
         else
            if (KEYIDX .le. 35) then
               if (BUF1(I:) .ne. ' ') then
                  if (C .ne. '!') go to 520
                  C = ' '
                  if (LERRF) call TXTOUT (' ',
     1               ' The above line contains a Fortran 90 comment.')
               end if
            else if (KEYIDX .ge. 41) then
               if (LERRF) call TXTOUT (' ',
     1            ' The above line contains Fortran 90 code.')
               go to 10
            end if
            LSTATE = LSNEXT
            if (KSTA .eq. KSNONE) go to 10
            go to 410
         end if
      end if
      go to 530
c                 No keyword, assume an assignment statement
  520 KSTA = KSLEFT
      LSTATE = LSVARN
  530 if (ISTA .ne. KSNONE) then
c                                 Assume we had an "if" statement.
         LKTOK = LKTOK + 2
         KTOK(LKTOK-1) = NAKSTA
         KTOK(LKTOK) = KSTA
         KSTA = KSSTUF
      end if
      if (LBUF2 .eq. LT) LSTATE = LSNEW
  550 if (I .gt. LENLN2) go to 410
      KB = I
      go to 30
c                 End of a file.
  700 KTOK(1) = NAEOF
      ITOK = 0
      return
c                     End of subroutine PACK
      end

      subroutine TXTOUT(ERRNAM, ERRTXT)
c                   Prints out text
      character ERRNAM*(*), ERRTXT*(*)
c
c ******************** Variable Declarations ***************************
c
c Common and parameters required in both GLOCH1 and GLOCH2
      integer MXGLOB, MXLREF, MXNAME, MXTYST, MXTLEN
      parameter (MXGLOB=2020, MXLREF=25000, MXNAME=2333, MXTYST=200,
     1   MXTLEN=256)
c    Misc. Parameters
      integer LONG, MXFILE
      parameter (LONG=7, MXFILE=1500)
c  Parameters used for character mappings
      integer MAPLC, MAPUC, MAPDIG, MAPDOT, MAPUPM, MAPOTH, MAPSTR,
     1  MAPEXC, MAPDIV, MAPEQ, MAPRP, MAPLP, MAPCOM, MAPCOL
      parameter (MAPLC=1, MAPUC=2, MAPDIG=3, MAPDOT=4, MAPUPM=5,
     1 MAPOTH=6, MAPSTR=7, MAPEXC=8, MAPDIV=9, MAPEQ=10,
     2 MAPLP = 11, MAPRP=12, MAPCOM=13, MAPCOL=14)
c Parameters associated with bits in symbol table entries.
      integer KBDEC, KBREF, KBDEF, KBARG, KBDUM, KBCOM, KBPAR, KBDIM,
     1   KBEXT, KBGLO, KBDIMW
      parameter (KBDEC=16, KBREF=32, KBDEF=64, KBARG=128, KBDUM=256,
     1  KBCOM=512, KBPAR=1024, KBDIM=2048, KBEXT=32768, KBGLO=65536)
      parameter (KBDIMW = KBEXT / KBDIM)
c
      character CTYPA*(MXTLEN), FILNAM(MXFILE)*8, GLTYPC*56,
     1   NAME(MXNAME)*8, TYPSTR(MXTYST)*(MXTLEN)
      integer KGLENT(MXGLOB), KGLFRO(MXGLOB), KGLNXT(MXGLOB),
     1   KGLTO(MXGLOB), LENT(MXFILE), LFILE,
     2   LGLOB, LNAME, LREF(MXLREF), LREFL, LTYPST, MAP(128)
      common /GLCHRB/ CTYPA, FILNAM, GLTYPC, NAME, TYPSTR
      common /GLINTB/ KGLENT, KGLFRO, KGLNXT, KGLTO,
     1   LENT, LFILE, LGLOB, LNAME, LREF, LREFL,
     2   LTYPST, MAP

c Common and parameters required only in GLOCH1
c  Parameters used for actions.
      integer KAETYP, KAITYP, KAFARG, KAREF, KABOTH, KADEF, KAWRIT,
     1   KACOM, KAAARG, KAPARM, KAPUSH, KAPOP, KAUPTY, KAKSTA, KAENDT,
     2   KAEXPR, KATYPE, KASTMT, KALINE, KAEND, KAEOF
      parameter (KAETYP=1, KAITYP=2, KAFARG=3, KAREF=4, KABOTH=5,
     1   KADEF=6, KAWRIT=7, KACOM=8, KAAARG=9, KAPARM=10, KAPUSH=11,
     2   KAPOP =12, KAUPTY=13, KAKSTA=14, KAENDT=15, KAEXPR=16,
     3   KATYPE=17, KASTMT=18, KALINE=19, KAEND=20, KAEOF=21)
c    Negative of the KAxxxx parameters.
      integer NAETYP, NAITYP, NAFARG, NAREF, NABOTH, NADEF, NAWRIT,
     1   NACOM, NAAARG, NAPARM, NAPUSH, NAPOP, NAUPTY, NAKSTA, NAENDT,
     2   NAEXPR, NATYPE, NASTMT, NALINE, NAEND, NAEOF
      parameter (NAETYP=-KAETYP, NAITYP=-KAITYP, NAFARG=-KAFARG,
     1   NAREF=-KAREF, NABOTH=-KABOTH, NADEF=-KADEF, NAWRIT=-KAWRIT,
     2   NACOM=-KACOM, NAAARG=-KAAARG, NAPARM=-KAPARM, NAPUSH=-KAPUSH,
     3   NAPOP =-KAPOP, NAUPTY=-KAUPTY, NAKSTA=-KAKSTA, NAEXPR=-KAEXPR,
     4   NAENDT=-KAENDT, NATYPE=-KATYPE, NASTMT=-KASTMT, NALINE=-KALINE,
     5   NAEND=-KAEND, NAEOF=-KAEOF)
c  Parameters used for types.
      integer KTYUND, KTYLOG, KTYCHR, KTYINT, KTYSP, KTYDP, KTYCOM,
     1   KTYSUB, KTYEXT
      parameter (KTYUND=0, KTYLOG=1, KTYCHR=2, KTYINT=3, KTYSP=4,
     1   KTYDP=5, KTYCOM=6, KTYSUB=7, KTYEXT=8)
c  Parameters used to track the state of processing.
      integer KSSTRT, KSNONE, KSTYPE, KSDIM, KSCOMM, KSDIMD, KSPARM,
     1   KSDATA, KSSTUF, KSBOTH, KSLEFT, KSWRIT, KSAREF, KSHEAD, KSXREF,
     2   KSDCOM, KSENTR, KSSUBR, KSFUNC
      parameter (KSSTRT=-1, KSNONE=0, KSTYPE=1, KSDIM=2, KSCOMM=3,
     1   KSDIMD=4,  KSPARM=5, KSDATA=6, KSSTUF=7, KSBOTH=8, KSLEFT=9,
     2   KSWRIT=10, KSAREF=11, KSHEAD=12, KSXREF=13, KSDCOM=14,
     3   KSENTR=15, KSSUBR=16, KSFUNC=17)
c  Parameters used for extended types.
      integer NTSCAL, NTFUNC, NTAELT, NTEXPR, NTSUBS, NTARR
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=32,
     1   NTARR=40)
c  Parameter for input line length.
      integer LENLN2, MSAVEN, MSAVGL
      parameter (LENLN2 = 74, MSAVEN=2000, MSAVGL=800)
c
c  Common block
      character BUF1*(LENLN2), BUF2*64, CURFIL*8
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, IMPTYP(26), IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN(MSAVEN), KSAVGL(MSAVGL), KSTA, KTOK(66), KXINFO, LASGLO,
     2   LETDIF, LETIDX, LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL,
     3   LTOK, MENTRY, NAMTYP(MXNAME), NEWTYP, NTYP
      common /GLCHR1/ BUF1, BUF2, CURFIL
      common /GLINT1/ IH, IMPTYP, IOENT, ITOK, KGREF, KOUNT,
     1   KSAVEN, KSAVGL, KSTA, KTOK, KXINFO, LASGLO, LETDIF, LETIDX,
     2   LFREEN, LGLOBX, LINE, LREFLX, LSAVEN, LSAVGL, LTOK, MENTRY,
     3   NAMTYP, NEWTYP, NTYP, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)
c
c   Locals
      integer J, K, L, LASTG, LASTL
      save LASFIL, LASTG, LASTL
      character LASFIL*8
      data LASFIL / '        ' /
      data LASTG, LASTL / 0, 0 /
c
 1000 format(/,'************  Diagnostics in file: ', A,'  Entry: ', A)
 1010 format(/,'******** Entry: ', A)
 1020 format(I5, ': ', A)
 1050 format(' Parameter MXFILE in GLOCHK must be larger.'/
     1  ' The following and all later files ignored.')

c
c ****************** Start of Executable Code **************************
c
      do 20 K = len(ERRTXT), 2, -1
         if (ERRTXT(K:K) .ne. ' ') go to 30
   20 continue
   30 if (ERRNAM(1:1) .eq. ' ') then
         if (ERRNAM .ne. ' ') then
            if (ERRNAM .eq. ' 1') then
               write (8, 1050)
            end if
            write (8, '(1X, A)') ERRTXT(1:K)
            return
         else
            J = 0
            if (K .eq. 1) then
               write(8, '(1X)')
               return
            end if
         end if
      else
         do 50 J = len(ERRNAM), 2, -1
            if (ERRNAM(J:J) .ne. ' ') go to 60
   50    continue
      end if
   60 if (CURFIL .ne. LASFIL)then
         LASTL = 0
         LASFIL = CURFIL
         if (LASGLO .ne. 0) write(8, 1000) LASFIL,
     1      NAME(KGLENT(LASGLO))
      else if (LASTG .ne. MENTRY) then
         write(8, 1010) NAME(KGLENT(MENTRY))
      end if
      LASTG = MENTRY
      if (LINE .ne. LASTL) then
         do 90 L = LENLN2, 2, -1
            if (BUF1(L:L) .ne. ' ') go to 100
   90    continue
  100    write(8, 1020) LINE, BUF1(1:L)
         LASTL = LINE
      end if
      if (J .ne. 0) then
         write (8, '(1X, 2A)') ERRNAM(1:J), ERRTXT(1:K)
      else
         write (8, '(1X, A)') ERRTXT(1:K)
      end if
      return
c                 End of TXTOUT
      end
c
      subroutine WAIT
      return
      end

