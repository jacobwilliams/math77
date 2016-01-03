      program MANGLE
c>> 2007-10-12 MANGLE  Krogh  ' in col 72 of cont. char. lit. fixed.
c>> 1999-12-22 MANGLE  Krogh  Must now be run from "mangle" dir.
c>> 1999-05-08 MANGLE  Krogh  Made /name/ in a save stmt. external.
c>> 1998-11-03 MANGLE  Krogh  Mangle code (almost?) debugged.
c>> 1998-10-15 MANGLE  Krogh  Converting from GLOCH1 to MANGLE
c## Add options to not give diagnostic for Fortran 90 code, and
c   to output a cross reference (5 or 6) pairs/line?)
c Does the initial processing for the list of files in mang
c Files used and their associated unit numbers are:
c mang    7 Input list of file names.  Full path names can be used
c            here.  The file name is remembered as that part of the line
c            from the last "/" or "\", or beginning of line to the text
c            just before the following "." or the end of line (up to a
c            maximum of 8 characters).
c mangdi  8  Output file for local diagnostics.
c ??????  9  File currently being read from.
c ?????? 10  File currently being written to.
c            the data listing all of the entries is written.
c manlin 11  Output file containing correspondence between input and
c            output line numbers.
c
c If the first character of a line in mang is a blank, it is assumed
c that the rest of the line contains letters defining options.  A upper
c case letter gives the option, a lower case one turns the option off.
c Also if the first two columns of the line are blank, the line gives
c a comment that is to appear at the start of each source file.  Any
c number of comment lines can be given.  If a comment line is given
c after a file name, it becomes a "first" comment line.  A blank line
c kills the output of comment lines.

c
c Currently there are no other options defined.
c
c One can change options at any time, although in most cases one should
c only want to set them on the first line.
c
c
c ************************ Routines  ***********************************
c
c  Main program
c  subroutine HASH (HNAME)
c  subroutine INIT
c     Initializes many variables.
c  subroutine LOCREF
c     Locates similar references (for entries and common)
c  subroutine PACKF
c     Analyzes the input lines for Fortran.
c  subroutine PACKC
c     Analyzes the input lines for C.
c  subroutine TXTOUT(ERRTXT)
c     Prints both error messages, and requested output.
c  subroutine MANGLE
c     Make up new names (as needed) and outputs names to output buffer.
c
c ******************** Variable Definitions ****************************
c
c BUF1   Contains the initial input line.
c BUF2   Contains line of upper case  names on the line.
c BUFLIN Holds output line number followed by line numbers spaced 10
c   apart by output line number of correspondng input line numbers.
c CURFIL File name being worked on.
c ERRNAM Name for an error message, blank if none.  The following names
c   have a special meaning.
c  ' 1'   Too many files in the input.
c  ' 2'   Output of type error info. for entries.
c  ' 3'   Output of type error info for types.
c ERRTXT Text for an error message.
c C      Temporary storage of a single character.
c CLAST  In PACK? the last character processed.
c I      Temporary index.
c I1,I2  Used as temporary indices.
c IH     Location of a symbol table entry (obtained from HASH).
c ISTA   Used in PACK? to save current KSTA, before keyword look up.
c J      Temporary index.
c K      Temporary index.
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
c   KAPUSH =11 Not used
c   KAPOP  =12 Not used
c   KAUPTY =13 Update the type information.
c   KAKSTA =14 Get new KSTA from next token in list.
c   KAENDT =15 Ends previous token
c   KAEXPR =16 Not really needed.
c   KATYPE =17 Not really needed
c   KASTMT =18 Start a new statement.
c   KALINE =19 End of a line of input.
c   KAEND  =20 End of a subprogram, save pertinent things.
c   KAEOF  =21 End of a file.
c KB     Used in PACK? for index in BUF1, at point of current processing.
c KEYIDX In PACK?, the index of the keyword for the statement.
c KLIN   Next column to write column number in in BUFLIN.
c KPAREN Used in PACK? to track parentheses.  0 initially, +1 for every
c   "(", -1 for every ")".  Also see NPAREN.
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
c KSxxx  See KSTA above.
c KTOK   Array giving tokens from PACKF.  Values are as follows.
c    >0  KTOK(.) give the last index in BUF2 containing characters for
c       for a symbol to be looked up.
c    <0 The negative of one of the KAxxx names defined above.  Some
c       of these tokens use the next value in KTOK to set another value.
c LASFIL Used in TXTOUT to remember if we have written already written
c   a message for the current file.
c LBUF2  Used in PACK? for the index of last character in BUF2.
c LENLN2 Parameter two bigger than the length of an imput line.
c LERREQ Equivalenced to LERRI, LERRL, LERRP, LERRF
c LERRF  If .true. Fortran 90 comments and implicit are diagnosed.
c LERRI  If .true. specific intrinsics that could be generic are flagged
c LERRL  If .true. local problems are to be diagnosed.
c LERRP  If .true. (and LERRL) unreferenced parameters are diagnosed.
c LETDIF Gives the amount to subtract from the index for a lower case
c   character to get the index for an upper case character.
c LETIDX ichar('A') -1, used to get the index of a letter..
c LFREEN NAMGEN(LFREEN) is a free (unused) symbol table entry.
c   NAMGEN(LFREEN) points to the next free one in the chain.
c LKTOK  Used in PACKF to track last location used in KTOK.
c LKTOK1 Used in PACKF to save the starting value of LKTOK.
c LNAME  Last index actually used in NAME for storing names.
c LONG   Parameter giving the length for a long interface string.
c   Strings this long or longer will have one copy stored for each use,
c   those shorter will share space, and thus must have a new copy when
c   changed.
c LSTATE Tracks the state of the processing in PACK?.  In the case of
c   PACKF:
c  LSSTMT = 1  At the start of the statement, or accumulating a keyword.
c  LSVARN = 2  Working on a variable name.
c  LSDOT  = 3  After a "." for an operand.
c  LSEXP  = 4  After an exponent letter.
c  LSEXP2 = 5  After digit or +/- following an exponent letter or do lab
c  LSINT  = 6  Working on an integer.
c  LSLOG  = 7  Working on logical
c  LSNEW  = 8  Looking for token to start.
c  LSIMPL = 9  Processing IMPLICIT
c  LSCLIT =10  In a string literal
c  LSQLIT =11  String literal that may be continued
c  LSNEXT =12  Get to start of next statement.
c LT     In PACK? for the last location in BUF2 used for a defined token.
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
c MXFILE Parameter giving the maximum number of files that can be
c   processed.
c MXNAME Parameter giving the dimension of NAME, and LENNAM.  Also used
c   for the size of the hash table, so it should be a prime.
c NUMCOM Number of comment lines to output at start of file.
c MXSTK  Maximum number of items on stack for dealing with parentheses
c   and the call tree.
c MXTLEN Parameter giving the most arguments we can handle in a given
c   argument list or common block.
c NAME   A character*MXLEN array of names, the symbol table of names.
c NCOMM  Used in PACKF to keep track of where we are in a common block.
c   (/ toggles the state and output a token to change the state.)  When
c   0, not in a common block, else has a value of KSDCOM or KSCOMM
c   depending on the state to output on the next "/".
c NEWGLO Index of first global entry that is not obtained from previous
c   processing.
c NEWOP  Used in PACKF together with NEWTOK to track types.  Set by
c   examining an operand.
c NEWTOK Used in PACKF to track the type of expressions.  If a token is
c   output that defines part of an expression, this gives the type.
c NPAREN NPAREN(KPAREN) is used in PACKF to give the action on a ")".
c    NPNONE=0  No special action.
c    NPPOP= 1  Output a "pop" token.
c    NPSUBS=2  ")" is associated with a substring specification (kk:kk).
c    NPIF=  3  ")" ends an "if" statement.
c    NPPAR= 4  ")" ends a "parameter" statement.
c    NPKEYP=5  ")" used for many keyword that are followed by a "("
c    NPREAD=6  ")" (or ",") changes state to KSLEFT.
c    NPWRIT=7  Special actions for write statement.
c    100 is added to an NPAREN value if we have set the state to flag
c    the next token as defined, and this state should be reset on the
c    next "," or ")".
c NPxxxx Parameters used for NPAREN actions, see above.
c TMPNAM Temporary character string.
c
c ********************** Specifications ********************************
c
c Common and parameters
      integer MXLEN, MXNAME, MXTYST, MXTLEN
c    If MXNAME is changed, keep it prime.  (Note it must also be defined
c    in GLOCH2.)
      parameter (MXLEN=24, MXNAME=6577, MXTYST=100,
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
c
      integer LNAME, MAP(128)
      common /GLINTB/ LNAME, MAP

c Common and parameters
c  Parameters used for types.
      integer KTYUND, KTYLOG, KTYCHR, KTYINT, KTYSP, KTYDP, KTYCOM,
     1   KTYSUB, KTYEXT
      parameter (KTYUND=0, KTYLOG=1, KTYCHR=2, KTYINT=3, KTYSP=4,
     1   KTYDP=5, KTYCOM=6, KTYSUB=7, KTYEXT=8)
c  Parameters used for extended types.
      integer NTSCAL, NTFUNC, NTAELT, NTEXPR, NTSUBS, NTARR
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=MXLEN,
     1   NTARR=40)
c
c  Common block
      integer LENLN2
      parameter (LENLN2=1000)
      character BUF1*(LENLN2), BUF2*64, BUFLIN*57, CURFIL*(MXLEN),
     1   NAME(MXNAME)*(MXLEN)
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, KLIN, KSTA,
     1   LETDIF, LETIDX, LFREEN, LINE, LINOUT, LENNAM(MXNAME)
      common /GLCHR1/ BUF1, BUF2, BUFLIN, CURFIL, NAME
      common /GLINT1/ IH, KLIN, KSTA,  LETDIF,
     1   LETIDX, LFREEN, LINE, LINOUT, LENNAM, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)
c
c   Locals
      integer MXSTK, NUMCOM
      parameter (MXSTK=30)
      integer  I, I1, I2, J, K, LENCOM(10)
      logical FORT
      character TMPNAM*(MXLEN), COMLIN(10)*(LENLN2)
c
c ********************** Start of Executable Code **********************
c
c                  Start main processing
      call INIT
      NUMCOM = 0
c               Get the next file name (or option spec.)
  100 continue
      BUF1 = ' '
      read (7, '(A)', END = 300) BUF1(1:80)
      I1 = 1
  150 if (BUF1(1:1) .eq. '!') go to 100
      if (BUF1(1:1) .eq. ' ') then
c                         Option specifications
            if (BUF1(2:2) .eq.  ' ') then
               if (NUMCOM .gt. 0) NUMCOM = 0
               do 160 K = 80, 3, -1
                  if (BUF1(K:K) .ne. ' ') go to 170
  160          continue
               go to 100
  170          NUMCOM = NUMCOM - 1
               COMLIN(-NUMCOM) = BUF1(3:K)
               LENCOM(-NUMCOM) = K - 2
            end if
c                        No other options currently defined.
         go to 100
      end if
c               Get the base name for the file.
      I2 = index(BUF1(I1:), '/')
c               char(92) is the backslash (\)
      if (I2 .eq. 0) I2 = index(BUF1(I1:), char(92))
      if (I2 .eq. 0) I2 = index(BUF1(I1:), ':')
      if (I2 .ne. 0) then
         I1 = I1 + I2
         go to 150
      end if
      K = index(BUF1, ' ') - 1
      J = index(BUF1(I1:), '.')
  180 if (J .eq. 0) then
         print '(''The file '', A,
     1     '' must have an extension starting with f,F,c, or C'')',
     2     BUF1(1:K)
         go to 100
      end if
      if (J .gt. 8) then
         print '(''MANGLE -- Filename, '', A, '' is too long.'')',
     1     BUF1(I1:I1+J-2)
         go to 100
      end if
      if (J .eq. 1) then
         if (I1 .ne. 1) print '(1X,A, A)', BUF1(1:I1+1),
     1       ' is not a valid file name.'
         go to 100
      end if
      I = index('fFcC', BUF1(I1+J:I1+J))
c              Assume Fortran if not fFc or C.
      FORT = I .le. 2

      CURFIL = BUF1(I1:I1+J-2)! The base name for the file.
      open(9, FILE=BUF1(1:K), STATUS='OLD')
      print '(1x, A)', BUF1(1:K)
c                        Output header info. for manlin file.
      write(11,'(A)') BUF1(1:K)
      KLIN = 7
      LINOUT = 0
      write (BUFLIN(1:7), '(I5, '': '')') LINOUT + 10
c
      TMPNAM = BUF1(I1+J-1:K)
      BUF1(I1:) = CURFIL(1:J-1)//TMPNAM(1:K-I1-J+2)
      open(10, FILE=BUF1(I1:K), STATUS='UNKNOWN')

      NUMCOM = abs(NUMCOM)
      do 200 LINOUT = 1, NUMCOM
         if (FORT) then
            write(10, '(''C '', A)') COMLIN(LINOUT)(1:LENCOM(LINOUT))
         else
            write(10, '(''/*'', A, ''*/'')')
     1         COMLIN(LINOUT)(1:LENCOM(LINOUT))
         end if
  200 continue
      LINOUT = NUMCOM
      LINE = 0
      if (FORT) then
c                   Initialize the symbol table.
         call MANGF(0, 0, TMPNAM, K, TMPNAM(1:1))
c               Get next line of the file and scan it.
         call PACKF
      else
c                   Clear the hash and symbol table.
         call MANGC(0,TMPNAM, K, KLIN)
         call PACKC
      end if
      if (KLIN .gt. 7) write (11, '(A)') BUFLIN(1:KLIN-5)
      close(9)
      close(10)
      go to 100
c
c  Done processing input files
  300 continue
      endfile(8)
      close(8)
      close(7)
      print 900
  900 format(/' Done with mangling files.'/)
      stop
c                   End of MANGLE
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
c    entry.)  On entry:
c   <0  to delete move some entries
c   =0  to lookup, but not insert if not found (returns 0 if not found)
c   >0  to lookup and insert if not found.
c L, L1, L2   Temporary indices.
c LOOP1 logical var. used to catch loop if hash table is full.
c
c ******************** Variable Declarations ***************************
c
c Common and parameters
      integer MXLEN, MXNAME, MXTYST, MXTLEN
      parameter (MXLEN=24, MXNAME=6577, MXTYST=100,
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
c
      integer LNAME, MAP(128)
      common /GLINTB/ LNAME, MAP

c Common and parameters
c  Parameters used for actions.
      integer KAETYP, KAITYP, KAFARG, KAREF, KABOTH, KADEF, KAWRIT,
     1   KACOM, KAAARG, KAPARM, KAPUSH, KAPOP, KAUPTY, KAKSTA, KAENDT,
     2   KAEXPR, KATYPE, KASTMT, KALINE, KAEND, KAEOF
      parameter (KAETYP=1, KAITYP=2, KAFARG=3, KAREF=4, KABOTH=5,
     1   KADEF=6, KAWRIT=7, KACOM=8, KAAARG=9, KAPARM=10, KAPUSH=11,
     2   KAPOP =12, KAUPTY=13, KAKSTA=14, KAENDT=15, KAEXPR=16,
     3   KATYPE=17, KASTMT=18, KALINE=19, KAEND=20, KAEOF=21)
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
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=MXLEN,
     1   NTARR=40)
c
c  Common block
      integer LENLN2
      parameter (LENLN2=1000)
      character BUF1*(LENLN2), BUF2*64, BUFLIN*57, CURFIL*(MXLEN),
     1   NAME(MXNAME)*(MXLEN)
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, KLIN, KSTA,
     1   LETDIF, LETIDX, LFREEN, LINE, LINOUT, LENNAM(MXNAME)
      common /GLCHR1/ BUF1, BUF2, BUFLIN, CURFIL, NAME
      common /GLINT1/ IH, KLIN, KSTA,  LETDIF,
     1   LETIDX, LFREEN, LINE, LINOUT, LENNAM, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)
c
c   Locals
      integer KBGLO
      parameter (KBGLO = 65536)

      logical LOOP1
      character HNAME*(MXLEN)
      integer K, K1, L, L1, L2, NEXTH
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
               LFREEN = 1
               HTABLE(1) = 0
               do 10 K = 2, MXNAME
                  HTABLE(K) = 0
                  LENNAM(K-1) = K
   10          continue
               LENNAM(MXNAME) = 0
            else
               stop 'MANGLE -- Unknown hash table request'
            end if
            return
         end if
      end if

c      if (IH .lt. 0) then
c                         Have a bunch of entries to delete or move.
c## Maybe we won't implement this??
c      end if
c
c                                          Do the Look up
      K1 = 0
c This was in so common names would hash to a different name space.  But
c we need to get fancier if we want to keep this, or common names and
c the same name in a save statement are treated differently.
c##      if (KSTA .eq. KSDCOM) K1 = 1
      do 20 L = 1, MXLEN-3, 4
         if (HNAME(L:L+3) .eq. '    ') go to 40
         K1 = K1 + ichar(HNAME(L:L)) * ichar(HNAME(L+1:L+1)) *
     1     ichar(HNAME(L+2:L+2)) * ichar(HNAME(L+3:L+3))
   20 continue
   40 L = abs(mod(K1, MXNAME)) + 1
      if (HTABLE(L) .eq. 0) then
         if (IH .eq. 0) return
         HTABLE(L) = LFREEN
         go to 450
      end if
      K = HTABLE(L)
      if (K .lt. 0) then
         if (IH .eq. 0) return
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
         if (IH .eq. 0) return
c Want to insert new entry into the chain.
         K = -LFREEN - KBGLO * L
      end if
c K has old entry to save
c L1 => entry that used to point to L and should now point to new one.
      LOOP1 = .false.
  100 NEXTH = NEXTH + 1
      if (NEXTH .gt. MXNAME) then
         if (LOOP1) stop 'Parameter MXNAME in MANGLE needs larger value'
         LOOP1 = .true.
         NEXTH = 1
      end if
      if (HTABLE(NEXTH) .ne. 0) go to 100
      HTABLE(L1) = mod(HTABLE(L1),KBGLO) + sign(NEXTH,HTABLE(L1))*KBGLO
      HTABLE(NEXTH) = K
c               Put in new entry
  450 if (LFREEN .eq. 0) stop
     1   'Larger value needed for MXNAME in MANGLE (several places)'
      NAME(LFREEN) = HNAME
      LNAME = max(LNAME, LFREEN)
      IH = -LFREEN
      LFREEN = LENNAM(LFREEN)
      return
c               Found an entry
  500 IH = K1
      return
c              End of HASH
      end

      subroutine INIT
c             Get everything initialized.
c Common and parameters
      integer MXLEN, MXNAME, MXTYST, MXTLEN
      parameter (MXLEN=24, MXNAME=6577, MXTYST=100,
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
c
      integer LNAME, MAP(128)
      common /GLINTB/ LNAME, MAP

c Common and parameters
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
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=MXLEN,
     1   NTARR=40)
c
c  Common block
      integer LENLN2
      parameter (LENLN2=1000)
      character BUF1*(LENLN2), BUF2*64, BUFLIN*57, CURFIL*(MXLEN),
     1   NAME(MXNAME)*(MXLEN)
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, KLIN, KSTA,
     1   LETDIF, LETIDX, LFREEN, LINE, LINOUT, LENNAM(MXNAME)
      common /GLCHR1/ BUF1, BUF2, BUFLIN, CURFIL, NAME
      common /GLINT1/ IH, KLIN, KSTA,  LETDIF,
     1   LETIDX, LFREEN, LINE, LINOUT, LENNAM, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)
c
c   Locals
      integer I
c
c ****************** Start of Executable Code **************************
c
c   Open the names file.
      open (7, FILE='mang', STATUS='OLD')
      open (11, FILE='manlin', STATUS='UNKNOWN')
      write (11, 20)
   20 format(
     1  ' First column gives output line no. rest are input line no.')
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
c
      open (8, FILE='mangdi', STATUS='UNKNOWN')
      return
c        End of subroutine INIT
      end


      subroutine PACKF
c Reads input into BUF1, sets BUF2 to characters needed for symbol table
c lookups (all letters upper case), and KTOK to information about the
c tokens.
c
c Common and parameters
      integer MXLEN, MXNAME, MXTYST, MXTLEN
      parameter (MXLEN=24, MXNAME=6577, MXTYST=100,
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
c
      integer LNAME, MAP(128)
      common /GLINTB/ LNAME, MAP

c Common and parameters
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
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=MXLEN,
     1   NTARR=40)
c
c  Common block
      integer LENLN2
      parameter (LENLN2=1000)
      character BUF1*(LENLN2), BUF2*64, BUFLIN*57, CURFIL*(MXLEN),
     1   NAME(MXNAME)*(MXLEN)
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, KLIN, KSTA,
     1   LETDIF, LETIDX, LFREEN, LINE, LINOUT, LENNAM(MXNAME)
      common /GLCHR1/ BUF1, BUF2, BUFLIN, CURFIL, NAME
      common /GLINT1/ IH, KLIN, KSTA,  LETDIF,
     1   LETIDX, LFREEN, LINE, LINOUT, LENNAM, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)
c
c   Locals
c    Parameters used to track state in processing tokens.
      integer LSSTMT, LSVARN, LSDOT, LSEXP, LSEXP2, LSINT, LSLOG,
     1  LSNEW, LSIMPL, LSCLIT, LSQLIT, LSNEXT
      parameter (LSSTMT=1, LSVARN=2, LSDOT=3, LSEXP=4, LSEXP2=5,
     1   LSINT=6, LSLOG=7, LSNEW=8, LSIMPL=9, LSCLIT=10, LSQLIT=11,
     2   LSNEXT=12)
c    Parameters used for state on parentheses (in NPAREN).
      integer NPNONE, NPPOP, NPSUBS, NPIF, NPPAR, NPKEYP, NPREAD, NPWRIT
      parameter (NPNONE=0, NPPOP=1, NPSUBS=2, NPIF=3, NPPAR=4,
     1   NPKEYP=5, NPREAD=6, NPWRIT=7)
c
      character C, CLAST, CTEMP*64, FORKEY*106, BUFOUT*80
      integer I, I1, I2, I3, INEND, ISTA, J, K, KB, KEYIDX, KOUT,
     1    KPAR1, KPAREN, KSTAKY(11:20), KTOK(66), L1, L2, L3, L4, LBUF2,
     2    LKTOK, LKTOK1,  LSTATE, LT, NCOMM, NEWOP, NEWTOK, NPAREN(0:30)
      integer LOCKEY(42)
      save CLAST, KPAR1, KPAREN, LSTATE, NCOMM, NPAREN
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

c
c ********************** Start of Executable Code **********************
c
      MAP(ichar('_')) = MAPOTH
      KOUT = 0
      KSTA = KSSTRT
c                                 Get the next line of input.
   10 LBUF2 = 0
   20 KB = 7

      if ((KOUT .eq. 6) .and. (BUF1(1:6) .eq. '      ')) KOUT = 0
      if (KOUT .ne. 0) then
         if (KOUT .gt. 72)  call TXTOUT(
     1' The above statement generates a line with > 72 characters.')

         write (10, '(A)') BUFOUT(1:KOUT)
         KOUT = 0
         LINOUT = LINOUT + 1
         if (mod(LINOUT, 10) .eq. 0) then
            write(BUFLIN(KLIN:KLIN+5), '(I5)') LINE
            KLIN = KLIN + 5
            if (KLIN .gt. 55) then
               write (11, '(A)') BUFLIN(1:KLIN-5)
               KLIN = 7
               write (BUFLIN(1:7), '(I5, '': '')') LINOUT + 10
            end if
         end if
      end if


      LINE = LINE + 1
      read (9, '(A)', END=700) BUF1(1:72)
      if (BUF1(1:1) .ne. ' ') then
c        The "%" below is for lines used by the converter to C.
         J = index('cC*!', BUF1(1:1))
c             Delete all comments.
         if (J .ne. 0) go to 20
      end if

c INEND  index in BUF1 of the end of line.
      J = 72
      if (BUF1(40:74) .eq. ' ') J = 40
      do 25 INEND = J, 1, -1
         if (BUF1(INEND:INEND) .ne. ' ') go to 27
   25 continue
c              Skip a blank line.
      go to 20
c
   27 continue

c# One could do something for tabs here if one knew whether the tab stop
c  was at column 6 or column 7.
      if ((BUF1(6:6) .ne. ' ') .and. (BUF1(6:6) .ne. '0')) then
         if (KSTA .eq. KSSTRT) then
c    Error -- shouldn't have continuation line as start of a statement.
            if (LERRL) call TXTOUT(
     1         ' The above statement starts badly and is ignored.')
            go to 20
         else if (LSTATE .eq. LSNEXT) then
            BUFOUT(1:INEND) = BUF1(1:INEND)
            KOUT = INEND
            go to 10
         else if (LSTATE .ge. LSCLIT) then
c              Take care of unfinished character literal
            if (LSTATE .eq. LSCLIT) then
               KB = index(BUF1(7:INEND), '''')
               if (KB .eq. 0) then
                  BUFOUT(1:INEND) = BUF1(1:INEND)
                  KOUT = INEND
                  go to 20
               end if
               KB = KB + 6
               go to 40
            else
c                   Character literal ended in col. 72 of previous line.
               if (BUF1(7:7) .eq. '''') then
c                   Looks like it is continued, as as if "'" in col. 6.
                  LSTATE = LSCLIT
                  KB = 6
                  go to 40
               else
c                   Start a new token
                  LSTATE  = LSNEW
               end if
            end if
         else if (LSTATE .eq. LSSTMT) then
            go to 40
         end if
      else
         if (BUF1(1:6) .eq. ' ') then
            if (BUF1(7:72) .eq. ' ') go to 20
            KSTA = KSNONE
         else
            KSTA = KSNONE
            if (BUF1(7:72) .eq. ' ') go to 20
         end if
         KPAREN = 0
         NEWOP = 0
         NCOMM = 0
         NPAREN(0) = NPNONE
         LSTATE = 1
         NEWTOK = KTYUND
         KPAR1 = 0
      end if
      LKTOK = 1
      LKTOK1 = LKTOK
   40 KOUT = KB - 1
      BUFOUT(1:KOUT)=BUF1(1:KOUT)
      LT = 0
   50 do 400 I = KB, INEND + 1
         C = BUF1(I:I)
         if (C .eq. ' ') then
            if (I .le. INEND) go to 400
c                    Need flag to end previous token.
            K = MAPEXC
            go to 60
         end if
         J = ichar(C)
         K = MAP(J)
c LSTATE Tracks the state of the processing here.
c  LSSTMT = 1  At the start of the statement, or accumulating a keyword.
c  LSVARN = 2  Working on a variable name.
c  LSDOT  = 3  After a "." for an operand.
c  LSEXP  = 4  After an exponent letter.
c  LSEXP2 = 5  After digit or +/- following an exponent letter or do lab
c  LSINT  = 6  Working on an integer.
c  LSLOG  = 7  Working on logical
c  LSNEW  = 8  Looking for token to start.
c  LSIMPL = 9  Processing IMPLICIT
c  LSCLIT =10  In a string literal
c  LSQLIT =11  String literal that may be continued
c  LSNEXT =12  Get to start of next statement.
   60    go to (70,100,140,150,155,160,170,180,175,228), LSTATE
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
c                               "!" at start is just skipped.
                  if (K .eq. MAPEXC) then
                     if ((BUFOUT(1:6) .eq. ' ') .and. (KOUT .lt. 7))
     1                  KOUT = 0
                     go to 10
                  end if
                  go to 190
               end if
c                            Error -- Bad start of statement
               if (LERRL) call TXTOUT(
     1            ' The above statement starts badly and is ignored.')
               LSTATE = LSNEXT
               KOUT = 0
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

                  BUFOUT(KOUT+1:KOUT+3) = 'IF('
                  KOUT = KOUT+3

                  go to 350
               end if
            else if (BUF2(LT+1:LBUF2) .eq. 'DO') then


               if (K .eq. MAPDIG) then
                  if (BUF2(LT+1:LBUF2) .eq. 'DO') then
c    This is the place to decide if DO is a keyword.
c 1. Find index for first '=' (If none, then not a do.)
c 2. Then for the first '(' first ',', and first ' following.
c 3. If get a comma then DO
c 4. If get any ' then not a DO
c 5. If get a ( then look for '(' and ')'
c 6. Do until ()'s balance, then go to step 2.
c If a DO output it, and set LT = LT+2 and change state to start
c of integer.
c Else Change state to working on variable.
                     L1 = index(BUF1(I:INEND), '=')
                     if (L1 .eq. 0) go to 80
                     L1 = L1 + I
                     if (index(BUF1(L1:INEND), '''') .ne. 0) go to 80
                     L2 = 0
   72                L3 = index(BUF1(L1:INEND), ',')
                     if (L3 .eq. 0)  go to 80
                     L4 = index(BUF1(L1:INEND), '(')
                     if ((L4 .eq. 0) .or. (L4 .gt. L3)) go to 78
   74                L2 = L2 + 1
                     L1 = L1 + L4
   76                L4 = index(BUF1(L1:INEND), '(')
                     L3 = index(BUF1(L1:INEND), ')')
                     if (L3 .eq. 0) go to 80
                     if ((L4 .ne. 0) .and. (L4 .lt. L3)) go to 74
                     L1 = L1 + L3
                     L2 = L2 - 1
                     if (L2 .eq. 0) go to 72
                     go to 76
c                           Got a do
   78                BUFOUT(KOUT+1:KOUT+2) = 'DO'
                     KOUT = KOUT + 2
                     KSTA = KSSTUF
                     LT = LBUF2
                     LSTATE = LSEXP2
                     go to 360
                  end if
               end if
   80          LSTATE = LSVARN
               go to 360
            else if (K .eq. MAPEXC) then
               if (BUF2(1:LBUF2) .eq. 'END') then
                  LKTOK = 1
                  KTOK(1) = NAEND
                  KSTA = KSSTRT
                  BUFOUT(KOUT+1:KOUT+3) = 'END'
                  KOUT = KOUT+4
c                           Flush the symbol table.
                  call MANGF(0, 0, BUFOUT, KOUT, C)
                  go to 410
               end if
            end if
   90       ISTA = KSTA
            go to 520
         end if
c Working on a variable or keyword
  100    if (K .le. MAPDIG) go to 360
         LSTATE = LSNEW
         if ((KPAREN .gt. 0) .and. (C .eq. '=')) then
            if (NPAREN(KPAREN) .gt. NPKEYP) then
c                 NAME= not transformed if inside "()" of I/O statement
               BUFOUT(KOUT+1:KOUT+LBUF2-LT) = BUF2(LT+1:LBUF2)
               KOUT = KOUT + LBUF2 - LT
            else
c                 Mangle the name
               call MANGF(LT+1, LBUF2, BUFOUT, KOUT, C)
            end if
            LT = LBUF2
            KOUT = KOUT + 1
            BUFOUT(KOUT:KOUT) = '='
            go to 350
         else if (LBUF2-LT .gt. 8) then
c Special logic to catch character*(???) function xxx
            if (KEYIDX .eq. 2) then
               if (BUF2(LT+1:LT+8) .eq. 'FUNCTION') then
                  CTEMP(1:LBUF2-LT-8) = BUF2(LT+9:LBUF2)

                  BUFOUT(KOUT+1:KOUT+8) = 'FUNCTION'
                  KOUT = KOUT+8

                  LBUF2 = LBUF2 - 8
                  BUF2(LT+1:LBUF2) = CTEMP(1:LBUF2-LT)
                  KSTA = KSFUNC
               end if
            end if
         end if
         call MANGF(LT+1, LBUF2, BUFOUT, KOUT, C)
         LT = LBUF2
         if (C .ne. '(') go to 190
         KOUT = KOUT+1
         BUFOUT(KOUT:KOUT) = '('
  120    KPAREN = KPAREN + 1
         NPAREN(KPAREN) = NPPOP
         KPAR1 = KPAR1 + 1
         if (KSTA .ge. KSXREF) then
            if (KPAREN .eq. 1) then
               KSTA = KSSTUF
               if ((KEYIDX .eq. 7) .or. (KEYIDX .eq. 17) .or.
     1            (KEYIDX .eq. 16)) KSTA = KSHEAD
            end if
         end if
         go to 350
c After an operand "." (maybe logical or floating)
  140    if (K .le. MAPDIG) then
            if (K .le. MAPUC) then
               LSTATE = LSEXP
               if ((C .eq. 'D') .or. (C .eq. 'd')) NEWOP = KTYDP
            end if
            go to 360
         end if
         LSTATE = LSNEW
         go to 180
c After an exponent letter
  150    LSTATE = LSEXP2
         if ((K .eq. MAPDIG) .or. (K .eq. MAPUPM)) go to 360
         if (K .le. MAPUC) then
            LSTATE = LSLOG
            go to 360
         end if
         LSTATE = LSNEW
         go to 180
c After into the exponent
  155    if (K .eq. MAPDIG) go to 360
         LSTATE = LSNEW
         go to 180
c Working on integer
  160    if (K .eq. MAPDIG) go to 360
         if (K .eq. MAPDOT) then
            LSTATE = LSDOT
            NEWOP = KTYSP
            go to 360
         end if
         BUFOUT(KOUT+1:KOUT+LBUF2-LT) = BUF2(LT+1:LBUF2)
         KOUT = KOUT + LBUF2 - LT
         LT = LBUF2
         if ((KPAREN .eq. 0) .and. (KEYIDX .eq. KTYCHR)) then
            if (K .ge. MAPDIG) go to 190
c                 Working on character*dd name
            NEWTOK = 0
            LSTATE = LSVARN
            go to 360
         end if
         if (K .eq. MAPLC) C = char(J - LETDIF)
         if ((C .eq. 'E') .or. (C .eq. 'D')) then
            NEWOP = KTYSP
            go to 140
         end if
         if (C .eq. 'T') then
c                           Assume part of assign GO TO.
            KB = I
  164       KB = KB + 1
            if (BUF1(KB:KB) .eq. ' ') go to 164
            if ((BUF1(KB:KB) .eq. 'O') .or. (BUF1(KB:KB) .eq. 'o')) then
               KB = KB + 1
               KSTA = KSLEFT
               LSTATE = LSNEW
               BUFOUT(KOUT+1:KOUT+LBUF2-LT) = BUF2(LT+1:LBUF2)
               KOUT = KOUT + LBUF2 - LT + 2
               BUFOUT(KOUT-1:KOUT) = 'TO'
               LT = LBUF2
               go to 50
            end if
         end if
         NEWOP = KTYINT
         LSTATE = LSNEW
         go to 180
c Working on logical
  170    if (K .le. MAPUC) go to 360
         if (K .eq. MAPDOT) then
            NEWOP = KTYLOG
            LSTATE = LSNEW
            go to 360
         end if
         call TXTOUT(
     1      ' Confusion on above line in examining a logical variable.')
         go to 350
c Working on implicit
  175    if (K .le. MAPUC) then
            if (K .eq. MAPLC)  then
               J = J - LETDIF
               C = char(J)
            end if
            BUF2(1:1) = C
         else if ((C .eq. ',') .or. (C .eq. ')')) then
            if (C .eq. ')') then
              BUFOUT(KOUT+1:KOUT+LBUF2-LT) = BUF2(LT+1:LBUF2)
              KOUT = KOUT + LBUF2 - LT + 1
              BUFOUT(KOUT:KOUT) = ')'
              go to 10
            end if
         end if
         go to 360
c Looking for something to start.
  180    if (LBUF2 .gt. LT) then
            BUFOUT(KOUT+1:KOUT+LBUF2-LT) = BUF2(LT+1:LBUF2)
            KOUT = KOUT + LBUF2 - LT
            LT = LBUF2
         end if
         if (NEWOP .ne. 0) then
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
               go to 360
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
            go to 360
         end if
c After an operand  (Should this be a separate case??)
  190    KOUT = KOUT + 1
         BUFOUT(KOUT:KOUT) = C
c                 . +/-*etc   '   !   /   =   (   )   ,    :
         go to (200,330,330,210,410,230,240,250,260,260, 290), K-MAPDIG

c Got a "."
  200    LSTATE = LSDOT
         go to 380
c Got a "'"
  210    LSTATE = LSCLIT
         KB = I
         KOUT = KOUT - 1
  220    I1 = index(BUF1(KB+1:INEND), '''')
  222    if (I1 .eq. 0) then
c           Character constant is continued, pad with blanks as needed.
           BUFOUT(KOUT+1:I) = ' '
           BUFOUT(I:72) = BUF1(I:72)
           KOUT = 72
            go to 10
         end if
         KB = KB + I1 + 1
  225    if (BUF1(KB:KB) .eq. '''') go to 220
         if (KB .le. 72) then
           BUFOUT(KOUT+1:KOUT+KB-I) = BUF1(I:KB-1)
           KOUT = KOUT + KB - I
           NEWOP = KTYCHR
           NEWTOK = NTEXPR
           LSTATE = LSNEW
           go to 50
         end if
         LSTATE = LSQLIT
         I1 = 0
         go to 222
c              Enter here to continue a character literal.
  228    KB = I + 1
         go to 225
c
  230    if (NCOMM .eq. 0) go to 340
c Got a "/" in a common or save statement.
         if (KPAREN .ne. 0) go to 340
c Line below is so /name/ in a save statment is treated as an external
         if (KSTA .eq. 7) NCOMM = KSCOMM
         if (NCOMM .eq. KSCOMM) then
            if (CLAST .eq. '/') then
               LKTOK = LKTOK + 1
               KTOK(LKTOK) = LBUF2
            end if
            NCOMM = KSDCOM
         else
            NCOMM = KSCOMM
         end if
         KSTA = NCOMM
         LKTOK = LKTOK + 2
         KTOK(LKTOK-1) = NAKSTA
         KTOK(LKTOK) = KSTA
         go to 350
c Got an "="
  240    if (KSTA .eq. KSDATA) then
c### Code is not going to work if input declares an array "DATA".
            if (KPAREN .eq. 0) KSTA = KSLEFT
         end if
         LKTOK = LKTOK + 2
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
               if (LERRL) call TXTOUT(
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
         end if
         go to 350

c Got a ":"
  290    if (NPAREN(KPAREN) .eq. NPPOP) then
            I1 = 0
            do 300 I2 = LKTOK, 1, -1
               if (KTOK(I2) .eq. NAPUSH) then
                  I1 = I1 - 1
                  if (I1 .lt. 0) go to 310
               else if (KTOK(I2) .eq. NAPOP) then
                  I1 = I1 + 1
               end if
               KTOK(I2+2) = KTOK(I2)
  300       continue
  310       KTOK(I2+1) = NAEXPR
            KTOK(I2+2) = NTSUBS
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
         go to 380
c Continue accumlating variable or keyword or ??
  360    if (K .eq. MAPLC) C = char(J - LETDIF)
         LBUF2 = LBUF2 + 1
         BUF2(LBUF2:LBUF2) = C
  380    CLAST = C
  400 continue
c
  410 KOUT = KOUT - 1
  420 if (C .eq. '!') then
         if (LERRF) call TXTOUT (
     1      ' The above line contains a Fortran 90 comment.')
      end if
      if (KSTA .ne. KSNONE) then
         KTOK(LKTOK+ 1) = NALINE
         go to 10
      end if

c### Can we get to this point and still have a valid statment??
c###      I = 75
      stop " Stopped above label 450 in PACKF for curiosity."
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
               if ((LBUF2 .eq. 4) .and. (C .eq. ' ')) then
                  BUFOUT(KOUT+1:KOUT+4) = 'ELSE'
                  KOUT = KOUT + 4
                  go to 10
               end if
               if (BUF2(LT+5:LBUF2) .ne. 'IF') go to 520
               if (C .ne. '(') go to 520
               BUFOUT(KOUT+1:KOUT+7) = 'ELSEIF('
               KOUT = KOUT + 7
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
c                        Extra check for a save statment
            if (index(BUF1(I:INEND), '=') .ne. 0) go to 520
         else if (I3 .eq. 9) then
c            Special checks  when start with "DATA"
            if (I .lt. INEND) then
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
                  LBUF2 = 0
                  KB = I + 1
                  go to 50
               end if
            end if
            if (I3 .eq. 4) then
               if (LBUF2 .gt. J+8) then
c                                        Flag function header
                  if (BUF2(J+1:J+8) .eq. 'FUNCTION') I3 = -1
               end if
            else if (I3 .eq. 6) then
               if (BUF2(2:LBUF2) .eq. 'IMPLICITNONE') then
                  if (LERRF) call TXTOUT (
     1               ' The above line is Fortran 90 statement.')
                  go to 10
               end if
            end if
         end if
      end if
  480 if (J .gt. LT + 4) then
         if (BUF2(LT+5:J) .ne. FORKEY(I1:I2-1)) go to 520
      end if
      if (I3 .lt. 0) J = J + 8
      BUFOUT(KOUT+1:KOUT+J-LT) = BUF2(LT+1:J)
      KOUT = KOUT + J - LT
      if (J .lt. LBUF2) then
c                   There are more
         if (LKTOK .eq. LKTOK1) then
c                When at start of line, save all.
            LT = J
         else
            CTEMP(1:LBUF2-J) = BUF2(J+1:LBUF2)
            LBUF2 = LBUF2 + LT - J
            BUF2(LT+1:LBUF2) = CTEMP(1:LBUF2-LT)
         end if
         LSTATE = LSVARN
      else
         if (LKTOK .eq. LKTOK1) then
            LT = LBUF2
         else
            LBUF2 = LT
         end if
         LSTATE = LSNEW
      end if
      if ((I3 .lt. 0) .and. (KEYIDX .gt. 6)) go to 520
      if (KEYIDX .le. 20) then
         if (KEYIDX .le. 10) then
            KSTA = KSTYPE
            if (I3 .lt. 0) then
               KSTA = KSFUNC
            else if (KEYIDX .ge. 7) then
               KSTA = KSXREF
               if (KEYIDX .eq. 7) then
                  KSTA = KSSUBR
               else if (KEYIDX .eq. 10) then
                  KSTA = KSXREF
               end if
            end if
            go to 530
         else
            KSTA = KSTAKY(KEYIDX)
            if (KSTA .eq. -1) then
c                       Process IMPLICIT statement
                LSTATE = LSIMPL
                go to 450
            else if (KSTA .eq. KSPARM) then
               KPAREN = KPAREN + 1
               NPAREN(KPAREN) = NPPAR
c##               I = I + 1
            else if (KSTA .eq. KSDCOM) then
               NCOMM = KSCOMM
            end if
         end if
      else
         if (I3 .eq. 2) then
c              Set special paren state for keyword that require paren.
            KPAREN = KPAREN + 1
            NPAREN(KPAREN) = NPKEYP
c##            I = I + 1
         end if
         if (KEYIDX .le. 31) then
            if (I3 .eq. 7) then
c                               Special check for I/O statements.
               if (C .eq. '(') then
                  KOUT = KOUT + 1
                  BUFOUT(KOUT:KOUT) = '('
                  I = I + 1
                  KPAREN = KPAREN + 1
                  if (KEYIDX .ge. 27) then
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
               if (BUF1(I:INEND) .ne. ' ') then
                  if (C .ne. '!') go to 520
                  KSTA = KSSTUF
                  go to 420
               end if
            else if (KEYIDX .ge. 41) then
               if (LERRF) call TXTOUT (
     1        ' The above line contains Fortran 90 code.')
               go to 10
            end if
            if (KEYIDX .eq. 35) go to 10
            if (KEYIDX .eq. 39) then
               NCOMM = KSDCOM
               KSTA = KSSTUF
               go to 540
            end if
            LSTATE = LSNEXT
            if (I .le. INEND) then
               BUFOUT(KOUT+1:KOUT+INEND-I+1) = BUF1(I:INEND)
               KOUT = KOUT + INEND - I + 1
            end if
            go to 10
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
      end if
  540 if (LBUF2 .eq. LT) LSTATE = LSNEW
  550 if (I .gt. 74) go to 410
      KB = I
      go to 50
c                 End of a file.
  700 return
c                     End of subroutine PACKF
      end

      subroutine TXTOUT(ERRTXT)
c                   Prints out text
      character ERRTXT*(*)
c
c ******************** Variable Declarations ***************************
c
c Common and parameters
      integer MXLEN, MXNAME, MXTYST, MXTLEN
      parameter (MXLEN=24, MXNAME=6577, MXTYST=100,
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
c
      integer LNAME, MAP(128)
      common /GLINTB/ LNAME, MAP

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
c  Parameters used for extended types.
      integer NTSCAL, NTFUNC, NTAELT, NTEXPR, NTSUBS, NTARR
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=32,
     1   NTARR=40)
c
c  Common block
      integer LENLN2
      parameter (LENLN2=1000)
      character BUF1*(LENLN2), BUF2*64, BUFLIN*57, CURFIL*(MXLEN),
     1   NAME(MXNAME)*(MXLEN)
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, KLIN, KSTA,
     1   LETDIF, LETIDX, LFREEN, LINE, LINOUT, LENNAM(MXNAME)
      common /GLCHR1/ BUF1, BUF2, BUFLIN, CURFIL, NAME
      common /GLINT1/ IH, KLIN, KSTA,  LETDIF,
     1   LETIDX, LFREEN, LINE, LINOUT, LENNAM, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)
c
c   Locals
      integer K, L, LASTL
      save LASFIL, LASTL
      character LASFIL*(MXLEN)
      data LASFIL / '        ' /
      data LASTL / 0 /
c
 1000 format(/,'************  Diagnostics in file: ', A)
 1010 format(/,'******** Entry: ', A)
 1020 format(I5, ': ', A)
 1050 format(' Parameter MXFILE in GLOCHK must be larger.'/
     1  ' The following and all later files ignored.')

c
c ****************** Start of Executable Code **************************
c
      do 20 K = len(ERRTXT), 2, -1
         if (ERRTXT(K:K) .ne. ' ') go to 60
   20 continue
   60 if (CURFIL .ne. LASFIL)then
         LASTL = 0
         LASFIL = CURFIL
         write(8, 1000) CURFIL
      end if
      if (LINE .ne. LASTL) then
         do 90 L = LENLN2, 2, -1
            if (BUF1(L:L) .ne. ' ') go to 100
   90    continue
  100    write(8, 1020) LINE, BUF1(1:L)
         LASTL = LINE
      end if
      write (8, '(1X, A)') ERRTXT(1:K)
      return
c                 End of TXTOUT
      end
c
      subroutine WAIT
      return
      end

      subroutine MANGF(K1, K2,BUFOUT, KOUT, C)
c   Look up name, replace with mangled name on first look up if not
c   external.  If seen before, use name that was used before.
c   Save the name in BUFOUT(KOUT+1:KOUT+?) and update KOUT.
c
c ****************** Formal Arguments **********************************
c
c BUF2(K1:K2) contains the name to be looked up.
c BUFOUT(KOUT+1:KOUT+?) is where the name to be used is stored.
c        KOUT is updated to the location of the last character stored.
c C      The last character read.
c
c ******************* Internal Variables *******************************
c
c GENNAM Character string to use for the next name generated.
c IH     Index returned from the hash table look up.
c LENGEN Number of characters in the name stored in GENNAM.
c LENNAM Array giving number of chars. in the names stored in NEWNAM().
c NENNAM Array giving names stored from previous values of GENNAM.
c
c ******************* Specification Statements *************************
c
c Common and parameters
c  Parameters used to track the state of processing.
      integer KSSTRT, KSNONE, KSTYPE, KSDIM, KSCOMM, KSDIMD, KSPARM,
     1   KSDATA, KSSTUF, KSBOTH, KSLEFT, KSWRIT, KSAREF, KSHEAD, KSXREF,
     2   KSDCOM, KSENTR, KSSUBR, KSFUNC
      parameter (KSSTRT=-1, KSNONE=0, KSTYPE=1, KSDIM=2, KSCOMM=3,
     1   KSDIMD=4,  KSPARM=5, KSDATA=6, KSSTUF=7, KSBOTH=8, KSLEFT=9,
     2   KSWRIT=10, KSAREF=11, KSHEAD=12, KSXREF=13, KSDCOM=14,
     3   KSENTR=15, KSSUBR=16, KSFUNC=17)
      integer MXLEN, MXNAME, MXTYST, MXTLEN
      parameter (MXLEN=24, MXNAME=6577, MXTYST=100,
     1   MXTLEN=256)
c Formals
      integer K1, K2, KOUT
      character BUFOUT*(*), C
c Locals
      integer NINTRI
      parameter (NINTRI=85)
      integer I, IHSAV, K, LENGEN
      character GENNAM*4, HNAME*(MXLEN), NEWNAM(MXNAME)*4,
     1   INTRIN(NINTRI)*6, TMPNAM*(MXLEN)
      save GENNAM, LENGEN, NEWNAM
c
c  Common block
      integer LENLN2
      parameter (LENLN2=1000)
      character BUF1*(LENLN2), BUF2*64, BUFLIN*57, CURFIL*(MXLEN),
     1   NAME(MXNAME)*(MXLEN)
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, KLIN, KSTA,
     1   LETDIF, LETIDX, LFREEN, LINE, LINOUT, LENNAM(MXNAME)
      common /GLCHR1/ BUF1, BUF2, BUFLIN, CURFIL, NAME
      common /GLINT1/ IH, KLIN, KSTA,  LETDIF,
     1   LETIDX, LFREEN, LINE, LINOUT, LENNAM, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)

      data INTRIN / 'INT   ', 'IFIX  ', 'IDINT ', 'REAL  ', 'FLOAT ',
     1    'SNGL  ', 'DBLE  ', 'CMPLX ', 'ICHAR ', 'CHAR  ', 'AINT  ',
     2    'DINT  ', 'ANINT ', 'DNINT ', 'NINT  ', 'IDNINT', 'ABS   ',
     3    'IABS  ', 'DABS  ', 'CABS  ', 'MOD   ', 'AMOD  ', 'DMOD  ',
     4    'SIGN  ', 'ISIGN ', 'DSIGN ', 'DIM   ', 'IDIM  ', 'DDIM  ',
     5    'DPROD ', 'MAX   ', 'MAX0  ', 'AMAX1 ', 'DMAX1 ', 'AMAX0 ',
     6    'MAX1  ', 'MIN   ', 'MIN0  ', 'AMIN1 ', 'DMIN1 ', 'AMIN0 ',
     7    'MIN1  ', 'LEN   ', 'INDEX ', 'AIMAG ', 'CONJG ', 'SQRT  ',
     8    'DSQRT ', 'CSQRT ', 'EXP   ', 'DEXP  ', 'CEXP  ', 'LOG   ',
     9    'ALOG  ', 'DLOG  ', 'CLOG  ', 'LOG10 ', 'ALOG10', 'DLOG10',
     A    'SIN   ', 'DSIN  ', 'CSIN  ', 'COS   ', 'DCOS  ', 'CCOS  ',
     B    'TAN   ', 'DTAN  ', 'ASIN  ', 'DASIN ', 'ACOS  ', 'DACOS ',
     C    'ATAN  ', 'DATAN ', 'ATAN2 ', 'DATAN2', 'SINH  ', 'DSINH ',
     D    'COSH  ', 'DCOSH ', 'TANH  ', 'DTANH ', 'LGE   ', 'LGT   ',
     E    'LLE   ', 'LLT   ' /
c
c ******************** Start of Exectuable Code ************************
c
      if (K1 .eq. 0) then
c                      Initialize for a new file.
         LENGEN = 1
         GENNAM = 'A'
         IH = 0
         HNAME = ' '
c                      Initialize the hash table.
         call HASH(HNAME)
c                      Enter the intrinsic functions.
         do 30 I = 1, NINTRI
            HNAME = INTRIN(I)
            call HASH(HNAME)
            IH = -IH
            NEWNAM(IH) = '    '
            do 10 K = 2, MXLEN
               if (HNAME(K:K) .eq. ' ') go to 20
   10       continue
   20       LENNAM(IH) = K - 1
   30    continue
         return
      end if
      IH = 1
      HNAME = BUF2(K1:K2)
      call HASH(HNAME)
      if (IH .lt. 0) then
c              Name not found
         IH = -IH
c           If external see if same as previously generated name.
         if ((KSTA .ge. KSXREF) .or. ((C .eq. '(') .and.
     1      (KSTA .ne. KSDIM) .and. (KSTA .ne. KSTYPE))) then
c                       Get length of name.
            do 40 K = 2, 8
               if (NAME(IH)(K:K) .eq. ' ') go to 50
   40       continue
   50       I = K - 1
            LENNAM(IH) = I
            if (I .le. LENGEN) then
               if ((I .lt. LENGEN) .or. (HNAME .lt. GENNAM)) then
                  call TXTOUT(
     1        'Generated mangled name conflicts with external name.')
                  write(8, '('' External name is: '', A)') HNAME(1:I)
               end if
            end if
            NEWNAM(IH) = ' '
         else
c              Use generated name for a new name
            IHSAV = IH
   90       IH = 0
            TMPNAM = GENNAM
            call HASH(TMPNAM)
c        Save generated name and generate a new one.
            NEWNAM(IHSAV) = GENNAM
            LENNAM(IHSAV) = LENGEN
            K = LENGEN
  100       if (GENNAM(K:K) .eq. '9') then
               GENNAM(K:K) = 'A'
            else if (GENNAM(K:K) .eq. 'Z') then
               GENNAM(K:K) = '0'
               K = K - 1
               if (K .ne. 0) go to 100
               GENNAM(1:1) = 'A'
               LENGEN = LENGEN+1
               GENNAM(LENGEN:LENGEN) = '0'
            else
               GENNAM(K:K) = char(ichar(GENNAM(K:K)) + 1)
            end if
            if (IH .ne. 0) go to 90
            IH = IHSAV
            if (KSTA .eq. KSHEAD) LENNAM(IH) = -LENNAM(IH)
         end if
      end if
      I = abs(LENNAM(IH))
      if (NEWNAM(IH) .eq. '   ') then
         BUFOUT(KOUT+1:KOUT+I) = HNAME(1:I)
      else
         if (KSTA .ge. KSXREF) then
            if (LENNAM(IH) .gt. 0) then
               call TXTOUT(
     1         'Name should not have been mangled.')
                write(8, '('' Original name is: '', A)') HNAME(1:I)
            end if
         end if
         BUFOUT(KOUT+1:KOUT+I) = NEWNAM(IH)(1:I)
      end if
      KOUT = KOUT + I
      return
      end

      subroutine PACKC
c As for PACKF, but for C code.
c Reads input into BUF1, and sets BUF2 to characters needed for symbol
c table lookups.
c KBRACE Count of depth of braces, {}.
c KPAREN Count of depth of parens and brackes, ([]).
c LSTATE Tracks the state of the processing here.
c  LINAME = 1  Accumulating a name.
c  LSTUFF = 2  Accumulating some unknown token.
c  LANAME = 3  Eating blanks after a name.
c  LSTUFB = 4  Blank after accumulating unknown token.
c  LBETOK = 5  Between tokens.
c  LINCOM = 6  Inside a comment
c  LINLIT = 8  Inside a character literal.
c LSTSAV Saved value of LSTATE when in a comment.
c MSTA   Tracks the the state of processing.  An error message is given
c        if a name to be mangled is found on the look up.
c  MPPNO  = 1   Getting the name for a preprocessor command.
c  MANGP  = 2   Mangle a name used in the preprocessor
c  MPPOK  = 3   Mangle is O.K. for preprocessor.
c  MANGDO = 4   Mangle of usual names if not found.
c  MANGOK = 5   Name can be mangled, but otherwise won't be.
c  MANGIF = 6   Mangle if see specific type declaration.
c  MANGNO = 7   Names should not be mangled.
c
c Common and parameters
      integer MXLEN, MXNAME, MXTYST, MXTLEN
      parameter (MXLEN=24, MXNAME=6577, MXTYST=100,
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
c
      integer LNAME, MAP(128)
      common /GLINTB/ LNAME, MAP

c Common and parameters
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
c  Parameters used for extended types.
      integer NTSCAL, NTFUNC, NTAELT, NTEXPR, NTSUBS, NTARR
      parameter (NTSCAL=0, NTFUNC=8, NTAELT=16, NTEXPR=24, NTSUBS=MXLEN,
     1   NTARR=40)
c
c  Common block
      integer LENLN2
      parameter (LENLN2=1000)
      character BUF1*(LENLN2), BUF2*64, BUFLIN*57, CURFIL*(MXLEN),
     1   NAME(MXNAME)*(MXLEN)
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, KLIN, KSTA,
     1   LETDIF, LETIDX, LFREEN, LINE, LINOUT, LENNAM(MXNAME)
      common /GLCHR1/ BUF1, BUF2, BUFLIN, CURFIL, NAME
      common /GLINT1/ IH, KLIN, KSTA,  LETDIF,
     1   LETIDX, LFREEN, LINE, LINOUT, LENNAM, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)

      integer LINAME, LSTUFF, LANAME, LSTUFB, LBETOK, LINCOM, LINLIT
      parameter (LINAME=1, LSTUFF=2, LANAME=3, LSTUFB=4, LBETOK=5,
     1   LINCOM=7, LINLIT=8)
      integer MPPNO, MANGP, MPPOK, MANGDO, MANGOK, MANGIF, MANGNO
      parameter(MPPNO=1, MANGP=2, MPPOK=3, MANGDO=4, MANGOK=5, MANGIF=6,
     1  MANGNO=7)
c
c   Locals
      character C, BUFOUT*(LENLN2), CRES*222, CPP*75
      integer I, I1, INEND, J, K, KB, KBRACE, KOUT, KPAREN, L, LBUF2,
     2    LPAR(100), LSTSAV, LSTATE, MSTA
c
c                  1                   21                             53
c    54                                                              119
c   120      129
      data CRES / '+extern+typedef+goto+struct+union+char+double+enum+fl
     1oat+int+void+auto+register+static+const+volatile+signed+unsigned+l
     2ong+short+if+break+case+continue+default+do+else+entry+for+return+
     3sizeof+switch+while+asm+fortran'/
c                  1                                                  53
c    54
      data CPP /  '+define+undef+if+ifdef+ifndef+else+elif+endif+defined
     1+include+pragma+error+' /

c
c ********************** Start of Executable Code **********************
c
      MAP(ichar('_')) = MAPLC
      KOUT = 0
      MSTA = MANGOK
      KBRACE = 0
      KPAREN = 0
      LSTATE = LBETOK
      BUF2(1:1) = '+'
c                                 Get the next line of input.
   10 LBUF2 = 1
   20 if (KOUT .ne. 0) then
         write (10, '(A)') BUFOUT(1:KOUT)
         KOUT = 0
         LINOUT = LINOUT + 1
         if (mod(LINOUT, 10) .eq. 0) then
            write(BUFLIN(KLIN:KLIN+5), '(I5)') LINE
            KLIN = KLIN + 5
            if (KLIN .gt. 55) then
               write (11, '(A)') BUFLIN(1:KLIN)
               KLIN = 7
               write (BUFLIN(1:7), '(I5, '': '')') LINOUT + 10
            end if
         end if
      end if


      LINE = LINE + 1
      read (9, '(A)', END=700) BUF1

c INEND  index in BUF1 of the end of line.
      J = LENLN2
      if (BUF1(50:LENLN2) .eq. ' ') J = 50
      do 40 INEND = J, 1, -1
         if (BUF1(INEND:INEND) .ne. ' ') go to 50
   40 continue
c
   50 continue
      if (LSTATE .eq. LINCOM) then
          I1 = 1
   60     J = index(BUF1(I1:INEND), '*')
          if (J .eq. 0) go to 20
          I1 = I1 + J
          if (BUF1(I1:I1) .ne. '/') go to 60
          KB = I1 + 1
          LSTATE = LSTSAV
          go to 120
      end if
c
      if (LSTATE .ne. LINLIT) go to 100
c                 Processing of character literals
      I1 = 1
   70 L = I1
   80 J = index(BUF1(I1:INEND), '"')
      LSTATE = LBETOK
      if (J .ne. 0) then
         I1 = I1 + J
         do 85 K = I1-2, 2, -2
c                   Checking for \
            if (BUF1(K:K) .ne. char(92)) go to 90
            if (BUF1(K-1:K-1) .ne. char(92)) go to 80
   85    continue
      else if (BUF1(INEND:INEND) .ne. char(92)) then
         call TXTOUT('Character literal not terminated??')
      else
         LSTATE = LINLIT
         I1 = INEND+1
      end if
   90 KB = I1-1
      BUFOUT(KOUT+1:KOUT+I1-L) = BUF1(L:KB)
      KOUT = KOUT + I1 - L
      if (KB .ge. INEND) go to 20
      go to 110
c
  100 if (INEND .eq. 0) go to 20
      KB = 0
  110 KB = KB + 1
      if (BUF1(KB:KB) .le. ' ') go to 110

  120 do 500 I = KB, INEND + 1
         C = BUF1(I:I)
         if (C .le. ' ') then
            if (LSTATE .le. LSTUFF) LSTATE = LSTATE + 2
            if (I .le. INEND) go to 500
            if (LSTATE .ge. LBETOK) then
               if (MSTA .le. MPPOK) MSTA = MANGOK
               go to 20
            end if
         end if
         J = ichar(C)
         K = MAP(J)
         go to (150, 390, 160, 180, 400), LSTATE
c Accumulating a name
  150    if (K .le. MAPDIG) go to 460
c Got an end to  name.  Check if keyword (is so set mangle class, if not,
c look up and mangle depending on the current character and on the state)
  160    LBUF2 = LBUF2 + 1
         BUF2(LBUF2:LBUF2) = '+'

         if (MSTA .le. MPPOK) then
            if (MSTA .eq. MPPNO) then
               if (BUFOUT(KOUT:KOUT) .eq. '#') then
                  L = index(CPP, BUF2(1:LBUF2))
                  if (L .eq. 1) then
                     MSTA = MANGP
                  else if (L .lt. 54) then
                     MSTA = MPPOK
                     if (L .eq. 0) call TXTOUT(
     1                     'Preprocessor keyword not recognized.')
                  end if
               end if
               BUFOUT(KOUT+1:KOUT+LBUF2-2) = BUF2(2:LBUF2-1)
               KOUT = KOUT + LBUF2 - 2
               go to 180
            else if (MSTA .eq. MANGP) then
               KSTA = MSTA
               MSTA = MPPOK
            else
               KSTA = MANGOK
            end if
         else
            L = index(CRES, BUF2(1:LBUF2))
            if (L .ne. 0) then
c                       Some kind of a C keyword
               if (L .lt. 21) then
c                       A keyword followed by an external name.
                  MSTA = MANGNO
               else if (L .lt. 129) then
c  Some kind of type declaration mangle except if at level 0 on {}()[]
                  if (MSTA .le. MANGIF) then
                     MSTA = MANGDO
                     if (KBRACE + KPAREN .eq. 0) MSTA = MANGNO
                  end if
               else
c  Other keyword, just follow previous action.
                  MSTA = MANGOK
               end if
               BUFOUT(KOUT+1:KOUT+LBUF2-2) = BUF2(2:LBUF2-1)
               KOUT = KOUT + LBUF2 - 2
               go to 180
            else if (K .gt. MAPUC) then
               KSTA = MSTA
               if (K .eq. MAPLP) KSTA = MANGOK
            else
c                 Never mangle if name is followed by a name.
               KSTA = MANGNO
            end if
         end if
c                 Look up, maybe mangle.
         call MANGC(LBUF2, BUFOUT, KOUT, KSTA)
  180    LBUF2 = 1
         if (K .le. MAPDIG) then
            KOUT = KOUT + 1
            BUFOUT(KOUT:KOUT) = ' '
            if (K .le. MAPUC) then
               LSTATE = LINAME
               go to 460
            else
               LSTATE = LSTUFF
               go to 220
            end if
         else
            if (MSTA .le. MPPOK) then
               if (BUF1(I-1:I-1) .le. ' ') then
                  KOUT = KOUT + 1
                  BUFOUT(KOUT:KOUT) = ' '
               end if
            end if
            LSTATE = LBETOK
         end if
c Some k
  200    L = index(',{([})];"''/=*#\', C)
c                 ,    {    (    [    }    )    ]    ;    "    '    /
         go to (230, 240, 250, 250, 260, 270, 270, 280, 290, 300, 340,
     1     350, 360, 370, 380), L
c            =    *    #    \
c Character that should just be copied.
  220    KOUT = KOUT + 1
         BUFOUT(KOUT:KOUT) = C
         go to 500
c "," May need MSTA reset.
  230    if (KPAREN .eq. 0) go to 220
         if (MSTA.eq.MANGDO) MSTA = MANGIF
         go to 220
c               Start new brace level.
  240    KBRACE = KBRACE + 1
         MSTA = MANGOK
         go to 220
c               Start new paren/bracket level
  250    KPAREN = KPAREN + 1
         LPAR(KPAREN) = MSTA
         if (MSTA .ge. MANGDO) then
            if (C .eq. '[') then
               MSTA = MANGOK
            else if (MSTA .ne. MANGOK) then
               MSTA = MANGIF
            end if
         end if
         go to 220
c               Drop down one brace level.
  260    if (KBRACE .gt. 0) then
            KBRACE = KBRACE - 1
            if (KPAREN .eq. 0) then
c                    Flush the symbol table
c               call MANGC(0, BUFOUT, KOUT, KSTA)
               go to 220
            end if
            call TXTOUT('Unbalanced brackets or parens.')
            KPAREN = 0
            go to 220
         end if
         call TXTOUT('Unbalanced braces.')
         KBRACE = 0
         go to 220
c                 Drop down one paren/bracket level.
  270    if (KPAREN .gt. 0) then
            MSTA = LPAR(KPAREN)
            KPAREN = KPAREN - 1
            go to 220
         end if
         call TXTOUT('Unbalanced brackets or parens.')
         go to 220
c
c ';' Start a new statement.
  280    MSTA = MANGOK
         LSTATE = LBETOK
         go to 220
c
c "  Start a character literal
  290    KOUT = KOUT + 1
         BUFOUT(KOUT:KOUT) = '"'
         I1 = I + 1
         go to 70
c '  A character constant
  300    if (BUF1(I+2:I+2) .ne. '''') then
            if ((BUF1(I+1:I+1) .ne. char(92)) .and.
     1         (BUF1(I+3:I+3) .ne. '''')) go to 220
            BUFOUT(KOUT+1:KOUT+4) = BUF1(I:I+3)
            KOUT = KOUT + 4
            KB = I + 4
            go to 120
         end if
         BUFOUT(KOUT+1:KOUT+3) = BUF1(I:I+2)
         KOUT = KOUT + 3
         KB = I + 3
         go to 120
c
c / May start a comment
  340    if (BUF1(I+1:I+1) .ne. '*') go to 220
         if (LSTATE .le. LSTUFF) LSTATE = LSTATE + 2
         L = index(BUF1(I+2:INEND), '*/')
         if (L .eq. 0) then
c                        Comment is continued
            if (LSTATE .le. LSTUFF) then
c                        Get name into ouput buffer.
               LSTATE = LSTATE + 2
               go to 70
            end if
            LSTSAV = LSTATE
            LSTATE = LINCOM
            go to 20
         end if
         KB = I + L + 3
         if (KB .gt. INEND) go to 20
         if (KOUT .ne. 0) then
            KOUT = KOUT+1
            BUFOUT(KOUT:KOUT) = ' '
         end if
         go to 120
c
c =  => no declarations after this
  350    if (MSTA .ge. MANGDO) MSTA = MANGOK
         go to 220

c * Check if blanks deleted when they shouldn't have been
  360    if (BUFOUT(KOUT:KOUT) .eq. '/') then
            KOUT = KOUT + 1
            BUFOUT(KOUT:KOUT) = ' '
         end if
         go to 220
c
c # Start preprocessor name
  370    if (KOUT .ne. 0) go to 220
         if (map(ichar(BUF1(I+1:I+1))) .gt. MAPUC) go to 220
         MSTA = MPPNO
         LSTATE = LINAME
         go to 220
c
c \ An escape character
  380    KOUT = KOUT + 1
         BUFOUT(KOUT:KOUT) = char(92)
c                   Continued line goes to get next line.
         if (I .eq. INEND) go to 10
c                            Some kind of escape character??
         KOUT = KOUT + 1
         BUFOUT(KOUT:KOUT) = BUF1(I:I)
         KB = I + 2
         go to 120
c
c Just accumulating something
  390    if (K .le. MAPUPM) then
            if ((K .lt. MAPUPM) .or.
     1         (MAP(ichar(BUF1(I+1:I+1))) .eq. MAPDIG)) go to 220
         end if
         LSTATE = LBETOK
         go to 200
c
c Between tokens
  400    if (K .ge. MAPDIG) then
            if (K .eq. MAPDIG) then
               LSTATE = LSTUFF
               go to 220
            end if
            go to 200
         end if
         LSTATE = LINAME
         LBUF2 = 1
c Continue accumlating variable or keyword or ??
  460    LBUF2 = LBUF2 + 1
         BUF2(LBUF2:LBUF2) = C
  500 continue
      if (MSTA .le. MPPOK) MSTA = MANGOK
      go to 10
c                 End of a file.
  700 return
c                     End of subroutine PACKC
      end

      subroutine MANGC(L,BUFOUT, KOUT, LSTA)
c   Look up name, replace with mangled name on first look up if not
c   external.  If seen before, use name that was used before.
c   Save the name in BUFOUT(KOUT+1:KOUT+?) and update KOUT.
c
c ****************** Formal Arguments **********************************
c
c BUF2(2:L-1) contains the name to be looked up.
c BUFOUT(KOUT+1:KOUT+?) is where the name to be used is stored.
c        KOUT is updated to the location of the last character stored.
c C      The last character read.
c
c ******************* Internal Variables *******************************
c
c GENNAM Character string to use for the next name generated.
c IH     Index returned from the hash table look up.
c LENGEN Number of characters in the name stored in GENNAM.
c LENNAM Array giving number of chars. in the names stored in NEWNAM().
c NENNAM Array giving names stored from previous values of GENNAM.
c
c ******************* Specification Statements *************************
c
c Formals
      integer L, KOUT, LSTA
      character BUFOUT*(*)
c Global parameters
      integer MXLEN, MXNAME, MXTYST, MXTLEN
      parameter (MXLEN=24, MXNAME=6577, MXTYST=100,
     1   MXTLEN=256)
c Locals
      integer I, J, IHSAV, K, LENGEN
      character GENNAM*4, HNAME*(MXLEN), NEWNAM(MXNAME)*4,
     1   TMPNAM*(MXLEN), TEXT*66
      save GENNAM, LENGEN, NEWNAM
c Parameters for LSTA
      integer MPPNO, MANGP, MPPOK, MANGDO, MANGOK, MANGIF, MANGNO
      parameter(MPPNO=1, MANGP=2, MPPOK=3, MANGDO=4, MANGOK=5, MANGIF=6,
     1  MANGNO=7)
c  Parameters used for character mappings
      integer MAPLC, MAPUC, MAPDIG, MAPDOT, MAPUPM, MAPOTH, MAPSTR,
     1  MAPEXC, MAPDIV, MAPEQ, MAPRP, MAPLP, MAPCOM, MAPCOL
      parameter (MAPLC=1, MAPUC=2, MAPDIG=3, MAPDOT=4, MAPUPM=5,
     1 MAPOTH=6, MAPSTR=7, MAPEXC=8, MAPDIV=9, MAPEQ=10,
     2 MAPLP = 11, MAPRP=12, MAPCOM=13, MAPCOL=14)
c
c  Common blocks
      integer LNAME, MAP(128)
      common /GLINTB/ LNAME, MAP

      integer LENLN2
      parameter (LENLN2=1000)
      character BUF1*(LENLN2), BUF2*64, BUFLIN*57, CURFIL*(MXLEN),
     1   NAME(MXNAME)*(MXLEN)
      logical LERREQ(4), LERRI, LERRL, LERRP, LERRF
      integer IH, KLIN, KSTA,
     1   LETDIF, LETIDX, LFREEN, LINE, LINOUT, LENNAM(MXNAME)
      common /GLCHR1/ BUF1, BUF2, BUFLIN, CURFIL, NAME
      common /GLINT1/ IH, KLIN, KSTA,  LETDIF,
     1   LETIDX, LFREEN, LINE, LINOUT, LENNAM, LERREQ
      equivalence (LERREQ(1),LERRI), (LERREQ(2),LERRL),
     1   (LERREQ(3),LERRP), (LERREQ(4),LERRF)

c
c ******************** Start of Exectuable Code ************************
c
      if (L .eq. 0) then
c                      Initialize for a new file.
         LENGEN = 2
         GENNAM = 'aA'
         IH = 0
         HNAME = ' '
         call HASH(HNAME)
         return
      end if
      IH = 1
      HNAME = BUF2(2:L-1)
      call HASH(HNAME)



      if (IH .lt. 0) then
c              Name not found
         IH = -IH
c           If external see if same as previously generated name.
         if (((LSTA .ne. MANGDO) .and. (LSTA .ne. MANGP)) .or.
     1      (HNAME(1:1) .eq. '_')) then
c                       Get length of name.
            do 40 K = 2, MXLEN
               if (NAME(IH)(K:K) .eq. ' ') go to 50
   40       continue
   50       I = K - 1
            LENNAM(IH) = I
            if ((I .le. LENGEN) .and. (I .gt. 1)) then
               if ((MAP(ichar(HNAME(1:1))) .eq. MAPLC)) then
                  do 60 J = 2, I
                     if (MAP(ichar(HNAME(J:J))) .eq. MAPLC) go to 70
   60             continue
                  call TXTOUT(
     1        'Generated mangled name conflicts with external name.')
                  write(8, '('' External name is: '', A)') HNAME(1:I)
               end if
            end if
   70       NEWNAM(IH) = ' '
         else
c              Use generated name for a new name
            IHSAV = IH
   90       IH = 0
            TMPNAM = GENNAM
            call HASH(TMPNAM)
c        Save generated name and generate a new one.
            NEWNAM(IHSAV) = GENNAM
            LENNAM(IHSAV) = LENGEN
            K = LENGEN

  100       if (GENNAM(K:K) .eq. '9') then
               GENNAM(K:K) = 'A'
            else if (GENNAM(K:K) .eq. 'Z') then
               GENNAM(K:K) = '0'
               K = K - 1
               go to 100
            else if (GENNAM(K:K) .eq. 'z') then
               GENNAM(1:1) = 'a'
               LENGEN = LENGEN+1
               GENNAM(LENGEN:LENGEN) = '0'
            else
               GENNAM(K:K) = char(ichar(GENNAM(K:K)) + 1)
            end if
            if (IH .ne. 0) go to 90
            IH = IHSAV
         end if
      end if
  120 I = abs(LENNAM(IH))
      if (NEWNAM(IH) .eq. '   ') then
         BUFOUT(KOUT+1:KOUT+I) = HNAME(1:I)
      else
         if (LSTA .ge. MANGIF) then
            do 140 K = 2, MXLEN
               if (NAME(IH)(K:K) .eq. ' ') go to 150
  140       continue
  150       I = K - 1
            LENNAM(IH) = I
            NEWNAM(IH) = '    '
            write (TEXT,
     1         '('' Warning -- "'',A,''" is being unmangled.'')')
     2         HNAME(1:I)
            call TXTOUT(TEXT(1:I+34))
            go to 120
         end if
         BUFOUT(KOUT+1:KOUT+I) = NEWNAM(IH)(1:I)
      end if
      KOUT = KOUT + I
      return
      end
