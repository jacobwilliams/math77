c++ CODE for .C. is inactive
C%% static FILE *c_handle[2], *scratch_file;
C%% static char *c_fname[2]={"MESSF-xx", "MESSF-xx"};
C%% char *ctmp;
c++ END
      subroutine MESS(MACT, TEXT, IDAT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2010-02-22 MESS  Krogh  Moved NSKIP=0 to start of code.
c>> 2009-10-30 MESS  Krogh  Defined DSCRN.
c>> 2009-02-28 MESS  Krogh  Added FMTT = ' ' for NAG compiler.
c>> 2009-02-28 MESS  Krogh  Fixed "f" format for C code.
c>> 2007-09-08 MESS  Krogh  Fixed definitions of MEVLAS.
c>> 2006-07-27 MESS  Krogh  Fixed boundary case in printing long text.
c>> 2006-03-20 MESS  Krogh  Added code for output of sparse vector.
c>> 2005-04-07 MESS  Krogh  Declared LFLGDB integer in MESSMH.
c>> 2004-12-15 MESS  Krogh  Added " - 1" at end of line on label 410.
c>> 2002-05-17 MESS  Krogh  Added way for user to get error count.
c>> 2001-12-28 MESS  Krogh  Added NSKIP for more flexible output values.
c>> 2000-12-30 MESS  Krogh  Fixed some types/casts in C code.
c>> 1997-12-12 MESS  Krogh  Prefixed 0P edit descriptor to F format.
c>> 1996-07-11 MESS  Krogh  Transpose matrix output for C.
c>> 1996-06-27 MESS  Krogh  fprintf(stdout, => printf( & memset now used
c>> 1996-06-18 MESS  Krogh  "Saved" NTEXTR.
c>> 1996-05-15 MESS  Krogh  Changes to use .C. and C%%.
c>> 1996-03-30 MESS  Krogh  Added external statement.
C>> 1996-01-24 MESS  Krogh  Fixed minor bug introduced with "$ " stuff.
C>> 1996-01-23 MESS  Krogh  Minor changes for C conversion.
C>> 1995-11-10 MESS  Krogh  Add code to change "$ " to " " in headings.
C>> 1995-08-11 MESS  Krogh  Made code default to not using UMESS.
C>> 1995-01-20 MESS  Krogh  Fixed unusual case in matrix output.
C>> 1994-12-15 MESS  Krogh  Removed block data for Cray T3D.
C>> 1994-11-11 MESS  Krogh  Declared all vars.
c>> 1994-09-14 MESS  Krogh  Fixed to get 1 more "$" in C output.
c>> 1994-09-08 MESS  Krogh  Added new matrix/vector capabilities.
c>> 1994-08-22 MESS  Krogh  Fix for conversion to C for new converter.
c>> 1994-07-05 MESS  Krogh  Fixed bug, KDI and FMTI could be inconsist.
c>> 1994-05-20 MESS  Krogh  Changes to MESSFT so line 1 can go to file.
c>> 1994-05-20 MESS  Krogh  Changes to setting output unit.
c>> 1994-05-09 MESS  Krogh  Integer vectors had overflow & space probs.
c>> 1993-05-19 MESS  Krogh  Changed TEXT to array of character strings.
c>> 1993-04-14 MESS  Krogh  Fixes for conversion to C. (C%% comments.)
c>> 1993-03-10 MESS  Krogh  Broke into smaller pieces.
c>> 1992-12-02 MESS  Krogh  Added save statement to block data subpr.
c>> 1992-07-13 MESS  Krogh  Add checks in heading set up.
c>> 1992-07-12 MESS  Krogh  Fixed so $$ prints a single $ in TEXT.
c>> 1992-07-12 MESS  Krogh  Set out of bound inputs to limit values.
c>> 1992-07-12 MESS  Krogh  Fixed so output works to alternate files.
c>> 1992-07-12 MESS  Krogh  Added integer declarations for parameters.
c>> 1992-06-24 MESS  Krogh  More blanks allowed on break of long lines.
c>> 1992-06-10 MESS  Krogh  Minor fix to vector output.
c>> 1992-05-27 MESS  Krogh  Fixed bug on line width setting.
c>> 1992-05-14 MESS  Krogh  Put common blocks in save statement.
c>> 1992-05-11 MESS  Krogh  Added label to assigned go to & a comment.
c>> 1992-04-08 MESS  Krogh  Unused labels 60, 220 and 320 removed.
c>> 1992-03-20 MESS  Krogh  Changed status on open to SCRATCH.
c>> 1992-03-10 MESS  Krogh  1 line below label 690 changed max to min.
c>> 1992-02-05 MESS  Krogh  Fixed bugs in printing matrix labels.
c>> 1992-01-29 MESS  Krogh  Added UMESS and multiple print option.
c>> 1991-12-09 MESS  Krogh  Fine tuning of vector output.
c>> 1991-10-10 MESS  Krogh  Insure no stop if stop level = 9.
c>> 1991-06-26 MESS  Krogh  Initial Code.
c Processes Messages -- Actions are controlled by MACT().
c This routine is intended for use primarily by other library routines.
c Users of library routines may want to use values of MACT from MERET-
c MESUNI, and may have an interest in using it to print messages
c from their own software.
c This routine has companion routines that are called with the same
c three arguments, plus one additional argument.  This argument is
c referred to here as FDAT since actions specified here can result
c in returns to print data from FDAT.  The name FDAT is used because
c this other routine will ordinarily print floating point data, but
c it could also print other kinds of data, e.g. logical.  At present
c only SMESS and DMESS are defined which are for single and double
c precision floating point data.
c MACT is a vector specifying sequentially the actions desired.
c Some of these actions require more than one location, in which
c case the next action follows the last datum required by the
c previous action.  Internal variables together with default
c values in parentheses which are used to keep track of locations
c are as follows:
c  NTEXT  (1)   The next text output starts at TEXT(NTEXT).
c  NIDAT  (1)   The next output from IDAT starts at IDAT(NIDAT).
c  NFDAT  (1)   The next output from FDAT starts at FDAT(NFDAT).
c  NMDAT  (1)   The next output from MDAT starts at MDAT(NMDAT), where
c               MDAT is defined by actions MEMDA1-MEMDA5 below, and
c               NMDAT is set to one at end of every text output.
c An action which uses data pointed to by one of the above will cause
c the pointer to be incremented to one past the last location used.  An
c exception is NMDAT which when it reaches 5 is not incremented and the
c value pointed to is incremented instead.
c Actions are encoded by values starting in MACT(1) as follows.
c (Arguments required are given in parentheses at the start of
c description.  These arguments follow the action index.  The next
c action follows the last argument for the preceding action.  Action
c indices have been selected so that it is easy to add new functionality
c without affecting codes using an earlier version.  Where bounds are
c indicated for an argument, if the argument is outside the bounds it is
c treated as if it had the value for the bound violated.)
c MESUNI=10  (0 .le. K10 .le. 99) Set the unit to use for a scratch
c            file.  The default unit for a scratch file is 30.  If a
c            scratch file is needed, (only needed here if a table
c            exceeds the line length), and unit 30 can not be opened as
c            a new scratch file, then units 29, 28, ..., will be tried
c            until an acceptable unit is found.  Library routines may
c            use this file but must be sure that the use does not
c            conflict with the printing of tables here, or the use by
c            any other library routines.  If K10 is 0, a scratch unit is
c            assumed not to be available, and tables with long lines
c            will be printed with each line on multiple lines.
c MEHEAD=11  (0 .le. K11 .le. 1) Defines the print that surrounds an
c            error message.  K11=0 gives nothing, and 1 gives the first
c            4 characters in TEXT repeated 18 times.  If this is not
c            used, one gets 72 $'s.  (To get a blank line use 1 with
c            TEXT = '    '.)
c MEDDIG=12  (-50 .le. K12 .le. 50) Set default digits to print for
c            floating point.  If K12 > 0 then K12 significant digits
c            will be printed, if K12 < 0, then -K12 digits will be
c            printed after the decimal point, and if K12 = 0, the
c            default will be used, which is the full machine precision.
c            Setting or getting this value will only work properly if
c            the action is taken by calling SMESS or DMESS as
c            appropriate.
c MEMLIN=13  (39 .le. K13 .le. 500) Set message line length to K13.
c            (Default is 128.)
c MEELIN=14  (39 .le. K14 .le. 500) Set error message line length to
c            K14. (Default is 79)
c MEMUNI=15  (-99 .le. K15 .le. 99) Messages go to unit K15.  If K15 = 0
c            (default), 'print' is used.  If K15 < 0, messages go to
c            both 'print' and to unit abs(K15).  If a write can not be
c            done to unit abs(K15), this unit will be opened with file
c            name MESS_Fxx.tmp, where xx is the value of abs(K15).
c MEEUNI=16  (-99 .le. K16 .le. 99) As for MEMUNI, except for Error
c            Messages.
c MESCRN=17  (0 .le. K17 .le. 100000000) Set number of lines to print to
c            standard output before pausing for "go" from user.  Default
c            is 0, which never stops.
c MEDIAG=18  (0 .le. K18 .le. 1000000000) Set the diagnostic level
c            desired.  This routine makes no use of K18.  It merely
c            serves as a place to set it and to answer inquiries on its
c            value.  It is intended to be set by users of library
c            software.  Library packages that make use of this number
c            are expected to use it as described below.  If K18 = 0 (the
c            default), no diagnostic print is being requested.  Else m =
c            mod(K18, 256) determines whether a package will do
c            diagnostic printing.  Associated with a library package is
c            a number L which must be a power of 2 < 129, and which
c            should be mentioned in the documentation for the package.
c            If the bit logical or(m,L) = L then diagnostic output for
c            the routine with the associated value of L is activated.
c            The value of L should have been selected by the following
c            somewhat vague rules.  Let base 2 log(L) = 2*i + j, where j
c            is 0 or 1.  Select i = level of the library package, where
c            the level is 0 if no other library routine that is likely
c            to be used with the package could reasonably be expected to
c            want any embedded diagnostics, and otherwise is
c            min(4, I+1), where I is the maximum level for any library
c            routine which is likely to be used with the package.
c            Select j = 0 if the user is relatively unlikely to want
c            diagnostics, and j = 1, if this is a routine for which
c            considering its level the user is relatively likely to want
c            diagnostic output.  The next 8 bits, mod(K18/256, 256), may
c            be used by the library routine to select the actual output
c            that is to be given.  These bits may be ignored,  but if
c            they are used, the lowest order bits should correspond to
c            less voluminous output that is more likely to be requested.
c            Finally, K18 / (2**16) may be used to give a count on how
c            many times to print the diagnostics that are given.  This
c            count may be interpreted by library routines in slightly
c            different ways, but when used it should serve to turn off
c            all output after a certain limit is reached.  By
c            convention, if this is 0 there is no upper bound on the
c            count.
c MEMAXE=19  (0 .le. K19 .le. 1000000000) Set the maximum error value.
c            When retrieving this value, it is the maximum value seen
c            for 10000*s + 1000*p + i, where s, p, and i are the stop
c            and print levels, and the index on the last error message
c            processed, respectively.  See MEEMES below.
c MESTOP=20  (0 .le. K20 .le. 8) Set the stop level for error messages.
c            If an error message has a stop index > min(K20, 8), the
c            program is stopped after processing the message.  The
c            default value is K20=3.
c MEPRNT=21  (0 .le. K21 .le. 8) Set the print level for error messages.
c            If an error message has a print index > K21, or the message
c            is going to stop when finished, information in an error
c            message is processed, else all the actions including
c            printing are skipped.  (MESTOP controls stopping.)  The
c            default value is MEPRNT = 3.
c An action index of -i, for i < METDIG, will return in the location
c ordinarily used for Ki the current default value for the internal
c variable set by Ki.  In the case of MESUNI, if the scratch unit has
c not been opened, it will be opened before returning the unit number.
c
c METDIG=22  (-50 .le. K22 .le. 50) As for MEDDIG, except the value here
c            is temporary, lasting until the return, or next use of this
c            action.  If 0, the internal value for K12 is used instead.
c MENTXT=23  (1 .le. K23 .le. 10000000) Set value of NTEXT to K23.
c MEIDAT=24  (1 .le. K24 .le. 1000000000) Set value of NIDAT to K24.
c MEFDAT=25  (1 .le. K25 .le. 1000000000) Set value of NFDAT to K25.
c MEMDAT=26  (1 .le. K26 .le. 5) Set value of NMDAT to K26.
c MEMDA1=27  (K27) set MDAT(1) to K27.  See description of NMDAT above.
c MEMDA2=28  (K28) set MDAT(2) to K28.
c MEMDA3=29  (K29) set MDAT(3) to K29.
c MEMDA4=30  (K30) set MDAT(4) to K30.
c MEMDA5=31  (K31) set MDAT(5) to K31.
c METABS=32  (1 .le. K32 .le. 100) set spacing for tabs to K32.
c MECONT=50  Exit, but no print of current print buffer.  The error or
c            diagnostic message is to be continued immediately.
c MERET=51   All done with diagnostic or error message, complete
c            processing and return, or for some error messages stop.
c MEEMES=52  (K52, L52, M52) Start an error message with severity level
c            K52,index for the error of L52, and message text starting
c            at TEXT(M52).  If M52 is 0, message text starts at
c            TEXT(NTEXT), and if M52 < 0, no message text is
c            printed as part of this action.  Library routines should
c            set K52 = 10*s + p, where s is the stop level desired, and
c            p the print level, and should have 10 > p .ge. s .ge. 0.
c            We offer the following guidelines as a yardstick for
c            setting the value of s.
c   = 9  User has ignored warning that program was going to be stopped.
c   = 8  Program has no way to continue.
c   = 7  User has given no indication of knowing that functionality of
c        results is reduced.  (E.g. not enough space for some result.)
c   = 6  Program could continue but with reduced functionality.
c   = 5  Results far worse than user expected to want.
c   = 4  User has given no indication of knowing that results do not
c        meet requested or expected accuracy.
c   = 3  Warning is given that program will be stopped without some
c        kind of response from the calling program.
c   = 2  Program is not delivering requested or expected accuracy.
c   = 1  Some kind of problem that user could correct with more care in
c        coding or in problem formulation.
c   = 0  Message is for information of uncritical nature.
c            Print levels might be counted down so that warnings given
c            several times are no longer given, or be counted up so
c            that a warning is only given after a certain threshold is
c            reached.  Levels should be selected with the understanding
c            that the default is to print only levels above 3.
c METEXT=53  Print TEXT, starting at TEXT(NTEXT).  Print ends
c            with the last character preceding the first '$'.  Special
c            actions are determined by the character following the '$'.
c            Except as noted, the '$' and the single character which
c            follows are not printed.  In the text below, "to continue",
c            means to continue print of TEXT with the next character
c            until the next "$".  Except for the one case noted, NTEXT
c            is set to point to the second character after the "$".
c            Note, letters must be in upper case.  Possibilities are:
c      B  Break text, but don't start a new line.
c      E  End of text and line.
c      R  Break text, don't change the value of NTEXT.  Thus next
c         text Repeats the current.
c      N  Start a New line, and continue.
c      I  Print IDAT(NIDAT), set NIDAT=NIDAT+1, and continue.
c      J  As for I above, except use the last integer format
c         defined by a "$(", see below.
c      F  Print FDAT(NFDAT), set NFDAT=NFDAT+1, and continue.
c      G  As for F above, except use the last floating format
c         defined by a "$(", see below.
c      M  Print MDAT(NMDAT), set NMDAT=NMDAT+1, and continue.
c      H  Marks terminator for column and row Headings, see table,
c         vector, and matrix output below.  This causes enough blanks to
c         be generated to keep column headings centered over their
c         columns.  After the blanks are generated, text is continued
c         until the next '$'.  This is not to be used except inside
c         column or row headings.  The last row or column should be
c         terminated with a '$E' or if appropriate, a '$#' for a row or
c         column label.
c      (  Starts the definition of a format for integer or floating
c         point output.  The format may not contain a "P" field, and
c         must require no more than 12 characters for floating point
c         (e.g. "(nnEww.ddEe)", where each of the lower case letters
c         represents a single digit), and no more than 7 characters for
c         integer output.  Following the ")" that ends the format, if
c         the next character is not a "$" then "$J" or "$G" type output
c         is done, see above.  In either case processing of TEXT then
c         continues.
c      T  Tab.
c      #  Used in matrix row or column labels this prints the current
c         row or column index, respectively, ends the text for the
c         current row or column, and resets the text pointer to where
c         it started.
c      $  a single '$' is printed, continue till the next '$'.
c      -  Start a negative number for skipping.
c     0-9 Digits for skipping.
c      C  Only used by PMESS which deletes it and the preceding '$'.
c         Used at the end of a line to indicate continued text.
c   other Don't use this -- the '$' is ignored, but new features may
c         change the action.  (E.g. $P might be added to get a prompt.)
c ME????=54  Not used.
c METABL=55  (K55, L55, M55, N55)  Note this action automatically
c            returns when done, further locations in MACT are not
c            examined.  This action prints a heading and/or data that
c            follows a heading.  If K55 is 1, then the heading text
c            starting in TEXT(NTEXT) is printed.  This text
c            should contain embedded "$H"'s to terminate columns of the
c            heading.  If there is no heading on a column, use " $H".
c            Note the leading blank.  If the heading is to continue
c            over k columns, begin the text with "$H" repeated k-1
c            times with no other embedded characters.  The very last
c            column must be terminated with "$E" rather than "$H".
c            After tabular data is printed, K55 is incremented by 1,
c            and compared with L55.  If K55 > L55, K55 is reset to 1,
c            and if the data that was to be printed had lines that were
c            too long, data saved in the scratch file is printed using
c            the headings for the columns that would not fit on the
c            first pass.  Note that only one line of tabular data can
c            be printed on one call to this subroutine.
c            M55 gives the number of columns of data associated with the
c            heading.
c            N55 is a vector containing M55 entries.  The k-th integer
c            in N55 defines the printing action for the k-th column
c            of the table.  Let such an integer have a value defined by
c            rr + 100 * (t + 10 * (dd + 100 * ww)), i.e. wwddtrr, where
c            0 .le. rr,dd,ww < 100, and 0 .le. t < 10.
c      rr    The number of items to print.
c      t     The type of output.
c            1  Print text starting at TEXT(NTEXT), rr = 01.
c            2  Print the value of K55, rr = 01.
c            3  Print integers starting at IDAT(NIDAT).
c            4  Print starting at FDAT(NFDAT), using an F format.
c            5  Print starting at FDAT(NFDAT), using an E format.
c            6  Print starting at FDAT(NFDAT), using an G format.
c      dd    Number of digits after the decimal point.
c      ww    The total number of column positions used by the column,
c            including the space used to separate this column from the
c            preceding one.  This must be big enough so that the column
c            headings will fit without overlap.
c MEIVEC=57  (K57) Print IDAT as a vector with K57 entries.  The vector
c            output starts on the current line even if the current line
c            contains text.  This is useful for labeling the vector.
c            The vector starts at IDAT(NIDAT).
c            If K57 < 0,  indices printed in the labels for the vector
c            start at at NIDAT, and entries from NIDAT to -K57 are
c            printed.
c MEIMAT=58  (K58, L58, M58, I58, J58) Print IDAT as a matrix with K58
c            declared rows, L58 actual rows, and M58 columns.  If K58<0,
c            instead of using 1 for the increment between rows, and K58
c            for the increment between columns, -K58 is used for the
c            increment between rows, and 1 is used for the increment
c            between columns.  If L58<0, the number of actual rows is
c            mod(-L58, 100000), and the starting row index is -L58 /
c            100000.  Similarly for M58<0. TEXT(I58) starts the text for
c            printing row labels.  If I58 < 0, no row labels are
c            printed.  If I58 = 0, it is as if it pointed to text
c            containing "Row $E".  Any "$" in a row or column label must
c            be followed by "H" or "E" which terminates the text for the
c            label.  In the case of $H, text for the next label follows
c            immediately, in the case of $E the current row index is
c            printed in place of the $E and the next label uses the same
c            text.  J58 is treated similarly to I58, except for column
c            labels, and with "Row $E" replaced with "Col $E".  The
c            matrix starts at IDAT(NIDAT), and NIDAT points one past the
c            end of the matrix when finished.
c MEJVEC=59  (K59) As for MEIVEC, except use format set with $(.
c MEJMAT=60  (K60, L60, M60, I60, J60) As for MEIMAT, except use the
c            format set with $(.
c MEFVEC=61  (K61) As for MEIVEC, except print FDAT as a vector with
c            K61 entries.  The vector starts at FDAT(NFDAT).
c MEFMAT=62  (K62, L62, M62, I62, J62) As for action MEIMAT, but
c            instead print FDAT, and use NFDAT in place of NIDAT.
c MEGVEC=63  (K63) As for MEFVEC, except use format set with $(.
c MEGMAT=64  (K64, L64, M64, I64, J64) As for MEIMAT, except use the
c            format set with $(.
c MEIVCI=65  (K65, L65) As for MEIVEC, except the vector entries have a
c            spacing of K65, and there are L65 entries in the vector.
c MEJVCI=66  (K66) As for MEIVCI, except use format set with $(.
c MEFVCI=67  (K67, L67) As for MEFVEC, except the vector entries have a
c            spacing of K67, and there are L67 entries in the vector.
c MEGVCI=68  (K68) As for MEFVCI, except use format set with $(.
c MEFSPV=69  (K69) Output IDAT, FDAT as a sparse vector.
c
c
c ************************** Internal Variables ************************
c
c BUF    Character string holding characters to be output.
c C      Used for temp. storage of a character.
c DOLS   A heading/trailing string of characters, default = $'s.
c ERMSG  Default part of error message.
c ERRCNT Used to keep a count of error messages.
c EUNIT  Unit number for output of error messages.
c FDAT   Formal array, containing floating point data to output.  Only
c   appears external to this subroutine.
c FIRST  Set = .true. initially, then .false. after MESS is called.
c FMTC   Format for integer output of matrix column headings.
c FMTF   Format for floating point or other output.
c FMTG   Format set by user for floating point or other output.
c FMTI   Character string holding format for integer output.
c FMTIM  Equivalenced to FMTR, FMTC.
c FMTJ   Format set by user for integer output.
c FMTR   Value of FMTI for format of row indices.
c FMTT   Format to be stored in FMTJ or FMTG.
c GETW   Set true if still need to get width for a format.
c GOTFMT Set .true. if format has been set by user.  When printing
c   tables, set true when heading has been output.
c I      Index of current action from MACT.
c ICHAR0 Value of ICHAR('0')
c ICOL   Current column index in matrix output.
c IDAT   Formal array, containing integer data to output.
c IMAG   Magnitude of integer to output, with negative sign if integer
c   is < 0.
c INC    Increment between successive elements in a vector or in the
c    column of a matrix.
c INCM   Array giving amount of space used by the options.
c INERR  0 if not processing an error message, 1 if printing an error
c   message, -1 if in an error message that is not being printed, and >1
c   if printing an error message that stops.  Set to -2 when the error
c   message is supposed to stop.
c IOUT   Integer to be output.
c IRC    = 1 for rows, = 2 for columns when determining labels for
c   matrix output.
c IROW   Row index for matrix output.  Also used in table output to
c    count lines for printing line index on read from scratch unit.
c IROW1  Starting row index for matrix output.
c ITEXT  Index of the element of TEXT use for the next text output.
c ITXTSV Saved value of NTEXT when doing matrix output.
c IVAR   Integer array, that is equivalenced to a number of integer
c   variables that can be set by the user.
c IWF    Width to be used in a floating pt. (or other) format.
c IWG    Value like IWF for user set format.
c J      Used as a temporary index.
c JJ      Used as a temporary index.
c K      Used as a temporary index.
c K      Used as a temporary index.
c K1     Used as a temporary index.
c K2     Used as a temporary index.
c KDF    Current number of digits to print for floating point.  See
c   description of MACT(*) = METDIG above.
c KDFDEF Current default for KDF, see description of MACT(*) = MEDDIG.
c KDI    Number of digits used to print last integer.
c KDIAG  Not directly referenced.  Holds place in IVAR for reference
c   from outside.  See comments above.
c KDILAB Length for printing index in vector output.
c KDJ    As for KDI, except for format set by user.
c KK     Temporary index.
c KLINE  Count of number of things to print on a line.  (In table print
c   is the number to print for one spec field.)
c KNT    In vector output gives the current index for output.
c KOLWID Length to use for printing a column heading.  Equivalenced to
c   MAXWID(2).
c KP     Index from error action input for the print action.
c KRES1  Holds place in common block for future use.
c KS     Index from error action input for the stop action.
c KSCRN  Number of lines to "print" before pausing.
c KSHIFT Amount to shift column heading before printing.
c KSPEC  Defines action after looking for character in TEXT.  (Also
c   used as a temporary index.)
c   1.  $B   Break the text here continue on same line.
c   2.  $E   Break text, print what is in BUF.
c   3.  $R   Break text, continue on same line, NTEXT set to repeat the
c            current text.
c   4.  $N   Print BUF, continue with following text.
c   5.  $I   Print IDAT(NIDAT), continue TEXT.
c   6.  $F   Print FDAT(NFDAT), continue TEXT.
c   7.  $M   Print MDAT(NMDAT), continue TEXT.
c   8.  $J   As for $I, but with user format.
c   9.  $G   As for $F, but with user format.
c  10.  $(   Set a user format.
c  11.  $T   Tab.
c  12.       Set when done with an action.
c  13.       Set when done with boiler plate text for an error message.
c   0. Other Ignore the "$", continue with TEXT.
c KT     Used for logic in output of headings.
c        = 1 Output table headings.
c        = 2 Get row/column widths for matrix output.
c        = 3 Output column headings for matrix output.
c LASKNT In vector output value index for last element to print.
c LASTI  Last index for matrix output, or for finding values that
c   determine format.
c LBUF   Position of characters in BUF, usually the last to print.
c LBUF1  Start of text to shift when shifting text in BUF to the left.
c LBUF2  End of text to shift when shifting text in BUF to the left.
c LENBUF Parameter giving the number of character in BUF.
c LENLIN Gives number of character in output lines.
c LENOUT Length of output for table or vector/matrix output.
c LENTXT Length of character array elements in TEXT.
c LENTRY Tells what to do on entry (and sometimes other places.)
c   = 1  The value on first entry.
c   = 2  A previous entry is to be continued.
c   = 3  A non printing error message is to be continued
c   = 4  Just done output from inside a METEXT action.
c   = 5  Got "maximum" value for entries in a vector.
c   = 6  Got "maximum" value for entries in a matrix.
c   = 7  Vector (either print or get format for label indices.)
c   = 8  Matrix (either print or get format for label indices.)
c   = 9  Output of data in a table.
c   =10  Get "maximum" valur for entries in a sparse vector.
c   =11  Output a sparse vector.
c LHEAD  If = 0 no print of DOLS, else DOLS printed in error messages.
c LINERR Gives LENLIN for error messages.
c LINMSG Gives LENLIN for diagnostic messages.
c LINSTR Space at start of line for label in vector and matrix output.
c   Equivalenced to MAXWID(1).
c LNERR  Parameter giving the default value for LENERR, only in XMESS.
c LNMSG  Parameter giving the default value for LINMSG, only in XMESS.
c LOCBEG Index of first item in vector and matrix output.
c LPRINT For error messages with a print level .le. LPRINT nothing is
c   printed (unless the message would result in a stop).
c LSTOP  As for LPRINT, except with the stop level, and stopping.
c LSTRT  Starting character position in BUF for storing next characters.
c LTEXT  Length of heading text in TEXT.
c M      Index for the current action.
c MACT   Formal integer array specifying the actions, see above.
c MAXERR Value save in IVAR for user to get indication of last (really
c   the maximum error seen so far = 1000 * (10*stop + print) + index.
c MAXWID Equivalenced to  LINSTR and KOLWID.
c MBNDHI Upper bounds for inputs to IVAR.
c MBNDLO Lower bounds for inputs to IVAR.
c MDAT   Array where user can store integers in IVAR for later output.
c   Also used to store column indices when tables are saved on scratch
c   unit.
c
c The following parameter names starting with ME define actions
c   which the user can request.  They have been documented in the
c   description above, except for the ones defined just below.
c MEGBAS is 1 less than the smallest action that involves something
c   other than just storing or retrieving a value.
c MEMAXI is the largest action which a user can request.
c MEVBAS is the smallest action index, used to set the starting index in
c   IVAR.
c MEVLAS is the largest index for a variable in IVAR.
c MECONT,  MEDDI,  MEELI,  MEEME, MEEUNI, MEFDAT, MEFMAT, MEFSPV,
c MEFVCI, MEFVEC, MEGBAS, MEGMAT, MEGVCI, MEGVEC, MEHEAD, MEIDAT,
c MEIMAT, MEIVCI, MEIVEC, MEJMAT, MEJVCI, MEJVEC, MEMAXE, MEMAXI,
c MEMDA1, MEMDA2, MEMDA3, MEMDA4, MEMDA5, MEMDAT, MEMLIN, MEMUNI,
c MENTXT, MEPRNT, MESCRN, MERES1, MERES2, MERES3,  MERET, MESTOP,
c MESUNI,  METAB, METDIG, METEXT
c MPT  Current pointer to data for matrix or vector output.
c MTEXT  Equivalenced to MTEXTR and MTEXTC.
c MTEXTC TEXT(MTEXTC) starts text for printing column labels.
c MTEXTR TEXT(MTEXTR) starts text for printing row labels.
c MUNIT  Output unit used for messages that aren't in an error message.
c NCOL   Number of columns for matrix output, 0 for vector output,
c   count of column left for table output.
c NDIM   Distance between columns for matrix output.
c NFDAT  Index of next item in FDAT to print.
c NIDAT  Index of next item in IDAT to print.
c NLINE  Maximum number of data items to print on a line for matrix and
c   vector output.  If the scratch file is used for table output,
c   NLINE gives the original end of the buffer.
c NMDAT  Pointer to next thing to print from MDAT.
c NROCO  Equivalenced to (NROW, NCOL).  Used in matrix output.
c NROW   Number of rows for matrix output.    When printing tables,
c   MDAT(NROW) gives place where line was split.  (=0 if not split)
c NSKIP  The amount to skip ahead on the next floating or integer
c   output.
c NTEXT  Index inside an element of TEXT for the next text output.
c NTEXTR Value of NTEXT to use if get a $R.
c NTXTSV Saved value of NTEXT when doing matrix output.
c OUNIT  Index of the current output unit.
c SC     Parameter for special character used to introduce actions.
c   Default value is '$'.  If this is changed the "$"'s in comments
c   should be changed to the new value of the character.  (Note that
c   SC = '\' is not portable.)
c SCRNAM Name of file constructed for error output or message output.
c SUNIT  Index for the scratch unit, -1 if not yet assigned.
c TEXT   Formal argument giving the character string from which all text
c   is taken.
c UMESS  Name of subroutine called that does nothing, but which may be
c   modified by the user to cause different actions to be taken.
c   The usual version of MESS has the call to UMESS commented out.
c XARG   If .true., output data is not integer, and a return is made to
c   print data from FDAT.
c XARGOK Set .true. if call is from program that will print data from
c   FDAT.
c
c++ CODE for .C. is inactive
c      integer  kciwid, kccwid, kcrwid, lbeg, lend, lfprec, lgprec
c      common /MESSCC/ kciwid,kccwid,kcrwid,lbeg,lend,lfprec,lgprec
C%%    long int kc;
c++ END
      integer LNMSG, LNERR
      parameter (LNMSG=128)
      parameter (LNERR=79)
c
c ************** Parameters Defining Actions (See Above) ***************
c
      integer   MESUNI, MEHEAD, MEDDIG, MEMLIN, MEELIN, MEMUNI, MEEUNI,
     1  MESCRN, MEDIAG, MEMAXE, MESTOP, MEPRNT, METDIG, MENTXT, MEIDAT,
     2  MEFDAT, MEMDAT, MEMDA1, MEMDA2, MEMDA3, MEMDA4, MEMDA5, METABS,
     3  MEERRS, MECONT, MERET , MEEMES, METEXT, METABL, MERES3, MEIVCI,
     4  MEIVEC, MEIMAT, MEJVCI, MEJVEC, MEJMAT, MEFVCI, MEFVEC, MEFMAT,
     5  MEGVCI, MEGVEC, MEGMAT, MEMAXI, MEGBAS, MEVBAS, MEVLAS, MEFSPV
c Parameters for changing the environment.
      parameter (MESUNI=10,MEHEAD=11,MEDDIG=12,MEMLIN=13,MEELIN=14,
     1 MEMUNI=15,MEEUNI=16,MESCRN=17,MEDIAG=18,MEMAXE=19,MESTOP=20,
     2 MEPRNT=21,METDIG=22,MENTXT=23,MEIDAT=24,MEFDAT=25,MEMDAT=26,
     3 MEMDA1=27,MEMDA2=28,MEMDA3=29,MEMDA4=30,MEMDA5=31,METABS=32,
     4 MEERRS=33)
c Parameters for actions.
      parameter (MECONT=50, MERET=51,MEEMES=52,METEXT=53,MEFSPV=54,
     1 METABL=55,MERES3=56,MEIVEC=57,MEIMAT=58,MEJVEC=59,MEJMAT=60,
     2 MEFVEC=61,MEFMAT=62,MEGVEC=63,MEGMAT=64,MEIVCI=65,MEJVCI=66,
     2 MEFVCI=67,MEGVCI=68)
c Parameter derived from those above.
      parameter (MEMAXI=68,MEGBAS=49,MEVBAS=10,MEVLAS=33)
c
c ************************** Variable Declarations *********************
c
      external MESSGS
      integer    MACT(*), IDAT(*)
      character  TEXT(*)*(*)
c
      integer    I, ICOL, INCM(MECONT:MEIMAT), INERR, IOUT, IROW, IROW1,
     1    ITEXTR, ITXTSV, J, JJ, K, K1, K2, KDILAB, KK, KNT, KOLWID, KP,
     2    KS, LASKNT, LBUF1, LBUF2, LENBUF, LINSTR, M,
     3    MBNDHI(MEVBAS:MEVLAS), MBNDLO(MEVBAS:MEVLAS), MTEXT(2),
     4    MTEXTC, MTEXTR, NLINE, NROCO(2), NSKIP, NTEXTR, NTXTSV
      integer MESSGS
      logical   GETW, FIRST
      character ERMSG*63, ERMSG1*27
      character SC, C
      parameter (SC='$')
      save  FIRST, I, ICOL, INERR, IROW, IROW1, ITXTSV, KDILAB, KNT,
     1   LASKNT, M, MTEXT, NLINE, NSKIP, NTEXTR, NTXTSV
      save /CMESSI/, /CMESSC/
      equivalence (MTEXT(1), MTEXTR), (MTEXT(2), MTEXTC)
c
c ************************** Data from common block ********************
c
      parameter (LENBUF=250)
      logical          XARG, GOTFMT, XARGOK
      integer          ERRCNT, EUNIT, ICHAR0, IRC, IVAR(MEVBAS:MEVLAS),
     1   IMAG, INC, ITEXT, IWF, IWG, KDF, KDFDEF, KDI, KDIAG, KDJ,
     2   KLINE, KSCRN, KSHIFT, KSPEC, KT, MAXERR, LASTI, LBUF, LENLIN,
     3   LENOUT, LENTRY, LENTXT, LHEAD, LINERR, LINMSG, LOCBEG, LPRINT,
     4   LSTOP, LSTRT, LTEXT, MAXWID(2), MDAT(5), MPT, MUNIT, NCOL,
     5   NDIM, NFDAT, NIDAT, NMDAT, NROW, NTEXT, OUNIT, SUNIT, TABSPA
c
      character BUF*(LENBUF), DOLS*72, FMTC*7, FMTF*20, FMTG*15,
     1  FMTI*7, FMTIM(2)*7, FMTJ*7, FMTR*7, FMTT*15
      common /CMESSI/ SUNIT, LHEAD, KDFDEF, LINMSG, LINERR, MUNIT,
     1   EUNIT, KSCRN, KDIAG, MAXERR, LSTOP, LPRINT, KDF, NTEXT, NIDAT,
     2   NFDAT, NMDAT, MDAT, TABSPA, ERRCNT, ICHAR0, IMAG, INC, IRC,
     3   ITEXT, IWF, IWG, KDI, KDJ, KLINE, KSHIFT, KSPEC, KT, LASTI,
     4   LBUF, LENLIN, LENOUT, LENTRY, LENTXT, LOCBEG, LSTRT, LTEXT,
     5   MAXWID, MPT, NROW, NCOL, NDIM, OUNIT, GOTFMT, XARG, XARGOK
      common /CMESSC / BUF, DOLS, FMTF, FMTG, FMTI, FMTJ, FMTT, FMTIM
      equivalence (IVAR(MEVBAS), SUNIT)
      equivalence (FMTIM(1), FMTR), (FMTIM(2), FMTC)
c
      equivalence (NROCO, NROW)
      equivalence (MAXWID(1), LINSTR), (MAXWID(2), KOLWID)
c ************************** End of stuff from common block ************
c
      data INERR, FIRST / 0, .true. /
      data ERMSG /
     1' reports error: Stop level = x, Print level = y, Error index = '/
      data ERMSG1 / ': Print level = y, Index = ' /
c                 50  51, 52  53  54 55 56 57 58
      data INCM /  1,  1,  4,  1,  2, 0, 0, 2, 6 /
      data MBNDLO /  0, 0, -50,  39,  39, -99, -99,         0,
     1       0,          0, 0, 0, -50,        1,          1,
     2       1, 1, -1000000000, -1000000000, -1000000000, -1000000000,
     3       -1000000000,   1,  0 /
      data MBNDHI / 99, 1,  50, 500, 500,  99,  99, 100000000,
     1  1000000000, 1000000000, 8, 8,  50, 10000000, 1000000000,
     2  1000000000, 5, 1000000000, 1000000000, 1000000000, 1000000000,
     3  1000000000, 100, 1000000000 /
c
c ************************* Start of Executable Code *******************
c
c
      NSKIP = 0
      if (FIRST) then
         FIRST = .false.
c Initialize common block
         SUNIT = -1
         LHEAD = 1
         LINMSG = LNMSG
         LINERR = LNERR
         MUNIT = 0
         EUNIT = 0
         KSCRN = 0
         MAXERR = 0
         TABSPA = 6
         LSTOP = 3
         LPRINT = 3
         ERRCNT = 0
         ICHAR0 = ICHAR('0')
         KDI = 1
         KDJ = 6
         LENLIN = LNMSG
         LENTRY = 1
         OUNIT = 0
c++ CODE for ~.C. is active
         DOLS(1:40) = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
         DOLS(41:72) ='$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
         FMTI = '(99I01)'
         FMTJ = '(99I06)'
         FMTG = '(1P,99Exx.xx)  '
c++ CODE for .C. is inactive
C%%    memset(cmessc.dols,'$',72);
C      FMTI = '%*d'
C      FMTJ = '%*d\0'
C      FMTG = '%*.*E\0'
c++ END
      else
c               1  2  3   4    5    6    7    8   9   10   11
         go to (5,10,20,850,1160,1620,1130,1530,960,1210,1220), LENTRY
      end if
c                             First entry for a message
    5 LBUF = 0
c                             Usual continuation entry
   10 I = 1
      NTEXT = 1
      ITEXT = 1
      LENTXT = len(TEXT(1))
      NIDAT = 1
      NFDAT = 1
      NMDAT = 1
      go to 120
c                     Continuation entry when have a non printing error
c Skip all actions -- Inside non-printing error message.
   20 I = 1
   30 K = MACT(I)
      if (K .le. MERET) then
         if (K .eq. MERET) go to 120
         if (K .eq. MECONT) return
         if (K .le. -MEGBAS) go to 180
         I = I + 2
      else
         if (K .gt. MEIMAT) then
            if (K .gt. MEMAXI) go to 180
            K = MEIVEC + mod(K - MEIVEC, 2)
         end if
         I = I + INCM(K)
      end if
      go to 30
c
c Print BUF
   40 call MESSPR
c                             Usual place to end an action request.
  100 I = I + INCM(M)
c                             Pick up the next action request
  120 M = MACT(I)
      if (M .gt. MEGBAS) go to 140
      I = I + 2
      if (abs(M) .gt. MEVLAS) go to 180
      if (M .gt. 0) then
         IVAR(M) = MACT(I-1)
         if (IVAR(M) .lt. MBNDLO(M)) then
            IVAR(M) = MBNDLO(M)
         else if (IVAR(M) .gt. MBNDHI(M)) then
            IVAR(M) = MBNDHI(M)
         end if
c            MEHEAD, MEDDIG, MEMLIN, MEELIN, MEMUNI, MEEUNI
         go to (122,    124,    126,    126,    128,    128), M - MESUNI
         if (M .ne. MENTXT) go to 120
         ITEXT = (NTEXT-1) / LENTXT
         NTEXT = NTEXT - LENTXT*ITEXT
         ITEXT = ITEXT + 1
         go to 120
  122    if (LHEAD .ne. 0) then
         end if
         go to 120
  124    KDF = KDFDEF
         go to 120
  126    LENLIN = LINMSG
         go to 120
  128    if (IVAR(M) .ne. 0) then
C%%          k = labs(cmessi.ounit);
C%%          c_fname[m-15][6] = k / 10 + '0';
C%%          c_fname[m-15][7] = k % 10 + '0';
C%%          if (strcmp(&c_fname[16-m][6], &c_fname[m-15][6]))
C%%             c_handle[m-15] = fopen(c_fname[m-15],"w");
C%%          else
C%%             c_handle[m-15] = c_handle[16-m];
            K = abs(IVAR(M))
         end if
         OUNIT = MUNIT
         go to 120
      end if
      if (M .eq. -MESUNI) then
C%%      if (cmessi.sunit == -1L) {
C%%          scratch_file = tmpfile();
C%%          cmessi.sunit = 1L;}
         if (SUNIT .le. 0) SUNIT = MESSGS()
      end if
C
      MACT(I-1) = IVAR(-M)
      go to 120
c  ME ..    CONT  RET EMES ETXT  FSPV TABL
  140 go to (170, 200, 310, 400, 1200, 910, 180), M-MEGBAS
      if (M .le. MEGVCI) go to 1000
      go to 180
c
c Action MECONT -- Continue message on next entry
  170 LENTRY = 2
      return
c
c Some kind of error in message specification.
  180 continue
c++ CODE for ~.C. is active
      BUF(1:57) =
     1   'Actions in MESS terminated due to error in usage of MESS.'
c++ CODE for .C. is inactive
C%%   memcpy(cmessc.buf,
C%%   "Actions in MESS terminated due to error in usage of MESS.",57);
c++ END
      LBUF = 57
c
c Action MERET -- Finish a message.
  200 LENTRY = 1
      J = INERR
      INERR = 0
      if (J .ge. 2) INERR = -2
      if (J .gt. 0) go to 330
c                       Finish print before exit.
      call MESSPR
      return
c
c Action MEEMES -- Start an error message
  310 LENTRY = 3
      ERRCNT = ERRCNT + 1
c++  Code for UMESS is inactive
C      call UMESS(TEXT, MACT(I+1), IVAR)
c++  End
      IMAG = max( 0, min(999, MACT(I+2)))
      K = MACT(I+1)
      MAXERR = max(MAXERR, 1000*K + IMAG)
      KS = K / 10
      KP = K - 10 * KS
      if (KS .le. min(LSTOP, 8)) then
         if (KP .le. LPRINT) then
            INERR = -1
            go to 20
         end if
         INERR = 1
      else
         INERR = 2
      end if
      OUNIT = EUNIT
      LENLIN = LINERR
c                        Output a blank line.
      BUF(1:1) = ' '
      LBUF = 1
  330 call MESSPR
c                        Put out line of $'s
      if (LHEAD .ne. 0) then
         LBUF = min(len(DOLS), LENLIN)
c++ CODE for ~.C. is active
         BUF(1:LBUF) = DOLS(1:LBUF)
         if (INERR.lt.0) BUF(5:37)=' Fatal error -- Program stopped. '
c++ CODE for .C. is inactive
C%%      memcpy(cmessc.buf, cmessc.dols, cmessi.lbuf);
C%%      if (inerr < 0L)
C%%      memcpy(&cmessc.buf[4]," Fatal error -- Program stopped. ",34);
c++ END
         call MESSPR
      end if
      if (INERR .le. 0) then
c                                 Just finished an error message
         if (INERR .ne. 0) stop
         OUNIT = MUNIT
         LENLIN = LINMSG
         return
      end if
c                     Just starting an error message get program name
      NTEXTR = 0
      go to 410
c                     Got the program name in BUF.
  370 LBUF = min(LBUF, 40)
      if (KS .eq. 0) then
         ERMSG1(17:17) = char(KP + ICHAR0)
C%%       memcpy(&cmessc.buf[cmessi.lbuf], ermsg1, strlen(ermsg1));
         BUF(LBUF+1:LBUF+len(ERMSG1)) = ERMSG1
         LBUF = LBUF + len(ERMSG1)
      else
         ERMSG(30:30) = char(KS + ICHAR0)
         ERMSG(47:47) = char(KP + ICHAR0)
C%%       memcpy(&cmessc.buf[cmessi.lbuf], ermsg, strlen(ermsg));
         BUF(LBUF+1:LBUF+len(ERMSG)) = ERMSG
         LBUF = LBUF + len(ERMSG)
      end if
      LSTRT = LBUF + 1
      call MESSFI
      LBUF = LBUF + KDI
C%%   sprintf(&cmessc.buf[cmessi.lstrt-1L], "%*ld",
C%%           (int)messcc.kciwid, cmessi.imag);
      write (BUF(LSTRT:LBUF), FMTI) IMAG
c          Finish up the start error message action.
      if (MACT(I+3) .lt. 0) go to 40
      if (MACT(I+3) .ne. 0) then
         ITEXT = (MACT(I+3)-1) / LENTXT
         NTEXT = MACT(I+3) - LENTXT*ITEXT
         ITEXT = ITEXT + 1
      end if
      KSPEC = 13
      go to 480
c                  Take care of any left over print from error header
  390 if (LBUF .ne. 0) call MESSPR
c
c Action METEXT -- Print string from TEXT
  400 LENTRY = 4
      NTEXTR = NTEXT
      ITEXTR = ITEXT
c                  Continue with print from TEXT
c K     take at most K-1 chars., but if 0 take max number
c K1    is last loc. used from TEXT if LENTXT is BIG.
c NEXT  is first character location in TEXT(ITEXT)
c K2    is last character location in TEXT(ITEXT)
c LSTRT is first character position in BUF
c LBUF  is last used character position in BUF

 410  LSTRT = LBUF + 1
      K2 = min(LENTXT, NTEXT + (LENBUF - LSTRT))
C%%       if ((ctmp=memchr(TEXT(cmessi.itext-1L,cmessi.ntext-1), SC,
C%%          k2 - cmessi.ntext + 1)) == NULL)
C%%             k = 0;
C%%       else
C%%             k = ctmp - TEXT(cmessi.itext-1L,cmessi.ntext-1) + 1;
      K = index(TEXT(ITEXT)(NTEXT:K2), SC)
      if (K .eq. 0) then
c Want to take all that we can.
         LBUF = LSTRT + K2 - NTEXT
C%%       memcpy(&cmessc.buf[cmessi.lstrt-1L], TEXT(cmessi.itext-1L,
C%%         cmessi.ntext-1), k2 - cmessi.ntext + 1L);
         BUF(LSTRT:LBUF) = TEXT(ITEXT)(NTEXT:K2)
         if (K2 .eq. LENTXT) then
           ITEXT = ITEXT + 1
           NTEXT = 1
           if (LBUF .le. LENLIN) go to 410
         else
           NTEXT = K2 + 1
         end if
         KSPEC = 12
         if (ITEXT - ITEXTR .lt. 4000) go to 480
         KSPEC = 2
         go to 430
      end if
      LBUF = LBUF + K - 1
C%%   if (k >= 2) memcpy(&cmessc.buf[cmessi.lstrt-1],
C%%     TEXT(cmessi.itext-1L, cmessi.ntext-1), k - 1L);
      if (K .ge. 2) BUF(LSTRT:LBUF) = TEXT(ITEXT)(NTEXT:NTEXT+K-2)
c        Jump to location below if get $ after computing an NSKIP.
  415 continue
      NTEXT = NTEXT + K + 1
      if (NTEXT .gt. LENTXT) then
         ITEXT = ITEXT + 1
         if (NTEXT .eq. LENTXT + 1) then
            C = TEXT(ITEXT-1)(LENTXT:LENTXT)
            NTEXT = 1
         else
            C = TEXT(ITEXT)(1:1)
            NTEXT = 2
         end if
      else
         C = TEXT(ITEXT)(NTEXT-1:NTEXT-1)
      end if
      if (C .eq. ' ') then
c                Special code to take care of " " following "$".
         NTEXT = NTEXT - 1
         if (NTEXT .eq. 0) then
            ITEXT = ITEXT - 1
            NTEXT = LENTXT
         end if
         go to 410
      end if
      if (NTEXTR .eq. 0) then
         if (LENTRY .eq. 3) go to 370
         go to 1510
      end if
      KSPEC = index('BERNIMFJG(T', C)
  430 if (LBUF .gt. LENLIN) go to 480
c              1   2   3   4   5   6   7   8   9  10  11  12, 13
c              B   E   R   N   I   M   F   J   G   (   T done end err
      go to (455,480,450,460,700,680,900,700,900,600,690,410,390), KSPEC
c               No match  -- Check for setting NSKIP
      if (((C .ge. '0') .and. (C .le. '9')) .or. (C .eq. '-')) then
         NSKIP = 0
         K1 = 1
         if (C .ne. '-') go to 436
         K1 = -1
  433    C = TEXT(ITEXT)(NTEXT:NTEXT)
         NTEXT = NTEXT + 1
         if (NTEXT .ge. LENTXT) then
            ITEXT = ITEXT + 1
            NTEXT = 1
         end if
  436    if ((C .ge. '0') .and. (C .le. '9')) then
            NSKIP = 10 * NSKIP + K1 * (ICHAR(C) - ICHAR0)
            go to 433
         end if
         if (C .eq. '$') then
            K = 0
            go to 415
         end if
      end if
c
c Continue with the text.
  440 LBUF = LBUF + 1
      BUF(LBUF:LBUF) = C
      go to 410
c                        Reset NTEXT for $R
  450 NTEXT = NTEXTR
      ITEXT = ITEXTR
c                             Done with METEXT action.
  455 NMDAT = 1
      go to 100
c           At this point want to output all in BUF
  460 do 470 LBUF = LBUF, 1, -1
         if (BUF(LBUF:LBUF) .ne. ' ') go to 480
  470 continue
  480 LBUF2 = LBUF
      if (LBUF2 .eq. 0) then
         LBUF = 1
         BUF(1:1) = ' '
      else if (LBUF .gt. LENLIN) then
         do 485 K = LENLIN+1, LENLIN/3, -1
            if (BUF(K:K) .eq. ' ') then
               LBUF = K - 1
               go to 490
            end if
  485    continue
         LBUF = LENLIN
      end if
  490 LBUF1 = LBUF
      call MESSPR
      if (LBUF1 .ge. LBUF2) then
c                       The entire buffer has been printed.
         if (KSPEC .le. 2) go to 455
         if (KSPEC .ne. 4) go to 430
         go to 410
      end if
c                       Remove trailing blanks
      do 510 LBUF1 = LBUF1+1, LBUF2
         if (BUF(LBUF1:LBUF1) .ne. ' ') go to 520
  510 continue
c                       Shift the contents of the buffer.
  520 LBUF = LBUF2-LBUF1+1
      LSTRT = 1
  530 if (LBUF .ge. LBUF1) then
c                              Take care of overlap.
         K = 2*LBUF1 - LSTRT
C%%memcpy(&cmessc.buf[cmessi.lstrt-1],&cmessc.buf[lbuf1-1],k-lbuf1);
         BUF(LSTRT:LBUF1-1) = BUF(LBUF1:K-1)
         LSTRT = LBUF1
         LBUF1 = K
         go to 530
      end if
C%% if (cmessi.lbuf>=cmessi.lstrt) memcpy(&cmessc.buf[cmessi.lstrt-1],
C%%       &cmessc.buf[lbuf1-1L], lbuf2-lbuf1+1);
      if (LBUF .ge. LSTRT) BUF(LSTRT:LBUF) = BUF(LBUF1:LBUF2)
      go to 430
c
c Get information on user format
  600 KSPEC = 8
c              I,   i,   F,   f,   E,   e,   G,   g
      go to (604, 604, 601, 601, 602, 602, 602, 602),
     1   index('IiFfEeGg',TEXT(ITEXT)(NTEXT:NTEXT))
      go to 180
  601 continue
c++ CODE for ~.C. is active
      FMTG='(0P,99F'
c++ CODE for .C. is inactive
C%%   strcpy(cmessc.fmtg, "%*.*f\0");
C%%   messcc.lgprec = 0;
c++ END
      go to 603
  602 continue
c++ CODE for ~.C. is active
      FMTG='(1P,99'//TEXT(ITEXT)(NTEXT:NTEXT)
c++ CODE for .C. is inactive
C%%   strcpy(cmessc.fmtg, "%*.*E\0");
C      FMTG(5:5) = TEXT(ITEXT)(NTEXT:NTEXT)
C%%   messcc.lgprec = 0;
c++ END
  603 KSPEC = 9
  604 IMAG = 0
      GETW = .true.
      K = NTEXT
      FMTT = ' '
  606 continue
         NTEXT = NTEXT + 1
         if (NTEXT .gt. LENTXT) then
            ITEXT = ITEXT + 1
            NTEXT = 1
         end if
c++ CODE for ~.C. is active
         FMTT(NTEXT-K:NTEXT-K) = TEXT(ITEXT)(NTEXT:NTEXT)
c++ END
         JJ = ichar(TEXT(ITEXT)(NTEXT:NTEXT)) - ICHAR0
         if (GETW) then
            if ((JJ .ge. 0) .and. (JJ .le. 9)) then
               IMAG = 10*IMAG + JJ
            else
               if (TEXT(ITEXT)(NTEXT:NTEXT) .eq. ')')  go to 610
               if (TEXT(ITEXT)(NTEXT:NTEXT) .ne. '.')  go to 180
               GETW = .false.
            end if
         else
            if (TEXT(ITEXT)(NTEXT:NTEXT) .eq. ')') go to 610
            if ((JJ .lt. 0) .or. (JJ .gt. 9)) go to 180
c++ CODE for .C. is inactive
C%%         messcc.lgprec = 10*messcc.lgprec + jj;
c++ END
         end if
      go to 606
c
  610 NTEXT = NTEXT + 1
      if (NTEXT .gt. LENTXT) then
         ITEXT = ITEXT + 1
         NTEXT = 1
      end if
c++ CODE for ~.C. is active
      if (KSPEC .eq. 8) then
         KDJ = IMAG
         FMTJ(5:7) = FMTT
      else
         IWG = IMAG
         FMTG(8:15) = FMTT
      end if
c++ CODE for .C. is inactive
C%%   if (cmessi.kspec == 8)
C%%       cmessi.kdj = cmessi.imag;
C%%   else
C%%       cmessi.iwg = cmessi.imag;
c++ END
      if (TEXT(ITEXT)(NTEXT:NTEXT) .eq. SC) go to 410
      if (KSPEC .eq. 8) go to 700
      if (XARGOK) return
      go to 440
c
c                         Print from MDAT
  680 IOUT = MDAT(NMDAT)
      if (NMDAT .ge. 6) then
         MDAT(NMDAT) = MDAT(NMDAT) + 1
      else
         NMDAT = NMDAT + 1
      end if
      go to 720
c
c                         Process a tab
  690 LSTRT = LBUF + 1
      LBUF = min(LBUF + TABSPA - mod(LBUF, TABSPA), LENLIN+1)
C%%  for (kc=cmessi.lstrt-1; kc<cmessi.lbuf; kc++) cmessc.buf[kc]=' ';
      BUF(LSTRT:LBUF) = ' '
      go to 850
c                         Print from IDAT
  700 NIDAT = NIDAT + NSKIP
      NSKIP = 0
      IOUT = IDAT(NIDAT)
      NIDAT = NIDAT + 1
  720 LSTRT = LBUF + 1
      IMAG = IOUT
      if (KSPEC .ge. 8) then
         LBUF = LBUF + KDJ
C%%   sprintf(&cmessc.buf[cmessi.lstrt-1],"%*ld",(int)cmessi.kdj, iout);
      write (BUF(LSTRT:LBUF), FMTJ) IOUT
         go to 850
      end if
c
c                Get format for integer output.
      call MESSFI
      LBUF = LBUF + KDI
C%% sprintf(&cmessc.buf[cmessi.lstrt-1],"%*ld",(int)messcc.kciwid,iout);
      write (BUF(LSTRT:LBUF), FMTI) IOUT
c                         Entry here to check line after numeric output.
  850 if (LBUF .le. LENLIN) go to 410
      KSPEC = 12
      go to 480
c
c                          Take care of output for extra argument.
  900 if (XARGOK) return
      go to 180
c
c Action METABL -- Start a table
  910 GOTFMT = MACT(I+1) .ne. 1
      if (.not. GOTFMT) then
         IROW = 0
         KOLWID = 0
      end if
      LENTRY = 9
      if (LBUF .ne. 0) call MESSPR
  920 continue
C%%   memset(cmessc.buf,' ',LENBUF);
      BUF = ' '
      NROW = 1
      NCOL = MACT(I+3)
      ICOL = I + 3
  940 ICOL = ICOL + 1
      JJ = MACT(ICOL)
      KLINE = mod(JJ, 100)
      LENOUT = JJ / 100000
      NCOL = NCOL - max(KLINE, 1)
      if (GOTFMT) then
c                                 Print the data
         LSTRT = LBUF + 1
         LBUF = min(LBUF + KLINE * LENOUT, LENBUF)
         JJ = JJ / 100
         KK = mod(JJ, 10)
c              Text,   I   I',   F    E    G
         go to (948, 941, 941, 943, 945, 944), KK
         go to 180
c                             Integer output
  941    continue
c++ CODE for ~.C. is active
         KDI = LENOUT
         FMTI(5:5) = char(LENOUT / 10 + ichar0)
         FMTI(6:6) = char(mod(LENOUT, 10) + ichar0)
c++ END
         if (KK .eq. 3) then
C%%         sprintf(&cmessc.buf[cmessi.lstrt-1], "%*ld",
C%%            (int)cmessi.lenout, mact[i]);
            write (BUF(LSTRT:LBUF), FMTI) MACT(I+1)
            go to 960
         end if
c                            Regular integer output
         NIDAT = NIDAT + NSKIP
         NSKIP = 0
c++ CODE for ~.C. is active
         write (BUF(LSTRT:LBUF), FMTI) (IDAT(K), K = NIDAT,
     1      NIDAT+KLINE-1)
         NIDAT = NIDAT + KLINE
c++ CODE for .C. is inactive
C%%  kk = cmessi.nidat;
C%%  for (cmessi.nidat=kk; cmessi.nidat<kk+cmessi.kline; cmessi.nidat++)
C%%     sprintf(&cmessc.buf[cmessi.lstrt+cmessi.lenout*(cmessi.nidat
C%%       - kk) - 1], "%*ld", (int)cmessi.lenout, idat[cmessi.nidat-1]);
c++ END
         go to 960
c                           Various floating point output
  943    continue
c++ CODE for ~.C. is active
         FMTF = '(0P,99F  .  )'
c++ END
         go to 946
  944    continue
c++ CODE for ~.C. is active
         FMTF = '(1P,99G  .  )'
c++ END
         go to 946
  945    continue
c++ CODE for ~.C. is active
         FMTF = '(1P,99E  .  )'
c++ END
  946    JJ = mod(JJ/10, 100)
c++ CODE for ~.C. is active
         FMTF(8:8) = char(ICHAR0 + LENOUT / 10)
         FMTF(9:9) = char(ICHAR0 + mod(LENOUT, 10))
         FMTF(11:11) = char(ICHAR0 + JJ / 10)
         FMTF(12:12) = char(ICHAR0 + mod(JJ, 10))
c++ CODE for .C. is inactive
C%%      strcpy(cmessc.fmtf, "%*.*E\0");
C        IWF = LENOUT
C        lfprec = JJ
c++ END
         if (.not. XARGOK) go to 180
         MPT = NFDAT
         NFDAT = NFDAT + KLINE
         return
c                           Text output
  948    K1 = NTEXT + LBUF - LSTRT
C%%    memcpy(&cmessc.buf[cmessi.lstrt-1], TEXT(cmessi.itext-1,
C%%       cmessi.ntext -1), k1 - cmessi.ntext);
         BUF(LSTRT:LBUF) = TEXT(ITEXT)(NTEXT:K1-1)
         NTEXT = K1
      else
c                                 Print the heading
         KT = 1
         call MESSMH(TEXT)
         if (KT .lt. 0) go to 180
      end if
  960 if ((LBUF .le. MDAT(NROW)) .and. (NCOL .gt. 0)) go to 940
      if (NROW .eq. 1) then
         JJ = LBUF
         LBUF = MDAT(1)
         call MESSPR
         LBUF = JJ
      else
         if (IROW .eq. 0) then
            if (NROW .eq. 2) then
c++ CODE for ~.C. is active
               if (SUNIT .le. 0) SUNIT = MESSGS()
               rewind(SUNIT)
c++ CODE for .C. is inactive
C%%        if (cmessi.sunit == -1) {
C%%           scratch_file = tmpfile();
C%%           cmessi.sunit = 1;}
C%%        rewind(scratch_file);
c++ END
            end if
         end if
C%%       fwrite(&cmessc.buf[4], cmessi.mdat[cmessi.nrow-1]-4, 1,
C%%          scratch_file);
         write(SUNIT) BUF(5:MDAT(NROW))
      end if
      if (LBUF .gt. MDAT(NROW)) then
C%%  memcpy(&cmessc.buf[4], &cmessc.buf[cmessi.mdat[cmessi.nrow-1]],
C%%     cmessi.lbuf - cmessi.mdat[cmessi.nrow-1]);
         BUF(5:LBUF - MDAT(NROW) + 4) = BUF(MDAT(NROW)+1:LBUF)
         LBUF = LBUF - MDAT(NROW) + 4
         NROW = NROW + 1
         if (.not. GOTFMT) then
            if (NROW .gt. 5) go to 180
            MDAT(NROW) = LBUF
         end if
         if (NCOL .eq. 0) go to 960
         go to 940
      end if
      LBUF = 0
      if (.not. GOTFMT) then
         GOTFMT = .true.
         IROW = IROW - 1
         go to 920
      end if
      MACT(I+1) = MACT(I+1) + 1
      if (MACT(I+1) .le. MACT(I+2)) go to 999
      MACT(I+1) = 1
      if (NROW .eq. 1) go to 999
C%%    fputc(EOF, scratch_file);
      endfile SUNIT
      KK = 1
  994 KK = KK + 1
      if (KK .gt. NROW) go to 999
C%%   rewind(scratch_file);
      rewind(SUNIT)
      IROW = -1
      K = KK
  995 LBUF = 5
      IROW = IROW + 1
      if (IROW .ne. 0) then
C%%      sprintf(cmessc.buf, "%4ld",  irow%10000);
         write(BUF(1:4), '(I4)') mod(IROW, 10000)
      else
C%%    memset(cmessc.buf,' ',4);
         BUF(1:4) = ' '
      end if
      do 996 J = 2, K
         if (J .eq. K) LBUF = MDAT(KK)
C%%       if (fread(&cmessc.buf[4], cmessi.lbuf-4, 1,
C%%         scratch_file) == 0) goto L_994;
         read(SUNIT, END = 994) BUF(5:LBUF)
  996 continue
      K = NROW
      call MESSPR
      go to 995
  999 LENTRY = 1
      return
c
c                          Get started with vector or matrix output
 1000 INC = 1
      LOCBEG = NIDAT
      if (M .gt. MEGMAT) then
c Have a user set increment between entries of a vector.
        M = MEIVEC + 2 * (M - MEIVCI)
        I = I + 1
        INC = MACT(I)
      end if
      XARG = M .gt. MEJMAT
      if (XARG) then
         M = M - 4
         LOCBEG = NFDAT
         if (.not. XARGOK) go to 40
      end if
      GOTFMT = M .gt. MEIMAT
      if (GOTFMT) M = M - 2
      LOCBEG = LOCBEG + NSKIP
      NSKIP = 0
      MPT = LOCBEG
      if (M .eq. MEIMAT) go to 1300
c                           Take care of setup for vector output
      KNT = 0
      LASKNT = MACT(I+1)
      if (LASKNT .le. 0) then
         LASKNT = -LASKNT
         KNT = LOCBEG - 1
         if (LASKNT .le. KNT) go to 40
      end if
      IMAG = LASKNT
      LASTI = LOCBEG + INC * (LASKNT - 1 - KNT)
      NCOL = 0
c                          Get format for label output.
      call MESSFI
C%%   messcc.kcrwid = messcc.kciwid;
      FMTR = FMTI
      KDILAB = KDI+1
      LINSTR = 2*KDILAB+2
      if (XARG) then
         if (.not. GOTFMT) go to 1150
         IWF = IWG
         FMTF = FMTG
c++ CODE for .C. is inactive
C%%      cmessi.iwf = cmessi.iwg;
C%%      messcc.lfprec = messcc.lgprec;
c++ END
         go to 1160
      end if
      call MESSFD(IDAT)
c                                          After integer format
      LENOUT = KDI
      NIDAT = LASTI + 1
c                                          Common code continues here
 1080 NLINE = (LENLIN - LINSTR + 1) / LENOUT
      if (LBUF .eq. 0) go to 1090
      K = max(LINSTR, LBUF+1)
      if (((K-LINSTR)/LENOUT + (LENLIN-K+1)/LENOUT) .lt. NLINE) K = K +
     1   LENOUT - mod(K-LINSTR, LENOUT)
      KLINE = (LENLIN - K + 1) / LENOUT
      if (KLINE .lt. min(LASKNT-KNT, NLINE/2)) go to 1085
      LINSTR = K - LENOUT * ((K - LINSTR) / LENOUT)
      if (KLINE .ge. LASKNT-KNT)  then
         KLINE = LASKNT - KNT
         K = LBUF + 1
      end if
      KNT = KNT + KLINE
C%%    for (kc=cmessi.lbuf; kc < k; kc++) cmessc.buf[kc] = ' ';
      BUF(LBUF+1:K) = ' '
      LBUF = K
      go to 1110
 1085 call MESSPR
 1090 continue
c++ CODE for ~.C. is active
      BUF = ' '
      write (BUF(1:KDILAB), FMTR) KNT+1
c++ CODE for .C. is inactive
C%%   memset(cmessc.buf,' ',LENBUF);
C%%   sprintf(cmessc.buf, "%*ld", (int)messcc.kcrwid, knt+1);
c++ END
      BUF(KDILAB:KDILAB) = '-'
      KLINE = min(NLINE, LASKNT - KNT)
      KNT = KNT + KLINE
C%%    sprintf(&cmessc.buf[kdilab], "%*ld", (int)messcc.kcrwid, knt);
      write (BUF(KDILAB+1:2*KDILAB), FMTR) KNT
C%%    cmessc.buf[kdilab*2L-1] = ':';
C%%    for (kc=kdilab*2L; kc < *linstr-1; kc++) cmessc.buf[kc] = ' ';
      BUF(2*KDILAB:LINSTR-1) = ':'
      LBUF = LINSTR
 1110 LSTRT = LBUF
      LBUF = LBUF + LENOUT * KLINE - 1
      if (XARG) return
c                                    Integer output
c++ CODE for ~.C. is active
      write (BUF(LSTRT:LBUF), FMTI) (IDAT(K), K = MPT,
     1    MPT+INC*(KLINE-1), INC)
c++ CODE for .C. is inactive
C%%   for (k=cmessi.mpt; k<=cmessi.mpt+cmessi.kline-1; k++)
C%%  sprintf(&cmessc.buf[cmessi.lstrt+messcc.kciwid*(k-cmessi.mpt)-1],
C%%      "%*ld", (int)messcc.kciwid, idat[cmessi.inc*k-1]);
c++ END
      MPT = MPT + KLINE * INC
c
c                                     Entry here after vector output.
 1130 if (MPT .le. LASTI) go to 1085
      go to 40
c                                          Get other format
 1150 LENTRY = 5
      return
c                                          After other format
 1160 LENOUT = IWF
      LENTRY = 7
      NFDAT = LASTI + 1
      go to 1080


c                         Sparse vector output.
 1200 XARG = .true.
      if (.not. XARGOK) go to 40
      GOTFMT = .false.
      MPT = 1
      LOCBEG = 1
      INC = 1
      LASKNT = MACT(I+1)
      LASTI = LASKNT
      LENTRY = 10
      return

c                Entry after getting format for sparse data output.
 1210 LENOUT = IWF
      LENTRY = 11
      NLINE = LENLIN / IWF

 1220 call MESSPR
      KLINE = min(LASKNT - MPT + 1, NLINE)
      if (KLINE .le. 0) go to 40
      LBUF = LENOUT * KLINE
      return

c
c                           Take care of setup for matrix output
 1300 continue
      NDIM = MACT(I+1)
      if (NDIM .le. 0) then
         if (NDIM .eq. 0) go to 40
         INC = -NDIM
         NDIM = 1
      end if
      ICOL = 1
      IROW1 = 1
      NROW = MACT(I+2)
      if (NROW .le. 0) then
         if (NROW .eq. 0) go to 40
         IROW1 = -NROW / 100000
         NROW = -NROW - 99999 * IROW1 - 1
      end if
      NCOL = MACT(I+3)
      if (NCOL .le. 0) then
         if (NCOL .eq. 0) go to 40
         ICOL = -NCOL / 100000
         NCOL = -NCOL - 99999 * IROW1 - 1
      end if
      NTXTSV = NTEXT
      ITXTSV = ITEXT
      IRC = 1
c                        Compute widths for row and column labels
 1320 MAXWID(IRC) = 0
      MTEXT(IRC) = MACT(I+IRC+3)
      IMAG = NROCO(IRC)
      KLINE = IMAG
 1330 NTEXT = MTEXT(IRC)
      if (NTEXT .ge. 0) then
         if (NTEXT .eq. 0) then
            LTEXT = 5
         else
c                        Go get row/column widths
            KT = 2
            call MESSMH(TEXT)
            if (KT .lt. 0) then
               MTEXT(IRC) = 0
               go to 1330
            end if
         end if
         call MESSFI
         MAXWID(IRC) = max(MAXWID(IRC), LTEXT + KDI+1)
C%%      if (cmessi.irc == 1)
C%%         messcc.kcrwid = cmessi.kdi;
C%%      else
C%%         messcc.kccwid = cmessi.kdi;
         FMTIM(IRC) = FMTI
      end if
      IRC = IRC + 1
      if (IRC .eq. 2) go to 1320
c                 Widths for Row and column titles have been computed.
      KSHIFT = 1
      LASTI = LOCBEG + INC * (NROW - IROW1)
      if (XARG) then
         if (.not. GOTFMT) go to 1610
c++ CODE for ~.C. is active
         IWF = IWG
         FMTF = FMTG
c++ CODE for .C. is inactive
C%%      cmessi.iwf = cmessi.iwg;
C%%      messcc.lfprec = messcc.lgprec;
c++ END
         go to 1620
      end if
      call MESSFD(IDAT)
c
      If (KDI .ge. KOLWID) then
         LENOUT = KDI
      else
         KSHIFT = (KOLWID - KDI + 2) /2
         LENOUT = KOLWID
c++ CODE for ~.C. is active
         KDI = KOLWID
         FMTI(5:5) = char(ICHAR0 + KOLWID / 10)
         FMTI(6:6) = char(ICHAR0 + mod(KOLWID, 10))
c++ CODE for .C. is inactive
C%%  messcc.kciwid = *kolwid;
c++ END
      end if
      NIDAT = NIDAT + NDIM*NCOL
c                              Continue with commmon code
 1390 NLINE = (LENLIN - LINSTR) / LENOUT
      if (LBUF .le. LINSTR) go to 1420
 1400 call MESSPR
 1420 IROW = IROW1
      KLINE = min(NLINE, NCOL-ICOL+1)
c                       Output column labels (if any)
      if (MTEXTC .lt. 0) go to 1480
      NTEXT = MTEXTC
      IMAG = ICOL
      KT = 3
      call MESSMH(TEXT)
      if (KT .lt. 0) go to 180
c                       Return from output of column labels.
      MTEXTC = NTEXT
 1480 ICOL = ICOL + KLINE
 1490 call MESSPR
c
c                      Output row labels (if any)
      if (MTEXTR .lt. 0) go to 1520
      if (MTEXTR .eq. 0) then
C%%       memcpy(&cmessc.buf[cmessi.lbuf],"Row ", 4);
         BUF(LBUF+1:LBUF+4) = 'Row '
         LBUF = LBUF + 4
         go to 1515
      end if
      NTEXT = MTEXTR
      ITEXT = (NTEXT-1) / LENTXT
      NTEXT = NTEXT - ITEXT * LENTXT
      ITEXT = ITEXT + 1

c                     Go get text for row label
      NTEXTR = 0
      go to 410
c                     Return from getting text for row label
 1510 if (C .ne. '#') then
         MTEXTR = NTEXT + LENTXT * (ITEXT-1)
C%%    for (kc=cmessi.lbuf; kc < *linstr; kc++) cmessc.buf[kc] = ' ';
         BUF(LBUF+1:LINSTR) = ' '
         go to 1520
      end if
 1515 continue
C%%   sprintf(&cmessc.buf[cmessi.lbuf],"%*ld",(int)messcc.kcrwid,irow);
C%%    for (kc=cmessi.lbuf+messcc.kcrwid;
C%%       kc < *linstr; kc++) cmessc.buf[kc] = ' ';
      write (BUF(LBUF+1:LINSTR), FMTR) IROW
 1520 LSTRT = LINSTR + 1
      LBUF = LINSTR + LENOUT*KLINE
      LASTI = MPT + NDIM * KLINE - 1
      if (XARG) return
c                                    Integer output
C%% for (k=cmessi.mpt; k<=cmessi.lasti; k+=cmessi.ndim)
C%%  sprintf(&cmessc.buf[cmessi.lstrt + messcc.kciwid*(k-cmessi.mpt)/
C%%     cmessi.ndim - 1], "%*ld", (int)messcc.kciwid, idat[k-1]);
         write (BUF(LSTRT:LBUF), FMTI) (IDAT(K), K=MPT,LASTI,NDIM)
c
c                                     Entry here after matrix output.
 1530 MPT = MPT + INC
      IROW = IROW + 1
c
      if (IROW .le. NROW) go to 1490
      if (ICOL .gt. NCOL) then
         NTEXT = NTXTSV
         ITEXT = ITXTSV
         go to 40
      end if
      MPT = NDIM*(ICOL-1) + 1
      MTEXTR = MACT(I+4)
      call MESSPR
      LBUF = 1
      BUF(1:1) = ' '
      go to 1400
c                                Need to get format for matrix print.
 1610 LENTRY = 6
      return
c                                Entry after got format for matrix print
 1620 If (IWF .ge. KOLWID) then
         LENOUT = IWF
      else
         KSHIFT = (KOLWID - IWF + 2) /2
         LENOUT = KOLWID
C%%      cmessi.iwf = *kolwid;
C%%      strcpy(cmessc.fmtf, "%*.*E\0");
         write (FMTF(7:8), '(I2)') KOLWID
      end if
      NFDAT = NFDAT + NDIM*NCOL
      LENTRY = 8
      go to 1390
      end

      subroutine MESSFD(IDAT)
c Get the format for data to be printed in vectors and arrays.
c
c ************** Variable only used here *******************************
c
c K      Temporary index.
c J      Temporary index.
c IDAT   Input array to MESS
c IMAX   Used when computing largest integer in array.
c IMIN   Used when computing smallest integer in array.
c
      integer J, K, IDAT(*), IMAX, IMIN
c
c For comments on other variables, see the listing for MESS.
      integer   LENBUF, MEVBAS, MEVLAS
      parameter (LENBUF=250)
      parameter (MEVBAS=10)
      parameter (MEVLAS=33)
      logical          XARG, GOTFMT, XARGOK
      integer          ERRCNT, EUNIT, ICHAR0, IRC, IVAR(MEVBAS:MEVLAS),
     1   IMAG, INC, ITEXT, IWF, IWG, KDF, KDFDEF, KDI, KDIAG, KDJ,
     2   KLINE, KSCRN, KSHIFT, KSPEC, KT, MAXERR, LASTI, LBUF, LENLIN,
     3   LENOUT, LENTRY, LENTXT, LHEAD, LINERR, LINMSG, LOCBEG, LPRINT,
     4   LSTOP, LSTRT, LTEXT, MAXWID(2), MDAT(5), MPT, MUNIT, NCOL,
     5   NDIM, NFDAT, NIDAT, NMDAT, NROW, NTEXT, OUNIT, SUNIT, TABSPA
c
      character BUF*(LENBUF), DOLS*72, FMTC*7, FMTF*20, FMTG*15,
     1  FMTI*7, FMTIM(2)*7, FMTJ*7, FMTR*7, FMTT*15
      common /CMESSI/ SUNIT, LHEAD, KDFDEF, LINMSG, LINERR, MUNIT,
     1   EUNIT, KSCRN, KDIAG, MAXERR, LSTOP, LPRINT, KDF, NTEXT, NIDAT,
     2   NFDAT, NMDAT, MDAT, TABSPA, ERRCNT, ICHAR0, IMAG, INC, IRC,
     3   ITEXT, IWF, IWG, KDI, KDJ, KLINE, KSHIFT, KSPEC, KT, LASTI,
     4   LBUF, LENLIN, LENOUT, LENTRY, LENTXT, LOCBEG, LSTRT, LTEXT,
     5   MAXWID, MPT, NROW, NCOL, NDIM, OUNIT, GOTFMT, XARG, XARGOK
      common /CMESSC / BUF, DOLS, FMTF, FMTG, FMTI, FMTJ, FMTT, FMTIM
      equivalence (IVAR(MEVBAS), SUNIT)
      equivalence (FMTIM(1), FMTR), (FMTIM(2), FMTC)
c
      save /CMESSI/, /CMESSC/
c
      if (GOTFMT) then
         KDI = KDJ
C%%      messcc.kciwid = cmessi.kdj;
         FMTI = FMTJ
         return
      end if
      K = 1
      IMAX = 1
      IMIN = 0
   10 do 20 J = LOCBEG, LASTI, INC
         IMAX = max(IMAX, IDAT(J))
         IMIN = MIN(IMIN, IDAT(J))
   20 continue
      if (NCOL .ne. 0) then
         K = K + 1
         LOCBEG = LOCBEG + NDIM
         LASTI = LASTI + NDIM
         if (K .le. NCOL) go to 10
      end if
      IMAG = IMAX
      if ((IMAG/10) + IMIN .lt. 0) IMAG = IMIN
      KDI = -KDI
      call MESSFI
      return
      end


      subroutine MESSFI
c Get the format for the integer IMAG.
c
c ************** Variable only used here *******************************
c
c I, K, KD are used in determining number of characters needed to
c          represent IMAG.
c
      integer I, K, KD
c
c For comments on other variables, see the listing for MESS.
      integer   LENBUF, MEVBAS, MEVLAS
      parameter (LENBUF=250)
      parameter (MEVBAS=10)
      parameter (MEVLAS=33)
      logical          XARG, GOTFMT, XARGOK
      integer          ERRCNT, EUNIT, ICHAR0, IRC, IVAR(MEVBAS:MEVLAS),
     1   IMAG, INC, ITEXT, IWF, IWG, KDF, KDFDEF, KDI, KDIAG, KDJ,
     2   KLINE, KSCRN, KSHIFT, KSPEC, KT, MAXERR, LASTI, LBUF, LENLIN,
     3   LENOUT, LENTRY, LENTXT, LHEAD, LINERR, LINMSG, LOCBEG, LPRINT,
     4   LSTOP, LSTRT, LTEXT, MAXWID(2), MDAT(5), MPT, MUNIT, NCOL,
     5   NDIM, NFDAT, NIDAT, NMDAT, NROW, NTEXT, OUNIT, SUNIT, TABSPA
c
      character BUF*(LENBUF), DOLS*72, FMTC*7, FMTF*20, FMTG*15,
     1  FMTI*7, FMTIM(2)*7, FMTJ*7, FMTR*7, FMTT*15
      common /CMESSI/ SUNIT, LHEAD, KDFDEF, LINMSG, LINERR, MUNIT,
     1   EUNIT, KSCRN, KDIAG, MAXERR, LSTOP, LPRINT, KDF, NTEXT, NIDAT,
     2   NFDAT, NMDAT, MDAT, TABSPA, ERRCNT, ICHAR0, IMAG, INC, IRC,
     3   ITEXT, IWF, IWG, KDI, KDJ, KLINE, KSHIFT, KSPEC, KT, LASTI,
     4   LBUF, LENLIN, LENOUT, LENTRY, LENTXT, LOCBEG, LSTRT, LTEXT,
     5   MAXWID, MPT, NROW, NCOL, NDIM, OUNIT, GOTFMT, XARG, XARGOK
      common /CMESSC / BUF, DOLS, FMTF, FMTG, FMTI, FMTJ, FMTT, FMTIM
      equivalence (IVAR(MEVBAS), SUNIT)
      equivalence (FMTIM(1), FMTR), (FMTIM(2), FMTC)
c
      save /CMESSI/, /CMESSC/
c
      KD = 1
      if (KDI .lt. 0) then
c              KDI < 0 to flag need for extra space -- avoids overflows
         KDI = -KDI
         KD = 2
      end if
      K = 1
      if (IMAG .lt. 0) then
         IMAG = -IMAG
         KD = KD + 1
      end if
      I = IMAG / 10
      if (I .ne. 0) then
   10    K = 10 * K
         KD = KD + 1
         if (I .ge. K) go to 10
      end if
      if (KD .ne. KDI) then
         KDI = KD
c++ CODE for ~.C. is active
         FMTI(5:5) = char(ICHAR0 + KDI / 10)
         FMTI(6:6) = char(ICHAR0 + mod(KDI, 10))
c++ CODE for .C. is inactive
C%%      messcc.kciwid = cmessi.kdi;
c++ END
      end if
      return
      end

c++ CODE for ~.C. is active
      integer function MESSGS()
c                                 Get a scratch unit assigned.
      integer J
c
      MESSGS = 31
   10 MESSGS = MESSGS - 1
      if (MESSGS .eq. 0) stop 'Could not assign scratch unit in MESS.'
      open (MESSGS, STATUS='SCRATCH', ACCESS='SEQUENTIAL',
     1    FORM='UNFORMATTED', IOSTAT=J)
      if (J .ne. 0) go to 10
      return
      end
c++ END

      subroutine MESSMH(TEXT)
c Processing of multiple headings:
c
c J     Used as a temporary index.
c K     Used as a temporary index.
c KB    Number of initial blanks
c KK    Used as a temporary index.
c KT    Used for logic in output of headings.  Set <0 on exit if there
c       is an input error.
c     KT = 1 Output table headings.  (Set to -1 on fatal error.)
c     KT = 2 Get row/column widths for matrix output.  (Set to -2 if
c            error results in no headings.)
c     KT = 3 Output column headings.  (Set to -1 on fatal error.)
c L     Used as a temporary index.
c LFLGDB 2 in usual case, 3 if see a "$ ".
c LSTRDB Value of LSTRT when see a "$ ".
c LTXTDB Value of LTEXT when see a "$ ".
c TEXT  Original input character vector.
c
      integer J, K, KB, KK, L, LFLGDB, LSTRDB, LTXTDB
      character*(*)  TEXT(*)
      character SC, C
      parameter (SC='$')
c For comments on other variables, see the listing for MESS.
      integer   KOLWID, LINSTR, LENBUF, MEVBAS, MEVLAS
      parameter (LENBUF=250)
      parameter (MEVBAS=10)
      parameter (MEVLAS=33)
      logical          XARG, GOTFMT, XARGOK
      integer          ERRCNT, EUNIT, ICHAR0, IRC, IVAR(MEVBAS:MEVLAS),
     1   IMAG, INC, ITEXT, IWF, IWG, KDF, KDFDEF, KDI, KDIAG, KDJ,
     2   KLINE, KSCRN, KSHIFT, KSPEC, KT, MAXERR, LASTI, LBUF, LENLIN,
     3   LENOUT, LENTRY, LENTXT, LHEAD, LINERR, LINMSG, LOCBEG, LPRINT,
     4   LSTOP, LSTRT, LTEXT, MAXWID(2), MDAT(5), MPT, MUNIT, NCOL,
     5   NDIM, NFDAT, NIDAT, NMDAT, NROW, NTEXT, OUNIT, SUNIT, TABSPA
c
      character BUF*(LENBUF), DOLS*72, FMTC*7, FMTF*20, FMTG*15,
     1  FMTI*7, FMTIM(2)*7, FMTJ*7, FMTR*7, FMTT*15
      common /CMESSI/ SUNIT, LHEAD, KDFDEF, LINMSG, LINERR, MUNIT,
     1   EUNIT, KSCRN, KDIAG, MAXERR, LSTOP, LPRINT, KDF, NTEXT, NIDAT,
     2   NFDAT, NMDAT, MDAT, TABSPA, ERRCNT, ICHAR0, IMAG, INC, IRC,
     3   ITEXT, IWF, IWG, KDI, KDJ, KLINE, KSHIFT, KSPEC, KT, LASTI,
     4   LBUF, LENLIN, LENOUT, LENTRY, LENTXT, LOCBEG, LSTRT, LTEXT,
     5   MAXWID, MPT, NROW, NCOL, NDIM, OUNIT, GOTFMT, XARG, XARGOK
      common /CMESSC / BUF, DOLS, FMTF, FMTG, FMTI, FMTJ, FMTT, FMTIM
      equivalence (IVAR(MEVBAS), SUNIT)
      equivalence (FMTIM(1), FMTR), (FMTIM(2), FMTC)
c
      equivalence (MAXWID(1), LINSTR), (MAXWID(2), KOLWID)

      save /CMESSI/, /CMESSC/
c++ CODE for .C. is inactive
C%%      long int kc;
c++ END
      save LFLGDB
      data LFLGDB / 2 /
c
      if (NTEXT .ne. 0) then
         ITEXT = (NTEXT-1) / LENTXT
         NTEXT = NTEXT - ITEXT * LENTXT
         ITEXT = ITEXT + 1
      end if
      do 300 J = 1, max(1,KLINE)
         if (NTEXT .eq. 0) then
            K = KOLWID
            go to 210
         end if
         LFLGDB = 2
         LTEXT = 0
  110    continue
C%%       ctmp=memchr(TEXT(cmessi.itext-1L,cmessi.ntext-1), SC,
C%%          cmessi.lentxt - cmessi.ntext + 1);
C%%       if (ctmp == NULL)
C%%             l = 0;
C%%       else
C%%             l = ctmp - TEXT(cmessi.itext-1L,cmessi.ntext-1) + 1;
         L = index(TEXT(ITEXT)(NTEXT:LENTXT), SC)
         if (L .eq. 0) then
            LTEXT = LTEXT + LENTXT - NTEXT + 1
            if (LTEXT .lt. 80) then
               ITEXT = ITEXT + 1
               NTEXT = 1
               go to 110
            end if
            LTEXT = 0
            if (KT .eq. 3) go to 310
            go to 160
         end if
         NTEXT = NTEXT + L + 1
         LTEXT = L + LTEXT - 1
         if (NTEXT .gt. LENTXT) then
            ITEXT = ITEXT + 1
            if (NTEXT .eq. LENTXT + 1) then
               C = TEXT(ITEXT-1)(LENTXT:LENTXT)
               NTEXT = 1
            else
               C = TEXT(ITEXT)(1:1)
               NTEXT = 2
            end if
         else
            C = TEXT(ITEXT)(NTEXT-1:NTEXT-1)
         end if
         if (C .eq. 'H') go to (180, 190, 200), KT
         if (C .eq. 'E') go to (180, 310, 200), KT
         if (C .eq. '#') go to (140, 310, 200), KT
         if (C .eq. ' ') then
c  Special code to set for removing the "$" preceding a blank.
            LSTRDB = LSTRT
            LFLGDB = 3
            LTXTDB = LTEXT
            LTEXT = LTEXT + 1
            go to 110
         end if
         if (KT .ne. 1) go to 160
  140    LTEXT = LTEXT + 2
         go to 110
  160    KT = -KT
         go to 310
c
  180    KOLWID = KOLWID + LENOUT
         if (LTEXT .eq. 0) go to 300
         KB = KOLWID-LTEXT
         if (KB .lt. 0) stop
     1   'Stopped in MESS -- Column width too small in a heading.'
         if (XARG)  KB = 1 + KB/2
         LSTRT = LBUF + KB + 1
         LBUF = LBUF + KOLWID
         if (LBUF .le. LENLIN) MDAT(NROW) = LBUF
         KOLWID = 0
         go to 220
c
c                                  Set up column widths
  190    MAXWID(IRC) = max(MAXWID(IRC), LTEXT)
         go to 300
c
c                                  Output matrix column
  200    K = KOLWID
         if (C .ne. '#') K = LTEXT
  210    KB = LENOUT - KOLWID
         if (J .eq. 1) then
c                        Special setup for the first column.
            if (XARG) KB = (KB + 1) / 2
            KB = KB + KSHIFT + LINSTR - LBUF
         end if
         KB = KB + KOLWID - K
         LSTRT = LBUF + KB + 1
         LBUF = LSTRT + K - 1
c                                  Set initial blanks
  220    continue
C%%      if (kb > 0) for (kc=cmessi.lstrt-kb-1; kc<cmessi.lstrt-1; kc++)
C%%         cmessc.buf[kc] = ' ';
         if (KB .gt. 0) BUF(LSTRT-KB:LSTRT-1) = ' '
c                                  Move characters
         if (NTEXT .eq. 0) then
C%%       memcpy(&cmessc.buf[cmessi.lstrt-1],"Col ", 4);
            BUF(LSTRT:LSTRT+3) = 'Col '
            C = '#'
            LSTRT = LSTRT+4
         else
            K = NTEXT - LTEXT - LFLGDB
            if (K .le. 0) then
               KK = max(0, 3-NTEXT)
C%%       memcpy(&cmessc.buf[cmessi.lstrt-1L], TEXT(cmessi.itext-2L,
C%%         cmessi.lentxt+k-1), -k-kk+1L);
               BUF(LSTRT:LSTRT-K-KK)=TEXT(ITEXT-1)(LENTXT+K:LENTXT-KK)
               LSTRT = LSTRT-K-KK+1
               K = 1
            end if
            if (NTEXT .gt. 3) then
C%%       memcpy(&cmessc.buf[cmessi.lstrt-1L], TEXT(cmessi.itext-1L,
C%%         k-1), cmessi.ntext-k-2L);
               BUF(LSTRT:LSTRT+NTEXT-K-3) = TEXT(ITEXT)(K:NTEXT-3)
               LSTRT = LSTRT + NTEXT - K - 2
            end if
         end if
         if (LFLGDB .eq. 3) then
c  Special code to remove the "$" preceding a blank.  Only works for 1.
            do 250 L = LSTRDB + LTXTDB + max(0, KB), LSTRT
               BUF(L:L) = BUF(L+1:L+1)
  250       continue
            LFLGDB = 2
            LSTRT = LSTRT - 1
         end if
         if (C .eq. '#') then
c                                  Output column index
C%%         sprintf(&cmessc.buf[cmessi.lstrt-1], "%*ld ",
C%%           (int)(cmessi.lbuf-cmessi.lstrt), cmessi.imag+j-1);
            write (BUF(LSTRT:LBUF), FMTC) IMAG + J - 1
            if (NTEXT .ne. 0) NTEXT = K
            go to 300
         end if
c                                  Set trailing blanks
C%%      if (cmessi.lstrt <= cmessi.lbuf)
C%%           for (kc=cmessi.lstrt-1; kc < cmessi.lbuf; kc++)
C%%              cmessc.buf[kc] = ' ';
         if (LSTRT .le. LBUF) BUF(LSTRT:LBUF) = ' '
  300 continue
  310 return
      end

      subroutine MESSPR
c Prints the buffer for MESS
c
c ************** Variable only used here *******************************
c
c NSCRN  Number of lines currently on CRT from messages.
c
      integer   NSCRN, K
      character SCRNAM*12
      save      NSCRN
c
c For comments on other variables, see the listing for MESS.
      integer   LENBUF, MEVBAS, MEVLAS
      parameter (LENBUF=250)
      parameter (MEVBAS=10)
      parameter (MEVLAS=33)
      logical          XARG, GOTFMT, XARGOK
      integer          ERRCNT, EUNIT, ICHAR0, IRC, IVAR(MEVBAS:MEVLAS),
     1   IMAG, INC, ITEXT, IWF, IWG, KDF, KDFDEF, KDI, KDIAG, KDJ,
     2   KLINE, KSCRN, KSHIFT, KSPEC, KT, MAXERR, LASTI, LBUF, LENLIN,
     3   LENOUT, LENTRY, LENTXT, LHEAD, LINERR, LINMSG, LOCBEG, LPRINT,
     4   LSTOP, LSTRT, LTEXT, MAXWID(2), MDAT(5), MPT, MUNIT, NCOL,
     5   NDIM, NFDAT, NIDAT, NMDAT, NROW, NTEXT, OUNIT, SUNIT, TABSPA
c
      character BUF*(LENBUF), DOLS*72, FMTC*7, FMTF*20, FMTG*15,
     1  FMTI*7, FMTIM(2)*7, FMTJ*7, FMTR*7, FMTT*15
      common /CMESSI/ SUNIT, LHEAD, KDFDEF, LINMSG, LINERR, MUNIT,
     1   EUNIT, KSCRN, KDIAG, MAXERR, LSTOP, LPRINT, KDF, NTEXT, NIDAT,
     2   NFDAT, NMDAT, MDAT, TABSPA, ERRCNT, ICHAR0, IMAG, INC, IRC,
     3   ITEXT, IWF, IWG, KDI, KDJ, KLINE, KSHIFT, KSPEC, KT, LASTI,
     4   LBUF, LENLIN, LENOUT, LENTRY, LENTXT, LOCBEG, LSTRT, LTEXT,
     5   MAXWID, MPT, NROW, NCOL, NDIM, OUNIT, GOTFMT, XARG, XARGOK
      common /CMESSC / BUF, DOLS, FMTF, FMTG, FMTI, FMTJ, FMTT, FMTIM
      equivalence (IVAR(MEVBAS), SUNIT)
      equivalence (FMTIM(1), FMTR), (FMTIM(2), FMTC)
c
      save /CMESSI/, /CMESSC/
      data NSCRN / 0 /
c
      if (LBUF .ne. 0) then
 10     if (BUF(LBUF:LBUF) .eq. ' ') then
          if (LBUF .gt. 1) then
            LBUF = LBUF - 1
            go to 10
          end if
        end if
        if (OUNIT .le. 0) then
          if (KSCRN .gt. 0) then
            if (NSCRN .ge. KSCRN) then
C%%               printf( " Type 'Enter' to continue\n" );
              print '('' Type "Enter" to continue'')'
C%%               scanf( "%*[^\n]%*c" );
              read (*, *)
              NSCRN = 0
            end if
            NSCRN = NSCRN + 1
          end if
C%%      printf( "%.*s\n", (int)cmessi.lbuf, cmessc.buf);
          print '(1X, A)', BUF(1:LBUF)
          if (OUNIT .eq. 0) go to 20
        end if
c++ CODE for ~.C. is active
        K = abs(OUNIT)
        write (K, '(A)', ERR=30) BUF(1:LBUF)
c++ CODE for .C. is inactive
C%%      fprintf(c_handle[labs(cmessi.ounit)-1], "%.*s\n",
C%%      (int)cmessi.lbuf, cmessc.buf);
c++ END
 20     LBUF = 0
      end if
      return
c++ CODE for ~.C. is active
c              See if opening fixes the error
30    write(SCRNAM, '(A, I2.2, A)') 'MESSF_', K, '.tmp'
      open (UNIT=K, STATUS='UNKNOWN', FILE=SCRNAM)
      write (K, '(A)') BUF(1:LBUF)
      return
c++ END
      end

      subroutine MESSFT(MACT, FTEXT)
c  Prints FTEXT, which contains a Fortran character string, and then
c  call MESS to do the actions in MACT.  Actions in MACT can not do
c  anything other than actions that reference MACT.
c  This routine intended for use by library subroutines getting text in
c  the form of a Fortran character string.
c
      integer MACT(*)
      character FTEXT*(*)
c
      integer J, K, IDAT(1), MECONT, MEPRNT, MESUNI
c++ CODE for ~.C. is active
      character TEXT(1)*1
c++ CODE for .C. is inactive
C      character TEXT(1)*2
c++ END
      parameter (MESUNI=10, MEPRNT=21, MECONT=50)
c
      INTEGER LENBUF, MEVBAS, MEVLAS
      parameter (LENBUF=250)
      parameter (MEVBAS=10)
      parameter (MEVLAS=33)
      logical          XARG, GOTFMT, XARGOK
      integer          ERRCNT, EUNIT, ICHAR0, IRC, IVAR(MEVBAS:MEVLAS),
     1   IMAG, INC, ITEXT, IWF, IWG, KDF, KDFDEF, KDI, KDIAG, KDJ,
     2   KLINE, KSCRN, KSHIFT, KSPEC, KT, MAXERR, LASTI, LBUF, LENLIN,
     3   LENOUT, LENTRY, LENTXT, LHEAD, LINERR, LINMSG, LOCBEG, LPRINT,
     4   LSTOP, LSTRT, LTEXT, MAXWID(2), MDAT(5), MPT, MUNIT, NCOL,
     5   NDIM, NFDAT, NIDAT, NMDAT, NROW, NTEXT, OUNIT, SUNIT, TABSPA
c
      character BUF*(LENBUF), DOLS*72, FMTC*7, FMTF*20, FMTG*15,
     1  FMTI*7, FMTIM(2)*7, FMTJ*7, FMTR*7, FMTT*15
      common /CMESSI/ SUNIT, LHEAD, KDFDEF, LINMSG, LINERR, MUNIT,
     1   EUNIT, KSCRN, KDIAG, MAXERR, LSTOP, LPRINT, KDF, NTEXT, NIDAT,
     2   NFDAT, NMDAT, MDAT, TABSPA, ERRCNT, ICHAR0, IMAG, INC, IRC,
     3   ITEXT, IWF, IWG, KDI, KDJ, KLINE, KSHIFT, KSPEC, KT, LASTI,
     4   LBUF, LENLIN, LENOUT, LENTRY, LENTXT, LOCBEG, LSTRT, LTEXT,
     5   MAXWID, MPT, NROW, NCOL, NDIM, OUNIT, GOTFMT, XARG, XARGOK
      common /CMESSC / BUF, DOLS, FMTF, FMTG, FMTI, FMTJ, FMTT, FMTIM
      equivalence (IVAR(MEVBAS), SUNIT)
      equivalence (FMTIM(1), FMTR), (FMTIM(2), FMTC)
c
      do 10 J = 1, 100, 2
         K = abs(MACT(J))
         if ((K .gt. MEPRNT) .or. (K .lt. MESUNI)) go to 20
  10  continue
  20  K = MACT(J)
      MACT(J) = MECONT
      call MESS(MACT, TEXT, IDAT)
      MACT(J) = K
C%%      k = strlen(ftext);
      K = len(FTEXT)
      NTEXT = 1
      if (K .ne. 0) then
         if (FTEXT(1:1) .eq. '0') then
            NTEXT = 2
            K = K - 1
            if (LBUF .eq. 0) then
               BUF(1:1) = ' '
               LBUF = 1
            end if
         end if
         call MESSPR
         LBUF = K
C%%      memcpy(cmessc.buf, &ftext[cmessi.ntext-1], k);
         BUF(1:K) = FTEXT(NTEXT:NTEXT+K-1)
      end if
      ICHAR0 = ICHAR('0')
      if (MACT(J) .ne. MECONT) call mess(MACT(J), TEXT, IDAT)
      return
      end
