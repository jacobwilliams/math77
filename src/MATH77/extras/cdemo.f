      program CDEMO
c     .  Copyright (C) 1989, California Institute of Technology.
c     .  All rights reserved.  U. S. Government sponsorship under
c     .  NASA contract NAS7-918 is acknowledged.
c>> 2004-09-10 CDEMO Krogh  Msp control chars < ' ' to ' ' (for tabs)
c>> 2003-07-22 CDEMO Krogh  '#' in col. 1 for comment in config. file.
c>> 2001-04-25 CDEMO Krogh  Allowed integer ',' to compare as F.P.
c>> 2000-02-08 CDEMO Krogh  Introduced NFERRI for F.P. errors.
c>> 2000-02-01 CDEMO Krogh  Minor fix to avoid underflow on Cray res.
c>> 2000-01-23 CDEMO Krogh  Partial line matching optional. Other fixes.
c>> 2000-01-06 CDEMO Krogh  In config. make blank line a comment.
c>> 1999-12-21 CDEMO Krogh  Fixed compare on integers.
c>> 1999-01-07 CDEMO Krogh  Fixed so 0 and -0 compare as equal.
c>> 1998-04-24 CDEMO Krogh  Fixed fatal bugs.
c>> 1998-01-20 CDEMO Krogh  Allowed more blanks after =name and rest.
c>> 1997-06-12 CDEMO Krogh  Extensive Rewrite
c>> 1997-05-14 CDEMO Krogh  Fixed a matching problem.
c>> 1997-04-23 CDEMO Krogh  Made matching on saved stuff symmetric.
c>> 1997-03-27 CDEMO Krogh  Many bugs on complicated compares fixed.
c>> 1996-07-31 CDEMO Krogh  More care on +/- following integer.
c>> 1996-07-08 CDEMO Krogh  Allow integer / floating comparison.
c>> 1995-10-19 CDEMO Krogh  Initial Code
c Compares results from running demonstration drivers for MATH77 and
c mathc90 on different machines, different languages, or different
c precisions.
c Author's Note: I've had a great deal of trouble getting this code to
c to work on all of the cases that should be handled.  This code almost
c certainly has bugs, and also probably has code that is not necessary.
c At least on June 12, 1997 it appears to work on the cases it is has
c been tested on.
c
c ******************** Variable Definitions ****************************
c
c Variables with a # at the end of the name should have the "#" replaced
c with 1 and 2, where 1 is for variables associated with the first
c buffer, i.e. corresponding to data read from unit 9, and 2 for the
c second with data from unit 10.
c
c BTST#  Contains text string looked for to find start of demo run.
c BUF#   An array of character string buffers, used to read data into,
c    and for storage of lines not yet matched.
c C1     Temporary storage for a single character from BUF1.
c ENFAIL Set = .true. if failure to match at end of line tries partial
c    line match.
c ETST#  Contains text string looked for to find end of demo run.
c FLT#   Contains floating point numbers to be printed for error msg.
c HEAD   The text used for heading output for current demo comparison.
c I      A temporary index.
c I#     Index where current data is being read into BUF#.
c I2K    Used in end check with LSTAT2 .ge. 0.  I2 if no check on
c    buffer 1, else I2 - 1.
c IB#    Array giving index in opposite buffer which matches BUF# with
c    a given index.
c IC#    Index into BUF# for starting on trying to get a match with
c    saved buffers.
c J      Temporary index.
c K      Temporary index.
c K1, K2 Temporary indices.
c KB#    Index for best match on BUF#.
c KNT#   Array giving count of number of characters to examine in a
c    saved buffer.
c LBEG#  Last location to examine in looking for a line that starts
c    output for one demo driver run, see BTST#.
c LENNAM Length of the name for the current driver.
c LEND#  Last location to examine in looking for a line that starts
c    output for one demo driver run, see ETST#.
c LERR   0 if no errors have been seen.
c       >0 count of number of errors.
c       -1 Skipped output in buffer 1 since buffer 2 was empty.
c       -2 Skipped output in buffer 2 since buffer 1 was empty.
c LINB#  The line number for the last header line.
c LINE#  The current line number for the file.
c LOC#   Array giving the starting column to use in checking this buffer
c    with the last input for the other buffer.
c LOCN#  As for LOC#, except the value to use on moving data up.
c LOCEQ1 Location of first "=" in the HEAD.
c LOCEND Location of end in HEAD.
c LSAV#  Array giving line numbers of lines in the saved buffers.
c LSTAT# Indicates the current status as follows.
c    -7    Got an end of file
c    -6    Waiting for other buffer to end output for current demo.
c    -5    Waiting for other buffer to get to start of next demo.
c    -4    First wait on partial line match, don't know which is longer.
c    -3    Waiting for partial lines to be matched.  Failure means
c          a mismatch.
c    -2    Waiting to confirm partial line matches.  Failure is a
c          skipped line.  (Early failure should start over??)  Here
c          we are matching a part of the input line.
c    -1    Waiting to get a match on the first partial line.  After
c          40 failures, change mind about the partial line match, and
c          start over on processing.
c     0    Reading lines to get start of next demo.
c     1    Just reading lines and trying to match them.
c MA     Set = abs(MATCH)
c MAP    Array defining mappings of characters to integers.
c MATCH  Result of comparing two lines.  Interpreted as follows.
c     1  Lines matched perfectly, except perhaps for blanks, the case
c        on letters, and a very few other special cases.
c     2  Lines matched.
c     3  Matched except for floating point.
c     4  Matched except for integers.
c     5  Lines did not compare correctly.
c    <0  Same as for >0, except one line is shorter.
c   This has a special meaning when comparing the header lines for a
c   demo run.  In this case,
c    -2  First name is less than the second.
c     1  Names matched, except possibly for the third letter of name.
c     2  Second name is less than the first.
c MAXBUF Parameter giving the maximum number of buffers that can be
c    saved.
c MAXLEN Parameter giving the longest length of a line that is allowed
c    to be output by a demo driver.
c MBEST  Smallest value seen for MB# on current inputs.
c MB#    Array giving the value of MA, on the best match of the line
c    in BUF# matching some line in the opposite buffer.
c N#     Number of character last read into BUF# not counting trailing
c    blanks.
c NBLN1  Number of nonblank lines seen in buffer 1 since getting a
c    reset.  (Automatic resets happen when starting a drivers output.
c NEEDM  Defines type of match needed.
c   = 0   Any
c   = 1   Full line only
c   =-1   Partial line, NEWLOC > 0   All of buffer 2 matches
c   =-3   Partial line, NEWLOC < 0   All of buffer 1 matches
c NEWLOC Location at which the unfinshed buffer is at when it has
c    matched all of the other buffer.  (If < 0, then buffer one has been
c    totally matched, else it is buffer two that has been matched.)
c NFERR  Number of floating point errors pending to be printed.
c NFERRI Number of floating point errors prior to the current check.
c NNXT   If 0 starting check in the configuration file, if =1000 there
c    is no configuration file, if < 0, need to continue reading the
c    configuration file, else is last location of interest in the
c    current line of the configurtion file.
c NUM0   = ichar('0'), the integer mapping for a zero.
c NUMF   Number of floating point compares.
c TOLDP# Tolerance for comparing double precision numbers.
c TOLSP# Tolerance for comparing numbers, one of which in not double.
c TP     Temporary variable
c XLINOK Set = .true. if it is O.K. to have more lines in one file than
c    the other, set .false. if extra lines are errors.
c
c
c ********************** Specifications ********************************
c
c
      integer I, I2K, J, K, K1, K2, KB1, KB2, LBEG1, LBEG2, LEND1,
     1   LEND2, LINB1, LINB2,LINE1, LINE2, MA, MATCH, MBEST, N1, N2,
     2   NFERRI
      character BTST1*8, BTST2*8, ETST1*8, ETST2*8
      double precision TP
      external ALLNUM
      logical ALLNUM
c Declarations for the common block
      integer MAXBUF, MAXLEN
      parameter (MAXBUF = 60, MAXLEN = 140)
      integer I1, I2, IB1(MAXBUF), IB2(MAXBUF), IC1, IC2, KNT1(MAXBUF),
     1   KNT2(MAXBUF), LERR, LENNAM, LOC1(MAXBUF+1), LOC2(MAXBUF+1),
     2   LOCEQ1, LOCEND, LOCN1(MAXBUF+1), LOCN2(MAXBUF+1),
     3   LSAV1(MAXBUF), LSAV2(MAXBUF), LSTAT1, LSTAT2, MAP(0:127),
     4   MB1(MAXBUF), MB2(MAXBUF), NBLN1, NEEDM,
     5   NEWLOC, NFERR, NNXT, NUM0, NUMF
      logical ENFAIL, XLINOK
      character BUF1(MAXBUF)*(MAXLEN), BUF2(MAXBUF)*(MAXLEN), HEAD*79
      double precision FLT1(10), FLT2(10), TOLDP1, TOLDP2, TOLSP1,
     1   TOLSP2
      common / CCDEMO / TOLDP1, TOLDP2, TOLSP1, TOLSP2, FLT1, FLT2, I1,
     1   I2, IB1, IB2, IC1, IC2, KNT1, KNT2, LENNAM, LERR, LOC1, LOC2,
     2   LOCEQ1, LOCEND, LOCN1, LOCN2, LSAV1, LSAV2, LSTAT1, LSTAT2,
     3   MAP, MB1, MB2, NBLN1, NEEDM, NEWLOC, NFERR, NNXT, NUM0, NUMF,
     4   ENFAIL, XLINOK
      common / CCDEMC / BUF1, BUF2, HEAD
c End of declarations for the common block
c
c ******************** Formats *****************************************
c
 1001 format ('Results from comparing demo drivers for MATH77/mathc90'/
     1   '1) = ', A/ '2) = ', A)
c
c ****************** Start of Executable Code **************************
c
      TOLSP1 = 4.D-6
      TOLDP1 = 7.D-15
      open (8, FILE='cdjob', STATUS='OLD')
      read (8, '(A)') HEAD
      NNXT = 1000
      if (HEAD(1:4) .ne.  '    ') then
         NNXT = 0
         open (7, FILE=HEAD, STATUS='OLD')
      end if
c                 Initialize stuff in subroutine COMP.
      call COMP(MATCH, 0, BUF1, 0, BUF2)
      read(8, '(A)', END = 920) BUF1(1)
      open(9, FILE=BUF1(1), STATUS='OLD')
      read(8, '(A)', END = 920) BUF2(1)
      read(8, *, END=30) TOLSP1, TOLDP1
      if (TOLSP1 .gt. TOLDP1) then
         TP = TOLDP1
         TOLDP1 = TOLSP1
         TOLSP1 = TP
      end if
   30 TOLSP2 = TOLSP1
      TOLDP2 = TOLDP1
      close(8)
      open(10, FILE=BUF2(1), STATUS='OLD')
      open(8, FILE='cdres', STATUS='UNKNOWN')
      do 50 N1 = MAXLEN, 1, -1
        if (BUF1(1)(N1:N1) .gt. ' ') go to 60
   50 continue
   60 do 70 N2 = MAXLEN, 1, -1
        if (BUF2(1)(N2:N2) .gt. ' ') go to 80
   70 continue
   80 write(8, 1001) BUF1(1)(1:N1), BUF2(1)(1:N2)
c
      LINE1 = 1
      LINE2 = 1
c                 Take care of the first lines
  100 read(9, '(A)', END = 900) BUF1(1)
      if (BUF1(1)(1:1) .eq. '=') then
         LSTAT1 = -5
         LBEG1 = 1
         LEND1 = 1
         ETST1 = '='
         BTST1 = '='
      else
         LINE1 = LINE1 + 1
         LEND1 = index(BUF1(1), '=')
         if (LEND1 .eq. 0) go to 100
         ETST1 = BUF1(1)(1:LEND1)
         do 120 I = MAXLEN, LEND1, -1
            if (BUF1(1)(I:I) .gt. ' ') go to 130
  120    continue
  130    LBEG1 = I - LEND1
         if (LBEG1.eq.0) stop 'No string given to flag start of output'
         BTST1 = BUF1(1)(LEND1+1:LEND1+LBEG1)
         if (BTST1(LBEG1:LBEG1) .eq. '~') BTST1(LBEG1:LBEG1) = ' '
         LSTAT1 = 0
      end if
  140 read(10, '(A)', END = 910) BUF2(1)
      if (BUF2(1)(1:1) .eq. '=') then
         LSTAT2 = -5
         LBEG2 = 1
         LEND2 = 1
         ETST2 = '='
         BTST2 = '='
      else
         LINE2 = LINE2 + 1
         LEND2 = index(BUF2(1), '=')
         if (LEND2 .eq. 0) go to 140
         ETST2 = BUF2(1)(1:LEND2)
         do 150 I = MAXLEN, LEND2, -1
            if (BUF2(1)(I:I) .gt. ' ') go to 160
  150    continue
  160    LBEG2 = I - LEND2
         if (LBEG2.eq.0) stop 'No string given to flag start of output'
         BTST2 = BUF2(1)(LEND2+1:LEND2+LBEG2)
         if (BTST2(LBEG2:LBEG2) .eq. '~') BTST2(LBEG2:LBEG2) = ' '
         LSTAT2 = 0
      end if
c                    Starting on next demo results.
  170 NUMF = 0
      LERR = 0
      NEEDM = 0
      NFERR = 0
  180 IC1 = 1
      IC2 = 1
c
  200 I1 = 1
      I2 = 1
      LOC1(1) = 0
      LOC2(1) = 0
  210 if (LOC1(I1) .ne. 0) then
         if (LOC1(I1) .lt. 0) then
            if (LOC1(I1) .eq. -10) go to 250
            I1 = I1 + 1
            IC1 = IC1 + 1
            go to 210
         end if
         N1 = KNT1(I1)
         go to 250
      end if
  220 if (LSTAT1 .ge. 0) then
         LOCN1(I1) = 1
  230    LINE1 = LINE1 + 1
         if (I1 .ge. MAXBUF) then
            call OUTPUT (2, 1, 10, 1, 0)
            call MOVEUP(10, 0)
         end if
         read(9, '(A)', END = 800) BUF1(I1)
         do 240 N1 = MAXLEN, 1, -1
           if (BUF1(I1)(N1:N1) .gt. ' ') then
             LOC1(I1) = 1
             LOC1(I1+1) = 0
             NBLN1 = NBLN1 + 1
             KNT1(I1) = N1
             LSAV1(I1) = LINE1
             if (LSTAT1 .eq. 0) go to 400
             go to 250
           end if
 240     continue
         go to 230
      end if
  250 continue

  260 if (LOC2(I2) .ne. 0) then
         if (LOC2(I2) .lt. 0) then
            if (LOC2(I2) .eq. -10) go to 400
            I2 = I2 + 1
            IC2 = IC2 + 1
            go to 260
         end if
         N2 = KNT2(I2)
         go to 400
      end if
c
 270  if (LSTAT2 .ge. 0) then
        LOCN2(I2) = 1
 280    LINE2 = LINE2 + 1
        if (I2 .ge. MAXBUF) then
          call OUTPUT (2, 1, 0, 1, 10)
          call MOVEUP(0, 10)
        end if
        read(10, '(A)', END = 840) BUF2(I2)
        do 290 N2 = MAXLEN, 1, -1
          if (BUF2(I2)(N2:N2) .gt. ' ') then
            LOC2(I2) = 1
            LOC2(I2+1) = 0
            KNT2(I2) = N2
            LSAV2(I2) = LINE2
            go to 400
          end if
 290    continue
        go to 280
      end if
c            Got two nonblank lines to compare.
 400  if (LSTAT1 * LSTAT2 .eq. 0) then
c                     We are looking for initial lines.
         if (LSTAT1 .eq. 0) then
            if (BUF1(I1)(1:LBEG1) .ne. BTST1(1:LBEG1)) then
               I1 = 1
               go to 220
            end if
            if (LSTAT2 .eq. -7) then
               call OUTPUT(3, I1, LBEG1, 0, -1)
               I1 = 1
               go to 220
            end if
            LSTAT1 = -5
         end if
  410    if (LSTAT2 .eq. 0) then
            if (BUF2(I2)(1:LBEG2) .ne. BTST2(1:LBEG2)) then
               I2 = 1
               go to 270
            end if
            if (LSTAT1 .eq. -7) then
               call OUTPUT(3, 0, -1, I2, LBEG2)
               I2 = 1
               go to 270
            end if
            LSTAT2 = -5
         end if
      end if
      if (LSTAT1 .eq. -5) then
         if (LSTAT2 .eq. -5) then
c                     We have found the initial lines.
            LINB1 = LINE1
            LINB2 = LINE2
            call COMP(MATCH,-1,BUF1(I1)(LBEG1+1:),-1,BUF2(I2)(LBEG2+1:))
            if (MATCH .ne. 1) then
               if (MATCH .lt. 0) then
                  call OUTPUT(3, I1, LBEG1, 0, -1)
                  LSTAT1 = 0
                  I1 = 1
                  go to 220
               else
                  call OUTPUT(3, 0, -1, I2, LBEG2)
                  LSTAT2 = 0
                  I2 = 1
                  go to 270
               end if
            end if
            LSTAT1 = 1
            LSTAT2 = 1
            if (KNT1(I1) .gt. 0) BUF1(I1)(KNT1(I1)+1:KNT1(I1)+2) = '  '
            K = LBEG1+LENNAM
            if (BUF1(I1)(K+1:K+5) .ne. '     ') then
  420          K = K + 1
               if (BUF1(I1)(K:K) .le. ' ') go to 420
            end if
            I = index(BUF1(I1)(K+1:), '  ')
            if (I .ne. 0) then
               I = K + I
            else
               I = N1
            end if
            I = min(I, 30)
            HEAD = BUF1(I1)(LBEG1+1:I) // ' =='
            LOCEQ1 = I - LBEG1 + 2
c
            if (KNT2(I2) .gt. 0) BUF2(I2)(KNT2(I2)+1:KNT2(I2)+2) = '  '
            K = LBEG2+LENNAM
            if (BUF1(I2)(K+1:K+5) .ne. '     ') then
 430          K = K + 1
              if (BUF1(I2)(K:K) .le. ' ') go to 430
            end if
            I = index(BUF2(I2)(K+1:), '  ')
            if (I .ne. 0) then
               I = K + I
            else
               I = N2
            end if
            I = min(I, 30)
            HEAD(LOCEQ1 + 4:) = BUF2(I2)(LBEG2+1:I)
            LOCEND = LOCEQ1 + I - LBEG2 + 2
            go to 170
         end if
      end if
      if (BUF1(I1)(1:LEND1) .eq. ETST1(1:LEND1)) then
         if (LSTAT1 .gt. -6) then
            LSTAT1 = -6
            if (LINE1 .eq. LINB1 + 1) then
               LERR = -2
               if (LSTAT2 .eq. 1) LSTAT2 = 0
            else if ((LSTAT2 .lt. 0) .and. (LSTAT2 .ge. -4)) then
               go to 810
            end if
         end if
      end if
      if (BUF2(I2)(1:LEND2) .eq. ETST2(1:LEND2)) then
         if (LSTAT2 .gt. -6) then
            LSTAT2 = -6
            if (LINE2 .eq. LINB2 + 1) then
               LERR = -1
               if (LSTAT1 .eq. 1) LSTAT1 = 0
            else if ((LSTAT1 .lt. 0) .and. (LSTAT1 .ge. -4)) then
               go to 850
            end if
         end if
      end if
  450 if ((LSTAT1 .lt. -5) .or. (LSTAT2 .lt. -5)) then
         if (LSTAT1 .lt. -5) then
            if (LSTAT2 .lt. -5) then
c  Both have reached their terminination, take care of left overs
               if (I1 - IC1 + I2 - IC2 .gt. 0)
     1            call OUTPUT (2, IC1, I1-1, IC2, I2-1)
               call OUTPUT(1, 0, 0, 0, 0)
               if (LSTAT1 .eq. -7) then
                  if (LSTAT2 .eq. -7) stop
     1            'CDEMO -- Finished comparison, result in file: cdres'
               else
                  LSTAT1 = 0
               end if
               if (LSTAT2 .ne. -7) LSTAT2 = 0
               go to 400
            end if
            if (LOC1(1) .ne. 0) go to 500
            I2 = I2 + 1
         else
            if (LOC2(1) .ne. 0) go to 500
            I1 = I1 + 1
         end if
         go to 210
      end if
c                  Try to find matching lines.
  500 MB1(I1) = 5
      MB2(I2) = 5
      MBEST = 5
      IB1(I1) = 0
      IB2(I2) = 0
      I2K = I2
      if (LSTAT2 .gt. 0) then
         I2K = I2K - 1
c Check if buffer 2 input matches saved part of BUF1.
         K = IC1
  510    continue
         if (K .eq. I1) then
            if (NEEDM .ne. 0) then
c                                    Move up data and start a new phase.
               call MOVEUP(K, I2-1)
               if (LSTAT2 .le. -3) go to 210
            end if
         end if
         K1 = LOCN1(K)
  520    K2 = KNT1(K) - K1 + 1
         NFERRI = NFERR
         call COMP(MATCH,K2,BUF1(K)(K1:),N2,BUF2(I2))
  530    MA = abs(MATCH)
         if (MA .lt. 5) then
            if (MATCH * NEEDM .ge. 0) then
               if (NEEDM .ge. 0) go to 550
               if ((NEEDM+2)*NEWLOC .gt. 0) go to 550
            end if
         end if
         NFERR = NFERRI
         if (K1 .ne. 1) then
            if (LSTAT1 .eq. -1) then
               if (I2 .gt. 40) then
c                          Give up on partial line match.
                  do I = 1, IC2
                     LOC2(I) = 1
                  end do
                  IC2 = 1
                  LSTAT1 = 1
                  LOCN1(K) = 1
                  K1 = 1
                  go to 500
               end if
               I2 = I2 + 1
               LOC2(I2) = 0
               go to 210
            end if
         end if
         if ((NEEDM .ne. 0) .or. (LSTAT1 .eq. -4)) then
c              Failure causes the new line to be ignored.
            call OUTPUT(2, K, 0, I2, I2)
            LOC2(I2) = 0
            go to 210
         end if
c                Want to check all of the possibilities.
  540    K = max(K, IC1) + 1
         if (K .le. I1) go to 510
         go to 600
c                     Got a match (probably)
  550    J = MA
         if (MATCH .gt. 0) then
            J = J - 1
         else if (LSTAT1 + LSTAT2 .eq. 2) then
c             Extra checks to avoid bad partial line matches
            if (abs(NEWLOC) .le. 30) go to 540
c Short line is long enough, insist the it contain non-numbers
            if (NEWLOC .lt. 0) then
               if (ALLNUM(BUF1(K), -NEWLOC)) go to 540
            else
               if (ALLNUM(BUF2(I2), NEWLOC)) go to 540
            end if
         end if
         if (J .lt. MBEST) then
            if (MBEST .ne. 5) then
               MB1(KB1) = 5
               IB1(KB1) = 0
               LOCN1(KB1) = LOC1(KB1)
               MB2(KB2) = 5
               IB2(KB2) = 0
               LOCN2(KB2) = LOC2(KB2)
            end if
            do I = NFERRI+1, NFERR
               FLT1(I - NFERRI) = FLT1(I)
               FLT2(I - NFERRI) = FLT2(I)
            end do
            NFERRI = NFERR - NFERRI
            MBEST = J
            KB1 = K
            KB2 = I2
            MB1(K) = MA
            MB2(I2) = MA
            if (K1 .gt. 1) then
               if (IB1(K) .gt. 1000) then
                  IB2(IB1(K) / 1000) = 0
                  IB1(K) = mod(IB1(K), 1000)
               end if
               IB1(K) = 1000*IB1(K) + I2
            else
               if (IB1(K) .ne. 0) then
                  if (IB1(K) .gt. 1000) then
                     IB2(IB1(K) / 1000) = 0
                     IB1(K) = mod(IB1(K), 1000)
                  end if
                  IB2(IB1(K)) = 0
               end if
               IB1(K) = I2
            end if
            IB2(I2) = K
            LOC1(K) = LOCN1(K)
            LOCN1(K) = -1
            LOC2(I2) = LOCN2(I2)
            LOCN2(I2) = -1
            if (MATCH .lt. 0) then
               if (NEWLOC .lt. 0) then
c                         BUF1 contents completely matched
                  LOCN2(I2) = -NEWLOC
               else
c                         BUF2 contents completely matched
                  LOCN1(K) = NEWLOC
               end if
            end if
            if (LSTAT1 .le. 0) then
               if (LSTAT1 .eq. -1) then
                  call MOVEUP(K, I2)
               else if (LSTAT1 .le. -5) then
                  if ((MA .gt. 2) .or. (MATCH .lt. 0))
     1                call output (2, K, K, I2, I2)
                  LOC2(I2) = 0
                  IC1 = IC1 + 1
               else
c                    Print diagnostics as they occur when possible.
                  if (MA .gt. 2) call output (2, K, K, I2, I2)
                  IC1 = IC1 + 1
                  I2 = I2 + 1
                  LOC2(I2) = 0
                  if (LSTAT1 .ne. -4) go to 210
                  LSTAT1 = -3
                  go to 720
               end if
               go to 210
            end if
         end if
  570    NFERR = NFERRI
         K = max(K, IC1) + 1
         if ((MATCH .ne. 0) .and. (K .le. I1)) go to 510
      end if
c
  600 if (LSTAT1 .gt. 0) then
c Check if buffer 2 input matches saved part of BUF1.
         K = IC2
         if ((IB2(K) .eq. I1) .or. (K .gt. I2K)) go to 670
  610    continue
         if (K .eq. I2) then
            if (NEEDM .ne. 0) then
c                                    Move up data and start a new phase.
               call MOVEUP(I1-1, K)
               if (LSTAT1 .le. -3) go to 210
            end if
         end if
         K1 = LOCN2(K)
  620    K2 = KNT2(K) - K1 + 1
         NFERRI = NFERR
         call COMP(MATCH,N1, BUF1(I1), K2,BUF2(K)(K1:))
  630    MA = abs(MATCH)
         If (MA .lt. 5) then
            if (MATCH * NEEDM .ge. 0) then
               if (NEEDM .ge. 0) go to 650
               if ((NEEDM+2)*NEWLOC .gt. 0) go to 650
            end if
         end if
         NFERR = NFERRI
         if (K1 .ne. 1) then
            if (LSTAT2 .eq. -1) then
               if (I1 .gt. 40) then
c                          Give up on partial line match.
                  do I = 1, IC1
                     LOC1(I) = 1
                  end do
                  IC1 = 1
                  LSTAT2 = 1
                  LOCN2(K) = 1
                  K1 = 1
                  go to 500
               end if
               I1 = I1 + 1
               LOC2(I1) = 0
               go to 210
            end if
         end if

         if ((NEEDM .ne. 0) .or. (LSTAT2 .eq. -4)) then
c              Failure causes the new line to be ignored.
            call OUTPUT(2, I1, I1, K, 0)
            LOC1(I1) = 0
            go to 210
         end if
c                Want to check all of the possibilities.
  640    K = max(K, IC2) + 1
         if (K .le. I2) go to 610
         go to 700
c                     Got a match (probably)
  650    J = MA
         if (MATCH .gt. 0) then
            J = J - 1
         else if (LSTAT1 + LSTAT2 .eq. 2) then
c             Extra checks to avoid bad partial line matches
            if (abs(NEWLOC) .le. 50) go to 640
c Short line is long enough, insist the it contain non-numbers
            if (NEWLOC .lt. 0) then
               if (ALLNUM(BUF1(I1), -NEWLOC)) go to 640
            else
               if (ALLNUM(BUF2(K), NEWLOC)) go to 640
            end if
         end if
         if (J .lt. MBEST) then
            if (MBEST .ne. 5) then
               MB1(KB1) = 5
               IB1(KB1) = 0
               LOCN1(KB1) = LOC1(KB1)
               MB2(KB2) = 5
               IB2(KB2) = 0
               LOCN2(KB2) = LOC2(KB2)
            end if
            do I = NFERRI+1, NFERR
               FLT1(I - NFERRI) = FLT1(I)
               FLT2(I - NFERRI) = FLT2(I)
            end do
            NFERRI = NFERR - NFERRI
            MBEST = J
            KB2 = K
            KB1 = I1
            MB2(K) = MA
            MB1(I1) = MA
            if (K1 .gt. 1) then
c                       Have a partial line match.
               if (IB2(K) .gt. 1000) then
                  IB1(IB2(K) / 1000) = 0
                  IB2(K) = mod(IB2(K), 1000)
               end if
               IB2(K) = 1000*IB2(K) + I1
            else
               if (IB2(K) .ne. 0) then
                  if (IB2(K) .gt. 1000) then
                     IB1(IB2(K) / 1000) = 0
                     IB2(K) = mod(IB2(K), 1000)
                  end if
                  IB1(IB2(K)) = 0
               end if
               IB2(K) = I1
            end if
            IB1(I1) = K
            LOC2(K) = LOCN2(K)
            LOCN2(K) = -1
            LOC1(I1) = LOCN1(I1)
            LOCN1(I1) = -1
            if (MATCH .lt. 0) then
               if (NEWLOC .lt. 0) then
c                         BUF1 contents completely matched
                  LOCN2(K) = -NEWLOC
               else
c                         BUF2 contents completely matched
                  LOCN1(I1) = NEWLOC
               end if
            end if
            if (LSTAT2 .le. 0) then
               if (LSTAT2 .eq. -1) then
                  call MOVEUP(I1, K)
               else if (LSTAT2 .le. -5) then
                  if ((MA .gt. 2) .or. (MATCH .lt. 0))
     1                call output (2, I1, I1, K, K)
                  LOC1(I1) = 0
                  IC2 = IC2 + 1
               else
c                    Print diagnostics as they occur when possible.
                  if (MA .gt. 2) call output (2, I1, I1, K, K)
                  IC2 = IC2 + 1
                  I1 = I1 + 1
                  LOC1(I1) = 0
                  if (LSTAT2 .ne. -4) go to 210
                  LSTAT2 = -3
                  go to 720
               end if
               go to 210
            end if
         end if
  670    NFERR = NFERRI
         K = max(K, IC2) + 1
         if ((MATCH .ne. 0) .and. (K .le. I2K)) go to 610
      end if
  700 if (MBEST .eq. 5) then
         if (LSTAT1 .gt. 0) then
            I1 = I1 + 1
            LOC1(I1) = 0
         end if
         if (LSTAT2 .gt. 0) then
            I2 = I2 + 1
            LOC2(I2) = 0
         end if
         go to 210
      end if
      if (KB1 + KB2 .gt. IC1 + IC2) then
c                 Output for skipped lines
         call OUTPUT(2, IC1, KB1-1, IC2, KB2-1)
         do I = IC1, KB1-1
            LOCN1(I) = -1
         end do
         do I = IC2, KB2-1
            LOCN2(I) = -1
         end do
c                 Do compare over if have diagnostics
         if (MB1(KB1) .gt. 2) then
            call COMP(MATCH, KNT1(KB1), BUF1(KB1)(LOC1(KB1):),
     1         KNT2(KB2), BUF2(KB2)(LOC2(KB2):))
            call OUTPUT(2, KB1, KB1, KB2, KB2)
         end if
         call MOVEUP(KB1, KB2)
c###      else if ((LOCN1(KB1) .gt. 0) .or. (LOCN2(KB2) .gt. 0)) then
      else if (I1 + I2 .gt. 2) then
         call MOVEUP(KB1, KB2)
      else
         if (MB1(KB1) .gt. 2) call OUTPUT(2, KB1, KB1, KB2, KB2)
         LOC1(IC1) = 0
         LOC2(IC2) = 0
      end if
      go to 210
c
c           Just set LSTAT# from -4 to -3
  720 if (NEWLOC .eq. 0) then
         NEEDM = 1
      else if (NEWLOC .lt. 0) then
         NEEDM = -3
      else
         NEEDM = -1
      end if
      go to 210
c
c           End of file on buffer 1.
  800 LSTAT1 = -7
  810 if ((LSTAT2 .lt. 0) .and. (LSTAT2 .ge. -4)) then
         IC1 = max(IC1-1, 1)
         IB1(IC1) = 0
         LOC1(IC1) = 1
         LOCN1(IC1) = 1
         LSTAT2 = 1
         LOC1(I1) = -10
         NEEDM = 0
      end if
      go to 210
c           End of file on buffer 2.
  840 LSTAT2 = -7
  850 if ((LSTAT1 .lt. 0) .and. (LSTAT1 .ge. -4)) then
         IC2 = max(IC2-1, 1)
         IB2(IC2) = 0
         LOC2(IC2) = 1
         LOCN2(IC2) = 1
         LSTAT1 = 1
         LOC2(I2) = -10
         NEEDM = 0
      end if
      go to 210
c           End of file before get anything on buffer 1.
  900 stop 'CDEMO -- End of file in buffer 1 before seeing anything.'
c           End of file before get anything on buffer 2.
  910 stop 'CDEMO -- End of file in buffer 2 before seeing anything.'
c           Premature end of file in CDJOB
  920 stop 'CDEMO -- Premature end of file in CDJOB.'
      end

      logical function ALLNUM(BUF, N)
c Check if BUF(1:N) appears to be all numbers (including floating point)
c ALLNUM set to .true. if BUF appears to be all numbers.
      integer N
      character BUF*(*)
c Declarations for the common block
      integer MAXBUF, MAXLEN
      parameter (MAXBUF = 60, MAXLEN = 140)
      integer I1, I2, IB1(MAXBUF), IB2(MAXBUF), IC1, IC2, KNT1(MAXBUF),
     1   KNT2(MAXBUF), LERR, LENNAM, LOC1(MAXBUF+1), LOC2(MAXBUF+1),
     2   LOCEQ1, LOCEND, LOCN1(MAXBUF+1), LOCN2(MAXBUF+1),
     3   LSAV1(MAXBUF), LSAV2(MAXBUF), LSTAT1, LSTAT2, MAP(0:127),
     4   MB1(MAXBUF), MB2(MAXBUF), NBLN1, NEEDM,
     5   NEWLOC, NFERR, NNXT, NUM0, NUMF
      logical ENFAIL, XLINOK
      character BUF1(MAXBUF)*(MAXLEN), BUF2(MAXBUF)*(MAXLEN), HEAD*79
      double precision FLT1(10), FLT2(10), TOLDP1, TOLDP2, TOLSP1,
     1   TOLSP2
      common / CCDEMO / TOLDP1, TOLDP2, TOLSP1, TOLSP2, FLT1, FLT2, I1,
     1   I2, IB1, IB2, IC1, IC2, KNT1, KNT2, LENNAM, LERR, LOC1, LOC2,
     2   LOCEQ1, LOCEND, LOCN1, LOCN2, LSAV1, LSAV2, LSTAT1, LSTAT2,
     3   MAP, MB1, MB2, NBLN1, NEEDM, NEWLOC, NFERR, NNXT, NUM0, NUMF,
     4   ENFAIL, XLINOK
      common / CCDEMC / BUF1, BUF2, HEAD
c End of declarations for the common block
c Locals
      integer MAPDIG
      parameter    (MAPDIG = 6)
      integer I, K
      character C
c
      ALLNUM = .true.
      do I = 1, N
        C = BUF(I:I)
        if (C .gt.  ' ') then
          K = MAP(ichar(C))
          if (K .ne. MAPDIG) then
            if (index('.-EDed', C) .ne. 0) then
              ALLNUM = .false.
              return
            end if
          end if
        end if
      end do
      return
      end

      subroutine COMP (MATCH, N1, BF1, N2, BF2)
c Does the comparisons between BF1 and BF2, and gets the table
c used for mapping characters.
c
c ******************** Variable Definitions ****************************
c
c BF#    Buffers containin the text being compared.
c C#     Character just read from BF#.
c CVAL   Current (operand 1) value when evaluating floating point
c    expressions.
c CX#    = ' ' unless third letters of demo names are different, in
c    which case CX# is that third letter for the demo (in upper case).
c    Used to allow S and D to compare as equal in S/D comparisons.
c I      Temporary index.
c IGNMAP Array used to map states (see KTRAN) to increments to add to
c    IGNORV.  Increments that can be added are:
c   1      Ignore integer compare errors.
c   2      Ignore floating point compare errors.
c   4      Ignore floating errors if compare is o.k. except for signs.
c   8      Get Tolerances
c  16      Ignore any error.
c  32      Reset and continue reading the configuration file.
c  64      Ignore extra lines
c 128      Ignore end failure and try for parial line match.
c    A negative value for IGNMAP is used to index a TOL in TOLSV().
c IGNORE Array used to decide what comparison failures can be ignored.
c    Values are set from data accumlated in IGNORV.  The first location
c    in this array is reserved for "ON" conditions based on getting to
c    a given line or string.  The second location is reserved for
c    exceptions that apply everyplace.  Remaining locations are reserved
c IGNORL Value to use for IGNORE(1) when line count gives exceptional
c    case.
c IGNORS Value to use for IGNORE(1) when string match gives exceptional
c    case.
c IGNORV Accumulates the values to be stored in IGNORE, see IGNMAP.
c    for exceptions that depend on the column index.
c INCTOK Constant array used in COMP giving the extra increment
c   required by a special name when processing the configuration file.
c INT1   The first integer specifying either lines or columns when
c   processing the configuration file.
c IX#    Floating point number is SUM#*10**IX#. (IX#>9989 is an integer)
c J      Temporary index.
c K      Temporary index.
c KACT  Constant array used in COMP to define actions for processing the
c   configuration file depending on the current input and the current
c   state.
c   1     Do nothing special
c   2     Set to ignore something based on state.  If making transition
c         to state 1, activate the ignores now.
c   3     Set initial state for computing TOL value.
c   4     Use value & TOL state to get new value and new TOL state.
c   5     Use input to set TOL state.
c   6     Set TOL = special case value.
c   7     Remember special case TOL
c   8     Remember "LINE" state
c   9     Remmember "COLUMN" state
c  10     Process string
c  11     Get integer 1, set integer 2
c  12     Process integer 1 : integer 1
c   Indices associated with Inputs are as follows:
c    0   float pt. 10   ALL           20   TOL
c    1   (         11   COLUMNS       21   INTEGER
c    2   )         12   EXTRA         22   SQRT
c    3   =         13   FLOATING      23   SIGN
c    4   *         14   IGNORE        24   TOL
c    5   /         15   LINES         25   MEND
c    6   ,         16   MAX           26   MFAIL
c    7   :         17   ON    
c    8   ;         18   RESET
c    9   '         19   SET
c KALLOW Constant array used in COMP to define allowed inputs for a
c   given state when processing the configuration file.  See KTRAN.
c KOLCHK Used in tracking where to store things in IGNORE and where to
c   reference them.  When KOLCHK is 0, there are no exceptions to the
c   standard matching rules.  A value of 2 means we are checking on all
c   lines, >2 means there are special checks for certain column ranges,
c   When checking on the current line, KOLCHK is replaced by -1-KOLCHK.
c KSTAT  The current state when processing the configuration file.  See
c   the column headed "Value" for KTRAN.
c KT     Current type index, values obtained from LTOK.
c KTOL   Array used to contain the state when evaluatng floating point
c   expressions.  Values here define what is to be done when the top of
c   the stack is exposed as follows
c     1  Stack needs to be popped.
c     2  Initial State  -- Get back here and we are done.
c     3  Compute the square root.
c     4  Multiply
c     5  Divide.
c     6  Max has been computed, pop the stack on a ")".
c KTOLSV Index where last floating point number was stored.
c KTRAN array used in COMP to define how one state in processing the
c   configiguration file is mapped to another.  The indices associated
c   with states are as follows
c     Value    After             Possible Inputs
c       1    Init.             IGNORE TOL RESET SET
c       2    IGNORE            FLOATING INTEGER ALL EXTRA END
c       3      " FLOATING      ; , SIGN ON
c       4         " SIGN       ; , ON
c       5      " INTEGER       ; , ON
c       6      " ALL           ; ON
c       7      " EXTRA         LINES
c       8        " LINES       ;
c       9      " END           FAILURE
c      10       " FAILURE      ;
c      11    TOL               =
c      12     "  = [etc]       TOL Floating# SQRT MAX
c      13       " Value        * / ; , ( ) ON
c      14    RESET             ; ON
c      15    SET               TOLERANCES
c      16     "  TOLERANCES    ON
c      17    ? ON              LINES COLUMNS '
c      18      " LINES/COLS    integer
c      19         " integer    :
c      20             " :      integer ;
c      21    ?                 ;
c KTOK   Count of tokens processed.  Set to 0 initially and incremented
c   when starting a new token.  If this is 0, and (MATCH1 = 1 and BF1
c   starts with an integer) or (MATCH1 = 2 and BF2 starts with an
c   integer) then the aforementioned integer is skipped, KTOK is set to
c   -1000 and matching is retried.  On a failure with KTOK = -1000, KTOK
c   is set to 1 and we start over at the beginning of the line.
c LAST   The last character in the current line of the configurtion
c   buffer, BUFC.
c L#     Current location in BF#.  L2 is negated when we want to try
c   advancing in BF1, without a similar advance in BF2.  It is shortly
c   thereafter negated again.
c LB#    Starting location for current token.
c LENSTR The number of characters currently in STRC.
c LETDIF Difference between lower case and upper case indices.  Used in
c   converting letters from lower to upper case.
c LINCH1 First line for a range of lines to be treated specially.
c LINCH2 Last line for a range of lines to be treated specially.
c LINT   If nonzero there are special checks to be made based on either
c   the nonblank line count or a string match.
c LOC01  Location where a '0' is stored in buffer 1 in order to insure
c   that floating point numbers get compared for setting tolerances.
c LOC02  As for LOC01, except for the second floating point number.
c LTOK   used in tracking current token type.  Using the current token
c   type as a row index, and the current value of the mapped character
c   as the column index gives the current token type state.  See names
c   starting with MT.
c LTRAN Constant array usen in COMP for defining the range of locations
c   to consider as possible in inputs in KALLOW.  LTRAN(state index) to
c   LTRAN(state index+1)-1 gives the range of values.
c M#     Mapped value of C#.
c MAP    Array used to get an index to associate with a character.  See
c   names starting with MAP
c MAPxxx gives parameter names corresponding to these "mapped" values.
c   MAPLC   2  Lower case letter except for "e".
c   MAPLCE  3  Lower case "e".  Separate because of use in floating pt.
c   MAPUC   4  Upper case letters except for "E".
c   MAPUCE  5  Upper case "E".
c   MAPDIG  6  A digit.
c   MAPDOT  7  A "."
c   MAPSIG  8  A plus or minus sign.
c   MAPSEP  9  Not alphabetic or used in floating point numbers.
c MATCH1 Defines what happens on a failure with the first match.
c   =0 Accept no match
c   =1 if BF1 starts with an integer and next token in BF1 is a better
c      match for the start of BF2 skip the first token.
c   =2 as above with BF1 and BF2 exchanged.
c MTxxx  gives parameter names for tokens.
c   MTBEG  1  Beginning a new token
c   MTNAM  2  Processing a name -- Letter followed by letters & digits.
c   MTINT  3  Processing an integer
c   MTFPT  4  Processing floating point
c   MTFPE  5  Processing floating point, seen an E.
c   MTFPX  6  Processing floating point, inside exponent.
c N#     Number of characters in BF#.
c ND#    Count of significant digits in floating point numbers.
c SIGN#  Sign of floating point number or integer
c STRC   A string used to compare with with input on "1" to see if
c    this line is an exception to the usual matching rules.
c SUM#   Value accumulated for floating point number or integer.
c TOL    The current value for the tolerance.
c TOLP   Permanent value for tolerance (till next reset or program).
c TOLSV  Place where saved values of the tolerance are saved.
c TOLX   Value for tolerance in exceptional cases.
c TP?    Used to hold temporary floating point numbers.
c TPVAL  Array containing floating point numbers when evaluating
c   expressions.  Allows for up to 4 nested parentheses.
c TVAL   Temporary floating point value (operand 2) when evaluating
c   floating point expressions.
c
c ******************** Formats *****************************************
c
 2020 format(/
     1  ' Execution Stopped on bad line in CDCFG -- This line is:'/
     2  1X, A / ' Processing got to:'/ 1X, A)
c
c ********************** Specifications ********************************
c
c Declarations for the common block
      integer MAXBUF, MAXLEN
      parameter (MAXBUF = 60, MAXLEN = 140)
      integer I1, I2, IB1(MAXBUF), IB2(MAXBUF), IC1, IC2, KNT1(MAXBUF),
     1   KNT2(MAXBUF), LERR, LENNAM, LOC1(MAXBUF+1), LOC2(MAXBUF+1),
     2   LOCEQ1, LOCEND, LOCN1(MAXBUF+1), LOCN2(MAXBUF+1),
     3   LSAV1(MAXBUF), LSAV2(MAXBUF), LSTAT1, LSTAT2, MAP(0:127),
     4   MB1(MAXBUF), MB2(MAXBUF), NBLN1, NEEDM,
     5   NEWLOC, NFERR, NNXT, NUM0, NUMF
      logical ENFAIL, XLINOK
      character BUF1(MAXBUF)*(MAXLEN), BUF2(MAXBUF)*(MAXLEN), HEAD*79
      double precision FLT1(10), FLT2(10), TOLDP1, TOLDP2, TOLSP1,
     1   TOLSP2
      common / CCDEMO / TOLDP1, TOLDP2, TOLSP1, TOLSP2, FLT1, FLT2, I1,
     1   I2, IB1, IB2, IC1, IC2, KNT1, KNT2, LENNAM, LERR, LOC1, LOC2,
     2   LOCEQ1, LOCEND, LOCN1, LOCN2, LSAV1, LSAV2, LSTAT1, LSTAT2,
     3   MAP, MB1, MB2, NBLN1, NEEDM, NEWLOC, NFERR, NNXT, NUM0, NUMF,
     4   ENFAIL, XLINOK
      common / CCDEMC / BUF1, BUF2, HEAD
c End of declarations for the common block
c Locals
      integer MATCH, N1, N2
      character BF1*(*), BF2*(*)
      integer MAPLC, MAPLCE, MAPUC, MAPUCE, MAPDIG, MAPDOT, MAPSIG,
     1   MAPSEP, MTBEG, MTNAM, MTINT, MTFPT, MTFPE, MTFPX
      parameter    ( MAPLC  = 2, MAPLCE = 3, MAPUC  = 4, MAPUCE = 5,
     1   MAPDIG = 6, MAPDOT = 7, MAPSIG = 8, MAPSEP = 9 )
      parameter (MTBEG = 1, MTNAM = 2, MTINT = 3, MTFPT = 4,
     1   MTFPE = 5, MTFPX = 6)
      INTEGER MFPT, MLPAR, MRPAR, MEQUAL, MMULT, MDIV, MCOMMA, MCOLON,
     1     MSEMIC, MAPOST, MALL, MCOLS, MEND, MEXTRA, MFAIL, MFLOAT,
     2     MIGNOR, MINT, MLINES, MMAX, MON, MRESET, MSET, MSQRT, MSIGN,
     3     MTOLER, MTOL
      parameter (MFPT = 0, MLPAR=1, MRPAR=2, MEQUAL=3, MMULT=4, MDIV=5,
     1   MCOMMA=6, MCOLON=7, MSEMIC=8, MAPOST=9, MALL=10, MCOLS=11,
     2   MEXTRA=12, MFLOAT=13, MIGNOR=14, MINT=21, MLINES=15, MMAX=16,
     3   MON=17, MRESET=18, MSET=19, MSQRT=22, MSIGN=23, MTOLER=20,
     4   MTOL=24, MEND=25, MFAIL=26)
      integer I, IGNMAP(3:15), IGNORE(30), IGNORL, IGNORS, IGNORV,
     1   INCTOK(10:26), IX1, IX2, INT1, J, K, KACT(49), KALLOW(49),
     2   KOL1(30), KOL2(30), KOLCHK, KSTAT,  KT, KTOK, KTOL(5),
     3   KTRAN(49), KTOLSV, L1, L2, LAST, LB1, LB2, LENSTR, LETDIF,
     4   LINCH1, LINCH2, LINT, LOC01, LOC02, LTOL, LTRAN(22), M1, M2,
     5   MATCH1, ND1, ND2, LTOK(MTFPX, MAPLC:MAPSEP)
      logical SETLIN
      character C, C1, C2, CX1, CX2, BUFC*(MAXLEN), STRC*(MAXLEN),
     1   NAME1*64, NAME2*64
      double precision CVAL, SIGN1, SIGN2, SUM1, SUM2, TOL, TOLP,
     1    TOLSV(30), TOLX, TP, TP1, TP2, TVAL, TPVAL(5)
      save BUFC, CX1, CX2, IGNORE, IGNORL, IGNORS, KOL1, KOL2, KTOLSV,
     1    LENSTR, LETDIF, LINCH1, LINCH2, STRC, TOLP, TOLSV, TOL
c
      data IGNORV, KOL1(1), KOL2(1), KOL1(2), KOLCHK, LINT /
     1          0,       1,    1000,       1,      0,    0 /
c
c (Cols)  MAPLC, MAPLCE,  MAPUC, MAPUCE, MAPDIG, MAPDOT, MAPSIG, MAPSEP
c Note data statement has rows and columns interchanged.
c (Rows)          MTBEG, MTNAM, MTINT, MTFPT, MTFPE, MTFPX
      data LTOK / MTNAM, MTNAM, MTBEG, MTBEG, MTBEG, MTBEG,
     e            MTNAM, MTNAM, MTFPE, MTFPE, MTBEG, MTBEG,
     U            MTNAM, MTNAM, MTBEG, MTBEG, MTBEG, MTBEG,
     E            MTNAM, MTNAM, MTFPE, MTFPE, MTBEG, MTBEG,
     5            MTINT, MTNAM, MTINT, MTFPT, MTFPX, MTFPX,
     .            MTFPT, MTBEG, MTFPT, MTFPX, MTBEG, MTBEG,
     +            MTBEG, MTBEG, MTFPX, MTFPX, MTFPX, MTBEG,
     *            MTBEG, MTBEG, MTBEG, MTBEG, MTBEG, MTBEG /
c
c                   3  4  5   6  7   8  9   10  11 12 13  14 15
      data IGNMAP / 2, 4, 1, 16, 0, 64, 0, 128,  0, 0, 0, 32, 8 /
c  J value:        10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
      data INCTOK / 2, 6, 4, 7, 5, 4, 2, 1, 4, 2, 9, 6, 3, 3, 2, 2, 6 /
c  State index:    1  2  3    4   5,  6   7   8   9  10  11  12  13  14
c        15  16  17  18  19, 20, 21, end
      data LTRAN / 1, 5, 10, 14, 17, 20, 22, 23, 24, 25, 26, 27, 31, 38,
     1   40, 41, 42, 45, 46, 47, 49, 50 /
c  KALLOW index:         1       2       3       4*      5       6
c 1             7        8       9*     10      11      12      13*
c 2            14       15      16*     17      18      19*     20
c 3            21*      22*     23*     24*     25*     26*     27
c 4            28       29      30*     31      32      33      34
c 5            35       36      37*     38      39*     40*     41*
c 6            42       43      44*     45*     46*     47      48*
c 7            49*
      data KALLOW / MIGNOR,   MTOL, MRESET,   MSET, MFLOAT,   MINT,
     1        MALL, MEXTRA,   MEND, MSEMIC, MCOMMA,  MSIGN,    MON,
     2      MSEMIC, MCOMMA,    MON, MSEMIC, MCOMMA,    MON, MSEMIC,
     3         MON, MLINES, MSEMIC,  MFAIL, MSEMIC, MEQUAL,   MTOL,
     4        MFPT,  MSQRT,   MMAX,  MMULT,   MDIV, MSEMIC, MCOMMA,
     5       MLPAR,  MRPAR,    MON, MSEMIC,    MON, MTOLER,    MON,
     6      MLINES,  MCOLS, MAPOST,   MFPT, MCOLON,   MFPT, MSEMIC,
     7      MSEMIC /
      data KTRAN /       2,     11,     14,     15,      3,      5,
     1           6,      7,      9,      1,      2,      4,     17,
     2           1,      2,     17,      1,      2,     17,      1,
     3          17,      8,      1,     10,      1,     12,     13,
     4          13,     13,     13,     12,     12,      1,     12,
     5          12,     13,     17,      1,     17,     16,     17,
     6          18,     18,      1,     19,     20,     21,      1,
     7           1 /
      data KACT  /       1,      1,      1,      1,      1,      1,
     1           1,      1,      1,      2,      2,      1,      2,
     2           2,      2,      2,      2,      2,      2,      2,
     3           2,      1,      2,      1,      2,      3,      4,
     4           4,      5,      5,      5,      5,      6,      1,
     5           5,      4,      7,      2,      2,      2,      1,
     6           8,      9,     10,     11,      1,     12,     12,
     7           1 / 
c
c ****************** Start of Executable Code **************************
c
c
   10 if (N1 .gt. 0) then
c                            Compare BF1, and BF2
         KTOK = 0
         MATCH = 1
         NEWLOC = 0
c                  Check for special cases
         if (LINT .ne. 0) then
            if (KOLCHK .lt. 0) KOLCHK = -1 - KOLCHK
            IGNORE(1) = 0
            TOLX = 0.D0
            if (IGNORS .ne. 0) then
               if (NBLN1 .gt. 0) then
c                             Check string.
                  if (index(BF1(1:N1), STRC(1:LENSTR)) .ne. 0) then
c                             Set special actions for this line
                     if (IGNORS .ge. 32) go to 425
                     if (IGNORS .ge. 16) return
                     if (IGNORS .lt. 0) then
                        TOLX = TOLSV(-IGNORS)
                     else
                        IGNORE(1) = IGNORS
                     end if
                     KOLCHK = -1 - KOLCHK
                  end if
               end if
            end if
            if (IGNORL .ne. 0) then
               if (NBLN1 .ge. LINCH1) then
c                            Check if we should do a reset
                  if (IGNORL .ge. 32) go to 425
                  if (NBLN1 .le. LINCH2) then
c                            Check if any result is a match
                     if (IGNORL .ge. 16) return
c                            Set special cases for these lines.
                     if (IGNORL .lt. 0) then
                        TOLX = max(TOLX, TOLSV(-IGNORL))
                        if (KOLCHK .ge. 0) KOLCHK = -1 - KOLCHK
                     else if (IGNORE(1) .eq. 0) then
                        IGNORE(1) = IGNORL
                        KOLCHK = -1 - KOLCHK
                     else
c                            Combine the flags
                        if ((IGNORE(1) .lt. 4) .and. (IGNORL .ge. 4))
     1                     IGNORE(1) = IGNORE(1) + 4
                        if ((mod(IGNORE(1), 4) .lt. 2) .and.
     1                     (mod(IGNORL, 4).ge.2)) IGNORE(1)=IGNORE(1)+2
                        if ((mod(IGNORE(1), 2) .eq. 0) .and.
     1                     (mod(IGNORL, 2).ne.0)) IGNORE(1)=IGNORE(1)+1
                     end if
                  else
c        Restore usual envrionment and process more of configuration..
                     if (KOLCHK .lt. 0) KOLCHK = -1 - KOLCHK
                     go to 430
                  end if
               end if
            end if
            if (KOLCHK .lt. 0) then
               if (IGNORE(1) .eq. 8) then
c           Make sure floating points numbers get computed.
                  LOC01 = 0
                  LOC02 = 0
                  K = 1
    7             L1 = index(BF1(K:), '.')
                  L2 = index(BF2(K:), '.')
                  if (L1 * L2 .ne. 0) then
                     L1 = L1 + K
                     L2 = L2 + K
    8                L1 = L1 - 1
                     L2 = L2 - 1
                     if (BF1(L1:L1) .eq. BF2(L2:L2)) then
                        if (BF1(L1:L1) .gt. ' ') go to 8
                        BF1(L1:L1) = '0'
                        if (K .eq. 1) then
                           LOC01 = L1
                        else
                           LOC02 = L1
                        end if
                     end if
                     if (K .eq. 1) then
                        K = L1 + 8
                        go to 7
                     end if
                  end if
               end if
            end if
         else if (KOLCHK .eq. 2) then
c                        Matched if we are ignoring all.
            if (IGNORE(2) .ge. 16) return
         end if
   20    KT = MTBEG
         L1 = 0
         L2 = 0
c Start by getting start lined up and doing a quick compare.
   30    L1 = L1 + 1
   40    L2 = L2 + 1
   50    C1 = BF1(L1:L1)
         if (C1 .lt. ' ') C1 = ' '
         C2 = BF2(L2:L2)
         if (C2 .lt. ' ') C2 = ' '
         if (C1 .eq. C2) then
            if (C1 .le. ' ') go to 30
c The quick compare is here.
            if (BF1(L1:N1) .eq. BF2(L2:N2)) return
         else if (L1 + L2 .eq. 2) then
c                 Check for problem with Fortran vertical format control
            if ((C1 .eq. '0') .or. (C1 .eq. '1')) then
               if (ichar(C2) .ne. 12) then
                  if (N1 .eq. 1) go to 60
                  if (BF1(2:2) .ne. C2) go to 60
                  L1 = L1 + 1
                  go to 50
               end if
               if (N1 + N2 .eq. 2) return
               go to 30
            end if
            if ((C2 .eq. '0') .or. (C2 .eq. '1')) then
               if (ichar(C1) .ne. 12) then
                  if (N2 .eq. 1) go to 60
                  if (BF2(2:2) .ne. C1) go to 60
                  go to 40
               end if
               if (N1 + N2 .eq. 2) return
               go to 30
            end if
         else if (C1 .le. ' ') then
            L1 = L1 + 1
            go to 50
         else if (C2 .le. ' ') then
            go to 40
         end if
c                    Given up on the quick compare.
   60    L1 = L1 - 1
         LB1 = L1
         L2 = L2 - 1
         LB2 = L2
         MATCH1 = 0
         if (LSTAT1 .lt. 0) then
            if (LSTAT1 .ge. -4) MATCH1 = 2
         end if
         if (LSTAT2 .lt. 0) then
            if (LSTAT2 .ge. -4) MATCH1 = 1
         end if
         go to 120
c                    Just passed a column check
   70    J = L2 - L1
         L1 = KOL2(K)
         if (L1 .ge. N1) then
            L1 = N1
            L2 = N2
         else
            L2 = L1 + J
         end if
         go to 120
c
  100    L2 = -L2
  120    continue
         L1 = L1 + 1
         if (L1 .gt. N1) then
            if (L2 .ge. N2) return
c                           Check for number
            if (KT .ge. MTINT) go to 210
            NEWLOC = -1 - abs(L2)
            if (ENFAIL) then
               MATCH = -MATCH
            else
               MATCH = 5
            end if
            return
         end if
         C1 = BF1(L1:L1)
         M1 = MAP(ichar(C1))
  130    if (L2 .lt. 0) then
            L2 = -L2
         else
            L2 = L2 + 1
            if (L2 .gt. N2) then
c                           Check for number
               if (KT .ge. MTINT) go to 210
               NEWLOC = abs(L1)
               if (ENFAIL) then
                  MATCH = -MATCH
               else
                  MATCH = 5
               end if
               return
            end if
            C2 = BF2(L2:L2)
            M2 = MAP(ichar(C2))
         end if
         if (C1 .lt. ' ') C1 = ' '
         if (C2 .lt. ' ') C2 = ' '
         if (C1 .ne. C2) go to 180
  140    KT = LTOK(KT, M1)
  150    if (KT .eq. MTBEG) then
            LB1 = L1
            LB2 = L2
            if (KTOK .eq. 0) then
               if (C1 .ne. C2) then
c Logic to decide if integer at start of line should be skipped.
                  KTOK = 1
                  if (MATCH1 .ne. 0) then
                     if (MATCH1 .eq. 1) then
                        if (M1 .eq. MAPDIG) then
                           do I = L1, N1-1
                              C1 = BF1(I:I)
                              if ((MAP(ichar(C1)) .ne. MAPDIG) .and.
     1                           (C1 .ne. '-') .and. (C1 .ne. ':'))
     2                           go to 160
                           end do
  160                      if (BF1(I:I) .le. ' ') then
                              KTOK = -1000
                              LB1 = I
                              L1 = I
                           end if
                           go to 120
                        end if
                     else if (M2.eq.MAPDIG) then
                        do I = L2, N2
                           C2 = BF2(I:I)
                           if ((MAP(ichar(C2)) .ne. MAPDIG) .and.
     1                        (C2 .ne. '-') .and. (C2 .ne. ':'))
     2                        go to 170
                        end do
  170                   if (BF2(I:I) .le. ' ') then
                           KTOK = -1000
                           LB2 = I
                           L2 = I
                        end if
                        go to 120
                     end if
                  end if
               end if
            else if (KTOK .eq. -1000) then
               KTOK = 1
               go to 20
            end if
         end if
         KTOK = KTOK + 1
         go to 120
c                Get cases the same and check if that fixes.
  180    if ((M1 .lt. MAPDIG) .and. (M2 .lt. MAPDIG)) then
            if (M1 .lt. MAPUC) C1 = char(ichar(C1) - LETDIF)
            if (M2 .lt. MAPUC) C2 = char(ichar(C2) - LETDIF)
            if (C1 .eq. C2) go to 140
c                  Allow S and D to match for single/double comparisons.
            if ((C1 .eq. CX1) .and. (C2 .eq. CX2)) go to 140
         end if
         if (KT .eq. 1) then
c                              Both are starting a new token.
c                 Skip mismatch on blanks on boundaries
            if (C1 .le. ' ') then
               LB1 = L1
               go to 100
            end if
            if (C2 .le. ' ') then
               LB2 = L2
               go to 130
            end if
c                            Check for "-" "|" mixup
            if (C1 .eq. '-') then
               if (C2 .eq. '|') go to 120
            else if (C1 .eq. '|') then
               if (C2 .eq. '-') go to 120
            end if
c                      Decided that match can't be "perfect"
            MATCH = max(MATCH, 2)
c                       Check for start of floating point.
            if ((M1 .ge. MAPDIG) .and. (M2 .ge. MAPDIG)) then
               if ((M1 .ne. MAPSEP) .and. (M2 .ne. MAPSEP)) go to 200
            end if
            if (KT .ge. MTINT) go to 210
            go to 190
         end if
         if (KT .ge. MTINT) go to 210
         if (KT .eq. MTNAM) then
c                            Skip if trailing F on a name
            if ((C1 .eq. 'F') .or. (C1 .eq. 'f')) then
               if (LTOK(KT, M2) .eq. MTBEG) then
                  if (L1 .eq. N1) go to 100
                  if (LTOK(KT, MAP(ichar(BF1(L1+1:L1+1)))) .eq. MTBEG)
     1               go to 100
               end if
            else if ((C2 .eq. 'F') .or. (C2 .eq. 'f')) then
               if (LTOK(KT, M1) .eq. MTBEG) then
                  if (L2 .eq. N2) go to 130
                  if (LTOK(KT, MAP(ichar(BF2(L2+1:L2+1)))) .eq. MTBEG)
     1               go to 130
               end if
            end if
         end if
  190    continue
         if (KOLCHK .ge. 3) then
c                    Check if exceptions allow comparison failure
            K = KOLCHK
  195       if (KOL1(K) .gt. 0) then
               if ((LB1 .ge. KOL1(K)) .and. (LB1 .le. KOL2(K))) then
                  if (IGNORE(K) .ge. 16) go to 70
               end if
               K = K + 1
               go to 195
            end if
         end if
         MATCH = 5
         return
c
c                     Process integer or floating point.
  200    SIGN1 = 1.D0
         SIGN2 = 1.D0
         if (M1 .eq. MAPSIG) then
            LB1 = LB1 + 1
            if (C1 .eq. '-') SIGN1 = -1.D0
         end if
         if (M2 .eq. MAPSIG) then
            LB2 = LB2 + 1
            if (C2 .eq. '-') SIGN2 = -1.D0
         end if
         go to 220
c
  210    continue
         SIGN1 = 1.D0
         SIGN2 = 1.D0
c                      Get SUM1
  220    L1 = LB1 + 1
         call GETNUM(BF1, L1, ND1, SUM1, IX1)
         if (IX1 .eq. -10000) go to 190
c                      Now get SUM2
         L2 = LB2 + 1
         call GETNUM(BF2, L2, ND2, SUM2, IX2)
         if (IX2 .eq. -10000) go to 190
c           Compare the numbers
         if (IX1 .ge. 9900) then
            if ((IX2 .lt. 9900) .and.
     1         ((BF1(L1:L1) .le. ' ') .or. (BF1(L1:L1) .eq. '-') .or. 
     2          (BF1(L1:L1) .eq. ','))) then
c                     Try integer as floating point.
               if (MATCH1 .eq. 1) then
                  if ((KTOK .eq. 1) .and. (BF1(LB1+1:LB1+1) .eq.
     1                BF2(LB2+1:LB2+1))) KTOK = 0
                  if (KTOK .eq. 0) then
                     L2 = LB2
                     KT = MTBEG
                     M1 = MAPDIG
                     go to 150
                  else if (KTOK .eq. -1000) then
                     KTOK = 1
                     go to 20
                  end if
               end if
               IX1 = 0
               go to 240
            end if
            if (SUM1 .eq. SUM2) then
c                       Check if integers are equal.
               if ((SIGN1 .eq. SIGN2) .or. (SUM1 .eq. 0.D0)) go to 290
            end if
         else if (IX2 .ge. 9900) then
            if  ((BF2(L2:L2) .le. ' ') .or. (BF2(L2:L2) .eq. '-') .or. 
     1           (BF2(L2:L2) .eq. ',')) then
               if (MATCH1 .eq. 2) then
                  if ((KTOK .eq. 1) .and. (BF1(LB1+1:LB1+1) .eq.
     1                BF2(LB2+1:LB2+1))) KTOK = 0
                  if (KTOK .eq. 0) then
                     L1 = LB1
                     KT = MTBEG
                     M2 = MAPDIG
                     go to 150
                  else if (KTOK .eq. -1000) then
                     KTOK = 1
                     go to 20
                  end if
               end if
               IX2 = 0
               go to 240
            end if
         else
            go to 240
         end if
c                  Integers -- can't compare or would have already
         if (KOLCHK .ne. 0) then
c                 Check if exceptions allow comparison failure
            K = max(KOLCHK, 1)
  230       if (KOL1(K) .gt. 0) then
               if ((LB1.ge.KOL1(K)) .and. (LB1.le.KOL2(K))) then
                  J = IGNORE(K)
                  if ((J.ge.16) .or. (mod(J,2).eq.1)) go to 290
               end if
               K = K + 1
               go to 230
            end if
         end if
         MATCH = max(MATCH, 4)
         if (min(IX1, IX2) .lt. 9900) MATCH = 5
         go to 290
c
  240    K = IX1
         if (IX1 .gt. 100) then
            IX1 = mod(IX1, 100)
            IX2 = IX2 + IX1 - K
         else if (IX1 .lt. -100) then
            IX1 = 0
            SUM1 = 0.D0
         end if
         K = IX2
         if (IX2 .gt. 100) then
            IX2 = mod(IX2, 100)
            IX1 = IX1 + IX2 - K
            go to 240
         else if (IX2 .lt. -100) then
            IX2 = 0
            SUM2 = 0.D0
         end if
         SUM1 = SUM1 * 10.D0 ** IX1
         SUM2 = SUM2 * 10.D0 ** IX2
         TP = max(SUM1, SUM2)
         SUM1 = SIGN1 * SUM1
         SUM2 = SIGN2 * SUM2
         NUMF = NUMF + 1
         TP1 = TP * 10.D0 ** (1-min(ND1, ND2))
         if (KOLCHK .lt. 0) then
            if (IGNORE(1) .ge. 8) then
               if (IGNORE(1) .eq. 8) then
                  if (LOC01 .ne. 0) BF1(LOC01:LOC01) = ' '
                  IGNORE(1) = 9
                  TOLSP1 = 33.D0 * SUM1
                  TOLSP2 = 33.D0 * SUM2
               else
                  if (LOC02 .ne. 0) BF1(LOC02:LOC02) = ' '
                  TOLDP1 = 33.D0 * SUM1
                  TOLDP2 = 33.D0 * SUM2
               end if
            end if
         end if
  260    TP2 = abs(SUM1 - SUM2)
         if (TP2 .gt. max(max(1.D0, TP)*TOL, TP1)) then
            if (KOLCHK .ne. 0) then
c                       Check if exceptions allow comparison failure
               K = max(KOLCHK, 1)
  270          if (KOL1(K) .gt. 0) then
                  if ((LB1.ge.KOL1(K)) .and. (LB1.le.KOL2(K))) then
                     J = IGNORE(K)
                     if (J .lt. 0) then
                        if (TP2.le.max(1.D0, TP)*TOLSV(-J)) go to 290
                     else if (J .ge. 4) then
                        if (J .ge. 8) go to 290
                        if (SIGN1 * SIGN2 .lt. 0.D0) then
                           SIGN2 = SIGN1
                           SUM2 = sign(SUM2, SIGN2)
                           go to 260
                        end if
                        J = J - 4
                     end if
                     if (J .ge. 2) go to 290
                  end if
                  K = K + 1
                  go to 270
               end if
               if (KOLCHK .lt. 0) then
                  if (TOLX .ne. 0.D0) then
                     if (TP2 .le. max(1.D0, TP) * TOLX) go to 290
                  end if
               end if
            end if
            MATCH = max(MATCH, 3)
            if (NFERR .lt. 10) then
               NFERR = NFERR + 1
               FLT1(NFERR) = SUM1
               FLT2(NFERR) = SUM2
            end if
         end if
  290    L1 = L1 - 1
         LB1 = L1
         L2 = L2 - 1
         LB2 = L2
         KT = MTBEG
         KTOK = KTOK + 1
         go to 120
      else if (N1 .eq. 0) then
c                   Initialization Stuff
         LETDIF = ichar('a') - ichar('A')
         NUM0 = ichar('0')
         do 400 I = 0, 127
            MAP(I) = MAPSEP
  400    continue
         MAP(ichar('a')) = MAPLC
         MAP(ichar('b')) = MAPLC
         MAP(ichar('c')) = MAPLC
         MAP(ichar('d')) = MAPLC
         MAP(ichar('e')) = MAPLCE
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
         MAP(ichar('E')) = MAPUCE
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
         MAP(ichar('+')) = MAPSIG
         MAP(ichar('-')) = MAPSIG
         return
      end if
      if (N1 .eq. -1) then
c                  Process initial names.
         MATCH = 1
         CX1 = ' '
         CX2 = ' '
         do 410 I = 1, 64
            C1 = BF1(I:I)
            if (MAP(ichar(C1)) .lt. MAPUC) C1 = char(ichar(C1) - LETDIF)
            NAME1(I:I) = C1
            C2 = BF2(I:I)
            if (MAP(ichar(C2)) .lt. MAPUC) C2 = char(ichar(C2) - LETDIF)
            NAME2(I:I) = C2
            if (C1 .ne. C2) then
               if (I .eq. 3) then
                  if ((C1 .eq. 'S') .or. (C1 .eq. 'D')) then
                     if ((C2 .eq. 'S') .or. (C2 .eq. 'D')) then
                        CX1 = C1
                        CX2 = C2
                        go to 410
                     end if
                  end if
               end if
               if (C1 .lt. C2) then
                  MATCH = -2
                  if (NAME2(1:3) .eq. 'DRM') then
                     if ((BF2(4:6).eq.'ach') .or. (BF2(4:6).eq.'ACH'))
     1                   MATCH = 2
                  end if
               else
                  MATCH = 2
                  if (NAME1(1:3) .eq. 'DRM') then
                     if ((BF1(4:6).eq.'ach') .or. (BF1(4:6).eq.'ACH'))
     1                   MATCH = -2
                  end if
               end if
               return
            end if
            if (C1 .le. ' ') go to 420
  410    continue
         stop ' In COMP -- Names not reasonable.'
  420    LENNAM = I - 1
         TOL = max(TOLDP1, TOLDP2)
         if (NAME1(3:3) .ne. 'D') TOL = max(TOL, TOLSP1)
         if (NAME2(3:3) .ne. 'D') TOL = max(TOL, TOLSP2)
         TOLP = TOL
         if (NNXT .gt. 0) then
            if (NNXT .eq. 1000) go to 435
            LAST = NNXT
            go to 510
         end if
         NNXT = 0
      end if
      go to 440
c          Reset for very beginning or after reset.
  425 NBLN1 = 0
      IGNORS = 0
      KTOLSV = 0
      KOLCHK = 0
      KOL2(2) = 0
      IGNORE(2) = 0
      ENFAIL = .false.
      XLINOK = .false.
      TOL = TOLP
c                   Set initial state
  430 KOL1(3) = 0
      IGNORV = 0
      IGNORL = 0
      if (NNXT .lt. 0) go to 440
c              Exit from processing the configuration file.
  435 KOLCHK = min(KOLCHK, 3)
      LINT = abs(IGNORL) + abs(IGNORS)
      if (KOL2(2) .ne. 0) KOLCHK = 2
      if (N1 .lt. 0) return
      go to 10
c       Get the next line in the configuration file to process.
  440 LAST = MAXLEN
  450 read (7, '(A)', END = 435) BUFC
      C1 = BUFC(1:1)
      if (BUFC(1:1) .ne. '=') then
         if (NNXT .ge. 0) go to 450
         if ((C1.eq.'#') .or. (C1.eq.'C') .or. (C1.eq.'c')) go to 450
         if (BUFC(1:8) .eq. '        ') go to 450
      else
         LAST = index(BUFC, ' ') - 1
      end if
c          Convert to upper case, get numbers and strings.
      do 500 I = 1, LAST
         if (BUFC(I:I) .gt. ' ') then
            LAST = I
            C1 = BUFC(I:I)
            if (C1 .eq. '''') then
c                              Take care of a trailing string.
               do 480 K = MAXLEN, 1, -1
                  if (BUFC(K:K) .ne. ' ') go to 490
  480          continue
  490          if (BUFC(K:K) .eq. '''') K = K - 1
               LENSTR = K - I
               STRC = BUFC(I+1:K)
               go to 510
            end if
            K = ichar(C1)
            if (MAP(K) .le. MAPLCE) then
               BUFC(I:I)=char(K - LETDIF)
            end if
         end if
  500 continue
  510 if (BUFC(1:1) .eq. '=') then
         if (NNXT .lt. 0) then
            NNXT = LAST
            go to 435
         end if
         if ((NAME1(1:LAST-1) .ne. BUFC(2:LAST)) .and.
     1      (NAME2(1:LAST-1) .ne. BUFC(2:LAST))) then
            if ((NAME1(1:LAST-1) .gt. BUFC(2:LAST)) .and.
     1         (NAME2(1:LAST-1) .gt. BUFC(2:LAST))) go to 440
            if (BUFC(2:LAST) .eq. 'DRMACH') go to 440
            NNXT = LAST
            go to 435
         else
            NNXT = -1
         end if
         go to 425
      end if
      I = 0
      KSTAT = 1
      go to 1100
  550 continue
      I = I + INCTOK(J)
  570 continue
c                Get transition / action index
      do 620 K = LTRAN(KSTAT), LTRAN(KSTAT+1) - 1
         if (J .eq. KALLOW(K)) go to 710
  620 continue
c          Some kind of error in processing the configuration file.
  700 print 2020, BUFC(1:LAST), BUFC(1:I)
      stop
c                Take the action
c               1   2   3   4   5   6   7   8  9   10   11   12
  710 go to (1090,760,830,870,950,960,750,970,980,990,1010,1020),KACT(K)
c   7     Save TOL for special case action
  750 if (KTRAN(K) .eq. 1) then
            TOL = CVAL
         else
            KTOLSV = KTOLSV + 1
            TOLSV(KTOLSV) = CVAL
            IGNORV = -KTOLSV
         end if
         go to 1090
c 2 Save state for applying later.
  760 IGNORV = IGNORV + IGNMAP(KSTAT)
         if (KTRAN(K) .eq. 1) then
c                            Change to be applied right now.
            if (IGNORV .ge. 32) then
               if (IGNORV .ge. 64) then
                  if (IGNORV .ge. 128) then
                     IGNORV = IGNORV - 128
                     ENFAIL = .true.
                  end if
                  IGNORV = IGNORV - 64
                  XLINOK = .true.
               else
                  go to 425
               end if
            else
               KOLCHK = 2
               IGNORE(2) = IGNORE(2) + IGNORV
               KOL2(2) = 1000
            end if
         end if
         go to 1090
c 3 Set initial state for computing TOL value.
  830 LTOL = 1
         KTOL(1) = 2
         go to 1090
c 4 Use value + TOL state to get new value and new TOL state.
  840    TVAL = CVAL
  850    CVAL = TPVAL(LTOL-1)
  860    LTOL = LTOL - 1
c             (1  in2  sq3   *4   /5 max6
  870 go to (840, 880, 890, 910, 920, 930), KTOL(LTOL)
c 2  Initial State
  880    CVAL = TVAL
            go to 1090
c 3  SQRT
  890    TVAL = sqrt(TVAL)
            KTOL(LTOL) = 1
            go to 860
c 4  *
  910    TVAL = CVAL * TVAL
            go to 860
c 5  /
  920    TVAL = CVAL / TVAL
            go to 860
c 6  max6  -- If got ")" then pop the stack.
  930    if (J .eq. 2) go to 840
            CVAL = max(CVAL, TVAL)
            go to 1090
c   5     Use input (an operator) to set TOL state.
  950 if (J .eq. 1) then
c                          Got a "("
         if (KTOL(LTOL) .eq. MSQRT) then
c                               For SQRT
            KTOL(LTOL) = 3
            go to 1090
         else if (KTOL(LTOL) .eq. MMAX) then
            CVAL = 0.D0
            KTOL(LTOL) = 6
            go to 1090
         else
            TPVAL(LTOL) = CVAL
         end if
      end if
      if (J .eq. 6) then
c                     Got a comma
         if (KTOL(LTOL) .ne. 6) go to 700
      else
         CVAL = TVAL
         LTOL = LTOL + 1
         KTOL(LTOL) = J
      end if
      go to 1090
c   6     Save new value for TOL
  960 TOL = CVAL
         go to 1090
c   8     Remember "LINE" state
  970 SETLIN = .true.
         go to 1090
c   9     Remmember "COLUMN" state
  980 SETLIN = .false.
         go to 1090
c  10     Process string
  990 if (IGNORV .gt. 8) IGNORV = IGNORV - mod(IGNORV, 8)
         IGNORS = IGNORV
         IGNORV = 0
         if (IGNORS .ne. 32) go to 440
         go to 435
c  11     Get integer 1, set integer 2
 1010 INT1 = TVAL
         TVAL = 1000000000
         go to 1090
c  12     Process integer 1 : integer 2
 1020 if (IGNORV .gt. 8) IGNORV = IGNORV - mod(IGNORV, 8)
         if (SETLIN) then
c                    Set to interupt on INT1 until TVAL
             LINT = 1
             IGNORL = IGNORV
             LINCH1 = INT1
             LINCH2 = TVAL
             go to 435
         else
c                    Set up column memory
             KOLCHK = max(KOLCHK, 2) + 1
             IGNORE(KOLCHK) = IGNORV
             IGNORV = 0
             KOL1(KOLCHK) = INT1
             KOL1(KOLCHK+1) = 0
             KOL2(KOLCHK) = TVAL
         end if
c
 1090 KSTAT = KTRAN(K)
 1100 I = I + 1
      if (I .gt. LAST) then
         if (C .eq. ';') go to 440
         C = ';'
      else if (BUFC(I:I) .le. ' ') then
         go to 1100
      else
         C = BUFC(I:I)
      end if
c                12345678 90123456789012345678
      J = index('()=*/,:;''ACEFILMORST', C)
      go to (1110, 1120, 1130, 1140, 1150, 1160, 1170, 1180, 1190,
     1   1200, 1210), J - 9
      if (J .gt. 0) go to 570
      K = MAP(ICHAR(C))
      if (K .ge. MAPDIG) then
c                       Get a number
         if (K .eq. MAPSEP) go to 700
         call GETNUM(BUFC, I, ND1, TVAL, IX1)
         if (IX1 .eq. -10000) go to 700
         if (IX1 .gt. 200) then
            if (IX1 .lt. 9990) then
               print *, 'Floating point number way too big'
               go to 700
            end if
         else
            TVAL = TVAL * 10.D0 ** IX1
         end if
         I = I - 1
         go to 570
      else
         go to 700
      end if
c 'A'
 1110 if (BUFC(I+1:I+2) .eq. 'LL') go to 550
         go to 700
c 'C'
 1120 if (BUFC(I+1:I+6) .eq. 'OLUMNS') go to 550
         go to 700
c 'E'
 1130 if (BUFC(I+1:I+4) .eq. 'XTRA') go to 550
         J = 25
         if (BUFC(I+1:I+2) .eq. 'ND') go to 550
         go to 700
c 'F'
 1140 if (BUFC(I+1:I+7) .eq. 'LOATING') go to 550
         J = 26
         if (BUFC(I+1:I+6) .eq. 'AILURE') go to 550
         go to 700
c 'I'
 1150 if (BUFC(I+1:I+5) .eq. 'GNORE') go to 550
         J = 21
         if (BUFC(I+1:I+6) .eq. 'NTEGER') go to 550
         go to 700
c 'L'
 1160 if (BUFC(I+1:I+4) .eq. 'INES') go to 550
         go to 700
c 'M'
 1170 if (BUFC(I+1:I+2) .eq. 'AX') go to 550
         go to 700
c 'O'
 1180 if (BUFC(I+1:I+1) .eq. 'N') go to 550
         go to 700
c 'R'
 1190 if (BUFC(I+1:I+4) .eq. 'ESET') go to 550
         go to 700
c 'S'
 1200 if (BUFC(I+1:I+2) .eq. 'ET') go to 550
         J = 22
         if (BUFC(I+1:I+3) .eq. 'QRT') go to 550
         J = 23
         if (BUFC(I+1:I+3) .eq. 'IGN') go to 550
         go to 700
c 'T'
 1210 if (BUFC(I+1:I+9) .eq. 'OLERANCES') go to 550
         J = 24
         if (BUFC(I+1:I+2) .ne. 'OL') go to 700
         TVAL = TOL
         go to 550
      end

      subroutine GETNUM(BUF, L, ND, SUM, IX)
c Computes the number at BUF(L:), returns the number in (SUM, IX), and
c the character position following the number in L.
c
c BUF    The character string containing the number.
c IX     Floating point number is SUM*10**IX. (IX>9989 is an integer)
c        IX = -10000 means string is not a number.
c IXSAVE Saved value for IX when checking if have an exponent.
c L      Current position in BUF.
c LSAVE  Saved value for L when chekcing if have an exponent.
c ND     Count of significant digits in floating point number.
c SUM    See IX.
      character BUF*(*)
      integer L, L1, LSAVE, ND, IX, IXSAVE
      double precision SUM
      integer MAPLC, MAPLCE, MAPUC, MAPUCE, MAPDIG, MAPDOT, MAPSIG,
     1   MAPSEP
      parameter    ( MAPLC  = 2, MAPLCE = 3, MAPUC  = 4, MAPUCE = 5,
     1   MAPDIG = 6, MAPDOT = 7, MAPSIG = 8, MAPSEP = 9 )
      character C
      integer K, M, IXS
c Declarations for the common block
      integer MAXBUF, MAXLEN
      parameter (MAXBUF = 60, MAXLEN = 140)
      integer I1, I2, IB1(MAXBUF), IB2(MAXBUF), IC1, IC2, KNT1(MAXBUF),
     1   KNT2(MAXBUF), LERR, LENNAM, LOC1(MAXBUF+1), LOC2(MAXBUF+1),
     2   LOCEQ1, LOCEND, LOCN1(MAXBUF+1), LOCN2(MAXBUF+1),
     3   LSAV1(MAXBUF), LSAV2(MAXBUF), LSTAT1, LSTAT2, MAP(0:127),
     4   MB1(MAXBUF), MB2(MAXBUF), NBLN1, NEEDM,
     5   NEWLOC, NFERR, NNXT, NUM0, NUMF
      logical ENFAIL, XLINOK
      character BUF1(MAXBUF)*(MAXLEN), BUF2(MAXBUF)*(MAXLEN), HEAD*79
      double precision FLT1(10), FLT2(10), TOLDP1, TOLDP2, TOLSP1,
     1   TOLSP2
      common / CCDEMO / TOLDP1, TOLDP2, TOLSP1, TOLSP2, FLT1, FLT2, I1,
     1   I2, IB1, IB2, IC1, IC2, KNT1, KNT2, LENNAM, LERR, LOC1, LOC2,
     2   LOCEQ1, LOCEND, LOCN1, LOCN2, LSAV1, LSAV2, LSTAT1, LSTAT2,
     3   MAP, MB1, MB2, NBLN1, NEEDM, NEWLOC, NFERR, NNXT, NUM0, NUMF,
     4   ENFAIL, XLINOK
      common / CCDEMC / BUF1, BUF2, HEAD
c End of declarations for the common block
c
         LSAVE = 0
         ND = 0
         SUM = 0.D0
         IX = 10000
         L1 = L
         do 20 L = L, MAXLEN
            C = BUF(L:L)
            M = MAP(ichar(C))
            if (M .eq. MAPDIG) then
               SUM = 10.D0 * SUM + ichar(C) - NUM0
               if (SUM .ne. 0.D0) ND = ND + 1
               IX = IX - 1
            else if (M .eq. MAPSEP) then
               go to 80
            else
c                         1234567
               K = index('.EeDd+-', C)
               if (K .eq. 0) then
c A '.' followed by a garbage is not a number.
                  if ((L .le. L1 + 1) .and. (IX .eq. 0)) IX = -10000
                  go to 80
               end if
               if (K .eq. 1) then
                  if (IX .le. 0) go to 80
                  IX = 0
               else if (K .lt. 6) then
                  IXS = 1
                  if (IX .le. 0) go to 40
                  IX = 0
                  go to 30
               else
                  IXS = -1
                  if  (K .eq. 6) IXS = 1
                  if (IX .gt. 9900) return
c      May be exponent following, IX set small when got a ".".
                  go to 30
               end if
            end if
   20    continue
         go to 80
   30    LSAVE = L
         IXSAVE = IX
   40    C = BUF(L+1:L+1)
         M = MAP(ichar(C))
         if (M .eq. MAPSIG) then
            L = L + 1
            if (C .eq. '-') IXS = -1
         end if
   50    K = 0
         L = L + 1
         C = BUF(L:L)
         if (MAP(ichar(C)) .ne. MAPDIG) then
            IX = -10000
            if (LSAVE .ne. 0) then
               L = L - 1
               IX = IXSAVE
            end if
         else
            do 60 L = L, MAXLEN
               C = BUF(L:L)
               if (MAP(ichar(C)) .ne. MAPDIG) go to 70
               K = 10 * K + ichar(C) - NUM0
   60       continue
   70       if (LSAVE .ne. 0) then
               if ((K .gt. 3000) .or. (C .eq. '.') .or.
     1            (MAP(ichar(C)) .lt. MAPDIG)) then
                  L = LSAVE
                  IX = IXSAVE
                  if (MAP(ichar(C)) .lt. MAPDIG) IX = -10000
                  go to 80
               end if
            end if
            if (IXS .lt. 0) K = -K
            IX = IX + K
         end if
   80    continue
         return
         end
      subroutine OUTPUT(IOUT, K1, J1, K2, J2)
c Output results.
c
c IOUT Identifies the type of output.
c   = 1  Output the heading, if there have been no errors, else just
c        output the floating point comparison count.
c   = 2  If the heading has not yet been output, then output it.  Then
c        output additional lines of the form:
c     k:LINE  A line of output from buffer k.
c        where k is 1 and the buffer text is from K1 to J1, and then k
c        is 2 and the buffer text is from K2 to J2.
c        Finally if NFERR is not 0, print out floating point errors.
c   = 3  Flag that skipping output for an entire program.
c K#    Starting index for BUF#.
c J#    Final index for BUF#.
c
c ******************** Variable Definitions ****************************
c
c BLANKS Constant -- Just blanks for output.
c
c ********************** Specifications ********************************
c
c Declarations for the common block
      integer MAXBUF, MAXLEN
      parameter (MAXBUF = 60, MAXLEN = 140)
      integer I1, I2, IB1(MAXBUF), IB2(MAXBUF), IC1, IC2, KNT1(MAXBUF),
     1   KNT2(MAXBUF), LERR, LENNAM, LOC1(MAXBUF+1), LOC2(MAXBUF+1),
     2   LOCEQ1, LOCEND, LOCN1(MAXBUF+1), LOCN2(MAXBUF+1),
     3   LSAV1(MAXBUF), LSAV2(MAXBUF), LSTAT1, LSTAT2, MAP(0:127),
     4   MB1(MAXBUF), MB2(MAXBUF), NBLN1, NEEDM,
     5   NEWLOC, NFERR, NNXT, NUM0, NUMF
      logical ENFAIL, XLINOK
      character BUF1(MAXBUF)*(MAXLEN), BUF2(MAXBUF)*(MAXLEN), HEAD*79
      double precision FLT1(10), FLT2(10), TOLDP1, TOLDP2, TOLSP1,
     1   TOLSP2
      common / CCDEMO / TOLDP1, TOLDP2, TOLSP1, TOLSP2, FLT1, FLT2, I1,
     1   I2, IB1, IB2, IC1, IC2, KNT1, KNT2, LENNAM, LERR, LOC1, LOC2,
     2   LOCEQ1, LOCEND, LOCN1, LOCN2, LSAV1, LSAV2, LSTAT1, LSTAT2,
     3   MAP, MB1, MB2, NBLN1, NEEDM, NEWLOC, NFERR, NNXT, NUM0, NUMF,
     4   ENFAIL, XLINOK
      common / CCDEMC / BUF1, BUF2, HEAD
c End of declarations for the common block
c Locals
      integer I, IOUT, K1, K2, J1, J2, K, M1, M2, M3, M4
      logical FIRST
      character BLANKS*(MAXLEN), BLNKEQ(8)*16
      save FIRST
c
      equivalence (BLANKS, BLNKEQ)
      data BLNKEQ / 8*'             ' /
      data FIRST / .true. /
c
c ******************** Formats *****************************************
c
  200       format(' TOLSP1 =',1PE15.8, '    TOLSP2=', E15.8/
     1             ' TOLDP1 =',  E15.8, '    TOLDP2=', E15.8/)
c
c ****************** Start of Executable Code **************************
c
      if (IOUT .eq. 1) then
         if (FIRST) then
            FIRST = .false.
            write(8, 200) TOLSP1, TOLSP2, TOLDP1, TOLDP2
         end if
         if (LERR .eq. 0) then
            if (LOCEND .ne. 0) write(8, '(A, ''  FCOMP ='', I4)')
     1         HEAD(1:LOCEND), NUMF
         else if (LERR .gt. 0) then
            write(8, '(8X, ''FCOMP ='', I4, ''   Number of errors ='',
     1         I4)') NUMF, LERR
         else
            if (LERR .eq. -1) then
               M1 = LOCEQ1 + 4
               M2 = LOCEND
               M3 = 1
               M4 = LOCEQ1 - 2
            else
               M1 = 1
               M2 = LOCEQ1 - 2
               M3 = LOCEQ1 + 4
               M4 = LOCEND
            end if
            write(8, '(''No output seen for "'', A, ''"; "'',
     1         A, ''" skipped.'')') HEAD(M1:M2), HEAD(M3:M4)
            print '('' No output seen for "'', A, ''"; "'', A,
     1         ''" skipped.'')', HEAD(M1:M2), HEAD(M3:M4)
            return
         end if
         print *, HEAD(1:LOCEND)
         return
      else if (IOUT .eq. 3) then
        if (J1 .ge. 0) then
           K = J1 + index(BUF1(K1)(J1+1:), '  ') - 1
           write(8, '(''Skipping all output for '', A)')BUF1(K1)(J1+1:K)
           LOCN1(K1) = 0
        else
           K = J2 + index(BUF2(K2)(J2+1:), '  ') - 1
           write(8, '(''Skipping all output for '', A)')BUF2(K2)(J2+1:K)
           LOCN2(K2) = 0
        end if
        LOCEND = 0
        return
      end if
      if (XLINOK .and. (I1 .lt. MAXBUF) .and. (I2 .lt. MAXBUF)) then
         if ((NFERR .eq. 0) .and. ((J1.lt.K1) .or. (J2.lt.K2))) return
      end if
      if (LERR .eq. 0) then
         HEAD(LOCEQ1:LOCEQ1) = '/'
         write(8, '(A)') HEAD(1:LOCEND)
      end if
      LERR = LERR + 1
      do 50 I = K1, J1
         if (LOC1(I) .eq. 1) then
            write(8,'(''1)'', I6,'': '',A)') LSAV1(I),BUF1(I)(1:KNT1(I))
            LOCN1(K1) = 0
         else if (LOC1(I) .gt. 1) then
            write(8,'(''1)'',I6,'': '',A)')LSAV1(I),BUF1(I)(1:LOC1(I)-1)
            write(8, '(2A)') BLANKS(1:LOC1(I)+7),
     1         BUF1(I)(LOC1(I):KNT1(I))
        end if
   50 continue
      do 60 I = K2, J2
         if (LOC2(I) .eq. 1) then
            write(8,'(''2)'', I6,'': '',A)') LSAV2(I),BUF2(I)(1:KNT2(I))
            LOCN2(K2) = 0
         else if (LOC2(I) .gt. 1) then
            write(8,'(''2)'',I6,'': '',A)')LSAV2(I),BUF2(I)(1:LOC2(I)-1)
            write(8, '(2A)') BLANKS(1:LOC2(I)+7),
     1         BUF2(I)(LOC2(I):KNT2(I))
        end if
   60 continue
      if (NFERR .ne. 0) then
         do 90 I = 1, NFERR
            write(8, '(6X, ''F1='', 1PE19.12, ''  F2='', E19.12,
     1         ''  DIFF='', E10.3, ''  SF='',0PF5.2)')
     2         FLT1(I), FLT2(I), FLT1(I)-FLT2(I),
     3         -log10(2.D0 * (abs(FLT1(I) - FLT2(I))) /
     4         (abs(FLT1(I)) + abs(FLT2(I)))) 
   90    continue
         NFERR = 0
      end if
      return
      end

      subroutine MOVEUP(ID1, ID2)
c                Shift data up in the buffers.
c ID1 and ID2 give amount to shift the buffers.
      integer ID1, ID2
      integer I, IDI1, IDI2, K, NLSAV
      save NLSAV
c Declarations for the common block
      integer MAXBUF, MAXLEN
      parameter (MAXBUF = 60, MAXLEN = 140)
      integer I1, I2, IB1(MAXBUF), IB2(MAXBUF), IC1, IC2, KNT1(MAXBUF),
     1   KNT2(MAXBUF), LERR, LENNAM, LOC1(MAXBUF+1), LOC2(MAXBUF+1),
     2   LOCEQ1, LOCEND, LOCN1(MAXBUF+1), LOCN2(MAXBUF+1),
     3   LSAV1(MAXBUF), LSAV2(MAXBUF), LSTAT1, LSTAT2, MAP(0:127),
     4   MB1(MAXBUF), MB2(MAXBUF), NBLN1, NEEDM,
     5   NEWLOC, NFERR, NNXT, NUM0, NUMF
      logical ENFAIL, XLINOK
      character BUF1(MAXBUF)*(MAXLEN), BUF2(MAXBUF)*(MAXLEN), HEAD*79
      double precision FLT1(10), FLT2(10), TOLDP1, TOLDP2, TOLSP1,
     1   TOLSP2
      common / CCDEMO / TOLDP1, TOLDP2, TOLSP1, TOLSP2, FLT1, FLT2, I1,
     1   I2, IB1, IB2, IC1, IC2, KNT1, KNT2, LENNAM, LERR, LOC1, LOC2,
     2   LOCEQ1, LOCEND, LOCN1, LOCN2, LSAV1, LSAV2, LSTAT1, LSTAT2,
     3   MAP, MB1, MB2, NBLN1, NEEDM, NEWLOC, NFERR, NNXT, NUM0, NUMF,
     4   ENFAIL, XLINOK
      common / CCDEMC / BUF1, BUF2, HEAD
c End of declarations for the common block
c
      IDI1 = ID1
      IDI2 = ID2
   10 if (LSTAT1 .lt. 0) then
         if (LSTAT1 .eq. -1) then
            if (MB1(IC1) .gt. 2) call OUTPUT(2, IC1, IC1, I2, I2)
            if (LOC1(I1) .eq. 0) LOCN1(I1) = 0
            I1 = min(I1, IC1 + 1)
            LOC1(I1) = 0
            LOC1(IC1) = LOCN1(IC1)
            NEEDM = 0
            if (I2 .le. IC2 + 1) then
c       Take care of case when no table is involved.
               if (NEWLOC .le. 0) then
                  LSTAT1 = 1
                  if (NEWLOC .lt. 0) then
                     LSTAT2 = -1
                  end if
               end if
               go to 40
            end if
            LSTAT2 = -2
            LSTAT1 = 1
            NEEDM = -1
            NLSAV = NEWLOC
            return
         else if (LSTAT1 .eq. -2) then
            IDI2 = IC2 - 1
            LSTAT1 = 1
            LSTAT2 = -3
            if (NLSAV .eq. 0) then
               NEEDM = 0
            else if (NLSAV .lt. 0) then
               NEEDM = -3
               IDI2 = IDI2 - 1
            else
               NEEDM = -1
            end if
            IC2 = IC2 - IDI2
         else if (LSTAT1 .eq. -3) then
            if (NEEDM .eq. 0) then
               LSTAT1 = 1
            else if (NEEDM .eq. -3) then
               LSTAT1 = 1
               LSTAT2 = -4
            else
               LSTAT1 = -4
            end if
            NEEDM = 0
            go to 40
         end if
      end if
   20 if (LSTAT2 .lt. 0) then
         if (LSTAT2 .eq. -1) then
            if (MB2(IC2) .gt. 2) call OUTPUT(2, I1, I1, IC2, IC2)
            if (LOC2(I2) .eq. 0) LOCN2(I2) = 0
            I2 = min(I2, IC2 + 1)
            LOC2(I2) = 0
            LOC2(IC2) = LOCN2(IC2)
            NEEDM = 0
            if (I1 .le. IC1 + 1) then
c       Take care of case when no table is involved.
               if (NEWLOC .ge. 0) then
                  LSTAT2 = 1
                  if (NEWLOC .gt. 0) then
                     LSTAT1 = -1
                  end if
               end if
               go to 40
            end if
            LSTAT1 = -2
            LSTAT2 = 1
            NEEDM = -3
            NLSAV = NEWLOC
            return
         else if (LSTAT2 .eq. -2) then
            IDI1 = IC1 - 1
            LSTAT2 = 1
            LSTAT1 = -3
            if (NLSAV .eq. 0) then
               NEEDM = 1
            else if (NLSAV .lt. 0) then
               NEEDM = -3
            else
               NEEDM = -1
               IDI1 = IDI1 - 1
            end if
            IC1 = IC1 - IDI1
         else if (LSTAT2 .eq. -3) then
            if (NEEDM .ge. 0) then
               LSTAT2 = 1
            else if (NEEDM .eq. -1) then
               LSTAT2 = 1
               LSTAT1 = -4
            else
               LSTAT2 = -4
            end if
            NEEDM = 0
         end if
      end if
c
   40 do I = 1, IDI1
         if (LOCN1(I) .gt. 0) then
            IDI1 = I - 1
            IDI2 = IC2 - 1
            if (LSTAT2 .le. 0) go to 50
            if (LOCN1(I) .eq. 1) go to 50
            LSTAT1 = -1
            NEEDM = 0
            IC1 = I
            IC2 = IC2 + 1
            go to 120
         end if
      end do
   50 do I = 1, IDI2
         if (LOCN2(I) .gt. 0) then
            IDI2 = I - 1
            if (LSTAT1 .le. 0) go to 60
            if (LOCN2(I) .eq. 1) go to 60
            LSTAT2 = -1
            NEEDM = 0
            IC2 = I
            IC1 = IC1 + 1
            go to 120
         end if
      end do
c                Take care of BUF1
   60 K = I1
      I1 = K - IDI1
      do I = IDI1+1, K
         LOC1(I-IDI1) = LOCN1(I)
         LOCN1(I-IDI1) = LOCN1(I)
         LSAV1(I-IDI1) = LSAV1(I)
         KNT1(I-IDI1) = KNT1(I)
         BUF1(I-IDI1)(1:KNT1(I)) = BUF1(I)(1:KNT1(I))
         MB1(I-IDI1) = MB1(I)
         IB1(I-IDI1) = IB1(I) - IDI2
         if (IB1(I-IDI1) .le. 0) then
c### Is this still needed -- i.e. can we get here? YES!
            IB1(I-IDI1) = 0
            MB1(I-IDI1) = 5
         end if
      end do
      LOC1(I1+1) = 0
      do I1 = I1, 1, -1
         if (LOC1(I1) .ge. 0) go to 70
      end do
   70 if (I1 .eq. 0) then
         I1 = 1
         LOC1(1) = 0
      else if (LSTAT2 .eq. -3) then
         I1 = I1 + 1
         LOC1(I1) = 0
      end if
      if (LSTAT1 .ne. -3) IC1 = 1
c             Take care of BUF2
      K = I2
      I2 = K - IDI2
      do I = IDI2+1, K
         LOC2(I-IDI2) = LOCN2(I)
         LOCN2(I-IDI2) = LOCN2(I)
         LSAV2(I-IDI2) = LSAV2(I)
         KNT2(I-IDI2) = KNT2(I)
         BUF2(I-IDI2)(1:KNT2(I)) = BUF2(I)(1:KNT2(I))
         MB2(I-IDI2) = MB2(I)
         IB2(I-IDI2) = IB2(I) - IDI1
         if (IB2(I-IDI2) .le. 0) then
            IB2(I-IDI2) = 0
            MB2(I-IDI2) = 5
         end if
      end do
      LOC2(I2+1) = 0
      do I2 = I2, 1, -1
         if (LOC2(I2) .ge. 0) go to 90
      end do
   90 if (I2 .eq. 0) then
         I2 = 1
         LOC2(1) = 0
      else if (LSTAT1 .eq. -3) then
         I2 = I2 + 1
         LOC2(I2) = 0
      end if
      if (LSTAT2 .ne. -3) IC2 = 1
c Reread old stuff if stuff to be read can't have seen it.
      if (I1 .eq. 1) then
         if (LSTAT2 .eq. 0) I2 = 1
      else if (I2 .eq. 1) then
         if (LSTAT1 .eq. 0) I1 = 1
      else if (LOC1(I1-1) + LOC2(I2-1) .gt. 2) then
         if ((LSTAT1 .ge. 0) .and. (LSTAT2 .ne. -3)) I1 = 1
         if ((LSTAT2 .ge. 0) .and. (LSTAT1 .ne. -3)) I2 = 1
      end if
      return
c          Exit with LSTAT# = -1
  120 if (LSTAT2 .eq. -1) then
         if (IC1 .gt. I1) LOC1(I1) = 0
         I1 = IC1
      else
         if (IC2 .gt. I2) LOC2(I2) = 0
         I2 = IC2
      end if
      return
      end


