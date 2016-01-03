      program CDEMOM
c     .  Copyright (C) 1989, California Institute of Technology.
c     .  All rights reserved.  U. S. Government sponsorship under
c     .  NASA contract NAS7-918 is acknowledged.
c>> 1996-06-13 CDEMOM  Krogh  Added "C Only" option.
c>> 1995-10-23 CDEMOM  Krogh  Initial Code
c Makes up "batch", "script", "add", etc. files for various machines.
c These are run on various machines, and the output resutling from these
c runs is used for input into the program CDEMO.
c Files are constructed using the a template file and the configuration
c file CDCFG.  The template file starts with lines (if any) which are to
c be output once at the beginning, then lines that are to be output once
c for each of the demonstration drivers, and finally lines (if any)
c which are to be output once at the very end.  Special meaning is
c attached to the following character sequences.
c   #1 This is to be replaced by the name of the demonstration driver.
c   #2 This is to be replaced by the name of the demonstration driver in
c      upper case.  (#1 is lower case)  If a #2 is used anyplace, the
c      name will be upper case wherever it is substituted.
c   #3 This is the end of the last line for output to occur one time at
c      the very beginning.  (#1/#2 has no special meaning in this text.)
c   #4 This is the end of the last line that is to be output for each of
c      the demonstration drivers.
c   #5 Text following this is treated as a comment.
c As an example of a templace file, we list here PCLF77L.T a template
c file to use the Lahey Fortran F77l compliler on the PC under DOS.
c   del pc.res /n#3
c   echo =#1  PC - Lahey F77L >>pc.res
c   f77l k:\math77\demo\#1, #1.obj /NS
c   d:\progs\link #1,,,k:\math77\fortran\obj\math77;
c   #1 >>pc.res
c   del #1.* /n#4
c Note that even if you were on a PC using the Lahey Fortran F77L
c compiler under DOS, you will still have to modify this template file
c to use the directories appropriate for your machine, i.e. the
c directories for the demonstration driver source, the directory where
c the Lahey linker resides, and the directory where the library file for
c the MATH77 library resides.
c
c ******************** Variable Definitions ****************************
c
c cdcfg  the name of the configuration file.
c C      Temporary storage for a single character.
c I      Temporary index.
c J      Temporary index.
c K      Temporary index.
c CCODE  Logical variable set .true. if processing template for
c    generating output.
c KACLIN Defines where the name of the demo driver is to be output.
c    Text from characters KACLIN(J-1, K) to KACLIN(J, K) -3 are taken
c    from line K, then if not at the end, the text for the demo driver
c    name is inserted, J incremented and the process repeated.
c L      Temporary index.
c LBEGMA Index for the first line that is output for every demo. driver.
c LENDMA Index for the last line that is output for every demo. driver.
c LENLIN LENLIN(K) gives the index of the last nonblank character in
c    LINE(K).  (If the line ends with #3, #4, or #5, it gives the index
c    of the character just precding the "#".
c LETDIF The differece ichar('A') - ichar('a').
c LINE() Place where lines from the template file are stored.
c LNAM   Index of last nonblank in NAME.
c LONAM  The number of character in ONAME with trailing blanks removed.
c LSPEC  Initially -1.  If this gets set > 0, then in addition to
c    looking for '=' in column 1, code looks for '.for' at end.
c LTNAM  The number of chacters in TNAME with trailing blanks removed.
c MAXLIN The most lines that can appear in a template file.  (Not
c    counting full line comment lines.
c MLCA   = ichar('a')
c MLCZ   = ichar('z')
c MUCA   = ichar('A')
c MUCZ   = ichar('U')
c N      Temporary index.
c NL     Indexes the lines of the template file begin read in.  At the
c    end contains one more than the index of the last line to output.
c NAME   Holds name of the current demonstration driver.  (The first
c    character is '=' and is not part of the name.)
c TNAME  The name of the template file.
c ONAME  The name of the output file.
c UPCASE = .TRUE. if names are to be converted to upper case.
c
c
c ********************** Specifications ********************************
c
      integer MAXLIN
      parameter (MAXLIN = 30)
      integer I, J, K, KACLIN(0:10, MAXLIN), L, LBEGMA, LENDMA,
     1    LENLIN(MAXLIN), LETDIF, LNAM, LONAM, LSPEC, LTNAM, MLCA, MLCZ,
     2    MUCA, MUCZ, N, NL
      logical CCODE, UPCASE
      character C
      character*128 BUF, LINE(MAXLIN), NAME, ONAME, TNAME
      data LSPEC / -1 /
c
c ******************** Formats *****************************************
c
 1000 format(/, ' Input name of template file:  ')
 1010 format(' Output file is:  ', A)
c
c ****************** Start of Executable Code **************************
c
      MUCA = ichar('A')
      MLCA = ichar('a')
      MUCZ = ichar('Z')
      MLCZ = ichar('z')
      LETDIF = MUCA - MLCA
      UPCASE = .FALSE.
      open(7, FILE='cdcfg', STATUS='OLD')
   30 print 1000
      read(*, '(A)') TNAME
      do 40 I = 1, 128
         if (TNAME(I:I) .eq. ' ') go to 60
   40 continue
      go to 30
   60 LTNAM = I - 1
      open(8, FILE=TNAME(1:LTNAM), STATUS='OLD')
c            Get the output file name
      ONAME = TNAME
      CCODE = .false.
      K = index('FfCc', ONAME(LTNAM:LTNAM))
      if (K .ne. 0) then
         if (K .gt. 2) CCODE = .true.
         LONAM = LTNAM - 1
         if ((ONAME(LONAM:LONAM) .eq. 'T') .or.
     1      (ONAME(LONAM:LONAM) .eq. 't')) then
            LONAM = LONAM - 1
            if (ONAME(LONAM:LONAM) .eq. '.') LONAM = LONAM - 1
         end if
      else
         LONAM = LTNAM+1
         ONAME(LONAM:LONAM) = 'M'
      end if
      if (ONAME(1:2) .eq. 'PC') then
         ONAME(LONAM+1:LONAM+4) = '.BAT'
         LONAM = LONAM + 4
      else if (ONAME(1:2) .eq. 'pc') then
         ONAME(LONAM+1:LONAM+4) = '.bat'
         LONAM = LONAM + 4
      end if
      open(9, FILE=ONAME(1:LONAM), STATUS='UNKNOWN')
c          Got the ouput file set up
      LBEGMA = 1
      LENDMA = 0
c          Process the template file
      do 180 NL = 1, MAXLIN
         J = 0
         KACLIN(0, NL) = 1
   80    read (8, '(A)', END=200) LINE(NL)
         do 100 I = 128, 1, -1
            if (LINE(NL)(I:I) .ne. ' ') go to 120
  100    continue
  120    LENLIN(NL) = I
         L = 0
  140    I = L + index(LINE(NL)(L+1:), '#')
         if (I .eq. L) then
            J = J + 1
            KACLIN(J, NL) = LENLIN(NL) + 3
         else
            C = LINE(NL)(I+1:I+1)
            if (C .le. '2') then
               if (C .eq. '2') UPCASE = .TRUE.
               J = J + 1
               L = I + 1
               KACLIN(J, NL) = L + 1
               go to 140
            else if (C .eq. '3') then
c                              Flags the end of a prefix
               LBEGMA = NL + 1
            else if (C .eq. '4') then
c                              Flags the end of the main loop.
               LENDMA = NL
            else if (C .eq. '5') then
c                               Take care of an end of line comment
               if (I .eq. 1) go to 80
            else
               L = I + 1
               go to 140
            end if
c                               Back off from the "#..." ending the line
            J = J + 1
            KACLIN(J, NL) = I + 2
            LENLIN(NL) = I - 1
         end if
         KACLIN(J+1, NL) = 0
  180 continue
      stop 'Capacity exceeded -- increase value of parameter MAXLIN'
  200 NL = NL - 1
      if (LENDMA .eq. 0) LENDMA = NL
c
c                                Take care of the prefix lines
      do 250 K = 1, LBEGMA-1
         write (9, '(A)') LINE(K)(1:LENLIN(K))
  250 continue
c
c                                Take care of the main loop.
  300 continue
      read (7, '(A)', END = 500) NAME
      LSPEC = LSPEC + 1
      if (NAME(1:1) .ne. '=') then
         if (LSPEC .lt. 0) go to 300
         if (LSPEC .eq. 0) then
            K = index(NAME, '.FOR')
            if (K .eq. 0) K = index(NAME, '.for')
            if (K .eq. 0) then
               LSPEC = -100000
               go to 300
            end if
         else
            K = index(NAME, '.FOR')
            if (K .eq. 0) K = index(NAME, '.for')
            if (K .eq. 0) go to 300
         end if
         LNAM = K
         NAME(LNAM:) = ' '
         do 315 K = LNAM, 2, -1
            NAME(K:K) = NAME(K-1:K-1)
  315    continue
         NAME(1:1) = '='
      end if
      do 320 LNAM = 2, 32
        if (NAME(LNAM:LNAM) .eq. ' ') go to 330
  320 continue
  330 LNAM = LNAM - 1
      K = LNAM
      if (UPCASE) then
         do 340 J = 2, K
            L = ichar(NAME(J:J))
            if ((L .ge. MLCA) .and. (L .le. MLCZ)) NAME(J:J) =
     1         char(L + LETDIF)
  340    continue
         if (CCODE) then
            if (index(NAME(LNAM:), 'Fortran only') .ne. 0) go to 300
         else
            if (index(NAME(LNAM:), 'C only') .ne. 0) go to 300
         end if
      else
         do 350 J = 2, K
            L = ichar(NAME(J:J))
            if ((L .ge. MUCA) .and. (L .le. MUCZ)) NAME(J:J) =
     1         char(L - LETDIF)
  350    continue
         if (CCODE) then
            if (index(NAME(LNAM:), 'Fortran only') .ne. 0) go to 300
         else
            if (index(NAME(LNAM:), 'C only') .ne. 0) go to 300
         end if
      end if
      do 400 K = LBEGMA, LENDMA
         L = 1
         do 370 J = 1, 9
            N = KACLIN(J, K) - KACLIN(J-1, K) - 2
            if (N .gt. 0) then
               BUF(L:L+N-1) = LINE(K)(KACLIN(J-1, K):KACLIN(J, K)-3)
               L = L + N
            end if
            if (KACLIN(J+1, K) .eq. 0) go to 380
            BUF(L:L+LNAM-2) = NAME(2:LNAM)
            L = L + LNAM - 1
  370    continue
  380    write (9, '(A)') BUF(1:L-1)
  400 continue
c                   On to the next program
      go to 300
c
c                   Take care of the postfix lines
  500 do 550 K = LENDMA+1, NL
         write (9, '(A)') LINE(K)(1:LENLIN(K))
  550 continue
      print 1010, ONAME(1:LONAM)
      stop
      end


