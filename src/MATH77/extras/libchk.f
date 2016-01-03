      program LIBCHK
c
c This program assumes that one has run GLOCH1 and GLOCH2 to get a list
c of entry names called by a collection of programs (using option
c "C f e") and that this output has been put in a file named "uselst",
c and that a similar action has been taken to get the list of entry
c points in a collection of programs making up a library (using option
c "C e f.") and that this output has been put in a file named "liblst".
c This probgram reads "liblst", and "uselst" and for each program in
c "uselst", gives a list of the files in "liblst" that it uses.  This
c output goes in the file "uselib".  It also gives a list of the entries
c and files used by any program in "uselib".  This list is output at
c in the end of "uselib".
c
c>> 1997-04-25 LIBCHK  Krogh  Initial Library version of the code.
c Code written 3/31/ 97 by Fred Krogh at the Jet Propulsion Laboratory.
c
c *************************** Variable Definitions *********************
c
c BUF    Use to hold lines read
c ELOOK  Name of entry being looked for in LIBENT
c FILCUR Name of file for which entries are being looked up.
c I      Temporary index.
c IO     Used in implied do loop.
c IC     Tracks columns when looking up entry names.
c IL     Lower index on binary search
c IU     Upper index on binary search
c K      Temporary index.
c LIBENT List of entries in library.
c LIBFIL For each entry in LIBENT, gives the associated file.
c LIBUSE Number of times LIBENT is referenced.
c LSTENT Holds list of indices for entries seen for file FILCUR.
c MAXENT Parameter giving the maximum number of library entries allowed.
c NLIB   One more than the number of library entries.
c NUMLST Number of library entries found for file FILCUR.
c
c *************************** Variable Declarations
c
      integer MAXENT
      parameter (MAXENT=1500)
      integer I, IC, IL, IO, IU, K, LIBUSE(MAXENT+1), NLIB, NUMLST,
     1    LSTENT(200)
      character*8 ELOOK, FILCUR, LIBENT(MAXENT), LIBFIL(MAXENT)
      character BUF*144
c
c ************************* Start of Executable Code *******************
c
c Open the input files
      open(7, FILE='liblst', STATUS='OLD')
      open(8, FILE='uselst', STATUS='OLD')
c                           Save the data for the library.
      do NLIB = 1, MAXENT
         LIBUSE(NLIB) = 0
         read(7, '(T2,A8,T11,A8)', END=100) LIBENT(NLIB), LIBFIL(NLIB)
      end do
  100 if (NLIB .le. 1) stop 'Error -- No library data'
      NLIB = NLIB
c
c Set up to save the output data
      open(9, FILE='uselib', STATUS='UNKNOWN')
c
      BUF = ' '
      NUMLST = 0
c
c Read the "uselst" data one line at a time.
  220 read(8, '(A)', END=230) BUF
      go to 240
c                    Set flag for end of file.
  230 BUF(1:1) = '.'
  240 if (BUF(1:9) .ne. ' ') then
         if (NUMLST .ne. 0) then
c                         Something processed previously, output results
            K = 1
            write (9, '(8A9)') FILCUR, (LIBENT(LSTENT(IO)),
     1         IO = K, min(K+6, NUMLST))
            K = K + 7
  260       if (K .lt. NUMLST) then
               write (9,'(9X, 7A9)') (LIBENT(LSTENT(IO)),
     1            IO = K, min(K+6,NUMLST))
               K = K + 7
               go to 260
            end if
            NUMLST = 0
         end if
         if (BUF(1:1) .eq. '.') go to 400
         FILCUR = BUF(2:8)
      end if
      IC = 11
  300 if (BUF(IC:IC+7) .ne. ' ') then
         ELOOK = BUF(IC:IC+7)
         IC = IC + 9
c                        Get entry name if any (binary search)
         IL = 1
         IU = NLIB
  350    I = (IU + IL) / 2
         if (ELOOK .le. LIBENT(I)) then
            if (ELOOK .eq. LIBENT(I)) then
               NUMLST = NUMLST + 1
               LSTENT(NUMLST) = I
               LIBUSE(I) = LIBUSE(I) + 1
               go to 300
            end if
            if (I .eq. IU) go to 300
            IU = I
         else if (I .ne. IL) then
            IL = I
         else
c                  No find in the search
            go to 300
         end if
         go to 350
      end if
      go to 220
c
c Data processed
  400 write(9,
     1'(//'' Usage of Library Entries''/'' Entry     File    Count'')')
      do I = 1, NLIB
         if (LIBUSE(I) .ne. 0) write (9,'(1X,A8,1X,A8,I6)') LIBENT(I),
     1       LIBFIL(I), LIBUSE(I)
      end do
      stop 'Finished -- Output in file "uselib".'
      end
