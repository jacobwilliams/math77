      subroutine OPTCHK(INTCHK, IOPT, ETEXT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1998-11-01 OPTCHK  Krogh  ERRSEV => MACT(2) for "mangle".
c>> 1996-05-13 OPTCHK  Krogh  Changes to use .C. and C%%.
C>> 1995-03-10 OPTCHK  Krogh  Added "abs(.) just below "do 140 ..."
C>> 1994-11-11 OPTCHK  Krogh  Declared all vars.
c>> 1993-05-17 OPTCHK  Krogh  Additions for Conversion to C.
c>> 1991-11-25 OPTCHK  Krogh  More comments, little clean up of code.
c>> 1991-10-09 OPTCHK  Krogh  More comments, little clean up of code.
c>> 1991-06-27 OPTCHK  Krogh  Initial Code.
c
c OPTCHK -- Fred T. Krogh, Jet Propulsion Lab., Pasadena, CA.
c This subroutine is intended for the use of other library routines.
c It is used to check the storage used by options for some array.
c
c INTCHK is an array that provides information on how storage has been
c   allocated.  Information is provided in groups of three words, after
c   an initial group of 4 that contain:
c    0. INTCHK(0) / 10 is the index to use for error messages, and
c       mod(INTCHK(0), 10) defines actions as follows:
c       Value  Action on Errors  Action if no errors
c         0    Prints & Stops               Returns
c         1    Prints & Returns             Returns
c         2    Prints & Stops      Prints & Returns
c         3    Prints & Returns    Prints & Returns
c        >3  Any error message will be continued, subtract 4 from the
c            value to get one of the actions above.
c    1. Contains LAST, 1 more than the index of the last location in
c       INTCHK used to store the information as described below.
c    2. The amount of storage available in the array being checked.  If
c       this is 0, it is assumed that the user would like to know what
c       size to declare for the array, and if it is -1, it is assumed
c       that a library routine is doing the check without knowing what
c       the declared size is.
c    3. The amount of storage required in this array if no options were
c       used.  (This + 1 is first loc. available for option storage.)
c   The rest should be set as follows, for k = 5, LAST, 3:
c    k-1 Option index, 0 indicates storage required by default.  (This
c        may depend on input parameters, but doesn't depend explicitly
c        on an option.)
c    k   If >0, gives the start of space used by the option.
c        If <0, gives -(amount of space required by an option), for
c               which a starting location is to be determined.
c    k+1 If preceding entry is >0, gives space required by the option.
c        Else it is assumed that the space requested is to be found,
c        and a diagnostic will be given if space can not be found.  Else
c        INTCHK(K+1) IOPT(INTCHK(k+1)) Diagnostic on successful alloc.?
c            0           ----          No
c            <0          ----          Yes
c            >0          .ne. -1       Yes
c            >0          .eq. -1       No, and IOPT(INTCHK(k+1)) is set
c                                      to starting loc. of space found.
c        When this program finds the space for an option, values of
c        INTCHK(j) for j .ge. LAST will be used.  INTCHK(k+1) is set
c        temporarily to -(space required) and INTCHK(k-1) is reduced by
c        2000000 to flag the fact that the location index must be saved
c        after INTCHK(LAST).  INTCHK(LAST) is assumed to contain 1 if
c        the largest space requested is required, and to contain
c        -(minimal space needed) if the large amount requested is not
c        essential.
c   On exit, INTCHK(1) is set = -LAST if there was some kind of error.
c   (Unless the user has called MESS this return can't happen.)
c   INTCHK(2) is set to suggest a value for the storage to be declared.
c   The remaining locations are changed as follows:
c    k-1 Negated if there was a problem with this option, or if the
c        following location was initially <0.
c    k   Starting location used or suggested for the option.
c    k+1 Last location used or suggested for the option.
c
c        In addition if values of INTCHK(j) for j .ge. LAST are used,
c        INTCHK(j), for j = LAST+1, LAST+2, ..., will be set so that
c        INTCHK(j) is equal to (one of the k's above) - 1 for an option
c        that has had a starting location assigned, and INTCHK(LAST) is
c        set to the last index value for j in the above list.
c
c IOPT  This is stored to if INTCHK(k) is < 0, see above.
c ETEXT Input text of the form 'Package_name / Argument_name$E'.  Thus
c   for argument KORD of the package DIVA, this would = 'DIVA / KORD$E'.
c
c ************************** Variable definitions **********************
c
c ERRBAD Flag to use for error severity if get an error.  17 if errors
c        are to print but not stop, 57 otherwise.
c ETEXT  (Input) Used to construct error messages, see above.
c I      Induction variable for accessing INTCHK().
c INTCHK (InOut) See above.
c IOPT   (Out) If space information is being obtained results are saved
c        here.  See above.
c ISTRT  Starting index for placing options with unknown locations.
c KEX    Points to the last place in INTCHK where indices of entries
c        that have had locations determined here are stored.
c L      Temporary index.
c LAST   First free location in INTCHK.  (=INTCHK(1))
c LNEG   Index of the first INTCHK entry that is to be positioned.
c LOPT   Location in IOPT to get location of INTCHK entry that is
c        positioned.
c LTXTAx Variables setup by PMESS defining the locations in MTXTAA where
c        various error messages start.
c LTXTEE Location in MTXTAA where data in ETEXT is stored.
c LTXTZZ Number of characters available for saving ETEXT in MTXTAA.
c LWANT  -Number of locations wanted by INTCHK entry being positioned.
c MACT   Vector used to specify error printing actions, see MESS.
c        MACT(2) flags error/diagnostic level.  = 0 none; 07 is used to
C        get diagnostics only; and ERRBAD otherwise.
c MEEMES Parameter specifying that error message is to be printed.
c MEIDAT Parameter specifying location in INTCHK to start printing
c        integer in MESS.
c MEIMAT Parameter specifying an integer matrix is to be printed.
c MENTXT Parameter specifying location in INTCHK to start printing
c        text from MTXTAA in MESS.
c MERET  Parameter specifying the end of an error message.
c MESS   Routine to print error and diagnostc messages.
c METEXT Parameter specifying that text is to be printed.
c MI     Temporary index used to save in acceptable location.
c MTXTAA Used to contain error message text and instructions, see MESS.
c MTXTAx Character variables setup by PMESS and equivalenced into ETEXT
c        used to contain parts of the error messages.
c MTXTZZ As for MTXTAx, except not setup by PMESS.  Used to hold text
c        from ETEXT.
c MV     Temporary, which contains value associated with INTCHK(MI).
c N      Temporary value.
c NERBAD Array telling what to do concerning errrors.  ERRBAD is set
c        from NERBAD(mod(INTCHK(0), 10)), and the default value for
c        MACT(2) is set from NERBAD(INTCHK(0)+4).
c
c ************************** Variable Declarations *********************
c
      integer INTCHK(0:*), IOPT(*)
      character ETEXT*(*)
      integer I, ISTRT, KEX, L, LAST, LNEG, LOPT, LWANT, MI, MV, N
c Declarations for error messages.
      integer MENTXT, MEIDAT, MECONT, MERET, MEEMES, METEXT, MEIMAT,
     1   LTXTEE, LTXEND
      parameter (MENTXT =23)
      parameter (MEIDAT =24)
      parameter (MECONT =50)
      parameter (MERET =51)
      parameter (MEEMES =52)
      parameter (METEXT =53)
      parameter (MEIMAT =58)
      integer MACT(16), ERRBAD, NERBAD(0:7)
c
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA OPTCHK$B
cAB "Option #" is negated if option needs attention.$N
c   "Option 0" is for space not associated with a specific option.$N
c   "First Loc." is negated if user did not set value.$N
c   Space avail. = $I; all options must have first loc. > $I$E
cAC Option #$HFirst Loc.$HLast Loc.$E
cAD From subprogram/argument: $B
cAE Space for ETEXT.$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE
      parameter (LTXTAA=  1,LTXTAB=  9,LTXTAC=233,LTXTAD=267,LTXTAE=295)
      character MTXTAA(2) * (156)
c                          Next 4 lines not automatically generated
c%%    #define LTXTEE  137
      parameter (LTXTEE = LTXTAE - 156 - 2)
      parameter (LTXEND = 156)
C
      data MTXTAA/'OPTCHK$B"Option #" is negated if option needs attenti
     *on.$N"Option 0" is for space not associated with a specific option
     *.$N"First Loc." is negated if user di','d not set value.$NSpace av
     *ail. = $I; all options must have first loc. > $I$EOption #$HFirst$
     * Loc.$HLast Loc.$EFrom subprogram/argument: $BSpace for ETEXT.$E'/
c
c                      1 2 3      4       5  6       7      8       9
      data MACT / MEEMES,0,1,LTXTAD, MEIDAT, 2, MENTXT,LTXTAB, METEXT,
     1   MEIMAT,3,3,0,LTXTAC,-1, MERET /
c            10    13     14 15     16
      data NERBAD / 57, 17, 57, 17, 0, 0, 7, 7 /
c
c *************************** Start of Executable Code *****************
c
      MACT(3) = INTCHK(0) / 10
      MACT(16)=MERET
      I = INTCHK(0) - 10*MACT(3)
      if (I .gt. 3) then
         I = I - 4
         MACT(16) = MECONT
      end if
      ERRBAD = NERBAD(I)
      MACT(2) = NERBAD(I+4)
      LAST = INTCHK(1)
      KEX = LAST
   20 LNEG = 0
      do 100 I = 5, LAST, 3
c       Loop to sort on the low indices -- Inefficient algorithm to keep
c       code short -- LAST should never be very big.
         MI = I
         MV = INTCHK(I)
         do 50 L = I+3, LAST, 3
c                                    Find mimimum from here down.
            if (INTCHK(L) .lt. MV) then
               MI = L
               MV = INTCHK(L)
            end if
   50    continue
         if (MI .ne. I) then
c                                   Interchange to get low at top.
            do 70 L = -1, 1
               N = INTCHK(I+L)
               INTCHK(I+L) = INTCHK(MI+L)
               INTCHK(MI+L) = N
   70       continue
         end if
         if (MV .lt. 0) then
c                Save loc. of last entry that needs space to be found.
            LNEG = I
         else if (LNEG .eq. 0) then
c        Entry I and previous entries are in their correct sorted order.
            if (INTCHK(I+1) .lt. 0) then
               if (INTCHK(I-1) .lt. -1000000) then
                  INTCHK(I-1) = INTCHK(I-1) + 2000000
                  INTCHK(I+1) = -INTCHK(I+1)
c                            Save INTCHK index defining allocated space.
                  KEX = KEX + 1
                  INTCHK(KEX) = I - 1
               else
c                   Error -- Got request for a negative amount of space.
                  MACT(2) = ERRBAD
                  INTCHK(I-1) = -abs(INTCHK(I-1))
               end if
            end if
c                Save final location used by the option.
            INTCHK(I+1) = INTCHK(I) + INTCHK(I+1) - 1
            if (INTCHK(I) .le. INTCHK(I-2)) then
c                                           Error -- options overlap.
               INTCHK(I-1) = -abs(INTCHK(I-1))
               MACT(2) = ERRBAD
            end if
         end if
  100 continue
      if (LNEG .ne. 0) then
c     Find spaces that need to be allocated, starting with the smallest.
         ISTRT = LNEG
         I = LNEG
  120    LWANT = INTCHK(LNEG)
         LOPT = INTCHK(LNEG+1)
         if (I .eq. LNEG) then
c                         Make fake entry to get started.
            INTCHK(LNEG) = 1
            INTCHK(LNEG+1) = INTCHK(3)
         end if
         do 140 ISTRT = ISTRT, LAST-3, 3
            if(INTCHK(I)+abs(INTCHK(I+1))-LWANT .lt. INTCHK(ISTRT+3))
     1         go to 150
            I = ISTRT + 3
  140    continue
  150    INTCHK(LNEG) = INTCHK(I) + abs(INTCHK(I+1))
         if (LOPT .ne. 0) then
            if (LOPT .gt. 0) then
               if (IOPT(LOPT) .eq. -1) then
                  IOPT(LOPT) = INTCHK(LNEG)
                  go to 160
               end if
            end if
c                     Error -- IOPT not expecting starting loc.
            INTCHK(LNEG-1) = -abs(INTCHK(LNEG-1))
            MACT(2) = ERRBAD
         end if
  160    INTCHK(LNEG+1) = LWANT
         INTCHK(LNEG-1) = INTCHK(LNEG-1) - 2000000
         if (LNEG .lt. 8) go to 20
         I = LNEG
         LNEG = LNEG - 3
         go to 120
      end if
      if (INTCHK(LAST-1) .gt. INTCHK(2)) then
         if (INTCHK(2) .lt. 0) go to 180
         if (LAST .ne. KEX) then
            if (INTCHK(KEX) .eq. LAST - 3) then
               if (INTCHK(LAST) .le. 0) then
                  if (INTCHK(LAST-2)-INTCHK(LAST)-1 .le. INTCHK(2)) then
                     INTCHK(LAST-1) = INTCHK(2)
                     go to 180
                  end if
               end if
            end if
         end if
         INTCHK(LAST-3) = -abs(INTCHK(LAST-3))
         MACT(2) = ERRBAD
      end if
  180 if (LAST .ne. KEX) INTCHK(LAST) = KEX
      if (MACT(2) .gt. 0) then
  190    if (LAST .ne. KEX) then
            do 200 I = LAST+1, abs(KEX)
               INTCHK(INTCHK(I)+1) = -INTCHK(INTCHK(I)+1)
  200       continue
            if (KEX .lt. 0) go to 210
            KEX = -KEX
         end if
         MACT(13) = (LAST - 4) / 3
c%%       strcpy(&mtxtaa[1][LTXTEE-1], etext);
         MTXTAA(2)(LTXTEE:LTXEND)=ETEXT(1:)
         call MESS(MACT, MTXTAA, INTCHK(1))
         if (MACT(2) .gt. 10) INTCHK(1) = -LAST
         if (LAST .ne. KEX) go to 190
      end if
  210 INTCHK(2) = INTCHK(LAST-1)
      return
      end
