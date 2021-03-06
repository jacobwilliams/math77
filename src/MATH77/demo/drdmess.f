c     program DRDMESS
c>> 1998-11-02 DRDMESS Krogh  Typed all variables.
c>> 1994-09-09 DRDMESS Krogh  Added CHGTYP code.
c>> 1993-06-25 DRDMESS Krogh  Additions for Conversion to C.
c>> 1992-03-24 DRDMESS Krogh -- Initial Code.
c
c--D replaces "?": DR?MESS, ?MESS
c
      integer          I, IDAT(3), ILOC(4), MACT(5), MACT1(3)
      double precision FDAT(2)
      logical          LIN40
c
      integer MERET, MEEMES, MESTOP, MEELIN
      parameter (MERET  =51)
      parameter (MEEMES =52)
      parameter (MESTOP =20)
      parameter (MEELIN =14)
      integer MLOC(4)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DRDMESS$B
cAB Description of error.  (With one real)  FDATA1 = $F.$E
cAC Description of 2nd error.  (With no data)$E
cAD Description of 3rd error.  (With one integer)  IDATA1 = $I.$E
cAE Description of 4th error.  (With two integers and one real)$N
c   IDATA1 = $I, IDATA2 = $I, FDATA2 = $F.$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE
      parameter (LTXTAA=  1,LTXTAB= 10,LTXTAC= 64,LTXTAD=107,LTXTAE=168)
      character MTXTAA(2) * (135)
      data MTXTAA/'DRDMESS$BDescription of error.  (With one real)  FDAT
     *A1 = $F.$EDescription of 2nd error.  (With no data)$EDescription o
     *f 3rd error.  (W','ith one integer)  IDATA1 = $I.$EDescription of$
     * 4th error.  (With two integers and one real)$NIDATA1 = $I, IDATA2
     * = $I, FDATA2 = $F.$E '/
c  --------- End of code generated by PMESS from DRDMESS.ERR -----------
      data MLOC /LTXTAB,LTXTAC,LTXTAD,LTXTAE/
c
c                 1       2 3 4      5
      data MACT / MEEMES,25,0,0, MERET /
      data MACT1 / MEELIN, 40, MERET /
      data FDAT / 1.7D-12, -12.3456789D0 /
      data IDAT / 17, -178, 4 /
      data ILOC / 1, 1, 2, 2 /
c
      LIN40 = .false.
c                        Loop to print error messages.
   10 do 100 I = 1, 4
         MACT(3) = I
         MACT(4) = MLOC(I)
         call DMESS(MACT, MTXTAA, IDAT(ILOC(I)), FDAT(ILOC(I)))
  100 continue
      if (LIN40) stop
c                        Change the line length to 40 and do it again.
      call MESS(MACT1, MTXTAA, IDAT)
      LIN40 = .true.
      go to 10
      end
