      subroutine DVECPR(V, N, TEXT, LWIDTH, LUNIT, NUMDIG)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2010-02-22 DVECPR Krogh Initliazed TTEXT to avoid compiler worrirs.
C>> 2000-12-01 DVECPR Krogh Removed unused parameter METXTF.
C>> 1994-11-11 DVECPR Krogh Declared all vars.
c>> 1994-10-20 DVECPR Krogh Added M77CON code.
c>> 1992-05-03 DVECPR Krogh Convert to use MESSFT for Fortran text.
c>> 1992-04-08 DVECPR Krogh Replaced dummy K with MACT in ?MESS call.
c>> 1991-11-22 DVECPR F. Krogh   Initial code
c++I DEFAULT MACT1D=3, TAIL=", LUNIT"
c++  DEFAULT MACT1D=5, TAIL=", LUNIT, NUMDIG"
c++  REPLACE ", LUNIT, NUMDIG" = TAIL
c--D replaces "?": ?VECPR, ?MESS
c
c     *****     Formal Arguments     ***********************************
c
c V      Vector to be output, V = V(I), I = 1, N
c N      Number of vector components.
c TEXT   a variable length character type that gives a message to print.
c LWIDTH Line width in characters.  If this or any of the following
c        parameter are < 0, then current defaults set in MESS are used.
c LUNIT  Logical unit number.  (0 prints to the standard output.)
c NUMDIG Number of significant digits to print (Not used for integer).
c
c  ******************** Parameter for interfacing to MESS  *************
c
      integer MEMLIN, MEMUNI, METDIG, MECONT, MERET, MACT1D
      parameter (MEMLIN =13)
      parameter (MEMUNI =15)
      parameter (METDIG =22)
      parameter (MECONT =50)
      parameter (MERET  =51)
c++ Substitute for MACT1D below
      parameter (MACT1D = 5)
c
      integer           N, LWIDTH, LUNIT, NUMDIG
      character*(*)     TEXT
      character*2       TTEXT(1)
      data TTEXT(1) / '  ' /
      integer           K, MACT(9), MACT1(MACT1D)
c--D Next line special: I
      double precision  V(N)
c++  Code for {I} is inactive
C      integer MEIVEC
C      parameter (MEIVEC=57)
C      data MACT1 / MEIVEC, 0, MERET /
C      MACT1(2) = max(N, 0)
c++  Code for ~{I} is active
      integer MEFVEC
      parameter (MEFVEC=61)
      data MACT1 / METDIG, 0, MEFVEC, 0, MERET /
      MACT1(2) = max(0, NUMDIG)
      MACT1(4) = max(N, 0)
c++  End
c
      K = 1
      if (LWIDTH .gt. 20) then
         MACT(1) = -MEMLIN
         MACT(3) = MEMLIN
         MACT(4) = LWIDTH
         K = 5
      end if
      if (LUNIT .ge. 0) then
         MACT(K) = -MEMUNI
         MACT(K+2) = MEMUNI
         MACT(K+3) = LUNIT
         K = K + 4
      end if
      MACT(K) = MECONT
      if (K .gt. 1) call MESSFT(MACT, TEXT)
c++  Code for {I} is inactive
C      call MESS(MACT1, TTEXT, V)
c++  Code for ~{I} is active
      call DMESS(MACT1, TTEXT, MACT, V)
c++  End
      if (MACT(1) .lt. 0) then
c                           Restore MESS parameters to original state
         MACT(1) = - MACT(1)
         MACT(3) = MERET
         if (MACT(5) .lt. 0) then
            MACT(3) = MEMUNI
            MACT(4) = MACT(6)
            MACT(5) = MERET
         end if
         call MESS(MACT, TTEXT, MACT)
      end if
      return
      end
