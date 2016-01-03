      subroutine IMATPR(A, IDIMA, M, N, TEXT, LWIDTH, LUNIT        )
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 2000-12-01 IMATPR Krogh Removed unused parameter METXTF.
C>> 1994-11-11 IMATPR Krogh Declared all vars.
c>> 1994-10-20 IMATPR Krogh Added M77CON code.
c>> 1992-05-03 IMATPR Krogh Convert to use MESSFT for Fortran text.
c>> 1992-04-08 IMATPR Krogh Replaced dummy K with MACT in ?MESS calls.
c>> 1991-11-22 IMATPR F. Krogh   Initial code
c
c++I DEFAULT MSTOR=2, MACT1D=7, TAIL=", LUNIT"
c++  DEFAULT MSTOR=4, MACT1D=9, TAIL=", LUNIT, NUMDIG"
c++  REPLACE ", LUNIT        " = TAIL
c--I replaces "?": ?MATPR, ?MESS
c
c All versions require MESS and MESSFT which is in MESS.
c
c     *****     Formal Arguments     ***********************************
c
c A      Matrix to be output, A = A(I,J), I = 1, M; J = 1, N
c IDIMA  Declared row dimension of A
c N      Number of columns in the matrix.
c M      Number of rows in the matrix.
c TEXT   a variable length character type that gives a message to print.
c LWIDTH Line width in characters.  If this or any of the following
c        parameter are < 0, then current defaults set in MESS are used.
c LUNIT  Logical unit number.  (0 prints to the standard output.)
c NUMDIG Number of significant digits to print (not used in integer).
c
c  ******************** Parameter for interfacing to MESS  *************
c
      INTEGER MEMLIN,MEMUNI,METDIG,MECONT,MERET,MACT1D,MSTOR
      parameter (MEMLIN =13)
      parameter (MEMUNI =15)
      parameter (METDIG =22)
      parameter (MECONT =50)
      parameter (MERET  =51)
c++ Substitute for MACT1D, MSTOR below
      parameter (MACT1D=7, MSTOR=2)
c
      integer           N, LWIDTH, LUNIT        , IDIMA, M
      character*(*)     TEXT
      character*2       TTEXT(1)
      integer           K, MACT(9), MACT1(MACT1D)
c--I Next line special: I
      integer           A(IDIMA, N)
c++  Code for {I} is active
      integer MEIMAT
      parameter (MEIMAT = 58)
      data MACT1 / MEIMAT, 0, 0, 0, 0 ,0, MERET /
c++  Code for ~{I} is inactive
C      integer MEFMAT
C      parameter (MEFMAT = 62)
C      data MACT1 / METDIG, 0, MEFMAT, 0, 0, 0, 0 ,0, MERET /
C      MACT1(2) = max(0, NUMDIG)
c++  End
      MACT1(MSTOR) = IDIMA
      MACT1(MSTOR+1) = max(M, 0)
      MACT1(MSTOR+2) = max(N, 0)
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
      call MESSFT(MACT, TEXT)
c++  Code for {I} is active
      call MESS(MACT1, TTEXT, A)
c++  Code for ~{I} is inactive
C      call IMESS(MACT1, TTEXT, MACT, A)
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
