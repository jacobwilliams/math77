c      program DRDIVDB
c>> 2009-11-03 DRDIVDB  Krogh -- Used option 11
c>> 2001-07-16 DRDIVDB  Krogh -- Declared type for paramter TOL.
c>> 2001-05-25 DRDIVDB  Krogh -- Added comma to format.
c>> 1996-06-14 DRDIVDB  Krogh  Small change in output format
c>> 1994-09-12 DRDIVDB  Krogh  Fixed for CHGTYP.
c>> 1993-07-18 DRDIVDB  Krogh  Fixed to get same output in S.P. and D.P.
c>> 1993-05-05 DRDIVDB  Krogh  Adjusted to simplify conversion to C.
c>> 1989-05-04 Fred T. Krogh
c
c--D replaces "?": DR?IVDB, ?IVA, ?IVADB, ?IVAF, ?IVAO, ?MESS
c
c Sample driver for DIVA --  Set up to solve two second order equations.
c Tests debug output
c
      integer INEQ, IFDIM, IYDIM
      parameter (INEQ=2, IFDIM=16*INEQ+1, IYDIM=4*INEQ)
      integer          NEQ, KORD(6), IOPT(7)
c++ Code for SHORT_LINE is inactive
c      integer          MACT(5)
c++ END
      double precision TSPECS(4), T, H, DELT, TFINAL
      double precision F(IFDIM), Y(IYDIM)
      external DIVAO, DIVAF
      equivalence (TSPECS(1), T), (TSPECS(2), H), (TSPECS(3), DELT),
     1   (TSPECS(4), TFINAL)
      double precision TOL
      integer NDIG
c++S Default NDIG = 4
c++  Default NDIG = 10
c++ Substitute for NDIG below
      parameter (NDIG = 10)
      parameter (TOL = 10.D0 **(-NDIG))
c
c Parameters for setting up message processor to specify the number of
c digits, and the line length.
      integer MEDDIG, MEMLIN, MERET
      parameter (MEDDIG=12, MEMLIN=13, MERET=51)
c
      data   NEQ,    T,    H,                   DELT, TFINAL /
     1         2, 0.D0, 1.D0, 6.283185307179586477D0,   2.D1 /,
     2       Y(1), Y(2), Y(3), Y(4) /
     3       1.D0, 0.D0, 0.D0, 1.D0 /
c
c Set option for error control, local absolute error < 1.D-10.
      data   IOPT(1), IOPT(2), IOPT(3) /
     1            16,       6,   3 /,
c Group the system to be treated as a single unit, set tolerance value
     2                KORD(6), F(3)/
     3                      2, TOL/
c Set option for second order equations
      data   IOPT(4), IOPT(5) /
     1            17,       2 /
c Set option to initialize some space to 0, and end of options.
      data IOPT(6), IOPT(7) / 11, 0 /

c
c
c Do the integration
c
      KORD(1) = 0
  100 continue
         call DIVA(TSPECS,Y,F,KORD,NEQ,DIVAF,DIVAO,4,IYDIM,IFDIM,6,IOPT)
         if (KORD(1) .NE. 1) go to 100
c++ Code for SHORT_LINE is inactive
c      MACT(1) = MEDDIG
c      MACT(2) = 7
c      MACT(3) = MEMLIN
c      MACT(4) = 79
c      MACT(5) = MERET
c      call DMESS(MACT, ' ', MACT, F)
c++ END
      call DIVADB(44, TSPECS, Y, F, KORD, ' Test of DIVADB')
      stop
      end

      subroutine DIVAF(T, Y, F, KORD)
c
c Sample derivative subroutine for use with DIVA
c This evaluates derivatives for a simple two body problem.
c
      integer          KORD
      double precision T, F(2), Y(4)
      double precision TP
c
c Evaluate the derivatives
c
      TP = Y(1)*Y(1) + Y(3)*Y(3)
      TP = 1.D0 / (TP * SQRT(TP))
      F(1) = -Y(1) * TP
      F(2) = -Y(3) * TP
      return
      end

      subroutine DIVAO(TSPECS, Y, F, KORD)
c
c Sample output subroutine for use with DIVA.
c This subroutine gives output for a simple two body problem.
c
      integer          KORD
      double precision TSPECS(4), Y(4), F(2)
 1000 format (12X,
     1   'RESULTS FOR A SIMPLE 2-BODY PROBLEM'//
     2   8X, 'T', 13X, 'U/V', 11X, 'UP/VP', 9X, 'UPP/VPP')
 1001 format (1P,SP,4E15.6 / 15X, 3E15.6/' ')
c
c Do the output
c
      if (KORD .EQ. 1) then
        write (*, 1000)
      end if
      write (*, 1001) TSPECS(1), Y(1), Y(2), F(1), Y(3), Y(4), F(2)
c
      return
      end
