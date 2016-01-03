c      program DRSIVDB
c>> 2009-11-03 DRSIVDB  Krogh -- Used option 11
c>> 2001-07-16 DRSIVDB  Krogh -- Declared type for paramter TOL.
c>> 2001-05-25 DRSIVDB  Krogh -- Added comma to format.
c>> 1996-06-14 DRSIVDB  Krogh  Small change in output format
c>> 1994-09-12 DRSIVDB  Krogh  Fixed for CHGTYP.
c>> 1993-07-18 DRSIVDB  Krogh  Fixed to get same output in S.P. and D.P.
c>> 1993-05-05 DRSIVDB  Krogh  Adjusted to simplify conversion to C.
c>> 1989-05-04 Fred T. Krogh
c
c--S replaces "?": DR?IVDB, ?IVA, ?IVADB, ?IVAF, ?IVAO, ?MESS
c
c Sample driver for SIVA --  Set up to solve two second order equations.
c Tests debug output
c
      integer INEQ, IFDIM, IYDIM
      parameter (INEQ=2, IFDIM=16*INEQ+1, IYDIM=4*INEQ)
      integer          NEQ, KORD(6), IOPT(7)
c++ Code for SHORT_LINE is inactive
c      integer          MACT(5)
c++ END
      real             TSPECS(4), T, H, DELT, TFINAL
      real             F(IFDIM), Y(IYDIM)
      external SIVAO, SIVAF
      equivalence (TSPECS(1), T), (TSPECS(2), H), (TSPECS(3), DELT),
     1   (TSPECS(4), TFINAL)
      real             TOL
      integer NDIG
c++S Default NDIG = 4
c++  Default NDIG = 10
c++ Substitute for NDIG below
      parameter (NDIG = 4 )
      parameter (TOL = 10.E0 **(-NDIG))
c
c Parameters for setting up message processor to specify the number of
c digits, and the line length.
      integer MEDDIG, MEMLIN, MERET
      parameter (MEDDIG=12, MEMLIN=13, MERET=51)
c
      data   NEQ,    T,    H,                   DELT, TFINAL /
     1         2, 0.E0, 1.E0, 6.283185307179586477E0,   2.E1 /,
     2       Y(1), Y(2), Y(3), Y(4) /
     3       1.E0, 0.E0, 0.E0, 1.E0 /
c
c Set option for error control, local absolute error < 1.E-10.
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
         call SIVA(TSPECS,Y,F,KORD,NEQ,SIVAF,SIVAO,4,IYDIM,IFDIM,6,IOPT)
         if (KORD(1) .NE. 1) go to 100
c++ Code for SHORT_LINE is inactive
c      MACT(1) = MEDDIG
c      MACT(2) = 7
c      MACT(3) = MEMLIN
c      MACT(4) = 79
c      MACT(5) = MERET
c      call SMESS(MACT, ' ', MACT, F)
c++ END
      call SIVADB(44, TSPECS, Y, F, KORD, ' Test of SIVADB')
      stop
      end
 
      subroutine SIVAF(T, Y, F, KORD)
c
c Sample derivative subroutine for use with SIVA
c This evaluates derivatives for a simple two body problem.
c
      integer          KORD
      real             T, F(2), Y(4)
      real             TP
c
c Evaluate the derivatives
c
      TP = Y(1)*Y(1) + Y(3)*Y(3)
      TP = 1.E0 / (TP * SQRT(TP))
      F(1) = -Y(1) * TP
      F(2) = -Y(3) * TP
      return
      end
 
      subroutine SIVAO(TSPECS, Y, F, KORD)
c
c Sample output subroutine for use with SIVA.
c This subroutine gives output for a simple two body problem.
c
      integer          KORD
      real             TSPECS(4), Y(4), F(2)
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
