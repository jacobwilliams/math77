      program DRSIVA
c>> 2010-06-09 DRSIVA  Krogh Used parameters for all dimenssions.
c>> 2001-05-25 DRSIVA  Krogh Minor change for making .f90 version.
c>> 1996-06-14 DRSIVA  Krogh  Small change in output format
c>> 1994-11-02 DRSIVA  Krogh  Changes to use M77CON
c>> 1994-07-18 DRSIVA  Krogh   Last change.
c--S replaces "?": DR?IVA, ?IVA, ?IVAF, ?IVAO
c Sample driver for SIVA --  Set up to solve two second order equations.
c
      integer INEQ, IFDIM, IKDIM, ITDIM, IYDIM
      parameter (INEQ=2,IFDIM=16*INEQ+1,IKDIM=6,ITDIM=4,IYDIM=4*INEQ)
      integer          NEQ, KORD(IKDIM), IOPT(6)
c--S Next line special: P=>D, X=>Q
      real             TSPECS(ITDIM), Y(IYDIM), T, H, DELT, TFINAL
      integer NDIG
      real             F(IFDIM), TOL
      equivalence (TSPECS(1), T), (TSPECS(2), H), (TSPECS(3), DELT),
     1  (TSPECS(4), TFINAL)
      external SIVAO, SIVAF
c++SP Default NDIG = 4
c++  Default NDIG = 10
c++ Substitute for NDIG below
      parameter (NDIG = 4 )
      parameter (TOL = 10.E0 **(-NDIG))
c
      data   NEQ,    T,    H,                   DELT, TFINAL /
     1         2, 0.E0, 1.E0, 6.283185307179586477E0,   2.E1 /,
     2       Y(1), Y(2), Y(3), Y(4) /
     3       1.E0, 0.E0, 0.E0, 1.E0 /
c
c Set option for error control, local absolute error < TOL.
      data   IOPT(1), IOPT(2), IOPT(3) /
     1            16,       6,   3 /,
c Group the system to be treated as a single unit, set tolerance value
     2                KORD(6), F(3)/
     3                      2,  TOL/
c Set option for second order equations
      data   IOPT(4), IOPT(5) /
     1            17,       2 /
c  Flag end of options
      data   IOPT(6) / 0/
c
c Do the integration
c
      KORD(1) = 0
  100 continue
      call SIVA(TSPECS, Y, F, KORD, NEQ, SIVAF, SIVAO,
     1  ITDIM, IYDIM, IFDIM, IKDIM, IOPT)
      if (KORD(1) .NE. 1) go to 100
      stop
      end
 
      subroutine SIVAF(T, Y, F, KORD)
c Sample derivative subroutine for use with SIVA
c This evaluates derivatives for a simple two body problem.
c
      integer          KORD
c--S Next line special: P=>D, X=>Q
      real             T, Y(4), TP
      real             F(2)
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
c Sample output subroutine for use with SIVA.
c This subroutine gives output for a simple two body problem.
c
      integer          KORD
c--S Next line special: P=>D, X=>Q
      real             TSPECS(4), Y(4)
      real             F(2)
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
