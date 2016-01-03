      program DRDIVX
c>> 2009-11-03 DRDIVX  Krogh -- Used option 11
c>> 2006-04-10 DRDIVX  Krogh -- * dim for Y and F in divao.
c>> 2001-07-16 DRDIVX  Krogh -- Declared type for parameter TOL.
c>> 2001-05-25 DRDIVX  Krogh Minor change for making .f90 version.
c>> 1996-06-14 DRDIVX  Krogh   Small change in output format
c>> 1994-09-12 DRDIVX  Krogh   Fixed for CHGTYP.
c>> 1994-08-19 DRDIVX  Krogh   Specified no. of digits in the output.
c>> 1993-05-05 DRDIVX  Krogh   Adjusted to simplify conversion to C.
c>> 1992-03-10 DRDIVX Fred T. Krogh
c
c--D replaces "?": DR?IVX, ?IVA, ?IVACO, ?IVADB, ?IVAF, ?IVAG, ?IVAO,
c--&              ?MESS
c
c Sample driver for DIVA --  Set up to solve two second order equations.
c Test DIVAG, DIVADB, and DIVACO
c
      integer INEQ, IFDIM, IYDIM
      parameter (INEQ=2, IFDIM=16*INEQ+1, IYDIM=4*INEQ)
      integer          NEQ, KORD(6), IOPT(12)
      integer          ID(5)
      double precision TSPECS(4), T, H, DELT, TFINAL
      double precision F(IFDIM), Y(IYDIM)
      double precision RD(3)
      external DIVAO, DIVAF
      equivalence (TSPECS(1), T), (TSPECS(2), H), (TSPECS(3), DELT),
     1  (TSPECS(4), TFINAL)
      double precision TOL
      integer NDIG
c++S Default NDIG = 4
c++  Default NDIG = 10
c++ Substitute for NDIG below
      parameter (NDIG = 10)
      parameter (TOL = 10.D0 **(-NDIG))
c
c Parameters for setting up message processor to specify no. of digits.
      integer MEDDIG, MERET
      parameter (MEDDIG=12, MERET=51)
      integer MACT(3)
      character MDUMMY(1)*2
      data MACT / MEDDIG, 7, MERET /
      data MDUMMY / '  ' /
c
      data   NEQ,    T,    H,   DELT, TFINAL /
     1         2, 0.D0, 1.D0, 100.D0,   4.D0 /,
     2       Y(1), Y(2), Y(3), Y(4) /
     3       1.D0, 0.D0, 0.D0, 1.D0 /
c
c Set option for error control, local absolute error < 1.D-10.
      data   IOPT(1), IOPT(2), IOPT(3) /
     1            16,       6,   3 /,
c Group the system to be treated as a single unit, set tolerance value
     2    KORD(6), F(3) /
     3          2, TOL /
c Set option for second order equations
      data   IOPT(4), IOPT(5) /
     1            17,       2 /
c Set option for a G-Stop
      data   IOPT(6), IOPT(7) /
     1             6,       1 /
c Get diagnostic output for 5 steps to test MESS
      data   IOPT(8), IOPT(9), IOPT(10) /
     1            10,       5,        0 /
c Set option to initialize some space to 0, and end of options.
      data IOPT(11), IOPT(12) / 11, 0 /
c
 1000 format(' KEMAX=',I1, '  KSTEP=', I4, '  NUMDT=', I2, '  EMAX=',
     1   1P,E14.7)
c
c Specify number of digits in the output.
      call DMESS(MACT, MDUMMY, MACT, F)
c
c Do the integration
c
      KORD(1) = 0
  100 continue
         call DIVA(TSPECS,Y,F,KORD,NEQ,DIVAF,DIVAO,4,IYDIM,IFDIM,6,IOPT)
         if (KORD(1) .NE. 1) go to 100
         call DIVADB(45, TSPECS, Y, F, KORD, '0Sample of DIVA Debug')
         call DIVACO(ID, RD)
         print 1000, ID(1), ID(2), ID(3), RD(1)
      stop
      end

      subroutine DIVAF(T, Y, F, KORD)
c
c Sample derivative subroutine for use with DIVA
c This evaluates derivatives for a simple two body problem.
c
      integer          KORD(*)
      double precision T(1), F(2), Y(4)
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
      integer          KORD(*), IFLAG, NSTOP
      double precision TSPECS(4), Y(*), F(*), G6(1), GT6(1)
      save             G6, GT6
c
 1000 format (12X,
     1   'RESULTS FOR A SIMPLE 2-BODY PROBLEM'//
     2   5X, 'T/IFLAG', 10X, 'U/V', 11X, 'UP/VP', 9X, 'UPP/VPP')
 1001 format (1P,SP,4E15.6 / 8X,I3, 4X, 3E15.6/' ')
c
c Do the output
c
      IFLAG = 0
      if (KORD(1) .EQ. 1) then
        write (*, 1000)
      end if
      if (KORD(1) .EQ. 6) then
  100     G6(1) = Y(3)
          call DIVAG(TSPECS, Y, F, KORD, IFLAG, NSTOP, G6, GT6)
          if ((IFLAG .EQ. 1) .OR. (IFLAG .EQ. 3)) return
          if (IFLAG .EQ. 4) go to 100
      end if
      write (*,1001) TSPECS(1),Y(1),Y(2),F(1),IFLAG,Y(3),Y(4),F(2)
      return
      end
