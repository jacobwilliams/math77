      SUBROUTINE SINTF (ANSWER, WORK, IOPT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1996-04-26 SINTF  Krogh  Added ANSWER=0, for getting prototypes.
C>> 1994-10-20 SINTF  Krogh  Changes to use M77CON
C>> 1990-01-23 SINTF  CLL Added type stmts. Avoids msgs from compilers.
C>> 1988-06-07 SINTF  Snyder  Replaced " by '.
C>> 1987-08-14 SINTF  Snyder  Initial code.
c--S replaces "?": ?INTF, ?INT
C
C     Dummy SINTF subprogram that prints
C     'You have neglected either to write the SINTF subprogram'
C     'or to select reverse communication before invoking SINT.'
C     and then STOPs.
C
      real             ANSWER, WORK(*)
      integer IOPT(*)
      ANSWER = 0.E0
      PRINT *,'You have neglected either to write the SINTF subprogram'
      PRINT *,'or to select reverse communication before invoking SINT*'
      STOP
      END
