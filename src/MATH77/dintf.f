      SUBROUTINE DINTF (ANSWER, WORK, IOPT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1996-04-26 DINTF  Krogh  Added ANSWER=0, for getting prototypes.
C>> 1994-10-20 DINTF  Krogh  Changes to use M77CON
C>> 1990-01-23 DINTF  CLL Added type stmts. Avoids msgs from compilers.
C>> 1988-06-07 DINTF  Snyder  Replaced " by '.
C>> 1987-08-14 DINTF  Snyder  Initial code.
c--D replaces "?": ?INTF, ?INT
C
C     Dummy DINTF subprogram that prints
C     'You have neglected either to write the DINTF subprogram'
C     'or to select reverse communication before invoking DINT.'
C     and then STOPs.
C
      double precision ANSWER, WORK(*)
      integer IOPT(*)
      ANSWER = 0.D0
      PRINT *,'You have neglected either to write the DINTF subprogram'
      PRINT *,'or to select reverse communication before invoking DINT*'
      STOP
      END
