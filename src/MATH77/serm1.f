      SUBROUTINE SERM1(SUBNAM,INDIC,LEVEL,MSG,LABEL,VALUE,FLAG)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-10-20 SERM1  Krogh  Changes to use M77CON
C>> 1994-04-20 SERM1  CLL Edited to make DP & SP files similar.
C>> 1985-08-02 SERM1  Lawson  Initial code.
c--S replaces "?": ?ERM1, ?ERV1
C
      CHARACTER*(*) SUBNAM,MSG,LABEL
      CHARACTER*1 FLAG
      integer INDIC, LEVEL
      REAL             VALUE
C
      CALL ERMSG(SUBNAM,INDIC,LEVEL,MSG,',')
      CALL SERV1(LABEL,VALUE,FLAG)
C
      RETURN
      END

