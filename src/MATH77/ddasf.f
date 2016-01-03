      subroutine DDASF(T,Y,YPRIME,DELTA,D,LDD,C,IRES,RWORK,IWORK)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2008-10-24 ddasf Krogh Shortened line 1 to 72 cols., declared IRES.
c>> 2008-10-01 ddasf Hanson added row dimension argument.
c>> 2001-12-15 ddasf Krogh  Initial code.
c--D Replaces "?": ?dasf, ?daslx
c
c This is a dummy routine that can be used when all communication
c with DDASLX uses reverse communication.  It should never actually
c get called..
c
      double precision T, Y(*), YPRIME(*), DELTA(*), D(*), C, RWORK(*)
      integer LDD, IRES, IWORK(*)
      integer MACT(5)
      data MACT / 52, 8, 0, 1, 51 /

      call MESS(MACT, "DDASLX$BYou either need to code your own "//
     &  "subroutine DDASF or you did not set up your reverse "//
     &   "communication correctly.", IWORK)
      return
      end
