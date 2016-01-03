      subroutine UMESS(TEXT, MACT, IVAR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-25 UMESS  Krogh  Included full set of parameters.
c>> 1992-01-29 UMESS  Krogh  Initial code.
c
c Dummy routine called by the error message routine MESS.  This gives
c the default action when an error message is requested, which is to do
c nothing here and take the default actions in MESS.  One can change the
c actions taken by changing the values in MACT or IVAR.  Comments here
c refer to things defined in the listing for MESS.
c Variables in the calling sequence are defined as follows:
c
c TEXT  Same as TEXT passed into MESS.  The characters up to the first
c       "$" sign are the name of the subroutine that made the call to
c       MESS.
c MACT  Contents of MACT connected with the error message.  The first
c       location is the value of K52 = 10*s + p, where s is the stop
c       level specified for the error message, and p the print level.
c       The next is the value of L52, which is an error index associated
c       with an error message, and the next is the value of M52.  These
c       values or a subset of them together with TEXT should serve to
c       uniquely identify the error message.  The values of MACT(1)
c       (=K52), or  MACT(2) (=L52) can be changed and the new values
c       will be used if a return is made to MESS.  (For example MACT(1)
c       = MACT(1) - mod(MACT(1), 10), would turn off printing of a
c       message.)  Note that if MACT(1) > 90, and it is replaced with a
c       value which results in MESS returning rather than stopping,
c       unpredictable results are to be expected.
c IVAR  An array containing the parameters defining default values for
c       MESS.  The parameter values below can be used to access data in
c       this array.  For example, IVAR(MESTOP) gives the current stop
c       value, so one can tell (using s from MACT(1) above) if an error
c       is going to stop, and perhaps do some clean up before the stop
c       occurs.  Values in IVAR can be changed, but this should be done
c       with great care, since if MESS doesn't stop, the next error or
c       diagnostic message will use the new value, and diagnostic
c       messages do not call this routine.
c
      integer   MESUNI, MEHEAD, MEDDIG, MEMLIN, MEELIN, MEMUNI, MEEUNI,
     1  MESCRN, MEDIAG, MEMAXE, MESTOP, MEPRNT, METDIG, MENTXT, MEIDAT,
     2  MEFDAT, MEMDAT, MEMDA1, MEMDA2, MEMDA3, MEMDA4, MEMDA5, METABS,
     3  MEERRS, MECONT, MERET , MEEMES, METEXT, METABL, MERES3, MEIVCI,
     4  MEIVEC, MEIMAT, MEJVCI, MEJVEC, MEJMAT, MEFVCI, MEFVEC, MEFMAT,
     5  MEGVCI, MEGVEC, MEGMAT, MEMAXI, MEGBAS, MEVBAS, MEVLAS, MEFSPV
c Parameters for changing the environment.
      parameter (MESUNI=10,MEHEAD=11,MEDDIG=12,MEMLIN=13,MEELIN=14,
     1 MEMUNI=15,MEEUNI=16,MESCRN=17,MEDIAG=18,MEMAXE=19,MESTOP=20,
     2 MEPRNT=21,METDIG=22,MENTXT=23,MEIDAT=24,MEFDAT=25,MEMDAT=26,
     3 MEMDA1=27,MEMDA2=28,MEMDA3=29,MEMDA4=30,MEMDA5=31,METABS=32,
     4 MEERRS=33)
c Parameters for actions.
      parameter (MECONT=50, MERET=51,MEEMES=52,METEXT=53,MEFSPV=54,
     1 METABL=55,MERES3=56,MEIVEC=57,MEIMAT=58,MEJVEC=59,MEJMAT=60,
     2 MEFVEC=61,MEFMAT=62,MEGVEC=63,MEGMAT=64,MEIVCI=65,MEJVCI=66,
     2 MEFVCI=67,MEGVCI=68)
c Parameter derived from those above.
      parameter (MEMAXI=68,MEGBAS=49,MEVBAS=10,MEVLAS=33)
c
      character*(*) TEXT(*)
      integer   MACT(*), IVAR(MEVBAS:MEVLAS)
c
c Code for your actions goes here.
c
      return
      end
