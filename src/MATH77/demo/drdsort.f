c     program DRDSORT
c>>   1994-10-19 DRDSORT  Krogh  Changes to use M77CON
c>>   1989-06-13 DRDSORT  CLL Changed "dranua (r" to "dranua (d"
c>>   1988-11-20 DRDSORT  Snyder  Initial code.
c--D replaces "?": DR?SORT, ?SORT, ?SORTP, ?RANUA
c
c     Test driver for DSORT and DSORTP.
c
c     Construct an array of 1000 random numbers using DRANUA.
c     Sort it using DSORTP.
c     Check whether it is in order.
c     Sort it using DSORT.
c     Check whether it is in order.
c
      logical OK
      integer I, P(1:1000)
      double precision X(1:1000)
c
c     Generate 1000 random numbers
      call dranua (x, 1000)
c     Sort them using DSORTP.  Assume the sort will work.
      ok=.TRUE.
      call dsortp (x,1,1000,p)
c     Check the order.
      do 10 i = 2, 1000
         if (x(p(i)).lt.x(p(i-1))) ok=.FALSE.
   10 continue
c     Print the results.
      if(ok)then
         print *,'DSORTP succeeded'
      else
         print *,'DSORTP failed'
      end if
c     Sort them using DSORT.  Assume the sort will work.
      ok=.TRUE.
      call dsort (x,1,1000)
c     Check the order.
      do 20 i = 2, 1000
         if (x(i).lt.x(i-1)) ok=.FALSE.
   20 continue
c     Print the results.
      if(ok)then
         print *,'DSORT succeeded'
      else
         print *,'DSORT failed'
      end if
c
      end
