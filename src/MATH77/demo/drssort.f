c     program DRSSORT
c>>   1994-10-19 DRSSORT  Krogh  Changes to use M77CON
c>>   1989-06-13 DRSSORT  CLL Changed "sranua (r" to "sranua (d"
c>>   1988-11-20 DRSSORT  Snyder  Initial code.
c--S replaces "?": DR?SORT, ?SORT, ?SORTP, ?RANUA
c
c     Test driver for SSORT and SSORTP.
c
c     Construct an array of 1000 random numbers using SRANUA.
c     Sort it using SSORTP.
c     Check whether it is in order.
c     Sort it using SSORT.
c     Check whether it is in order.
c
      logical OK
      integer I, P(1:1000)
      real             X(1:1000)
c
c     Generate 1000 random numbers
      call sranua (x, 1000)
c     Sort them using SSORTP.  Assume the sort will work.
      ok=.TRUE.
      call ssortp (x,1,1000,p)
c     Check the order.
      do 10 i = 2, 1000
         if (x(p(i)).lt.x(p(i-1))) ok=.FALSE.
   10 continue
c     Print the results.
      if(ok)then
         print *,'SSORTP succeeded'
      else
         print *,'SSORTP failed'
      end if
c     Sort them using SSORT.  Assume the sort will work.
      ok=.TRUE.
      call ssort (x,1,1000)
c     Check the order.
      do 20 i = 2, 1000
         if (x(i).lt.x(i-1)) ok=.FALSE.
   20 continue
c     Print the results.
      if(ok)then
         print *,'SSORT succeeded'
      else
         print *,'SSORT failed'
      end if
c
      end
