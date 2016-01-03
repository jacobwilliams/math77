c>>   1988-11-22  DRISORT  Snyder  Initial code.
c
c     Test driver for ISORT and ISORTP.
c
c     Construct an array of 1000 random numbers using SRANUA.
c     Scale it onto 1..1,000,000 and convert to integers.
c     Sort it using ISORTP.
c     Check whether it is in order.
c     Sort it using ISORT.
c     Check whether it is in order.
c
      logical OK
      integer P(1:1000)
      real R(1:1000)
      integer I, V(1:1000)
c
c     Generate 1000 random numbers
      call sranua (r, 1000)
c     Scale them onto 1..1,000,000 and convert to integers.
      do 10 i = 1, 1000
         v(i) = 1000000*r(i)
10    continue
c     Sort them using ISORTP.  Assume the sort will work.
      ok=.TRUE.
      call isortp (v,1,1000,p)
c     Check the order.
      do 20 i = 2, 1000
         if (v(p(i)).lt.v(p(i-1))) ok=.FALSE.
20    continue
c     Print the results.
      if(ok)then
         print *,'ISORTP succeeded'
      else
         print *,'ISORTP failed'
      end if
c     Sort them using ISORT.  Assume the sort will work.
      ok=.TRUE.
      call isort (v,1,1000)
c     Check the order.
      do 30 i = 2, 1000
         if (v(i).lt.v(i-1)) ok=.FALSE.
30    continue
c     Print the results.
      if(ok)then
         print *,'ISORT succeeded'
      else
         print *,'ISORT failed'
      end if
c
      end
