c>>   1996-05-31  DRCSORT  Krogh   Added comments for conversion to C.
c>>   1988-11-22  DRCSORT  Snyder  Initial code.
c
c     Test driver for CSORT.
c
c     Construct an array of 1000 random strings of length 10.
c     Sort it on the 2:9 substring using CSORTP.
c     Check whether it is in order.
c     Sort it on the 2:9 substring using CSORT.
c     Check whether it is in order.
c
      character*10 C(1:1000)
c++ CODE for ~.C. is active
      character CTEMP*10
c++ END
      character*26 LETTRS
      integer I, J, K
      logical OK
      integer P(1:1000)
      real SRANU
      external SRANU
      data LETTRS /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
c
c     Generate 1000 random strings of length 10
      do 20 i = 1, 1000
         do 10 j = 1, 10
            k = 25*sranu() + 1
            c(i)(j:j) = lettrs(k:k)
10       continue
20    continue
c     Sort them using CSORTP.
      ok=.TRUE.
      call csortp (c,1,1000,2,9,p)
c     Check the order.
      do 30 i = 2, 1000
         ok = ok .AND. lge(c(p(i))(2:9),c(p(i-1))(2:9))
30    continue
c     Print the results.
      if(ok)then
         print *,'CSORTP succeeded'
      else
         print *,'CSORTP failed'
      end if
c     Sort them using CSORT.
      ok=.TRUE.
c++ CODE for ~.C. is active
      call csort (c,1,1000,2,9,ctemp)
c++ CODE for .C. is inactive
c      call csort (c,1,1000,2,9)
c++ END
c     Check the order.
      do 40 i = 2, 1000
         ok = ok .AND. lge(c(i)(2:9),c(i-1)(2:9))
40    continue
c     Print the results.
      if(ok)then
         print *,'CSORT succeeded'
      else
         print *,'CSORT failed'
      end if
c
      end
