c>>   1995-05-28  DRGSORTP  Krogh   Converted SFTRAN to Fortran
c>>   1988-11-22  DRGSORTP  Snyder  Initial code.
c
c     Test driver for GSORTP.
c
c     Construct an array of 1000 random numbers using SRANUA.
c     Sort it using GSORTP.
c     Check whether it is in order.
c
      logical OK
      integer I, COMPAR, P(1:1000)
      external COMPAR
      real R(1:1000)
      common /RCOM/ R
c
c     Generate 1000 random numbers
      call sranua (r, 1000)
c     Sort them using GSORTP.
      ok=.TRUE.
      call gsortp (compar,1000,p)
c     Check the order.
      do 10 i = 2, 1000
         if (r(p(i)).lt.r(p(i-1))) ok=.FALSE.
10    continue
c     Print the results.
      if(ok)then
         print *,'GSORTP succeeded'
      else
         print *,'GSORTP failed'
      end if
c
      end
      integer function COMPAR(I,J)
c
c     Determine the relative order of R(I) and R(J), where R is in
c     the common block /RCOM/.  Return -1 if R(I) should preceed R(J)
c     in the sorted order, +1 if R(I) should follow R(J), and 0
c     otherwise.
c
      integer I,J
      real R(1:1000)
      common /RCOM/ R
      if (R(I)-R(J)) 10,20,30
10    compar=-1
      return
20    compar=0
      return
30    compar=+1
      return
c
      end
