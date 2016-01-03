c>>   1990-02-09  DRINSORT  Snyder  Initial code.
c
c     Test driver for INSORT.
c
c     Construct an array of 1000 random numbers using SRANUA.
c     Sort it using INSORT.
c     Check whether it is in order.
c
      logical OK
      integer COMPAR,L(1:1000),L1,LS,NCOMP
      external COMPAR
      real PREV,R(1:1000)
      common /RCOM/ NCOMP,R
c
c     Generate 1000 random numbers
      call sranua (r, 1000)
c     Sort them using INSORT.
      ncomp=0
      call insort (compar,1000,l,l1)
      ls = l1
c     Check the order.
      ok=.TRUE.
      prev=-1.0
10    if (l1.ne.0) then
         if (r(l1).lt.prev) ok=.FALSE.
         prev=r(l1)
         l1=l(l1)
         go to 10
      end if
c     Convert l to a permutation vector.
      call pvec (l,ls)
c     Check the order again.
      do 20 ls = 2, 1000
         if (r(l(ls)) .lt. r(l(ls-1))) ok=.FALSE.
20    continue
c     Print the results.
      if (ok) then
         print '('' INSORT succeeded using'',i6,'' compares'')', ncomp
      else
         print *,'INSORT failed'
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
      integer I,J, NCOMP
      real R(1:1000)
      common /RCOM/ NCOMP,R
      ncomp=ncomp+1
      if (R(I)-R(J)) 10,20,30
10    compar=-1
      return
20    compar=0
      return
30    compar=+1
      return
c
      end
