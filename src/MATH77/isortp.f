      subroutine ISORTP (A, M, N, P)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1995-11-15 ISORTP  Krogh  SFTRAN => Fortran, removed mult. entry.
c>> 1994-10-19 ISORTP  Krogh  Changes to use M77CON
c>> 1992-11-23 ISORTP  Snyder  Add entry ISORTQ.
C>> 1991-04-02 ISORTP  Snyder  Repair no permutation vector if m-n < 10
C>> 1988-11-22 ISORTP  Snyder  Initial code.
c--I replaces "?": ?SORTP, ?SORTQ
c
c     Sort the M:N-vector A.
c     A is not disturbed.  P is set so that A(P(J)) is the J'th element
c     of the sorted sequence.
c     Enter at ISORTQ to use pre-specified permutation vector.
c
c     To sort an array A' into descending order, let A = -A'
c     To sort an array A' into ascending order according to the
c     absolute value of the elements let A = ABS(A').
c     To sort an array A' into decending order according to the
c     absolute value of the elements let A = -ABS(A').
c
      integer M, N, P(*)
c--I Next line special: I
      integer          A(*)
      integer CL
c                      Get permutation vector for sorting
      do 20 cl = m, n
         p(cl)=cl
   20 continue
      call ISORTQ (A, M, N, P)
      return
      end

      subroutine ISORTQ (A, M, N, P)
      integer M, N
c--I Next line special: I
      integer          A(*)
      integer P(*)
c
c     *****     Local Variables     ************************************
c
c BL      is the left bound of the sub-array to be sorted at the next
c         step.
c BR      is the right bound of the sub-array to be sorted at the next
c         step.
c CL      is the current left bound of the unsorted sub-array.
c CR      is the current right bound of the unsorted sub-array.
c PARTN   is the partition element.
c PTEMP   holds elements of P during exchanges.
c STACKL  keeps track of the left bounds of sub-arrays waiting to be
c         sorted.
c STACKR  keeps track of the right bounds of sub-arrays waiting to be
c         sorted.
c STKTOP  keeps track of the top of the stacks.
c
      integer BL,BR,CL,CR
c--I Next line special: I
      integer          PARTN
      integer PTEMP,STACKL(32),STACKR(32),STKTOP
c
c     *****     Executable Statements     ******************************
c
      if (n-m.ge.10) then
         stktop=1
         stackl(1)=m
         stackr(1)=n
   40    continue
            bl=stackl(stktop)
            br=stackr(stktop)
            stktop=stktop-1
c           Choose a partitioning element.  Use the median of the first,
c           middle and last elements.  Sort them so the extreme elements
c           can serve as sentinels during partitioning.
            cl=(bl+br)/2
            ptemp=p(cl)
            if (a(p(bl)).gt.a(ptemp)) then
               p(cl)=p(bl)
               p(bl)=ptemp
               ptemp=p(cl)
            end if
            if (a(p(bl)).gt.a(p(br))) then
               cr=p(bl)
               p(bl)=p(br)
               p(br)=cr
            end if
            if (a(ptemp).gt.a(p(br))) then
               p(cl)=p(br)
               p(br)=ptemp
               ptemp=p(cl)
            end if
            p(cl)=p(br-1)
            p(br-1)=ptemp
            partn=a(ptemp)
c           Partition the sub-array around PARTN.  Exclude the above
c           considered elements from partitioning because they're al-
c           ready in the correct subfiles.  Stop scanning on equality to
c           prevent files containing equal values from causing a loop.
            cl=bl
            cr=br-1
   80       continue
  100          cl=cl+1
               if (a(p(cl)) .lt. partn) go to 100
  120          cr=cr-1
               if (a(p(cr)) .gt. partn) go to 120
               if (cl.gt.cr) go to 150
               ptemp=p(cl)
               p(cl)=p(cr)
               p(cr)=ptemp
               go to 80
  150       continue
c           Put sub-arrays on the stack if they're big enough.  Put the
c           larger under the smaller, so the smaller will be done next.
c           This makes the upper bound of the stack depth log2 (n-m+1).
c           (The "Hibbard" modification of quicksort).
            if (cl-bl .gt. br-cr) then
               if (cl-bl.gt.10) then
                  stktop=stktop+1
                  stackl(stktop)=bl
                  stackr(stktop)=cr
               end if
               if (br-cr.gt.10) then
                  stktop=stktop+1
                  stackl(stktop)=cl
                  stackr(stktop)=br
               end if
            else
               if (br-cr.gt.10) then
                  stktop=stktop+1
                  stackl(stktop)=cl
                  stackr(stktop)=br
               end if
               if (cl-bl.gt.10) then
                  stktop=stktop+1
                  stackl(stktop)=bl
                  stackr(stktop)=cr
               end if
            end if
         if (stktop .ne.0) go to 40
      end if
c     Clean up small subfiles using insertion sort on everything.
      do 200 cr = m+1, n
         ptemp=p(cr)
         partn=a(ptemp)
         cl=cr
  180    if (a(p(cl-1)) .gt. partn) then
            p(cl)=p(cl-1)
            cl=cl-1
            if (cl .gt. m) go to 180
         end if
         p(cl)=ptemp
  200 continue
      return
      end
