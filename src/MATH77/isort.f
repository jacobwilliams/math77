      subroutine ISORT (A, M, N)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1995-11-17 ISORT  Krogh  Converted SFTRAN to Fortran 77.
C>> 1994-10-19 ISORT  Krogh  Changes to use M77CON
C>> 1988-11-22  ISORT  Snyder  Initial code.
c--I replaces "?": ?SORT, ?SORTP
c
c     Sort the M:N-vector A into ascending order.
c
c     To sort an array A' into descending order, let A = -A'
c     To sort an array A' into ascending order according to the
c     absolute value of the elements let A = ABS(A').
c     To sort an array A' into decending order according to the
c     absolute value of the elements let A = -ABS(A').
c
c     To keep track of the original elements, use ISORTP.
c
      integer M, N
c--I Next line special: I
      integer          A(*)
c
c     *****     Local Variables     ************************************
c
c BL      is the left bound of the sub-array to be sorted at the next
c         step.
c BR      is the right bound of the sub array to be sorted at the next
c         step.
c CL      is the current left bound of the unsorted sub-array.
c CR      is the current right bound of the unsorted sub-array.
c PARTN   is the partition element.
c STACKL  keeps track of the left bounds of sub-arrays waiting to be
c         sorted.
c STACKR  keeps track of the right bounds of sub-arrays waiting to be
c         sorted.
c STKTOP  keeps track of the top of the stacks.
c TEMP    holds elements of A during exchanges.
c
      integer BL,BR,CL,CR
c--I Next line special: I
      integer          PARTN,TEMP
      integer STACKL(32),STACKR(32),STKTOP
c
c     *****     Executable Statements     ******************************
c
      if (n-m.ge.10) then
         stktop=1
         stackl(1)=m
         stackr(1)=n
   10    continue
            bl=stackl(stktop)
            br=stackr(stktop)
            stktop=stktop-1
c           Choose a partitioning element.  Use the median of the first,
c           middle and last elements.  Sort them so the extreme elements
c           serve as sentinels during partitioning.
            cl=(bl+br)/2
            partn=a(cl)
            if (a(bl).gt.partn) then
               a(cl)=a(bl)
               a(bl)=partn
               partn=a(cl)
            end if
            if (a(bl).gt.a(br)) then
               temp=a(br)
               a(br)=a(bl)
               a(bl)=temp
            end if
            if (partn.gt.a(br)) then
               a(cl)=a(br)
               a(br)=partn
               partn=a(cl)
            end if
            a(cl)=a(br-1)
            a(br-1)=partn
c           Partition the sub-array around PARTN.  Exclude the above
c           considered elements from partitioning because they're al-
c           ready in the correct subfiles.  Stop scanning on equality to
c           prevent files containing equal values from causing a loop.
            cl=bl
            cr=br-1
   20       continue
   30          cl=cl+1
                  if (a(cl) .lt. partn) go to 30
   40          cr=cr-1
                  if (a(cr) .gt. partn) go to 40
               if (cl.gt.cr) go to 50
               temp=a(cl)
               a(cl)=a(cr)
               a(cr)=temp
            go to 20
   50       continue
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
         if (stktop .ne.0) go to 10
      end if
c     Clean up small subfiles using insertion sort on everything.
      do 70 cr = m+1, n
         partn=a(cr)
         cl=cr
   60    if (a(cl-1) .gt. partn) then
            a(cl)=a(cl-1)
            cl=cl-1
            if (cl .gt. m) go to 60
         end if
         a(cl)=partn
   70 continue
      return
      end
