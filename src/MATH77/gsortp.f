      subroutine GSORTP (COMPAR, N, P)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1998-01-20  GSORTP  Snyder  Allow not initializing P.
c>> 1996-05-01  GSORTP  Krogh   Changes to use .C. and C%%.
C>> 1995-11-17  GSORTP  Krogh   Converted SFTRAN to Fortran 77.
C>> 1991-04-02  GSORTP  Snyder  Repair no permutation vector if m-n < 10
C>> 1988-11-22  GSORTP  Snyder  Initial code.
c
c     Sort an N-vector of objects of unknown type and organization.
c     P is set so that the P(J)'th element of the original sequence is
c     the J'th element of the sorted sequence.  The order is defined by
c     the integer function COMPAR.  An invocation COMPAR(I,J) should
c     return -1 if the I'th element of the data is to preceed the J'th
c     element in the sorted sequence, +1 if the J'th element is to
c     preceed the I'th element in the sorted sequence, and 0 if the I'th
c     and J'th elements are the same.
c
c     This subprogram is unaware of the data, and cannot manipulate it.
c     It is the caller's responsibility to make the data known to the
c     COMPAR function.
c
      integer COMPAR, N, P(*)
      external COMPAR
c
c     *****     Local Variables     ************************************
c
c BL      Left bound of the sub-array to be sorted at the next step.
c BR      Right bound of the sub array to be sorted at the next step.
c CL      Current left bound of the unsorted sub-array.
c CR      Current right bound of the unsorted sub-array.
c MYN     My N.
c PARTN   is the partition element.
c PTEMP   holds elements of P during exchanges.
c STACKL  keeps track of the left bounds of sub-arrays waiting to be
c         sorted.
c STACKR  keeps track of the right bounds of sub-arrays waiting to be
c         sorted.
c STKTOP  keeps track of the top of the stacks.
c
      integer BL,BR,CL,CR,MYN,PARTN,PTEMP
      integer STACKL(32),STACKR(32),STKTOP
c
c     *****     Executable Statements     ******************************
c
      do 20 cl = 1, n
         p(cl)=cl
   20 continue
      myn = abs(n)
      if (myn.ge.10) then
         stktop=1
         stackl(1)=1
         stackr(1)=myn
c           Start until loop
   40    continue
            bl=stackl(stktop)
            br=stackr(stktop)
            stktop=stktop-1
c           Choose a partitioning element.  Use the median of the first,
c           middle and last elements.  Sort them so the extreme elements
c           can serve as sentinels during partitioning.
            cl=(bl+br)/2
            partn=p(cl)
C%%         if ((*compar)( P[bl], partn ) > 0 ){
            if (compar(p(bl),partn).gt.0) then
               p(cl)=p(bl)
               p(bl)=partn
               partn=p(cl)
c%%         }
            end if
C%%         if ((*compar)( P[bl], P[br] ) > 0 ){
            if (compar(p(bl),p(br)).gt.0) then
               ptemp=p(bl)
               p(bl)=p(br)
               p(br)=ptemp
c%%         }
            end if
C%%         if ((*compar)( partn, P[br] ) > 0 ){
            if (compar(partn,p(br)).gt.0) then
               p(cl)=p(br)
               p(br)=partn
               partn=p(cl)
c%%         }
            end if
            p(cl)=p(br-1)
            p(br-1)=partn
c           Partition the sub-array around PARTN.  Exclude the above
c           considered elements from partitioning because they're al-
c           ready in the correct subfiles.  Stop scanning on equality to
c           prevent files containing equal values from causing a loop.
            cl=bl
            cr=br-1
c              Start forever block
   60       continue
   80          cl=cl+1
C%%               if ((*compar)( P[cl], partn ) < 0) goto L_80;
                  if (compar(p(cl),partn) .lt. 0) go to 80
  100          cr=cr-1
C%%               if ((*compar)( P[cr], partn ) > 0) goto L_100;
                  if (compar(p(cr),partn) .gt. 0) go to 100
               if (cl.gt.cr) go to 120
               ptemp=p(cl)
               p(cl)=p(cr)
               p(cr)=ptemp
               go to 60
c              End forever block
  120       continue
c           Put sub-arrays on the stack if they're big enough.  Put the
c           larger under the smaller, so the smaller will be done next.
c           This makes the upper bound of the stack depth log2 (myn).
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
c           End until loop
         if (stktop .ne. 0) go to 40
      end if
c     Clean up small subfiles by using insertion sort on everything.
      do 160 cr = 2, myn
         ptemp=p(cr)
         cl=cr
  140    continue
c%%      if ((*compar)( P[cl - 1], ptemp ) > 0) {
         if (compar(p(cl-1),ptemp).gt.0) then
            p(cl)=p(cl-1)
            cl=cl-1
            if (cl .gt. 1) go to 140
c%%      }
         end if
         p(cl)=ptemp
  160 continue
      return
      end
