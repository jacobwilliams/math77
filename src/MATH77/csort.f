c++ CODE for ~.C. is active
      subroutine CSORT (C, M, N, K, L, CTEMP)
c++ CODE for .C. is inactive
C      subroutine CSORT (C, M, N, K, L)
c++ END
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-04-27  CSORT  Krogh   Changes to use .C. and C%%.
C>> 1996-01-19  CSORT  Krogh   Changes to automate conversion to C.
C>> 1995-11-17  CSORT  Krogh   Changes for C conversion, SFTRAN => F77.
C>> 1994-11-14  CSORT  Krogh   Declared all vars.
c>> 1988-11-22  CSORT  Snyder  Initial code.
c
c     Sort the M:N-vector of character strings C according to the (K:L)
c     substring of each element.  CTEMP is a temporary scalar character
c     string at least as long as an element of C.
c
      integer M, N
      character*(*) C(*)
c
c     *****     Local Variables     ************************************
c
c BL      is the left bound of the sub-array to be sorted at the next
c         step.
c BR      is the right bound of the sub array to be sorted at the next
c         step.
c CL      is the current left bound of the unsorted sub-array.
c CR      is the current right bound of the unsorted sub-array.
c CTEMP   holds elements of C during exchanges.
c PARTN   is the subscript of the partition element.
c STACKL  keeps track of the left bounds of sub-arrays waiting to be
c         sorted.
c STACKR  keeps track of the right bounds of sub-arrays waiting to be
c         sorted.
c STKTOP  keeps track of the top of the stacks.
c
      integer BL,BR,CL,CR,PARTN, K, L
      integer STACKL(32),STACKR(32),STKTOP
c%%    long size = l - k + 1L;  /* Length of char strings */
c%%    long km1 = k - 1L;
C%%    char *ctemp;
C%%    ctemp = (char*)malloc(c_s+1);/*Temp space for swapping strings.*/
      character*(*) CTEMP
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
c++ CODE for ~.C. is active

            if (c(bl)(k:l).gt.c(cl)(k:l)) then
               ctemp=c(cl)
               c(cl)=c(bl)
               c(bl)=ctemp
            end if
            if (c(bl)(k:l).gt.c(br)(k:l)) then
               ctemp=c(bl)
               c(bl)=c(cl)
               c(cl)=ctemp
            end if
            if (c(cl)(k:l).gt.c(br)(k:l)) then
               ctemp=c(cl)
               c(cl)=c(bl)
               c(bl)=ctemp
            end if
            partn=cl
            ctemp=c(br-1)
            c(br-1)=c(cl)
            c(cl)=ctemp
c++ CODE for .C. is inactive
C%%        if( strncmp(C(bl - 1L,km1), C(cl - 1L,km1),size ) > 0 ){
C%%            strncpy( ctemp, C(cl - 1L,0), c_s );
C%%            strncpy( C(cl - 1L,0), C(bl - 1L,0), c_s );
C%%            strncpy( C(bl - 1L,0), ctemp, c_s );
C%%        }
C%%        if( strncmp(C(bl - 1L,km1), C(br - 1L,km1),size) > 0 ){
C%%            strncpy( ctemp, C(bl - 1L,0), c_s );
C%%            strncpy( C(bl - 1L,0), C(cl - 1L,0), c_s );
C%%            strncpy( C(cl - 1L,0), ctemp, c_s );
C%%        }
C%%        if( strncmp(C(cl - 1L,km1), C(br - 1L,km1),size) > 0 ){
C%%            strncpy( ctemp, C(cl - 1L,0), c_s );
C%%            strncpy( C(cl - 1L,0), C(bl - 1L,0), c_s );
C%%            strncpy( C(bl - 1L,0), ctemp, c_s );
C%%        }
C%%        partn = cl;
C%%        strncpy( ctemp, C(br - 2L,0), c_s );
C%%        strncpy( C(br - 2L,0), C(cl - 1L,0), c_s );
C%%        strncpy( C(cl - 1L,0), ctemp, c_s );
c++ END
c           Partition the sub-array around PARTN.  Exclude the above
c           considered elements from partitioning because they're al-
c           ready in the correct subfiles.  Stop scanning on equality to
c           prevent files containing equal values from causing a loop.
            cl=bl
            cr=br-1
   20       continue
   30          cl=cl+1
c++ CODE for ~.C. is active
                  if (c(cl)(k:l) .lt. c(partn)(k:l)) go to 30
   40          cr=cr-1
                  if (c(cr)(k:l) .gt. c(partn)(k:l)) go to 40
               if (cl.gt.cr) go to 50
               ctemp=c(cl)
               c(cl)=c(cr)
               c(cr)=ctemp
c++ CODE for .C. is inactive
C%%      if( strncmp(C(cl-1,km1), C(partn-1,km1), size) < 0 ) goto L_30;
C   40          cr=cr-1
C%%      if( strncmp(C(cr-1,km1), C(partn-1,km1), size) > 0 ) goto L_40;
C               if (cl.gt.cr) go to 50
C%%        strncpy( ctemp, C(cl - 1L,0), c_s );
C%%        strncpy( C(cl - 1L,0), C(cr - 1L,0), c_s );
C%%        strncpy( C(cr - 1L,0), ctemp, c_s );
c++ END
               if (partn.eq.cl) partn=cr
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
         cl=cr
c++ CODE for ~.C. is active
         ctemp=c(cr)
   60    if (c(cl-1)(k:l).gt.ctemp(k:l)) then
            c(cl)=c(cl-1)
            cl=cl-1
            if (cl .gt. m) go to 60
         end if
         c(cl)=ctemp
c++ CODE for .C. is inactive
C%%        strncpy( ctemp, C(cr-1,0), c_s );
C  60     continue
C%%        if( strncmp(C(cl-2,k-1), ctemp+(short)(k-1),size) > 0 ){
C%%            strncpy( C(cl-1,0), C(cl-2,0), c_s );
C            cl=cl-1
C            if( cl .gt. m ) go to 60
C%%        }
C%%        strncpy( C(cl-1,0), ctemp, c_s );
c++ END
  70  continue
      return
      end
