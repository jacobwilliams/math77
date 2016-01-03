      subroutine CSORTP (C, M, N, K, L, P)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2008-10-29  CSORTP  Krogh   Moved the string.h line up.
c>> 2005-12-06  CSORTP  Krogh   Added include of string.h for C version.
c>> 1996-04-27  CSORTP  Krogh   Changes to use .C. and C%%.
C>> 1996-01-19  CSORTP  Krogh   Changes to automate conversion to C.
C>> 1995-11-09  CSORTP  Krogh   Got rid of Mult. entries and SFTRAN.
C>> 1994-11-14  CSORTP  Krogh   Declared all vars.
c>> 1992-11-23  CSORTP  Snyder  Add entry CSORTQ.
C>> 1991-04-02  CSORTP  Snyder  Repair no permutation vector if m-n < 10
c>> 1988-11-22  CSORTP  Snyder  Initial code.
c
c     Sort the M:N-vector of character strings C according to the (K:L)
c     substring of each element.  C is not disturbed.  P is set
c     so that C(P(J)) is the J'th element of the sorted sequence.
c     Enter at CSORTQ to use pre-specified permutation vector.
c
      integer K, L, M, N, P(*)
      character*(*) C(*)
      integer CL
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
c PTEMP   holds elements of P during exchanges.
c STACKL  keeps track of the left bounds of sub-arrays waiting to be
c         sorted.
c STACKR  keeps track of the right bounds of sub-arrays waiting to be
c         sorted.
c STKTOP  keeps track of the top of the stacks.
c
c     *****     Executable Statements     ******************************
c
      do 10 CL = M, N
         P(CL) = CL
   10 continue
      call CSORTQ (C, M, N, K, L, P)
      return
      end
c
c++ CODE for .C. is inactive
c%%#include <string.h>
c++ END

      subroutine CSORTQ (C, M, N, K, L, P)
      integer K, L, M, N
      character*(*) C(*)
      integer P(*)
      integer BL, BR, CL, CR, PARTN, PTEMP
      integer STACKL(32), STACKR(32), STKTOP

c++ CODE for .C. is inactive
C%%long size = l - k + 1L; /* Length of char string to be compared. */
C%%long km1 = k - 1L;
c++ END
      if (N-M .ge. 10) then
         STKTOP = 1
         STACKL(1) = M
         STACKR(1) = N
   20    continue
            BL = STACKL(STKTOP)
            BR = STACKR(STKTOP)
            STKTOP = STKTOP - 1
            PARTN = P(BL)
c           Choose a partitioning element.  Use the median of the first,
c           middle and last elements.  Sort them so the extreme elements
c           can serve as sentinels during partitioning.
            CL = (BL+BR) / 2
            PTEMP = P(CL)
C%%        if( strncmp(C(p[bl-1]-1,km1), C(ptemp-1,km1),size ) > 0 ){
            if (C(P(BL))(K:L) .gt. C(PTEMP)(K:L)) then
               P(CL) = P(BL)
               P(BL) = PTEMP
               PTEMP = P(CL)
c++ CODE for ~.C. is active
            end if
            if (C(P(BL))(K:L) .gt. C(P(BR))(K:l)) then
c++ CODE for .C. is inactive
C%%        }
C%%        if( strncmp(C(p[bl-1]-1,km1), C(p[br-1]-1,km1), size) > 0 ){
c++ END
               CR = P(BL)
               P(BL) = P(BR)
               P(BR) = CR
c++ CODE for ~.C. is active
            end if
            if (C(PTEMP)(K:L) .gt. C(P(BR))(K:L)) then
c++ CODE for .C. is inactive
C%%        }
C%%        if( strncmp(C(ptemp-1,km1), C(p[br-1]-1,km1),size) > 0 ){
c++ END
               P(CL) = P(BR)
               P(BR) = PTEMP
               PTEMP = P(CL)
C%%        }
            end if
            P(CL) = P(BR - 1)
            P(BR - 1) = PTEMP
            PARTN = P(CL)
c           Partition the sub-array around PARTN.  ExCLude the above
c           considered elements from partitioning because they're al-
c           ready in the correct subfiles.  Stop scanning on equality to
c           prevent files containing equal values from causing a loop.
            CL = BL
            CR = BR-1
   30       continue
   40          CL = CL+1
c++ CODE for ~.C. is active
               if (C(P(CL))(K:L) .lt. C(PARTN)(K:L)) go to 40
   50          CR = CR-1
               if (C(P(CR))(K:L) .gt. C(PARTN)(K:L)) go to 50
c++ CODE for .C. is inactive
C%%  if( strncmp(C(p[cl-1]-1,km1), C(partn-1,km1),size) < 0 ) goto L_40;
C   50          CR = CR-1
C%%  if( strncmp(C(p[cr-1]-1,km1), C(partn-1,km1),size) > 0 ) goto L_50;
c++ END
               if (CL .gt. CR) go to 60
               PTEMP = P(CL)
               P(CL) = P(CR)
               P(CR) = PTEMP
            go to 30
   60       continue
c           Put sub-arrays on the stack if they're big enough.  Put the
c           larger under the smaller, so the smaller will be done next.
c           This makes the upper bound of the stack depth log2 (n-m+1).
c           (The "Hibbard" modification of quicksort).
            if (CL-BL  .gt.  BR - CR) then
               if (CL - BL .gt. 10) then
                  STKTOP = STKTOP+1
                  STACKL(STKTOP) = BL
                  STACKR(STKTOP) = CR
               end if
               if (BR - CR .gt. 10) then
                  STKTOP = STKTOP+1
                  STACKL(STKTOP) = CL
                  STACKR(STKTOP) = BR
               end if
            else
               if (BR - CR .gt. 10) then
                  STKTOP = STKTOP+1
                  STACKL(STKTOP) = CL
                  STACKR(STKTOP) = BR
               end if
               if (CL - BL .gt. 10) then
                  STKTOP = STKTOP+1
                  STACKL(STKTOP) = BL
                  STACKR(STKTOP) = CR
               end if
            end if
         if (STKTOP .ne. 0) go to 20
      end if
c     Clean up small subfiles using insertion sort on everything.
      do 80 CR = M + 1, N
         PTEMP = P(CR)
         CL = CR
   70    continue
C%%      if( strncmp(C(p[cl-2]-1,km1), C(ptemp-1,km1),size) > 0 ){
         if (C(P(CL-1))(K:L) .gt. C(PTEMP)(K:L)) then
            P(CL) = P(CL-1)
            CL = CL - 1
            if (CL .gt. M) go to 70
C%%      }
         end if
         P(CL) = PTEMP
   80 continue
      return
      end
