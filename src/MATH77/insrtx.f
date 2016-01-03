C++ CODE for INSORT_VER = INSRTX is active
      subroutine INSRTX (COMPAR,N,LIST,LIST1)
C++ CODE for INSORT_VER = INSORT is inactive
C      subroutine INSORT (COMPAR,N,LIST,LIST1)
C      integer COMPAR
c++ End
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-04-30 INSORT Krogh Changes to use .C. and C%%.
c>> 1996-03-30 INSORT Krogh IABS => ABS
C>> 1995-11-16 INSORT Krogh Setup for separate INSORT/INSRTX.
C>> 1994-11-11 INSORT Krogh Declared all vars.
c>> 1990-02-08 INSORT WV Snyder at JPL 91109, adapted for MATH77
c     5 February 1990.
c
c     General sort routine.
c
c     If INSORT is invoked, COMPAR is a 2-argument INTEGER function
c     having a negative value if the record referenced by the first
c     argument is to be sorted before the record referenced by the
c     second argument, a zero value if the order is immaterial, and a
c     positive value otherwise.
c
c     If INSRTX is invoked, COMPAR is a 4-argument subroutine, invoked
c     by CALL COMPAR (8,I,J,IORDER).  IORDER should be negative if the
c     record referenced by I should be sorted before the record refer-
c     enced by J, a zero value if the order is immaterial, and a posi-
c     tive value otherwise.  INSRTX is used by the external sorting
c     subroutine EXSORT.
c
c     Since INSORT deals with record indices only, the sort keys may be
c     arbitrarily complex, and the records to be sorted need not be
c     in memory.
c
c     LIST is at least N words long, where N is the number of records
c     to be sorted.  LIST defines the ordering of the sorted records
c     when INSORT is finished.  Let K=LIST(I).  Then if K=0, record I
c     is the last record of the sorted data set.  Otherwise, record K
c     is the immediate successor to record I.
c
c     LIST1 is the first sorted record.
c
c     Usage:
c     external COMPAR
c     call INSORT (COMPAR,N,LIST,LIST1)
c     do while (LIST1.NE.0)
c        do any processing for record LIST1 here.
c        LIST1=LIST(LIST1)
c     end while
c
      integer  LIST(*), LIST1, N
      external COMPAR
c
c     *****     Local Variables     ************************************
c
c DIRECT  is the length of a string, and the sign indicates the
c         direction of initially detected order: 0 = none, positive =
c         increasing, negative = decreasing.
c HEADS(2*I-1) and HEADS(2*I) hold heads of strings of length between
c         2**(I-1) and 2**I - 1.
c LENGTH  contains the lengths of the strings indexed by the corres-
c         ponding element of HEADS.
c MXSORT  is log base 2 of the maximum sort capacity.
c MYN     is the local value of N.
c NHEADS  is 2 * log base 2 of the maximum sort capacity.
      integer MXSORT, NHEADS
      integer JHEAD, NHEAD, IP1, I, NCOMP, M, J1, J2, I1, I2, NCHAIN
      parameter (MXSORT = 32, NHEADS = 2*MXSORT)
      integer DIRECT,HEADS(NHEADS),LENGTH(NHEADS),MYN,PHASE
c
c     *****     Executable Statements     *****************************
c
c     Scan records and detect ascending or descending chains of
c     existing ordering.
c
      myn=n
      jhead=myn
      phase=0
      nhead=0
      ip1=1
      do 30 i=1,32
30       heads(i)=0
c     Indicate beginning of chain - no order assigned yet.
40    direct=0
c     Assume a forward chain will be found.
      jhead=ip1
50    i=ip1
      ip1=i+1
      if (ip1.gt.myn) if (direct) 150,160,160
c
c     Check sequence of two adjacent records.
c
C++ CODE for ~.C. & (INSORT_VER = INSORT) is inactive
C      ncomp=compar(i,ip1)
C++ CODE for .C. & (INSORT_VER = INSORT) is inactive
C%%   ncomp = (*compar)( i, ip1 );
C++ CODE for ~.C. & (INSORT_VER = INSRTX) is active
      call compar(8,i,ip1,ncomp)
C++ CODE for .C. & (INSORT_VER = INSRTX) is inactive
c%%   (*compar)( 8, i, ip1, &ncomp );
c++ End
      if (ncomp) 100,90,120
90    if (direct) 140,110,110
c
c     Records I,I+1 are in ascending sequence.
c
100   if (direct.lt.0) go to 150
c
c     Forward chain.
c
110   direct=direct+1
      list(I)=ip1
      go to 50
c
c     Records I,I+1 are in descending sequence.
c
120   if (direct) 140,130,160
c
c     Start a backward chain.
c
c     Indicate end of chain.
130   list(i)=0
c
c     Continue a backward chain.
c
140   direct=direct-1
      list(ip1)=i
      go to 50
c
c     Had a chain, got a sequence change.
c
150   jhead=i
      go to 170
c     Indicate end of chain
160   list(i)=0
C     Compute log base 2 of ABS(direct).
170   direct=abs(direct)+1
180   m=1
      phase=-phase
      do 190 i=1,mxsort
         if (m.ge.direct) go to 200
190      m=m+m
      i=mxsort
200   m=i+i
220   if (heads(m-1).ne.0) then
         if (heads(m).ne.0) then
            j1=heads(m-1)
            j2=heads(m)
            i1=length(m-1)
            i2=length(m)
c           Merge two shortest strings of three in the bucket.
            if (direct.lt.i2) then
               j2=jhead
               jhead=heads(m)
               i2=direct
               direct=length(m)
            end if
            heads(m-1)=jhead
            length(m-1)=direct
            direct=i1+i2
            nhead=nhead-1
            go to 310
         end if
         if (direct.gt.length(m-1)) then
            m=m+1
         else
            heads(m)=heads(m-1)
            length(m)=length(m-1)
         end if
      end if
      heads(m-1)=jhead
      length(m-1)=direct
      nhead=nhead+1
      if (ip1.le.myn) go to 40
      if (nhead.eq.1) then
         list1=jhead
         return
      end if
      do 280 m=1,nheads
        if (heads(m).ne.0) go to 290
280   continue
290   j1=heads(m)
      direct=length(m)
      heads(m)=0
300   m=m+1
      if (heads(m).eq.0) go to 300
      direct=direct+length(m)
      nhead=nhead-2
      phase=-1
c
c     Merge chains together.
c
      j2=heads(m)
310   heads(m)=0
      nchain=0
320   continue
C++ CODE for ~.C. & (INSORT_VER = INSORT) is inactive
C      ncomp=compar(j1,j2)
C++ CODE for .C. & (INSORT_VER = INSORT) is inactive
C%%    ncomp = (*compar)( j1, j2 );
C++ CODE for ~.C. & (INSORT_VER = INSRTX) is active
      call compar(8,j1,j2,ncomp)
C++ CODE for .C. & (INSORT_VER = INSRTX) is inactive
c%%    (*compar)( 8, j1, j2, &ncomp );
c++ End
      if (ncomp.gt.0) then
c
c        J2 should come before J1.
c
         if (nchain) 380,360,370
360      jhead=j2
         go to 380
370      list(i1)=j2
380      nchain=-1
         i2=j2
         j2=list(i2)
         if (j2.ne.0) go to 320
         list(I2)=j1
      else
c
c        J1 should come before J2.
c
         if (nchain) 400,410,420
400      list(i2)=j1
         go to 420
410      jhead=j1
420      nchain=1
         i1=j1
         j1=list(i1)
         if (j1.ne.0) go to 320
         list(i1)=j2
      end if
      if (phase.lt.0) go to 180
      m=m+2
      go to 220
c
      end
