      subroutine PVEC (LIST,HEAD)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 PVEC  Krogh   Declared all vars.
c>> 1990-02-08 PVEC WV Snyder at JPL 91109, adapted for MATH77
c
c     PVEC converts a chain of items in LIST, starting at HEAD, to a
c     permutation vector.
c
      integer LIST(*),HEAD
      integer I, J, N, K
      n=0
      i=head
c
c     Convert the chain in LIST to an inverse permutation vector.
c
    1 if (i.ne.0) then
         n=n+1
         j=list(i)
         list(i)=n
         i=j
         go to 1
      end if
c
c     Invert the inverse permutation vector in LIST.
c
    2 if (n.eq.0) return
      j=n
      k=list(j)
      if (k.ne.n) then
    3    i=list(j)
         if (i.gt.0) then
            list(j)=-k
            k=j
            j=i
            if (j.ne.n) go to 3
            list(n)=k
         else
            list(j)=-i
         end if
      end if
      n=n-1
      go to 2
      end
