      subroutine DSDIF ( K, N, T, BCOEF, NDERIV, BDIF )
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 DSDIF Krogh  Changes to use M77CON
c>> 1992-11-02 DSDIF C. L. Lawson, JPL
c>> 1988-03-16 C. L. Lawson, JPL
c
c     Constructs a divided difference table in BDIF(,), preparatory to
c     derivative calculation.
c     The subroutine, BSPLPP, given on pp. 140-141 of
c     A PRACTICAL GUIDE TO SPLINES by Carl De Boor, Springer-Verlag,
c     1978, has been recoded as separate subroutines: DSTOT (or DSTOP)
c     calling DSDIF and DSVALA.  This subroutine has the functionality
c     of lines 57-71 of BSPLPP in the book.
c     ------------------------------------------------------------------
c--D replaces "?": ?SDIF
c     ------------------------------------------------------------------
      integer I, ID, K, KMID, N, NDERIV
      double precision BCOEF(N), BDIF(N,NDERIV+1)
      double precision DIFF, FKMID, T(N+K), ZERO
      parameter(ZERO = 0.0D0)
c     ------------------------------------------------------------------
      do 10 I=1,N
         BDIF(I,1) = BCOEF(I)
   10 continue
      KMID = K
      do 20 ID=2,NDERIV+1
         KMID = KMID - 1
         FKMID = dble(KMID)
         do 20 I=ID,N
            DIFF = T(I + KMID) - T(I)
            if (DIFF .ne. ZERO) then
               BDIF(I,ID) = FKMID*(BDIF(I,ID-1) - BDIF(I-1,ID-1))/DIFF
            else
               BDIF(I,ID) = ZERO
            endif
   20       continue
      return
      end
