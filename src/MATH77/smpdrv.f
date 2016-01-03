      SUBROUTINE SMPDRV (C,NC,D,ND)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-10-20 SMPDRV Krogh  Changes to use M77CON
C>> 1987-12-09 SMPDRV Lawson  Initial code.
C     DERIVATIVE OF POLYNOMIAL REPRESENTED BY COEFFS REL TO THE
C     MONOMIAL BASIS INCLUDING A LINEAR TRANSFORMATION OF THE
C     ARGUMENT.
C     C.L.LAWSON, JPL, 1973 DEC 6
C
C     (C(I),I=1,2)       SCALE FACTORS
C     (C(I+2),I=1,NA+1)  COEFS OF POLY
C     NC                 DEGREE OF POLY
C     (D(I),I=1,2)       OUTPUT..  SCALE FACTORS
C     (D(I+2),I=1,ND+1)OUTPUT..COEFSOFDIFFERENTIATED
C                                  POLY
C     ND                 OUTPUT..  DEGREE OF POLY
c     -----------------------------------------------------------------
c--S replaces "?": ?MPDRV
c     Both versions use IERM1
c     -----------------------------------------------------------------
      integer i, nc, ncp2, nd
      real             d(*), c(*), fac
c     -----------------------------------------------------------------
      if (nc .lt. 0) then
        call IERM1('SMPDRV',1,0,'REQUIRE NC .GE. 0','NC',nc,'.')
      else
        d(1)=c(1)
        d(2)=c(2)
        if (nc .eq. 0) then
          nd=0
          d(3)=0.0e0
        else
          ncp2=nc+2
          nd  =ncp2-3
          fac =0.0e0
          do 20 i=3,ncp2
            fac=fac+1.0e0
   20       d(i)=c(i+1)*fac/d(2)
        endif
      end if
      return
      end
