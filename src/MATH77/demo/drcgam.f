      program DRCGAM
c>> 2001-05-22 DRCGAM Krogh Minor change for making .f90 version.
c>> 1996-06-14 DRCGAM Krogh Changes for C conversion.
c>> 1994-08-17 CLL
c>> 1991-10-18 Demo driver for CGAM
c     ------------------------------------------------------------------
c--   D version uses DRZGAM, ZGAM
c--   S version uses DRCGAM, CGAM
c     ------------------------------------------------------------------
      real ARG(2),GAM(2),LGAM(2), X(2,8)
      real ERR0, ERR1
      integer I, J
      data ( ( X(I,J),I=1,2 ), J=1,8 ) /
     * 1.0E0,0.0E0, 1.0E0,0.5E0, 1.5E0,0.0E0, 1.5E0,0.5E0,
     * 0.0E0,0.5E0, -1.5E0,0.5E0,  -1.5E0,0.0E0,  -1.5E0,-0.5E0 /
c     ------------------------------------------------------------------
      print '(5x,''X          GAMMA        Err Est'', 11x,
     * ''LGAMA         Err Est'')'
      do 10 J = 1,8
        ARG(1) = X(1,J)
        ARG(2) = X(2,J)
        call CGAM (ARG,GAM,ERR1,1)
        call CGAM (ARG,LGAM,ERR0,0)
        print
     *  '(1x,f5.1,g20.10,g10.2,g22.10,g10.2/1x,f5.1,g20.10,g32.10/)',
     *        ARG(1),GAM(1),ERR1, LGAM(1), ERR0,
     *        ARG(2),GAM(2),      LGAM(2)
  10  continue
      end
