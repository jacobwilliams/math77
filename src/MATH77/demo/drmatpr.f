c     DRMATPR
c>> 2006-04-02 DRMATPR Krogh Added checks with exponents.
c>> 2001-05-22 DRMATPR Krogh Minor change for making .f90 version.
c>> 1991-11-22 DRMATPR  Krogh Initial Code.
c
      integer LWIDTH, LUNIT, NUMDIG, M, N, I, J
      parameter (LWIDTH=79)
      parameter (LUNIT=0)
      parameter (NUMDIG = 0)
      parameter (M = 4)
      parameter (N = 3)
c
      integer A(M,N)
      real B(M,N)
      double precision C1(M,N), C2(M,N), C3(M,N)
c
      data ((A(I,J),J=1,N),I=1,M) / 1,2,3,4,5,6,7,8,9,10,11,12 /
      data ((B(I,J),J=1,N),I=1,M) / 1.,2.,3.,4.,5.,6.,7.,8.,9.,
     *                              10.,11.,12. /
      data ((C1(I,J),J=1,N),I=1,M) / 1.D0,2.D0,3.D0,4.D0,5.D0,
     *                              6.D0,7.D0,8.D0,9.D0,10.D0,
     *                              11.D0,12.D0 /
      data ((C2(I,J),J=1,N),I=1,M) / 1.D0,2.D0,3.D0,4.D0,5.D0,
     *                              6.D0,7.D0,8.D99,9.D0,10.D0,
     *                              -11.D0,12.D0 /
      data ((C3(I,J),J=1,N),I=1,M) / 1.D0,2.D0,3.D0,4.D0,5.D0,
     *                              6.D0,7.D0,8.D123,9.D0,10.D0,
     *                              11.D0,12.D0 /
c
      call IMATPR (A,M,M,N,'0 IMATPR.. A(,)=', LWIDTH, LUNIT)
      call SMATPR (B,M,M,N,'0 SMATPR.. B(,)=', LWIDTH, LUNIT, NUMDIG)
      call DMATPR (C1,M,M,N,'0 DMATPR.. C1(,)=', LWIDTH, LUNIT, NUMDIG)
      call DMATPR (C2,M,M,N,'0 DMATPR.. C2(,)=', LWIDTH, LUNIT, NUMDIG)
      call DMATPR (C3,M,M,N,'0 DMATPR.. C3(,)=', LWIDTH, LUNIT, NUMDIG)
      stop
c
      end
