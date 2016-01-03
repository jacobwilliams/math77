      program DRMATP
c>> 2001-05-22 DRMATP Krogh Minor change for making .f90 version.
c>> 1992-04-22 CLL
c>> 1987-12-09 DRMATP Lawson  Initial Code.
c
      integer I, J, M, N
c
      integer A(4,3)
      real B(4,3)
      double precision C(4,3)
      data ((A(I,J),J=1,3),I=1,4) / 1,2,3,4,5,6,7,8,9,10,11,12 /
      data ((B(I,J),J=1,3),I=1,4) / 1.,2.,3.,4.,5.,6.,7.,8.,9.,
     *                              10.,11.,12. /
      data ((C(I,J),J=1,3),I=1,4) / 1.D0,2.D0,3.D0,4.D0,5.D0,
     *                              6.D0,7.D0,8.D0,9.D0,10.D0,
     *                              11.D0,12.D0 /
      data M / 4 /
      data N / 3 /
c
      call IMATP (A,M,M,N,'0 IMATP.. A(,)=')
      call SMATP (B,M,M,N,'0 SMATP.. B(,)=')
      call DMATP (C,M,M,N,'0 DMATP.. C(,)=')
c
      end
