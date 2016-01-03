      program DRVECP
c>> 2001-05-22 DRVECPR Krogh Minor change for making .f90 version.
c>> 1992-04-22 CLL
c>> 1987-12-09 DRVECP   Lawson  Initial Code.
c
      integer A(4), I, N
      real B(4)
      double precision C(4)
c
      data (A(I),I=1,4) / 1,2,3,4 /
      data (B(I),I=1,4) / 1.,2.,3.,4. /
      data (C(I),I=1,4) / 1.D0, 2.D0, 3.D0, 4.D0 /
      data N / 4 /
c
      call IVECP (A,N,'0 IVECP.. A()=')
      call SVECP (B,N,'0 SVECP.. B()=')
      call DVECP (C,N,'0 DVECP.. C()=')
c
      end
