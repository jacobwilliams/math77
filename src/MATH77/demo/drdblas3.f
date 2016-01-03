c     program DRDBLAS3
c>> 2001-05-22 DRDBLAS3 Krogh Minor change for making .f90 version.
c>> 1996-05-28 DRDBLAS3 Krogh Added external statement.
c>> 1994-10-19 DRDBLAS3 Krogh  Changes to use M77CON
c>> 1992-03-16 DRDBLAS3 CLL
c>> 1991-07-25 DRDBLAS3 CLL
c
c     Demonstrate usage of
c     DASUM, DNRM2, DSCAL, DSWAP, and IDAMAX
c     from the BLAS.
c     Also uses DAXPY.
c     ------------------------------------------------------------------
c--D replaces "?": DR?BLAS3, ?AXPY, ?ASUM, ?NRM2
c--&                 ?SCAL, ?SWAP, I?AMAX
c     ------------------------------------------------------------------
      external DASUM, DNRM2, IDAMAX
      double precision DASUM, DNRM2
      integer IDAMAX
      integer M3, M4, N3, N4
      parameter ( M3=10, M4=12 )
      parameter ( N3=3, N4=4 )
      integer IT1, J
      double precision E(M3,M4)
      double precision B(M3), C(5), C2(5), C3(5), D(5), D2(5), D3(5)
      double precision T1
c
      data (B(J),J=1,N3)   /  7.0d0, -3.0d0, 5.0d0 /
      data (C(J),J=1,5)   /  7.0d0, -3.0d0,  5.0d0, -4.0d0, 1.0d0 /
      data (C2(J),J=1,5)   /  7.0d0, -3.0d0,  5.0d0, -4.0d0, 1.0d0 /
      data (C3(J),J=1,5)   /  7.0d0, -3.0d0,  5.0d0, -4.0d0, 1.0d0 /
      data (D(J),J=1,5)   / 14.0d0, -6.0d0, 10.0d0, -8.0d0, 2.0d0 /
      data (D2(J),J=1,5)   / 14.0d0, -6.0d0, 10.0d0, -8.0d0, 2.0d0 /
      data (D3(J),J=1,5)   / 14.0d0, -6.0d0, 10.0d0, -8.0d0, 2.0d0 /

      data (E(1,J),J=1,N4) / -4.0d0, 2.0d0,  3.0d0, -6.0d0 /
      data (E(2,J),J=1,N4) /  7.0d0, 5.0d0, -6.0d0, -3.0d0 /
      data (E(3,J),J=1,N4) /  3.0d0, 4.0d0, -2.0d0,  5.0d0 /

c     ---------------------------------------------------------------
c
      T1 = DASUM(N3, B, 1) - 15.0d0
      print*,'Test of DASUM(): ',T1
c
      T1 = DNRM2(5, C, 1) - 10.0d0
      print*,'Test of DNRM2(): ',T1
c
      call DSCAL(5, 2.0d0, C2, 1)
      call DAXPY(5, -1.0d0, C2, 1, D2, 1)
      T1 = DASUM(5, D2, 1)
      print*,'Test of DSCAL(): ',T1
c
      call DSWAP(5, C3, 1, D3, 1)
      call DAXPY(5, -1.0d0, C, 1, D3, 1)
      call DAXPY(5, -1.0d0, D, 1, C3, 1)
      T1 = DASUM(5, C3, 1) + DASUM(5, D3, 1)
      print*,'Test of DSWAP(): ',T1
c
      IT1 = IDAMAX(N4, E(1,1), M3) - 4
      print*,'Test of IDAMAX(): ',IT1
      stop
      end
