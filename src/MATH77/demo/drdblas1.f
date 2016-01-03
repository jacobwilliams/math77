c     program DRDBLAS1
c>> 1996-06-18 DRDBLAS1 Krogh  Minor format change for C conversion.
c>> 1996-05-28 DRDBLAS1 Krogh Added external statement.
c>> 1994-10-19 DRDBLAS1 Krogh  Changes to use M77CON
c>> 1992-03-24 DRDBLAS1 CLL Removed reference to DBLE() function.
c>> 1991-12-02 DRDBLAS1 CLL
c>> 1991-07-25 DRDBLAS1 CLL
c>> 1987-12-09 DRBLAS1  Lawson  Initial Code.
c
c     Demonstrate usage of DAXPY, DCOPY, and DDOT from the BLAS
c     by computing
c     (1) p = A * b              using DDOT
c     (2) q = A * b              using DCOPY & DAXPY
c     (3) r = (A Transposed) * p using DDOT
c     (4) S = A * E              using DDOT
c     ------------------------------------------------------------------
c--D replaces "?": DR?BLAS1, ?AXPY, ?COPY, ?DOT
c     ------------------------------------------------------------------
      external DDOT
      double precision DDOT
      integer M2, M3, M4, N2, N3, N4
      parameter ( M2=5, M3=10, M4=12 )
      parameter ( N2=2, N3=3, N4=4 )
      integer I, J
      double precision A(M2,M3), E(M3,M4), S(M2,M4)
      double precision B(M3), P(M2), Q(M2), R(M3)
      double precision ZERO(1)
c
      data ZERO(1) / 0.0d0 /
      data (A(1,J),J=1,N3) /  2.0d0, -4.0d0, 3.0d0 /
      data (A(2,J),J=1,N3) / -5.0d0, -2.0d0, 6.0d0 /
      data (B(J),J=1,N3)   /  7.0d0, -3.0d0, 5.0d0 /
      data (E(1,J),J=1,N4) / -4.0d0, 2.0d0,  3.0d0, -6.0d0 /
      data (E(2,J),J=1,N4) /  7.0d0, 5.0d0, -6.0d0, -3.0d0 /
      data (E(3,J),J=1,N4) /  3.0d0, 4.0d0, -2.0d0,  5.0d0 /
c     ---------------------------------------------------------------
c
c                                  1. p = A * b using DDOT
c
      do 10 I = 1, N2
        P(I) = DDOT(N3,A(I,1),M2,B,1)
   10 continue
c
c                                  2. q = A * b using DCOPY and DAXPY
c
      call DCOPY(N2,ZERO,0,Q,1)
      do 20 J = 1, N3
        call DAXPY(N2,B(J),A(1,J),1,Q,1)
   20 continue
c
c                                  3. r = (A Transposed) * p using DDOT
c
      do 30 J = 1, N3
        R(J) = DDOT(N2,A(1,J),1,P,1)
   30 continue
c
c                                  4. S = A * E using DDOT
c
      do 50 I = 1, N2
        do 40 J = 1, N4
          S(I,J) = DDOT(N3,A(I,1),M2,E(1,J),1)
   40   continue
   50 continue
c
      print*,'DRDBLAS1..  Demo driver for DAXPY, DCOPY, and DDOT'
      print'(/'' P() = '', 7x,4f8.1)', (P(J),J=1,N2)
      print'(/'' Q() = '',7x,4f8.1)', (Q(J),J=1,N2)
      print'(/'' R() = '',7x,4f8.1)', (R(J),J=1,N3)
      print'(/'' S(,) = '')'
      do 60 I = 1,N2
         print'(''   Row '',i2,5x,4f8.1)', I,(S(I,J),J=1,N4)
   60 continue
      stop
      end
