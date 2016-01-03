c     program DRSBLAS1
c>> 1996-06-18 DRSBLAS1 Krogh  Minor format change for C conversion.
c>> 1996-05-28 DRSBLAS1 Krogh Added external statement.
c>> 1994-10-19 DRSBLAS1 Krogh  Changes to use M77CON
c>> 1992-03-24 DRSBLAS1 CLL Removed reference to REAL() function.
c>> 1991-12-02 DRSBLAS1 CLL
c>> 1991-07-25 DRSBLAS1 CLL
c>> 1987-12-09 DRBLAS1  Lawson  Initial Code.
c
c     Demonstrate usage of SAXPY, SCOPY, and SDOT from the BLAS
c     by computing
c     (1) p = A * b              using SDOT
c     (2) q = A * b              using SCOPY & SAXPY
c     (3) r = (A Transposed) * p using SDOT
c     (4) S = A * E              using SDOT
c     ------------------------------------------------------------------
c--S replaces "?": DR?BLAS1, ?AXPY, ?COPY, ?DOT
c     ------------------------------------------------------------------
      external SDOT
      real             SDOT
      integer M2, M3, M4, N2, N3, N4
      parameter ( M2=5, M3=10, M4=12 )
      parameter ( N2=2, N3=3, N4=4 )
      integer I, J
      real             A(M2,M3), E(M3,M4), S(M2,M4)
      real             B(M3), P(M2), Q(M2), R(M3)
      real             ZERO(1)
c
      data ZERO(1) / 0.0e0 /
      data (A(1,J),J=1,N3) /  2.0e0, -4.0e0, 3.0e0 /
      data (A(2,J),J=1,N3) / -5.0e0, -2.0e0, 6.0e0 /
      data (B(J),J=1,N3)   /  7.0e0, -3.0e0, 5.0e0 /
      data (E(1,J),J=1,N4) / -4.0e0, 2.0e0,  3.0e0, -6.0e0 /
      data (E(2,J),J=1,N4) /  7.0e0, 5.0e0, -6.0e0, -3.0e0 /
      data (E(3,J),J=1,N4) /  3.0e0, 4.0e0, -2.0e0,  5.0e0 /
c     ---------------------------------------------------------------
c
c                                  1. p = A * b using SDOT
c
      do 10 I = 1, N2
        P(I) = SDOT(N3,A(I,1),M2,B,1)
   10 continue
c
c                                  2. q = A * b using SCOPY and SAXPY
c
      call SCOPY(N2,ZERO,0,Q,1)
      do 20 J = 1, N3
        call SAXPY(N2,B(J),A(1,J),1,Q,1)
   20 continue
c
c                                  3. r = (A Transposed) * p using SDOT
c
      do 30 J = 1, N3
        R(J) = SDOT(N2,A(1,J),1,P,1)
   30 continue
c
c                                  4. S = A * E using SDOT
c
      do 50 I = 1, N2
        do 40 J = 1, N4
          S(I,J) = SDOT(N3,A(I,1),M2,E(1,J),1)
   40   continue
   50 continue
c
      print*,'DRSBLAS1..  Demo driver for SAXPY, SCOPY, and SDOT'
      print'(/'' P() = '', 7x,4f8.1)', (P(J),J=1,N2)
      print'(/'' Q() = '',7x,4f8.1)', (Q(J),J=1,N2)
      print'(/'' R() = '',7x,4f8.1)', (R(J),J=1,N3)
      print'(/'' S(,) = '')'
      do 60 I = 1,N2
         print'(''   Row '',i2,5x,4f8.1)', I,(S(I,J),J=1,N4)
   60 continue
      stop
      end
