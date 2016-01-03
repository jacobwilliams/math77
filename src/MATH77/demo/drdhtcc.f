c     program DRDHTCC
c>> 1994-10-19 DRDHTCC  Krogh  Changes to use M77CON
c>> 1987-12-09 DRDHTCC  Lawson  Initial Code.
c     Demo driver for DHTCC, Householder transformations.
c--D replaces "?": DR?HTCC, ?HTCC, ?MATP
      integer IDIM, JDIM, J, M, N
      parameter(IDIM = 3, JDIM = 5, M = 3, N = 2)
      double precision D(IDIM,JDIM), UPARAM, ZERO
      parameter(ZERO = 0.0D0)
      data (D(1,J),J=1,5) /  0.870D0,  0.796D0,    1.0D0, 0.0D0, 0.0D0 /
      data (D(2,J),J=1,5) /  0.571D0, -0.804D0,    0.0D0, 1.0D0, 0.0D0 /
      data (D(3,J),J=1,5) / -0.960D0,  0.346D0,    0.0D0, 0.0D0, 1.0D0 /
c     ------------------------------------------------------------------
      print*,'DRDHTCC..  Demonstrate computation of a QR decomposition.'
      print*,'Given matrix A, compute orthogonal Q and triangular R'
      print*,'such that A = Q * R.'
      call DMATP(D,IDIM,M,N,'0 A =')
      do 20002 J=1,N
         call DHTCC(1,J,J+1,M,D(1,J),UPARAM,D(1,J+1),IDIM,M+N-J)
20002 continue
      call DMATP(D(1,N+1),IDIM,M,M,'0 Q [Transposed] =')
      D(2,1) = ZERO
      D(3,1) = ZERO
      D(3,2) = ZERO
      call DMATP(D,IDIM,M,N,'0 R =')
      stop
      end
