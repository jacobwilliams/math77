c     program DRSHTCC
c>> 1994-10-19 DRSHTCC  Krogh  Changes to use M77CON
c>> 1987-12-09 DRSHTCC  Lawson  Initial Code.
c     Demo driver for SHTCC, Householder transformations.
c--S replaces "?": DR?HTCC, ?HTCC, ?MATP
      integer IDIM, JDIM, J, M, N
      parameter(IDIM = 3, JDIM = 5, M = 3, N = 2)
      real             D(IDIM,JDIM), UPARAM, ZERO
      parameter(ZERO = 0.0E0)
      data (D(1,J),J=1,5) /  0.870E0,  0.796E0,    1.0E0, 0.0E0, 0.0E0 /
      data (D(2,J),J=1,5) /  0.571E0, -0.804E0,    0.0E0, 1.0E0, 0.0E0 /
      data (D(3,J),J=1,5) / -0.960E0,  0.346E0,    0.0E0, 0.0E0, 1.0E0 /
c     ------------------------------------------------------------------
      print*,'DRSHTCC..  Demonstrate computation of a QR decomposition.'
      print*,'Given matrix A, compute orthogonal Q and triangular R'
      print*,'such that A = Q * R.'
      call SMATP(D,IDIM,M,N,'0 A =')
      do 20002 J=1,N
         call SHTCC(1,J,J+1,M,D(1,J),UPARAM,D(1,J+1),IDIM,M+N-J)
20002 continue
      call SMATP(D(1,N+1),IDIM,M,M,'0 Q [Transposed] =')
      D(2,1) = ZERO
      D(3,1) = ZERO
      D(3,2) = ZERO
      call SMATP(D,IDIM,M,N,'0 R =')
      stop
      end
