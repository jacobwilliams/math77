      subroutine SSYMQL(A, LDA, N, EVAL, WORK, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 SSYMQL  Krogh  Changes to use M77CON
c>> 1992-04-24 SSYMQL  CLL  Minor edits.
c>> 1992-04-08 SSYMQL  Krogh Unused label 130 removed.
c>> 1991-10-23 SSYMQL  Krogh Initial version, converted from EISPACK.
c
c     This subroutine is a slight modification of the EISPACK subroutine
c     TRED2 (dated August 1983) which reduces a real matrix to a
c     symmetric tridiagonal matrix using and accumulating orthogonal
c     similarity transformations.  This subroutine then calls SIMQL to
c     compute the eigenvalues and eigenvectors of the matrix.
c     TRED2 was in turn a translation of the ALGOL procedure, TRED2,
c     Num. Math. 11, 181-195(1968) by Martin, Reinsch, and Wilkinson.
c
c     On input
c
c     LDA  must be set to the row dimension of the two-dimensional array
c          A as declared in the calling program dimension statement.
c
c     N    is the order of the matrix.
c
c     A    contains the real symmetric input matrix.  only the
c          lower triangle of the matrix need be supplied.
c
c     On output
c
c     A    contains the eigenvectors.
c
c     EVAL contains the eigenvalues in ascending order.  If an
c          error exit is made, the eigenvalues are correct but
c          unordered for indices 1,2,...,IERR-1.
c
c     WORK used for working storage.
c
c     IERR is set to:
c          zero  for normal return,
c          J     if the J-th eigenvalue has not been determined after 30
c                iterations.
c     ------------------------------------------------------------------
c--S replaces "?": ?SYMQL, ?IMQL
c     ------------------------------------------------------------------
      integer N, LDA, IERR
      integer I, J, K, L
      real             A(LDA,N), EVAL(N), WORK(N)
      real             F, G, H, HH, SCALE
c
c     ------------------------------------------------------------------
c
      do 100 I = 1, N
         EVAL(I) = A(N,I)
  100 continue
c
      if (N .le. 1) then
         if (N .eq. 1) then
            A(1,1) = 1.0E0
         else
            call IERM1('SSYMQL',-1, 0, 'Require N > 0.', 'N', N, '.')
            IERR = -1
         end if
         return
      end if
      do 300 I = N, 2, -1
         L = I - 1
         H = 0.0E0
         SCALE = 0.0E0
         if (L .ge. 2) then
c     .......... scale row (algol tol then not needed) ..........
            do 120 K = 1, L
               SCALE = SCALE + abs(EVAL(K))
  120       continue
            if (SCALE .ne. 0.0E0) go to 140
         end if
c
         WORK(I) = EVAL(L)
c
         do 135 J = 1, L
            EVAL(J) = A(L,J)
            A(I,J) = 0.0E0
            A(J,I) = 0.0E0
  135    continue
c
         go to 290
c
  140    do 150 K = 1, L
            EVAL(K) = EVAL(K) / SCALE
            H = H + EVAL(K) * EVAL(K)
  150    continue
c
         F = EVAL(L)
         G = -sign(sqrt(H),F)
         WORK(I) = SCALE * G
         H = H - F * G
         EVAL(L) = F - G
c     .......... form A*u ..........
         do 170 J = 1, L
            WORK(J) = 0.0E0
  170    continue
c
         do 240 J = 1, L
            F = EVAL(J)
            A(J,I) = F
            G = WORK(J) + A(J,J) * F
c
            do 200 K = J+1, L
               G = G + A(K,J) * EVAL(K)
               WORK(K) = WORK(K) + A(K,J) * F
  200       continue
c
            WORK(J) = G
  240    continue
c     .......... form P ..........
         F = 0.0E0
c
         do 245 J = 1, L
            WORK(J) = WORK(J) / H
            F = F + WORK(J) * EVAL(J)
  245    continue
c
         HH = F / (H + H)
c     .......... form q ..........
         do 250 J = 1, L
            WORK(J) = WORK(J) - HH * EVAL(J)
  250    continue
c     .......... form reduced A ..........
         do 280 J = 1, L
            F = EVAL(J)
            G = WORK(J)
c
            do 260 K = J, L
               A(K,J) = A(K,J) - F * WORK(K) - G * EVAL(K)
  260       continue
c
            EVAL(J) = A(L,J)
            A(I,J) = 0.0E0
  280    continue
c
  290    EVAL(I) = H
  300 continue
c     .......... accumulation of transformation matrices ..........
      do 500 I = 2, N
         L = I - 1
         A(N,L) = A(L,L)
         A(L,L) = 1.0E0
         H = EVAL(I)
         if (H .ne. 0.0E0) then
c
            do 330 K = 1, L
               EVAL(K) = A(K,I) / H
  330       continue
c
            do 360 J = 1, L
               G = 0.0E0
c
               do 340 K = 1, L
                  G = G + A(K,I) * A(K,J)
  340          continue
c
               do 350 K = 1, L
                  A(K,J) = A(K,J) - G * EVAL(K)
  350          continue
  360       continue
         end if
c
         do 400 K = 1, L
            A(K,I) = 0.0E0
  400    continue
c
  500 continue
c
      do 520 I = 1, N
         EVAL(I) = A(N,I)
         A(N,I) = 0.0E0
  520 continue
c
      A(N,N) = 1.0E0
      call SIMQL(A, LDA, N, EVAL, WORK, IERR)
      return
      end
