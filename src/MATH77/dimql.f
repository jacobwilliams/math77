      subroutine DIMQL(A, LDA, N, EVAL, WORK, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1994-10-20 DIMQL  Krogh  Changes to use M77CON
c>> 1992-04-24 DIMQL  CLL   Minor edits
c>> 1991-10-23 DIMQL  Krogh Initial version, converted from EISPACK.
c
c     This subroutine is a slight modification of the EISPACK subroutine
c     IMTQL2 (dated August 1983) which computes the eigenvalues and
c     eigenvectors of a real symmetric tridiagonal matrix.
c     IMTQL2 was in turn a translation of the ALGOL procedure, IMTQL2,
c     Num. Math. 12, 377-383(1968) by Martin and Wilkinson as modified
c     in Num. Math. 15, 450(1970) by Dubrelle.  Handbook for Auto.
c     Comp., vol.ii-linear algebra, 241-248(1971).
c
c     On input
c     LDA  must be set to the row dimension of the two-dimensional array
c          A as declared in the calling program dimension statement.
c     N    is the order of the matrix.
c     EVAL contains the diagonal elements of the input matrix.
c     WORK contains the subdiagonal elements of the input matrix
c          in its last n-1 positions.  e(1) is arbitrary.
c     A    contains the transformation matrix produced in the
c          reduction to tridiagonal form.  if the eigenvectors
c          of the tridiagonal matrix are desired, A must contain
c          the identity matrix.
c
c     On output
c     A    contains the eigenvectors.
c     EVAL contains the eigenvalues in ascending order.  If an
c          error exit is made, the eigenvalues are correct but
c          unordered for indices 1,2,...,IERR-1.
c     WORK used for working storage.
c     IERR is set to:
c          zero  for normal return,
c          J     if the J-th eigenvalue has not been determined after 30
c                iterations.
c     ------------------------------------------------------------------
c--D replaces "?": ?IMQL
c     ------------------------------------------------------------------
      integer N, LDA, IERR
      integer I, J, K, L, M
      double precision A(LDA,N), EVAL(N), WORK(N)
      double precision F, G, B, C, P, R, S, TST1, TST2
c
c     ------------------------------------------------------------------
c
      IERR = 0
c
      do 600 I = 2, N
         WORK(I-1) = WORK(I)
  600 continue
c
      WORK(N) = 0.0D0
c
      do 740 L = 1, N
         J = 0
c     .......... look for small sub-diagonal element ..........
  605    do 610 M = L, N
            if (M .eq. N) go to 620
            TST1 = abs(EVAL(M)) + abs(EVAL(M+1))
            TST2 = TST1 + abs(WORK(M))
            if (TST2 .eq. TST1) go to 620
  610    continue
c
  620    P = EVAL(L)
         if (M .ne. L) then
            if (J .eq. 30) then
c                   .......... set error -- no convergence to an
c                              eigenvalue after 30 iterations ..........
               call ERMSG('DIMQL',L, 0,
     1'ERROR NO. is index of eigenvalue causing convergence failure.',
     2           '.')
               IERR = L
               return
            end if
            J = J + 1
c        .......... form shift ..........
            G = (EVAL(L+1) - P) / (2.0D0 * WORK(L))
            if (abs(G) .gt. 1.0D0) then
               R = G * sqrt(1.0D0 + 1.0D0 / G ** 2)
            else
               R = sign(sqrt(1.0D0 + G ** 2), G)
            end if
            G = EVAL(M) - P + WORK(L) / (G + R)
            S = 1.0D0
            C = 1.0D0
            P = 0.0D0
            do 700 I = M-1, L, -1
               F = S * WORK(I)
               B = C * WORK(I)
               if (abs(F) .gt. abs(G)) then
                  R = abs(F) * sqrt(1.D0 + (G / F)**2)
               else if (G .ne. 0.0D0) then
                  R = abs(G) * sqrt(1.D0 + (F / G)**2)
               else
c        .......... recover from underflow ..........
                  WORK(I+1) = 0.0D0
                  EVAL(I+1) = EVAL(I+1) - P
                  WORK(M) = 0.0D0
                  go to 605
               end if
               WORK(I+1) = R
               S = F / R
               C = G / R
               G = EVAL(I+1) - P
               R = (EVAL(I) - G) * S + 2.0D0 * C * B
               P = S * R
               EVAL(I+1) = G + P
               G = C * R - B
c        .......... form vector ..........
               do 680 K = 1, N
                  F = A(K,I+1)
                  A(K,I+1) = S * A(K,I) + C * F
                  A(K,I) = C * A(K,I) - S * F
  680          continue
c
  700       continue
c
            EVAL(L) = EVAL(L) - P
            WORK(L) = G
            WORK(M) = 0.0D0
            go to 605
         end if
  740 continue
c     .......... order eigenvalues and eigenvectors ..........
      do 800 I = 1, N - 1
         K = I
         P = EVAL(I)
c
         do 760 J = I+1, N
            if (EVAL(J) .lt. P) then
               K = J
               P = EVAL(J)
            end if
  760    continue
c
         if (K .ne. I) then
            EVAL(K) = EVAL(I)
            EVAL(I) = P
c
            do 780 J = 1, N
               P = A(J,I)
               A(J,I) = A(J,K)
               A(J,K) = P
  780       continue
         end if
c
  800 continue
c
      return
      end
