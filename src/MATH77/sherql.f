      subroutine SHERQL(AR, AI, LDA, N, EVAL, VR, VI, WORK, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-10-20 SHERQL Krogh  Changes to use M77CON
C>> 1994-04-20 SHERQL CLL Edited comment to make DP & SP files similar.
c>> 1992-04-23 CLL  Declaring all variables.
c>> 1992-04-08 SHERQL  Removed unused label 220.
c>> 1991-10-23 SHERQL  Krogh Initial version, converted from EISPACK.
c
c     This subroutine reduces a complex hermitian matrix to a real
c     symmetric tridiagonal matrix using unitary similarity
c     transformations, computes the eigenvalues and eigenvectors of the
c     tridiagonal matrix, and then computes the eigenvectors of the
c     original matrix.
c
c     This subroutine is a concatenation of a slight modification of the
c     EISPACK subroutine HTRIDI, a call to SIMQL which is a slight
c     modification of the EISPACK subroutine IMTQL2, and a slight
c     modification of the EISPACK subroutine HTRIBK.  These subroutines
c     in turn are a translation of a complex analogue of the ALGOL
c     procedure TRED1, Num. Math. 11, 181-195(1968) by Martin, Reinsch,
c     and Wilkinson. Handbook for Auto. Comp., Vol.ii-Linear Algebra,
c     212-226(1971), and the complex analogue of the ALGOL procedure
c     TRBAK1, Num. Math. 11, 181-195(1968) by Martin, Reinsch, and
c     Wilkinson.  Handbook for Auto. Comp., vol.ii-Linear Algebra,
c     212-226(1971).
c     EISPACK routines are dated August 1983.
c
c     On input
c
c        AR and AI contain the real and imaginary parts,
c          respectively, of the complex hermitian input matrix.
c          only the lower triangle of the matrix need be supplied.
c
c        LDA must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        N is the order of the matrix.
c
c     On output
c
c     AR and AI contain information about the unitary transformations
c          used in the reduction in their full lower triangles.  Their
c          strict upper triangles and the diagonal of AR are unaltered.
c
c     EVAL contains the eigenvalues in ascending order.  If an
c          error exit is made, the eigenvalues are correct but
c          unordered for indices 1,2,...,IERR-1, and no eigenvectors are
c          computed.
c
c     VR and VI contain the the real and imaginary parts of the
c     eigenvectors with column k corresponding to the eigenvalue in
c     EVAL(k).  Note that the last component of each returned vector
c     is real and that vector euclidean norms are preserved.
c
c     WORK used for working storage.
c
c     IERR is set to:
c          zero  for normal return,
c          J     if the J-th eigenvalue has not been determined after 30
c                iterations.
c
c     ------------------------------------------------------------------
c--S replaces "?": ?HERQL, ?IMQL
c     Both versions use IERM1
c     ------------------------------------------------------------------
      integer I, IERR, J,K,L,N,LDA,N1,N2
      real             AR(LDA,N),AI(LDA,N),EVAL(N),WORK(*)
      real             VR(LDA,N),VI(LDA,N)
      real             F,G,H,FI,GI,HH,SI,SCALE
      real             S
c     ------------------------------------------------------------------
      N1 = N
      N2 = N1 + N1
      WORK(N2) = 1.0E0
      WORK(N2 + N1) = 0.0E0
c
      do 100 I = 1, N1
         EVAL(I) = AR(I,I)
  100 continue
      if (N1 .le. 1) then
         if (N1 .eq. 1) then
            AR(1,1) = 1.0e0
            AI(1,1) = 0.0e0
         else
            call IERM1('SHERQL',-1, 0, 'Require N > 0.', 'N', N1, '.')
            IERR = -1
         end if
         return
      end if
      do 300 I = N1, 1, -1
         L = I - 1
         H = 0.0E0
         SCALE = 0.0E0
         if (L .lt. 1) go to 130
c     .......... scale row (algol tol then not needed) ..........
         do 120 K = 1, L
            SCALE = SCALE + abs(AR(I,K)) + abs(AI(I,K))
  120    continue
c
         if (SCALE .ne. 0.0E0) go to 140
         WORK(N1 + L) = 1.0E0
         WORK(N2 + L) = 0.0E0
  130    WORK(I) = 0.0E0
         go to 290
c
  140    do 150 K = 1, L
            AR(I,K) = AR(I,K) / SCALE
            AI(I,K) = AI(I,K) / SCALE
            H = H + AR(I,K) * AR(I,K) + AI(I,K) * AI(I,K)
  150    continue
c
         G = sqrt(H)
         WORK(I) = SCALE * G
         F = abs(AR(I,L))
         SI = abs(AI(I,L))
         if (F .gt. SI) then
            F = F * sqrt(1.0e0 + (SI / F) **2)
         else if (SI .ne. 0.0e0) then
            F = SI * sqrt(1.0e0 + (F / SI) **2)
         else
            WORK(N1 + L) = -WORK(N1 + I)
            SI = WORK(N2 + I)
            AR(I,L) = G
            go to 170
         end if
c     .......... Form next diagonal element of matrix t ..........
         WORK(N1+L) = (AI(I,L) * WORK(N2+I) - AR(I,L) * WORK(N1+I)) / F
         SI = (AR(I,L) * WORK(N2 + I) + AI(I,L) * WORK(N1 + I)) / F
         H = H + F * G
         G = 1.0E0 + G / F
         AR(I,L) = G * AR(I,L)
         AI(I,L) = G * AI(I,L)
         if (L .eq. 1) go to 270
         F = 0.0E0
c
  170    do 240 J = 1, L
            G = 0.0E0
            GI = 0.0E0
c     .......... form element of a*u ..........
            do 180 K = 1, J
               G = G + AR(J,K) * AR(I,K) + AI(J,K) * AI(I,K)
               GI = GI - AR(J,K) * AI(I,K) + AI(J,K) * AR(I,K)
  180       continue
c
            do 200 K = J+1, L
               G = G + AR(K,J) * AR(I,K) - AI(K,J) * AI(I,K)
               GI = GI - AR(K,J) * AI(I,K) - AI(K,J) * AR(I,K)
  200       continue
c     .......... form element of p ..........
            WORK(J) = G / H
            WORK(N2 + J) = GI / H
            F = F + WORK(J) * AR(I,J) - WORK(N2 + J) * AI(I,J)
  240    continue
c
         HH = F / (H + H)
c     .......... form reduced a ..........
         do 260 J = 1, L
            F = AR(I,J)
            G = WORK(J) - HH * F
            WORK(J) = G
            FI = -AI(I,J)
            GI = WORK(N2 + J) - HH * FI
            WORK(N2 + J) = -GI
c
            do 250 K = 1, J
               AR(J,K) = AR(J,K) - F * WORK(K) - G * AR(I,K)
     x                           + FI * WORK(N2 + K) + GI * AI(I,K)
               AI(J,K) = AI(J,K) - F * WORK(N2 + K) - G * AI(I,K)
     x                           - FI * WORK(K) - GI * AR(I,K)
  250       continue
  260    continue
c
  270    do 280 K = 1, L
            AR(I,K) = SCALE * AR(I,K)
            AI(I,K) = SCALE * AI(I,K)
  280    continue
c
         WORK(N2 + L) = -SI
  290    HH = EVAL(I)
         EVAL(I) = AR(I,I)
         AR(I,I) = HH
         AI(I,I) = SCALE * sqrt(H)
  300 continue
c
c     ------------------------------------------------------------------
c
      do 420 I = 1, N1
         do 410 J = 1, N1
            VR(I,J) = 0.0e0
  410    continue
         VR(I,I) = 1.0e0
  420 continue
      call SIMQL(VR, LDA, N1, EVAL, WORK, IERR)
      if (IERR .ne. 0) return
c
c     ------------------------------------------------------------------
c
c     .......... transform the eigenvectors of the real symmetric
c                tridiagonal matrix to those of the hermitian
c                tridiagonal matrix. ..........
      do 550 K = 1, N1
c
         do 540 J = 1, N1
            VI(K,J) = -VR(K,J) * WORK(N2 + K)
            VR(K,J) = VR(K,J) * WORK(N1 + K)
  540    continue
  550 continue
c
c     .......... Recover and apply the Householder matrices ..........
      do 640 I = 2, N1
         L = I - 1
         H = AI(I,I)
         if (H .ne. 0.0E0) then
            do 630 J = 1, N1
               S = 0.0E0
               SI = 0.0E0
               do 610 K = 1, L
                  S = S + AR(I,K) * VR(K,J) - AI(I,K) * VI(K,J)
                  SI = SI + AR(I,K) * VI(K,J) + AI(I,K) * VR(K,J)
  610          continue
c        .......... double divisions avoid possible underflow ..........
               S = (S / H) / H
               SI = (SI / H) / H
c
               do 620 K = 1, L
                  VR(K,J) = VR(K,J) - S * AR(I,K) - SI * AI(I,K)
                  VI(K,J) = VI(K,J) - SI * AR(I,K) + S * AI(I,K)
  620          continue
  630       continue
         end if
  640 continue
c
      return
      end
