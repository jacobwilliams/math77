      subroutine DEVUN(A, LDA, N, EVALR, EVALI, IFLAG)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 DEVUN  Krogh  MIN0 => MIN
c>> 1994-10-20 DEVUN  Krogh  Changes to use M77CON
c>> 1992-04-24 DEVUN  CLL   Minor edits.
c>> 1992-04-23 DEVUN  Krogh Made DP version compatible with SP version.
c>> 1991-10-25 DEVUN  Krogh Initial version, converted from EISPACK.
c
c     This subroutine uses slight modifcations of EISPACK routines
c     BALANC, ELMHES, and HQR to get the eigenvalues of a general real
c     matrix by the QR method.  The first two are are encapsulated in
c     DEVBH.  HQR, which is inline here, is a translation of the ALGOL
c     procedure HQR, Num. Math. 14, 219-231(1970) by Martin, Peters, and
c     Wilkinson.
c     Handbook for Auto. Comp., Vol.ii-Linear Algebra, 359-371(1971).
c
c     On input
c     A      contains the input matrix whose eigenvalues are desired.
c     LDA    must be set to the row dimension of two-dimensional array
c            parameters as declared in the calling program dimension
c            statement.
c     N      is the order of the matrix.
c
c     On output
c     A      has been destroyed.
c     EVALR and EVALI contain real and imaginary parts, respectively, of
c          the eigenvalues.  The eigenvalues are given in order of
c          increasing real parts.  When real parts are equal they are
c          given in order of increasing absolute complex part.  Complex
c          conjugate pairs of values appear consecutively with
c          the eigenvalue having the positive imaginary part first.  If
c          an error exit is made, the eigenvalues should be correct
c          (but unordered) for indices IFLAG(1)+1,...,N.
c     IFLAG(1) is set to
c          1   If all eigenvalues are real.
c          2   If some eigenvalues are complex.
c          3   If N < 1 on the initial entry.
c          4   If the limit of 30*N iterations is exhausted.
c     ------------------------------------------------------------------
c--D replaces "?": ?EVUN, ?EVBH
c     ------------------------------------------------------------------
      integer I,J,K,L,M,N,EN,NA,LDA,IGH,ITN,ITS,LOW,MP2,ENM2,IFLAG(N)
      integer LTYPE
      double precision A(LDA,N),EVALR(N),EVALI(N)
      double precision P,Q,R,S,T,W,X,Y,ZZ,NORM,TST1,TST2
      logical NOTLAS
c
c     ------------------------------------------------------------------
c
      call DEVBH(A, LDA, N, LOW, IGH, IFLAG, EVALR)
      LTYPE = 1
      NORM = 0.0D0
      K = 1
c     .......... store roots isolated by DEVBH
c                and compute matrix norm ..........
      do 50 I = 1, N
         do 40 J = K, N
            NORM = NORM + abs(A(I,J))
   40    continue
         K = I
         if ((I .lt. LOW) .or. (I .gt. IGH)) then
            EVALR(I) = A(I,I)
            EVALI(I) = 0.0D0
         end if
   50 continue
      EN = IGH
      T = 0.0D0
      ITN = 30*N
c     .......... search for next eigenvalues ..........
   60 if (EN .lt. LOW) go to 300
      ITS = 0
      NA = EN - 1
      ENM2 = NA - 1
c     .......... look for single small sub-diagonal element
   70 do 80 L = EN, LOW+1, -1
         S = abs(A(L-1,L-1)) + abs(A(L,L))
         if (S .eq. 0.0D0) S = NORM
         TST1 = S
         TST2 = TST1 + abs(A(L,L-1))
         if (TST2 .eq. TST1) go to 100
   80 continue
c     .......... form shift ..........
  100 X = A(EN,EN)
      if (L .eq. EN) go to 270
      Y = A(NA,NA)
      W = A(EN,NA) * A(NA,EN)
      if (L .eq. NA) go to 280
      if (ITN .le. 0) then
c     .......... set error -- all eigenvalues have not
c                converged after 30*N iterations ..........
         call ERMSG('DEVUN', EN, 0,
     1'ERROR NO. is index of eigenvalue causing convergence failure.',
     2            '.')
         IFLAG(1) = 4
         if (EN .le. 0) IFLAG(1) = 3
         return
      end if
      if (ITS .ne. 10 .and. ITS .ne. 20) go to 130
c     .......... form exceptional shift ..........
      T = T + X
      do 120 I = LOW, EN
         A(I,I) = A(I,I) - X
  120 continue
      S = abs(A(EN,NA)) + abs(A(NA,ENM2))
      X = 0.75D0 * S
      Y = X
      W = -0.4375D0 * S * S
  130 ITS = ITS + 1
      ITN = ITN - 1
c     .......... look for two consecutive small sub-diagonal elements.
      do 140 M = ENM2, L, -1
         ZZ = A(M,M)
         R = X - ZZ
         S = Y - ZZ
         P = (R * S - W) / A(M+1,M) + A(M,M+1)
         Q = A(M+1,M+1) - ZZ - R - S
         R = A(M+2,M+1)
         S = abs(P) + abs(Q) + abs(R)
         P = P / S
         Q = Q / S
         R = R / S
         if (M .eq. L) go to 150
         TST1 = abs(P)*(abs(A(M-1,M-1)) + abs(ZZ) + abs(A(M+1,M+1)))
         TST2 = TST1 + abs(A(M,M-1))*(abs(Q) + abs(R))
         if (TST2 .eq. TST1) go to 150
  140 continue
  150 MP2 = M + 2
      do 160 I = MP2, EN
         A(I,I-2) = 0.0D0
         if (I .ne. MP2) A(I,I-3) = 0.0D0
  160 continue
c     .......... double QR step involving rows L to EN and
c                columns M to EN ..........
      do 260 K = M, NA
         NOTLAS = K .ne. NA
         if (K .ne. M) then
            P = A(K,K-1)
            Q = A(K+1,K-1)
            R = 0.0D0
            if (NOTLAS) R = A(K+2,K-1)
            X = abs(P) + abs(Q) + abs(R)
            if (X .eq. 0.0D0) go to 260
            P = P / X
            Q = Q / X
            R = R / X
         end if
         S = sign(sqrt(P*P+Q*Q+R*R),P)
         if (K .ne. M) then
            A(K,K-1) = -S * X
         else
            if (L .ne. M) A(K,K-1) = -A(K,K-1)
         end if
         P = P + S
         X = P / S
         Y = Q / S
         ZZ = R / S
         Q = Q / P
         R = R / P
         if (NOTLAS) then
c        .......... row modification ..........
            do 200 J = K, N
               P = A(K,J) + Q * A(K+1,J) + R * A(K+2,J)
               A(K,J) = A(K,J) - P * X
               A(K+1,J) = A(K+1,J) - P * Y
               A(K+2,J) = A(K+2,J) - P * ZZ
  200       continue
            J = min(EN,K+3)
c        .......... column modification ..........
            do 210 I = 1, J
               P = X * A(I,K) + Y * A(I,K+1) + ZZ * A(I,K+2)
               A(I,K) = A(I,K) - P
               A(I,K+1) = A(I,K+1) - P * Q
               A(I,K+2) = A(I,K+2) - P * R
  210       continue
         else
c     .......... row modification ..........
            do 230 J = K, N
               P = A(K,J) + Q * A(K+1,J)
               A(K,J) = A(K,J) - P * X
               A(K+1,J) = A(K+1,J) - P * Y
  230       continue
            J = min(EN,K+3)
c        .......... column modification ..........
            do 240 I = 1, J
               P = X * A(I,K) + Y * A(I,K+1)
               A(I,K) = A(I,K) - P
               A(I,K+1) = A(I,K+1) - P * Q
  240       continue
         end if
  260 continue
      go to 70
c     .......... one root found ..........
  270 EVALR(EN) = X + T
      EVALI(EN) = 0.0D0
      EN = NA
      go to 60
c     .......... two roots found ..........
  280 P = (Y - X) / 2.0D0
      Q = P * P + W
      ZZ = sqrt(abs(Q))
      X = X + T
      if (Q .ge. 0.0D0) then
c        .......... real pair ..........
         ZZ = P + sign(ZZ,P)
         EVALR(NA) = X + ZZ
         EVALR(EN) = EVALR(NA)
         if (ZZ .ne. 0.0D0) EVALR(EN) = X - W / ZZ
         EVALI(NA) = 0.0D0
         EVALI(EN) = 0.0D0
      else
c        .......... complex pair ..........
         LTYPE = 2
         EVALR(NA) = X + P
         EVALR(EN) = X + P
         EVALI(NA) = ZZ
         EVALI(EN) = -ZZ
      end if
      EN = ENM2
      go to 60
c
  300 continue
c-- Begin mask code changes
c                        Set up for Shell sort
c             Sort so real parts are algebraically increasing
c             For = real parts, so abs(imag. parts) are increasing
c             For both =, sort on index -- preserves complex pair order
c-- End mask code changes
      do 2000 I = 1, N
         IFLAG(I) = I
 2000 continue
      L = 1
      do 2010 K = 1, N
         L = 3*L + 1
         if (L .ge. N) go to 2020
 2010 continue
 2020 L = max(1, (L-4) / 9)
 2030 do 2100 J = L+1, N
         K = IFLAG(J)
         P = EVALR(K)
         I = J - L
 2040    if (P - EVALR(IFLAG(I))) 2070, 2050, 2080
 2050    if (abs(EVALI(K)) - abs(EVALI(IFLAG(I)))) 2070, 2060, 2080
 2060    if (K .gt. IFLAG(I)) go to 2080
 2070    IFLAG(I+L) = IFLAG(I)
         I = I - L
         if (I .gt. 0) go to 2040
 2080    IFLAG(I+L) = K
 2100 continue
      L = (L-1) / 3
      if (L .ne. 0) go to 2030
c              Indices in IFLAG now give the desired order --
c              Move entries to get this order.
 2110 do 2150 I = L+1, N
         if (IFLAG(I) .ne. I) then
            L = I
            M = I
            P = EVALR(I)
            Q = EVALI(I)
 2120       K = IFLAG(M)
            IFLAG(M) = M
            if (K .ne. L) then
               EVALR(M) = EVALR(K)
               EVALI(M) = EVALI(K)
               M = K
               go to 2120
            else
               EVALR(M) = P
               EVALI(M) = Q
               go to 2110
            end if
         end if
 2150 continue
      IFLAG(1) = LTYPE
      return
      end
