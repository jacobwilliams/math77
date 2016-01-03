      subroutine SEVVUN(A, LDA, N, EVALR, EVALI, VEC, IFLAG, WORK)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c     This file contains SEVVUN and CDIV.
c>> 1996-03-30 DEVUNN Krogh  MIN0 => MIN, added external statement.
c>> 1994-11-02 SEVVUN Krogh  Changes to use M77CON
c>> 1992-04-23 SEVVUN CLL   Declaring all variables.
c>> 1992-04-22 SEVVUN Krogh Removed unused labels 330 and 1220.
c>> 1992-03-05 SEVVUN Krogh Initial version, converted from EISPACK.
c
c     This subroutine uses slight modifcations of EISPACK routines
c     BALANC, ELMHES, ELTRAN, HQR2 and BALBAK to get the eigenvalues and
c     eigenvectors of a general real matrix by the QR method.  The first
c     two are are encapsulated in SEVBH.  The following three are given
c     inline here.
c
c     ELTRAN is a translation of the algol procedure ELMTRANS,
c     Num. Math. 16, 181-204(1970) by Peters and Wilkinson.
c     Handbook for Auto. Comp., Vol.ii-Linear Algebra, 372-395(1971).
c
c     HQR2 is a translation of the ALGOL procedure HQR2,
c     Num. Math. 16, 181-204(1970) by Peters and Wilkinson.
c     Handbook for Auto. Comp., Vol.ii-Linear Algebra, 372-395(1971).
c
c     BALBAK is a translation of the ALGOL procedure BALBAK,
c     Num. Math. 13, 293-304(1969) by Parlett and Reinsch.
c     Handbook for Auto. Comp., Vol.ii-Linear Algebra, 315-326(1971).
c
c     This subroutine finds the eigenvalues and eigenvectors
c     of a real general matrix by the QR method.
c
c     On input
c
c     LDA  must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c     N    is the order of the matrix.
c     A    contains the input matrix whose eigenvalues and eigenvectors
c          are desired.
c
c     On output
c     A    has been destroyed.
c     EVALR and EVALI contain real and imaginary parts, respectively, of
c          the eigenvalues.  The eigenvalues are given in order of
c          increasing real parts.  When real parts are equal they are
c          given in order of increasing absolute complex part.  Complex
c          conjugate pairs of values appear consecutively with
c          the eigenvalue having the positive imaginary part first.  If
c          an error exit is made, the eigenvalues should be correct
c          (but unordered) for indices IFLAG(1)+1,...,N.
c     VEC  contains the real and imaginary parts of the eigenvectors.
c          if the I-th eigenvalue is real, the I-th column of VEC
c          contains its eigenvector.  If the I-th eigenvalue is complex
c          with positive imaginary part, the I-th and (I+1)-th
c          columns of VEC contain the real and imaginary parts of its
c          eigenvector.  The eigenvectors are unnormalized.  If an
c          error exit is made, none of the eigenvectors has been found.
c     IFLAG(1) is set to
c          1   If all eigenvalues are real.
c          2   If some eigenvalues are complex.
c          3   If N < 1 on the initial entry.
c          4   If the limit of 30*N iterations is exhausted.
c     ------------------------------------------------------------------
c--S replaces "?": ?EVVUN, ?EVBH, ?NRM2, ?SCAL, ?CDIV
c     ------------------------------------------------------------------
      external SNRM2
      integer I,J,K,L,M,N,EN,NA,LDA,IGH,ITN,ITS,LOW,MP,MP2,ENM2
      integer II, IFLAG(N), LTYPE
      real             A(LDA,N),EVALR(N),EVALI(N),VEC(LDA,N), WORK(N)
      real             P,Q,R,S,T,W,X,Y,RA,SA,VI,VR,ZZ,NORM,TST1,TST2
      real             SNRM2
      logical NOTLAS
c     ------------------------------------------------------------------
c
      call SEVBH(A, LDA, N, LOW, IGH, IFLAG, WORK)
      LTYPE = 1
c
c    -------------------------------- ELTRAN ---------------------------
c
c     Accumulate the stabilized elementary similarity transformations
c     used in the preceding reduction of the matrix to upper Hessenberg
c     form.
c
c     .......... initialize VEC to identity matrix ..........
      do 20 J = 1, N
         do 10 I = 1, N
            VEC(I,J) = 0.0E0
   10    continue
         VEC(J,J) = 1.0E0
   20 continue
      if (IGH .gt. LOW+1) then
         do 50 MP = IGH-1, LOW+1, -1
            do 30 I = MP+1, IGH
               VEC(I,MP) = A(I,MP-1)
   30       continue
            I = IFLAG(MP)
            if (I .ne. MP) then
               do 40 J = MP, IGH
                  VEC(MP,J) = VEC(I,J)
                  VEC(I,J) = 0.0E0
   40          continue
               VEC(I,MP) = 1.0E0
            end if
   50    continue
      end if
c
c    -------------------------------- HQR2 -----------------------------
c
c     Find the eigenvalues of a real upper Hessenberg matrix by the QR
c     method.
c
      IFLAG(1) = 0
      NORM = 0.0E0
      K = 1
c     .......... store roots isolated by balanc
c                and compute matrix norm ..........
      do 70 I = 1, N
         do 60 J = K, N
            NORM = NORM + abs(A(I,J))
   60    continue
         K = I
         if ((I .lt. LOW) .or. (I .gt. IGH)) then
            EVALR(I) = A(I,I)
            EVALI(I) = 0.0E0
         end if
   70 continue
      EN = IGH
      T = 0.0E0
      ITN = 30*N
c     .......... search for next eigenvalues ..........
   80 if (EN .lt. LOW) go to 340
      ITS = 0
      NA = EN - 1
      ENM2 = NA - 1
c     .......... look for single small sub-diagonal element
   90 do 100 L = EN, LOW+1, -1
         S = abs(A(L-1,L-1)) + abs(A(L,L))
         if (S .eq. 0.0E0) S = NORM
         TST1 = S
         TST2 = TST1 + abs(A(L,L-1))
         if (TST2 .eq. TST1) go to 110
  100 continue
c     .......... form shift ..........
  110 X = A(EN,EN)
      if (L .eq. EN) go to 270
      Y = A(NA,NA)
      W = A(EN,NA) * A(NA,EN)
      if (L .eq. NA) go to 280
      if (ITN .le. 0) then
c     .......... set error -- all eigenvalues have not
c                converged after 30*N iterations ..........
         call ERMSG('SEVVUN', EN, 0,
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
      X = 0.75E0 * S
      Y = X
      W = -0.4375E0 * S * S
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
         A(I,I-2) = 0.0E0
         if (I .ne. MP2) A(I,I-3) = 0.0E0
  160 continue
c     .......... double QR step involving rows L to EN and
c                columns M to EN ..........
      do 260 K = M, NA
         NOTLAS = K .ne. NA
         if (K .ne. M) then
            P = A(K,K-1)
            Q = A(K+1,K-1)
            R = 0.0E0
            if (NOTLAS) R = A(K+2,K-1)
            X = abs(P) + abs(Q) + abs(R)
            if (X .eq. 0.0E0) go to 260
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
c        .......... accumulate transformations ..........
            do 220 I = LOW, IGH
               P = X * VEC(I,K) + Y * VEC(I,K+1) + ZZ * VEC(I,K+2)
               VEC(I,K) = VEC(I,K) - P
               VEC(I,K+1) = VEC(I,K+1) - P * Q
               VEC(I,K+2) = VEC(I,K+2) - P * R
  220       continue
         else
c        .......... row modification ..........
            do 230 J = K, N
               P = A(K,J) + Q * A(K+1,J)
               A(K,J) = A(K,J) - P * X
               A(K+1,J) = A(K+1,J) - P * Y
  230       continue
c
            J = min(EN,K+3)
c        .......... column modification ..........
            do 240 I = 1, J
               P = X * A(I,K) + Y * A(I,K+1)
               A(I,K) = A(I,K) - P
               A(I,K+1) = A(I,K+1) - P * Q
  240       continue
c        .......... accumulate transformations ..........
            do 250 I = LOW, IGH
               P = X * VEC(I,K) + Y * VEC(I,K+1)
               VEC(I,K) = VEC(I,K) - P
               VEC(I,K+1) = VEC(I,K+1) - P * Q
  250       continue
         end if
  260 continue
      go to 90
c     .......... one root found ..........
  270 A(EN,EN) = X + T
      EVALR(EN) = A(EN,EN)
      EVALI(EN) = 0.0E0
      EN = NA
      go to 80
c     .......... two roots found ..........
  280 P = (Y - X) / 2.0E0
      Q = P * P + W
      ZZ = sqrt(abs(Q))
      A(EN,EN) = X + T
      X = A(EN,EN)
      A(NA,NA) = Y + T
      if (Q .ge. 0.0E0) then
c        .......... real pair ..........
         ZZ = P + sign(ZZ,P)
         EVALR(NA) = X + ZZ
         EVALR(EN) = EVALR(NA)
         if (ZZ .ne. 0.0E0) EVALR(EN) = X - W / ZZ
         EVALI(NA) = 0.0E0
         EVALI(EN) = 0.0E0
         X = A(EN,NA)
         S = abs(X) + abs(ZZ)
         P = X / S
         Q = ZZ / S
         R = sqrt(P*P+Q*Q)
         P = P / R
         Q = Q / R
c        .......... row modification ..........
         do 290 J = NA, N
            ZZ = A(NA,J)
            A(NA,J) = Q * ZZ + P * A(EN,J)
            A(EN,J) = Q * A(EN,J) - P * ZZ
  290    continue
c        .......... column modification ..........
         do 300 I = 1, EN
            ZZ = A(I,NA)
            A(I,NA) = Q * ZZ + P * A(I,EN)
            A(I,EN) = Q * A(I,EN) - P * ZZ
  300    continue
c        .......... accumulate transformations ..........
         do 310 I = LOW, IGH
            ZZ = VEC(I,NA)
            VEC(I,NA) = Q * ZZ + P * VEC(I,EN)
            VEC(I,EN) = Q * VEC(I,EN) - P * ZZ
  310    continue
      else
c        .......... complex pair ..........
         LTYPE = 2
         EVALR(NA) = X + P
         EVALR(EN) = X + P
         EVALI(NA) = ZZ
         EVALI(EN) = -ZZ
      end if
      EN = ENM2
      go to 80
c     .......... all roots found.  Backsubstitute to find
c                vectors of upper triangular form ..........
  340 if (NORM .eq. 0.0E0) go to 1000
      do 800 EN = N, 1, -1
         P = EVALR(EN)
         Q = EVALI(EN)
         NA = EN - 1
         if (Q) 710, 600, 800
c     .......... real vector ..........
  600    M = EN
         A(EN,EN) = 1.0E0
         do 700 I = NA, 1, -1
            W = A(I,I) - P
            R = 0.0E0
            do 610 J = M, EN
               R = R + A(I,J) * A(J,EN)
  610       continue
            if (EVALI(I) .lt. 0.0E0) then
               ZZ = W
               S = R
            else
               M = I
               if (EVALI(I) .eq. 0.0E0) then
                  T = W
                  if (T .eq. 0.0E0) then
                     TST1 = NORM
                     T = TST1
  640                T = 0.01E0 * T
                     TST2 = NORM + T
                     if (TST2 .gt. TST1) go to 640
                  end if
                  A(I,EN) = -R / T
               else
c           .......... solve real equations ..........
                  X = A(I,I+1)
                  Y = A(I+1,I)
                  Q = (EVALR(I)-P) * (EVALR(I)-P) + EVALI(I) * EVALI(I)
                  T = (X * S - ZZ * R) / Q
                  A(I,EN) = T
                  if (abs(X) .gt. abs(ZZ)) then
                     A(I+1,EN) = (-R - W * T) / X
                  else
                     A(I+1,EN) = (-S - Y * T) / ZZ
                  end if
               end if
c           .......... overflow control ..........
               T = abs(A(I,EN))
               if (T .ne. 0.0E0) then
                  TST1 = T
                  TST2 = TST1 + 1.0E0/TST1
                  if (TST2 .le. TST1) then
                     do 690 J = I, EN
                        A(J,EN) = A(J,EN)/T
  690                continue
                  end if
               end if
            end if
  700    continue
c     .......... end real vector ..........
         go to 800
c
c     .......... complex vector ..........
  710    M = NA
c     .......... last vector component chosen imaginary so that
c                eigenvector matrix is triangular ..........
         if (abs(A(EN,NA)) .gt. abs(A(NA,EN))) then
            A(NA,NA) = Q / A(EN,NA)
            A(NA,EN) = -(A(EN,EN) - P) / A(EN,NA)
         else
            call SCDIV(0.0E0,-A(NA,EN),A(NA,NA)-P,Q,A(NA,NA),A(NA,EN))
         end if
         A(EN,NA) = 0.0E0
         A(EN,EN) = 1.0E0
         ENM2 = NA - 1
         do 760 I = ENM2, 1, -1
            W = A(I,I) - P
            RA = 0.0E0
            SA = 0.0E0
            do 730 J = M, EN
               RA = RA + A(I,J) * A(J,NA)
               SA = SA + A(I,J) * A(J,EN)
  730       continue
            if (EVALI(I) .lt. 0.0E0) then
               ZZ = W
               R = RA
               S = SA
            else
               M = I
               if (EVALI(I) .eq. 0.0E0) then
                  call SCDIV(-RA,-SA,W,Q,A(I,NA),A(I,EN))
               else
c           .......... solve complex equations ..........
                  X = A(I,I+1)
                  Y = A(I+1,I)
                  VR = (EVALR(I)-P)*(EVALR(I)-P)+EVALI(I)*EVALI(I)-Q*Q
                  VI = (EVALR(I) - P) * 2.0E0 * Q
                  if (VR .eq. 0.0E0 .and. VI .eq. 0.0E0) then
                     TST1 = NORM * (abs(W)+abs(Q)+abs(X)+abs(Y)+abs(ZZ))
                     VR = TST1
  740                VR = 0.01E0 * VR
                     TST2 = TST1 + VR
                     if (TST2 .gt. TST1) go to 740
                  end if
                  call SCDIV(X*R-ZZ*RA+Q*SA,X*S-ZZ*SA-Q*RA,VR,VI,
     x                   A(I,NA),A(I,EN))
                  if (abs(X) .gt. abs(ZZ) + abs(Q)) then
                     A(I+1,NA) = (-RA - W * A(I,NA) + Q * A(I,EN)) / X
                     A(I+1,EN) = (-SA - W * A(I,EN) - Q * A(I,NA)) / X
                  else
                     call SCDIV(-R-Y*A(I,NA),-S-Y*A(I,EN),ZZ,Q,
     x                   A(I+1,NA),A(I+1,EN))
                  end if
               end if
c           .......... overflow control ..........
               T = max(abs(A(I,NA)), abs(A(I,EN)))
               if (T .ne. 0.0E0) then
                  TST1 = T
                  TST2 = TST1 + 1.0E0/TST1
                  if (TST2 .le. TST1) then
                     do 750 J = I, EN
                        A(J,NA) = A(J,NA)/T
                        A(J,EN) = A(J,EN)/T
  750                continue
                  end if
               end if
            end if
  760    continue
c     .......... end complex vector ..........
  800 continue
c     .......... end back substitution.
c                vectors of isolated roots ..........
      do 840 I = 1, N
         if (I .ge. LOW .and. I .le. IGH) go to 840
c
         do 820 J = I, N
            VEC(I,J) = A(I,J)
  820    continue
c
  840 continue
c     .......... multiply by transformation matrix to give
c                vectors of original full matrix.
      do 880 J = N, LOW, -1
         M = min(J,IGH)
c
         do 880 I = LOW, IGH
            ZZ = 0.0E0
c
            do 860 K = LOW, M
               ZZ = ZZ + VEC(I,K) * A(K,J)
  860       continue
c
            VEC(I,J) = ZZ
  880 continue
c
 1000 continue
c
c    ------------------------------ BALBAK -----------------------------
c
c     Form eigenvectors of a real general matrix by back transforming
c     those of the corresponding balanced matrix determined by BALANC.
c
      if (IGH .ne. LOW) then
         do 1110 I = LOW, IGH
            S = WORK(I)
c        .......... left hand eigenvectors are back transformed
c                   if the foregoing statement is replaced by
c                   S=1.0E0/WORK(I). ..........
            do 1100 J = 1, N
               VEC(I,J) = VEC(I,J) * S
 1100       continue
 1110    continue
      end if
c     ......... for I=LOW-1 step -1 until 1,
c               IGH+1 step 1 until N do -- ..........
      do 1140 II = 1, N
         I = II
         if ((I .lt. LOW) .or. (I .gt. IGH)) then
            if (I .lt. LOW) I = LOW - II
            K = WORK(I)
            if (K .ne. I) then
               do 1130 J = 1, N
                  S = VEC(I,J)
                  VEC(I,J) = VEC(K,J)
                  VEC(K,J) = S
 1130          continue
            end if
         end if
 1140 continue
c                        Normalize the eigenvectors
      do 1220 I = 1, N
         P = SNRM2(N, VEC(1, I), 1)
         if (EVALI(I) .eq. 0.E0) then
            call SSCAL(N, sign(1.E0, VEC(1,I)) / P, VEC(1,I), 1)
         else if (EVALI(I) .gt. 0.E0) then
            Q = SNRM2(N, VEC(1, I+1), 1)
            if (P .gt. Q) then
               P = P * sqrt(1.E0 + (Q/P)**2)
            else if (Q .ne. 0.E0) then
               P = Q * sqrt(1.E0 + (P/Q)**2)
            else
               go to 1220
            end if
            if (VEC(1, I+1) .eq. 0.E0) then
               P = sign(1.E0, VEC(1,I+1)) / P
               Q = 0.E0
            else if (abs(VEC(1, I)) .gt. abs(VEC(1, I+1))) then
               P = 1.E0 / (P*sqrt(1.E0+(VEC(1,I+1)/VEC(1,I))**2))
               Q = P * (VEC(1,I+1) / abs(VEC(1,I)))
               P = sign(P, VEC(1, I))
            else
               Q = 1.E0 / (P*sqrt(1.E0+(VEC(1,I)/VEC(1,I+1))**2))
               P = Q * (VEC(1,I) / abs(VEC(1,I+1)))
               Q = sign(Q, VEC(1, I+1))
            end if
            do 1200 J = 1, N
               R = P * VEC(J, I) + Q * VEC(J, I+1)
               VEC(J, I+1) = P * VEC(J, I+1) - Q * VEC(J, I)
               VEC(J, I) = R
 1200       continue
         end if
 1220 continue
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
            do 2115 J = 1, N
               WORK(J) = VEC(J, I)
 2115       continue
 2120       K = IFLAG(M)
            IFLAG(M) = M
            if (K .ne. L) then
               EVALR(M) = EVALR(K)
               EVALI(M) = EVALI(K)
               do 2130 J = 1, N
                  VEC(J, M) = VEC(J, K)
 2130          continue
               M = K
               go to 2120
            else
               EVALR(M) = P
               EVALI(M) = Q
               do 2140 J = 1, N
                  VEC(J, M) = WORK(J)
 2140          continue
               go to 2110
            end if
         end if
 2150 continue
      IFLAG(1) = LTYPE
      return
      end
c     ==================================================================
      subroutine SCDIV(AR,AI,BR,BI,CR,CI)
c
c     complex division, (CR,CI) = (AR,AI)/(BR,BI)
c     ------------------------------------------------------------------
      real             AR,AI,BR,BI,CR,CI
      real             S,ARS,AIS,BRS,BIS
      S = abs(BR) + abs(BI)
      ARS = AR/S
      AIS = AI/S
      BRS = BR/S
      BIS = BI/S
      S = BRS**2 + BIS**2
      CR = (ARS*BRS + AIS*BIS)/S
      CI = (AIS*BRS - ARS*BIS)/S
      return
      end
