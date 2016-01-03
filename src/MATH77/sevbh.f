      subroutine SEVBH(A, LDA, N, LOW, IGH, IFLAG, SCALE)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2001-05-25 SEVBH  Krogh Minor change for making .f90 version.
c>> 1996-03-30 SEVBH  Krogh  Added external statement.
c>> 1994-10-20 SEVBH  Krogh  Changes to use M77CON
c>> 1992-04-23 SEVBH  CLL  Declaring all variables.
c>> 1992-04-22 SEVBH  Krogh Removed unused label 80.
c>> 1991-10-25 SEVBH  Krogh Initial version, converted from EISPACK.
c
c     This subroutine is a slight modification of EISPACK subroutines
c     BALANC and ELMHES (dated August 1983).  These routines in turn are
c     translations of the ALGOL procedures BALANCE, Num. Math. 13,
c     293-304(1969) by Parlett and Reinsch. Handbook for Auto. Comp.,
c     Vol.ii-Linear Algebra, 315-326(1971), and
c     ELMHES, Num. Math. 12, 349-368(1968) by Martin and Wilkinson.
c     Handbook for Auto. Comp., Vol.ii-Linear Algebra, 339-358(1971).
c     BALANC balances a real matrix & isolates eigenvalues if possible.
c     ELMHES reduces a submatrix situated in rows and columns LOW
c     through IGH of a real general matrix to upper Hessenberg form by
c     stabilized elementary similarity transformations.
c     ------------------------------------------------------------------
c--S replaces "?": ?EVBH
c     Both versions use I1MACH
c     ------------------------------------------------------------------
      external I1MACH
      integer I1MACH
      integer I,J,K,L,M,N,LDA,IGH,LOW,IEXC,IFLAG(N)
      integer LA,KP1,MM1
      real             A(LDA,N),SCALE(N)
      real             C,F,G,R,S,B2,BASE
      real             X,Y
      logical NOCONV
c
c     On input
c     A      contains the input matrix to be balanced and reduced to
c            upper Hessenburg form.
c     LDA    must be set to the row dimension of two-dimensional array
c            parameters as declared in the calling program dimension
c            statement.
c     N      is the order of the matrix.
c
c     On output
c     A     contains the balanced Hessenburg matrix.  The multipliers
c           which were used in the reduction are stored in the remaining
c           triangle under the Hessenberg matrix.
c     LOW   and IGH are two integers such that A(i,j)
c           is equal to zero if
c             (1) i is greater than j and
c             (2) j=1,...,LOW-1 or i=IGH+1,...,N.
c     SCALE contains information determining the permutations and
c           scaling factors used.  Suppose that the principal submatrix
c           in rows LOW through IGH has been balanced, that p(j) denotes
c           the index interchanged with j during the permutation step,
c           and that the elements of the diagonal matrix used are
c           denoted by d(i,j).  Then
c        SCALE(j) = p(j),    for j = 1,...,LOW-1
c                 = d(j,j),      j = LOW,...,IGH
c                 = p(j)         j = IGH+1,...,N.
c           The order in which the interchanges are made is N to IGH+1,
c           then 1 to LOW-1.
c           Note that 1 is returned for IGH if IGH is zero formally.
c     IFLAG contains information on the rows and columns interchanged in
c           the reduction.  Only elements LOW through IGH are used.
c
c     The ALGOL procedure exc contained in balance appears in
c     BALANC inline.  (Note that the ALGOL roles of identifiers
c     K,L have been reversed.)
c
c     --------------------------- BALANC -------------------------------
c
      BASE = I1MACH(10)
      B2 = BASE * BASE
      K = 1
      L = N
      go to 100
c     .......... in-line procedure for row and column exchange .........
   20 SCALE(M) = J
      if (J .ne. M) then
         do 30 I = 1, L
            F = A(I,J)
            A(I,J) = A(I,M)
            A(I,M) = F
   30    continue
         do 40 I = K, N
            F = A(J,I)
            A(J,I) = A(M,I)
            A(M,I) = F
   40    continue
      end if
      if (IEXC .ne. 0) go to 130
c     .......... search for rows isolating an eigenvalue
c                and push them down ..........
      if (L .le. 1) go to 280
      L = L - 1
  100 do 120 J = L, 1, -1
         do 110 I = 1, L
            if (I .eq. J) go to 110
            if (A(J,I) .ne. 0.0E0) go to 120
  110    continue
         M = L
         IEXC = 0
         go to 20
  120 continue
      go to 140
c     .......... search for columns isolating an eigenvalue
c                and push them left ..........
  130 K = K + 1
c
  140 do 170 J = K, L
         do 150 I = K, L
            if (I .eq. J) go to 150
            if (A(I,J) .ne. 0.0E0) go to 170
  150    continue
         M = K
         IEXC = 1
         go to 20
  170 continue
c     .......... now balance the submatrix in rows K to L ..........
      do 180 I = K, L
  180 SCALE(I) = 1.0E0
c     .......... iterative loop for norm reduction ..........
  190 NOCONV = .false.
      do 270 I = K, L
         C = 0.0E0
         R = 0.0E0
         do 200 J = K, L
            if (J .ne. I) then
               C = C + abs(A(J,I))
               R = R + abs(A(I,J))
            end if
  200    continue
c     .......... guard against zero C or R due to underflow ..........
         if ((C .ne. 0.0E0) .and. (R .ne. 0.0E0)) then
            G = R / BASE
            F = 1.0E0
            S = C + R
  210       if (C .lt. G) then
               F = F * BASE
               C = C * B2
               go to 210
            end if
            G = R * BASE
  230       if (C .ge. G) then
               F = F / BASE
               C = C / B2
               go to 230
            end if
c        .......... now balance ..........
            if ((C + R) / F .lt. 0.95E0 * S) then
               G = 1.0E0 / F
               SCALE(I) = SCALE(I) * F
               NOCONV = .true.
               do 250 J = K, N
                  A(I,J) = A(I,J) * G
  250          continue
               do 260 J = 1, L
                  A(J,I) = A(J,I) * F
  260          continue
            end if
         end if
  270 continue
      if (NOCONV) go to 190
  280 LOW = K
      IGH = L
c
c     --------------------------- ELMHES -------------------------------
c
      LA = IGH - 1
      KP1 = LOW + 1
      if (LA .lt. KP1) return
      do 380 M = KP1, LA
         MM1 = M - 1
         X = 0.0E0
         I = M
         do 300 J = M, IGH
            if (abs(A(J,MM1)) .le. abs(X)) go to 300
            X = A(J,MM1)
            I = J
  300    continue
         IFLAG(M) = I
         if (I .eq. M) go to 330
c     .......... interchange rows and columns of A ..........
         do 310 J = MM1, N
            Y = A(I,J)
            A(I,J) = A(M,J)
            A(M,J) = Y
  310    continue
         do 320 J = 1, IGH
            Y = A(J,I)
            A(J,I) = A(J,M)
            A(J,M) = Y
  320    continue
c     .......... end interchange ..........
  330    if (X .eq. 0.0E0) go to 380
         do 360 I = M + 1, IGH
            Y = A(I,MM1)
            if (Y .eq. 0.0E0) go to 360
            Y = Y / X
            A(I,MM1) = Y
            do 340 J = M, N
               A(I,J) = A(I,J) - Y * A(M,J)
  340       continue
            do 350 J = 1, IGH
               A(J,M) = A(J,M) + Y * A(J,I)
  350       continue
  360    continue
  380 continue
      return
      end
