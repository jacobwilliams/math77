      subroutine sspge(N, ISPEC, IA, A, B, OPT)
c Time-stamp: <2015-07-08 15:34:13 m>
c Copyright (c) 2006, Math a la Carte, Inc.
 
c Solve the system of linear equations, A X = B, return X in B.
c>> 2015-07-08 SSPGE Krogh -- Probably this code is worthless      
c>> 2006-03-12 SSPGE    Krogh Initial code
 
c--S replaces "?": ?SPGE, ?MESS
 
c
c ************************* Arguments **********************************
 
c A      The input matrix and associated working store.
c IA     Array for holding data defining the sparity structure and also
c   used for working storage.  IA(1) contains the number of nonzeros in
c   column 1, the next IA locations contain the row indexes for this
c   column in increasing order. This is followed by the number of
c   nonzeros in column 2, etc. Additional working space is needed to
c   hold data as we factor the matrix.  The data in A is stored at the
c   corresponding locations in A, with anything being stored in A where
c   the column counts are stored in IA.
c ISPEC  Used as follows.
c ISPEC(1) Must be 0 if the matrix is not factored and the value
c   returned on the first call must be preserved on later calls when
c   the matrix already factored.  If the returned value is < 0,
c   then the matrix was not factored and the value is the negative of
c   the values mentioned in Error message indexes below.
c ISPEC(2) Declared dimension of IA() (or at least all that you want
c   this routine to know about).  Typically this should be the same as
c   ISPEC(3) + 4N.
c ISPEC(3) [in] Declared dimension of A(). This number should be the
c   space required for the original matrix + the maximal space expected
c   for the same type of representation of the factored matrix + 2N.
c ISPEC(4) The lesser of the amount of space in A or IA that was not
c   needed.
c ISPEC(5) Start of options, <For now this must contain 0>
c OPT    Used for return of optional values.
c   OPT(1) The reciprocal of the condition number if requested.
c   OPT(2) If requested the determinant is OPT(2) * 10 ** OPT(3)
c   OPT(3) The exponent for the determinant.  (0 if the determinant is
c          not to big or too small.
c Error message indexes.
c   1   Matrix appears singular.
c   2   Matrix has an empty column.
c   3   Matrix has an empty row.
c   4   No more space.
c   5   N .le. 0
c   6   An unknown option
c   7   Problem with column size
c   8   Problem with row indexes
c   9   Bug -- Can't find an unused column
c  10   Bug -- Pivot row not found where expected
c
c ************************ Other Variables *****************************
c
c COND   Usually .false., set =.true. if condition estimate wanted.
c DET    Usually .false., set=.true. if determinant is wanted.
c I      Temporary index
c IB     Base address for the current column in B.
c IC     Temporary index of a column.
c ICP    Initial index of current pivot column.
c IL     Temporary index.
c INCB   Increment between columns of B.
c IP     Physical row index.
c IPIVOT Logical index of the pivot column.
c IR     Temporary index of a row.
c J      Temporary index
c K      Temporary index.
c KK     Temporary index.
c KB     Index of best choice for a pivot and counter on columns in B.
c L      Temporary index.
c LASTR  Index of last logical row used for selecing the next column.
c LC     Location of a column.
c LCFW  Base address in IA mapping logical index of a column to its
c    factored location.  Corresponding space in A is used to accumulate
c    transformations into the next pivot column.
c LCLISC  Base address in IA mapping logical index of a column to its
c    initial index.  Values here are set negative if this column has
c    been used to adjust row counts, and is set to 0 if the column
c    is used as a pivot column.  Also used as the base address for
c    the column scaling.
c LCOLI  Base address in IA mapping intial index of a column to its
c    location.
c LCP   Location of original column data for the pivot column.
c LCOLSZ Base address for adjusted size of coluumns.  These are stored
c    in initial column order.
c LFREE  Address of the last available free space in IA.
c LNEXT  Base location where next factored data is saved.
c LR     Location of a row.
c LROWL  Base address for pointers to the start of list of initial
c    columns for each row.  These are stored in initial row order.
c    The pointers point to the number of nonzero columns in the row
c    followed by the list of column entries.
c LROWLI Base address for mapping logical row indexes to initial
c    indexes.
c LRSWSC Base address in IA for tracking row swaps.  Also used for the
c    base address of the row scaling.  The scales are stored in iniitial
c    row order.
c LROWSZ Base address for adjusted size of rows.  These are stored
c    in logical row order.
c MROWSZ The smallest adjusted row size.
c NB     Number of columns in B.
c NI     Internal value of N.
c NP     Index of a pivot in U.
c PVAL   Value in pivot location (if no swap) when factoring.  Also
c    used for reciprocal of the L1 condition number of the orig. matrix.
c TP,TP1,TP2,TP3 Temporary variables..
c TRAN   Usuall .false., set = .true. if solving U^T x = b.
 
c
c Notes on permutation:
c   When permuting the mapping from logical to iniital indexes, we need
c   to know the two logical indexes and then we swap the contents of
c   IA at these two logical indexes.
c   When permuting the mapping from initial to logical indexes, we need
c   to know the two intial indexes and then we swap the contents of
c   IA at these tow initial indexes.
c   It is frequently the case that we need one mapping to get the
c   indexes for permuting the other.
c  Given the indices to swap, say IL1, IL2, and LI1, and LI2, where
c  the IL indexes are initial indexes which we map to logical indexes,
c  and the LI indexes are logical indexes whichg map to initial indexes.
c  Then   New IL1 mapping is LI2, New IL2 mapping is LI1, and
c         New LI1 mapping is IL2, New LI2 mapping is IL1.
c   Once IL1, IL2, LI1, LI2 have been determined new assignments are
c      [IL1] = LI2  [IL2] = LI1  [LI1] = IL2  [LI2= IL1
c   where [IL1] means location IL1 in the permutation mapping initial
c   indexes to logical, etc.
c
c Notes on storage in IA and A.
c                                                  First Location Used
c   Contents  (IA / A )                               IA           A
c   Input matrix                                       1           1
c   Column: Logical => Initial Perm/Scaling         LCLISC+1    LCLISC+1
c   Row swap history/Row Scaling                    LRSWSC+1    LRSWSC+1
c   Map logical col => factored loc. / Various      LCFW+1      LCFW+1
c
c   Place to store rest of columns of factored part LNEXT+1     LNEXT+1
c   End of space available for factored part.       LFREE       LFREE
c   Data below only uses space while factoring the matrix.
c   Map initial row index to data for that row.     LROWL+1
c   Adjusted row sizes                              LROWSZ+1
c   Initial size of columns                         LCOLSZ+1
c   Pointer to columns of the original matrix       LCOLI+1
c   Map logical to initial row indexes              LROWLI+1
c
 
c A factored column has the form
c   s The space used by the column. (location LNEXT+1 or IA(LCFW+J))
c   p The location of the pivot is p. (location LNEXT+2 or IA(LCFW+J+1))
c   p - 2 locations containing a column of U.  (locations LNEXT+3 to p)
c   The reciprocal of the pivot. (location p)
c   s - (p-LNEXT) - 1 locations for the subdiagonal entries of the
c           L factors (locations p+1 to LNEXT+s-1)
c Except for the first two entries, IA contains the logical row indexes
c for U and L, and A contains the corresponding values.
c
c ************************** Declarations ******************************
      integer N, ISPEC(*), IA(*)
      real             A(*), B(*), OPT(3)
 
      integer I, IB, ICP, IL, INCB, IP, IPIVOT, IR, J, K, KB,
     1  KK, L, LASTR, LC, LCFW, LCOLI, LCLISC, LCOLSZ, LCP,
     2  LFREE, LNEXT, LR, LROWL, LROWLI, LRSWSC, LROWSZ,
     3  M, MROWSZ, NB,  NI, NP
      real              PVAL, TP, TP1, TP2, TP3
      logical COND, DET, TRANS
 
      integer IPBDIM, IPLAST
      parameter (IPBDIM=2, IPLAST=5)
 
      integer MECONT, MEEMES, MENTXT, MERET,  METEXT
      parameter (MENTXT=23, MECONT=50, MERET=51, MEEMES=52, METEXT=53)
 
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA SSPGE$B
cAB N = $I, IA dim. = $I, A dim. = $I, Options: $E
cAC With $I columns yet to factor, our best pivot is $F$N
c     The matrix appears singular.$E
cAD Column $I is all 0, the matrix is singular.$E
cAE Row $I is all 0, the matrix is singular.$E
cAF With $I columns yet to factor we have run out of space.$E
cAG The dimension provided is .le. 0$E
cAH The last option index you provided is not supported.$E
cAI The column with index $I has $I entries?$E
cAJ In column $I, row index $I follows row index $I?$N
c   Row indexes must be increasing and in the inteval [1,N].$E
cAK A bug in SSPGE -- Could not find unused column.$E
cAL A bug in SSPGE -- Pivot row not found where expected.$E
c   $
cAM $I $I $I;  $B
c   $
cAN $I.$N$B
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM,LTXTAN
      parameter (LTXTAA=  1,LTXTAB=  8,LTXTAC= 54,LTXTAD=137,LTXTAE=182,
     * LTXTAF=225,LTXTAG=282,LTXTAH=316,LTXTAI=370,LTXTAJ=413,
     * LTXTAK=521,LTXTAL=570,LTXTAM=  1,LTXTAN=  1)
      character MTXTAA(3) * (209)
      character MTXTAB(1) * (13)
      character MTXTAC(1) * (7)
      data MTXTAA/'SSPGE$BN = $I, IA dim. = $I, A dim. = $I, Options: $E
     *With $I columns yet to factor, our best pivot is $F$NThe matrix ap
     *pears singular.$EColumn $I is all 0, the matrix is singular.$ERow$
     * $I is all 0, the matrix',' is singular.$EWith $I columns yet to f
     *actor we have run out of space.$EThe dimension provided is .le. 0$
     *EThe last option index you provided is not supported.$EThe column$
     * with index $I has $I entries?$EIn col','umn $I, row index $I foll
     *ows row index $I?$NRow indexes must be increasing and in the intev
     *al [1,N].$EA bug in SSPGE -- Could not find unused column.$EA bug$
     * in SSPGE -- Pivot row not found where expected.$E  '/
      data MTXTAB/'$I $I $I;  $B'/
      data MTXTAC/'$I.$N$B'/
 
c *********** End of generated error message data **********
      integer MLOC(8), MACTER(5), MACTOP(2), MACTTX(4), IDAT(4)
      real             FDAT(2)
      data MLOC / LTXTAC, LTXTAD, LTXTAE, LTXTAF, LTXTAG, LTXTAH,
     1  LTXTAI, LTXTAJ /
      data MACTER / MEEMES, 0, 0, 0, MECONT /
      data MACTOP / METEXT, MECONT /
      data MACTTX / MENTXT, 0, METEXT, MERET /
 
 
c
 
c++  Default DEBUG = 0
 
c++  Code for DEBUG > 0 is inactive
C      integer MEFSPV, MEFVEC, MEIVEC, METDIG, MEMDA1, MEMDA2
C      parameter (MEFSPV=54, MEFVEC=61, MEIVEC=57, METDIG=22,
C     1   MEMDA1=27, MEMDA2=28)
C      integer MACTSP(10), MACTFV(6), MACTSV(6), MACTIV(4), MACTXX(2)
C      character SPCOL(1)*32, SPROW(1)*32, BVEC(1)*4, COLSCA(1)*16,
C     1  ROWSCA(1)*16, PCOLLI(1)*36, ROWSZ(1)*12, COLSZ(1)*12,
C     2  COLLOC(1)*11, PROB(1)*46, COLU(1)*60, RMROW(1)*56
C      integer DIGS
C      parameter (DIGS=8)
Cc
C      data SPCOL(1) / '        Col $M, $M Row Entries$B' /
C      data SPROW(1) / '        Row $M, $M Col Entries$B' /
C      data BVEC(1) / 'B:$B'/
C      data COLSCA(1) / 'Column Scales:$B'/
C      data ROWSCA(1) / '   Row Scales:$B'/
C      data PCOLLI(1) / 'Logical => Initial Column mapping:$E' /
C      data ROWSZ(1) / 'Row sizes:$E' /
C      data COLSZ(1) / 'Col sizes:$E' /
C      data COLLOC(1) /'Col locs:$E' /
C      data RMROW(1) /
C     1  'Col $I removed from row $I, (logical row $I), $I left.$E' /
C      data PROB(1) / 'N = $I, Nonzeros = $I, Fraction nonzero = $F$E'/
C      data COLU(1) /
C     1 'Col. $I factored to col. $I with pivot $F in init. row $I.$E' /
Cc                      1 2      3 4      5      6    7      8 9    10
C      data MACTSP/MEMDA1,1,MEMDA2,1,METEXT,METDIG,DIGS,MEFSPV,1,MERET/
Cc                        1       2     3       4  5      6
C      data MACTFV / METEXT, METDIG, DIGS, MEFVEC, 0, MERET /
C      data MACTSV / METEXT, METDIG,      4, MEFVEC, 0, MERET /
C      data MACTIV / METEXT, MEIVEC, 0, MERET /
C      data MACTXX / METEXT, MERET /
C      MACTIV(3) = N
C      MACTFV(5) = N
C      MACTSV(5) = N
c++  End
 
c      integer DIGS
c      parameter (DIGS=8)
c      integer MEFSPV, MEFVEC, MEIVEC, METDIG, MEMDA1, MEMDA2
c      parameter (MEFSPV=54, MEFVEC=61, MEIVEC=57, METDIG=22,
c     1   MEMDA1=27, MEMDA2=28)
c      character NEWROW(1)*27
c      integer MACTNR(10)
cc                      1 2      3 4      5      6    7      8 9    10
c      data MACTNR/MEMDA1,1,MEMDA2,1,METEXT,METDIG,DIGS,MEFSPV,1,MERET/
c      data NEWROW(1) / 'Row at $M, $M Row Entries$B' /
 
      NI = N
      if (NI .le. 0) then
        MACTER(3) = 5
        go to 2000
      end if
      NB = 1
      INCB = 1
      TRANS = .false.
      COND = .false.
      DET = .false.
 
c Options checked here
      K = 4
 10   continue
      K = K + 1
      J = ISPEC(K)
      if (J .gt. 1) then
        if (J .eq. 2) then
c             Setting the number of columns in B.
          NB = ISPEC(K+1)
          INCB = ISPEC(K+2)
          K = K + 2
        else if (J .eq. 3) then
          COND = .true.
        else if (J .eq. 4) then
          DET = .true.
        else
c               Unknnown option
          MACTER(3) = 6
          go to 2000
        end if
        go to 10
      end if
      TRANS = J .eq. 1
      if (ISPEC(1) .eq. 0) then
        LC = 1
        L = 0
        do 110 I = 1, N
c  Get size of user data.
          M = IA(LC+L)
          if ((M .le. 0) .or. (M .gt. NI)) then
c              Bad column size
            MACTER(3) = 7
            IDAT(1) = I
            IDAT(2) = M
            go to 2000
          end if
          if (L .eq. 0) then
            do 70 K = LC + 1, LC + M
              if (A(K) .eq. 0.E0) go to 80
 70         continue
            LC = LC + M + 1
            go to 110
          end if
c Matrix has 0 stored, remove them.
 80       continue
          KB = LC + L
          KK = M
          do 100 K = KB + 1, KB + M
            if (A(K) .ne. 0) then
              A(K-L) = A(K)
              IA(K-L) = IA(K)
            else
              L = L + 1
              KK = KK - 1
            end if
 100      continue
 
          IA(LC) = KK
          LC = LC + KK + 1
 110    continue
      else
        LC = ISPEC(1)
      end if
c         Set pointers for internal vectors.
 
      LCLISC = LC - 1
      LRSWSC = LC + NI
      LCFW = LRSWSC + NI
      LNEXT = LCFW + NI
c Go directly to solve if matrix is factored.
      if (ISPEC(1) .gt. 0) go to 750
      ISPEC(1) = LC
      LROWLI = ISPEC(2) - NI
      LCOLI = LROWLI - NI
      LCOLSZ = LCOLI - NI
      LROWSZ = LCOLSZ - NI
      LROWL = LROWSZ - NI
      IA(LROWL+1) = min(LROWL, ISPEC(3)) - LC
      IA(IA(LROWL+1)) = 0
      if (LNEXT + NI .ge. IA(LROWL+1)) then
c              Insufficient memory
        MACTER(3) = 4
        IDAT(1) = NI
        go to 2000
      end if
 
c Initiialize scaling  (Later an option to avoid this may be added.)
      do 120 I = LC, LCFW
        A(I) = 0.E0
 120  continue
      do 130 I = LROWSZ+1, LCOLSZ
        IA(I) = 0
 130  continue
 
c Get sizes, check indexes.
      LC = 1
      do 160 J = 1, NI
        IA(LROWLI+J) = J
        IA(LCLISC+J) = J
        IA(LCOLI+J) = LC
        A(LCFW+J) = 0.E0
        I = 0
        do 140 K = LC + 1, LC + IA(LC)
          if ((IA(K) .le. I) .or. (IA(K) .gt. NI)) then
c                Bad row indexes
            MACTER(3) = 8
            IDAT(1) = J
            IDAT(2) = IA(K)
            IDAT(3) = I
            go to 2000
          end if
          I = IA(K)
          IA(LROWSZ+I) = IA(LROWSZ+I) + 1
c                Column scaling
          A(LCLISC+J) = max(A(LCLISC+J), abs(A(K)))
 140    continue
        LC = K
 160  continue
 
c       Get row data locations and index of shortest row.
      MROWSZ = IA(LROWSZ+1)
      LASTR = 1
      do 170 I = 2, NI
        IA(LROWL+I) = IA(LROWL+I-1) + IA(LROWSZ+I-1) + 1
        if (IA(LROWSZ+I) .lt. MROWSZ) then
          MROWSZ = IA(LROWSZ+I)
          LASTR = I
        end if
        IA(IA(LROWL+I)) = 0
 170  continue
 
c  Fix scaling and get data indexes ordered by row then column.
 
      do 190 J = 1, NI
        if (A(LCLISC+J) .eq. 0.E0) then
c             Column is all 0.
          MACTER(3) = 2
          IDAT(1) = J
          MACTER(2) = 28
          go to 2010
        end if
        A(LCLISC+J) = 1.E0 / A(LCLISC+J)
        L = IA(LCOLI+J)
        IA(LCOLSZ+J) = IA(L)
        do 180 K = L + 1, L + IA(L)
          I = IA(K)
          IL = IA(LROWL+I)
          IA(IL) = IA(IL)+1
          IA(IL+IA(IL)) = J
          A(IL+IA(IL)) = A(K) * A(LCLISC+J)
c               Update row scale.
          A(LRSWSC+I) = max(A(LRSWSC+I), abs(A(IL+IA(IL))))
 180    continue
 190  continue
 
 
      LFREE = IA(LROWL+1) - NI
 
c++  Code for DEBUG > 0 is inactive
C      IDAT(1) = NI
C      IDAT(2) = LCLISC - NI
C      FDAT(1) = real(IDAT(2)) / real(NI*NI)
C      call SMESS(MACTXX, PROB, IDAT, FDAT)
c++  End
c++  Code for DEBUG > 9 is inactive
Cc              Print the initial matrix
C      K = 1
C      do 380 I = 1, N
C        MACTSP(2) = I
C        MACTSP(4) = IA(K)
C        MACTSP(9) = IA(K)
C        call SMESS(MACTSP, SPCOL, IA(K+1), A(K+1))
C        K = K + IA(K) + 1
C 380  continue
c++  End
c++  Code for DEBUG > 7 is inactive
Cc              Print the initial B
C      call SMESS(MACTFV, BVEC, IDAT, B)
c++  End
c++  Code for DEBUG > 4 is inactive
Cc              Print the scaling
C      call SMESS(MACTSV,COLSCA, IDAT, A(LCLISC+1))
C      call SMESS(MACTSV,ROWSCA, IDAT, A(LRSWSC+1))
c++  End
c++  Code for DEBUG > 4 is inactive
Cc              Print permutations and row sizes and column locations
C      call MESS(MACTIV,PCOLLI, IA(LCLISC+1))
C      call MESS(MACTIV,ROWSZ, IA(LROWSZ+1))
C      call MESS(MACTIV,COLLOC, IA(LCOLI+1))
c++  End
      if (2*LCFW + NI .ge. LFREE) then
c                Insuffient memory
        MACTER(3) = 4
        IDAT(1) = NI
        go to 2000
      end if
 
      do 200 I = 1, NI
c            Save reciprocal of the row scale factors
        if (A(LRSWSC+I) .eq. 0.E0) then
c             Row is all 0.
          MACTER(3) = 3
          IDAT(1) = I
          MACTER(2) = 28
          go to 2010
        end if
        A(LRSWSC+I) = 1.E0 / A(LRSWSC+I)
 200  continue
 
 
c
c ********************** Do the Factorization **************************
 
 
c
c??? Need some checks various places for out of memory.
      LR = IA(LROWL+LASTR)
      LASTR = NI
      do 700 IPIVOT = NI, 1, -1
c         Pick the pivot column
 400    continue
c++  Code for DEBUG > 1 is inactive
Cc              Print the currently selected row
C        MACTSP(2) = IA(LROWLI+LASTR)
C        MACTSP(4) = IA(LR)
C        MACTSP(9) = IA(LR)
C        call SMESS(MACTSP, SPROW, IA(LR+1), A(LR+1))
c++  End
        TP = 1.E30
        KB = 0
        J = IA(LR)
        L = 1000000000
        do 410 K = LR + 1, LR + J
 405      continue
          if (IA(LCOLI+IA(K)) .eq. 0) then
 
c++  Code for DEBUG > 1 is inactive
Cc      Print col. removed from row.
C            IDAT(1) = IA(K)
C            IDAT(2) = IA(LROWLI+LASTR)
C            IDAT(3) = LASTR
C            IDAT(4) = IA(LR) - 1
C        call MESS(MACTSP, RMROW, IDAT)
c++  End
            IA(K) = IA(LR+IA(LR))
            A(K) = A(LR+IA(LR))
            IA(LR) = IA(LR) - 1
 
            IA(LROWSZ+IA(LROWLI+LASTR)) = IA(LR)
 
            if (K .le. LR + IA(LR)) go to 405
            go to 415
          end if
          KK = IA(LCOLSZ+IA(K))
          if (LASTR .gt. IPIVOT) then
            if (L .gt. KK) then
              L = KK
              KB = K
            end if
          else if (abs(A(K)) * TP .gt. real(KK)) then
            TP = real(KK) / abs(A(K))
            KB = K
          end if
          if (K .gt. LR + IA(LR)) go to 415
 410    continue
 415    continue
        if (KB .eq. 0) then
c               We need to pick another row to get the column
          LASTR = LASTR - 1
          if (LASTR .le. IPIVOT) then
            KB = 1000000000
            do 420 I = 1, IPIVOT
              IR = IA(LROWLI+I)
              K = IA(LROWSZ+IR)
              if (K .lt. KB) then
                if (K .ne. 0) then
                  LASTR = I
                  KB = K
                end if
              end if
 420        continue
            if (KB .eq. 1000000000) then
              LASTR = IPIVOT
              do 425 ICP = 1, NI
                if (IA(LCOLI+ICP) .ne. 0) go to 428
 425          continue
c Can't find an unused column -- a bug.
              MACTER(3) = 9
              go to 2000
            end if
          end if
          LR = IA(LROWL+IA(LROWLI+LASTR))
          LASTR = max(LASTR, IPIVOT)
          go to 400
        else
          ICP = IA(KB)
        end if
 
 428    continue
        LCP = abs(IA(LCOLI+ICP))
c++  Code for DEBUG > 1 is inactive
Cc              Print the currently selected column
C      MACTSP(2) = ICP
C      MACTSP(4) = IA(LCP)
C      MACTSP(9) = IA(LCP)
C      call SMESS(MACTSP, SPCOL, IA(LCP+1), A(LCP+1))
c++  End
c Save logical to initial mapping for factored columns.
        IA(LCLISC+IPIVOT) = ICP
c Copy in the original column data
        do 430 I = LCP + 1, LCP + IA(LCP)
          A(LCFW+IA(I)) = A(I) * A(LRSWSC+IA(I)) * A(LCLISC+ICP)
 430    continue
 
c Apply the transformations
c??? This loop could be stopped one sooner, and then do
c    a last one storing directly to LNEXT??
        do 480 J = NI, IPIVOT+1, -1
          LC = IA(LCFW+J)
          NP = IA(LC+1)
c                     Row swap
          TP = A(LCFW+IA(LRSWSC+J))
          A(LCFW+IA(LRSWSC+J)) = A(LCFW+J)
          A(LCFW+J) = TP
          if (TP .ne. 0.E0) then
c               There is a transformation to do
            TP = -TP * A(NP)
            do 470 I = LC + 2, NP - 1
              A(LCFW+IA(I)) = A(LCFW+IA(I)) + TP * A(I)
 470        continue
            A(LCFW+J) = TP
 
c++  Code for DEBUG > 8 is inactive
C            print '(''IPIVOT,J:'', 2I6)', IPIVOT, J
C            call DVECPR(A(LCFW+1), N, 'New Col:', 0, 0, 8)
c++  End
          end if
 480    continue
 
        IA(LCFW+IPIVOT) = LNEXT + 1
        K = LNEXT + 2
        PVAL = A(LCFW+IPIVOT)
        TP = 1.E30
        do 500 I = 1, IPIVOT
c Copy U data to factored space and select the new pivot.
          if (A(LCFW+I) .ne. 0.E0) then
            K = K + 1
            IA(K) = I
            A(K) = A(LCFW+I)
            A(LCFW+I) = 0.E0
            TP1 = (real(IA(LROWSZ+IA(LROWLI+I))) + .125E0)**2
 
            J = IA(LROWL+IA(LROWLI+I))
 
            L = IA(J)
 
            do 490 KK = J + 1, J + L
              if (IA(KK) .eq. ICP) then
c++  Code for DEBUG > 1 is inactive
Cc      Print col. removed from row.
C                IDAT(1) = ICP
C                IDAT(2) = IA(LROWLI+I)
C                IDAT(3) = I
C                IDAT(4) = IA(J) - 1
C                call MESS(MACTSP, RMROW, IDAT)
c++  End
                IA(KK) = IA(J+IA(J))
                A(KK) = A(J+IA(J))
                IA(J) = IA(J) - 1
                go to 495
              end if
 490        continue
 495        continue
            IA(LROWSZ+IA(LROWLI+I)) = IA(J)
 
            if (abs(A(K)) * TP .gt. TP1) then
              TP = TP1 / abs(A(K))
              KB = K
            end if
          end if
 500    continue
        if (TP .ge. 1.E18) then
c               Matrix appears to be singular.
          if (TP .eq. 1.E30) then
            FDAT(1) = 0.E0
          else
            FDAT(1) = A(KB)
          end if
          IDAT(1) = IPIVOT
          MACTER(3) = 1
          OPT(1) = 0.E0
          ISPEC(4) = -IPIVOT
          MACTER(2) = 28
          go to 2010
        end if
 
        IP = IA(KB)
c Get pivot stored where it should be
        if (IP .eq. IPIVOT) then
          IA(LRSWSC+IPIVOT) = IPIVOT
        else
          IA(LRSWSC+IPIVOT) = IP
          J = IA(LROWLI+IPIVOT)
          IA(LROWLI+IPIVOT) = IA(LROWLI+IA(KB))
          IA(LROWLI+IA(KB)) = J
          if (PVAL .ne. 0.E0) then
c                      Just an exchange
            if (IA(K) .ne. IPIVOT) then
c  Pivot row not found where expected -- a bug.
              MACTER(3) = 10
              go to 2000
            end if
            A(K) = A(KB)
            A(KB) = PVAL
          else
c                     Old data needs to move up.
            TP = A(KB)
            do 580 I = KB, K - 1
              IA(I) = IA(I+1)
              A(I) = A(I+1)
 580        continue
            A(I) = TP
            IA(K) = IPIVOT
          end if
        end if
        if (LASTR .eq. IPIVOT) LR = IA(LROWL+IA(LROWLI+IPIVOT))
        IA(LNEXT+2) = K
c           Save reciprocal of the pivot.
        A(K) = 1.E0 / A(K)
        do 680 I = IPIVOT + 1, NI
c Copy L data to factored space.
          if (A(LCFW+I) .ne. 0.E0) then
            K = K + 1
            IA(K) = I
            A(K) = A(LCFW+I)
            A(LCFW+I) = 0.E0
          end if
 680    continue
        IA(LNEXT+1) = K - LNEXT
        IA(LCOLI+ICP) = 0
 
c++  Code for DEBUG > 1 is inactive
Cc               Print data about the pivot.
C        IDAT(1) = IA(LCLISC+IPIVOT)
C        IDAT(2) = IPIVOT
C        IDAT(3) = IA(LROWLI+IPIVOT)
C        FDAT(1) = 1.E0 / A(IA(LNEXT+2))
C        call SMESS(MACTXX, COLU,IDAT, FDAT)
C        call MESS(MACTIV, ROWSZ, IA(LROWSZ+1))
C        call MESS(MACTIV, COLSZ, IA(LCOLSZ+1))
c++  End
 
 
c++  Code for DEBUG > 6 is inactive
C        call MESS(MACTIV,PCOLLI, IA(LCLISC+1))
C        MACTSP(2) = IPIVOT
C        MACTSP(4) = IA(LNEXT+1) - 2
C        MACTSP(9) = MACTSP(4)
C        call SMESS(MACTSP, SPCOL, IA(LNEXT+3), A(LNEXT+3))
C        call MESS(MACTIV,COLLOC, IA(LCFW+1))
c++  End
        LNEXT = K
        if (LNEXT .ge. LFREE) then
c          Insufficient space
          MACTER(3) = 4
          MACTER(2) = 16
          IDAT(1) = IPIVOT
          ISPEC(4) = -NI
          go to 2010
        end if
 700  continue
c
c ****************** Compute A^{-1} b and A^{-T} b  ********************
c
c We have   A x = b  =>  (P_r A P_c) P_c^{-1} x = P_r b
c                       = (U L^{-1}) P_c^{-1} x = P_r b
c Thus x = P_c L U^{-1} P_r b, where
c P_r = permutations mapping initial row indexes to locgical row indexes
c P_c = permutations mapping initial col indexes to locgical col indexes
c
 
c First get b
 750  continue
      if (NB .eq. 0) go to 1000
      IB = 0
      KB = 1
      if (NB .lt. 0) then
        NB = NI
        do 780 J = 1, NI
          do 770 I = IB + 1, IB + NI
            B(I) = 0.E0
 770      continue
          B(IB+J) = 1.E0
          IB = IB + INCB
 780    continue
        IB = 0
      end if
 
      if (TRANS) then
c Solving the transposed system A^T x = b
 810    continue
        do 820 I = 1, NI
c               Store the B vector in logical column order and scale..
          A(LCFW+I) = B(IB+IA(LCLISC+I)) * A(LCLISC+IA(LCLISC+I))
 820    continue
c               Get L^T P_c b
        do 840 J = NI, 1, -1
          TP = A(LCFW+J)
          K = IA(LCFW+J)
          do 830 I = IA(K+1) + 1, K + IA(K) - 1
            TP = TP + A(I) * A(LCFW+IA(I))
 830      continue
          A(LCFW+J) = TP
 840    continue
c++  Code for DEBUG > 8 is inactive
C            call DVECPR(A(LCFW+1), N, 'After L^{T}:', 0, 0, 8)
c++  End
c               Now U^{-T} of the above.
        do 860 J = 1, NI
          K = IA(LCFW+J)
          TP = A(LCFW+J)
          do 850 I = K + 2, IA(K+1) - 1
            TP = TP - A(I) * A(LCFW+IA(I))
 850      continue
          TP = TP * A(I)
c Row swap
          L = IA(LRSWSC+J)
          A(LCFW+J) = A(LCFW+L)
          A(LCFW+L) = TP
 860    continue
c++  Code for DEBUG > 8 is inactive
C            call DVECPR(A(LCFW+1), N, 'After U^{-T}:', 0, 0, 8)
c++  End
c              Get stored back in B (with scaling.
        do 870 I = 1, NI
          B(IB+I) = A(LCFW+I) * A(LRSWSC+I)
 870    continue
        if (KB .lt. NB) then
          KB = KB +1
          IB = IB + INCB
          go to 810
        end if
 
      else
c            Solving A x = b
 910    continue
        do 920 I = 1, NI
          A(LCFW+I) = B(IB+I) * A(LRSWSC+I)
 920    continue
c  The backsolve using w = U^{-1} b
        do 940 J = NI, 1, -1
          LC = IA(LCFW+J)
          NP = IA(LC+1)
          TP = A(LCFW+IA(LRSWSC+J))
          A(LCFW+IA(LRSWSC+J)) = A(LCFW+J)
          A(LCFW+J) = TP * A(NP)
          do 930 I = LC + 2, NP - 1
            A(LCFW+IA(I)) = A(LCFW+IA(I)) - A(LCFW+J) * A(I)
 930      continue
 940    continue
c++  Code for DEBUG > 8 is inactive
C            call DVECPR(A(LCFW+1), N, 'After U^{-1}:', 0, 0, 8)
c++  End
c           Now x = L w
        do 960 J = 1, NI - 1
          LC = IA(LCFW+J)
          do 950 I = IA(LC+1) + 1, LC + IA(LC) - 1
            A(LCFW+IA(I)) = A(LCFW+IA(I)) + A(LCFW+J) * A(I)
 950      continue
 960    continue
c++  Code for DEBUG > 8 is inactive
C            call DVECPR(A(LCFW+1), N, 'Perm. X:', 0, 0, 8)
c++  End
c  Now get x in correct permuted order
 
        do 980 I = 1, NI
          B(IB+IA(LCLISC+I)) = A(LCFW+I) * A(LCLISC+IA(LCLISC+I))
 980    continue
c++  Code for DEBUG > 7 is inactive
C            call DVECPR(A(LNEXT+1), N, ' X:', 0, 0, 8)
c++  End
        if (KB .lt. NB) then
          KB = KB +1
          IB = IB + INCB
          go to 910
        end if
 
      end if
 1000 continue
 
      if (COND) then
c  Get estimate of reciprocal of the condition number.  We ignore column
c  permutations as column permuatations don't affect condition number.
c  Get the condition number of the original scaled matrix.
 
        PVAL = 0.E0
        LC = 1
        do 1020 J = 1, NI
          TP = 0.E0
          do 1010 I = LC + 1, LC + IA(LC)
            TP = TP + abs(A(I)) * A(LRSWSC+IA(I))
 1010     continue
          LC = I
          PVAL = max(PVAL, TP * A(LCLISC+I))
 1020   continue
 
c Solve U y = +/- 1, each choice picks biggest norm.
        do 1110 J = LCFW + 1, LCFW + NI
          A(J) = 0.E0
 1110   continue
        do 1140 J = NI, 1, -1
          LC = IA(LCFW+J)
          NP = IA(LC+1)
c                  Row "swap"
          TP = A(LCFW+IA(LRSWSC+J))
          A(LCFW+IA(LRSWSC+J)) = A(LCFW+J)
c  Want to try both +1 and -1 here.
          TP2 = A(NP) * (TP + 1.E0)
          TP3 = A(NP) * (TP - 1.E0)
          TP = abs(TP2)
          TP1 = abs(TP3)
          do 1120 I = LC + 2, NP - 1
            TP = TP + abs(A(LCFW+IA(I)) - TP2 * A(I))
            TP1 = TP1 + abs(A(LCFW+IA(I)) - TP3 * A(I))
 1120     continue
          if (TP .gt. TP1) then
            TP = TP2
          else
            TP = TP3
          end if
          A(LCFW+J) = TP
          do 1130 I = LC + 2, NP - 1
            A(LCFW+IA(I)) = A(LCFW+IA(I)) - TP * A(I)
 1130     continue
 1140   continue
c           Multiply this solution times L
        do 1160 J = 1, NI - 1
          LC = IA(LCFW+J)
          do 1150 I = IA(LC+1) + 1, LC + IA(LC) - 1
            A(LCFW+IA(I)) = A(LCFW+IA(I)) + A(LCFW+J) * A(I)
 1150     continue
 1160   continue
c Now doing a backsolve with A^T.
c     Get max norm of previous result and multiply by L^T
        TP1 = 0.E0
        do 1230 J = NI, 1, - 1
          TP = A(LCFW+J)
          TP1 = max(TP1, abs(TP))
          K = IA(LCFW+J)
          do 1200 I = IA(K+1) + 1, K + IA(K) - 1
            TP = TP + A(I) * A(LCFW+IA(I))
 1200      continue
          A(LCFW+J) = TP
 1230   continue
c               Now U^{-T} of the above.
        TP2 = 0.E0
        do 1250 J = 1, NI
          K = IA(LCFW+J)
          TP = A(LCFW+J)
          do 1240 I = K + 2, IA(K+1) - 1
            TP = TP - A(I) * A(LCFW+IA(I))
 1240     continue
          TP = TP * A(I)
          L = IA(LRSWSC+J)
          A(LCFW+J) = A(LCFW+L)
          A(LCFW+L) = TP
          TP2 = max(TP2, abs(A(LCFW+J)))
 1250   continue
        OPT(1) = TP1 / (PVAL * TP2)
      end if
      if (DET) then
c                  Compute the determinate
        OPT(3) = 0.E0
c                  First get sign change due to permutations.
        TP = 1
        do 1410 I = 1, NI
c               First for column swaps.
          if (IA(LCLISC+I) .lt. 0) then
            IA(LCLISC+I) = - IA(LCLISC+I)
          else if (IA(LCLISC+I) .ne. I) then
            J = I
 1400       continue
            IA(LCLISC+J) = -IA(LCLISC+J)
            J = -IA(LCLISC+J)
            if (J .ne. I) then
              TP = -TP
              go to 1400
            end if
          end if
c                Row swaps are easier
          if (IA(LRSWSC+I) .ne. I) TP = -TP
 1410   continue
        do 1440 J = 1, NI
          K = IA(IA(LCFW+J)+1)
          TP = TP * A(K) * (A(LRSWSC+J) * A(LCLISC+J))
          if (abs(TP) .gt. 1.E25) then
            TP = TP * 1.E-25
            OPT(3) = OPT(3) - 25.E0
          else if (abs(TP) .lt. 1.E-25) then
            TP = TP * 1.E25
            OPT(3) = OPT(3) + 25.E0
          end if
 1440   continue
        OPT(2) = 1.E0 / TP
      end if
      ISPEC(4) = LFREE - LNEXT - 1
 
      return
 
c
c *********************** Error processing *****************************
c
 2000 continue
      MACTER(2) = 88
 2010 continue
      MACTTX(2) = MLOC(MACTER(3))
c              Output start of error message
      ISPEC(1) = NI
      call MESS(MACTER, MTXTAA, ISPEC(1))
c              Output of options
      K = 5
 2020 continue
      J = 7
      I = 1
      if (ISPEC(K) .eq. IPBDIM) then
        J = 1
        I = 3
      end if
      if ((ISPEC(K) .gt. 3) .and. (ISPEC(K) .le. IPLAST)) then
      call MESS(MACTOP,MTXTAB(1)(J:), ISPEC(K))
        K = K + I
        go to 2020
      end if
      call MESS(MACTOP, MTXTAC, ISPEC(K))
      call SMESS(MACTTX, MTXTAA, IDAT, FDAT)
      ISPEC(1) = -MACTER(3)
      return
      end
