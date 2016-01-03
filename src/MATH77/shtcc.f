      SUBROUTINE SHTCC (MODE,LPIVOT,L1,M,U,  UPARAM,C,LDC,NVC)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 SHTCC  Krogh   Added external statement.
C>> 1994-11-11 SHTCC  Krogh   Declared all vars.
C>> 1994-10-20 SHTCC  Krogh  Changes to use M77CON
C>> 1987-08-19 SHTCC  Lawson  Initial code.
c--S replaces "?": ?HTCC, ?NRM2
c
C        Construction and/or application of a single Householder
C     transformation..     Q = I + U*(U**T)/b
c     where I is the MxM identity matrix, b is a scalar, and U is an
c     M-dimensional Householder vector.
c        All vectors are M-vectors but only the components in positions
c     LPIVOT, and L1 through M, will be referenced.
c        This version, identified by CC at the end of its name, is
c     specialized for the Column-Column case, i.e. U() is a vector or
c     a column of a matrix and C() is regarded a containing a set
c     of column vectors to which transformations will be applied.
c     ------------------------------------------------------------------
C                         Subroutine arguments
c
C     MODE  [in]  = 1 OR 2   When MODE = 1 this subr determines the
c           parameters for a Householder transformation and applies
c           the transformation to NVC vectors.  When MODE = 2 this
c           subr applies a previously determined Householder
c           transformation.
C     LPIVOT  [in]  The index of the pivot element.
C     L1,M  [in]  If L1 .le. M  elements LPIVOT and L1 through M will
c           be referenced.  If L1 .gt. M the subroutine returns
c           immediately.  This may be regarded
c           as performing an identity transformation.
C     U()  [inout]  Contains an M-dimensional vector with storage
c           spacing of 1 between elements.
c           When MODE = 1 this is the vector from which Householder
c           parameters are to be determined.
c           When MODE = 2 this is the result from previous computation
c           with MODE = 1.
c     UPARAM  [inout]  Holds a value that supplements the
c           contents of U() to complete the definition of a
c           Householder transformation.  Computed when MODE = 1 and
c           reused when MODE = 2.
c           UPARAM is the pivot component of the Householder U-vector.
C     C()  [inout]  On entry contains a set of NVC M-vectors to which a
c          Householder transformation is to be applied.
c          On exit contains the set of transformed vectors.
C          These vectors are the columns of an M x NVC matrix in C(,).
C     LDC  [in]  Leading dimension of C(,).  Require LDC .ge. M.
C     NVC  [in]  Number of vectors in C(,) to be transformed.
c           If NVC .le. 0 no reference will be made to the array C(,).
c     ------------------------------------------------------------------
c     Subprograms referenced: SNRM2
c     ------------------------------------------------------------------
c          This code was originally developed by Charles L. Lawson and
c     Richard J. Hanson at Jet Propulsion Laboratory in 1973.  The
c     original code was described and listed in the book,
c
c                  Solving Least Squares Problems
c                  C. L. Lawson and R. J. Hanson
c                  Prentice-Hall, 1974
c
c     Feb, 1985, C. L. Lawson & S. Y. Chan, JPL.  Adapted code from the
c     Lawson & Hanson book to Fortran 77 for use in the JPL MATH77
c     library.
c     Prefixing subprogram names with S or D for s.p. or d.p. versions.
c     Using generic names for intrinsic functions.
c     Adding calls to BLAS and MATH77 error processing subrs in some
c     program units.
c     July, 1987. CLL.  Changed user interface so method of specifying
c     column/row storage options is more language-independent.
C     ------------------------------------------------------------------
      external SNRM2
      real             U(*), UPARAM, C(*), SNRM2
      real             B, FAC, HOLD, VNORM, ONE, ZERO, SUM, BINV
      integer MODE, LPIVOT, L1, M, LDC, NVC
      integer JCBASE, JCPIV, IUL0, J, I
      parameter (ONE = 1.0E0, ZERO = 0.0E0)
C     ------------------------------------------------------------------
      if (0.ge.LPIVOT .or. LPIVOT.ge.L1 .or. L1.gt.M) return
      if( MODE .eq. 1) then
C                            ****** CONSTRUCT THE TRANSFORMATION. ******
         IUL0 = L1 - 1
         if(IUL0 .eq. LPIVOT) then
            VNORM = SNRM2(M-L1+2, U(IUL0), 1)
         else
            HOLD = U(IUL0)
            U(IUL0) = U(LPIVOT)
            VNORM = SNRM2(M-L1+2, U(IUL0), 1)
            U(IUL0) = HOLD
         endif
c
         if (U(LPIVOT) .gt. ZERO) VNORM = -VNORM
         UPARAM = U(LPIVOT)-VNORM
         U(LPIVOT) = VNORM
      endif
C            ****** Apply the transformation  I + U*(U**T)/B  to C. ****
C
      if (NVC .le. 0) return
      B = UPARAM * U(LPIVOT)
C                                 Here B .le. 0.  If B  =  0., return.
      if (B .eq. ZERO) return
      BINV = ONE / B
      JCBASE = 0
c
      do 120 J = 1,NVC
         JCPIV = JCBASE + LPIVOT
         SUM = C(JCPIV) * UPARAM
         do 90 I = L1, M
            SUM = SUM + C(JCBASE+I)*U(I)
   90    continue
         if (SUM .NE. ZERO) then
            FAC = SUM * BINV
            C(JCPIV) = C(JCPIV) + FAC*UPARAM
            do 110 I =  L1, M
               C(JCBASE+I) = C(JCBASE+I) + FAC*U(I)
  110       continue
         endif
         JCBASE = JCBASE + LDC
  120 continue
      return
      end
