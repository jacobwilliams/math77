      SUBROUTINE SHTGEN (MODE,LPIVOT,L1,M,U,LDU,COLU,UPARAM,
     *                   C,LDC,NVC,COLC)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 1996-03-30 SHTGEN Krogh Added external statement.
C>> 1994-11-11 SHTGEN Krogh Declared all vars.
C>> 1994-10-20 SHTGEN Krogh Changes to use M77CON
C>> 1987-08-19 SHTGEN Lawson Initial code.
c--S replaces "?": ?HTGEN, ?AXPY, ?DOT, ?NRM2
c
C        Construction and/or application of a single Householder
C     transformation..     Q = I + U*(U**T)/b
c     where I is the MxM identity matrix, b is a scalar, and U is an
c     M-dimensional Householder vector.
c        All vectors are M-vectors but only the components in positions
c     LPIVOT, and L1 through M, will be referenced.
c        This version, identified by GEN at the end of its name,
c     has the GENerality to handle the options of the U-vector being
c     stored either as a column or row of a matrix, and the vectors in
c     C() may be either column or row vectors.
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
C     U()  [inout]  Contains the "pivot" vector.  Typically U() will be
c           a two-dimensional array in the calling program and the pivot
c           vector may be either a column or row in this array.
c           When MODE = 1 this is the vector from which Householder
c           parameters are to be determined.
c           When MODE = 2 this is the result from previous computation
c           with MODE = 1.
c     LDU  [in]  Leading dimensioning parameter for U() in the calling
c           program where U() is a two-dimensional array.  Gives
c           storage spacing between elements in a row of U() when U() is
c           regarded as a two-dimensional array.
C     COLU  [in]  True means the pivot vector is a column of the 2-dim
c           array U().  Thus the successive elements of the pivot vector
c           are at unit storage spacing.
c           False means the pivot vector is a row of the 2-dim array U()
c           Thus the storage spacing between successive elements is LDU.
c     UPARAM  [inout]  Holds a value that supplements the contents
c           of U() to complete the definition of a
c           Householder transformation.  Computed when MODE = 1 and
c           reused when MODE = 2.
c           UPARAM is the pivot component of the Householder U-vector.
C     C()  [inout]   On entry contains a set of NVC M-vectors to which a
c          Householder transformation is to be applied.
c          On exit contains the set of transformed vectors.
c          Typically in the calling program C() will be a 2-dim array
c          with leading dimensioning parameter LDC.
C          These vectors are the columns of an M x NVC matrix in C(,) if
c          COLC = true, and are rows of an NVC x M matrix in C(,) if
c          COLC = false.
C     LDC  [in]  Leading dimension of C(,).  Require LDC .ge. M if
c           COLC = true.  Require LDC .ge. NVC if COLC = false.
C     NVC  [in]  Number of vectors in C(,) to be transformed.
c           If NVC .le. 0 no reference will be made to the array C(,).
c     COLC  [in]  True means the transformations are to be applied to
c           columns of the array C(,).  False means the transformations
c           are to be applied to rows of the array C(,).
c     ------------------------------------------------------------------
c     Subprograms referenced: SAXPY, SDOT, SNRM2
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
      external SDOT, SNRM2
      real             U(*), UPARAM, C(*), SDOT, SNRM2
      real             B, FAC, HOLD, VNORM, ONE, ZERO, SUM, BINV
      integer MODE, LPIVOT, L1, M, LDU, LDC, NVC
      integer IUPIV, IUL1, IUINC, IUL0
      integer ICE, ICV, I2, I3, INCR, NTERMS, J
      logical COLU, COLC
      parameter (ONE = 1.0E0, ZERO = 0.0E0)
C     ------------------------------------------------------------------
      if (0.ge.LPIVOT .or. LPIVOT.ge.L1 .or. L1.gt.M) return
      if(COLU) then
         IUPIV = LPIVOT
         IUL1 = L1
         IUINC = 1
      else
         IUPIV = 1 + LDU * (LPIVOT-1)
         IUL1 = 1 + LDU * (L1-1)
         IUINC =  LDU
      endif
c
      if( MODE .eq. 1) then
C                            ****** CONSTRUCT THE TRANSFORMATION. ******
         IUL0 = IUL1 - IUINC
         if(IUL0 .eq. IUPIV) then
            VNORM = SNRM2(M-L1+2, U(IUL0), IUINC)
         else
            HOLD = U(IUL0)
            U(IUL0) = U(IUPIV)
            VNORM = SNRM2(M-L1+2, U(IUL0), IUINC)
            U(IUL0) = HOLD
         endif
c
         if (U(IUPIV) .gt. ZERO) VNORM = -VNORM
         UPARAM = U(IUPIV)-VNORM
         U(IUPIV) = VNORM
      endif
C            ****** Apply the transformation  I + U*(U**T)/B  to C. ****
C
      if (NVC.LE.0) return
      B = UPARAM * U(IUPIV)
c                                 Here B .le. 0.  If B  =  0., return.
      if (B .eq. ZERO) return
      BINV = ONE / B
c                                  I2 = 1 - ICV + ICE*(LPIVOT-1)
c                                  INCR = ICE * (L1-LPIVOT)
      if(COLC) then
         ICE = 1
         ICV = LDC
         I2 = LPIVOT - LDC
         INCR = L1 - LPIVOT
      else
         ICE = LDC
         ICV = 1
         I2 = ICE*(LPIVOT-1)
         INCR = ICE*(L1-LPIVOT)
      endif
c
      NTERMS = M-L1+1
      do 120 J = 1,NVC
         I2 = I2 + ICV
         I3 = I2 + INCR
         SUM = UPARAM * C(I2) + SDOT(NTERMS, U(IUL1),IUINC, C(I3),ICE)
         if (SUM .ne. ZERO) then
            FAC = SUM*BINV
            C(I2) = C(I2) + FAC*UPARAM
            call SAXPY(NTERMS, FAC, U(IUL1),IUINC, C(I3),ICE)
         endif
  120 continue
      return
      end
