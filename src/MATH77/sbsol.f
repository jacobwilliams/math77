      SUBROUTINE SBSOL (MODE,G,LDG,NB,IR,JTPREV,X,N,RNORM, IERR3)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 SBSOL  Krogh  Declared all vars.
C>> 1994-10-20 SBSOL  Krogh  Changes to use M77CON
C>> 1987-11-24 SBSOL  Lawson Initial code.
c--S replaces "?": ?BSOL
c     Solution of banded least squares problem following use of _BACC to
c     accumulate the data.
c     ------------------------------------------------------------------
c                            Subroutine arguments
c
C     MODE  [in]  Flag selecting operation to be done.
c          = 1     SOLVE R*X=Y   WHERE R AND Y ARE IN THE G( ) ARRAY
C                  AND X WILL BE STORED IN THE X( ) ARRAY.
C            2     SOLVE (R**T)*X=Y   WHERE R IS IN G( ),
C                  Y IS INITIALLY IN X( ), AND X REPLACES Y IN X( ),
C            3     SOLVE R*X=Y   WHERE R IS IN G( ).
C                  Y IS INITIALLY IN X( ), AND X REPLACES Y IN X( ).
C
c     G(,)  [in]  Array in which the transformed problem data has
c           been left by _BACC.
c     LDG   [in]  Leading dimensioning parameter for G(,).
c     NB    [in]  Bandwidth of data matrix.
c     IR    [in]  Must have value set by previous call to _BACC.
c     JTPREV    [in]  Must have value set by previous call to _BACC.
c     X()   [inout]  Array of length at least N.
c           On entry with MODE = 2 or 3 contains the y vector.
c           In all (non-error) cases contains the solution vector on
c           return.
c     N     [in]  Specifies the dimension of the desired solution
c           vector.  Can be set smaller than the max value the data
c           would support.
c     RNORM  [out]  Set to the euclidean norm of the residual vector
c           when MODE = 1.  Set to zero otherwise.
c     IERR3  [out]  =0 means no errors detected.
c                  =1 means a diagonal element is zero.
c     ------------------------------------------------------------------
C     C.L.LAWSON AND R.J.HANSON, JET PROPULSION LABORATORY, 1973 JUN 12
C     Reference:   'SOLVING LEAST SQUARES PROBLEMS', PRENTICE-HALL, 1974
c     Comments in this code refer to Algorithm Step numbers on
c     pp 213-217 of the book.
c     ------------------------------------------------------------------
c     1984 July 11, C. L. Lawson, JPL.  Adapted code from book
c     to Fortran 77 for JPL MATH 77 library.
c     Prefixing subprogram names with S or D for s.p. or d.p. versions.
c     Using generic names for any intrinsic functions.
c     July 1987.  Using _HTCC in place of _H12.
c     ------------------------------------------------------------------
c     Subprograms called: IERM1, IERV1
c     ------------------------------------------------------------------
      integer MODE, LDG, NB, JTPREV, IR, N, IERR3
      integer J, NP1, IRM1, II, I, L, IE, JG, IX, I1, I2
      real             G(LDG,NB+1),X(N), ZERO, RNORM, RSQ, S
      parameter( ZERO = 0.0E0)
c     ------------------------------------------------------------------
C
      IERR3 = 0
      RNORM=ZERO
      if( MODE .eq. 1) then
C                                   ********************* MODE = 1
C                                   ALG. STEP 26
         DO 20 J=1,N
   20       X(J)=G(J,NB+1)
         RSQ=ZERO
         NP1=N+1
         IRM1=IR-1
         if (NP1 .le. IRM1) then
            DO 30 J=NP1,IRM1
   30            RSQ=RSQ+G(J,NB+1)**2
            RNORM=SQRT(RSQ)
         endif
      endif
c
      if( MODE .eq. 2) go to 90
C                                   ********************* MODE = 1 or 3
C                                   ALG. STEP 27
           DO 80 II=1,N
           I=N+1-II
C                                   ALG. STEP 28
           S=ZERO
           L=MAX(0,I-JTPREV)
C                                   ALG. STEP 29
           if (I .ne. N) then
C                                   ALG. STEP 30
              IE=MIN(N+1-I,NB)
              DO 60 J=2,IE
                 JG=J+L
                 IX=I-1+J
   60            S=S+G(I,JG)*X(IX)
           endif
C                                   ALG. STEP 31
           if (G(I,L+1) .eq. ZERO) go to 130
   80      X(I)=(X(I)-S)/G(I,L+1)
C                                   ALG. STEP 32
      return
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C                                   ********************* MODE = 2
   90      DO 120 J=1,N
              S=ZERO
              if (J .ne. 1) then
                 I1=MAX(1,J-NB+1)
                 I2=J-1
                 DO 100 I=I1,I2
                    L=J-I+1+MAX(0,I-JTPREV)
  100               S=S+X(I)*G(I,L)
              endif
              L=MAX(0,J-JTPREV)
              if (G(J,L+1) .eq. ZERO) then
                 I = J
                 go to 130
              endif
  120         X(J)=(X(J)-S)/G(J,L+1)
      return
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  130 continue
      IERR3 = I
      call IERM1('SBSOL', IERR3, 0,'Singular matrix',
     * 'MODE', MODE, ',')
      call IERV1('Index of zero diag elt', I, '.')
      return
      end
