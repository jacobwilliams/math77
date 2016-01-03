      SUBROUTINE SCHOL (A, NDA, N, B, S, TOL, IERR)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c>> 2006-04-10 SCHOL Krogh  Fix for out of range unused subscript.
c>> 2001-10-25 SCHOL Snyder & Krogh Added c$OMP PARALLEL DO, etc.
c>> 2001-05-25 SCHOL Snyder & Krogh Compute inner products using SDOT
c>> 1996-03-30 SCHOL Krogh  Added external statement.
c>> 1994-10-20 SCHOL Krogh  Changes to use M77CON
c>> 1991-06-26 SCHOL Krogh  Initial MATH77 version.

c     C. L. Lawson, JPL, 1970 April 30
c     Extensively modified by F. T. Krogh, JPL, 1991, September 23.

c Solution of positive definite system using Cholesky decomposition.

c Suppose the overdetermined system to be solved in the sense of least
c squares is   C*X = D

c Suppose normal equations are formed and stored as follows.

c                    A = (C**T)*C
c                    B = (C**T)*D
c                    S = (D**T)*D

c Given B and the upper triangle of A this subroutine computes the
c solution vector X, storing it in place of B.

c On input S should either = (D**T)D, or be zero.  If the former, then
c on return it is the euclidean norm of (CX - D).  Else the zero value
c is returned.

c The upper triangle of A is replaced by the upper triangular Cholesky
c matrix, R, satisfying  (R**T)*R = A.

c The value of TOL should be 10**(-k-1) where k is the minimum number of
c significant digits in A and B.   If TOL is < the relative precision in
c the computer's arithmetic, it is effectively replaced by that relative
c precision.  If some R(I,I)**2 is < TOL * (corresponding diagonal of
c A), then IERR is set to the value of I for the algebraically smallest
c value of R(I,I).  If R(I,I)**2 is .le. zero, then everything in the
c I-th row of R is set to zero, as is the I-th component of the
c solution, and IERR is replaced by -IERR to flag that this occured.

c--S replaces "?": ?CHOL, ?DOT

c ********************* Variable definitions ***************************

c A      Input matrix, only upper triangle is used.  Replace by factor.
c B      Input right hand side, output solution.
c R1MACH Library routine to get characteristics of floating point
c  numbers.  R1MACH(4) = smallest number, x, such that 1.0 + x .ne. 1.0.
c SDOT   Compute dot products.
c G      Temporary for accumulating sums.
c GMIN   Smallest value seen for G when working on a diagonal entry.
c I      Index used to access matrix entries.
c IERR   Used to return status, see above.
c J      Index used to access matrix entries.
c K      Index used to access matrix entries.
c N      Order of matrix.
c NDA    Declared first dimension for A.
c S      Formal argument, see above.
c TOL    Input value for the tolerance, see above.
c TOLI   Internal value for the tolerance.
c TOLM   Machine tolerance, lower limit for TOLI.
c TSQ    TOLI**2.

c ******************** Variable declarations ***************************

      external         R1MACH, SDOT
      integer          NDA, N, IERR
      real             A(NDA, N), B(N), S, TOL
      integer          I, J, K
      real             G, TSQ, GMIN, TOLI, TOLM
      real             R1MACH, SDOT
      save TOLM
      data TOLM /0.E0/

c ******************** Start of executable code ************************
      if (N .le. 0) return
      if (TOLM .le. 0.E0) TOLM = R1MACH(4)
      TOLI = max(TOL, TOLM)
      TSQ = TOLI**2
      GMIN = A(1, 1)
      IERR = 0
      DO 50 I = 1, N
         G = A(I, I) - SDOT( I-1, A(1, I), 1, A(1, I), 1 )
         if ( G .lt. TSQ * abs(A(I, I)) ) then
            if (G .le. GMIN) then
               GMIN = G
               IERR = I
               if (G .le. 0.E0) IERR = -I
            end if
            if (G .le. 0.E0) then
               do 20 K = I, N
                  A(I, K) = 0.E0
   20          continue
               B(I) = 0.E0
               go to 50
            end if
         end if
         A(I, I) = sqrt(G)

c$OMP PARALLEL DO
         DO 30 J = I + 1, N
            A(I,J) = (A(I,J) - SDOT(I-1, A(1,I), 1, A(1,J), 1)) / A(I,I)
   30    continue
c$OMP END PARALLEL DO
c                        Solve next row of first lower triangular system
         B(I) = (B(I) - SDOT( I-1, A(1, I), 1, B, 1 )) / A(I, I)
   50 continue
c                        Get the solution norm
      if ( S .gt. 0.E0 ) S = sqrt( max(0.E0,S-SDOT(N,B,1,B,1)) )
c                        Solve the second lower triangular system.
      if (A(N,N) .gt. 0.E0) B(N) = B(N) / A(N,N)
      DO 80  I = N-1, 1, -1
        if (A(I, I) .gt. 0.E0) then
          B(I) = (B(I) - SDOT(N-I, A(I,I+1), NDA, B(I+1), 1) ) / A(I, I)
        end if
   80 continue
      end
