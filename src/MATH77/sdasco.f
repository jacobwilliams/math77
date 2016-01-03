      subroutine sdasco (a, lda, neq, wt, delta)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2006-05-20 sdasco Hanson Fix bug for computing norm
c>> 2006-05-18 sdasco Hanson return residual norm value in delta(neq+1)
c>> 2006-04-26 sdasco Hanson installed pivoting and test for def rank
c>> 2003-03-06 sdasco Hanson accounted for use of reciprocal weights
c>> 2002-03-19 sdasco Krogh  Added sqrt on the weights.
c>> 2001-11-23 sdasco Krogh  Changed many names per library conventions.
c>> 2001-11-04 sdasco Krogh  Fixes for F77 and conversion to single
c>> 2001-11-01 sdasco Hanson Provide code to Math a la Carte.
c--S replaces "?":?dasco, ?nrm2, ?dot, ?scal, ?rotg, ?rot, ?copy, ?swap,
c--& ?axpy, i?amax
c      implicit none
c     Compute a single Newton step:
c     Solve an underdetermined system.
c     The number of constraint rows is NC=LDD-NEQ.
c
c     On input the first neq entries of delta contain the reciprocals of
C     the weights.  The next entries contain the constraint residuals.
c     On the other hand, the matrix, a, contains only the partials for
C     the constraint rows.
c
      external isamax
      integer isamax
      integer    i, imax, j, lda, neq, nc, nrank
      real             a(lda,*), wt(*), delta(*), c, s, z, tn
      external sdot, snrm2
      real             sdot, snrm2
      nc  = lda - neq
      if (nc .le. 0) return
c     Use weights derived from error norm.
      do 10  j=1, neq
         call sscal (nc, sqrt(wt(j)), a(1,j), 1)
   10 continue
 
c     Triangularize constraint matrix using right-handed
c     plane rotations.  These are stored in the entry
c     eliminated and then reconstructed.
      do 30  i=1, min(neq,nc - 1)
c     Do row pivoting to maximize diagonal term (in L2 norm)
c     so that smallest pivots are at the SE part of the lower triangle.
         do 25  j=i,nc
            delta(j)=sdot(neq-i+1,a(j,i),lda,a(j,i),lda)
   25    continue
         imax = isamax(nc-i+1,delta(i),1)
c     If found a row with larger L2 norm, interchange this row
c     and the right-hand side.
         if(imax .gt. 1) then
            call sswap(neq,a(i,1),lda,a(i+imax-1,1),lda)
            call sswap(1, delta(neq+i),1,delta(neq+i+imax-1),1)
         end if
c     Eliminate the non-zero entries and store the rotation
c     in the place eliminated.  These are used to compute
c     the least-distance move back to the constraints.
c     Note that this uses an often overlooked feature of ?rotg.
         do 20  j=i + 1, neq
            call srotg (a(i,i), a(i,j), c, s)
            call srot (nc-i, a(i+1,i), 1, a(i+1,j), 1, c, s)
   20    continue
   30 continue
c     This is a special elimination step for the last row.
      do 40  j=nc + 1, neq
         call srotg (a(nc,nc), a(nc,j), c, s)
   40 continue
c     System is now lower triangular.  Forward solve step.
c     The right-hand side for this system is in DELTA(NEQ+I),
c     I=1,NC.
      DELTA(1)=0.E0
      CALL SCOPY(NEQ, DELTA, 0, DELTA, 1)
      nrank = min(nc,neq)
      do 50  i=1, nc
c     Due to the weighting used, a small number is .le. 1 for
c     a diagonal term.
          if(abs(a(i,i)) .le. 1.e0) then
            nrank=i-1
            go to 55
          end if
         delta(i) = delta(neq+i)
   50 continue
   55 continue
c Solve for the update.
      do 60  i=1, nrank - 1
         z = delta(i)/a(i,i)
         call saxpy (nrank-i, -z, a(i+1,i), 1, delta(i+1), 1)
         delta(i)=z
   60 continue
      delta(nrank) = delta(nrank)/a(nrank,nrank)
c     Compute residuals on those equations not solved.
c     They should be consistent but may not due to
c     modelling errors or large numerical errors.
      do 70 i=nrank+1,nc
         delta(neq+i)=delta(neq+i)-sdot(nrank,a(i,1),lda,delta,1)
   70 continue
c     Now apply the Givens transformations in reverse order.
c     They are stored in one entry form so the values are
c     reconstructed as in the 1979 Level 1 BLAS paper or
c     LINPACK, page A.5.
      do 90  i=min(neq,nc), 1, -1
         do 80  j=neq, i + 1, -1
            z = a(i,j)
            if (abs(z) .lt. 1.E0) then
               s = z
               c = sqrt((1.E0-s)*(1.E0+s))
            else if (abs(z) .gt. 1.E0) then
               c = 1.E0/z
               s = sqrt((1.E0-c)*(1.E0+c))
            else if (z .eq. 1.E0) then
               c = 0.E0
               s = 1.E0
            end if
            z        = c*delta(i) - s*delta(j)
            delta(j) = s*delta(i) + c*delta(j)
            delta(i) = z
   80    continue
   90 continue
 
 
c     Compute the amount of inconsistency in the constraint
c     equations.  Return a factor that allows for testing.
      if(nrank .lt. nc) then
        tn = snrm2(nc-nrank, delta(neq+nrank+1),1)
      else
        tn = 0.e0
      end if
c     Store the value in delta(neq+1) for testing after returning.
c     Make that error test based on the size of delta(neq+1).
      delta(neq+1) = tn
c     Apply weights used for step.
C     They were also input in DELTA(:).
      do 100  j=1, neq
          delta(j) = delta(j)*sqrt(wt(j))
  100 continue
      return
      end
