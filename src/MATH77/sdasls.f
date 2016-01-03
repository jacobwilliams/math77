      SUBROUTINE sdasls (sdasf, neq, t, y, yprime, info, ftol, rnktol,
     &  c, ldc, ltd, idid, rwork, lrw, iwork, liw)
 
c Copyright (c) 2008, Math a la Carte, Inc.
c R. J. Hanson, Visual Numerics, Inc., 23 June 2008.
c F. T. Krogh, Math a la Carte, Inc.
 
c>>2009/11/02 sdasls Krogh Avoided ref. to undefined IWORK locations.
c>>2008/10/26 sdasls Krogh Changed 'No' no 'N' in call to sgemv.
c>>2008/09/18 sdasls Krogh obtained from Hanson set up single prec.
c--S replaces "?":?daslx, ?dasls, ?dasf, ?swap, ?scal, ?axpy, ?copy,
c--&  ?asum, ?gbfa, ?gbsl, ?gemv, ?gefa, ?gesl
 
c Provide starting values for y' == yprime(*) for a differential-
c algebraic equation f(t,y,y') = 0.
c This routine is normally to be used before a call to SDASLX
c or SDASLX to get consistent initial conditions for y',
c i.e. f(t,y,y') = 0 at the initial t.
 
c The assumptions are:
c A) The system has index 0 or index 1
c B) Initial values for (t,y) are known but maybe not all
c    values of y'.
c C) All values of y' have some meaningful initial estimates.
c    The partials f_t, f_y and f_y' are computable and continuous
c    at (t,y,y').  The rank of f_y' must be positive.
c D) The initial values (t,y) are fixed.  Any values for y' are
c    allowed to change to achieve f(t,y,y') = 0.
 
c This routine is organized so that a user can add additional cases in
c the evaluation routine SDASF written for SDASLX that separately
c computes f_t, f_y and f_y'.  The evaluation of f(t,y,y') uses the
c same flag as SDASLX.
 
 
c  SDASF:EXTERNAL  -- Depending on how INFO(5) is set, for forward or
c     reverse communication, this routine is used to compute four
c     terms f, f_t, f_y, and f_y' as requested.  It has the form
 
c         SUBROUTINE SDASF(T,Y,YPRIME,DELTA,C,LDC,CJ,IRES,RWORK,IWORK)
 
c     The actions taken depend on the value of the IRES argument
c     to SDASF.  The values of IRES that occur when using sdasls are:
c   = 0 Initialize subroutine parameters or any storage requirements.
c       If this does not apply, then ignore this value and return
c       without any further action.  Initial conditions for t, y and y'
c       can be assigned to T, Y(:) and YPRIME(:).
c   = 1 Evaluate f(t,y,y') and return the result in DELTA(1:NEQ).
c   = 6 Evaluate the NEQ-vector f_t (evaluated at (t,y,y')) and return
c       the result in DELTA(1:NEQ)
c   = 7 Evaluate the NEQ by NEQ matrix f_y' and return the result in
c       C(1:NEQ,1:NEQ), for dense Jacobians.  For banded Jacobians
c       return the result in C(1:ML+MU+1,1:NEQ) according to the
c       Linpack data structure for banded matrices.
c   = 8 Evaluate the NEQ by NEQ matrix f_y  and return the result in
c       C(1:NEQ,1:NEQ).  For banded Jacobians return the result in
c       C(1:ML+MU+1,1:NEQ).This matrix is required when the
c       system has index 1, i.e. when f_y' is rank-deficient.
c
c  NEQ:IN   This is the number of equations to be solved.  NEQ .ge. 1.
c
c  T:IN   This is the current value of the independent variable.
c      Set this to the value of the initial point.
c
c  Y(.ge. NEQ):IN  This array contains the solution components at T.
c      Initialize to the values at the initial point.
c
c  YPRIME(.ge. NEQ):INOUT  This array contains the current derivatives
c      of the solution components at T.  You must provide some intial
c      values, but they need not satisfy f(t,y,y')=0.
c
c  INFO(.ge. 16):IN  The only task of the code is to solve the system
c     f(t,y,y')=0 at the initial point.  Values of INFO(:) are used
c     for the same purposes as will occur when integrating the system
c     using SDASLX.
c     When the Jacobians are banded matrices, |INFO(5)|=4.  In that
c     case a regularization paramter is required.  For this use
c     INFO(14) .eq. 0 to have the parameter with value macheps**(2./3.)
c     To use other values set INFO(14) > 0 and place the regularization
c     parameter in RWORK(INFO(14)).
c  FTOL:IN  This is the tolerance required for the L_1 norm of
c     f(t,y,y'). Try macheps ** (2./3.) * size of values occuring in f.
c  RNKTOL:IN  This is the relative column norm tolerance used for
c     determining rank deficiency of f_y'.  It is also used for
c     determining the rank of certain rows of the combined matrix
c     [f_y'
c      f_y].  If this latter matrix is rank deficient the system
c     is defined to have an index .gt. 1.  As such SDASLX does
c     not claim to solve it. Try macheps ** (2./3.).
c
c  C(LDC, LTD):INOUT  This is the working array where the partial
c     derivative matrices f_y' and f_y will be returned after
c     evaluation by sdasf or reverse communication.
c
c  LDC:IN This is the leading dimension of the array C(:,:).  It has
c     a value LDC .ge. NEQ when f_y' has full rank.  This occurs with
c     an index 0 problem.  When f_y' is rank deficient then
c     LDC .ge. 2*NEQ is required.  When f_y' is rank deficient,
c     and the Jacobian matrices are banded, then
c     LDC .ge. 4*ML+2*MU+4 is required.  This occurs when
c     the problem has index 1 or higher.
c
c  LDT:IN This is the second dimension of the array C(:,:).  It has
c     a value LTD .ge. NEQ when f_y' has full rank.  This occurs with
c     an index 0 problem for either dense or banded problesm.  When
c     f_y' is rank deficient, and the Jacobian matrices are banded,
c     then LDT .ge. 2*NEQ is required.  This occurs when the problem
c     has index 1 or higher.
c
c  IDID:OUT This is an output flag that shows the status of solving
c     f(t,y,y')=0 for values of y'.
c   = 0 The system is index 0 or 1 and consistent values for y'
c       were computed
c   =-1 An error or exceptional condition was noted.  Details are
c       flagged by various values in IWORK(1).
c .ge.1 Compute requested value using reverse communication.
c       Re-enter the routine sdasls() for further computations.
c IWORK(1) flag, giving more detail:
c Currently IOFF=8, IROFF=2.
c   = 0 The system has index 0 and consistent values for y'
c       were computed
c   = 1 The system has index 1 and consistent values for y'
c       were computed
c   = 2 The system has index 0 but the system f(t,y,y') is
c       not consistent using the tolerance FTOL. It should
c       be consistent so FTOL may be too small for this problem.
c       Iterations are done while the L_1 norm of f is .lt.
c       1/4 the L_1 norm of f from the previous iteration.
c   = 3 The system has index 1 but the equation f(t,y,y')=0
c       is not consistent using the tolerance FTOL.
c       Iterations are done while the L_1 norm of f is .lt.
c       1/4 the L_1 norm of f from the previous iteration.
c   = 4 The system appears to have an index .gt. 1.
c   = 5 The system has rank f_y' = 0.  This is not a DAE.
c   = 6 The value NEQ .le. 0, i.e. no system
c   = 7 LDC is too small.  Must be .ge. NEQ for index 0 problems,
c                          dense Jacobians.
c                          Must be .ge. (2*ML+MU+1), banded, index 0.
c       Here ML, MU are the lower and upper band widths. Provide the
c       lower ML and upper MU bandwidths by setting IWORK(1)=ML and
c       IWORK(2)=MU.
c       Must be .ge. 4*ML+2*MU+4 if problem is banded with index 1.
c   = 8 LDC is too small.  Must be .ge. 2*NEQ for index 1 problems,
c                          dense Jacobians.
c                          Must be .ge. 2*(2*ML+1)+2*(MU+1), banded.
c   = 9 LRW is too small.  Must be .ge. 2*NEQ+IROFF for dense
c                          systems and .ge. 7*NEQ+IROFF for
c                          banded systems.
c   = 10 LIW is too small.  Must be .ge. 2*NEQ+IOFF
c   = 11, Must have |INFO(5)|=2 or 4.  Routine supports dense
c         or banded matrices only. The user computes derivatives in
c       forward or reverse communication.
c   = 12, Must have FTOL .gt. 0. Now .le. 0.
c   = 13, Must have RNKTOL .ge. 0. Now .lt. 0.
c   = 14, Must have sub- and super- bandwidth parameters
c         ML, MU .ge. 0.  Now one of them is .lt. 0.
c   = 15, Must have second dimension, LTD, of C(*,*) .ge. NEQ
c         for dense or banded systems.  Value is now .lt. NEQ.
c         For banded systems of index 1 C(*,*) .ge. 2*NEQ.  This
c         is checked when the index is .gt. 0, or f_y' is singular.
c
c  RWORK(LRW .ge. 2*NEQ+IROFF):INOUT Working floating point
c         storage for dense systems,
c        LRW .ge. 7*NEQ+IROFF for banded systems.
c  IWORK(LIW .ge. 2*NEQ+IOFF):INOUT Working integer storage
c  User code can change values in these arrays past these minimum
c  limits for evaluation or other storage needs.
 
c  INTERNAL VARIABLE DICTIONARY -
c  Floating Point Variables:
c  DELTA(1:NEQ) of SDASF is associated with RWORK(1+IROFF:NEQ+IROFF).
c  Used for evaluation of f(t,y,y') and f_t.
c  EPS - Machine precision for this accuracy.  Obtained by a
c        call to R1MACH.
c  REGUL  - Value of the regularization parameter used if the system
c           is banded and of index .gt. 1.  Default is EPS**(2./3.)
c           but the user can provide an alternate value in
c           location RWORK(INFO(14)).
c  BIGVAL - Temporary value used to contain the largest magnitude
c      during full pivoting
c  CJ - A dummy floating point value, not used in this routine, but
c      it is an argument of SDASF.  Set to 0 for all SDASF calls.
c  ONE, ZERO - Constants, 1 and 0
c  TEMP - Temporary value used in swapping row values during
c         elimination
c  BIGVAL - Temporary value used for choosing pivots
c  CJ - Value used as temporary for forward communication with SDASF.
c       This is not used but the value is assigned 0.
c  ONE, ZERO - Local copies of 1.e0 and 0.e0.
 
c  Integer Variables:
c  IRES  - A flag for directing user action in routine sdasf
c  I - A dummy index for looping
c  IS - A dummy subscript for building interleaved matrix array
c  IROW - The row index of the largest magnitude in pivoting
c  IFLAG - Indicate a flat zero on the diagonal using
c         LINPACK LU factorization code, SGEFA.
c  IOFF - The offset value in IWORK(*) that is
c         used for storage of integer values and
c         communication flags.  So IWORK(IOFF+1:IOFF+2*NEQ)
c         is used for pivoting information.
c         IWORK(1:IOFF) is used for storage and must remain
c         unchanged during forward or reverse communication.
c         The present value is IOFF=8.
c  IROFF - The offset value in RWORK(*) that is used for
c         storage of floating point parameters.
c         So RWORK(IROFF+1:IROFF+2*NEQ) is used for storage
c         of intermediate values and requested values from
c         user code.  The present value is IROFF=2.
c
c  J - A dummy index for looping
c  JCOL - The column index of the largest magnitude in pivoting
c  I1,I2 - Temporary indices for placement of interleaved
c          matrix values
c  IREVC - Type of matrix, dense or banded, forward
c          or reverse communication mode
c  IRANK - Rank of f_y' for index 1 problems
c  ML, MU - For banded Jacobians, these are temps that hold
c           the number of lower and upper co-diagonals that
c           are non-zero.
c  M       - A temporary integer holding subscripts for the
c           banded interleaved Jacobian.
c  MLP, MUP - For banded Jacobians, these are temps that hold
c           the number of lower and upper co-diagonals when
c           the interleaved and regularized matrix is constructed.
c           Have MLP=2*ML+1, MUP=max(2*MU,1).
c  MP       - A temporary integer holding subscripts for the
c           banded interleaved Jacobian.
c  NROWS - A temporary that holds the number of rows to zero
c          in the C(:,:) array for computing f_y' and f_y, later.
c  Logical Variables:
c  DENSE, BANDED - Logical variables signifying that the
c           Jacobians are either dense or banded matrices.
c           Exactly one of these variables are .TRUE.
c           This information is taken from the input value
c           INFO(5).
 
c  Variable typing, subroutine arguments
      EXTERNAL sdasf
      INTEGER neq,ldc,ltd,info(*),idid,lrw,liw,iwork(*)
      REAL             eps,t,y(*),yprime(*),c(ldc,*),rwork(*),ftol,
     &  rnktol,regul
 
c  Variable typing, local arguments or routines called
      INTEGER i,i1,i2,is,iflag,ioff,iroff,ires,irank,irevc,irow,j,jcol,
     &  k,ml,mu,mlp,mup,m,mp,nrows
      EXTERNAL r1mach,sasum,sscal,saxpy,sswap,scopy,sgemv,sgefa,sgesl,
     &  sgbfa,sgbsl
      REAL             r1mach,bigval,cj,sasum,one,zero,temp
      LOGICAL dense,banded
 
      ires=0
      one=1.e0
      zero=0.e0
c This defines the starting index in IWORK(*) where
c row and column pivoting interchanges are stored.
c Locations of IWORK(1:IOFF) are needed for possible reverse
c communication storage and signalling.
      ioff=8
c This defines the starting index in RWORK(*) where
c requested results for f and f_t will be placed.  Locations
c RWORK(1:IROFF) are needed for storage of routine values.
      iroff=2
c This routine only recognizes |IREVC| = 2 or 4.
c These values correspond to dense or banded matrices.
      irevc=info(5)
c Check that these are the allowed values.
      i=abs(irevc)
      dense=i.eq.2
      banded=i.eq.4
 
      IF (.not.(dense.or.banded)) THEN
        iwork(1)=11
        idid=-1
        GO TO 370
      END IF
      if (info(1) .eq. 1) then
c When reverse communication is used IRANK is saved.
c With forward communication IRANK is not defined until
c the LU factorization of f_y' is completed.
        irank=iwork(6)
c Extract band widths.  If the Jacobian is dense these are not
c used but it does not matter.
        ml=iwork(7)
        mu=iwork(8)
      end if
c Give this parameter a defined value.  It is not used
c but the evaluator routine needs a slot for it.
      cj=zero
 
 
c Re-enter to just past this subroutine return point when reverse
c communication is used.
      GO TO (40,70,170,190),info(1)
c The first entry drops through here, INFO(1) == 0.
c Check sizes of arrays and other parameters.  Branch to
c a block of code that does this and then branches back here.
      GO TO 360
c Set up counters and flags for reverse communication.
c This is done just once because INFO(1) is .gt. 0 for
c reverse communication entries.
 
 
c This is where the results for an evaluation of f or f_t
c should be placed, RWORK(IWORK(4)+1:IWORK(4)+NEQ)
   10 iwork(4)=iroff
      iwork(5)=0
 
 
c Make initialization call.  This is done even when reverse
c communication is used.
      ires=0
      CALL sdasf (t, y, yprime, rwork(iroff+1), c, ldc, cj, ires, rwork,
     & iwork)
 
c Start of overall loop to compute Newton update for y'
 
c Make a call to sdasf to obtain the value of f(t,y,y').  The
c result is returned in RWORK(IROFF+1:IROFF+NEQ).
   20 ires=1
c If IRES=1 or 6, clear out the spot where the results will go.
c These are respectively the function and f_t.
      DO 30 i=1,neq
        rwork(i+iroff)=zero
   30 CONTINUE
      IF (irevc.gt.0) THEN
        CALL sdasf (t, y, yprime, rwork(iroff+1), c, ldc, cj, ires,
     &   rwork, iwork)
      ELSE
        info(1)=1
        GO TO 380
      END IF
c Branch back place if reverse communication in use.
   40 CONTINUE
 
c After one Newton update step, check the size of the
c function f(t,y,y').  It should be zero but won't be
c due to rounding and other approximations.  Check its
c value against the user tolerance, FTOL.
      IF (iwork(5).gt.0) THEN
c Need to distinguish index 0 and 1 cases here.
        temp=sasum(neq,rwork(iroff+1),1)
        IF (temp.le.ftol) THEN
          idid=0
          iwork(1)=0
c Return L_1 norm of f here.
          rwork(2)=temp
        ELSE
c After a Newton update step the defect or residual in the DAE
c should be small.  If it is not decreasing at an acceptable
c rate (1/4) then flag a possible inconsistency.
 
          IF (temp.lt.0.25*rwork(2)) THEN
c Save the current norm of f for checking the next iteration.
            rwork(2)=temp
            GO TO 50
          END IF
          idid=-1
          iwork(1)=2
          IF (irank.lt.neq) iwork(1)=3
c Return L_1 norm of f here.
          rwork(2)=temp
        END IF
        IF(IDID .lt. 0) GO TO 370
c No errors here.  Function is small, everything normal.
c No error processing needed here.
        info(1)=0
        RETURN
      END IF
c Branch point for looping while norm of f is decreasing
c at an acceptable rate.
   50 CONTINUE
c Obtain the NEQ by NEQ partial matrix with respect to y'
c Note that there is no check on the norm of f until at least one
c update step for y'.  This is to catch systems that are of
c index .gt. 1, but the user has provided consistent initial data.
      ires=7
c Clear out the working matrix C(:,1:NEQ) before
c computing the partials f_y'.
      IF (dense) nrows=neq
      IF (banded) nrows=2*ml+mu+1
      DO 65 j=1,neq
        DO 60 i=1,nrows
        c(i,j)=zero
   60 CONTINUE
   65 CONTINUE
      IF (irevc.gt.0) THEN
        CALL sdasf (t, y, yprime, rwork(iroff+1), c, ldc, cj, ires,
     &   rwork, iwork)
      ELSE
        info(1)=2
        GO TO 380
      END IF
c Branch back place if reverse communication in use.
   70 CONTINUE
c For the dense Jacobian case:
c Compute LU factorization with full pivoting and explicit
c interchanges. The use of full pivoting is to achieve the
c best numerical stability and to put the small block, if
c there is one, in the lower SE corner of the matrix.
!----------------------------------------------------------------------
      IF (dense) THEN
        DO 100 i=1,neq
c Search in the remaining SE block for the largest magnitude.
c Record the row and column where it is located.
          bigval=abs(c(i,i))
          irow=i
          jcol=i
          DO 85 j=i,neq
            DO 80 k=i,neq
            IF (abs(c(k,j)).le.bigval) GO TO 80
            bigval=abs(c(k,j))
            irow=k
            jcol=j
   80     CONTINUE
   85     CONTINUE
c Record the row interchanges in IWORK(IOFF+1:IOFF+NEQ-1)
c Record the column interchanges in IWORK(IOFF+NEQ+1:IOFF+2*NEQ)
          iwork(i+ioff)=irow
          iwork(i+neq+ioff)=jcol
c Interchange this column if necessary.  Then make a rank
c test and proceed on to the next phase if partials are
c rank deficient.
          IF (jcol.gt.i) CALL sswap (neq, c(1,i), 1, c(1,jcol), 1)
          IF (irow.gt.i) THEN
            temp=c(i,i)
            c(i,i)=c(irow,i)
            c(irow,i)=temp
          END IF
          IF (abs(c(i,i)).gt.rnktol*sasum(i,c(1,i),1)) THEN
c Compute multipliers and apply column operations to rest of matrix.
            temp=-one/c(i,i)
            CALL sscal (neq-i, temp, c(i+1,i), 1)
            DO 90 j=i+1,neq
              temp=c(irow,j)
c Note that any interchange in a row is done just before
c the column operation.
              IF (irow.gt.i) THEN
                c(irow,j)=c(i,j)
                c(i,j)=temp
              END IF
              CALL saxpy (neq-i, temp, c(i+1,i), 1, c(i+1,j), 1)
   90       CONTINUE
c Apply to the right-hand side, f(t,y,y').
            temp=rwork(irow+iroff)
            IF (irow.gt.i) THEN
              rwork(irow+iroff)=rwork(i+iroff)
              rwork(i+iroff)=temp
            END IF
            CALL saxpy (neq-i, temp, c(i+1,i), 1, rwork(i+1+iroff), 1)
          ELSE
c Have rank deficiency.  Proceed to index 1 phase.
            irank=i-1
 
c If IRANK == 0 the problem is an algebraic equation.
c This exceptional case is flagged and a return is made.
            GO TO 150
          END IF
  100   CONTINUE
 
        irank=neq
c Have an index 0 problem.  But it may not be consistent.
c Back-solve for the update to y'.
        DO 110 i=neq,1,-1
          temp=-rwork(i+iroff)/c(i,i)
          CALL saxpy (i-1, temp, c(1,i), 1, rwork(iroff+1), 1)
          rwork(i+iroff)=-temp
  110   CONTINUE
c Rearrange solution corresponding to column pivoting.
        DO 120 i=neq,1,-1
          j=iwork(neq+i+ioff)
          IF (j.gt.i) THEN
            temp=rwork(j+iroff)
            rwork(j+iroff)=rwork(i+iroff)
            rwork(i+iroff)=temp
          END IF
  120   CONTINUE
!----------------------------------------------------------------------
      ELSE
c Compute LU factorization of banded matrix.
c The size of leading and trailing dimensions of
c C(*,*) have been checked for adequate size.
 
c If there is room in the last rows of C(*,*) to save
c f_y' then do so.  This anticipates that the problem
c may have index 1 or greater.  Doing this copy saves
c an evaluation of f_y' when the index is 1 or higher.
        IF (ldc.gt.4*ml+2*mu+3) THEN
          DO 130 j=1,neq
            CALL scopy (2*ml+mu+1, c(1,j), 1, c(2*ml+mu+2,j), 1)
  130     CONTINUE
        END IF
c Save the current value of f(t,y,y') in
c RWORK(2*NEQ+IROFF+1:3*NEQ+IROFF). This may be unnecessary if
c the problem has index 0.  If it has index 1 this saves an
c evaluation of f().  The size of RWORK() has been checked.
        CALL scopy (neq, rwork(iroff+1), 1, rwork(2*neq+iroff+1), 1)
        CALL sgbfa (c, ldc, neq, ml, mu, iwork(ioff+1), iflag)
 
c Check for a nearly rank deficient problem.
c IF IFLAG .eq. 0 the code SGBFA did not encounter a zero diagonal.
c A better check is to see if any diagonal terms are small
c compared to a column L_1 norm.
        DO 140 i=1,neq
          IF (abs(c(ml+mu+1,i)).le.sasum(ml+mu+1,c(1,i),1)*rnktol) THEN
            iflag=i-1
c Process the index 1 (or higher) case.  This requires computing
c df/dy and df/dt.
            GO TO 150
          END IF
  140   CONTINUE
        irank=neq
c There is no rank deficiency using the banded matrix
c df/dy'.  Compute the Newton step update and apply it
        CALL sgbsl (c, ldc, neq, ml, mu, iwork(ioff+1),
     &     rwork(iroff+1), 0)
      END IF
!----------------------------------------------------------------------
c Update the values of y' for index 0 problems.
c Loop back to check size of f(t,y,y') again.
      CALL saxpy (neq, -one, rwork(iroff+1), 1, yprime, 1)
 
c This is an iteration counter. It is used
c as a one-time check to allow one iteration before
c checking the norm of f(t,y,y').
      iwork(5)=iwork(5)+1
c Loop back to get another function evaluation
c and check its norm against the user tolerance FTOL.
 
      GO TO 20
 
 
!-----------------------------------------------------------------------
c This is the index 1 phase of solving for y'.
  150 CONTINUE
 
 
      IF (dense) THEN
c Check if rank of f_y' .eq. 0.  If so flag and return.
c This gives the rank only when full pivoting was done.
        IF (irank.eq.0) THEN
          idid=-1
          iwork(1)=5
          GO TO 370
        END IF
c Copy the transformed value of f(t,y,y') and save it.
        CALL scopy (neq, rwork(iroff+1), 1, rwork(neq+iroff+1), 1)
      END IF
c The space in RWORK(:) is used for f_t.
c Get the partial f_t.
      ires=6
c If IRES=1 or 6, clear out the spot where the results will go.
c These are respectively the function and f_t.
      DO 160 i=1,neq
        rwork(i+iroff)=zero
  160 CONTINUE
      IF (irevc.gt.0) THEN
        CALL sdasf (t, y, yprime, rwork(iroff+1), c, ldc, cj, ires,
     &   rwork, iwork)
      ELSE
        info(1)=3
        GO TO 380
      END IF
c Branch back place if reverse communication in use.
  170 CONTINUE
 
c Get the partial f_y.  This is placed in C(1:NEQ,1:NEQ)
c so check that there is room.  Require LDC .ge. 2*NEQ in the
c dense Jacobian case and .gt. 4*ML+2*MU+3 in the banded case.
      IF (dense) THEN
        IF (ldc.lt.2*neq) THEN
          idid=-1
          iwork(1)=8
          info(1)=0
          RETURN
        END IF
      ELSE
        IF (ldc.le.4*ml+2*mu+3) THEN
          idid=-1
          iwork(1)=8
          info(1)=0
          RETURN
        END IF
c In the banded case the second dimension must
c be at least 2*NEQ.
        IF (ltd.lt.2*neq) THEN
          idid=-1
          iwork(1)=15
          info(1)=0
          RETURN
        END IF
      END IF
 
      ires=8
c Clear out rows of the working matrix before computing
c the partials f_y.
      IF (dense) nrows=neq
      IF (banded) nrows=2*ml+mu+1
 
      DO 185 j=1,neq
c Copy the working store for the LU factorization into rows
c NEQ+1:2*NEQ for dense problems.
        IF (dense) THEN
          CALL scopy (neq, c(1,j), 1, c(neq+1,j), 1)
        END IF
c Zero the working rows in C(,) that will be used to
c compute the matrix f_y.
        DO 180 i=1,nrows
        c(i,j)=zero
  180 CONTINUE
  185 CONTINUE
      IF (irevc.gt.0) THEN
        CALL sdasf (t, y, yprime, rwork(iroff+1), c, ldc, cj, ires,
     &   rwork, iwork)
      ELSE
        info(1)=4
        GO TO 380
      END IF
c Branch back place if reverse communication in use.
  190 CONTINUE
 
c Compute the matrix-vector operation: RWORK <- f_t + f_y*y'
c Use a dense matrix-vector product operation.
      IF (dense) THEN
        CALL sgemv ('N', neq, neq, one, c, ldc, yprime, 1, one,
     &   rwork(iroff+1), 1)
      ELSE IF (banded) THEN
        DO 200 j=1,neq
          i1=max(1,j-mu)
          i2=min(neq,j+ml)
          CALL saxpy (i2-i1+1, yprime(j), c(i1-j+ml+mu+1,j), 1,
     &     rwork(iroff+i1), 1)
  200   CONTINUE
      END IF
 
      IF (dense) THEN
c Apply the transformations and column pivoting to f_y.
        DO 220 i=1,irank
c Interchange columns as required.
          jcol=iwork(i+neq+ioff)
          IF (jcol.gt.i) CALL sswap (neq, c(1,i), 1, c(1,jcol), 1)
          irow=iwork(i+ioff)
          DO 210 j=1,neq
            temp=c(irow,j)
            IF (irow.gt.i) THEN
              c(irow,j)=c(i,j)
              c(i,j)=temp
            END IF
            IF (i.lt.neq) CALL saxpy (neq-i, temp, c(neq+i+1,i), 1,
     &        c(i+1,j), 1)
  210     CONTINUE
 
c Apply transformation to the right-hand side, f_t + f_y*y'
          temp=rwork(irow+iroff)
          IF (irow.gt.i) THEN
            rwork(irow+iroff)=rwork(i+iroff)
            rwork(i+iroff)=temp
          END IF
          IF (i.lt.neq) CALL saxpy (neq-i, temp, c(neq+i+1,i), 1,
     &     rwork(i+iroff+1), 1)
  220   CONTINUE
 
c Now use the IRANK by NEQ upper trapazoid of the transformed matrix
c f_y' and the last NEQ-IRANK by NEQ block of the now transformed
c matrix f_y. Eliminate using this special structure to complete the
c factorization.  The working space C(1:NEQ,1:NEQ) is used for
c this last step.  The resulting matrix should be of full rank but
c may not be if the problem has index .gt. 1.
        DO 240 i=1,neq
          DO 230 k=i+1,irank
c Explicitly zero entries to achieve upper trapazoidal form.
            c(k,i)=zero
  230     CONTINUE
c Copy the upper trapazoidal matrix into working space.
         CALL scopy (min(i,irank), c(neq+1,i), 1, c(1,i), 1)
  240    CONTINUE
c Copy the first IRANK entries of the transformed f(t,y,y')
c values into RWORK(:).
        CALL scopy (irank, rwork(neq+iroff+1), 1, rwork(iroff+1), 1)
c Use LINPACK code to compute LU factorization of the combined
c transformed blocks.
        CALL sgefa (c, ldc, neq, iwork(ioff+1), iflag)
c Check for a nearly rank deficient problem.
c IF IFLAG .eq. 0 the code SGEFA did not encounter a zero diagonal.
c A better check is to see if the diagonal terms are small
c compared to a column L_1 norm.
        DO 250 i=1,neq
          IF (abs(c(i,i)).le.sasum(i,c(1,i),1)*rnktol) THEN
            iflag=i-1
            idid=-1
            iwork(1)=4
            GO TO 370
c Branch place for rank deficiency when it is not
c expected for index 1 problems.  This indicates
c a value of the index greater than 1.
          END IF
  250   CONTINUE
c Compute updates of y' using LINPACK code.
        CALL sgesl (c, ldc, neq, iwork(ioff+1), rwork(iroff+1), 0)
c Rearrange solution corresponding to column pivoting.
c Note that only IRANK column permutations are recorded.
        DO 260 i=irank,1,-1
          j=iwork(neq+i+ioff)
          IF (j.gt.i) THEN
            temp=rwork(j+iroff)
            rwork(j+iroff)=rwork(i+iroff)
            rwork(i+iroff)=temp
          END IF
  260   CONTINUE
c Update the values of y' for index 0 problems.
c Loop back to check size of f(t,y,y') again.
      ELSE IF (banded) THEN
c Compute the regularization parameter.  If the user gives
c it then for INFO(14) .gt. 0 it is passed in RWORK(INFO(14)).
c If the user has not passed it then use macheps**(2./3.) for
c the value.
 
        IF (info(14).gt.0) THEN
          regul=rwork(info(14))
        ELSE
c Get machine eps for this precision -
 
          eps=r1mach(4)
          regul=eps**(2.e0/3.e0)
        END IF
c Construct the interleaved banded matrix from the banded
c matrices f_y' and f_y.  This requires use of the same
c data structure that now contains [f_y
c                                   f_y'] to hold the
c ultimate interleaved and regularized banded matrix.
        m=ml+mu+1
        mlp=2*ml+1
        mup=max(2*mu,1)
        mp=mlp+mup+1
        DO 320 j=neq,1,-1
c Copy the value J into columns 2*J-1 and 2*J of ultimate
c matrix banded representation.
 
c Clear space in buffer area of RWORK(:).
          DO 270 i=1,2*neq
            rwork(3*neq+iroff+i)=zero
            rwork(5*neq+iroff+i)=zero
  270     CONTINUE
c Place columns 2*J-1 and 2*J of the interleaved matrix.
c This involves values of column J from f_y, f_y' and the
c regularization parameter, REGUL.  Build the pair of columns
c in the buffer area first.
          i1=max(1,j-mu)
          i2=min(neq,j+ml)
          DO 280 i=i1,i2
            is=i-j+m
c Place values of f_y' into buffer space.
            rwork(3*neq+iroff+2*i-1)=c(is+2*ml+mu+1,j)
            rwork(5*neq+iroff+2*i)=c(is+2*ml+mu+1,j)
c Place values of f_y into buffer space.
            rwork(3*neq+iroff+2*i)=c(is,j)
  280     CONTINUE
c Place value of regularization parameter into buffer space.
          rwork(5*neq+iroff+2*j-1)=regul
c Clear target area for columns 2*J-1 and 2*J of interleaved
c banded matrix.
          DO 290 i=1,2*mlp+mup+1
            c(i,2*j-1)=zero
            c(i,2*j)=zero
  290     CONTINUE
c Copy from buffer area to target columns 2*J, then 2*J-1.
          i1=max(1,2*j-mup)
          i2=min(2*neq,2*j+mlp)
          DO 300 i=i1,i2
            is=i-2*j+mp
            c(is,2*j)=rwork(5*neq+iroff+i)
  300     CONTINUE
 
          i1=max(1,2*j-1-mup)
          i2=min(2*neq,2*j-1+mlp)
          DO 310 i=i1,i2
            is=i-(2*j-1)+mp
            c(is,2*j-1)=rwork(3*neq+iroff+i)
  310     CONTINUE
 
  320   CONTINUE
 
c Interleave values of f and f_t + f_y * y'
c These occupy array positions RWORK(NEQ+IROFF+1:3*NEQ+IROFF)
c after the merge step.
        DO 330 j=1,neq
c Get f component into place
          rwork(neq+iroff+2*j-1)=rwork(2*neq+iroff+j)
c Get f_t + f_y*y' component into alternate place
          rwork(neq+iroff+2*j)=rwork(iroff+j)
  330   CONTINUE
        CALL sgbfa (c, ldc, 2*neq, mlp, mup, iwork(ioff+1), iflag)
c Check for a zero diagonal.  This should not happen for
c index 1 problems.  Declare the problem to have index .gt. 1.
        IF (iflag.gt.0) THEN
          idid=-1
          iwork(1)=4
          GO TO 370
        END IF
        CALL sgbsl (c, ldc, 2*neq, mlp, mup, iwork(ioff+1),
     &     rwork(iroff+neq+1), 0)
        DO 340 j=1,neq
c Get the changes for values of y', now interleaved
          rwork(iroff+j)=rwork(iroff+neq+2*j-1)
  340   CONTINUE
 
      END IF
      CALL saxpy (neq, -one, rwork(iroff+1), 1, yprime, 1)
 
c This is an iteration counter. It is used
c as a one-time check to allow one iteration before
c checking the norm of f(t,y,y').
      iwork(5)=iwork(5)+1
c Loop back to get another function evaluation
c and check its norm against the user tolerance FTOL.
 
      GO TO 20
 
c This block of code checks values of array sizes and other
c parameters.  If there are errors a return is made with
c indicators flagged in IDID and IWORK(1).  It is executed
c just once per usage of sdasls.  It is not re-entered when
c reverse communication is used.
  360 CONTINUE
      if (banded) then
c For banded problems extract the lower and upper bandwidth.
        ml=iwork(1)
        mu=iwork(2)
      end if
      iwork(1)=0
      idid=0
      irank=-1
      IF (neq.le.0) THEN
        iwork(1)=6
        idid=-1
        GO TO 370
      END IF
c An additional requirement is that LDC .ge. 2*NEQ
c for index 1 dense problems.  This is checked as needed.
 
      IF (dense) THEN
        IF (ldc.lt.neq) THEN
          iwork(1)=7
          idid=-1
          GO TO 370
        END IF
      ELSE
 
c Test that bandwidth parameters are non-negative.
        IF (ml.lt.0.or.mu.lt.0) THEN
          iwork(1)=14
          idid=-1
          GO TO 370
        END IF
c The row dimension of the working array must be at least
c 2*ML+MU+1 for banded Jacobians.  If this is an index 1
c problem the value must be larger, 2*(2*ML+1)*2*MU+1.
c This is checked when an index 1 problem is encountered.
        IF (ldc.le.2*ml+mu) THEN
          iwork(1)=7
          idid=-1
          GO TO 370
        END IF
      END IF
c In the dense or banded case the second dimension must
c be at least NEQ.  For banded index 1 problems the value must
c be at least 2*NEQ.  This is checked as needed.
      IF (ltd.lt.neq) THEN
        idid=-1
        iwork(1)=15
        GO TO 370
      END IF
c Check sizes of additional work arrays.
      IF (dense) THEN
        IF (lrw.lt.2*neq+iroff) THEN
          iwork(1)=9
          idid=-1
          GO TO 370
        END IF
      ELSE IF (banded) THEN
        IF (lrw.lt.7*neq+iroff) THEN
          iwork(1)=9
          idid=-1
          GO TO 370
        END IF
      END IF
      IF (liw.lt.2*neq+ioff) THEN
        iwork(1)=10
        idid=-1
        GO TO 370
      END IF
 
      IF (ftol.le.zero) THEN
        iwork(1)=12
        idid=-1
        GO TO 370
      END IF
      IF (rnktol.lt.zero) THEN
        iwork(1)=13
        idid=-1
        GO TO 370
      END IF
c Set INFO(1)=0 so integration can start.  First the problem
c corresponding to IDID .lt. 0 must be fixed.
  370 CONTINUE
      IF (idid.lt.0) THEN
c Error messages can be put here.
        info(1)=0
        RETURN
      END IF
c Save an initial dummy large value for the L_1 norm of f.
c This is updated each iteration and is used to enforce
c a minimal rate of decrease in the L_1 norm of f.
      rwork(2)=r1mach(2)
 
      GO TO 10
 
c This is the fixup and save place for reverse communication
c function requests.  All saved values are stored and the direction
c flag, IRES, is stored for aiding the user in choosing
c which evalution to make.  No error processing here.
  380 CONTINUE
      iwork(3)=ires
      iwork(6)=irank
      iwork(7)=ml
      iwork(8)=mu
      idid=1
      RETURN
c Last statement of routine sdasls
      END
 
