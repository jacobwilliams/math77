      SUBROUTINE DJACG(MODE,M,N,Y,F,
     .           FJAC,LDFJAC,YSCALE,FAC,IOPT,
     .           WK,LWK,IWK,LIWK)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2006-04-11 DJACG  R. J. Hanson removed unneeded values saved.      
c>> 2003-07-08 DJACG  R. J. Hanson fixed bug checking FAC(*) < 0.
c>> 2003-07-08 DJACG  R. J. Hanson fixed bug on MODE < 0 or > N.
c>> 2002-06-21 DJACG  R. J. Hanson modified Salane Code
c--D replaces "?": ?JACG

c     Main routine DJACG, computing numerical derivatives.
c     -----------------------------------------------------------------
C
C***BEGIN PROLOGUE  DJACG
C***DATE WRITTEN  860410
C***Date Modified 060411
C***CATEGORY NO.  D4
C***KEYWORDS  NUMERICAL DIFFERENCING, Numerical Jacobians, Numerical
C             Derivatives
C***AUTHOR    SALANE, DOUGLAS E., SANDIA NATIONAL LABORATORIES
C             NUMERICAL MATHEMATICS DIVISION,1642
C             ALBUQUERQUE, NM 87185
C             Interface reworked by Richard J. Hanson, Rice University.
C             Last changes made by Hanson after leaving Rice University.
C
C***PURPOSE  SUBROUTINE DJACG
C
c  Subroutine DJACG uses finite differences to compute the Jacobian for
c  a system of (M) equations in (N) variables. DJACG is designed
c  for use in numerical methods for solving nonlinear problems where a
c  Jacobian is evaluated repeatedly at neighboring arguments.
c  For example in a Gauss-Newton method for solving non-linear least squares
c  problems. DJACG is suitable for applications in which the  Jacobian
c  is a dense rectangular matrix, over- exactly or under-determined. 
C  The interface to the values of equations is with reverse 
c  communication.  The code DJACG saves and restores values between
c  calls.  In particular DJACG is thread-safe.

C  The design allows for computation of derivatives that are a sum of
C  analytic and functional values that are not easily differentiated.
C***DESCRIPTION
C
C  SUBROUTINE PARAMETERS
C
C  MODE.......An integer flag that directs user action with reverse
C             communication.  Enter the routine with MODE=0.
C             DJACG returns with MODE=J when computing column J of
C             the Jacobian.  When MODE=0 on return, the Jacobian
C             has been computed.
C  N..........[in] THE NUMBER OF VARIABLES.
C  M..........[in] THE NUMBER OF EQUATIONS.
C  Y(*).......[inout] AN ARRAY OF DIMENSION N. THE POINT AT WHICH THE
C             Jacobian IS TO BE EVALUATED.
C  F(*).......[in] AN ARRAY OF DIMENSION M. THE EQUATIONS EVALUATED AT
C             THE POINT Y. This must be defined when MODE .eq. 0 or
C             just after the first perturbed column is computed, 
C             MODE .eq. 1.
C  FJAC(*,*)..[inout] AN ARRAY OF SIZE LDFJAC BY N. FJAC
C             CONTAINS THE Jacobian WHICH IS AN M BY N MATRIX.
c             Columns that are accumulated must have the analytic
c             part defined on entry or else be set to zero.
c             Columns that are skipped can be defined either before
c             or after the routine exits.
C  LDFJAC.....[in] THE LEADING DIMENSION OF THE ARRAY FJAC AS DIMENSIONED
C             IN THE CALLING ROUTINE. LDFJAC MUST BE GREATER OR
C             EQUAL TO M.
C  YSCALE(*)..[in] AN ARRAY OF LENGTH N. THE USER CAN PROVIDE
C             REPRESENTATIVE MAGNITUDES FOR Y VALUES IN THE ARRAY
C             YSCALE. THE USER CAN ALSO USE YSCALE TO PROVIDE
C             APPROPRIATE SIGNS FOR THE INCREMENTS.
 
C  FAC(*).....[inout] AN ARRAY OF LENGTH N. FAC(I) CONTAINS THE PERCENTAGE
C             FACTOR FOR DIFFERENCING.
C
c  If FAC(1)=0., the array FAC is set to the    square  root  of
c  machine  unit roundoff on the first call to DJACG.  
c  Unless the user wishes to initialize FAC,
c  the FAC array should not be altered between subsequent  calls
c  to  DJACG.   
c
c   The user  may  provide  FAC  values  if  desired.
C   If the user provides FAC values, FAC(i) should be 
c   set to a value between 0 and 1.  DJACG will not permit  FAC(i)  to
c   be  set  to  a  value  that results in too small an increment.  DJACG
c   ensures that
c                     FACMIN <= FAC(i) <= FACMAX.
C   For further details on how the code chooses FACMIN  and  FACMAX  see
c   the report [2].
c
C
C IOPT(*) [in] An integer array defining the methods used to
C   compute the derivatives.  The default is to use one sided differences.
C   Entries in this array are interpreted as follows.

C [0] Use the current settings for all remaining variables.  The starting
C     setting is to simply use one sided differences.
C [k > 0] Use the current settings for all variables from the last 
C     specified up to and including variable $k$.
C [-1] Set to use  one sided differences.  (Not needed at the 
C     beginning since this is the default state.)
C [-2] Set to use central differences.
C [-3] Set to accumulate the result from whatever type of differences
C     have been specified above into initial values provided in FJAC.  This 
C     must be followed by a number $\geq 0$ as any number $< 0$ will turn
C     this off.
C [-4] Skip variables.  This must be followed by a value $\geq 0$ 
C     as any number $< 0$ will turn this off.


C          Re. state -2:  For variable p, compute numerical derivatives using
C          central divided differences.  This will typically yield more 
C          accuracy than the one-sided differences, but with the expense
C          of an additional function evaluation per variable.
C          The increment used for central differencing
C          is the default T=$macheps^{-1/6}$
C          times the increment used in one-sided differencing.
C          To change this factor for succeeding
C          variables,  assign a new value between calls with MODE .gt. 0
C          in the storage location FAC(MODE).

C          The default value T=$macheps^{-1/6}$ is based on the
C          approximate relation T*FAC(J) =$macheps^{2/3}$.
C          This value is near an optimal choice, under certain conditions
C          on higher derivatives, provided FAC(J) = $macheps^{1/2}$.
C          Sometimes larger or smaller values of T will give more accuracy
C          than the default.

C 
c          For example if column 3 is not to be computed and column 4 is 
c          to be accumulated, then IOPT(1)=2, followed by:
C          IOPT(2)=-4, IOPT(3)=3, IOPT(4)=-3, IOPT(5)=4,
C          IOPT(6)=-1, IOPT(7)=0

c
c
c  wk(*)...[inout]a work array whose dimension is at least LWK.
c  LWK.....[in] the required length of the work array. LWK=3*M+18.
c  iwk(*)..[inout] an integer array whose dimension is at least LIWK.
c             The first 10 positions of IWK contain diagnostics
c             information.
c
c      IWK(1) gives the number of times a function evaluation was computed.
c
c      IWK(2) gives the number of columns in which three  attempts  were
c      made  to  increase  a percentage factor for differencing (i.e.  a
c      component in  the  FAC  array)  but  the  computed  DEL  remained
c      unacceptably  small relative to Y(JCOL) or YSCALE(JCOL).  In such
c      cases the percentage factor is set to the square root of the unit
c      roundoff of the machine.
c
c      IWK(3)  gives the number of columns in which the computed DEL was
c      zero to machine precision because  Y(JCOL)  or  YSCALE(JCOL)  was
c      zero.   In  such  cases DEL is set to the square root of the unit
c      roundoff.
c
c      IWK(4) gives the number of Jacobian columns which  had  to  be
c      recomputed because  the  largest  difference  formed  in the column
c      was close to zero relative to scale, where
c
c                    scale = max(|f (y)|,|f (y+del)|)
c                                  i       i
c
c      and i denotes the row index of  the  largest  difference  in  the
c      column  currently being processed.  IWK(10) gives the last column
c      where this occurred.
c
c      IWK(5) gives the number of columns whose  largest  difference  is
c      close  to  zero  relative  to  scale  after  the  column has been
c      recomputed.
c
c      IWK(6) gives the  number  of  times  scale  information  was  not
c      available  for  use  in  the roundoff and truncation error tests.
c      this occurs when
c
c                    min (|f (y)|,|f (y+del)|) = 0.
c                          i     i
c
c      where i is the index of the largest  difference  for  the  column
c      currently being processed.
c
c      IWK(7) gives the number of times the increment  for  differencing
c      (DEL) was computed and had to be increased because (Y(JCOL)+DEL )
c      -Y(JCOL)) was too small relative to Y(JCOL) or YSCALE(JCOL).
c
c      IWK(8) gives the number of times a component of the FAC array was
c      reduced because changes in function values were large and  excess
c      truncation error was suspected. IWK(9) gives the last column in
c      which this occurred.
c
c      IWK(9) gives the index of the last column where the corresponding
c      component of the FAC array had to be reduced because excessive
c      truncation error was suspected.
c
c      IWK(10) gives the index of the last column where the  differeence
c      was  small  and  the column had to be recomputed with an adjusted
c      increment (see iwk(4)).  the largest derivative  in  this  column
c      may be inaccurate due to excessive roundoff error.
c
c  LIWK.[in] the length of the array iwk. LIWK >= 10+LNTBUF=21.

c Using reverse communication, DJACG will return  control  to
c the  user  each  time  a  function  value  is required.  The user must
c provide the values of the function f evaluated at y  where  y  is  the
c array return by DJACG.  The user must set
c
c              wk(i)  =   i-th component of f evaluated at y
c
c for i = 1,2,...,M.  After wk is assigned, the user must recall DJACG.
c When DJACG returns to the user with
c
c               MODE = 0,
c
c the computation of the Jacobian is complete.
c
c  Roundoff and truncation errors.
c
c Subroutine DJACG takes advantage of the way in which the   Jacobian
c is  evaluated  to  adjust  increments  for  differencing  to control
c roundoff and truncation errors.  The routine  usually (but not always)
c requires  one additional  function evaluation to  compute a column of the
c Jacobian. Also, the routine returns  a variety of  error  diagnostics
c to warn users when computed derivatives may not be accurate.
c
c WARNING:  DJACG does not  guarantee  the  accuracy  of  the  computed
c derivatives.   In  order  to save on function evaluations, heuristic
c tecniques for increment adjustment and safeguarding  increments  are
c used.  These usually work well, but are not guaranteed.
c
c In order to determine the accuracy of computed derivates, users should
c pay attention to the diagnostic array positions IWK(4), IWK(8), IWK(9)
c and IWK(10). Non-zeros in these position indicate
c possible large roundoff or truncation errors. Here is a summary.
c
C IWK(4) NONZERO => ROUNDOFF ERROR.  IWK(10) LAST NOTED COLUMN.
C IWK(8) NONZERO => TRUNCATION ERROR. IWK(9) LAST NOTED COLUMN.
c
c WARNING:   Some  of the diagnostics returned can only be interpreted
c with a detailed knowledge of the routine.   Nevertheless,  they  are
c provided  to  give  users full access to the information produced by
c the subroutine.
c
c***references  (1)D.E. Salane and L. F. Shampine
c                  "An economical and efficient routine for computing
c                   sparse Jacobians", Report no. SAND85-0977,
c                   Sandia National Laboratories, Albuquerque,NM,87185.
c               (2)D.E. Salane
c                  "Adaptive routines for forming Jacobians
c                   numerically", Report no. SAND86-1319, Sandia
c                   National Laboratories, Albuquerque, NM, 87185
c
C***ROUTINES CALLED  D1MACH
C
C  REQUIRED FORTRAN INTRINSIC FUNCTIONS: 
C        ABS,MAX,MIN,SIGN,SQRT
C  REQUIRED Math a la Carte FUNCTIONS: 
C        D1MACH
C
C***END PROLOGUE  DJACG
C      IMPLICIT NONE
      INTEGER MODE, N, M, LDFJAC, LWK, LIWK
      DOUBLE PRECISION Y(N),F(M),FJAC(LDFJAC,*),WK(LWK),
     .     YSCALE(N),FAC(N)

      INTEGER IOPT(*),IWK(LIWK)

C
C SET CONSTANTS AND ALGORITHM PARAMETERS.
C PERT........IS USED IN EXTRAPOLATION TEST.
C FACMAX......FAC(I) <= FACMAX.
C EXPFMN......FAC(I) => FACMIN WHERE FACMIN = U ** EXPFMN.
C NIFAC.......ITERATION BOUND USED IN COMPUTING INCREMENT.

      EXTERNAL D1MACH
      DOUBLE PRECISION D1MACH
      
      DOUBLE PRECISION PERT, FACMAX
      DOUBLE PRECISION USQT, UEGT, UMEGT, UQRT, U3QRT,
     .     U7EGT, FACMIN, SAVY, AY, SGN, CFACT,
     .     DEL, DELM, T1, T2, SAVDEL, DMAX, RDEL, ADIFF,
     .     DIFF, SF, SDF, FJACL, FMJ, DFMJ, RMXFDF, RMNFDF, U
                
      INTEGER NIFAC
      INTEGER I, J, ITRY, IRDEL, IRCMP, IROWMX, IROW
      INTEGER JCOL, NS, STATEA, STATED, STATES
      

      INTEGER LRBUF, LNTBUF
      PARAMETER(LRBUF=18, LNTBUF=11)
      DOUBLE PRECISION RBUF(LRBUF)

      INTEGER INTBUF(LNTBUF)
C
C***FIRST EXECUTABLE STATEMENT  DJACG
      IF(MODE .gt. 0 .and. MODE .le. N) GO TO 90
C Do error checking on some parameters.  A returned value of MODE < 0
C denotes that argument number -MODE has an error condition associated
C with it.
C      SUBROUTINE DJACG(MODE,M,N,Y,F,
C     .          FJAC,LDFJAC,YSCALE,FAC,IOPT,
C     .        WK,LWK,IWK,LIWK)
C     Variables 1-5, 6-10, 11-14
C Must have MODE=0 the first time.
      IF(MODE .lt. 0 .or. MODE .gt. N) THEN
C This says that MODE < 0 or MODE > N is error.
         MODE = 1
         GO TO 1000
      END IF
      MODE=2
C Must have M > 0
      IF(M .le. 0) GO TO 1000
      MODE=3
C Must have N > 0       
      IF(N .le. 0) GO TO 1000
      MODE=7
C Must have LDFJAC .ge. M if N > 1.
      IF(LDFJAC .lt. M .and. N .gt. 1) GO TO 1000

C Must have FAC() >= 0. The value FAC(1)=0.
C skips this test on the rest of FAC(*).
      IF(FAC(1) .ne. 0.) THEN
         MODE=9
         DO 1010 J=1,N
            IF(FAC(J) .lt. 0.) GO TO 1000
 1010    CONTINUE
      END IF
      MODE=10
C Run throught the IOPT() array making sure that the
C state changes and indices make sense.
      NS=1
 1070 CONTINUE
C A positive value .gt. N is an error.          
      IF(IOPT(NS).gt. N) GO TO 1000
C A value of 0 (or N) says to use the present setting for
C the rest of the variables
      IF(IOPT(NS).eq. 0 .or. IOPT(NS) .eq. N) GO TO 1080
C A postitive value < N must be followed by a zero, N, or a
C state change.  
      IF(IOPT(NS).gt. 0) THEN
         NS=NS+1
         GO TO 1070
      END IF
      IF(IOPT(NS) .lt. -4) GO TO 1000
      IF(IOPT(NS) .ge. -2) THEN
         NS=NS+1
         GO TO 1070
      END IF
C An accumulation or skip must be followed by a non-negative index.
      IF(IOPT(NS+1) .lt. 0) GO TO 1000
      NS=NS+1
      GO TO 1070 
 1080 CONTINUE
      MODE=12
C Must have LWK >= 3*M+LRBUF
      IF(LWK .lt. 3*M+LRBUF) GO TO 1000
      MODE=14
C Must have LIWK >= 21
      IF(LIWK .lt. 21) GO TO 1000
C
c Since reverse communication, initialize JCOL only on first call.           
      JCOL=0
C
C COMPUTE APPROPRIATE MACHINE CONSTANTS.
C
      U = D1MACH(4)
      USQT = SQRT(U)

c          UQRT = U**P25
c          U3QRT = U**P75
c          UEGT = U**P125
c          U7EGT = U**P875
c          FACMIN = U**EXPFMN

      UQRT = SQRT(USQT)
      T1=1.D0/USQT
      U3QRT = U/UQRT
      UEGT = SQRT(UQRT)
      UMEGT = 1.D0/UEGT
      
      U7EGT = U*UMEGT
      FACMIN = U3QRT
      
      PERT=2.0D+0
      FACMAX=1.D-1
      
      NIFAC=3

      DO 10 J = 1,LIWK
         IWK(J) = 0
 10   CONTINUE
        
C  The value FAC=0. means that this code wants to initialize.
      IF(FAC(1) .eq. 0.D0) THEN
         DO 20 J = 1,N
            FAC(J) = USQT
 20      CONTINUE
      END IF

C Default to not accumulate:
      STATEA=1
C Default to use one-sided differences.
      STATED=1
      CFACT=0.D0
C Default to not skip computing columns.
      STATES=1
C Index for processing IOPT(*) states and changes.
      NS = 1
      
C
 30   CONTINUE
      JCOL = JCOL + 1
C IF IOPT(NS) .eq. 0, the current states hold for the rest
C of the columns.
 1020 CONTINUE
      IF(IOPT(NS) .ne. 0) THEN
C A state change will occur:
         IF(JCOL   .eq. IOPT(NS)+1) THEN
C This signals a state change.  Namely the last variable
C in the group has been processed and a change is required.
            NS=NS+1
         END IF
         IF(IOPT(NS) .lt. 0) THEN
C Set for one-sided or central differences.
            IF(IOPT(NS) .ge. -2) THEN
               STATED=-IOPT(NS)
               STATEA=1
            ELSE
C Set for accumulation or skipping variables.
               IF(IOPT(NS) .eq. -3) STATEA=2
               IF(IOPT(NS) .eq. -4) THEN
                  STATES=2
                  STATEA=1
               END IF
            END IF
            NS=NS+1
         ELSE 
            GO TO 1030
         END IF
C Still in range for a state, without change to that state.     
         GO TO 1020
      END IF    
 1030 CONTINUE
      IF(STATED .gt. 1) THEN
        
         IF(CFACT .eq. 0D0) THEN

C                    CFACT=U**(-1.D0/6.D0)
C Compute U**(-1/6) using Newton's method;  compute the cube
C root of U**(-1/2).  This is to avoid the ** elmentary function
C which may not be thread-safe or re-entrant. 
            CFACT = UMEGT*UMEGT
            DO 1050 J=1,14
               CFACT=(2.D0*CFACT+T1/(CFACT*CFACT))/3.D0
C Converged, to over 7/8 precision.  Escapes from loop.
               IF(ABS(CFACT*CFACT*CFACT*USQT - 1.D0) .lt. U7EGT)
     .              GO TO 1060
 1050       CONTINUE
 1060       CONTINUE
         END IF
      END IF
C Check if this column is to be skipped.
      IF(STATES .gt. 1) GO TO 170
      IRCMP = 0
      ITRY = 0
C Check if this column is to be accumulated.
      IF(STATEA .gt. 1) THEN
C Save the initial contents of the array column for final accumulation.
         DO 40 IROW=1,M
            WK(IROW+2*M)=FJAC(IROW,JCOL)
 40      CONTINUE
      END IF

C
 50   CONTINUE
      SAVY = Y(JCOL)
C
c  Compute DEL. If DEL is too small increase FAC and recompute
c  DEL. NIFAC attempts are made to increase FAC and find an
c  appropriate DEL. If DEL can't be found in this manner, DEL is computed
c  with FAC set to the square root of the machine precision(=USQT).
c  If DEL is zero to machine precision because T is zero or
c  YSCALE is zero, DEL is set to USQT.
C
      IRDEL = 0
      IF (YSCALE(1) .ne. 0.D0) THEN
         AY = ABS(YSCALE(JCOL))
         
      ELSE
         AY = ABS(Y(JCOL))
      END IF
      SGN=1.D0
      IF(YSCALE(1) .ne. 0.D0)SGN = SIGN(1.D0,YSCALE(JCOL))
      DELM = U7EGT*AY
C
      DO 60 J = 1,NIFAC
         DEL = FAC(JCOL)*AY*SGN
         IF (DEL.EQ.0.D0) THEN
            DEL = USQT*SGN
            IF (ITRY.EQ.0) IWK(3) = IWK(3) + 1
         END IF
         
         T1 = Y(JCOL) + DEL
         DEL = T1 - Y(JCOL)
         IF (ABS(DEL).LT.DELM) THEN
            IF (J.GE.NIFAC) GO TO 60
            IF (IRDEL.EQ.0) THEN
               IRDEL = 1
               IWK(7) = IWK(7) + 1
            END IF
            
            T1 = FAC(JCOL)*UMEGT
            FAC(JCOL) = MIN(T1,FACMAX)
            
         ELSE
            GO TO 70
         END IF
         
 60   CONTINUE
C
      FAC(JCOL) = USQT
      DEL = USQT*AY*SGN
      IWK(2) = IWK(2) + 1
C
 70   CONTINUE
C  See if central divided differences are intended.
      IF(STATED .gt. 1) THEN
         DEL = CFACT*DEL
      END IF

      SAVDEL = DEL
      Y(JCOL) = Y(JCOL) + DEL
      IWK(1) = IWK(1) + 1
C
c  With reverse communication, DJACG returns to calling routine
c  for function values. Afterwards, DJACG transfers control
c  to the statement after the comment - ENTRY POINT FOR REVERSE
C  COMMUNICATION. 

C     Pack up scalars that need saving.  Place in store within
C     WK(*) and IWK(*).
      GO TO 190
 80   CONTINUE
      MODE = JCOL
      RETURN

C  ENTRY  POINT FOR REVERSE COMMUNICATON.

 90   CONTINUE
C     Unpack scalars that needed saving.  
C     Take from WK(*) and IWK(*) where they are stored.
      GO TO 220
 100  CONTINUE
C See if this is the second evaluation for a central difference.
C This is true STATED .gt. 1 .and. ITRY .eq. 1.
C and ITRY = 1.
      RDEL = 1.D0/SAVDEL
      IF(STATED .eq. 1) GO TO 140
C IF ITRY = 1, proceed and compute the central divided difference.        
      IF(ITRY .eq. 1) GO TO 120
C Otherwise ITRY = 0, so an alternate evaluation is needed.
      Y(JCOL)=SAVY-DEL
      DO 110 I=1,M
         WK(I+M)=WK(I)
 110  CONTINUE
      ITRY=1
      IWK(1) = IWK(1) + 1
      GO TO 190

 120  CONTINUE
C Compute the central divided difference.
      RDEL=.5*RDEL
      DO 130 I=1,M
         FJAC(I,JCOL)=RDEL*(WK(I+M)-WK(I))
 130  CONTINUE

C Restore the component of Y() and move on.             
      Y(JCOL) = SAVY
      ITRY=0
      GO TO 170

C Restore the component of Y() and move on.             
 140  CONTINUE
      Y(JCOL) = SAVY

C
c  Compute the Jacobian entries.
c  Use largest elements in a column to determine scaling
c  information for roundoff and truncation error tests.
C
      DMAX = 0.D0
      IROWMX = 1
      
C
      DO 150 IROW = 1,M
         DIFF = WK(IROW) - F(IROW)
         ADIFF = ABS(DIFF)
         IF (ADIFF.GE.DMAX) THEN
            IROWMX = IROW
            DMAX = ADIFF
            SF = F(IROW)
            SDF = WK(IROW)
         END IF
         
         FJACL = DIFF*RDEL
         IF (ITRY.EQ.1) THEN
            WK(IROW) = FJACL
            WK(IROW+M) = FJAC(IROW,JCOL)
         END IF
         
         FJAC(IROW,JCOL) = FJACL
 150  CONTINUE
      
c  If a column is being recomputed (ITRY=1), this section of the
c  code performs an extrapolation test to enable the code to
c  compute small derivatives more accurately.
C
      IF (ITRY.EQ.1) THEN
         T1 = WK(IROWMX+M)
         T2 = WK(IROWMX)*FAC(JCOL)
         IF (ABS(T2).LT.ABS(T1)*PERT) THEN
            T1 = FAC(JCOL)*FAC(JCOL)
            FAC(JCOL) = MAX(T1,FACMIN)
            DO 160 IROW = 1,M
               FJACL = WK(IROW+M)
               FJAC(IROW,JCOL) = FJACL
 160        CONTINUE
         END IF
         
      END IF
C
      FMJ = ABS(SF)
      DFMJ = ABS(SDF)
      RMXFDF = MAX(FMJ,DFMJ)
      RMNFDF = MIN(FMJ,DFMJ)
C
c   If scale information is not available, perform no roundoff
c   or truncation error tests.
C
      IF (RMNFDF.NE.0.D0) THEN
C
c    Test for possible roundoff error (first test)
c    and also for possible serious roundoff error (second test).
C
         IF (DMAX.LE. (U3QRT*RMXFDF)) THEN
            IF (DMAX.LE. (U7EGT*RMXFDF)) THEN
               IF (ITRY.EQ.0) THEN
                  T1 = SQRT(FAC(JCOL))
                  FAC(JCOL) = MIN(T1,FACMAX)
                  IRCMP = 1
                  IWK(4) = IWK(4) + 1
                  IWK(10) = JCOL
                  
               ELSE
                  IWK(5) = IWK(5) + 1
               END IF
               
            ELSE
               T1 = UMEGT*FAC(JCOL)
               FAC(JCOL) = MIN(T1,FACMAX)
            END IF
            
         END IF
C
c     Test for possible truncation error.
C
         IF (DMAX.GT.UQRT*RMXFDF) THEN
            T1 = FAC(JCOL)*UEGT
            FAC(JCOL) = MAX(T1,FACMIN)
            IWK(8) = IWK(8) + 1
            IWK(9) = JCOL
         END IF
         
      ELSE
         IWK(6) = IWK(6) + 1
C     
      END IF
C
c     If serious roundoff error is suspected, recompute the
c     column.
C
C
      IF (IRCMP.EQ.1) THEN
         IRCMP = 0
         ITRY = 1
         GO TO 50
         
      END IF

      ITRY = 0

C Branch to this place if this column is skipped.
 170  CONTINUE
C May have asked for this column to be accumulated.
      IF(STATEA .gt. 1 .and. STATES .eq. 1) THEN
         DO 180 IROW=1,M
            FJAC(IROW,JCOL)=FJAC(IROW,JCOL)+WK(IROW+2*M)
 180     CONTINUE
      END IF

      IF (JCOL.LT.N) GO TO 30
C This is the signal that all Jacobian columns are computed.
      MODE = 0
      RETURN

C Internal procedure to save local data.
C This step is needed to make the code re-entrant and threadsafe.
 190  CONTINUE
      
      RBUF(01)=FACMIN
      RBUF(02)=FACMAX
      RBUF(03)=CFACT
      RBUF(04)=USQT
      RBUF(05)=UEGT
      
      RBUF(06)=UMEGT
      RBUF(07)=UQRT
      RBUF(08)=U3QRT
      RBUF(09)=U7EGT
      RBUF(10)=U
      
      RBUF(11)=CFACT
      RBUF(12)=SAVDEL
      RBUF(13)=PERT   
      RBUF(14)=SAVY
      RBUF(15)=AY
      
      RBUF(16)=SGN
      RBUF(17)=DEL
      RBUF(18)=DELM
C Save data values in calling program unit.  When the code is
C entered again these are extracted from storage the store
C in the calling program.  This eliminates the need to save
C any variables.  

      DO 200 I=1,LRBUF
         WK(I+3*M)=RBUF(I)
 200  CONTINUE
      
      INTBUF(01)=NIFAC
      INTBUF(02)=I
      INTBUF(03)=J
      INTBUF(04)=ITRY
      INTBUF(05)=IRDEL
      
      INTBUF(06)=IRCMP
      INTBUF(07)=JCOL
      INTBUF(08)=NS    
      INTBUF(09)=STATEA
      INTBUF(10)=STATED
      
      INTBUF(11)=STATES


C Save data values in calling program unit.  When the code is
C called again these are extracted.
      DO 210 I =1,LNTBUF
         IWK(I+10)=INTBUF(I)
 210  CONTINUE
      
      GO TO 80
C Internal procedure to unpack saved data.
 220  CONTINUE
C Extract saved data from store in the calling program unit.
      DO 230 I=1,LRBUF
         RBUF(I)= WK(I+3*M)
 230  CONTINUE
  
      FACMIN=RBUF(01)
      FACMAX=RBUF(02)
      CFACT =RBUF(03)
      USQT  =RBUF(04)
      UEGT  =RBUF(05)
      
      UMEGT =RBUF(06)
      UQRT  =RBUF(07)
      U3QRT =RBUF(08)
      U7EGT =RBUF(09)
      U     =RBUF(10)
      
      CFACT =RBUF(11)
      SAVDEL=RBUF(12)
      PERT  =RBUF(13)     
      SAVY  =RBUF(14)
      AY    =RBUF(15)
      
      SGN   =RBUF(16)
      DEL   =RBUF(17)
      DELM  =RBUF(18)
      
C Extract saved data from store in the calling program unit.
      DO 240 I =1,LNTBUF
         INTBUF(I)=IWK(I+10)
 240  CONTINUE
      
      NIFAC =INTBUF(01)
      I     =INTBUF(02)
      J     =INTBUF(03)
      ITRY  =INTBUF(04)
      IRDEL =INTBUF(05)
      
      IRCMP =INTBUF(06)
      JCOL  =INTBUF(07)
      NS    =INTBUF(08)        
      STATEA=INTBUF(09)
      STATED=INTBUF(10)
      
      STATES=INTBUF(11)
      GO TO 100
        
 1000 CONTINUE
C Error return when arguments are invalid.
      MODE=-MODE
      
      END
