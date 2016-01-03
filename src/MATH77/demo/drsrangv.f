c     program DRSRANGV
c>> 2001-05-22 DRSRANGV Krogh Minor change for making .f90 version.
c>> 1994-10-19 DRSRANGV Krogh  Changes to use M77CON
c>> 1987-12-09 DRSRANGV Lawson  Initial Code.
c--S replaces "?": DR?RANGV, ?RANGV
c        Demonstration driver for the Gaussian random vector generator,
c     SRANGV.
c     C. L. Lawson & S. Y. Chiu, JPL, Apr 1987.
c     ------------------------------------------------------------------
      integer I, J, N, IERR, NMAX, NSAMPL
      parameter(NMAX = 3)
      real             A(NMAX,NMAX), U(NMAX), X(NMAX)
      logical HAVEC
      data (U(  J),J=1,NMAX)/ 1.0E0,  2.0E0,  3.0E0 /
      data (A(1,J),J=1,NMAX)/ 0.05E0, 0.02E0, 0.01E0 /
      data (A(2,J),J=1,NMAX)/ 0.02E0, 0.07E0,-0.03E0 /
      data (A(3,J),J=1,NMAX)/ 0.01E0,-0.03E0, 0.06E0 /
c     ------------------------------------------------------------------
      N = NMAX
      NSAMPL = 20
      HAVEC = .false.
      print'(1x,a/1x)','Pseudorandom vectors computed by SRANGV'
      do 10 I = 1,NSAMPL
         call SRANGV(A, NMAX, N, U, X, HAVEC, IERR)
         if(IERR .ne. 0) then
            print '(1x,i3,i6)',N, IERR
         else
            print '(1x,i3,5x,3f12.3)',I,X
         endif
   10 continue
      stop
      end
