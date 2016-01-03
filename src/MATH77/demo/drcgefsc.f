c     program DRCGEFSC
c>> 2001-05-22 DRCGEFSC Krogh Minor change for making .f90 version.
c>> 1992-03-18 CLL Declaring all integers.
c>> 1988-05-11 DRCGEFSC Lawson  Initial Code.
c        Demo driver for CGEFSC.  Solution of square nonsingular system
c     of linear equations having complex elements.
c     S. Y. Chiu, JPL, 1987.  C. L. Lawson, JPL, May 1988.
c     ------------------------------------------------------------------
      integer I, J, NMAX
      parameter (NMAX=3)
      complex   A(3,3), B(3,1), Z(NMAX)
      real      RCOND
      integer   IPVT(NMAX)
      data (A(1,J),J=1,3) / (0.579E0,0.735E0), (0.394E0,0.537E0),
     *                      (0.915E0,0.143E0) /
      data (A(2,J),J=1,3) / (0.795E0,0.430E0), (0.226E0,0.384E0),
     *                      (0.868E0,0.494E0) /
      data (A(3,J),J=1,3) / (0.141E0,0.207E0), (0.329E0,0.635E0),
     *                      (0.286E0,0.971E0) /
      data (B(I,1),I=1,3) / (-2.405E0,-4.735E0), (3.87E0,-3.08E0),
     *                      (4.231E0,-0.213E0) /
c     ------------------------------------------------------------------
      call CGEFSC(A,NMAX,NMAX,B,NMAX,1,IPVT,RCOND,Z)
      print*,'DRCGEFSC..  Demo driver for CGEFSC.'
      print*,'True solution is (2,4), (-5,5), (3,-9)'
      print*,'Computed solution:'
      do 40 I = 1,3
        print 100, I, B(I,1)
  40  continue
      print '(1x/1x,a,f7.4)','RCOND = ', RCOND
  100 format(1x,'X(',I1,') =',' (',g15.8,',',g15.8,')')
      end
