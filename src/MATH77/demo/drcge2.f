c     program DRCGE2
c>> 1992-03-11 DRCGE2 Lawson               
c     Demo driver for CGEFS, CGEFA, CGESLT, CGED, CGEI.
c     Solution of square nonsingular system of linear equations
c     having complex elements.
c     ------------------------------------------------------------------
      integer I, INFO, J, NMAX
      parameter (NMAX=3)
      complex   A(3,3), AH(3,3), B(3,1), C(3,1), DET(2), WORK(3)
      integer   IPVT(NMAX)
      data (A(1,J),J=1,3) / (0.579e0,0.735e0), (0.394e0,0.537e0),
     *                      (0.915e0,0.143e0) /
      data (A(2,J),J=1,3) / (0.795e0,0.430e0), (0.226e0,0.384e0),
     *                      (0.868e0,0.494e0) /
      data (A(3,J),J=1,3) / (0.141e0,0.207e0), (0.329e0,0.635e0),
     *                      (0.286e0,0.971e0) /
      data (B(I,1),I=1,3) / (-2.405e0,-4.735e0), (3.87e0,-3.08e0),
     *                      (4.231e0,-0.213e0) /
      data (C(I,1),I=1,3) / (-2.405e0,-4.735e0), (3.87e0,-3.08e0),
     *                      (4.231e0,-0.213e0) /
c     ------------------------------------------------------------------
      print'(a)',
     * ' DRCGE2..  Demo driver for CGEFS, CGEFA, CGESLT, CGED, CGEI.'
c
c                            Store Hermitian transpose of A() into AH()
c
      do 30 I = 1,3
         do 20 J = 1,3
            AH(I,J) = conjg(A(J,I))
   20    continue
   30 continue
c
      call CGEFS(A,NMAX,NMAX,B,NMAX,1,IPVT,INFO)
      print'(a)',' ',
     * ' True solution of A*x = b is (2,4), (-5,5), (3,-9)',
     * ' Computed solution:'
      do 10 I = 1,3
        print '(1x,a,i1,a,f10.6,'','',f10.6,a)',
     *        'X(', I,') = (', B(I,1),')'
  10  continue
c
      call CGEFA( AH, NMAX, NMAX, IPVT, INFO)
      call CGESLT(AH, NMAX, NMAX, IPVT, C)
      print'(a)',' ', ' Set D = A**h and solve (D**h)*x = b.',
     * ' Expect same solution.', ' Computed solution:'
      do 40 I = 1,3
        print '(1x,a,i1,a,f10.6,'','',f10.6,a)',
     *        'X(', I,') = (', C(I,1),')'
  40  continue
c 
      call CGED(A, NMAX, NMAX, IPVT, DET)
      print'(/a/a,f10.6,'','',f10.6,a/a,f10.6,'','',f10.6,a)',
     *   ' Determinant of A is DET(1) * 10**DET(2) where',
     *   '    DET(1) = (',DET(1),') and',
     *   '    DET(2) = (',DET(2),')'
c 
      call CGEI(A, NMAX, NMAX, IPVT, WORK)
      print'(/a/
     *   ('' ('',f10.6,'','',f10.6,'')  ('',
     *           f10.6,'','',f10.6,'')  ('',
     *           f10.6,'','',f10.6,'')''))',
     *  ' Inverse of A:', ((A(I,J),J = 1,3),I=1,3)
      end
