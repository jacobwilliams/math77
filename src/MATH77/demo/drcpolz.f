c     Program DRCPOLZ
c>> 1994-09-13 DRCPOLZ  CLL
c>> 1992-03-06 DRCPOLZ  CLL
c>> 1987-12-09 DRCPOLZ  Lawson  Initial Code.
c     Demonstration driver for CPOLZ.
c     ------------------------------------------------------------------
      complex  A1(4), A2(6), A3(6), Z(5)
      real H(50)
      integer  I, N1, N2, N3, IERR
c
      data A1 / (1.,0.), (-4.,0.), (1.,0.), (-4.,0.) /
      data A2 / (1.,0.), (-15.,0.), (85.,0.), (-225.,0.),
     *          (274.,0.), (-120.,0.) /
      data A3 / (   1.00e0,  0.000e0),  (  -5.50e0,  -1.50e0),
     *          (   10.0e0,   7.75e0),  (  -7.50e0,  -13.25e0),
     *          (  -1.00e0,   11.0e0),  (   3.00e0,  -1.50e0)/
c     ------------------------------------------------------------------
      N1 = 3
      N2 = 5
      N3 = 5
c
  100 format(' Degree =',I2/' ',
     * 'Coefficients ='/(2(1X,:,'(',1X,f11.6,',',1X,f11.6,2X,')',2X)))
  200 format(' Roots ='/(2(1X,:,'(',1X,f11.6,',',1X,f11.6,2X,')',2X)))
c
      print 100, N1, (A1(I),I=1,N1+1)
      call CPOLZ(A1,N1,Z,H,IERR)
      print 200, (Z(I),I=1,N1)
c
      print'(/)'
      print 100, N2, (A2(I),I=1,N2+1)
      call CPOLZ(A2,N2,Z,H,IERR)
      print 200, (Z(I),I=1,N2)
c
      print'(/)'
      print 100, N3, (A3(I),I=1,N3+1)
      call CPOLZ(A3,N3,Z,H,IERR)
      print 200, (Z(I),I=1,N3)
c
      end
