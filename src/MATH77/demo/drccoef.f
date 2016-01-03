c     program DRCCOEF
c>> 1994-07-15 CLL
c>> 1987-12-09 DRCCOEF  Lawson  Initial Code.
c     Demo driver for CCOEF
c     C. L. Lawson & S. Chiu, JPL, 1987 Feb 17.
c     ------------------------------------------------------------------
      integer  I, NDEG1, NDEG2
      complex  RT1(3), RT2(2), ZC(4)
c
      data RT1 / (1.E0, 1.E0), (1.E0, -1.E0), (3.E0, 0.E0)/
      data RT2 / (2.E0, 1.E0), (3.E0, 2.E0)/
      data NDEG1, NDEG2 / 3, 2 /
c     ------------------------------------------------------------------
  100 format(1X/1X,A,I3)
  200 format(1X,A/(2(1X,:,'(',1X,G13.6,', ',G13.6,')',1X)))
c
      call CCOEF(NDEG1,RT1,ZC)
      print 100,'Degree   =',NDEG1
      print 200,'Roots    =',RT1
      print 200,'Coeffs   =',(ZC(I),I=1,NDEG1+1)
      print '(/)'
c
      call CCOEF(NDEG2,RT2,ZC)
      print 100,'Degree   =',NDEG2
      print 200,'Roots    =',RT2
      print 200,'Coeffs   =',(ZC(I),I=1,NDEG2+1)
      end
