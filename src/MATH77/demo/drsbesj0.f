c     DRSBESJ0
c>> 2009-10-26 DRSBESJ0 Krogh Moved function calls from print statement.
c>> 1996-05-30 DRSBESJ0 Krogh Added external statement.
c>> 1994-10-19 DRSBESJ0 Krogh  Changes to use M77CON
c>> 1994-09-13 DRSBESJ0 CLL Typed all variables.
c>> 1994-09-01 DRSBESJ0 WVS Moved formats to top for C conversion
c>> 1994-08-09 DRSBESJ0 WVS set up for CHGTYP
c>> 1992-04-29 DRSBESJ0 CAO Replaced '1' in format.
c>> 1987-12-09 DRSBESJ0 Lawson  Initial Code.
c--S replaces "?": ?BESJ0, ?BESJ1, ?BESY0, ?BESY1, DR?BESJ0
c     Demonstration driver for Bessel functions.
c
c     Z = (PI / 2) * X * ( J1(X)*Y0(X) - J0(X)*Y1(X) ) - 1.0
c
      integer IX
      real             BEJ0, BEJ1, BEY0, BEY1
      external SBESJ0, SBESJ1, SBESY0, SBESY1
      real             SBESJ0, SBESJ1, SBESY0, SBESY1, PI2, X, Z
c
      data PI2 / 1.5707963267948966192313216E0 /
c
  100 format (' ',2X,A1,7X,A5,8X,A5,8X,A5,8X,A5,8X,A1/3X,'-',7X,
     *        5('-'),8X,5('-'),8X,5('-'),8X,5('-'),8X,'-')
  150 format (' ',F4.1,1X,2(F12.7,1X),3X,'-INFINITY',4X,'-INFINITY')
  200 format (' ',F4.1,1X,4(F12.7,1X),1X,G9.2)
c
      print 100, 'X','J0(X)','J1(X)','Y0(X)','Y1(X)','Z'
c
      X = 0.E0
      BEJ0 = SBESJ0(X)
      BEJ1 = SBESJ1(X)
      print 150, X, BEJ0, BEJ1
c
      do 250 IX = 5, 50, 5
        X = IX / 10.E0
        BEJ0 = SBESJ0(X)
        BEJ1 = SBESJ1(X)
        BEY0 = SBESY0(X)
        BEY1 = SBESY1(X)
        Z = PI2 * X * ( BEJ1*BEY0  - BEJ0*BEY1 ) - 1.0e0
        print 200, X,BEJ0,BEJ1,BEY0,BEY1,Z
  250 continue
      stop
c
      end
 
