c     DRSCPLTK
c>> 2001-06-17 DRSCPLTK Krogh Changed T computation.
c>> 1996-05-30 DRSCPLTK Krogh Added external statement.
c>> 1994-10-19 DRSCPLTK Krogh  Changes to use M77CON
c>> 1994-09-01 DRSCPLTK WVS Moved formats to top for C conversion
c>> 1994-08-09 DRSCPLTK WVS set up for CHGTYP
c>> 1992-04-29 DRSCPLTK CAO Replaced '1' in format.
c>> 1991-11-19 DRSCPLTK CLL
c>> 1987-12-09 DRSCPLTK Lawson  Initial Code.
c--S replaces "?": DR?CPLTK, ?CPLTK, ?CPLTE
c
c     DEMONSTRATION DRIVER FOR ELLIPTIC INTEGRALS.
c
c     EVALUATE THE LEGENDRE'S RELATION:
c     Z = PI/2 - (K*E1 + K1*E - K*K1) = 0
c
      external R1MACH, SCPLTK, SCPLTE
      real             R1MACH, SCPLTK, SCPLTE
      real             EM(6),K,K1,E,E1, ONE
      real             PI2, T, TPRIME, Z, ZERO
      integer I
      data PI2 / 1.5707963267948966192313217E0 /
      data EM / 0.001E0, .2E0, .4E0, .6E0, .8E0, .999E0 /
      data ZERO,ONE / 0.E0, 1.E0 /
c
  200 format(5X,A2,9X,A10,7X,A10,8X,A1/' ')
  300 format(2X,F6.3,2X,F15.8,2X,F15.8,3X,G10.2)
  400 format(3X,A1,6X,F15.8,2X,F15.8,3X,G10.2)
  500 format(2X,F6.3,9X,A8,2X,F15.8,3X,G10.2)
  600 format(/'  TPRIME = Machine epsilon =',E10.2)
  700 format('  T = 1. - TPRIME')
c
      TPRIME = R1MACH(4)
      T = ONE - TPRIME
c
      print 200,'EM','SCPLTK(EM)','SCPLTE(EM)','Z'
      print 300,ZERO,SCPLTK(ZERO),SCPLTE(ZERO)
c
      do 800 I = 1, 6
        K  = SCPLTK(EM(I))
        K1 = SCPLTK(1-EM(I))
        E  = SCPLTE(EM(I))
        E1 = SCPLTE(1-EM(I))
        Z  = PI2 - (K*E1 + K1*E - K*K1)
        print 300, EM(I), K, E, Z
  800 continue
      K  = SCPLTK( T )
      K1 = SCPLTK( TPRIME )
      E  = SCPLTE( T )
      E1 = SCPLTE( TPRIME )
      Z  = PI2 - (K*E1 + K1*E - K*K1)
      print 400, 'T', K, E, Z
      print 500, ONE, 'INFINITY', SCPLTE(ONE)
      print 600, TPRIME
      print 700
c
      end
