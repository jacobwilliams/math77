c     DRDCPLTK
c>> 2001-06-17 DRDCPLTK Krogh Changed T computation.
c>> 1996-05-30 DRDCPLTK Krogh Added external statement.
c>> 1994-10-19 DRDCPLTK Krogh  Changes to use M77CON
c>> 1994-09-01 DRDCPLTK WVS Moved formats to top for C conversion
c>> 1994-08-09 DRDCPLTK WVS set up for CHGTYP
c>> 1992-04-29 DRDCPLTK CAO Replaced '1' in format.
c>> 1991-11-19 DRDCPLTK CLL
c>> 1987-12-09 DRDCPLTK Lawson  Initial Code.
c--D replaces "?": DR?CPLTK, ?CPLTK, ?CPLTE
c
c     DEMONSTRATION DRIVER FOR ELLIPTIC INTEGRALS.
c
c     EVALUATE THE LEGENDRE'S RELATION:
c     Z = PI/2 - (K*E1 + K1*E - K*K1) = 0
c
      external D1MACH, DCPLTK, DCPLTE
      double precision D1MACH, DCPLTK, DCPLTE
      double precision EM(6),K,K1,E,E1, ONE
      double precision PI2, T, TPRIME, Z, ZERO
      integer I
      data PI2 / 1.5707963267948966192313217D0 /
      data EM / 0.001D0, .2D0, .4D0, .6D0, .8D0, .999D0 /
      data ZERO,ONE / 0.D0, 1.D0 /
c
  200 format(5X,A2,9X,A10,7X,A10,8X,A1/' ')
  300 format(2X,F6.3,2X,F15.8,2X,F15.8,3X,G10.2)
  400 format(3X,A1,6X,F15.8,2X,F15.8,3X,G10.2)
  500 format(2X,F6.3,9X,A8,2X,F15.8,3X,G10.2)
  600 format(/'  TPRIME = Machine epsilon =',E10.2)
  700 format('  T = 1. - TPRIME')
c
      TPRIME = D1MACH(4)
      T = ONE - TPRIME
c
      print 200,'EM','DCPLTK(EM)','DCPLTE(EM)','Z'
      print 300,ZERO,DCPLTK(ZERO),DCPLTE(ZERO)
c
      do 800 I = 1, 6
        K  = DCPLTK(EM(I))
        K1 = DCPLTK(1-EM(I))
        E  = DCPLTE(EM(I))
        E1 = DCPLTE(1-EM(I))
        Z  = PI2 - (K*E1 + K1*E - K*K1)
        print 300, EM(I), K, E, Z
  800 continue
      K  = DCPLTK( T )
      K1 = DCPLTK( TPRIME )
      E  = DCPLTE( T )
      E1 = DCPLTE( TPRIME )
      Z  = PI2 - (K*E1 + K1*E - K*K1)
      print 400, 'T', K, E, Z
      print 500, ONE, 'INFINITY', DCPLTE(ONE)
      print 600, TPRIME
      print 700
c
      end
