      SUBROUTINE  DDASGH(EST,ESTOLD,RATIO,H,HNEW,TK)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2003-03-06 ddasgh Hanson installed Soderlind smoothing logic
C     This is Soderlind's 'H211B4' smoothing method.
C     It is  his preferred choice and it is our only choice.
c--D replaces "?": ?dasgh

      DOUBLE PRECISION EST, ESTOLD, RATIO, H, HNEW, TK
      DOUBLE PRECISION B1, B2, A2, DKAPPA, RHO

      B1 = 0.25D0/TK
      B2 = 0.25D0/TK
      A2 = 0.25D0
      DKAPPA = 1.D0
     
      RATIO =(2.0D0*EST+1.0D-8)**(-B1)*
     *     (2.0D0*ESTOLD+1.0D-8)**(-B2)*
     *     RATIO**(-A2)

      RHO = 1.0D0 + DKAPPA*ATAN((RATIO-1.0D0)/DKAPPA)
      
      HNEW = H*RHO
      
      RETURN
      END

