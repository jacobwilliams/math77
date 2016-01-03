      SUBROUTINE  SDASGH(EST,ESTOLD,RATIO,H,HNEW,TK)
c Copyright (c) 2006, Math a la Carte, Inc.
c>> 2003-03-06 sdasgh Hanson installed Soderlind smoothing logic
C     This is Soderlind's 'H211B4' smoothing method.
C     It is  his preferred choice and it is our only choice.
c--S replaces "?": ?dasgh
 
      REAL             EST, ESTOLD, RATIO, H, HNEW, TK
      REAL             B1, B2, A2, DKAPPA, RHO
 
      B1 = 0.25E0/TK
      B2 = 0.25E0/TK
      A2 = 0.25E0
      DKAPPA = 1.E0
 
      RATIO =(2.0E0*EST+1.0E-8)**(-B1)*
     *     (2.0E0*ESTOLD+1.0E-8)**(-B2)*
     *     RATIO**(-A2)
 
      RHO = 1.0E0 + DKAPPA*ATAN((RATIO-1.0E0)/DKAPPA)
 
      HNEW = H*RHO
 
      RETURN
      END
 
