      DOUBLE PRECISION FUNCTION DCPVAL (P,NDEGP,X)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-10-20 DCPVAL Krogh  Changes to use M77CON
C>> 1994-04-20 DCPVAL CLL Edited to make DP & SP files similar.
C>> 1987-12-09 DCPVAL Lawson  Initial code.
c--D replaces "?": ?CPVAL
C
C     C.L.LAWSON,JPL, 1969 DEC 17   MODIFIED 1973 JULY 24
C
C     MODIFIED 1974 NOV 19
C
C     EVALUATE A POLYNOMIAL OF DEGREE NDEGP GIVEN TRANSFORMATION
C     PARAMETERS, P(1) AND P(2), AND COEFFICIENTS RELATIVE TO THE
C     CHEBYSHEV BASIS.
C
C     NDEGP               DEGREE OF POLYNOMIAL
C     (P(I),I=1,NDEGP+3)  PARAMETERS DEFINING THE POLYNOMIAL
C     X                  INPUT ARGUMENT
C             THE POLYNOMIAL'S VALUE AT X IS DEFINED AS FOLLOWS.
C
C                             S = ( X - P(1) ) / P(2)
C
C                DCPVAL=SUM OF P(I+3)*T(I,S) FOR I=0,1,...NDEGP
C
C                             WHERE T(I,S) DENOTES THE CHEBYSHEV
C                             POLYNOMIAL OF DEGREE I EVALUATED AT S .
C
      integer J, NDEGP
      DOUBLE PRECISION P(*),W(3),S,S2,X
      W(1)=0.0D0
      W(2)=0.0D0
C                             TRANSFORM X TO S
      S=(X-P(1))/P(2)
      S2=S+S
C
C                             EVALUATE POLYNOMIAL USING RECURSION
C
      do 30 J = NDEGP+3, 4, -1
          W(3)=W(2)
          W(2)=W(1)
          W(1)=(S2*W(2)-W(3))+P(J)
   30 continue
      DCPVAL=(S*W(1)-W(2))+P(3)
      RETURN
      END
