      subroutine ZQUO(CNUM,CDENOM,CQUOT)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1995-10-30 ZQUO  Krogh   Fixed so M77CON can get S.P. for C conv.
C>> 1987-12-07 ZQUO  Lawson  Initial code.
c--Z replaces "?": ?QUO
C
C     Computes the quotient of two double precision complex numbers.
C     Computes Z = U / V where the data is given as
C        CNUM(1)   = Real part of U
C        CNUM(2)   = Imaginary part of U
C        CDENOM(1) = Real part of V
C        CDENOM(2) = Imaginary part of V
C     Result returned as
C        CQUOT(1)  = Real part of Z
C        CQUOT(2)  = Imaginary part of Z
C
c     If CDENOM = (0,0) this subr will divide by zero.  We assume this
c     will activate a system error stop.
C     C.L.Lawson & S.Chan, JPL, June 3,1986. 2/17/87.
C     ------------------------------------------------------------------
C
      double precision CNUM(2),CDENOM(2),CQUOT(2)
      double precision A1,B1,C1,D1,DENOM, ONE
      parameter(ONE = 1.0D0)
C
      if(abs(CDENOM(1)) .gt. abs(CDENOM(2))) then
         A1 = CNUM(1)/CDENOM(1)
         B1 = CNUM(2)/CDENOM(1)
         D1 = CDENOM(2)/CDENOM(1)
         DENOM = ONE + D1*D1
         CQUOT(1) = (A1 + B1*D1) / DENOM
         CQUOT(2) = (B1 - A1*D1) / DENOM
      else
         A1 = CNUM(1)/CDENOM(2)
         B1 = CNUM(2)/CDENOM(2)
         C1 = CDENOM(1)/CDENOM(2)
         DENOM = ONE + C1*C1
         CQUOT(1) = (A1*C1 + B1) / DENOM
         CQUOT(2) = (B1*C1 - A1) / DENOM
      end if
      return
      end
