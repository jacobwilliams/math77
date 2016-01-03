      SUBROUTINE DC2BAS(X,NDERIV,ISEG,NEWSEG, B,NB,P)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1994-11-11 DC2BAS Krogh  Declared all vars.
C>> 1994-10-20 DC2BAS Krogh  Changes to use M77CON
C>> 1987-12-09 DC2BAS Lawson Initial code.
c--D replaces "?": ?C2BAS
c
c          Given X in the segment ISEG, this subr evaluates at X the
c     four cubic B-spline basis functions that are not identically
c     zero throughout segment ISEG.
c          This is a reorganization of a set of three subroutines
c     SBAS1, SBAS2, & SBAS3 used in LIB*JPL$ from about 1970 on.
c     The reorganization improves portability and brings
c     conformance to Fortran 77.
c     More features are avalable from a similar subr in a package
c     due to Carl de Boor, however this subr is being preserved
c     for convenience of usage with some other JPL subrs --
c     particularly the size and interpretation of the B() array.
c          C. L. Lawson, JPL, 1984 July 9, July 1987.
c     ------------------------------------------------------------------
c               A R G U M E N T S
c
c     X  [in]  Argument at which basis fcns are to be evaluated.
c           Must satisfy B(ISEG) .le. X .le. B(ISEG+1)
c
c     NDERIV  [in]   = 0, 1, 2, or 3.  Indicates the order of derivative
c              to be computed.  0 selects function value,
c              1 selects 1st derivative, etc.
c
c     ISEG  [in] Index of current segment. Require 1 .le. ISEG .le. NB-1
c               Segment ISEG is the interval [B(ISEG), B(ISEG+1)].
c               The current X must be in this segment.
c
c     NEWSEG  [inout]  = .true. means ISEG is not the same as on
c             previous call.  In this case this subr will set new values
c             into U() and reset NEWSEG = .false.  The user prog must
c             set NEWSEG = .true. on the first call to this subr, and
c             on any subsequent call at which the user prog has set
c             ISEG to a new value.
c
c             = .false. means this subr can assume the values in U()
c             are correct for the current ISEG.
c
c     B()  [in]  Array of knots, including the left and right
c               endpoints.  Contains B(I), I = 1, NB.  Set by user.
c               Not changed by this subr.  Must be strictly increasing,
c               i.e. require B(i) .lt. B(i+1), i = 1, NB-1.
c
c     NB  [in]  Number of knots, including endpoints. The number
c               of degrees of freedom of the set of cubic splines
c               defined over this set of knots is NB+2.
c
c     P()  [out]  Array of length 4 into which this subr will store the
c             values of the 4 basis fcns that are not identically
c             zero over segment ISEG.  The indices of these basis
c             fcns are ISEG, ISEG+1, ISEG+2, & ISEG+3.  The values
c             will be stored in (P(i), i=1,4).
c     ------------------------------------------------------------------
c               IMPORTANT INTERNAL VARIABLES
c
c     U()       Array of length 8 in which this subr saves a scaled
c               record of the knots near the current segment.
c
c     SCALE  Scale factor for abcissas.  Computed as the reciprocal
c            of the average segment length.
c     ------------------------------------------------------------------
c
      integer NDERIV, ISEG, NB
      integer MB, IS, ISM4, IB1, IB2, IB, IU, NDP1, I, J, I2
      double precision U(8), B(NB), P(4), C6, C18, C36, CS
      double precision FLNSEG, H, SCALE, X, XS
      logical NEWSEG
      parameter(C6 = 6.0D0, C18 = 18.0D0, C36 = 36.0D0)
      save  U, SCALE
c     ------------------------------------------------------------------
      MB=NB
c                     The following statement does a type conversion.
      FLNSEG=MB-1
      IF( NEWSEG ) THEN
         NEWSEG = .FALSE.
         SCALE = FLNSEG / (B(NB) - B(1))
         IS=ISEG
         ISM4=IS-4
         IB1=max(1,IS-3)
         IB2=min(MB,IS+4)
C                               COPY PART OF B() INTO U()
             DO 20 IB=IB1,IB2
             IU=IB-ISM4
   20        U(IU)=SCALE*B(IB)
C                               ADJOIN ARTIFICIAL SEGMENTS AT LEFT END
         IU=IB1-ISM4-1
         IF(IU .NE. 0) THEN
            H=U(IU+2)-U(IU+1)
   30       U(IU)=U(IU+1)-H
            IU=IU-1
            IF(IU .GT. 0) GO TO 30
         END IF
C                               ADJOIN ARTIFICIAL SEGMENTS AT RIGHT END
         IU=IB2-ISM4+1
         IF(IU .LE. 8) THEN
            H=U(IU-1)-U(IU-2)
   50       U(IU)=U(IU-1)+H
            IU=IU+1
            IF(IU .LE. 8) GO TO 50
         END IF
      END IF
c     ------------------------------------------------------------------
      NDP1=NDERIV+1
      XS=X*SCALE
      GO TO (70,90,110,130),NDP1
   70     DO 80 I=1,4
   80     P(I)= (XS-U(I))**3
      CS= C6
      GO TO 150
   90     DO 100 I=1,4
  100     P(I)=(XS-U(I))**2
      CS= C18 * SCALE
      GO TO 150
  110     DO 120 I=1,4
  120     P(I)= XS-U(I)
      CS= C36 * SCALE**2
      GO TO 150
  130     DO 140 I=1,4
  140     P(I)= C36
      CS= SCALE**3
  150 CONTINUE
C
C                            COMPUTE FOURTH DIVIDED DIFFERENCES
C
          DO 170 J=1,4
               DO 160 I=1,3
               I2=I+J
  160          P(I)=(P(I+1)-P(I))/(U(I2)-U(I))
  170     P(4)=     -P(4)/(U(J+4)-U(4))
C
          DO 180 I=1,4
  180     P(I)=P(I)*CS
      return
      end
