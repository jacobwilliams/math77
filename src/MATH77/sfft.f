      SUBROUTINE SFFT(AR, AI, S)
c>> 1997-03-28 SFFT  Krogh  KEDIM increased, removed assigned go to's.
C>> 1994-11-11 SFFT  Krogh  Declared all vars.
c>> 1994-10-20 SFFT  Krogh  Changes to use M77CON
c>> 1989-08-15 SFFT  FTK -- Parameter constants given to more precision.
c>> 1989-04-21 SFFT  FTK & CLL
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
c
c     This subroutine computes one dimensional complex Fourier
c     transforms using the Cooley-Tukey algorithm. It is used by a
c     number of subroutines which compute Fourier transforms.
c
c     Programmed by Fred T. Krogh at the Jet Propulsion Laboratory,
c     Pasadena, Calif.   August 1, 1969.
c     Revised by Krogh at JPL -- January 19, 1988 -- For portability
c
      REAL             AR(*), AI(*), S(*)
c     Minimum dimensions are AR(ILAST), AI(ILAST), S(NT-1), where ILAST
c     and NT are defined in the common block below.
c
c AR and AI give the arrays used to hold the real and imaginary parts of
c     the Fourier coefficients and data.
c S   contains the sine table required in the calculations.
c
c     -----------------------------------------------------------------
c                Notes on COMMON, PARAMETER's, and local variables
c
c     SPI4 = SIN(PI/4) = .5 * SQRT(2)
c     PI4 = PI / 4
c     THE DIMENSION OF KE MUST BE AS LARGE AS THE MAXIMUM VALUE
c     PERMITTED FOR MM.
c
c     WHERE
c     NEEDST= .TRUE. if the sine table must be computed.
c     MT    = base 2 log(NT)
c     NT    = number of entries in the sine table
c     MM    = base 2 log(number of complex fourier coefficients)
c     KS    = distance in memory between successive points.  The i-th
c             coefficient, a(i) is given by AR((I+1)*KS)+AI((I+1)*KS)*
c             sqrt(-1), i=0, 1, ..., (2**MM)-1.
c     ILAST = KS * 2**MM
c     KE(L) = KS * 2**(MM-L), L=1, 2, ..., MM
c     -----------------------------------------------------------------
c--S replaces "?": ?FFT, C?FFTC
c     -----------------------------------------------------------------
      INTEGER I, I1, I2, I3, IJ
      INTEGER J, JDIF, JI, JI2, JJ, JR
      INTEGER K, KSI
      INTEGER L, L1, L4, LJ, LL
      INTEGER MTC

      REAL             HALF, PI4, SPI4
      REAL             T, T1, T1I, T2, T2I, T3, T3I, THETA
      REAL             TI, TP, TP1, TP1I, TPI
      REAL             WI1, WI2, WI3, WR1, WR2, WR3
      LOGICAL SPCASE

      PARAMETER (SPI4 = .70710 67811 86547 52440 08443 62104 8490E0)
      PARAMETER (PI4 = .78539 81633 97448 30961 56608 45819 8757E0)
      PARAMETER (HALF = .5E0)

c Common variables
      INTEGER KEDIM
      PARAMETER (KEDIM=30)
      LOGICAL NEEDST
      INTEGER MT, NT, MM, KS, ILAST, KE(KEDIM), KEE(KEDIM+1)
c Note that KEE(1) is equivalent to ILAST.
      EQUIVALENCE (KE(1), KEE(2))
      COMMON /CSFFTC/ NEEDST, MT, NT, MM, KS, ILAST, KE
      SAVE /CSFFTC/
c     -----------------------------------------------------------------
c
      if (NEEDST) then
c                      Compute the sine table
         NEEDST = .FALSE.
         MTC = MT
         NT = 2**MTC
         IF (MTC .GT. 0) THEN
c                            SET FOR L=1
            J = NT
            JJ = J / 2
            S(JJ) = SPI4
            IF (MTC .GT. 1) THEN
               THETA = PI4
               DO 20 L = 2, MTC
                  THETA = HALF * THETA
                  K = J
                  J = JJ
                  JJ = JJ / 2
c                       At this point J = 2**(MT-L+1) and JJ = 2**(MT-L)
                  S(JJ) = SIN(THETA)
                  L1 = NT - JJ
                  S(L1) = COS(THETA)
                  LL = NT - K
                  IF (LL .GE. J) THEN
                     DO 10 I = J, LL, J
                        I1 = NT - I
                        I2 = I + JJ
                        S(I2) = S(I) * S(L1) + S(JJ) * S(I1)
   10                CONTINUE
                  END IF
   20          CONTINUE
            END IF
         END IF
      ELSE
c                      Compute the transform
c                           Scramble A
c
         IJ = 1
         JI = 1
         L1 = KS
         IF (MM .GT. 1) THEN
   30       IJ = IJ + L1
            DO 40 I = 1, MM
               JI = JI + KE(I)
               KE(I) = -KE(I)
               IF (KE(I) .LT. 0) THEN
                  IF (IJ .LT. JI) THEN
c                       INTERCHANGE THE IJ-TH COEFFICIENT WITH THE JI-TH
                     T = AR(IJ)
                     AR(IJ) = AR(JI)
                     AR(JI) = T
                     T = AI(IJ)
                     AI(IJ) = AI(JI)
                     AI(JI) = T
                  END IF
                  GO TO 30
               END IF
   40       CONTINUE
         END IF
c                          END OF SCRAMBLING A
         JDIF = NT
         IF (MOD(MM, 2) .NE. 0) THEN
c                    SPECIAL CASE -- L = 1,  MM ODD  (RADIX 2 ALGORITHM)
            L1 = L1 + L1
            DO 50 I = 1, ILAST, L1
               KSI = I + KS
               T = AR(I)
               AR(I) = T + AR(KSI)
               AR(KSI) = T - AR(KSI)
               T = AI(I)
               AI(I) = T + AI(KSI)
               AI(KSI) = T - AI(KSI)
   50       CONTINUE
            JDIF = JDIF / 2
         END IF
c
         DO 140 L = 2, MM, 2
            L4 = 4 * L1
            LJ = L1 / 2
            SPCASE = .TRUE.
            J = 0
            JI = 0
c
c           START OF I LOOP  (RADIX 4 ALGORITHM)
   60       IJ = J + 1
            DO 120 I = IJ, ILAST, L4
               I1 = I + L1
               I2 = I1 + L1
               I3 = I2 + L1
               if (SPCASE) then
                  if (J .eq. 0) then
c                                   SPECIAL CASE -- J = 0
                     T = AR(I) + AR(I1)
                     T1 = AR(I) - AR(I1)
                     TI = AI(I) + AI(I1)
                     T1I = AI(I) - AI(I1)
                     T2 = AR(I2) + AR(I3)
                     T3 = AR(I2) - AR(I3)
                     T2I = AI(I2) + AI(I3)
                     T3I = AI(I2) - AI(I3)
                     GO TO 110
                  end if
c                                   SPECIAL CASE -- J = L1 / 2
                  T2 = SPI4 * AR(I2)
                  T2I = SPI4 * AI(I2)
                  T3 = SPI4 * AR(I3)
                  T3I = SPI4 * AI(I3)
                  TP = T2 - T2I
                  TPI = T2 + T2I
                  TP1 = -T3 - T3I
                  TP1I = T3 - T3I
                  T = AR(I) - AI(I1)
                  T1 = AR(I) + AI(I1)
                  TI = AI(I) + AR(I1)
                  T1I = AI(I) - AR(I1)
               else
c
c              USUAL CASE -- J .NE. 0  AND  J .NE. L1 / 2
c
c              WRK AND WIK (K = 1, 2, 3) ARE THE REAL AND IMAGINARY PART
c              RESP. OF EXP(SQRT(-1) * PI * K*(2**(-L-MOD(MM, 2)))*J/KS)
c                             =EXP(SQRT(-1) * PI * K * (J / (4 * L1)))
c
                  T2 = WR2 * AR(I1) - WI2 * AI(I1)
                  T2I = WI2 * AR(I1) + WR2 * AI(I1)
                  T = AR(I) + T2
                  T1 = AR(I) - T2
                  TI = AI(I) + T2I
                  T1I = AI(I) - T2I
                  TP = WR1 * AR(I2) - WI1 * AI(I2)
                  TPI = WI1 * AR(I2) + WR1 * AI(I2)
                  TP1 = WR3 * AR(I3) - WI3 * AI(I3)
                  TP1I = WI3 * AR(I3) + WR3 * AI(I3)
               end if
               T2 = TP + TP1
               T3 = TP - TP1
               T2I = TPI + TP1I
               T3I = TPI - TP1I
  110          AR(I) = T + T2
               AI(I) = TI + T2I
               AR(I1) = T1 - T3I
               AI(I1) = T1I + T3
               AR(I2) = T - T2
               AI(I2) = TI - T2I
               AR(I3) = T1 + T3I
               AI(I3) = T1I - T3
  120       CONTINUE
c           END OF I LOOP
c
            IF (J .LE. LJ) THEN
               IF (SPCASE) THEN
                  IF (J .NE. 0) go to 130
                  J = KS
                  SPCASE = .FALSE.
               ELSE
                  J = L1 - J
c                 COMPUTE WR-S AND WI-S FOR J REPLACED BY L1 - J
                  T = WI1
                  WI1 = WR1
                  WR1 = T
                  WR2 = -WR2
                  T = -WI3
                  WI3 = -WR3
                  WR3 = T
                  GO TO 60
               END IF
            ELSE
               J = L1 - J + KS
            END IF
            IF (J .LT. LJ) THEN
c                              COMPUTE WR-S AND WI-S
               JI = JI + JDIF
               JR = NT - JI
               WR1 = S(JR)
               WI1 = S(JI)
               JI2 = JI + JI
               WI2 = S(JI2)
               JR = NT - JI2
               WR2 = S(JR)
               JI2 = JI + JI2
               IF (JI2 .LE. NT) THEN
                  JR = NT - JI2
                  WR3 = S(JR)
                  WI3 = S(JI2)
                  GO TO 60
               END IF
               JR = JI2 - NT
               JI2 = NT - JR
               WI3 = S(JI2)
               WR3 = -S(JR)
               GO TO 60
            ELSE IF (J .EQ. LJ) THEN
               SPCASE = .TRUE.
               GO TO 60
            END IF
c           END OF COMPUTING WR-S AND WI-S
c
  130       L1 = L4
            JDIF = JDIF / 4
  140    CONTINUE
c        END OF L LOOP
      END IF
      RETURN
      END
