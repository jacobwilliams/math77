      SUBROUTINE XERBLA(SRNAME,INFO)
c>> 2009-11-05 XERBLA Krogh Got INFO into the errro messages.
*
*  -- LAPACK auxiliary routine (preliminary version) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER INFO
      CHARACTER*6 SRNAME


c Added by Krogh to use the MATH77 error processor
      integer MEEMES, MERET, MACT(5), IDAT(1)
      parameter (MEEMES=52, MERET=52)
      character TEXT(1)*63
c                       1234567890123456789012
      data TEXT /      ' ** On entry to xxxxxx paramter number $I had an
     1 illegal value.' /
      data MACT / MEEMES, 87, 1, 0, MERET / 
      IDAT(1) = INFO
      TEXT(1)(17:22) = SRNAME
      call MESS(MACT, TEXT, IDAT)
      stop

c And rest of code has cc at start of executable statements

*     ..
*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the LAPACK routines.
*  It is called by an LAPACK routine if an input parameter has an
*  invalid value.  A message is printed and execution stops.
*
*  Installers may consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Arguments
*  =========
*
*  SRNAME  (input) CHARACTER*6
*          The name of the routine which called XERBLA.
*
*  INFO    (input) INTEGER
*          The position of the invalid parameter in the parameter list
*          of the calling routine.
*
*
cc      WRITE (*,FMT=9999) SRNAME,INFO
*
cc      STOP
*
cc 9999 FORMAT (' ** On entry to ',A6,' parameter number ',I2,' had ',
cc     +       'an illegal value')
*
*     End of XERBLA
*
      END
