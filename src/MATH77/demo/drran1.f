      program drran1
c>> 1996-05-28 DRRAN1 Krogh Added external statement.
c>> 1992-05-15 CLL  Added <CR> after final END.
c>> 1992-03-13 CLL
c   Demo driver for RAN1 and RANSIZ
c   Also references SRANU, SRANG, SRANUA
c     ------------------------------------------------------------------
      integer I, KSIZE
      external SRANU, SRANG
      real SRANU, SRANG, X(5), Y(5)
c     ------------------------------------------------------------------
      call RANSIZ(KSIZE)
      print'(a//a,i3)',' DRRAN1..  Demo driver for RAN1 and RANSIZ.',
     * ' RANSIZ returns KSIZE = ',KSIZE
      call SRANUA(X, 3)
      X(4) = SRANU()
      X(5) = SRANG()
      call RAN1
      call SRANUA(Y, 3)
      Y(4) = SRANU()
      Y(5) = SRANG()
      do 10 I=1,5
         if(X(I) .ne. Y(I)) then
            print'(/a/a/a/1x,i2,2g14.6)',
     *      ' Reinitialization of the random number generators by use',
     *      ' of RAN1 failed.  The following two floating-point',
     *      ' numbers should have been identical.',I, X(I), Y(I)
            stop
         endif
   10 continue
      print'(/a/a)',
     *      ' Reinitialization of the random number generators by use',
     *      ' of RAN1 succeeded.'
      end
