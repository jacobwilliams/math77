c     program drdran
c>> 2001-07-16 DRDRAN Krogh  Added comma in two fomrats.
c>> 1996-06-19 DRDRAN Krogh  Minor format change for C conversion.
c>> 1994-10-19 DRDRAN Krogh  Changes to use M77CON
c>> 1992-02-24 DRDRAN CLL
c>> 1987-12-10 Original time stamp
c--D replaces "?": DR?RAN, ?RANUA
c++S Default NDIG = 6
c++  Default NDIG = 12
c++  Replace "f15.12" = "f"//NDIG+3//"."//NDIG
c     Reports MODE for host system, and prints a few integers in the
c     integer sequence underlying the pseudorandom number package for
c     the MATH77 library.  These integers should be exactly the
c     same on all host systems.  The listed integers include the
c     smallest and largest in the entire sequence.
c     ------------------------------------------------------------------
      integer I, KASE, KSEED(2,2), MODE
      double precision X(1)
      data KSEED(1,1), KSEED(2,1) / 249979, 65550 /
      data KSEED(1,2), KSEED(2,2) / 437215, 10953 /
c     ------------------------------------------------------------------
      do 20 KASE = 1,2
         print'(1x//4x,''Integer sequence'',10x,''Number returned''/1x)'

         print'(7x,i7,'','',i5)',KSEED(1,KASE),KSEED(2,KASE)
         call RANPUT(KSEED(1,KASE))
         do 10 I = 1,10
            call DRANUA(X,1)
            call RANGET(KSEED(1,KASE))
         print'(7x,i7,'','',i5,10x,f15.12)',KSEED(1,KASE),KSEED(2,KASE),
     *          X(1)
   10    continue
   20 continue
      call RN2(MODE)
      print'(/1x,a,i2)',
     *   'MODE may be 2, 3, or 4.  On the current host it is ',MODE
      stop
      end
