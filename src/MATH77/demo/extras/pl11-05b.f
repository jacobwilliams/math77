      program P1105B
c>> 1997-10-02 Krogh Generate plots for last four figures in ch11-05.tex
      integer MAXPT
      real RMAXPT, XA, XB, XSIZE, YSIZE
      parameter (MAXPT=101, RMAXPT=101.E0)
      parameter (XA = 0.0E0, XB = 6.0E0)
      parameter (XSIZE=3.2E0, YSIZE=2.5E0)
      real X(MAXPT), Y(MAXPT,2)
      external SSVAL
      real SSVAL
      integer I
      character*29 COPTC, COPTD, COPTE, COPTF
      real OPTC1(28), OPTC2(4), OPTC3(4)
      real OPTF1(28), OPTF2(4), OPTF3(2), PKNOT1(17), PKNOTF(21),
     1   BVPOSC(15), YPTC1(2), YPTC2(3), BCOEFD(8)
c
      real PLOUNI,PLOTXT,PLONY,PLOBOR,PLOSYM,PLOXMX,PLOYMX
      parameter (PLOUNI=1.E0, PLOTXT=14.E0, PLONY=2.E0, PLOBOR=4.E0,
     1  PLOSYM=10.E0, PLOXMX=8.E0, PLOYMX=9.E0)
c
c
      data OPTC1 / 0.E0, PLOBOR, 12.0E0, 1.0E0, 0.E0, PLOBOR, 34.0E0,
     1   0.E0, 0.E0, PLOTXT, -.2E0, 0.E0, 0.E0, PLOTXT, -.2E0, 1.E0,
     2   0.E0, PLOSYM, 80603.E0, PLOUNI, 54.E0,
     3   PLOXMX, 0.E0, 6.E0, PLOYMX, 0.E0, 1.E0, 0.E0 /
      data OPTC2 /0.E0, PLOUNI, 34.E0, 0.E0/
      data OPTC3 / 0.E0, PLOUNI, 30.E0, 0.E0 /
c
      data OPTF1 / 0.E0, PLOBOR, 12.0E0, 1.0E0, 0.E0, PLOBOR, 34.0E0,
     1   0.E0, 0.E0, PLOTXT, -.3E0, 0.E0, 0.E0, PLOTXT, -.3E0, 1.E0,
     2   0.E0, PLOSYM, 80603.E0, PLOUNI, 54.E0,
     3   PLOXMX, 0.E0, 9.E0, PLOYMX, 0.E0, 1.E0, 0.E0 /
      data OPTF2 /0.E0, PLOUNI, 4.E0, 0.E0/
      data OPTF3 / 0.E0, 0.E0 /
c                           111111111122222222223
c                  123456789012345678901234567890
      data COPTC /'F{pl11-05c.tex}[cl]{0}[cl]{1}'/
      data COPTD /'F{pl11-05d.tex}[cl]{0}[cl]{1}'/
      data COPTE /'F{pl11-05e.tex}[cl]{0}[cl]{1}'/
      data COPTF /'F{pl11-05f.tex}[cl]{0}[cl]{1}'/
c
      data PKNOT1 /3*0.E0, 1.E0, 2.E0, 3.E0, 4.E0, 5.E0, 6.E0, 7.E0,
     1   8.E0, 0.E0, 2.E0, 5.E0, 0.E0, 2.E0, 0.E0 /
      data BVPOSC / 9*.3E0, 3*.4E0, 2*.5E0, .6E0 /
      data YPTC1 / 1.E0, 1.E0 /
      data YPTC2 / 0.E0, 1.E0, 0.E0 /
      data BCOEFD / 3*0.E0, 1.E0, 4*0.E0 /
      data PKNOTF / 7*0.E0, 1.E0, 2.E0, 2.E0, 2.E0, 3.E0, 4.E0, 5.E0,
     1    5.E0, 6.E0, 7.E0, 8.E0, 3*9.E0 /
c
c Take care of third figure for Chapter 11-05.
c
      call SPLOT(XSIZE, .5E0, PKNOT1(4), 5, BVPOSC, OPTC1, COPTC)
      call SPLOT(XSIZE, .5E0, PKNOT1(4), 2, YPTC1, OPTC2, 'Q')
      call SPLOT(XSIZE, .5E0, PKNOT1(6), 3, YPTC2, OPTC3, 'Q')
c
c Take care of fourth figure for Chapter 11-05.
c
      call SPLOT(XSIZE, .5E0, PKNOT1(4), 4, BVPOSC, OPTC1, COPTD)
      do 300 I = 1, 22
            X(I) = PKNOT1(4) + real(I - 1) / 7.E0
            Y(I,1) = SSVAL(3, 7, PKNOT1(2), BCOEFD(2), X(I), 0)
  300 continue
      call SPLOT(XSIZE, .5E0, X, 22, Y, OPTF3, 'Q')
c
c Take care of fifth figure for Chapter 11-05.
c
      call SPLOT(XSIZE, .5E0, PKNOT1(4), 5, BVPOSC, OPTC1, COPTE)
      do 400 I = 1, 21
            X(I) = PKNOT1(4) + real(I - 1) / 5.E0
            Y(I,1) = SSVAL(4, 8, PKNOT1(1), BCOEFD, X(I), 0)
  400 continue
      call SPLOT(XSIZE, .5E0, X, 21, Y, OPTF3, 'Q')
c
c Take care of sixth figure for Chapter 11-05.
c
      call SPLOT(XSIZE, .5E0, PKNOT1(3), 15, BVPOSC, OPTF1, COPTF)
c      call SPLOT(XSIZE, .5E0, PKNOT1(3), 9, BVPOSC, OPTF1, COPTF)
      do 520 I = 1, 21
            X(I) = PKNOT1(1) + real(I - 1) / 20.E0
            Y(I,1) = SSVAL(4, 8, PKNOTF(1), BCOEFD, X(I), 0)
  520 continue
      call SPLOT(XSIZE, .5E0, X, 21, Y, OPTF2, 'Q')
c
      do 540 I = 1, 21
            X(I) = PKNOT1(5) + real(I - 1) / 10.E0
            Y(I,1) = SSVAL(4, 8, PKNOTF(6), BCOEFD, X(I), 0)
  540 continue
      call SPLOT(XSIZE, .5E0, X, 21, Y, OPTF2, 'Q')
c
      do 560 I = 1, 22
            X(I) = PKNOT1(8) + real(I - 1) / 7.E0
            Y(I,1) = SSVAL(4, 8, PKNOTF(11), BCOEFD, X(I), 0)
  560 continue
      call SPLOT(XSIZE, .5E0, X, 22, Y, OPTF3, 'Q')
c
      stop
      end
