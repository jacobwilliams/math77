      program P1603
c>> 1997-10-08 Krogh Generate plots for ch16-03.tex
      real XSIZEA, YSIZEA, XSIZEB, YSIZEB
      parameter (XSIZEA=2.7E0, YSIZEA=.75E0, XSIZEB=2.4E0, YSIZEB=.75E0)
      real DX, DXLIN, DXSYM, DY, OPTA(210), OPTB(157), X, Y
      real SPACES(14), DASHES(14), DOTS(14)
      integer I, J, K
      character COPTB*172
      real PLOUNI,PLOLIN,PLOBOR,PLOXMX,PLOYMX,PLOSY1,PLOTXT,PLOPLI
      parameter (PLOUNI=1.E0, PLOLIN=3.E0, PLOBOR=4.E0, PLOXMX=8.E0,
     1   PLOYMX=9.E0, PLOSY1=11.E0, PLOTXT=14.E0, PLOPLI=22.E0)
c
      data OPTA / 0.E0, PLOUNI, 0.E0, PLOBOR, 1234E0, 0.E0, 0.E0,
     1   PLOXMX, 0.E0, XSIZEA, PLOYMX, 0.E0, YSIZEA, 197*0.E0 /
c
      data SPACES / 1.E0, 1.E0, 2.E0, 2.E0, 2.E0, 3.E0, 3.E0, 3.E0,
     1  3.E0, 4.E0, 4.E0, 4.E0, 4.E0, 4.E0 /
      data DASHES / 1.E0, 2.E0, 1.E0, 2.E0, 3.E0, 1.E0, 2.E0, 3.E0,
     1  4.E0, 1.E0, 3.E0, 4.E0, 5.E0, 7.E0 /
      data DOTS / 1.E0, 1.5E0, 1.E0, 1.5E0, 2.E0, 1.E0, 2.E0, 2.5E0,
     2  3.E0, 2.E0, 2.5E0, 3.E0, 3.5E0, 4.E0 /
c
      data OPTB / 0.E0, PLOUNI, 0.E0, PLOBOR, 1234E0, 1.E0, 0.E0,
     1   PLOXMX, 0.E0, XSIZEB, PLOYMX, 0.E0, YSIZEB,
     2   PLOPLI, 3*0.E0, YSIZEB, PLOPLI, 3*0.E0, YSIZEB,
     3   PLOPLI, 3*0.E0, YSIZEB, 64*0.E0,
     4   3*0.E0, 603.E0, 3*0.E0, 203.E0, 3*0.E0, 3.E0, 3*0.E0, 403.E0,
     5   3*0.E0, 404.E0, 3*0.E0, 4.E0, 3*0.E0, 14.E0, 3*0.E0, 414.E0,
     6   3*0.E0, 215.E0, 3*0.E0, 255.E0, 3*0.E0, 426.E0, 3*0.E0, 0.E0,
     7   3*0.E0, 9215.E0, 3*0.E0, 9603.E0, 3*0.E0, 9004.E0,
     8   3*0.E0, 9000.E0, 0.E0 /
      data COPTB /
     1'F{pl16-03b.tex}[cr]{603}[cr]{203}[cr]{3}[cr]{403}[cr]{404}
     2 [cr]{4}[cr]{14}[cr]{414}[cr]{215}[cr]{255}[cr]{426}[cr]{0}
     3 [cr]{9215}[cr]{9603}[cr]{9004}[cr]{9000}' /
c
      DX = .195
      DXSYM = .05
      X = 0.E0
      do 40 J = 1, 14
         K = 14 * J
         OPTA(K) = PLOLIN
         OPTA(K+1) = 1.E5 * SPACES(J) + 1.E3 * DASHES(J) + 1.E0
         OPTA(K+2) = PLOPLI
         OPTA(K+3) = X
         OPTA(K+5) = X
         OPTA(K+6) = YSIZEA
         OPTA(K+7) = PLOLIN
         OPTA(K+8) = 1.E5 * SPACES(J) + 1.E4 * DOTS(J) + 4.E0
         OPTA(K+9) = PLOPLI
         OPTA(K+10) = X + DXSYM + (DOTS(J) - 1.E0) / 144.E0
         OPTA(K+12) = OPTA(K+10)
         OPTA(K+13) = YSIZEA
         X = X + DX
40    continue
      call SPLOT(XSIZEA,YSIZEA,OPTA,0.E0,OPTA,OPTA,'F{pl16-03a.tex}Q')
c
      DXSYM = .12E0
      DXLIN = .25E0
      DX = .6E0
      DY = .16E0
      X = .3E0
      do 80 J = 1, 4
         Y = .62E0
         K = 10 + 5*J
         if (J .ne. 4) then
            OPTB(K) = X + DXLIN
            OPTB(K+2) = OPTB(K)
         end if
         do 60 I = 1, 4
            K = 9 + 16*J + 4*I
            OPTB(K) = PLOTXT
            OPTB(K+64)= PLOSY1
            OPTB(K+1) = X
            OPTB(K+65)= X + DXSYM
            OPTB(K+2) = Y
            OPTB(K+66) = Y
            Y = Y - DY
   60    continue
         X = X + DX
   80 continue
      call SPLOT(XSIZEB, YSIZEB, OPTB, 0.E0, OPTB, OPTB, COPTB)
      stop
      end
