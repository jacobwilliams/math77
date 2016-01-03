c     program DRERMSG
c>> 1988-11-16 DRERMSG CLL
c
      integer IDELTA
      real SX
      double precision DX
c
      data SX,DX / 1.0E0,2.0D0 /
c
      call ERMSG('AAAA',1,0,'Description of error.',',')
      call SERV1('SX',SX,'.')
      call ERMSG('BBBB',2,0,'Description of 2nd error.','.')
      call SERM1('CCCC',3,0,'Description of 3rd error.','SX',SX,'.')
      call DERM1('DDDD',4,0,'Description of 4th error.','DX',DX,'.')
      do 10 IDELTA = -2, 3
        call ERMSET(IDELTA)
        call IERM1('EEEE',5,0,'Testing ERMSET','IDELTA',IDELTA,'.')
   10 continue
      stop
      end
