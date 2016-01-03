      subroutine SBI0K0 (x, BI0, BK0, want, status)
c Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
c ALL RIGHTS RESERVED.
c Based on Government Sponsored Research NAS7-03001.
C>> 1998-10-29 SBI0K0 Krogh  Moved external statement up for mangle.
c>> 1996-04-27 SBI0K0 Krogh  Changes to use .C. and C%%.
C>> 1995-11-28 SBI0K0 Krogh  Changes to simplify conversion to C.
C>> 1994-10-20 SBI0K0 Krogh  Changes to use M77CON
C>> 1990-10-18 SBI0K0 WV Snyder JPL Use Cody K0 for small X
C>> 1990-09-25 SBI0K0 WV Snyder JPL Use Fullerton codes from CMLIB
C--S replaces "?": ?BI0K0, ?CSEVL, ?ERM1, ?INITS
c
c     Compute hyperbolic Bessel functions I0 and K0.
c     Approximations for K0(X) on 0 < X < BOUNDK originally produced
c     by W. James Cody, ANL.  Other approximations originally produced
c     by Wayne Fullerton, LASL.
c
c     *****     Formal Arguments     ***********************************
c
c X [in] is the argument at which the functions are to be evaluated.
c BI0, BK0 [out] are the function values.
c WANT [integer,in] indicates the functions to be computed, and their
c                   scaling:
c     ABS(WANT) =
c        1 means compute I0(X)
c        2 means compute K0(X)
c        3 means compute both of I0(X) and K0(X)
c     WANT < 0 means compute EXP(-X)*I0(X) and/or EXP(X)*K0(X).
c     WANT=0 or ABS(WANT) > 3 causes an error message to be produced.
c STATUS [integer,out] indicates the outcome:
c     0 means normal computation,
c     1 means K0(X) is zero due to underflow,
c     < 0 means an error occurred:
c     -1 means WANT=0 or ABS(WANT)>3,
c     -2 means X was so big that I0(X) overflowed,
c     -3 means X was zero or negative and K0(X) is to be computed.
c     Negative values of STATUS are produced when the error message
c     processor is called with LEVEL=0; positive values of STATUS are
c     accompanied by LEVEL=-2.  See the description of the error message
c     handler for a description of the error level effects.
c     If status = -2 then BI0 = the largest representable number;
c     if status = -3 then BK0 = the largest representable number.
c     ------------------------------------------------------------------
      real             X, BI0, BK0
      integer WANT, STATUS
c
c     *****     External References     ********************************
c
      external SCSEVL, IERM1, SINITS, R1MACH, SERM1
      real             R1MACH, SCSEVL
c
c     *****     Local Variables     ************************************
c
      real             BI0CS(18), AI0CS(46), AI02CS(69)
      real             AK0CS(38), AK02CS(33)
      real             P(6), Q(2), PP(10), QQ(10), F(4), G(3)
      real             SUMF, SUMG, SUMP, SUMQ, Y, Z
      real             EXP10, EXPM10, LSQ2PI, LSQPI2
      parameter (EXP10 = (+.2202646579480671651695790E+5))
C     EXP10 = EXP(10)
      parameter (EXPM10 = (+.4539992976248485153559152E-4))
C     EXPM10 = EXP(-10)
      parameter (LSQ2PI = (+.9189385332046727417803296E+0))
C     LSQ2PI = LOG(SQRT (2 PI))
      parameter (LSQPI2 = (+.2257913526447274323630975E+0))
C     LSQPI2 = LOG(SQRT(PI / 2))
      real             BOUNDK, XIMAX, XIN, XISML, XKMAX, XKN, XKSML
      integer I, NTI0, NTAI0, NTAI02, NTAK0, NTAK02
      save NTI0, NTAI0, NTAI02, XIMAX, XISML
      save BOUNDK, NTAK0, NTAK02, XKMAX, XKSML
c
c     *****     Data     ***********************************************
c
c SERIES FOR BI0        ON THE INTERVAL  0.          TO  9.00000E+00
c                                        WITH WEIGHTED ERROR   9.51E-34
c                                        LOG WEIGHTED ERROR  33.02
c                               SIGNIFICANT FIGURES REQUIRED  33.31
c                                    DECIMAL PLACES REQUIRED  33.65
c
      data BI0CS / -.7660547252839144951081894976243285E-1,
     *   +.1927337953993808269952408750881196E+01,
     *   +.2282644586920301338937029292330415E+00,
     *   +.1304891466707290428079334210691888E-01,
     *   +.4344270900816487451378682681026107E-03,
     *   +.9422657686001934663923171744118766E-05,
     *   +.1434006289510691079962091878179957E-06,
     *   +.1613849069661749069915419719994611E-08,
     *   +.1396650044535669699495092708142522E-10,
     *   +.9579451725505445344627523171893333E-13,
     *   +.5333981859862502131015107744000000E-15,
     *   +.2458716088437470774696785919999999E-17,
     *   +.9535680890248770026944341333333333E-20,
     *   +.3154382039721427336789333333333333E-22,
     *   +.9004564101094637431466666666666666E-25,
     *   +.2240647369123670016000000000000000E-27,
     *   +.4903034603242837333333333333333333E-30,
     *   +.9508172606122666666666666666666666E-33/
c
c SERIES FOR AI0        ON THE INTERVAL  1.25000E-01 TO  3.33333E-01
c                                        WITH WEIGHTED ERROR   2.74E-32
c                                         LOG WEIGHTED ERROR  31.56
c                               SIGNIFICANT FIGURES REQUIRED  30.15
c                                    DECIMAL PLACES REQUIRED  32.39
c
c++ Save data by elements if ~.C.
      data AI0CS(1) /   +.7575994494023795942729872037438E-1 /
      data AI0CS(2) /   +.7591380810823345507292978733204E-2 /
      data AI0CS(3) /   +.4153131338923750501863197491382E-3 /
      data AI0CS(4) /   +.1070076463439073073582429702170E-4 /
      data AI0CS(5) /   -.7901179979212894660750319485730E-5 /
      data AI0CS(6) /   -.7826143501438752269788989806909E-6 /
      data AI0CS(7) /   +.2783849942948870806381185389857E-6 /
      data AI0CS(8) /   +.8252472600612027191966829133198E-8 /
      data AI0CS(9) /   -.1204463945520199179054960891103E-7 /
      data AI0CS(10) /  +.1559648598506076443612287527928E-8 /
      data AI0CS(11) /  +.2292556367103316543477254802857E-9 /
      data AI0CS(12) /  -.1191622884279064603677774234478E-9 /
      data AI0CS(13) /  +.1757854916032409830218331247743E-10 /
      data AI0CS(14) /  +.1128224463218900517144411356824E-11 /
      data AI0CS(15) /  -.1146848625927298877729633876982E-11 /
      data AI0CS(16) /  +.2715592054803662872643651921606E-12 /
      data AI0CS(17) /  -.2415874666562687838442475720281E-13 /
      data AI0CS(18) /  -.6084469888255125064606099639224E-14 /
      data AI0CS(19) /  +.3145705077175477293708360267303E-14 /
      data AI0CS(20) /  -.7172212924871187717962175059176E-15 /
      data AI0CS(21) /  +.7874493403454103396083909603327E-16 /
      data AI0CS(22) /  +.1004802753009462402345244571839E-16 /
      data AI0CS(23) /  -.7566895365350534853428435888810E-17 /
      data AI0CS(24) /  +.2150380106876119887812051287845E-17 /
      data AI0CS(25) /  -.3754858341830874429151584452608E-18 /
      data AI0CS(26) /  +.2354065842226992576900757105322E-19 /
      data AI0CS(27) /  +.1114667612047928530226373355110E-19 /
      data AI0CS(28) /  -.5398891884396990378696779322709E-20 /
      data AI0CS(29) /  +.1439598792240752677042858404522E-20 /
      data AI0CS(30) /  -.2591916360111093406460818401962E-21 /
      data AI0CS(31) /  +.2238133183998583907434092298240E-22 /
      data AI0CS(32) /  +.5250672575364771172772216831999E-23 /
      data AI0CS(33) /  -.3249904138533230784173432285866E-23 /
      data AI0CS(34) /  +.9924214103205037927857284710400E-24 /
      data AI0CS(35) /  -.2164992254244669523146554299733E-24 /
      data AI0CS(36) /  +.3233609471943594083973332991999E-25 /
      data AI0CS(37) /  -.1184620207396742489824733866666E-26 /
      data AI0CS(38) /  -.1281671853950498650548338687999E-26 /
      data AI0CS(39) /  +.5827015182279390511605568853333E-27 /
      data AI0CS(40) /  -.1668222326026109719364501503999E-27 /
      data AI0CS(41) /  +.3625309510541569975700684800000E-28 /
      data AI0CS(42) /  -.5733627999055713589945958399999E-29 /
      data AI0CS(43) /  +.3736796722063098229642581333333E-30 /
      data AI0CS(44) /  +.1602073983156851963365512533333E-30 /
      data AI0CS(45) /  -.8700424864057229884522495999999E-31 /
      data AI0CS(46) /  +.2741320937937481145603413333333E-31 /
c
c SERIES FOR AI02       ON THE INTERVAL  0.          TO  1.25000E-01
c                                        WITH WEIGHTED ERROR   1.97E-32
c                                         LOG WEIGHTED ERROR  31.71
c                               SIGNIFICANT FIGURES REQUIRED  30.15
c                                    DECIMAL PLACES REQUIRED  32.63
c
c++ Save data by elements if ~.C.
      data AI02CS(1) /   +.5449041101410883160789609622680E-1 /
      data AI02CS(2) /   +.3369116478255694089897856629799E-2 /
      data AI02CS(3) /   +.6889758346916823984262639143011E-4 /
      data AI02CS(4) /   +.2891370520834756482966924023232E-5 /
      data AI02CS(5) /   +.2048918589469063741827605340931E-6 /
      data AI02CS(6) /   +.2266668990498178064593277431361E-7 /
      data AI02CS(7) /   +.3396232025708386345150843969523E-8 /
      data AI02CS(8) /   +.4940602388224969589104824497835E-9 /
      data AI02CS(9) /   +.1188914710784643834240845251963E-10 /
      data AI02CS(10) /  -.3149916527963241364538648629619E-10 /
      data AI02CS(11) /  -.1321581184044771311875407399267E-10 /
      data AI02CS(12) /  -.1794178531506806117779435740269E-11 /
      data AI02CS(13) /  +.7180124451383666233671064293469E-12 /
      data AI02CS(14) /  +.3852778382742142701140898017776E-12 /
      data AI02CS(15) /  +.1540086217521409826913258233397E-13 /
      data AI02CS(16) /  -.4150569347287222086626899720156E-13 /
      data AI02CS(17) /  -.9554846698828307648702144943125E-14 /
      data AI02CS(18) /  +.3811680669352622420746055355118E-14 /
      data AI02CS(19) /  +.1772560133056526383604932666758E-14 /
      data AI02CS(20) /  -.3425485619677219134619247903282E-15 /
      data AI02CS(21) /  -.2827623980516583484942055937594E-15 /
      data AI02CS(22) /  +.3461222867697461093097062508134E-16 /
      data AI02CS(23) /  +.4465621420296759999010420542843E-16 /
      data AI02CS(24) /  -.4830504485944182071255254037954E-17 /
      data AI02CS(25) /  -.7233180487874753954562272409245E-17 /
      data AI02CS(26) /  +.9921475412173698598880460939810E-18 /
      data AI02CS(27) /  +.1193650890845982085504399499242E-17 /
      data AI02CS(28) /  -.2488709837150807235720544916602E-18 /
      data AI02CS(29) /  -.1938426454160905928984697811326E-18 /
      data AI02CS(30) /  +.6444656697373443868783019493949E-19 /
      data AI02CS(31) /  +.2886051596289224326481713830734E-19 /
      data AI02CS(32) /  -.1601954907174971807061671562007E-19 /
      data AI02CS(33) /  -.3270815010592314720891935674859E-20 /
      data AI02CS(34) /  +.3686932283826409181146007239393E-20 /
      data AI02CS(35) /  +.1268297648030950153013595297109E-22 /
      data AI02CS(36) /  -.7549825019377273907696366644101E-21 /
      data AI02CS(37) /  +.1502133571377835349637127890534E-21 /
      data AI02CS(38) /  +.1265195883509648534932087992483E-21 /
      data AI02CS(39) /  -.6100998370083680708629408916002E-22 /
      data AI02CS(40) /  -.1268809629260128264368720959242E-22 /
      data AI02CS(41) /  +.1661016099890741457840384874905E-22 /
      data AI02CS(42) /  -.1585194335765885579379705048814E-23 /
      data AI02CS(43) /  -.3302645405968217800953817667556E-23 /
      data AI02CS(44) /  +.1313580902839239781740396231174E-23 /
      data AI02CS(45) /  +.3689040246671156793314256372804E-24 /
      data AI02CS(46) /  -.4210141910461689149219782472499E-24 /
      data AI02CS(47) /  +.4791954591082865780631714013730E-25 /
      data AI02CS(48) /  +.8459470390221821795299717074124E-25 /
      data AI02CS(49) /  -.4039800940872832493146079371810E-25 /
      data AI02CS(50) /  -.6434714653650431347301008504695E-26 /
      data AI02CS(51) /  +.1225743398875665990344647369905E-25 /
      data AI02CS(52) /  -.2934391316025708923198798211754E-26 /
      data AI02CS(53) /  -.1961311309194982926203712057289E-26 /
      data AI02CS(54) /  +.1503520374822193424162299003098E-26 /
      data AI02CS(55) /  -.9588720515744826552033863882069E-28 /
      data AI02CS(56) /  -.3483339380817045486394411085114E-27 /
      data AI02CS(57) /  +.1690903610263043673062449607256E-27 /
      data AI02CS(58) /  +.1982866538735603043894001157188E-28 /
      data AI02CS(59) /  -.5317498081491816214575830025284E-28 /
      data AI02CS(60) /  +.1803306629888392946235014503901E-28 /
      data AI02CS(61) /  +.6213093341454893175884053112422E-29 /
      data AI02CS(62) /  -.7692189292772161863200728066730E-29 /
      data AI02CS(63) /  +.1858252826111702542625560165963E-29 /
      data AI02CS(64) /  +.1237585142281395724899271545541E-29 /
      data AI02CS(65) /  -.1102259120409223803217794787792E-29 /
      data AI02CS(66) /  +.1886287118039704490077874479431E-30 /
      data AI02CS(67) /  +.2160196872243658913149031414060E-30 /
      data AI02CS(68) /  -.1605454124919743200584465949655E-30 /
      data AI02CS(69) /  +.1965352984594290603938848073318E-31 /
c
c -------------------------------------------------------------------
C
C     Coefficients for K0 from Cody for XSMALL .LE.  ARG  .LE. 1.0
C
c -------------------------------------------------------------------
      DATA   P/ 5.8599221412826100000E-04, 1.3166052564989571850E-01,
     1          1.1999463724910714109E+01, 4.6850901201934832188E+02,
     2          5.9169059852270512312E+03, 2.4708152720399552679E+03/
      DATA   Q/-2.4994418972832303646E+02, 2.1312714303849120380E+04/
      DATA   F/-1.6414452837299064100E+00,-2.9601657892958843866E+02,
     1         -1.7733784684952985886E+04,-4.0320340761145482298E+05/
      DATA   G/-2.5064972445877992730E+02, 2.9865713163054025489E+04,
     1         -1.6128136304458193998E+06/
c -------------------------------------------------------------------
C
C     Coefficients for K0 from Cody for 1.0 .LT. ARG .LT. BOUNDK
C
c -------------------------------------------------------------------
      DATA  PP/ 1.1394980557384778174E+02, 3.6832589957340267940E+03,
     1          3.1075408980684392399E+04, 1.0577068948034021957E+05,
     2          1.7398867902565686251E+05, 1.5097646353289914539E+05,
     3          7.1557062783764037541E+04, 1.8321525870183537725E+04,
     4          2.3444738764199315021E+03, 1.1600249425076035558E+02/
      DATA  QQ/ 2.0013443064949242491E+02, 4.4329628889746408858E+03,
     1          3.1474655750295278825E+04, 9.7418829762268075784E+04,
     2          1.5144644673520157801E+05, 1.2689839587977598727E+05,
     3          5.8824616785857027752E+04, 1.4847228371802360957E+04,
     4          1.8821890840982713696E+03, 9.2556599177304839811E+01/
c -------------------------------------------------------------------
C
C SERIES FOR AK0        ON THE INTERVAL  1.25000E-01 TO  5.00000E-01
C                                        WITH WEIGHTED ERROR   2.85E-32
C                                         LOG WEIGHTED ERROR  31.54
C                               SIGNIFICANT FIGURES REQUIRED  30.19
C                                    DECIMAL PLACES REQUIRED  32.33
C
c++ Save data by elements if ~.C.
      data AK0CS(1) /   -.7643947903327941424082978270088E-1 /
      data AK0CS(2) /   -.2235652605699819052023095550791E-1 /
      data AK0CS(3) /   +.7734181154693858235300618174047E-3 /
      data AK0CS(4) /   -.4281006688886099464452146435416E-4 /
      data AK0CS(5) /   +.3081700173862974743650014826660E-5 /
      data AK0CS(6) /   -.2639367222009664974067448892723E-6 /
      data AK0CS(7) /   +.2563713036403469206294088265742E-7 /
      data AK0CS(8) /   -.2742705549900201263857211915244E-8 /
      data AK0CS(9) /   +.3169429658097499592080832873403E-9 /
      data AK0CS(10) /  -.3902353286962184141601065717962E-10 /
      data AK0CS(11) /  +.5068040698188575402050092127286E-11 /
      data AK0CS(12) /  -.6889574741007870679541713557984E-12 /
      data AK0CS(13) /  +.9744978497825917691388201336831E-13 /
      data AK0CS(14) /  -.1427332841884548505389855340122E-13 /
      data AK0CS(15) /  +.2156412571021463039558062976527E-14 /
      data AK0CS(16) /  -.3349654255149562772188782058530E-15 /
      data AK0CS(17) /  +.5335260216952911692145280392601E-16 /
      data AK0CS(18) /  -.8693669980890753807639622378837E-17 /
      data AK0CS(19) /  +.1446404347862212227887763442346E-17 /
      data AK0CS(20) /  -.2452889825500129682404678751573E-18 /
      data AK0CS(21) /  +.4233754526232171572821706342400E-19 /
      data AK0CS(22) /  -.7427946526454464195695341294933E-20 /
      data AK0CS(23) /  +.1323150529392666866277967462400E-20 /
      data AK0CS(24) /  -.2390587164739649451335981465599E-21 /
      data AK0CS(25) /  +.4376827585923226140165712554666E-22 /
      data AK0CS(26) /  -.8113700607345118059339011413333E-23 /
      data AK0CS(27) /  +.1521819913832172958310378154666E-23 /
      data AK0CS(28) /  -.2886041941483397770235958613333E-24 /
      data AK0CS(29) /  +.5530620667054717979992610133333E-25 /
      data AK0CS(30) /  -.1070377329249898728591633066666E-25 /
      data AK0CS(31) /  +.2091086893142384300296328533333E-26 /
      data AK0CS(32) /  -.4121713723646203827410261333333E-27 /
      data AK0CS(33) /  +.8193483971121307640135680000000E-28 /
      data AK0CS(34) /  -.1642000275459297726780757333333E-28 /
      data AK0CS(35) /  +.3316143281480227195890346666666E-29 /
      data AK0CS(36) /  -.6746863644145295941085866666666E-30 /
      data AK0CS(37) /  +.1382429146318424677635413333333E-30 /
      data AK0CS(38) /  -.2851874167359832570811733333333E-31 /
C
C SERIES FOR AK02       ON THE INTERVAL  0.          TO  1.25000E-01
C                                        WITH WEIGHTED ERROR   2.30E-32
C                                         LOG WEIGHTED ERROR  31.64
C                               SIGNIFICANT FIGURES REQUIRED  29.68
C                                    DECIMAL PLACES REQUIRED  32.40
C
c++ Save data by elements if ~.C.
      data AK02CS(1) /   -.1201869826307592239839346212452E-1 /
      data AK02CS(2) /   -.9174852691025695310652561075713E-2 /
      data AK02CS(3) /   +.1444550931775005821048843878057E-3 /
      data AK02CS(4) /   -.4013614175435709728671021077879E-5 /
      data AK02CS(5) /   +.1567831810852310672590348990333E-6 /
      data AK02CS(6) /   -.7770110438521737710315799754460E-8 /
      data AK02CS(7) /   +.4611182576179717882533130529586E-9 /
      data AK02CS(8) /   -.3158592997860565770526665803309E-10 /
      data AK02CS(9) /   +.2435018039365041127835887814329E-11 /
      data AK02CS(10) /  -.2074331387398347897709853373506E-12 /
      data AK02CS(11) /  +.1925787280589917084742736504693E-13 /
      data AK02CS(12) /  -.1927554805838956103600347182218E-14 /
      data AK02CS(13) /  +.2062198029197818278285237869644E-15 /
      data AK02CS(14) /  -.2341685117579242402603640195071E-16 /
      data AK02CS(15) /  +.2805902810643042246815178828458E-17 /
      data AK02CS(16) /  -.3530507631161807945815482463573E-18 /
      data AK02CS(17) /  +.4645295422935108267424216337066E-19 /
      data AK02CS(18) /  -.6368625941344266473922053461333E-20 /
      data AK02CS(19) /  +.9069521310986515567622348800000E-21 /
      data AK02CS(20) /  -.1337974785423690739845005311999E-21 /
      data AK02CS(21) /  +.2039836021859952315522088960000E-22 /
      data AK02CS(22) /  -.3207027481367840500060869973333E-23 /
      data AK02CS(23) /  +.5189744413662309963626359466666E-24 /
      data AK02CS(24) /  -.8629501497540572192964607999999E-25 /
      data AK02CS(25) /  +.1472161183102559855208038400000E-25 /
      data AK02CS(26) /  -.2573069023867011283812351999999E-26 /
      data AK02CS(27) /  +.4601774086643516587376640000000E-27 /
      data AK02CS(28) /  -.8411555324201093737130666666666E-28 /
      data AK02CS(29) /  +.1569806306635368939301546666666E-28 /
      data AK02CS(30) /  -.2988226453005757788979199999999E-29 /
      data AK02CS(31) /  +.5796831375216836520618666666666E-30 /
      data AK02CS(32) /  -.1145035994347681332155733333333E-30 /
      data AK02CS(33) /  +.2301266594249682802005333333333E-31 /
c
      data NTI0 /0/
c
c     *****     Statement Functions     ********************************
c
      xin(z) = ximax + 0.5*log(z/(1.0+1.0/(8.0*z))**2)
      xkn(z) = xkmax - 0.5*log(z/(1.0-1.0/(8.0*z))**2)
c
c     *****     Executable Statements     ******************************
c
      if (nti0 .le. 0) then
         z = 0.1*R1MACH(3)
         boundk = 1.757 - 6.22e-3 * log(z)
         call SINITS (bi0cs, 18, z,  nti0  )
         call SINITS (ai0cs, 46, z,  ntai0 )
         call SINITS (ai02cs, 69, z, ntai02)
         call SINITS (ak0cs, 38, z,  ntak0 )
         call SINITS (ak02cs, 33, z, ntak02)
         xisml = sqrt (80.0E0*z)
         ximax = log(R1MACH(2)) + lsq2pi
         ximax = xin(xin(ximax))
         xksml = sqrt (10.0*z*p(6)/p(5))
c        xksml = sqrt (10.0*z*min(p(6)/p(5),q(2)/q(1)))
         xkmax = lsqpi2 - log(R1MACH(1))
         xkmax = xkn(xkn(xkn(xkmax)))
      end if
c
      if (want.eq.0 .or. abs(want).gt.3) then
         call ierm1('SBI0K0',-1,0,'WANT = 0 OR ABS(WANT) > 3','WANT',
     1      want,'.')
         status=-1
         return
      end if
      status = 0
c
c     Compute I0 if requested.
c
      if (abs(want) .ne. 2) then
         y = abs(x)
         if (y.le.3.0) then
            if (y.le.xisml) then
              BI0 = 1.0E0
            else
              BI0 = 2.75E0 + SCSEVL(y*y/4.5E0-1.0E0, bi0cs, nti0)
            end if
            if (want.lt.0) BI0 = exp(-y) * BI0
         else
            if (y .le. 8.0E0) then
              BI0 = SCSEVL ((48.0E0/y-11.0E0)/5.0E0, ai0cs, ntai0)
            else
              BI0 = SCSEVL (16.0E0/y-1.0E0, ai02cs, ntai02)
            end if
           BI0 = (0.375E0 + BI0) / sqrt(y)
            if (want .gt. 0) then
               if (y .gt. ximax) then
                  call SERM1('SBI0K0',-2,0,'ABS(X) SO BIG I0 OVERFLOWS',
     1               'X',x,'.')
                  status = -2
                  BI0 = R1MACH(2)
c                 y > ximax => y > xkmax
                  BK0 = 0.0
                  if (x .gt. 0.0) return
               else
                  BI0 = (exp(y-10.0) * BI0) * exp10
               end if
            end if
         end if
      end if
c
c     Compute K0 if requested.
c
      if (abs(want) .gt. 1) then
         if (x .le. 0.0E0) then
            call SERM1('SBI0K0',-3,0,'X IS ZERO OR NEGATIVE','X',x,'.')
            status = -3
            BK0 = R1MACH(2)
         else if (x .le. 1.0) then
c                                         0.0 < x <= 1.0
            BK0 = log(x)
            if (x .lt. xksml) then
c                                         0.0 <= x < xksml
               BK0 = p(6)/q(2) - BK0
            else
c                                         xksml <= x <= 1.0
               y = x*x
               sump = ((((p(1)*y+p(2))*y+p(3))*y+p(4))*y+p(5))*y+p(6)
               sumq = (y + q(1))*y + q(2)
               sumf = ((f(1)*y + f(2))*y + f(3))*y + f(4)
               sumg = ((y + g(1))*y + g(2))*y + g(3)
               BK0 = sump/sumq - y * sumf * BK0 / sumg - BK0
            end if
            if (want .lt. 0) BK0 = exp(x) * BK0
         else if (x .le. boundk) then
c                                         1.0 < x <= boundk
            y = 1.0 / x
            sump = pp(1)
            do 120 i = 2, 10
               sump = sump*y + pp(i)
120         continue
            sumq = y
            do 140 i = 1, 9
               sumq = (sumq + qq(i))*y
140         continue
            sumq = sumq + qq(10)
            BK0 = sump / (sumq * sqrt(x))
            if (want .gt. 0) BK0 = exp(-x) * BK0
         else
c                                         boundk < x
            y = 16.0E0 / x
            if (x .le. 8.0E0) then
               BK0 = SCSEVL ((y-5.0E0)/3.0E0, ak0cs, ntak0)
            else
               BK0 = SCSEVL (y-1.0E0, ak02cs, ntak02)
            end if
            BK0 = (1.25E0 + BK0) / sqrt(x)
            if (want .gt. 0) then
               if (x .gt. xkmax) then
                  call SERM1('SBI0K0',1,-2,'X SO BIG K0 UNDERFLOWS','X',
     1               x,'.')
                  BK0 = 0.0
                  status = 1
               else
                  BK0 = (exp(10.0-x) * BK0) * expm10
               end if
            end if
         end if
      end if
C
      return
c
      end
