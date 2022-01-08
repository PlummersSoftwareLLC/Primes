        ; Prime Sieve implementation in M by Marisa Heit.
        ; Based on Daniel Spangberg's solution_2 for PrimeC.
	;
	; There are three different constructors here for three different wheel
	; sizes, but they all share the same implementation code otherwise.
	q
	;
construct8of30(sieve,size) ; Only count 8 of 30 (2*3*5), there are only 8 numbers not divisible by 2,3,5 within this range
	d constructWheel(.sieve,size)
	s sieve("label")="rheit_m_8of30"
	; Steps array for finding the next number not divisible by 2,3,5
	s (sieve("steps",0),sieve("steps",6))=6
	s (sieve("steps",1),sieve("steps",3),sieve("steps",5))=4
	s (sieve("steps",2),sieve("steps",4),sieve("steps",7))=2
	s sieve("numsteps")=8
	s sieve("startfactor")=7
	q
construct48of210(sieve,size) ; Only count 48 of 210 (2*3*5*7), there are only 48 numbers not divisible by 2,3,5,7 within this range
	d constructWheel(.sieve,size)
	s sieve("label")="rheit_m_48of210"
	; Steps array for finding the next number not divisible by 2,3,5,7
        s (sieve("steps",0),sieve("steps",46))=10
        s (sieve("steps",1),sieve("steps",3),sieve("steps",6),sieve("steps",9),sieve("steps",13),sieve("steps",16),sieve("steps",22),sieve("steps",24),sieve("steps",30),sieve("steps",33),sieve("steps",37),sieve("steps",40),sieve("steps",43),sieve("steps",45),sieve("steps",47))=2
        s (sieve("steps",2),sieve("steps",4),sieve("steps",8),sieve("steps",10),sieve("steps",15),sieve("steps",18),sieve("steps",21),sieve("steps",23),sieve("steps",25),sieve("steps",28),sieve("steps",31),sieve("steps",36),sieve("steps",38),sieve("steps",42),sieve("steps",44))=4
        s (sieve("steps",5),sieve("steps",7),sieve("steps",11),sieve("steps",12),sieve("steps",14),sieve("steps",17),sieve("steps",19),sieve("steps",27),sieve("steps",29),sieve("steps",32),sieve("steps",34),sieve("steps",35),sieve("steps",39),sieve("steps",41))=6
        s (sieve("steps",20),sieve("steps",26))=8
	s sieve("numsteps")=48
	s sieve("startfactor")=11
	q
construct480of2310(sieve,size) ; Only count 480 of 2310 (2*3*5*7*11), there are only 480 numbers not divisible by 2,3,5,7,11 within this range
	d constructWheel(.sieve,size)
	s sieve("label")="rheit_m_480of2310"
	; Steps array for finding the next number not divisible by 2,3,5,7,11
        s (sieve("steps",0),sieve("steps",42),sieve("steps",97),sieve("steps",105),sieve("steps",373),sieve("steps",381),sieve("steps",436),sieve("steps",478))=12
        s (sieve("steps",1),sieve("steps",3),sieve("steps",7),sieve("steps",9),sieve("steps",14),sieve("steps",17),sieve("steps",20),sieve("steps",22),sieve("steps",24),sieve("steps",26),sieve("steps",33),sieve("steps",35),sieve("steps",40),sieve("steps",45),sieve("steps",47),sieve("steps",51),sieve("steps",57),sieve("steps",60),sieve("steps",63),sieve("steps",65),sieve("steps",71),sieve("steps",76),sieve("steps",78),sieve("steps",82),sieve("steps",89),sieve("steps",91),sieve("steps",94),sieve("steps",96),sieve("steps",100),sieve("steps",103),sieve("steps",107),sieve("steps",109),sieve("steps",112),sieve("steps",115),sieve("steps",121),sieve("steps",125),sieve("steps",127),sieve("steps",133),sieve("steps",139),sieve("steps",144),sieve("steps",147),sieve("steps",150),sieve("steps",153),sieve("steps",156),sieve("steps",159),sieve("steps",163),sieve("steps",168),sieve("steps",170),sieve("steps",176),sieve("steps",178),sieve("steps",181),sieve("steps",183),sieve("steps",188),sieve("steps",190),sieve("steps",193),sieve("steps",195),sieve("steps",197),sieve("steps",200),sieve("steps",207),sieve("steps",209),sieve("steps",213),sieve("steps",215),sieve("steps",222),sieve("steps",226),sieve("steps",228),sieve("steps",232),sieve("steps",237),sieve("steps",239),sieve("steps",241),sieve("steps",246),sieve("steps",250),sieve("steps",252),sieve("steps",256),sieve("steps",263),sieve("steps",265),sieve("steps",269),sieve("steps",271),sieve("steps",278),sieve("steps",281),sieve("steps",283),sieve("steps",285),sieve("steps",288),sieve("steps",290),sieve("steps",295),sieve("steps",297),sieve("steps",300),sieve("steps",302),sieve("steps",308),sieve("steps",310),sieve("steps",315),sieve("steps",319),sieve("steps",322),sieve("steps",325),sieve("steps",328),sieve("steps",331),sieve("steps",334),sieve("steps",339),sieve("steps",345),sieve("steps",351),sieve("steps",353),sieve("steps",357),sieve("steps",363),sieve("steps",366),sieve("steps",369),sieve("steps",371),sieve("steps",375),sieve("steps",378),sieve("steps",382),sieve("steps",384),sieve("steps",387),sieve("steps",389),sieve("steps",396),sieve("steps",400),sieve("steps",402),sieve("steps",407),sieve("steps",413),sieve("steps",415),sieve("steps",418),sieve("steps",421),sieve("steps",427),sieve("steps",431),sieve("steps",433),sieve("steps",438),sieve("steps",443),sieve("steps",445),sieve("steps",452),sieve("steps",454),sieve("steps",456),sieve("steps",458),sieve("steps",461),sieve("steps",464),sieve("steps",469),sieve("steps",471),sieve("steps",475),sieve("steps",477))=4
        s (sieve("steps",2),sieve("steps",5),sieve("steps",8),sieve("steps",12),sieve("steps",15),sieve("steps",21),sieve("steps",23),sieve("steps",28),sieve("steps",30),sieve("steps",34),sieve("steps",37),sieve("steps",39),sieve("steps",41),sieve("steps",44),sieve("steps",46),sieve("steps",49),sieve("steps",55),sieve("steps",58),sieve("steps",64),sieve("steps",70),sieve("steps",73),sieve("steps",77),sieve("steps",80),sieve("steps",83),sieve("steps",86),sieve("steps",88),sieve("steps",90),sieve("steps",95),sieve("steps",98),sieve("steps",101),sieve("steps",106),sieve("steps",108),sieve("steps",114),sieve("steps",117),sieve("steps",120),sieve("steps",123),sieve("steps",126),sieve("steps",128),sieve("steps",130),sieve("steps",132),sieve("steps",136),sieve("steps",142),sieve("steps",145),sieve("steps",151),sieve("steps",158),sieve("steps",164),sieve("steps",166),sieve("steps",169),sieve("steps",171),sieve("steps",173),sieve("steps",175),sieve("steps",177),sieve("steps",182),sieve("steps",186),sieve("steps",194),sieve("steps",196),sieve("steps",204),sieve("steps",208),sieve("steps",211),sieve("steps",214),sieve("steps",216),sieve("steps",218),sieve("steps",220),sieve("steps",224),sieve("steps",227),sieve("steps",233),sieve("steps",238),sieve("steps",240),sieve("steps",245),sieve("steps",251),sieve("steps",254),sieve("steps",258),sieve("steps",260),sieve("steps",262),sieve("steps",264),sieve("steps",267),sieve("steps",270),sieve("steps",274),sieve("steps",282),sieve("steps",284),sieve("steps",292),sieve("steps",296),sieve("steps",301),sieve("steps",303),sieve("steps",305),sieve("steps",307),sieve("steps",309),sieve("steps",312),sieve("steps",314),sieve("steps",320),sieve("steps",327),sieve("steps",333),sieve("steps",336),sieve("steps",342),sieve("steps",346),sieve("steps",348),sieve("steps",350),sieve("steps",352),sieve("steps",355),sieve("steps",358),sieve("steps",361),sieve("steps",364),sieve("steps",370),sieve("steps",372),sieve("steps",377),sieve("steps",380),sieve("steps",383),sieve("steps",388),sieve("steps",390),sieve("steps",392),sieve("steps",395),sieve("steps",398),sieve("steps",401),sieve("steps",405),sieve("steps",408),sieve("steps",414),sieve("steps",420),sieve("steps",423),sieve("steps",429),sieve("steps",432),sieve("steps",434),sieve("steps",437),sieve("steps",439),sieve("steps",441),sieve("steps",444),sieve("steps",448),sieve("steps",450),sieve("steps",455),sieve("steps",457),sieve("steps",463),sieve("steps",466),sieve("steps",470),sieve("steps",473),sieve("steps",476),sieve("steps",479))=2
        s (sieve("steps",4),sieve("steps",6),sieve("steps",10),sieve("steps",11),sieve("steps",13),sieve("steps",16),sieve("steps",18),sieve("steps",27),sieve("steps",31),sieve("steps",32),sieve("steps",36),sieve("steps",48),sieve("steps",50),sieve("steps",52),sieve("steps",53),sieve("steps",54),sieve("steps",56),sieve("steps",59),sieve("steps",61),sieve("steps",66),sieve("steps",68),sieve("steps",72),sieve("steps",74),sieve("steps",75),sieve("steps",79),sieve("steps",81),sieve("steps",84),sieve("steps",92),sieve("steps",99),sieve("steps",102),sieve("steps",104),sieve("steps",111),sieve("steps",113),sieve("steps",116),sieve("steps",118),sieve("steps",122),sieve("steps",124),sieve("steps",134),sieve("steps",135),sieve("steps",137),sieve("steps",138),sieve("steps",140),sieve("steps",141),sieve("steps",143),sieve("steps",146),sieve("steps",148),sieve("steps",152),sieve("steps",155),sieve("steps",157),sieve("steps",160),sieve("steps",162),sieve("steps",167),sieve("steps",180),sieve("steps",184),sieve("steps",185),sieve("steps",187),sieve("steps",191),sieve("steps",199),sieve("steps",201),sieve("steps",202),sieve("steps",203),sieve("steps",205),sieve("steps",206),sieve("steps",210),sieve("steps",212),sieve("steps",221),sieve("steps",223),sieve("steps",225),sieve("steps",229),sieve("steps",230),sieve("steps",234),sieve("steps",244),sieve("steps",248),sieve("steps",249),sieve("steps",253),sieve("steps",255),sieve("steps",257),sieve("steps",266),sieve("steps",268),sieve("steps",272),sieve("steps",273),sieve("steps",275),sieve("steps",276),sieve("steps",277),sieve("steps",279),sieve("steps",287),sieve("steps",291),sieve("steps",293),sieve("steps",294),sieve("steps",298),sieve("steps",311),sieve("steps",316),sieve("steps",318),sieve("steps",321),sieve("steps",323),sieve("steps",326),sieve("steps",330),sieve("steps",332),sieve("steps",335),sieve("steps",337),sieve("steps",338),sieve("steps",340),sieve("steps",341),sieve("steps",343),sieve("steps",344),sieve("steps",354),sieve("steps",356),sieve("steps",360),sieve("steps",362),sieve("steps",365),sieve("steps",367),sieve("steps",374),sieve("steps",376),sieve("steps",379),sieve("steps",386),sieve("steps",394),sieve("steps",397),sieve("steps",399),sieve("steps",403),sieve("steps",404),sieve("steps",406),sieve("steps",410),sieve("steps",412),sieve("steps",417),sieve("steps",419),sieve("steps",422),sieve("steps",424),sieve("steps",425),sieve("steps",426),sieve("steps",428),sieve("steps",430),sieve("steps",442),sieve("steps",446),sieve("steps",447),sieve("steps",451),sieve("steps",460),sieve("steps",462),sieve("steps",465),sieve("steps",467),sieve("steps",468),sieve("steps",472),sieve("steps",474))=6
        s (sieve("steps",19),sieve("steps",62),sieve("steps",67),sieve("steps",93),sieve("steps",110),sieve("steps",149),sieve("steps",154),sieve("steps",161),sieve("steps",179),sieve("steps",189),sieve("steps",192),sieve("steps",198),sieve("steps",231),sieve("steps",236),sieve("steps",242),sieve("steps",247),sieve("steps",280),sieve("steps",286),sieve("steps",289),sieve("steps",299),sieve("steps",317),sieve("steps",324),sieve("steps",329),sieve("steps",368),sieve("steps",385),sieve("steps",411),sieve("steps",416),sieve("steps",459))=8
        s (sieve("steps",25),sieve("steps",453))=14
        s (sieve("steps",29),sieve("steps",38),sieve("steps",43),sieve("steps",69),sieve("steps",85),sieve("steps",87),sieve("steps",119),sieve("steps",129),sieve("steps",131),sieve("steps",165),sieve("steps",172),sieve("steps",174),sieve("steps",217),sieve("steps",219),sieve("steps",235),sieve("steps",243),sieve("steps",259),sieve("steps",261),sieve("steps",304),sieve("steps",306),sieve("steps",313),sieve("steps",347),sieve("steps",349),sieve("steps",359),sieve("steps",391),sieve("steps",393),sieve("steps",409),sieve("steps",435),sieve("steps",440),sieve("steps",449))=10
	s sieve("numsteps")=480
	s sieve("startfactor")=13
	q
constructWheel(sieve,size) ;
	k sieve
	s sieve("size")=size
	s sieve("run")="run^sieve3"
	s sieve("print")="printResults^sieve3"
	s sieve("faithful")="no"
	s sieve("algorithm")="wheel"
	q
	;
run(sieve) n factor,stop,num,maxints,step,numsteps
	s maxints=sieve("size")
	s stop=maxints**.5
	s step=1
	s factor=sieve("startfactor")
	s numsteps=sieve("numsteps")
	f  q:factor>stop  d
	. i '$d(sieve(factor)) f num=factor*factor:factor*2:maxints-1 s sieve(num)="" ; If prime, mask all integer multiples
	. s factor=factor+sieve("steps",step),step=step+1
	. s:step=numsteps step=0
	q
	;
printResults(sieve,showResults,duration,passes) n count,count2,factor,step,maxints,numsteps
	s factor=sieve("startfactor")
	s maxints=sieve("size")
	s count=0
	d
	. i maxints>1 s count=1 w:showResults "2, "
	. i maxints>2 s count=2 w:showResults "3, "
	. i maxints>4 s count=3 w:showResults "5, " q:factor=7
	. i maxints>6 s count=4 w:showResults "7, " q:factor=11
	. i maxints>10 s count=5 w:showResults "11, "
	s step=1
	s numsteps=sieve("numsteps")
	f  q:factor>maxints  d
	. i '$d(sieve(factor)) s count=count+1 w:showResults factor,", "
	. s factor=factor+sieve("steps",step),step=step+1
	. s:step=numsteps step=0
	w:showResults !
	;
	w "Passes: ",passes,", Time: ",duration,", Avg: ",duration/passes
	w ", Limit: ",sieve("size"),", Count: ",count,", Valid: ",count=$$getReferenceResult^primes(sieve("size")),!
	q
