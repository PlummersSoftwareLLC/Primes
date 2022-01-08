	; Prime Sieve implementation in M by Marisa Heit.
	;
	; This is the most faithful version, being the only one that uses 1 bit of storage
	; per number. This is not easy, because M does not officially have any bitwise
	; operators. That leaves this solution sub-optimal, but it's included because
	; Dave said he wanted as many faithful 1-bit solutions as he could get.
	;
	; Different vendors have different extensions that can work with bits. For
	; instance, GT.M/YottaDB have a set of $ZBIT functions that can work on bit
	; strings. Using them would tie this solution to a specific vendor. Moreover,
	; they offer little benefit in this context because they operate on bit
	; *strings* and not numbers. Setting and clearing a bit returns a new string
	; instead of operating on a number in-place.
	;
	; 19 bits per cell was found to give the best balance between space and
	; performance. (And at 60 bits and up, the routine is no longer able to
	; produce valid results.)
	;
	; Measurements running YottaDB r1.33 on an Intel(R) Core(TM) i3-10100 CPU:
	;
	; bits=1  Passes: 11, Time: 5266099, Avg: 478736.272727272727, Limit: 1000000, Count: 78498, Valid: 1
	; bits=2  Passes: 12, Time: 5023243, Avg: 418603.583333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=3  Passes: 13, Time: 5076484, Avg: 390498.76923076923, Limit: 1000000, Count: 78498, Valid: 1
	; bits=4  Passes: 14, Time: 5237432, Avg: 374102.285714285714, Limit: 1000000, Count: 78498, Valid: 1
	; bits=5  Passes: 14, Time: 5097275, Avg: 364091.071428571428, Limit: 1000000, Count: 78498, Valid: 1
	; bits=6  Passes: 15, Time: 5317358, Avg: 354490.533333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=7  Passes: 15, Time: 5267558, Avg: 351170.533333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=8  Passes: 15, Time: 5197674, Avg: 346511.6, Limit: 1000000, Count: 78498, Valid: 1
	; bits=9  Passes: 15, Time: 5139923, Avg: 342661.533333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=10 Passes: 15, Time: 5166230, Avg: 344415.333333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=11 Passes: 15, Time: 5147186, Avg: 343145.733333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=12 Passes: 15, Time: 5144634, Avg: 342975.6, Limit: 1000000, Count: 78498, Valid: 1
	; bits=13 Passes: 15, Time: 5138348, Avg: 342556.533333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=14 Passes: 15, Time: 5152776, Avg: 343518.4, Limit: 1000000, Count: 78498, Valid: 1
	; bits=15 Passes: 15, Time: 5109012, Avg: 340600.8, Limit: 1000000, Count: 78498, Valid: 1
	; bits=16 Passes: 15, Time: 5157688, Avg: 343845.866666666666, Limit: 1000000, Count: 78498, Valid: 1
	; bits=17 Passes: 15, Time: 5148658, Avg: 343243.866666666666, Limit: 1000000, Count: 78498, Valid: 1
	; bits=18 Passes: 15, Time: 5166711, Avg: 344447.4, Limit: 1000000, Count: 78498, Valid: 1
	; bits=19 Passes: 15, Time: 5137400, Avg: 342493.333333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=20 Passes: 14, Time: 5009095, Avg: 357792.5, Limit: 1000000, Count: 78498, Valid: 1
	; bits=21 Passes: 13, Time: 5030802, Avg: 386984.76923076923, Limit: 1000000, Count: 78498, Valid: 1
	; bits=22 Passes: 12, Time: 5117469, Avg: 426455.75, Limit: 1000000, Count: 78498, Valid: 1
	; bits=23 Passes: 12, Time: 5258810, Avg: 438234.166666666666, Limit: 1000000, Count: 78498, Valid: 1
	; bits=24 Passes: 12, Time: 5387491, Avg: 448957.583333333333, Limit: 1000000, Count: 78498, Valid: 1
	; bits=25 Passes: 12, Time: 5425287, Avg: 452107.25, Limit: 1000000, Count: 78498, Valid: 1
	; bits=26 Passes: 11, Time: 5062563, Avg: 460233, Limit: 1000000, Count: 78498, Valid: 1
	; bits=27 Passes: 11, Time: 5102893, Avg: 463899.363636363636, Limit: 1000000, Count: 78498, Valid: 1
	; bits=28 Passes: 11, Time: 5188296, Avg: 471663.272727272727, Limit: 1000000, Count: 78498, Valid: 1
	; bits=29 Passes: 11, Time: 5245356, Avg: 476850.545454545454, Limit: 1000000, Count: 78498, Valid: 1
	; bits=30 Passes: 11, Time: 5260019, Avg: 478183.545454545454, Limit: 1000000, Count: 78498, Valid: 1
	; bits=31 Passes: 11, Time: 5359233, Avg: 487203, Limit: 1000000, Count: 78498, Valid: 1
	; bits=32 Passes: 11, Time: 5435438, Avg: 494130.727272727272, Limit: 1000000, Count: 78498, Valid: 1
	; bits=33 Passes: 11, Time: 5435667, Avg: 494151.545454545454, Limit: 1000000, Count: 78498, Valid: 1
	; bits=34 Passes: 10, Time: 5139970, Avg: 513997, Limit: 1000000, Count: 78498, Valid: 1
	; bits=35 Passes: 10, Time: 5022957, Avg: 502295.7, Limit: 1000000, Count: 78498, Valid: 1
	; bits=36 Passes: 10, Time: 5102170, Avg: 510217, Limit: 1000000, Count: 78498, Valid: 1
	; bits=37 Passes: 10, Time: 5176807, Avg: 517680.7, Limit: 1000000, Count: 78498, Valid: 1
	; bits=38 Passes: 10, Time: 5219832, Avg: 521983.2, Limit: 1000000, Count: 78498, Valid: 1
	; bits=39 Passes: 10, Time: 5176330, Avg: 517633, Limit: 1000000, Count: 78498, Valid: 1
	; bits=40 Passes: 10, Time: 5270975, Avg: 527097.5, Limit: 1000000, Count: 78498, Valid: 1
	; bits=41 Passes: 10, Time: 5330863, Avg: 533086.3, Limit: 1000000, Count: 78498, Valid: 1
	; bits=42 Passes: 10, Time: 5285805, Avg: 528580.5, Limit: 1000000, Count: 78498, Valid: 1
	; bits=43 Passes: 10, Time: 5383242, Avg: 538324.2, Limit: 1000000, Count: 78498, Valid: 1
	; bits=44 Passes: 10, Time: 5432891, Avg: 543289.1, Limit: 1000000, Count: 78498, Valid: 1
	; bits=45 Passes: 10, Time: 5365276, Avg: 536527.6, Limit: 1000000, Count: 78498, Valid: 1
	; bits=46 Passes: 10, Time: 5515683, Avg: 551568.3, Limit: 1000000, Count: 78498, Valid: 1
	; bits=47 Passes: 10, Time: 5528455, Avg: 552845.5, Limit: 1000000, Count: 78498, Valid: 1
	; bits=48 Passes: 10, Time: 5532835, Avg: 553283.5, Limit: 1000000, Count: 78498, Valid: 1
	; bits=49 Passes: 10, Time: 5545391, Avg: 554539.1, Limit: 1000000, Count: 78498, Valid: 1
	; bits=50 Passes: 9, Time: 5000901, Avg: 555655.666666666666, Limit: 1000000, Count: 78498, Valid: 1
	; bits=51 Passes: 9, Time: 5024239, Avg: 558248.777777777777, Limit: 1000000, Count: 78498, Valid: 1
	; bits=52 Passes: 9, Time: 5115356, Avg: 568372.888888888888, Limit: 1000000, Count: 78498, Valid: 1
	; bits=53 Passes: 9, Time: 5142034, Avg: 571337.111111111111, Limit: 1000000, Count: 78498, Valid: 1
	; bits=54 Passes: 9, Time: 5144299, Avg: 571588.777777777777, Limit: 1000000, Count: 78498, Valid: 1
	; bits=55 Passes: 9, Time: 5104068, Avg: 567118.666666666666, Limit: 1000000, Count: 78498, Valid: 1
	; bits=56 Passes: 9, Time: 5226027, Avg: 580669.666666666666, Limit: 1000000, Count: 78498, Valid: 1
	; bits=57 Passes: 9, Time: 5202981, Avg: 578109, Limit: 1000000, Count: 78498, Valid: 1
	; bits=58 Passes: 9, Time: 5309129, Avg: 589903.222222222222, Limit: 1000000, Count: 78498, Valid: 1
	; bits=59 Passes: 9, Time: 5324139, Avg: 591571, Limit: 1000000, Count: 78498, Valid: 1
	; bits=60 Passes: 9, Time: 5598421, Avg: 622046.777777777777, Limit: 1000000, Count: 94764, Valid: 0
	;
	q
	;
construct(sieve,size) n index
	k sieve
	s sieve("size")=size
	s sieve("run")="run^sieve0"
	s sieve("print")="printResults^sieve0"
	s sieve("label")="rheit_m_bits"
	s sieve("algorithm")="base"
	s sieve("faithful")="yes"
	s sieve("bits")=1
	s size=size\2 ; Don't allocate storage for even numbers
	s size=(size+18)\19 ; Number of cells
	f index=0:1:size-1 s sieve(index)=0
	q
	;
getbit(sieve,bit) n cell
	s bit=bit\2
	s cell=bit\19
	s bit=bit#19
	q sieve(cell)\(2**bit)#2
	;
setbit(sieve,bit) n cell
	s bit=bit\2
	s cell=bit\19
	s bit=2**(bit#19)
	s:'(sieve(cell)\bit#2) sieve(cell)=sieve(cell)+bit
	q
	;
run(sieve) n factor,stop,num
	s factor=3
	s stop=sieve("size")**.5
	f  q:factor>stop  d  s factor=factor+2
	. f num=factor:2:sieve("size")-1 i '$$getbit(.sieve,num) s factor=num q
	. f num=factor*factor:factor*2:sieve("size")-1 d setbit(.sieve,num)
	q
	;
printResults(sieve,showResults,duration,passes) n num,count
	w:showResults "2, "
	s count=sieve("size")>=2
	f num=3:2:sieve("size")-1 i '$$getbit(.sieve,num) s count=count+1 w:showResults num,", "
	w:showResults !
	;
	w "Passes: ",passes,", Time: ",duration,", Avg: ",duration/passes
	w ", Limit: ",sieve("size"),", Count: ",count,", Valid: ",count=$$getReferenceResult^primes(sieve("size")),!
	q

