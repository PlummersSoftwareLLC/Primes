        ; Prime Sieve implementation in M by Marisa Heit.
        ;
	; Based on Dave Plummer's original C++ version, this version uses an
	; M array where subnodes exist only for numbers that are not prime.
	; Only allocating the subnodes as the factors are ruled out is what makes
	; this implementation non-faithful, but it is more idiomatic.
	q
	;
construct(sieve,size) ;
	k sieve
	s sieve("size")=size
	s sieve("run")="run^sieve2"
	s sieve("print")="printResults^sieve2"
	s sieve("label")="rheit_m_tree"
	s sieve("algorithm")="base"
	s sieve("faithful")="no"
	q
	;
run(sieve) n factor,stop,num
	s factor=3
	s stop=sieve("size")**.5
	f  q:factor>stop  d  s factor=factor+2
	. f num=factor:2:sieve("size")-1 i '$d(sieve(num)) s factor=num q
	. f num=factor*factor:factor*2:sieve("size")-1 s sieve(num)=""
	q
	;
printResults(sieve,showResults,duration,passes) n num,count,count2
	w:showResults "2, "
	s count=sieve("size")>=2
	f num=3:2:sieve("size")-1 i '$d(sieve(num)) s count=count+1 w:showResults num,", "
	w:showResults !
	;
	w "Passes: ",passes,", Time: ",duration,", Avg: ",duration/passes
	w ", Limit: ",sieve("size"),", Count1: ",count,", Count2: "
	s count2=$$countPrimes(.sieve)
	w count2,", Valid: ",count2=$$getReferenceResult^primes(sieve("size")),!
	q
	;
countPrimes(sieve) n count,index
	; This uses M's native $ORDER operator to iterate over the nodes
	; in the sieve array. Surprisingly, for large sieve sizes this
	; turns out to be quite a bit slower than iterating sequentially
	; over every possible index like printResults does. (At least with
	; YottaDB.)
	s index=""
	; Count the number of set indices in the sieve. Start at below 0,
	; because the "constructor" uses some non-numeric indices.
	f count=-6:1 s index=$O(sieve(index)) q:index=""
	; We have counted the number of not-primes (excluding all even
	; numbers), so figure out how many primes that translates into.
	q sieve("size")\2-count
