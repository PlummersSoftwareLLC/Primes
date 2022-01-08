        ; Prime Sieve implementation in M by Marisa Heit.
        ;
	; This is based on Dave Plummer's original C++ version. Here we
	; allocate the entire array at construction time and never
	; allocate nodes again.
	q
	;
construct(sieve,size) n index
	k sieve
	s sieve("size")=size
	s sieve("run")="run^sieve1"
	s sieve("print")="printResults^sieve1"
	s sieve("label")="rheit_m_array"
	s sieve("algorithm")="base"
	s sieve("faithful")="yes"
	f index=1:1:size-1 s sieve(index)=1
	q
	;
run(sieve) n factor,stop,num
	s factor=3
	s stop=sieve("size")**.5
	f  q:factor>stop  d  s factor=factor+2
	. f num=factor:2:sieve("size")-1 i sieve(num) s factor=num q
	. f num=factor*factor:factor*2:sieve("size")-1 s sieve(num)=0
	q
	;
printResults(sieve,showResults,duration,passes) n num,count
	w:showResults "2, "
	s count=sieve("size")>=2
	f num=3:2:sieve("size")-1 i sieve(num) s count=count+1 w:showResults num,", "
	w:showResults !
	;
	w "Passes: ",passes,", Time: ",duration,", Avg: ",duration/passes
	w ", Limit: ",sieve("size"),", Count: ",count,", Valid: ",count=$$getReferenceResult^primes(sieve("size")),!
	q

