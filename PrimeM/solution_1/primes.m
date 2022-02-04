	; Prime Sieve solution in M by Marisa Heit.
	;
	; This is some boilerplate to execute the different implementations
	; in the other files.
	;
	d main
	q
	;
main(size,showResults) n sieve,passes,hstart,hnow
	i $d(size)#10=0 s size=1000000
	i '$d(showResults) s showResults=""
	d runTest(size,showResults,"construct^sieve0")
	d runTest(size,showResults,"construct^sieve1")
	d runTest(size,showResults,"construct^sieve2")
	d runTest(size,showResults,"construct8of30^sieve3")
	d runTest(size,showResults,"construct48of210^sieve3")
	d runTest(size,showResults,"construct480of2310^sieve3")
	q
	;
test0	d runTest(1000000,0,"construct^sieve0")	q
test1	d runTest(1000000,0,"construct^sieve1") q
test2	d runTest(1000000,0,"construct^sieve2") q
test3	d runTest(1000000,0,"construct8of30^sieve3") q
test4	d runTest(1000000,0,"construct48of210^sieve3") q
test5	d runTest(1000000,0,"construct480of2310^sieve3") q
	;
runTest(size,showResults,construct) n sieve,passes,hstart,hnow
	s hstart=$$zhToMicros()
	f passes=0:1 s hnow=$$zhToMicros() q:hnow-hstart>=5000000  d @construct@(.sieve,size) d @sieve("run")@(.sieve)
	d:showResults'="" @sieve("print")@(.sieve,showResults,hnow-hstart,passes)
	w sieve("label"),";",passes,";",hnow-hstart/1000000,";1;algorithm=",sieve("algorithm"),",faithful=",sieve("faithful")
	w:$d(sieve("bits")) ";bits=",sieve("bits")
	w !
	q
	;
zhToMicros() q $p($zh,",",2)*1000000+$p($zh,",",3)
	;
getReferenceResult(size)
	; Historical data for validating our results - the number of primes
	; to be found under some limit, such as 168 primes under 1000
	q $s(size=10:4,size=100:25,size=1000:168,size=10000:1229,size=100000:9592,size=1000000:78498,size=10000000:664579,size=100000000:5761455,size=1000000000:50847534,size=10000000000:455052511,1:0)

