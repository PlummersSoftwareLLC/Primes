img.bmp: img.ps primesieve.ps
	gs -q -dNOSAFER -dNOPAUSE -dBATCH -sDEVICE=bmpmono -sOutputFile=img.bmp -g1000x1000 -f img.ps

.PHONY: clean
clean:
	rm *.bmp
